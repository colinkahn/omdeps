(ns omdeps.main
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer [>! put! chan]]
            [clojure.set]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:import  [goog.ui IdGenerator]))

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

(def app-state
  (atom {:editing nil
         :query ""
         :open-deps []
         :library [
            {:name "my-library"
             :root true
             :id "1"
             :version "1.0.0"
             :deps ["2" "4"]}
            {:name "backbone"
             :id "2"
             :version "1.0.0"
             :deps ["3"]}
            {:name "underscore"
             :id "3"
             :version "1.0.0"}
            {:name "angular"
             :id "4"
             :version "2.0.0"}]}))

;; HELPERS

(defn has-string [s c]
  (if s
    (not (= (.indexOf s c) -1))
    false))

(defn is-open [item app]
  (some #{(:id item)} (:open-deps app)))

(defn find-all
  "Finds every item item in items that matches the key values
   in props"
  [items props]
  ((clojure.set/index items  (keys props)) props))


(defn find-one
  "Finds the first item in prop that matches each key value
   in props"
  [items props]
  (-> items
       (find-all props)
       first))

(defn get-deps [item app]
  (let [items (:library app)]
    (reduce
      (fn [m id]
        (let [dep (find-one items {:id id})]
          (if dep
            (conj m dep)
            m)))
      [] (:deps item))))

(defn remove-item [items id]
  (remove #(= (:id %) id) items))

(defn remove-in-deps [items id]
  (map (fn [item]
         (assoc item :deps
           (remove #{id} (:deps item))))
       items))

(defn trim-tree [oitems odeps]
  (loop [deps odeps
         items oitems]
	(let [id (first deps)
        item (find-one items {:id id})]
      (if (not id)
        items
        (recur (remove #{id} deps)
               (-> items
                   (trim-tree (:deps item))
                   (remove-in-deps id)
                   (remove-item id)))))))

(defn prep-item [item app]
  (-> item
    (conj {:open (is-open item app)})))

(defn prevent-default [cb]
  (fn [event] (.preventDefault event) (cb)))

(defn toggle-in-vec
  "Finds the value v in vector l and removes if present or
   adds if it is not."
  [v l]
  (if (some #{v} l)
    (remove #(= v %) l)
    (conj l v)))

;; EVENT HANDLERS

(defn handle-filter [e app]
  (let [query (.. e -target -value)]
    (om/transact! app :query (fn [] query))))

(defn handle-delete [app item-id]
  (om/transact! app :library
    (fn [deps]
      (trim-tree deps [item-id]))))

(defn handle-input-field [k e owner]
  (om/set-state! owner k (.. e -target -value)))

(defn handle-new [app owner]
  (let [n (om/get-state owner :new-name)
        v (om/get-state owner :new-version)
        id (guid)]
    (om/transact! app :library
      (fn [deps]
        (conj
          (map
            (fn [item]
              (if (= (:id item) (:editing app))
                (assoc item :deps (conj (:deps item) id))
                item)) deps) {:name n :version v :id id})))))

(defn handle-update-item [app owner]
  (let [n (om/get-state owner :name)
        v (om/get-state owner :version)]
    (om/transact! app :library
      (fn [deps]
        (map
          (fn [item]
            (if (= (:id item) (:editing app))
              (assoc item :name n :version v)
              item))
          deps)))))

(defn handle-item-edit [e app item-id]
  (om/transact! app #(assoc % :editing item-id)))

(defn handle-item-toggle-open [e app item-id]
  (om/transact! app [:open-deps]
    (fn [items]
      (toggle-in-vec item-id items))))

(defn handle-event [app [type item-id :as e]]
  (case type
    :edit        (handle-item-edit e app item-id)
    :toggle-open (handle-item-toggle-open e app item-id)))

;; COMPONENTS

(defn recur-list [item owner]
  (reify
    om/IDisplayName
    (display-name [_]
      (str "Recur List: " (:name item)))
    om/IShouldUpdate
    (should-update [_ _ _]
      true)
    om/IRenderState
    (render-state [_ {:keys [comm app] :as state}]
      (let [item-deps (get-deps item app)
            item-deps-count (count item-deps)
            item-open (:open item)
            item-open-deps (:open-deps item)
            item-id (:id item)]
        (dom/li nil
          (when (not= item-deps-count 0)
            (dom/a #js {:onClick (prevent-default #(put! comm [:toggle-open item-id]))
                        :href "#"
                        :className "toggle"}
                      (if-not (nil? item-open) "-" "+")))
          (dom/a #js {:onClick (prevent-default #(put! comm [:edit item-id]))
                      :href "#"} (:name item))
          (str " v" (:version item))
          (when (and (not (nil? item-open)) (not= item-deps-count 0))
            (apply dom/ul nil
              (om/build-all recur-list item-deps
                            {:key :id
                             :state {:comm comm :app app}
                             :fn #(prep-item % app)}))))))))

(defn item-edit [item owner]
  (reify
    om/IDisplayName
    (display-name [_] "Item Edit")
    om/IInitState
    (init-state [_]
      {:name (:name item)
       :version (:version item)
       :id (:id item)
       :new-name ""
       :new-version ""})
    om/IWillReceiveProps
    (will-receive-props [_ next-state]
      (om/set-state! owner :id (:id next-state))
      (om/set-state! owner :name (:name next-state))
      (om/set-state! owner :version (:version next-state))
      (om/set-state! owner :new-name "")
      (om/set-state! owner :new-version ""))
    om/IRenderState
    (render-state [_ {:keys [comm app] :as state}]
      (let [deps (get-deps item app)
            fdps (filter #(has-string (:name %) (:query app)) deps)]
        (dom/div nil
          (dom/form nil
            (dom/h2 nil "Editing")
            (dom/fieldset nil
              (dom/legend nil "Library Properties")
              (dom/label nil "Name"
                (dom/input #js {:value (om/get-state owner [:name])
                                :onChange #(handle-input-field :name % owner)}))
              (dom/label nil "Version"
                (dom/input #js {:value (om/get-state owner [:version])
                                :onChange #(handle-input-field :version % owner)}))
              (dom/button #js {:onClick (prevent-default
                                          #(handle-update-item app owner))}
                          "Update"))
            (dom/fieldset nil
              (dom/legend nil "Manage Dependencies")
              (dom/label nil "Filter "
                (dom/input #js {:onKeyUp #(handle-filter % app)}))
              (into-array
                (map (fn [item]
                       (let [item-id (:id item)]
                        (dom/li nil
                          (:name item)
                          (dom/button #js {:onClick (prevent-default
                                                      #(handle-delete app item-id))}
                                      "Delete"))))
                      fdps)))
            (dom/fieldset nil
              (dom/legend nil "Add Dependency")
              (dom/label nil "Name "
                (dom/input #js {:value (om/get-state owner [:new-name])
                                :onChange #(handle-input-field :new-name % owner)}))
              (dom/label nil "Version "
                (dom/input #js {:value (om/get-state owner [:new-version])
                                :onChange #(handle-input-field :new-version % owner)}))
              (dom/button #js {:onClick (prevent-default #(handle-new app owner))}
                          "Add"))))))))


(defn projects-app [app owner]
  (reify
    om/IDisplayName
    (display-name [_] "App")
    om/IWillMount
    (will-mount [_]
      (let [comm (chan)]
        (om/set-state! owner [:comm] comm)
        (go (while true
              (handle-event app (<! comm))))))
    om/IRenderState
    (render-state [_ {:keys [comm] :as state}]
      (let [edit-id (:editing app)]
        (dom/div nil
          (dom/code #js {:className "state"} (pr-str app))
          (dom/div #js {:className "recur-list"}
            (om/build recur-list (find-one (:library app) {:root true})
              {:state {:comm comm :app app}
               :fn #(prep-item % app)}))
          (if (:editing app)
            (dom/div #js {:className "edit"}
              (om/build item-edit (find-one (:library app) {:id edit-id})
                        {:state {:comm comm :app app}}))))))))

(om/root projects-app app-state {:target js/document.body})
