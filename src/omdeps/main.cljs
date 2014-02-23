(ns omdeps.main
  (:require-macros [cljs.core.async.macros :refer [go alt!]])
  (:require [cljs.core.async :refer  [>! put! chan]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:import  [goog.ui IdGenerator]))

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

(def app-state (atom {:editing nil
                      :query ""
                      :open-deps []
                      :library {:name "my-library"
                                :version "1.0.0"
                                :id (guid)
                                :deps [{:name "backbone" 
                                        :version "1.0.0" 
                                        :id (guid)
                                        :deps [{:name "underscore"
                                                :version "1.0.0"
                                                :id (guid)}]}
                                       {:name "angular" 
                                        :version "2.0.0" 
                                        :id (guid)}]}}))

;; HELPERS

(defn has-string [s c]
  (not (= (.indexOf s c) -1)))

(defn is-open [item app]
  (some #{(:id item)} (:open-deps app)))

(defn prep-item [item app]
  (-> item
    (conj {:open (is-open item app)})))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn prevent-default [cb]
  (fn [event] (.preventDefault event) (cb)))

(defn toggle-in-vec [v l]
  (if (some #{v} l)
    (remove #(= v %) l)
    (conj l v)))

;; EVENT HANDLERS

(defn handle-filter [e app]
  (let [query (.. e -target -value)]
    (om/transact! app :query (fn [] query))))

(defn handle-delete [app item]
  (om/transact! app (butlast (.-path item))
    (fn [deps]
      (vec-remove (last (.-path item)) deps))))

(defn handle-input-field [k e owner]
  (om/set-state! owner k (.. e -target -value)))

(defn handle-new [app owner]
  (let [n (om/get-state owner :new-name)
        v (om/get-state owner :new-version)]
    (om/transact! app (conj (:editing @app) :deps)
      (fn [deps]
        (vec  (conj deps {:name n :version v :id (guid)}))))))

(defn handle-update-item [app owner]
  (let [n (om/get-state owner :name)
        v (om/get-state owner :version)]
    (om/transact! app (:editing @app)
      (fn [item] (assoc item :name n :version v)))))

(defn handle-item-edit [e app item]
  (om/transact! app :editing #(.-path item)))

(defn handle-item-toggle-open [e app item]
  (om/transact! app [:open-deps]
    (fn [items]
      (toggle-in-vec (:id @item) items))))

(defn handle-event [app [type item :as e]]
  (case type
    :edit        (handle-item-edit e app item)
    :toggle-open (handle-item-toggle-open e app item)))

;; COMPONENTS

(defn recur-list [item owner]
  (reify
    om/IShouldUpdate
    (should-update [_ _ _]
      true)
    om/IRenderState
    (render-state [_ {:keys [comm app] :as state}]
      (let [item-deps (:deps item)
            item-deps-count (count item-deps)
            item-open (:open item)
            item-open-deps (:open-deps item)]
        (dom/li nil 
          (when (not= item-deps-count 0)
            (dom/a #js {:onClick (prevent-default #(put! comm [:toggle-open item]))
                        :href "#"
                        :className "toggle"}
                      (if-not (nil? item-open) "-" "+")))
          (dom/a #js {:onClick (prevent-default #(put! comm [:edit item]))
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
    om/IInitState
    (init-state [_]
      {:name (:name item)
       :version (:version item)
       :id (:id item)
       :new-name ""
       :new-version ""})
    ; Change this to IWillRecieveProps when that exists
    om/IDidUpdate
    (did-update [_ _ prev-state _]
      (when (not= (:id prev-state) (:id item))
        (om/set-state! owner :id (:id item))
        (om/set-state! owner :name (:name item))
        (om/set-state! owner :version (:version item))
        (om/set-state! owner :new-name "")
        (om/set-state! owner :new-version "")))
    om/IRenderState
    (render-state [_ {:keys [comm app] :as state}]
      (let [fdps (filter #(has-string (:name %) (:query app)) (:deps item))]
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
                (map (fn [item] (dom/li nil
                        (:name item)
                        (dom/button #js {:onClick (prevent-default
                                                    #(handle-delete app item))}
                                    "Delete")))
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
    om/IWillMount
    (will-mount [_]
      (let [comm (chan)]
        (om/set-state! owner [:comm] comm)
        (go (while true
              (handle-event app (<! comm))))))
    om/IRenderState
    (render-state [_ {:keys [comm] :as state}]
      (let [edit-path (:editing app)]
        (dom/div nil
          (dom/code #js {:className "state"} (pr-str app))
          (dom/div #js {:className "recur-list"}
            (om/build recur-list (:library app)
              {:state {:comm comm :app app}
               :fn #(prep-item % app)}))
          (if (:editing app)
            (dom/div #js {:className "edit"}
              (om/build item-edit (get-in app edit-path)
                        {:state {:comm comm :app app}}))))))))

(om/root projects-app app-state {:target js/document.body})
