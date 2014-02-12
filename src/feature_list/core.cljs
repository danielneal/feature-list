(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! map< map> chan  pub sub unsub <! >!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [cljs.reader :as reader]
            [cljs-uuid.core :as uuid]
            [clojure.walk :as walk]
            [feature-list.bus :refer [bus]]))

(enable-console-print!)
;; -------------------------
;; Helpers
;; -------------------------

(defn classes
  "Combine CSS classes into a string"
  [& classes]
  (apply str (interpose " " classes)))

;; -------------------------
;;       Om Components
;; -------------------------

(extend-type js/String
           ICloneable
           (-clone [s] (js/String. s))
           om/IValue
           (-value [s] (str s)))

;; -------------------------
;; Editable component
;; -------------------------

(defn editable
  "An editable piece of text"
  [text owner {:keys [class on-commit type identifier] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
                {:editing false})

    om/IDidUpdate
    (did-update [_ prev-props prev-state root-node]
                (when (om/get-state owner :editing)
                  (.focus root-node)))

    om/IRenderState
    (render-state [_ {:keys [editing] :as state}]
                  (let [bus (om/get-shared owner :bus)
                        start-editing (fn [e] (om/set-state! owner :editing true))
                        handle-change (fn [e text owner] (om/transact! text (fn [_] (.. e -target -value))))
                        commit-change (fn [] (om/set-state! owner :editing false) (on-commit))]
                    (if editing
                      (dom/input #js {:onBlur commit-change
                                      :onChange #(handle-change % text owner)
                                      :value (om/value text)
                                      :className class
                                      :onKeyPress #(when (== (.-keyCode %) 13) (commit-change))})
                      (dom/span #js {:onClick #(start-editing %)
                                     :className (classes "clickable" class)}
                                (om/value text)))))))

;; -------------------------
;; Votes remaining view
;; -------------------------

(defn votes-view
  "Create an indication for how many votes a user has left"
  [votes]
  (reify
    om/IRender
    (render [this]
            (dom/h1 #js {:className "votes"}
                    (om/value votes)))))

;; -------------------------
;; Feature view
;; -------------------------
(defn vote-for-feature [feature owner]
  (let [bus (om/get-shared owner :bus)]
    (om/transact! feature :feature/votes inc)
    (put! bus {:message-type :vote :feature @feature})))

(defn feature-view
  "Create a react/om component that will display a single feature"
  [feature owner]
  (reify
    om/IInitState
    (init-state [_]
                {:expanded false})

    om/IRenderState
    (render-state [this {:keys [expanded vote]}]
      (let [toggle-description (fn [] (om/set-state! owner :expanded (not expanded)))
            bus (om/get-shared owner :bus)]
      (dom/li nil
       (dom/button #js {:className "pure-button button-small" :onClick #(vote-for-feature feature owner)} "Vote")
        (dom/span #js {:className "number-of-votes"} (:feature/votes feature))
        (dom/div #js {:className "feature"}
                 (dom/i #js {:className (classes "expand fa " (if expanded "fa-caret-down" "fa-caret-right")) :onClick toggle-description})
                 (om/build editable (:feature/title feature) {:opts {:class "title" :on-commit #(put! bus {:message-type :update-feature :feature @feature})}})
                 (when expanded (om/build editable (:feature/description feature) {:opts {:class "description" :on-commit #(put! bus {:message-type :update-feature :feature @feature})}}))))))))

;; -------------------------
;; Add new feature view
;; -------------------------

(defn add-feature
  [app owner]
  (let [bus (om/get-shared owner :bus)
        title (.-value (om/get-node owner "featuretitle"))
        description (.-value (om/get-node owner "featuredescription"))
        id-chan (chan)]
    (sub bus :id id-chan)
    (put! bus {:message-type :request-id})
    (go (let [{id :id} (<! id-chan)
              feature {:feature/title title :feature/description description :feature/votes 0 :feature/id id}]
          (om/transact! app :features conj feature)
          (om/set-state! owner :feature/title "")
          (om/set-state! owner :feature/description "")
          (put! bus {:message-type :add-feature :feature feature})
          (unsub bus :id id-chan)))))

;; -------------------------
;; Feature list view
;; -------------------------

(defn features-view
  "Create a react/om component that will display and manage a sorted list of features, with
  a text box to add new features"
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:feature/title ""
       :feature/description ""})

    om/IWillMount
    (will-mount [_]
      (let [bus (om/get-shared owner :bus)
            vote (chan)
            init (chan)
            update (chan)]
        (sub bus :init init)
        (sub bus :vote vote)
        (sub bus :update-feature update)
        (put! bus {:message-type :request-features})

        (go-loop []
              (when-let [{f :features} (<! init)]
                (om/transact! app :features (fn [features] (apply conj features f)))
                (recur)))

        (go-loop []
              (when-let [{feature :feature} (<! vote)]
                (om/transact! app :features (fn [features]
                                              (->> features
                                                   (map #(if (= (:feature/id %) (:feature/id feature)) (update-in % [:feature/votes] inc) %))
                                                   (sort-by :feature/votes (fn [a b] (> a b)))
                                                   (into []))))
                (om/transact! app :votes-remaining dec)
                (recur)))

        (go-loop []
                 (when-let [{feature :feature} (<! update)]
                   (om/transact! app :features (fn [features] (mapv #(if (= (:feature/id %) (:feature/id feature)) feature %) features)))
                   (recur)))))

    om/IRenderState
    (render-state [this state]
                  (letfn [(handle-change [e owner k] (om/set-state! owner k (.. e -target -value)))]
                    (dom/div #js {:className "page-container"}
                             (dom/h1 nil "Feature list")
                             (apply dom/ul nil
                                    (om/build-all feature-view (:features app) {:init-state state}))
                             (om/build votes-view (:votes-remaining app))
                             (dom/form #js {:className "pure-form"}
                                       (dom/input #js {:type "text" :placeholder "feature title" :ref "featuretitle" :value (:feature/title state) :onChange #(handle-change % owner :feature/title)})
                                       (dom/input #js {:type "text" :placeholder "feature description" :ref "featuredescription" :value (:feature/description state) :onChange #(handle-change % owner :feature/description)})
                                       (dom/button #js {:className "pure-button button-small" :onClick  (fn [e] (.preventDefault e) (add-feature app owner))} "Add feature")))))))

;; -------------------------
;;    Build the app
;; -------------------------
(def app-state (atom {:features []
                      :votes-remaining 10}))

(go (let [client-id (.-uuid (uuid/make-random))
          ws (<! (ws-ch (str "ws://localhost:3000/ws/" client-id)))
          bus (bus ws :message-type
                 :outbound-transform #(pr-str (merge {:client-id client-id} (walk/postwalk om/value %)))
                 :inbound-transform #(reader/read-string (:message %)))]
      (om/root app-state {:bus bus}
               features-view (. js/document (getElementById "features")))))

