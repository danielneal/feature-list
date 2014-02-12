(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! map< map> chan  pub sub unsub <! >!]]
            [clojure.data :as data]
            [clojure.string :as string]
            [feature-list.om-bus :refer [om-bus]]))

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
;; Focus input component
;; -------------------------

(defn focus-input
  "An input that focuses itself when it appears"
  [text owner]
  (reify
    om/IDidMount
    (did-mount [this node]
               (.focus node))

    om/IRenderState
    (render-state [this {:keys [class]}]
            (let [handle-change (fn [e text owner] (om/transact! text (fn [_] (.. e -target -value))))
                  commit-change (fn [])]
              (dom/input #js {:onBlur commit-change
                              :onChange #(handle-change % text owner)
                              :value (om/value text)
                              :className class
                              :onKeyPress #(when (== (.-keyCode %) 13) (commit-change))})))))

;; -------------------------
;; Editable component
;; -------------------------

(defn editable
  "An editable piece of text, uses focus-input."
  [text owner {:keys [class] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
                {:editing false})

    om/IWillMount
    (will-mount [_]
                (comment (om/set-state! owner :editing false)))


    om/IRenderState
    (render-state [_ {:keys [editing] :as state}]
                  (letfn [(start-editing [e] (om/set-state! owner :editing true))]
                    (if editing
                      (om/build focus-input text {:opts {:class class}})
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

(defn vote-for-feature
  [feature owner]
  (let [bus (om/get-shared owner :bus)]
    (om/transact! feature :votes inc)
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
      (let [toggle-description (fn [] (om/set-state! owner :expanded (not expanded)))]
      (dom/li nil
       (dom/button #js {:className "pure-button button-small" :onClick #(vote-for-feature feature owner)} "Vote")
        (dom/span #js {:className "number-of-votes"} (:votes feature))
        (dom/div #js {:className "feature"}
                 (dom/i #js {:className (classes "expand fa " (if expanded "fa-caret-down" "fa-caret-right")) :onClick toggle-description})
                 (om/build editable (:title feature) {:opts {:class "title"}})
                 (when expanded (om/build editable (:description feature) {:opts {:class "description"}}))))))))

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
              feature {:title title :description description :votes 0 :id id}]
          (om/transact! app :features conj feature)
          (om/set-state! owner :title "")
          (om/set-state! owner :description "")
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
      {:title ""
       :description ""})

    om/IWillMount
    (will-mount [_]
      (let [bus (om/get-shared owner :bus)
            vote (chan)
            init (chan)]
        (sub bus :init init)
        (sub bus :vote vote)
        (go-loop []
              (when-let [{state :state} (<! init)]
                (om/transact! app :features (fn [features] (apply conj features state)))
                (recur)))

        (go-loop []
              (when-let [{feature :feature} (<! vote)]
                (om/transact! app :features (fn [features] (into [] (sort-by :votes (fn [a b] (> a b)) features))))
                (om/transact! app :votes-remaining dec)
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
                                       (dom/input #js {:type "text" :placeholder "feature title" :ref "featuretitle" :value (:title state) :onChange #(handle-change % owner :title)})
                                       (dom/input #js {:type "text" :placeholder "feature description" :ref "featuredescription" :value (:description state) :onChange #(handle-change % owner :description)})
                                       (dom/button #js {:className "pure-button button-small" :onClick  (fn [e] (.preventDefault e) (add-feature app owner))} "Add feature")))))))

;; -------------------------
;;    Build the app
;; -------------------------

(def app-state (atom {:features []
                      :votes-remaining 10}))

(go (let [ws (<! (ws-ch "ws://localhost:3000/ws"))
          bus (om-bus ws :message-type)]
      (om/root app-state {:bus bus}
               features-view (. js/document (getElementById "features")))))

