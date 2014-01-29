(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! chan <! >! <!! >!!]]
            [clojure.data :as data]
            [clojure.string :as string]))

(enable-console-print!)

;; -------------------------
;;     Messaging
;; -------------------------

(go (def ws (<! (ws-ch "ws://localhost:3000/ws"))))

(defn send-message [message]
  (go (>! ws message)))

;; -------------------------
;;       Helpers
;; -------------------------

(defn handle-change
  [e owner {:keys [text]}]
  (om/set-state! owner :text (.. e -target -value)))

(defn add-feature
  [app owner]
  (let [new-feature (-> (om/get-node owner "new-feature")
                        .-value)]
    (when new-feature
      (om/transact! app :features conj {:title new-feature :description "" :votes 0})
      (om/set-state! owner :text "")
      (send-message {:message-type :new-feature :feature new-feature}))))


(defn vote-for-feature
  [feature]
  (om/transact! feature :votes inc)
  (send-message {:message-type :vote :feature @feature}))

;; -------------------------
;;       Om Components
;; -------------------------

(defn feature-view
  "Create a react/om component that will display a single feature"
  [feature owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [sort?]}]
      (letfn [(toggle-description [] (om/transact! feature :show-description not))]
      (dom/li nil
        (dom/button #js {:className "pure-button button-small" :onClick #(do (vote-for-feature feature) (put! sort? true))} "Vote")
        (dom/span #js {:className "number-of-votes"} (:votes feature))
        (dom/div #js {:className "feature"}
                 (dom/i #js {:className (str "expand fa " (if (:show-description feature) "fa-caret-down" "fa-caret-right")) :onClick toggle-description})
                 (dom/span #js {:className "title" :onClick toggle-description} (:title feature))
                 (when (:show-description feature) (dom/span #js {:className "description"} (:description feature)))))))))

(defn features-view
  "Create a react/om component that will display and manage a sorted list of features, with
  a text box to add new features"
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:sort? (chan)
       :text ""})

    om/IWillMount
    (will-mount [_]
      (let [sort? (om/get-state owner :sort?)]
        (go (loop []
              (let [_ (<! sort?)]
                (om/transact! app :features
                   (fn [xs] (vec (sort-by :votes (fn [a b] (> a b)) xs))))
                (recur))))))

    om/IRenderState
    (render-state [this state]
      (dom/div nil
        (dom/h1 nil "Feature list")
        (apply dom/ul nil
          (om/build-all feature-view (:features app)
            {:init-state state}))
        (dom/div nil
                 (dom/input #js {:type "text" :ref "new-feature" :value (:text state) :onChange #(handle-change % owner state)})
                 (dom/button #js {:className "pure-button button-small" :onClick  #(add-feature app owner)} "Add feature"))))))

;; -------------------------
;;    Build the app
;; -------------------------
(def app-state (atom {:features
                      [{:title "Cable Sizes" :description "We like cable sizes" :votes 0}
                       {:title "Holiday" :description "We want better holiday support" :votes 0}]}))

(om/root app-state features-view (. js/document (getElementById "features")))

