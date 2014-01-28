(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! chan <! >!]]
            [clojure.data :as data]
            [clojure.string :as string]))

(enable-console-print!)

;; -------------------------
;; Communication to server
;; -------------------------

(go (def ws (<! (ws-ch "ws://localhost:3000/ws"))))

;; -------------------------
;;  Application state model
;; -------------------------
(def app-state
  (atom
   {:ws ws
    :features
    [{:title "Cable Sizes" :description "We like cable sizes" :votes 0}
     {:title "Holiday" :description "We want better holiday support" :votes 0}]}))

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
      (go (>! (:ws @app) {:message-type :new-feature :feature new-feature})))))

(defn vote-for-feature
  [feature owner]
  (let [vote (om/get-state owner :vote)]
    (om/transact! feature :votes inc)
    (put! vote @feature)))

;; -------------------------
;;       Om Components
;; -------------------------

(defn feature-view
  "Create a react/om component that will display a single feature"
  [feature owner]
  (reify

    om/IRenderState
    (render-state [this {:keys [vote]}]
      (dom/li nil
        (dom/span nil (:title feature))
        (dom/span nil (:votes feature))
        (dom/button #js {:onClick #(vote-for-feature feature owner)} "Vote")))))

(defn features-view
  "Create a react/om component that will display and manage a sorted list of features, with
  a text box to add new features"
  [app owner]
  (reify

    om/IInitState
    (init-state [_]
      {:vote (chan)
       :text ""})

    om/IWillMount
    (will-mount [_]
      (let [vote (om/get-state owner :vote)]
        (go (loop []
              (let [feature (<! vote)]
                (om/transact! app :features
                   (fn [xs] (vec (sort-by :votes (fn [a b] (> a b)) xs))))
                (>! (:ws @app) {:message-type :vote :feature feature})
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
                 (dom/button #js {:onClick  #(add-feature app owner)} "Add feature"))))))

;; -------------------------
;;    Build the app
;; -------------------------
(om/root app-state features-view (. js/document (getElementById "features")))
