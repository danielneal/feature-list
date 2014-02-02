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

(defn send-message [ws msg]
  (go (>! ws (pr-str msg))))

;; -------------------------
;;       Commands
;; -------------------------

(defn add-feature
  [app owner]
  (let [title (.-value (om/get-node owner "featuretitle"))
        description (.-value (om/get-node owner "featuredescription"))]
    (om/transact! app :features conj {:title title :description description :votes 0})
    (send-message {:message-type :add-feature :feature {:title title :description description :votes 0}})
    (om/set-state! owner :title "")
    (om/set-state! owner :description "")))

(defn vote-for-feature
  [feature]
  (send-message {:message-type :vote :feature @feature})
  (om/transact! feature :votes inc))

(defn handle-change
  [e owner k]
  (om/set-state! owner k (.. e -target -value)))

;; -------------------------
;;       Om Components
;; -------------------------

(defn feature-view
  "Create a react/om component that will display a single feature"
  [feature owner]
  (reify
    om/IInitState
    (init-state [_]
                {:expanded false})
    om/IRenderState
    (render-state [this {:keys [vote expanded]}]
      (letfn [(toggle-description [] (om/set-state! owner :expanded (not expanded)))]
      (dom/li nil
        (dom/button #js {:className "pure-button button-small" :onClick #(do (vote-for-feature feature owner) (put! vote feature))} "Vote")
        (dom/span #js {:className "number-of-votes"} (:votes feature))
        (dom/div #js {:className "feature"}
                 (dom/i #js {:className (str "expand fa " (if expanded "fa-caret-down" "fa-caret-right")) :onClick toggle-description})
                 (dom/span #js {:className "title" :onClick toggle-description} (:title feature))
                 (when expanded (dom/span #js {:className "description"} (:description feature)))))))))

(defn features-view
  "Create a react/om component that will display and manage a sorted list of features, with
  a text box to add new features"
  [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:vote (chan)
       :title ""
       :description ""})

    om/IWillMount
    (will-mount [_]
      (let [vote (om/get-state owner :vote)
        (go (loop []
              (let [_ (<! vote)]
                (om/transact! app :features
                   (fn [xs] (vec (sort-by :votes (fn [a b] (> a b)) xs))))
                (recur))))))

    om/IRenderState
    (render-state [this state]
      (dom/div #js {:className "page-container"}
        (dom/h1 nil "Feature list")
        (apply dom/ul nil
          (om/build-all feature-view (:features app)
            {:init-state state}))
        (dom/form #js {:className "pure-form"}
                  (dom/input #js {:type "text" :placeholder "feature title" :ref "featuretitle" :value (:title state) :onChange #(handle-change % owner :title)})
                  (dom/input #js {:type "text" :placeholder "feature description" :ref "featuredescription" :value (:description state) :onChange #(handle-change % owner :description)})
                  (dom/button #js {:className "pure-button button-small" :onClick  (fn [e] (.preventDefault e) (add-feature app owner))} "Add feature"))))))

;; -------------------------
;;    Build the app
;; -------------------------
(def app-state (atom {:features []}))

(om/root app-state features-view (. js/document (getElementById "features")))

