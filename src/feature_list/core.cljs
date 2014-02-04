(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! map< map> go-loop chan pub sub <! >!]]
            [cljs.reader :as reader]
            [clojure.data :as data]
            [clojure.string :as string]))

(enable-console-print!)

;; -------------------------
;;       Commands
;; -------------------------

(defn add-feature
  [app owner]
  (go (let [id (om/get-shared owner :id)
           client->server (om/get-shared owner :client->server)
           title (.-value (om/get-node owner "featuretitle"))
            description (.-value (om/get-node owner "featuredescription"))
            _ (put! client->server {:message-type :request-id})
            {id :id} (<! id)
            feature {:title title :description description :votes 0 :id id}]
       (om/transact! app :features conj feature)
       (om/set-state! owner :title "")
       (om/set-state! owner :description "")
       (put! client->server {:message-type :add-feature :feature feature}))))

(defn vote-for-feature
  [feature owner]
  (let [client->server (om/get-shared owner :client->server)]
    (om/transact! feature :votes inc)
    (put! client->server {:message-type :vote :feature @feature})))

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

(defn feature-view
  "Create a react/om component that will display a single feature"
  [feature owner]
  (reify
    om/IInitState
    (init-state [_]
                {:expanded false})

    om/IRenderState
    (render-state [this {:keys [expanded]}]
      (let [toggle-description (fn [] (om/set-state! owner :expanded (not expanded)))]
      (dom/li nil
       (dom/button #js {:className "pure-button button-small" :onClick #(vote-for-feature feature owner)} "Vote")
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
      {:title ""
       :description ""})

    om/IWillMount
    (will-mount [_]
      (let [init (om/get-shared owner :init)]
        (go (loop []
              (when-let [{state :state} (<! init)]
                (om/transact! app :features (fn [features] (apply conj features state)))
                (recur))))))

    om/IRenderState
    (render-state [this state]
                  (letfn [(handle-change [e owner k] (om/set-state! owner k (.. e -target -value)))]
                    (dom/div #js {:className "page-container"}
                             (dom/h1 nil "Feature list")
                             (apply dom/ul nil
                                    (om/build-all feature-view (:features app) {:init-state state}))
                             (om/build votes-view (:votes app))
                             (dom/form #js {:className "pure-form"}
                                       (dom/input #js {:type "text" :placeholder "feature title" :ref "featuretitle" :value (:title state) :onChange #(handle-change % owner :title)})
                                       (dom/input #js {:type "text" :placeholder "feature description" :ref "featuredescription" :value (:description state) :onChange #(handle-change % owner :description)})
                                       (dom/button #js {:className "pure-button button-small" :onClick  (fn [e] (.preventDefault e) (add-feature app owner))} "Add feature")))))))

;; -------------------------
;;    Build the app
;; -------------------------
(def app-state (atom {:features []}))

(go (let [ws (<! (ws-ch "ws://localhost:3000/ws"))
          client->server (map> pr-str ws)
          server->client (map< (comp reader/read-string :message) ws)
          client-p (pub server->client :message-type)
          init (chan)
          id (chan)]
      (sub client-p :init init)
      (sub client-p :id id)
      (om/root app-state {:init init
                          :id id
                          :client->server client->server}
               features-view (. js/document (getElementById "features")))))

