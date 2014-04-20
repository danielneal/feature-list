(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [cljs.core.match.macros :refer [match]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [put! map< map> chan pub sub unsub <! >!]]
            [clojure.data :as data]
            [cljs.core.match :as core-match]
            [clojure.string :as string]
            [cljs.reader :as reader]
            [cljs-uuid.core :as uuid]
            [clojure.walk :as walk]
            [taoensso.encore :as encore :refer (logf)]
            [taoensso.sente :as sente :refer [cb-success?]]))

(enable-console-print!)

;; ----------------------------
;; Set up connection to server
;; ----------------------------

(let [{:keys [chsk ch-recv send-fn]}
      (sente/make-channel-socket! "/chsk" {} {:type :auto})]
  (def chsk chsk)
  (def ch-chsk ch-recv)
  (def chsk-send! send-fn))

;; -------------------------
;; Helpers
;; -------------------------

(defn classes
  "Combine CSS classes into a string"
  [& classes]
  (apply str (interpose " " classes)))

;; -------------------------
;; Editable component
;; -------------------------

(defn editable
  "Edit the value in the map cursor specified by the key `k` in the opts.
   On commit will update the state and tag the transaction with the given tag,
  the key and the new value"
  [m owner {:keys [k class tag-fn] :as opts}]
  (let [start-editing (fn [e] (om/set-state! owner :editing true))
        handle-change (fn [e] (om/set-state! owner :text (.. e -target -value)))
        commit-change (fn []
                        (let [text (om/get-state owner :text)]
                          (om/set-state! owner :editing false)
                          (om/transact! m nil #(assoc % k text) (tag-fn k text))))]
  (reify
    om/IInitState
    (init-state [_]
      {:editing false
       :text (k m)})

    om/IDidUpdate
    (did-update [_ prev-props prev-state]
      (when (om/get-state owner :editing)
        (let [node (om/get-node owner "editableinput")]
          (.focus node))))

    om/IRenderState
    (render-state [_ {:keys [editing text] :as state}]
      (if editing
        (dom/input #js {:onBlur commit-change
                        :onChange #(handle-change %)
                        :value text
                        :className class
                        :ref "editableinput"
                        :onKeyPress #(when (== (.-keyCode %) 13) (commit-change))})
        (dom/span #js {:onClick #(start-editing %)
                       :className (classes "clickable" class)}
                  text))))))

;; -------------------------
;; Feature (single)
;; -------------------------

(defn vote-for-feature [feature]
  "Increment the number of votes for the given feature"
  (om/transact! feature :feature/votes inc))

(defn feature-view
  "Create a react/om component that will display a single feature"
  [feature owner {:keys [on-delete] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      {:expanded false
       :on-delete on-delete})

    om/IRenderState
    (render-state [this {:keys [expanded delete-chan]}]
      (let [toggle-description (fn [] (om/set-state! owner :expanded (not expanded)))]
        (dom/li nil
                (dom/i #js {:className (classes "fa" "fa-times-circle") :onClick #(on-delete feature)})
                (dom/button #js {:className "pure-button button-small" :onClick #(vote-for-feature feature)} "Vote")
                (dom/span #js {:className "number-of-votes"} (:feature/votes feature))
                (dom/div #js {:className "feature"}
                         (dom/i #js {:className (classes "expand" "fa" (if expanded "fa-caret-down" "fa-caret-right")) :onClick toggle-description})
                         (om/build editable feature {:opts {:k :feature/title :class "title" :tag-fn (partial conj [:feature-list/update-feature (:feature/id feature)])}})
                         (when expanded (om/build editable feature {:opts {:k :feature/description :class "description" :tag-fn (partial conj [:feature-list/update-feature (:feature/id feature)])}}))))))))

;; -------------------------
;; Feature list
;; -------------------------

(defn add-feature
  "Add a new feature to the list"
  [features owner]
  (let [title (.-value (om/get-node owner "featuretitle"))
        description (.-value (om/get-node owner "featuredescription"))
        feature {:feature/title title :feature/description description :feature/votes 0}]
    (om/set-state! owner :feature/title "")
    (om/set-state! owner :feature/description "")
    (om/transact! features nil #(conj % feature) [:feature-list/add-feature feature])))

(defn delete-feature
  "Remove a feature from the list"
  [features feature]
  (om/transact! features nil (fn [fs] (into [] (remove #(= @feature %) fs))) [:feature-list/delete-feature @feature]))

(defn features-view
  "A component to display and manage a sorted list of features, with a text box to add new features"
  [features owner]
  (reify

    om/IDidMount
    (did-mount [_]
      (chsk-send! [:feature-list/init {}] 8000
                  #(om/update! features %)))

    om/IRenderState
    (render-state [this state]
      (letfn [(handle-change [e owner k] (om/set-state! owner k (.. e -target -value)))]
        (dom/div #js {:className "page-container"}
                 (dom/h1 nil "Feature list")
                 (apply dom/ul nil
                        (om/build-all feature-view features {:opts {:on-delete #(delete-feature features %)}}))
                 (dom/form #js {:className "pure-form"}
                           (dom/input #js {:type "text" :placeholder "feature title" :ref "featuretitle" :value (:feature/title state) :onChange #(handle-change % owner :feature/title)})
                           (dom/input #js {:type "text" :placeholder "feature description" :ref "featuredescription" :value (:feature/description state) :onChange #(handle-change % owner :feature/description)})
                           (dom/button #js {:className "pure-button button-small" :onClick  (fn [e] (.preventDefault e) (add-feature features owner))} "Add feature")))))))

;; -------------------------
;;  Build App
;; -------------------------

(def app-state (atom {}))

(defn build-app []
  (om/root features-view app-state
           {:target (. js/document (getElementById "features"))
            :tx-listen (fn [tx-data cursor]
                         (match [tx-data cursor]
                                [{:tag [:feature-list/update-feature id k v]} _]
                                (chsk-send! [:feature-list/update-feature [id k v]] 8000 println)

                                [{:tag [:feature-list/add-feature feature]} _]
                                (chsk-send! [:feature-list/add-feature feature] 8000 println)

                                :else (println "transaction not matched")))}))

(defn event-handler [[id data :as ev] _]
  (match [id data]
         [:chsk/recv [:my-app/alert-from-server payload]]
         (do (println "Pushed payload received from server!: %s" payload))

         [:chsk/state [:first-open _]]
         (do
           (logf "Channel socket successfully established!")
           (build-app))

         [:chsk/state new-state] (logf "Chsk state change: %s" new-state)
         [:chsk/recv  payload] (logf "From server: %s" payload)

         :else (println "Unmatched <!: %s" id)))

(let [ch-chsk ch-chsk
      ch-ui (chan)
      ch-merged (async/merge [ch-chsk ch-ui])]
  (sente/start-chsk-router-loop! event-handler ch-merged))


