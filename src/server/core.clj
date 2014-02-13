(ns server.core
  (:require
   [ring.util.response :refer [response]]
   [org.httpkit.server :refer [run-server]]
   [chord.http-kit :refer [with-channel]]
   [clojure.core.async :as async :refer [<! >! map> map< chan pub sub put! go close! go-loop]]
   [compojure.core :refer [defroutes GET routes]]
   [compojure.handler :refer [api]]
   [compojure.route :refer [resources files]]
   [datomic.api :as d :refer [q]]
   [clojure.set :as set]))

;; -------------------------------
;;  Database Setup
;; -------------------------------

(def uri "datomic:mem://feature-list")
(d/create-database uri)
(let [conn (d/connect uri)]
  (d/transact conn (-> "schema.edn" slurp read-string)))

;; -------------------------------
;;  Database helpers
;; -------------------------------

(defn hydrate [q db]
  (->> q
       (map first)
       (map (partial d/entity db))
       (map d/touch)
       (map (partial into {}))
       (into #{})))

;; -------------------------------
;;  Queries
;; -------------------------------

(defn features-all [db]
  (as-> (q '[:find ?e
            :in $
            :where [?e :feature/title ?f]] db) q
       (hydrate q db)
       (sort-by :feature/votes q)
        (reverse q)))

;; -------------------------------
;;  Message handling
;; -------------------------------

(defmulti process-message (fn [m ch] (:message-type m)))

(defmethod process-message :add-feature [m ch]
  (let [conn (d/connect uri)
        {{title :feature/title description :feature/description id :feature/id} :feature} m]
    (println m)
    (d/transact conn [{:feature/title title :feature/description description :feature/id id :feature/votes 0 :db/id (d/tempid :db.part/user)}])))

(defmethod process-message :update-feature [m ch]
  (let [conn (d/connect uri)
        {feature :feature} m]
    (println m)
    (d/transact conn [(merge feature {:db/id (d/tempid :db.part/user)})])))

(defmethod process-message :vote [m ch]
  (let [conn (d/connect uri)
        db (d/db conn)
        {{id :feature/id votes :feature/votes :as message} :feature} m]
    (println m)
    (d/transact conn [{:feature/id id :feature/votes (inc votes) :db/id (d/tempid :db.part/user)}])))

(defmethod process-message :request-id [m ch]
  (println m)
  (put! ch {:message-type :id :id (d/squuid)}))

(defmethod process-message :request-features [m ch]
  (let [conn (d/connect uri)
        db (d/db conn)]
    (println m)
    (put! ch {:message-type :init :features (features-all db)})))

(defmethod process-message :default [m ch]
  (println "Message type " (:message-type m) " "))


;; -------------------------------
;; Web socket
;; -------------------------------

(def state (atom {}))

(defn ws-handler [client-id req]
  (let [{aggregator-mix :aggregator-mix
         aggregator-mult :aggregator-mult}@state]
    (with-channel req ws
      (let [inbound (map< (comp clojure.edn/read-string :message) ws)
            outbound (map> pr-str ws)
            ch (chan)
            [process relay] (async/split #(= (:client-id %) client-id) ch)]

        (println "Opened connection from " (:remote-addr req) ", client-id " client-id)

        (async/admix aggregator-mix inbound)
        (async/tap aggregator-mult ch)
        (async/pipe relay outbound)

        (go-loop []
                 (when-let [m (<! process)]
                   (process-message m outbound)
                   (recur)))))))

;; -------------------------------
;; Routes
;; -------------------------------

(defroutes app-routes
  (GET "/ws/:client-id" [client-id :as req] (ws-handler client-id req))
  (files "/" {:root nil}))

(def webapp
  (-> app-routes
      api))


(defn start []
  (let [aggregator (chan)]
    (reset! state {:stop-server (run-server #'webapp {:port 3000})
                   :aggregator-mix (async/mix aggregator)
                   :aggregator-mult (async/mult aggregator)})))

(defn stop []
  (let [{stop-server :stop-server} @state]
    (stop-server)
    (reset! state {})))


