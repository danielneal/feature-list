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
(defn hydrate [q]
  (->> q
       (map first)
       (map (partial d/entity (d/db conn)))
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
       (hydrate q)
       (set/rename q {:feature/votes :votes
                      :feature/title :title
                      :feature/description :description
                      :feature/id :id})))
;; -------------------------------
;;  Message handling
;; -------------------------------

(defn process-add-feature [c]
  (go-loop []
           (when-let [{{title :title description :description id :id} :feature} (<! c)]
             (d/transact conn [{:feature/title title :feature/description description :feature/id id :feature/votes 0 :db/id (d/tempid :db.part/user)}])
             (recur))))

(defn process-vote [c]
  (go-loop []
           (when-let [{{id :id votes :votes :as message} :feature} (<! c)]
             (when-let [eid (ffirst (q '[:find ?e :in $ ?id :where [?e :feature/id ?id]] (d/db conn) id))]
               (d/transact conn [{:db/id eid :feature/votes votes}]))
             (recur))))

(defn process-id-requests [c server->client]
  (go-loop []
             (when-let [_ (<! c)]
               (put! server->client {:message-type :id :id (d/squuid)})
               (recur))))

;; -------------------------------
;; Web socket
;; -------------------------------

(defn ws-handler [req]
  (with-channel req ws
    (let [client->server (map< (comp clojure.edn/read-string :message) ws)
          server->client (map> pr-str ws)
          server-p (pub client->server :message-type)
          id-request (chan)
          add-feature (chan)
          vote (chan)]
      (println "Opened connection from" (:remote-addr req))
      (put! server->client {:message-type :init :state (features-all (d/db (d/connect uri)))})
      (sub server-p :request-id id-request)
      (sub server-p :add-feature add-feature)
      (sub server-p :vote vote)
      (process-id-requests id-request server->client)
      (process-add-feature add-feature)
      (process-vote vote))))

;; -------------------------------
;; Routes
;; -------------------------------
(defroutes app-routes
  (GET "/ws" [] ws-handler)
  (files "/" {:root nil}))

(def webapp
  (-> app-routes
      api))

(def server (atom nil))

(defn start-server []
  (reset! server (run-server #'webapp {:port 3000})))

(defn stop-server []
  (@server))

