(ns server.core
  (:require
   [ring.util.response :refer [response]]
   [org.httpkit.server :refer [run-server]]
   [clojure.core.async :as async :refer [<! >! map> map< chan pub sub put! go close! go-loop]]
   [taoensso.sente :as sente]
   [clojure.core.match :refer [match]]
   [compojure.core :refer [defroutes GET POST routes]]
   [compojure.handler :refer [api]]
   [compojure.route :refer [resources files]]
   [datomic.api :as d :refer [q]]
   [clojure.set :as set]))

;; -------------------------------
;;  Database Setup
;; -------------------------------

(def uri "datomic:mem://feature-list")
(d/create-database uri)
(def conn (d/connect uri))
(d/transact conn (-> "schema.edn" slurp read-string))

;; -------------------------------
;;  Queries
;; -------------------------------

(defn features-all [db]
  (->> (q '[:find ?e
            :in $
            :where [?e :feature/id ?f]] db)
       (map #(d/touch (d/entity db (first %))))
       (sort-by :feature/votes)
       (reverse)
       (into [])))

;; -------------------------------
;; Web socket and event handling
;; -------------------------------

(let [{:keys [ch-recv send-fn ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket! {})]
  (def ring-ajax-post                ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk                       ch-recv)
  (def chsk-send!                    send-fn))

(defn event-msg-handler
  [{:as ev-msg :keys [ring-req event ?reply-fn]} _]
  (let [session (:session ring-req)
        uid     (:uid session)
        [ev-id ev-data :as ev] event
        db (d/db conn)]
    (match [ev-id ev-data]

           [:feature-list/init _]
           (do
             (println "Init")
             (?reply-fn (features-all db)))

           [:feature-list/add-feature {:feature/title title :feature/description description}]
           (let [id (d/squuid)
                 feature {:feature/title title
                          :feature/description description
                          :feature/id id
                          :feature/votes 0}]
             (println "Add")
             (d/transact conn [(assoc feature :db/id (d/tempid :db.part/user))])
             (?reply-fn feature))

           [:feature-list/vote [id votes]]
           (let [new-votes (inc votes)]
             (println "vote")
             (d/transact conn [{:db/id [:feature/id id] :feature/votes new-votes }])
             (?reply-fn new-votes))

           [:feature-list/update-feature [id k v]]
           (do
             (println "update")
             (println id k v)
             (d/transact conn [{:db/id [:feature/id id] k v}]))

           :else
           (do
             (when-not (:dummy-reply-fn? (meta ?reply-fn))
               (?reply-fn (format "Unmatched event, echo: %s" ev)))))))

(sente/start-chsk-router-loop! event-msg-handler ch-chsk)

;; -------------------------------
;; Routes
;; -------------------------------

(defroutes app-routes
  (GET  "/chsk" req (#'ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (#'ring-ajax-post req))
  (files "/" {:root nil}))

(def webapp
  (-> app-routes
      api))

(def server (atom {}))

(defn start []
  (reset! server (run-server #'webapp {:port 3000})))

(defn stop []
  (@server)
  (reset! server {}))



