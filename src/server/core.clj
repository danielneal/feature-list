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
(def conn (d/connect uri))
(d/transact conn (-> "schema.edn" slurp read-string))

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

(defn features-all []
  (as-> (q '[:find ?e
            :in $
            :where [?e :feature/title ?f]] (d/db conn)) q
       (hydrate q)
       (set/rename q {:feature/votes :votes
                      :feature/title :title
                      :feature/description :description
                      :feature/id :id})))

;; -------------------------------
;;     Message handling
;; -------------------------------
(defmulti process-message :message-type)

(defmethod process-message :add-feature [message]
  (println "New feature"))

(defmethod process-message :vote [feature]
  (println "Vote for feature"))

(defmethod process-message :default [msg]
  (println msg))

;; -------------------------------
;;      Web socket
;; -------------------------------

(defn ws-handler [req]
  (with-channel req ws
    (println "Opened connection from" (:remote-addr req))
    (go (>! ws (pr-str {:message-type :init :state (features-all)})))
    (go-loop []
      (when-let [{:keys [message]} (<! ws)]
        (println "Message received:" message)
        (process-message (clojure.edn/read-string message))
        (recur)))))

;; -------------------------------
;;     routes
;; -------------------------------
(defroutes app-routes
  (GET "/ws" [] ws-handler)
  (files "/" {:root nil}))

(def webapp
  (-> app-routes
      api))

(def stop-server (run-server #'webapp {:port 3000}))



