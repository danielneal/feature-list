(ns server.core
  (:require
    [ring.util.response :refer [response]]
    [org.httpkit.server :refer [run-server]]
    [chord.http-kit :refer [with-channel]]
    [clojure.core.async :refer [<! >! put! close! go-loop]]
    [compojure.core :refer [defroutes GET routes]]
    [compojure.handler :refer [api]]
    [compojure.route :refer [resources files]]
    [datomic.api :as d :refer [q]]))

(defn ws-handler [req]
  (with-channel req ws
    (println "Opened connection from" (:remote-addr req))
    (go-loop []
      (when-let [{:keys [message]} (<! ws)]
        (println "Message received:" message)
        (recur)))))

(defroutes app-routes
  (GET "/ws" [] ws-handler)
  (files "/" {:root nil}))

(def webapp
  (-> app-routes
      api))

(def stop-server (run-server #'webapp {:port 3001}))



