(ns server.core
  (:require
    [ring.util.response :refer [response]]
    [org.httpkit.server :refer [run-server]]
    [chord.http-kit :refer [with-channel]]
    [clojure.core.async :refer [<! >! put! close! go-loop]]
    [compojure.core :refer [defroutes GET routes]]
    [compojure.handler :refer [api]]
    [compojure.route :refer [resources]]))

(defn ws-handler [req]
  (with-channel req ws
    (println "Opened connection from" (:remote-addr req))
    (go-loop []
      (when-let [{:keys [message]} (<! ws)]
        (println "Message received:" message)
        (recur)))))

(defroutes app-routes
  (GET "/" [] "Hello World!")
  (GET "/biscuit" [] "Biscuits are great")
  (GET "/cheese" [] "Chees is awesome")
  (GET "/ws" [] ws-handler)
  (resources "/js" {:root "js"})
  (resources "/out" {:root "out"}))

(def webapp
  (-> app-routes
      api))

(def stop-server (run-server #'webapp {:port 3000}))

