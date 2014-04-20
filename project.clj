(defproject feature-list "0.1.0-SNAPSHOT"
  :description "A simple feature list, written using om"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [om "0.6.0"]
                 [com.datomic/datomic-free "0.9.4724"]
                 [com.taoensso/sente "0.8.2"]
                 [com.taoensso/timbre "3.1.1"]
                 [com.taoensso/encore "0.9.5"]
                 [compojure "1.1.6"]
                 [org.clojure/core.match "0.2.1"]
                 [ring/ring-core "1.2.0"]
                 [http-kit "2.1.16"]
                 [cljs-uuid "0.0.4"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "feature-list"
              :source-paths ["src/feature_list"]
              :compiler {:output-to "feature_list.js"
                         :output-dir "out"
                         :optimizations :none
                         :source-map true}}]})
