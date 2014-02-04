(defproject feature-list "0.1.0-SNAPSHOT"
  :description "A simple feature list, written using om"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.3.5"]
                 [com.datomic/datomic-free "0.9.4470"]
                 [jarohen/chord "0.2.2"]
                 [compojure "1.1.6"]
                 [ring/ring-core "1.2.0"]
                 [http-kit "2.1.16"]
                 [cljs-uuid "0.0.4"]
                 [com.facebook/react "0.8.0.1"]]

  :plugins [[lein-cljsbuild "1.0.1"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "feature-list"
              :source-paths ["src"]
              :compiler {
                :output-to "feature_list.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})
