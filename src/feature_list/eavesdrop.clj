(ns feature-list.eavesdrop
  (:require [clojure.template :refer [apply-template]]))

(defmacro eavesdrop
  "Eavesdrop a mult. Creates a channel which taps a mult and executes the body with the new channel shadows the mult."
  [m & body]
  (let [ch (gensym)
        new-body (apply-template [m] body [ch])]
    `(let [~ch (cljs.core.async/chan)]
       (cljs.core.async/tap ~m ~ch)
       ~@new-body)))
