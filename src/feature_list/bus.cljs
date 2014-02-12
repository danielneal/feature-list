(ns feature-list.bus
  (:require [cljs.core.async :as async :refer [chan pipe pub map> map<]]
            [cljs.core.async.impl.protocols :as impl]))

(defn bus

    [ws topic-fn & {outbound-transform :outbound-transform
                    inbound-transform :inbound-transform}]
  (let [out (if-not outbound-transform ws (map> outbound-transform ws))
        in (if-not inbound-transform ws (map< inbound-transform ws))
        published (pub in topic-fn)]

      (reify
        async/Pub
        (sub* [p v ch close?] (async/sub* published v ch close?))
        (unsub* [p v ch] (async/unsub* published v ch))
        (unsub-all* [p] (async/unsub-all* published))
        (unsub-all* [p v] (async/unsub-all* published v))

        impl/WritePort
        (put! [_ val fn0]
              (impl/put! out val fn0)))))
