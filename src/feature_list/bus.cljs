(ns feature-list.bus
  (:require [cljs.core.async :as async :refer [chan pipe pub map> map<]]
            [cljs.core.async.impl.protocols :as impl]))

(defn bus
    "Create a client side 'bus' for Om from a Chord web socket.

    This 'bus' can be subscribed to like a pub, but can also be
    written to (using put! or >!). When written to, values are
    relayed back to the bus locally *and* are transmitted down
    the web socket.

    bus    local
    -----x------------->|                x---->
         |              | merged   pub   x---->
         |               ----------> x --x---->  subs
         |      ws      |                x---->
         |   x--------->|                x---->
         |   | (in)
         |   |
         |   x------------------x
         |                      |
         |                      |
         | ws                   |
         x-----------------> server
          (out)

    Accepts optional transforms to apply to inbound and outbound
    messages."

    [ws topic-fn & {outbound-transform :outbound-transform
                    inbound-transform :inbound-transform}]
    (let [local (chan)
          merged (chan)
          published (pub merged topic-fn)
          out (if-not outbound-transform ws (map> outbound-transform ws))
          in (if-not inbound-transform ws (map< inbound-transform ws))]

      (pipe local merged)
      (pipe in merged)

      (reify
        async/Pub
        (sub* [p v ch close?] (async/sub* published v ch close?))
        (unsub* [p v ch] (async/unsub* published v ch))
        (unsub-all* [p] (async/unsub-all* published))
        (unsub-all* [p v] (async/unsub-all* published v))

        impl/WritePort
        (put! [_ val fn0]
              (impl/put! out val fn0)
              (impl/put! local val fn0)))))
