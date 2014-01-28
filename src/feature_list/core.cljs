(ns feature-list.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [put! chan <! >!]]
            [clojure.data :as data]
            [clojure.string :as string]))

(enable-console-print!)


(defn middle-name [{:keys [middle middle-initial]}]
  (cond
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn parse-contact [contact-str]
  (let [[first middle last :as parts] (string/split contact-str #"\s+")
        [first last middle] (if (nil? last) [first middle] [first last middle])
        middle (when middle (string/replace middle "." ""))
        c (if middle (count middle) 0)]
    (when (>= (reduce + (map #(if % 1 0) parts)) 2)
      (cond-> {:first first :last last}
        (== c 1) (assoc :middle-initial middle)
        (>= c 2) (assoc :middle middle)))))

(defn handle-change [e owner {:keys [text]}]
  (let [value (.. e -target -value)]
    (if-not (re-find #"[0-9]" value)
      (om/set-state! owner :text value)
      (om/set-state! owner :text text))))

(defn add-contact [app owner]
     (let [new-contact (-> (om/get-node owner "new-contact")
                           .-value
                           parse-contact)
           ]
       (when new-contact
         (om/transact! app :contacts conj new-contact)
         (go (>! (:ws @app) {:message "boom"})))))

(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
        (dom/span nil (display-name contact))
        (dom/button #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))

(defn contacts-view [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:delete (chan)
       :text ""})
    om/IWillMount
    (will-mount [_]
      (let [delete (om/get-state owner :delete)]
        (go (loop []
              (let [contact (<! delete)]
                (om/transact! app :contacts
                  (fn [xs] (vec (remove #(= contact %) xs))))
                (recur))))))
    om/IRenderState
    (render-state [this state]
      (dom/div nil
        (dom/h1 nil "Contact list")
        (apply dom/ul nil
          (om/build-all contact-view (:contacts app)
            {:init-state state}))
        (dom/div nil
                 (dom/input #js {:type "text" :ref "new-contact" :value (:text state) :onChange #(handle-change % owner state)})
                 (dom/button #js {:onClick  #(add-contact app owner)} "Add contact"))))))

(go (let [ws (<! (ws-ch "ws://localhost:3000/ws"))
          app-state
          (atom
           {:ws ws
            :contacts
            [{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
             {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
             {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
             {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
             {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
             {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}]})]

      (om/root app-state contacts-view (. js/document (getElementById "contacts")))))
