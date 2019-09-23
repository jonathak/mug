(ns mug.cli.state
  (:gen-class))

(def ^:dynamic *inventory*   (atom []))
(def ^:dynamic *name*        (atom ""))
(def ^:dynamic *priorname*   (atom ""))
(def ^:dynamic *fridge*      (atom {}))
(def ^:dynamic *from*        (atom (fn [])))
(def ^:dynamic *bag-buffer*  (atom []))
(def ^:dynamic *subset*      (atom []))
(def ^:dynamic *universe*    (atom []))
(def ^:dynamic *history*     (atom []))
(def ^:dynamic *index*       (atom 0))
(def ^:dynamic *pair*        (atom []))
(def ^:dynamic *cooler*      (atom []))

(defn quitt [] 
  (do (swap! *from* (fn [_] (fn [] 0)))
      (println "Thanks for using Mug.\nGoodbye.")))