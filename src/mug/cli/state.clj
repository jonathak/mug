(ns mug.cli.state
  (:gen-class))

(def ^:dynamic *inventory*   (atom []))
(def ^:dynamic *name*        (atom "top"))
(def ^:dynamic *priorname*   (atom "top"))
(def ^:dynamic *fridge*      (atom {}))
(def ^:dynamic *from*        (atom (fn [] (fn [] true))))
(def ^:dynamic *bag-buffer*  (atom []))
(def ^:dynamic *subset*      (atom []))
(def ^:dynamic *universe*    (atom []))
;(def ^:dynamic *history*     (atom []))
(def ^:dynamic *index*       (atom 0))
(def ^:dynamic *pair*        (atom ['ibm 'mrk]))
(def ^:dynamic *cooler*      (atom []))

(defn quitt [] 
  (do (swap! *from* (fn [_] (fn [] true)))
      (println "Thanks for using Mug.")
      'Goodbye))