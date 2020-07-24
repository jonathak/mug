(ns mug.cli
  (:require [mug.core       :refer :all ]
            [incanter.core  :as i]
            [incanter.charts  :as c]
            [mug.cli.state  :refer :all ]
            [mug.cli.help   :as hlp]
            [mug.app        :as app]
            [mug.util       :as util]
            [mug.iex        :as iex]
            [mug.alphav     :as alphav]
            [mug.sec        :as sec]
            [clojure.string :as str]
            [clojure.java.browse :refer [browse-url]])
  (:gen-class))

(declare top)
(declare bag)
(declare sku)
(declare universe)
(declare pair)

(load "cli/common")
(load "cli/pair")
(load "cli/bag")
(load "cli/universe")
(load "cli/top")
(load "cli/sku")

(defn -main
" a command line interface to extensive uniquely summarized
 public financial equity data mixed with analytical tools.
 requires individual API acces to multiple financial data
 providors including iexcloud.io alpaca.markets and
 alphavantage.co."
  [& args]
;  (if (> (.getTime (java.util.Date.)) (+ 1568586302728 3592000000))
   (if false
      "
contact kaufman. something expired.
"
      (do (println "\n\n")
          (println "
Welcome to Mug!  
******************

(For help, enter .h (including the dot) at the prompt >)\n") (top))))

(defn top []
  (swap! *from* (fn [_] top))
  (if-let [cmd (do (print (str "top" "> ")) (flush) (read-line))]
    (do (top-case cmd))))

(defn sku [t]
  (if-let [cmd (do (print (str "." @*name* "> ")) (flush) (read-line))]
    (do (sku-case cmd t))))

(defn bag []
  (swap! *from* (fn [_] bag))
  (if-let [cmd (do (print (str ":" @*name* "> ")) (flush) (read-line))]
    (do (bag-case cmd))))

(defn universe []
  (swap! *from* (fn [_] universe))
  (if-let [cmd     (do (print (str "universe" "> ")) (flush) (read-line))]
      (do (universe-case cmd))))

(defn pair []
  (swap! *from* (fn [_] pair))
  (let [prompt (fn [[x y]] (str ":" x "-" y "> "))]
    (if-let [cmd (do (print (prompt @*pair*)) (flush) (read-line))]
      (do (pair-case cmd)))))