(ns mug.util
  (:require [clj-http.client :as client]
            [clojure.string :as str])
  (:use     [mug.fixtures]) ;biomktcaps
  (:gen-class))

;--------------------------------------------------------;

(def keychain (eval (read-string (slurp "resources/keychain.clj"))))

(def ^:dynamic *showcost* (atom false))
(defn verbose [] (swap! *showcost* (fn [_] true)))
(defn concise [] (swap! *showcost* (fn [_] false)))

(defn zip-strings
  "joins corsponding elements from two vectors of strings" 
  [[x & xs] [y & ys]]
    (if xs
      (cons (str (str x) " " (str y)) (zip-strings xs ys))
      (list (str (str x) " " (str y)))))

(defn parsint [s]
  {:pre [(string? s)] :post [(integer? %)]} 
  (let [n (-> (str/split s #"\.") (first) (str/replace #"," "") (read-string))]
    (if (integer? n) n 0)))

(defn kfmt [x] (-> x (str)  (str/upper-case)  (keyword)))
(defn tfmt [x] (-> x (str)  (str/upper-case)  (symbol)))
(defn t=? [x y] (= (tfmt x) (tfmt y)))

(defn stonum 
  "convert string to number"
  [s]
  (read-string
    (str/replace s #"," "")))

(defn doseq-interval
  "use to set speed for processing array elements"
  [f coll interval]
  (doseq [x coll]
    (Thread/sleep interval)
    (f x)))

(defn in? 
  "true if coll contains elm"
  [coll elm]  
  (some #(= elm %) coll))

;--------------------------------------------------------;

(defn from-epoch
  "convert epoc-milliseconds to reader-safe date-string
   Sat_Mar_30_20_00_00_EDT_2019"
  [msec]
  (let [cal-obj (java.util.Calendar/getInstance)
              _ (.setTimeInMillis cal-obj msec)]
    (-> (.toString (.getTime cal-obj))
        (str/replace #"\s" "_")
        (str/replace #"\:" "_"))))

(defn year
  "docstring"
  [msec]
  (let [[_ _ _ _ _ _ _ y]
        (-> (from-epoch msec)
            (str/split #"_"))]
    y))

(defn month
  "docstring"
  [msec]
  (let [[_ m _ _ _ _ _ _]
        (-> (from-epoch msec)
            (str/split #"_"))]
    m))

(defn day
  "docstring"
  [msec]
  (let [[_ _ d _ _ _ _ _]
        (-> (from-epoch msec)
            (str/split #"_"))]
    d))

(defn dayofyear
  "docstring"
  [msec]
  (let [cal-obj (java.util.Calendar/getInstance)
              _ (.setTimeInMillis cal-obj msec)]
    (.get cal-obj java.util.Calendar/DAY_OF_YEAR)))

(defn datetime
  "docstring"
  [msec]
  (let [cal-obj (java.util.Calendar/getInstance)
              _ (.setTimeInMillis cal-obj msec)
        dat-obj (.getTime cal-obj)]
    (.toString dat-obj)))

;--------------------------------------------------------;

(defn raw-to-clojure
  "convert json to clojure data"
  {:updated "20190722"}
  [s]
  (let [
               clipped   (str/replace s #"^[A-Z]+?\s" "")
                 cmnul   (str/replace clipped #"null" "null,")
              s-commas   (str/replace cmnul #"[A-Za-z]\," #(subs % 0 1))
             no-quotes   (str/replace s-commas #"\"" "")
                no-inc   (str/replace no-quotes #", Inc" " Inc")
            sub-commas   (str/replace no-inc #"," "\n")
             no-spaces   (str/replace sub-commas #" " "_")
           sub-newline   (str/replace no-spaces #"\n" " ")
             no-colons   (str/replace sub-newline #":" " ")
            more-clean   (str/replace no-colons #"(\(|\)|\.\$\#)" "_")
        lot-more-clean   (str/replace more-clean #"\s\d+?(?=([A-Za-z_]))" " ")
         squeaky-clean   (str/replace lot-more-clean #"\d\-\d" #(str/replace % #"\-" ""))
                 shiny   (str/replace squeaky-clean #"[A-Za-z]\." #(subs % 0 1))
            as-clojure   (read-string shiny)
       ]
    as-clojure))
;    no-inc))

(defn raw-to-table
  "convert json to table"
  [s]
  (let [data  (fn [ss] 
                (->> (str/split ss #"\n")
                  (map #(str/split % #":"))
                  (map second)
                  (map #(str/replace % " " "_"))
                  (reduce #(str %1 " " %2))))]
    (->>
      (-> (->> (-> s
                 (str/replace "\"" "")
                 (str/replace "},{" "\n\n")
                 (str/replace "[{" "")
                 (str/replace "}]" "")
                 (str/split #" "))
            (rest)
            (reduce #(str %1 " " %2)))
        (str/replace "," "\n")
        (str/split #"\n\n"))
      (map data)
      (reduce #(str %1 "\n" %2)))))

;--------------------------------------------------------;

(defn xtract [page field]
  (if page
    (-> (->> field
          (re-pattern)
          (str/split page)
          (rest)
          (first))
        (str/split #",")
        (first)
        (str/split #"\"")
        (last))))