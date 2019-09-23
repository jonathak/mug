(ns mug.alphav
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.test :as test])
  (:gen-class))

(def keychain (eval (read-string (slurp "resources/keychain.clj"))))

(def getp
  (let [hdr1 ""
        hdr2 ""
        key ""
        skey ""
        dataurl "https://www.alphavantage.co/query"]
    (fn
      ([ticker limit _]
        (let[
             q  (str "?function=TIME_SERIES_DAILY_ADJUSTED" "&symbol=" (str/upper-case (str ticker)))
             u  (str dataurl q "&apikey=" (:alphavantage keychain))
             s  (client/get u)
             s2 (str/replace s #"^.+RNN" "FARTS")
             s3 (str/replace s2 #"^FARTS.+\(Daily\)\\\":\s" "")
             s4 (str/replace s3 #"\\n\}\",\s:trace-redirects\s\[\]\}" "POOP")
             s5 (str/replace s4 #"POOP" "")
             s6 (-> s5 
                    (clojure.string/replace #"\\n" "") 
                    (clojure.string/replace #"\\\"" "") 
                    (clojure.string/replace #"\d\.\s" "")
                    (clojure.string/replace #"\s" "")
                    (clojure.string/replace #"\}," "} d")
                    (clojure.string/replace #"(:|,)" " ")
                    (clojure.string/replace #"^\{" "{d"))
             m  (read-string s6)
            ]
          s6))
      ([ticker limit]
        (let [bar "1D?"] ;choose from "1Min?" "5Min?" "15Min?" "1D?"
          (getp ticker limit bar)))
      ([ticker]
        (let [limit 1000] ;max is 1000
          (getp ticker limit))))))

(defn putfridge3
  "saves prices in a flat file for given ticker"
  ([t bar]
    (let [good (getp t 1000 bar)]
      (if (str/includes? good "\"o\"") ;testing validity of string
        (do 
          (spit (str 'resources "/" (str/upper-case (str t)) "_a_" bar ".txt") (str good "\n\n"))
          true)
        false)))
  ([t]
    (putfridge3 t "1D?")))