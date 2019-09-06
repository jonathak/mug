(ns alpaca.core
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.test :as test])
  (:gen-class))

(def keychain (eval (read-string (slurp "resources/keychain.clj"))))

(def getp
  (let [hdr1 "APCA-API-KEY-ID"
        hdr2 "APCA-API-SECRET-KEY"
        key (:alpaca1 keychain)
        skey (:alpaca2 keychain)
        dataurl "https://data.alpaca.markets/v1/bars/"]
    (fn
      ([ticker limit bar]
        (let[
             q  (str "limit=" limit "&symbols=" (str/upper-case (str ticker)))
             u  (str dataurl bar q)
             hm (client/get u {:headers {hdr1 key, hdr2 skey}})
             s  (:body hm)
            ]
          s))
      ([ticker limit]
        (let [bar "1D?"] ;choose from "1Min?" "5Min?" "15Min?" "1D?"
          (getp ticker limit bar)))
      ([ticker]
        (let [limit 100] ;max is 1000
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