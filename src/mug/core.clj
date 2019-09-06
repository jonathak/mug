(ns mug.core
  (:require [mug.app          :as app]
            [mug.util         :as util]
            [alphav.core         :as alphav]
            [sec.core            :as sec]
            [clojure.string      :as str]
            [clojure.java.browse :refer [browse-url]])
  (:gen-class))

;utilities --------------------------------------------------------------------------;
(def pl "println abbreviation" println)
(def lp "pl alias" pl)

(defn simplenum 
  "converts comma separated one to integer millions"
  [snum]
  (-> snum
      (str/replace #"," "")
      (read-string)
      (* 1e-6)
      (int)))

(def t? util/valid-ticker?)

;functions of t ---------------------------------------------------------------------;
(def cname "company name"            (fn [t](when (t? t) (app/cname t))))
(def i     "industry"                (fn [t](when (t? t) (app/industry t))))
(def web   "website"                 (fn [t](when (t? t) (app/website t))))
(def desc  "description"             (fn [t](when (t? t) (app/description t))))
(def ceo   "ceo"                     (fn [t](when (t? t) (app/ceo t))))
(def s     "sector"                  (fn [t](when (t? t) (app/sector t))))
(def emp   "employees"               (fn [t](when (t? t) (app/employees t))))

(def zipcode "zipcode"               sec/zipcode)

(defn oweb  [t]  (browse-url (web t)))
(defn yahoo [t]  (browse-url (str "https://finance.yahoo.com/quote/" t "/")))
(defn sec   [t]  (browse-url (str "https://www.sec.gov/cgi-bin/browse-edgar?CIK=" 
                                  t "&owner=exclude&action=getcompany")))

(def so    "sharesoutstanding"       (fn [t](when (t? t)
                                       (-> (app/sharesoutstanding+ t)
                                           (simplenum)
                                           ((fn [x] (if (= x 0) -1 x))))))) ; so 'so can be divisor

(def pf    "publicfloat"             (fn [t](when (t? t)
                                       (-> (app/publicfloat+ t)
                                           (simplenum)))))

(def p     "price"                   (fn [t] (when (t? t)
                                       (read-string (app/price! t)))))

(def v     "avg30Volume"             (fn [t](when (t? t)
                                       (-> (app/avg30Volume+ t)
                                           (str/replace #"," "")
                                           (read-string)
                                           ((fn [x] 
                                              (let [y (app/price! t)] 
                                                (* x (if (> (count y) 0) 
                                                         (read-string y)
                                                         0)))))
                                           (* 1e-3)
                                           (int)
                                           (* 1e-3)
                                           (str)
                                           (str/split #"\.")
                                           ((fn [[x y]] (str x "." (subs y 0 (min 2 (count y))))))
))))     

(def b     "beta"                    (fn [t] (when (t? t)
                                       (-> t (app/beta+ ) (read-string) (* 100.) (int) (/ 100.)))))

(def mkt   "marketcap"               (fn [t] (when (t? t)
                                       (-> (app/marketcap+ t)
                                           (simplenum)
                                           ((fn [x] (if (= x 0) -1 x))) ; so mkt can be divisor
                                        ))))

(def c     "cash"                    (fn [t] (when (t? t)
                                       (-> (app/cash+ t)
                                           (simplenum)))))

(def c?mkt "cash/marketcap"          (fn [t] (when (t? t)
                                       (-> (app/cash+ t)
                                           (str/replace #"," "")
                                           (read-string)
                                           (/ (-> (app/marketcap+ t)
                                                  (str/replace #"," "")
                                                  (read-string)))
                                           (* 1000.0)
                                           (int)
                                           (/ 1000.0)
                                        ))))

(def d     "debt"                    (fn [t](when (t? t) (app/debt+ t))))

(def r     "revenue"                 (fn [t] (when (t? t)
                                       (-> (app/revenue+ t)
                                           (simplenum)))))

(def g     "gross"                   (fn [t] (when (t? t)
                                       (-> (app/gross+ t)
                                           (simplenum)))))

(def e     "ebitda"                  (fn [t] (when (t? t)
                                       (-> (app/ebitda+ t)
                                           (simplenum)))))

(def d2e   "dbtoeqty"                (fn [t](when (t? t) (app/dbtoeqty+ t))))
(def ir    "insider roster"          (fn [t](when (t? t) (app/ir t))))
(def db    "deep book"               (fn [t](when (t? t) (app/db t))))
(def sp    "splits"                  (fn [t](when (t? t) (app/sp t))))
(def fo    "fund ownership"          (fn [t](when (t? t) (app/fo t))))
(def cffd   "cff detail"             (fn [t](when (t? t) (app/cff t))))

(def cff    "cff total"              (fn [t](when (t? t) 
                                       (->> (-> (app/cff t)(str/split #"\n"))
                                            (map #(str/split % #" "))
                                            (map (fn [x] (or (second x) "0.0")))
                                            (map read-string)
                                            (reduce +)
                                            (int)
                                            (str)))))

(def io    "institutional-ownership" (fn [t](when (t? t) (app/io t))))
(def cc    "ceo-compensation"        (fn [t](when (t? t) (app/cc t))))
(def sl    "symbol lookup"           (fn [t](when (t? t) (app/sl t))))


;functions of t or t,b --------------------------------------------------------------;
(def prices   "prices,volume->txt [t] [t b]"    app/print-all-prices)
(def fprices  "freshprices! [t] [t b]"          app/freshprices!)
(def ngm      "normalized gap-moves [t] [t b]"  app/normalized-gap-moves)
(def vp       "alphavantage [t]"                alphav/getp)

(def mov      "top 5 ngm instances [t]"         app/movements)

(def mvs      "scalar version of mov"           (fn [t] 
                                                  (-> (app/tot-abs-movements t) 
                                                      (* 100.0) (int) (/ 100.0))))

;other functions --------------------------------------------------------------------;
(def all app/all)
(def w "window [low,high] [data,low,high]" app/window)
(def wh "window-htm [low,high] [data,low,high]" app/window-htm)
(def wh0 "window-htm [low,high] [data,low,high] no rev" app/window0-htm)
(def nt "NUMber of TICKerS in mktcap range l h" app/numticks)

(defn zipcode-refresh 
  "refreshes zipcode fridge"
  [list-of-tickers]
  (let []
    (doseq [ticker list-of-tickers]
      (when (cname ticker)
        (do (spit "resources/zipcode.txt" (str ticker " " (zipcode ticker) "\n") :append true)
            (print ".")))))) 



