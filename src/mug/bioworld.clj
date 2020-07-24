(ns mug.bioworld
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.test :as test])
  (:gen-class))

(defn getp 
  ([biodate]
    (-> biodate
        (str/replace #"Mon_" "monday-")
        (str/replace #"Tue_" "tuesday-")
        (str/replace #"Wed_" "wednesday-")
        (str/replace #"Thu_" "thursday-")
        (str/replace #"Fri_" "friday-")
        (str/replace #"Sat_" "saturday-")
        (str/replace #"Sun_" "sunday-")
        (str/replace #"_0" "_")
        (str/replace #"-0" "-")
        (str/replace #"_2019" "-2019")
        (str/replace #"Jan_" "january-")
        (str/replace #"Feb_" "february-")
        (str/replace #"Mar_" "march-")
        (str/replace #"Apr_" "april-")
        (str/replace #"May_" "may-")
        (str/replace #"Jun_" "june-")
        (str/replace #"Jul_" "july-")
        (str/replace #"Aug_" "august-")
        (str/replace #"Sep_" "september-")
        (str/replace #"Oct_" "october-")
        (str/replace #"Nov_" "november-")
        (str/replace #"Dec_" "december-")
        ((fn [x] (println x) x))
        ((fn [x] (str "http://www.bioworld.com/content/" x)))
        (client/get {:cookie-policy :standard})
        (:body)))
  ([]
    (getp "monday-june-24-2019")))


#_(defn putfridge3
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