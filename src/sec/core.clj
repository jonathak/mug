(ns sec.core
  (:require [mug.util :as util]
            [mug.app  :as app]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:gen-class))

(def t->c ; convert ticker to cik
  (let [data (->> 
               (-> (slurp "resources/sectc.txt")
                 (str/split #"\n"))
               (map (fn [s] (str/split s #"\t")))
               (map (fn [[t c]] [(keyword t) (read-string c)]))
               (into {}))]
    (fn [tick] (get data (keyword (str/lower-case (symbol tick)))))))

(defn ^String secpage
  "scrape sec web page given ticker symbol" 
  [tick]  ; "tick", or 'tick
  (let [
        url (str "https://www.sec.gov/cgi-bin/browse-edgar?CIK="
                 (str/lower-case (str tick))
                 "&owner=exclude&action=getcompany")
        hmf #(try (client/get %) (catch Exception e 1))
       ]
      (if (app/cname (util/tfmt tick))
        (do (try (:body (hmf url)) (catch Exception e 2)))
        "_")))

(defn ^String zipcode
  "scrape zip code from sec page"
  [tick]  ; "tick", or 'tick
  (let [^String raw (secpage tick)]
    (if (= raw "_")
        "99999"
        (-> (re-seq #" \d\d\d\d\d(\s|\-)" raw)
            ((fn [x] (or (first x) "88888")))
            (str/replace #"\s" "")
            (str/replace #"\-" "")
            ))))