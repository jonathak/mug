(ns mug.iex
  (:require [mug.util :as util]
            [mug.fixtures]
            [clj-http.client :as client]
            [clojure.string :as str])
  (:gen-class))

(def ^:dynamic *showcost* (atom false))
(defn verbose [] (swap! *showcost* (fn [_] true)))
(defn concise [] (swap! *showcost* (fn [_] false)))

(def iex-symbols!
  "provides array of all symbols in iexcloud, optionally writes to file"
  (let [
        base-url "https://cloud.iexapis.com/"
        version "beta"
        endpoint-path "/ref-data/iex/symbols"
        query-string (str "?token=" (:iexcloud util/keychain))
       ]
    (fn
      ([] ;returns a list
        (->>
          (->
            (str base-url version endpoint-path query-string)
            (client/get)
            (:body)
            (str/split #"symbol\":\"")
            (rest))
          (map #(str/split %1 #","))
          (map #(first %1))
          (map #(str/replace %1 "\"" ""))
          (map symbol)))
      ([filename] ;writes to file and returns 0
        (->> 
          (iex-symbols!)
          (reduce #(str %1 "\n" %2))
          (spit filename))
        0))))

(def valid-ticker?
  (let [all (->> (-> "resources/iexsymbols.txt" (slurp) (str/split #"\n"))
                 (map #(symbol %)))]
    (fn [t]
      (->> t (util/tfmt) (util/in? all) ))))

(defn symbol-lookup "given name produces the most likely ticker"
  [bait]
    (let [
          clean #(str/replace % #"\s" "_")
          base-url "https://cloud.iexapis.com/"
          version "beta"
          endpoint-path (str "/search/" (clean bait))
          query-string (str "?token=" (:iexcloud util/keychain))
          u (str base-url version endpoint-path query-string)
          hmf #(try (client/get % {:cookie-policy :standard}) (catch Exception e 1))
         ]
      (if "add input validation here"
        (do 
          (when @*showcost* (println "just 1 point"))
          (try (:body (hmf u)) (catch Exception e 2)))
        (do
          (println "invalid bait")
          ""))))

(defn deepbook
  [t]
    (let [
          base-url "https://cloud.iexapis.com/"
          version "beta"
          endpoint-path "/deep/book"
          qstr (str "symbols=" (util/tfmt t))
          query-string (str "?token=" (:iexcloud util/keychain) "&" qstr)
          u (str base-url version endpoint-path query-string)
          hmf #(try (client/get % {:cookie-policy :standard}) (catch Exception e 1))
         ]
      (if (valid-ticker? (util/tfmt t))
        (do 
          (println "free")
          (try (:body (hmf u)) (catch Exception e 2)))
        (do
          (println "invalid ticker")
          ""))))

(defn iex-stock-api! 
  "returns raw response body for a call to stock APIs"
  {:created "20190713"}
  ([ticker path cost qstr]  
    (let [
          base-url "https://cloud.iexapis.com/"
          version "beta"
          endpoint-path (str "/stock/" ticker "/" path)
          query-string (str "?token=" (:iexcloud util/keychain) "&" qstr)
          u (str base-url version endpoint-path query-string)
          hmf #(try (client/get % {:cookie-policy :standard}) (catch Exception e 1))
         ]
      (if (valid-ticker? (util/tfmt ticker))
        (do 
          (when (and @*showcost* (> (count cost) 0)) (println (str cost " points!")))
          (try (:body (hmf u)) (catch Exception e 2)))
        (do
          (println "invalid ticker")
          ""))))
  ([ticker path cost]
    (iex-stock-api! ticker path cost "")))

(defn price! [t]
  (iex-stock-api! t "price" ""))

(defn ceo-compensation! [t]
  (iex-stock-api! t "ceo-compensation" "20"))

(defn insider-roster! [t] 
  (iex-stock-api! t "insider-roster" "5,000"))

(defn splits! [t]
  (iex-stock-api! t "splits/5y" "10 per symbol per record"))

(defn cash-flow! [t]
  (iex-stock-api! t "cash-flow" "12,000" "last=12"))

(defn advstats! [t]
  (iex-stock-api! t "advanced-stats" "3,000"))

(defn institutional-ownership! [t]
  (iex-stock-api! t "institutional-ownership" "10,000"))

(defn fund-ownership! [t]
  (iex-stock-api! t "fund-ownership" "10,000"))

(defn insider-transactions! [t]
  (iex-stock-api! t "insider-transactions" "10,000"))

(defn news! [t]
  (iex-stock-api! t "news" "50"))

(defn ^String refresh
  "puts fresh data into a flat-file fridge, aka memoize, used in iex-stock-api-generator"
  {:updated "20190713"
   :revised "20190906"}
  [ticker api-call filename]
  (let [r  (or (api-call ticker) "{}")
        f  (fn [x] (-> x (str) (str/upper-case) (str " " r "\n\n")))
        s  (f ticker)] 
    (do (spit filename s :append true)
        s)))

(defn ^String iex-stock-api-generator 
  "compare with iex-stock-api!, uses refresh"
  {:created "20190713"
   :revised "20190906"}
  [api-call filename]
  (let [fname (java.io.File. filename)]
    (if (.exists fname)
      (let [sp   #(str/split % #" ")
            sp2  #(str/split % #"\n\n")
            f    (fn [s] (-> (sp s) (first) (symbol) ((fn [x] [x s]))))
            *hm* (->> (-> (slurp filename) (sp2))
                   (map f) (into []) (into (sorted-map)) (atom))]
        (fn [ticker]
         (if (valid-ticker? ticker)
          (let [t (util/tfmt ticker)]
            (if-let [s (get @*hm* t)]
              s
              (if-let [ss (refresh t api-call filename)]
                (do
                  (swap! *hm* #(assoc % t (f ss)))
                  ss)
                "_")))
          "_")))
      ;else
      (fn [t]
       (if (valid-ticker? t)
        (do (refresh t api-call filename)
            ((iex-stock-api-generator api-call filename) t))
        "_")))))

(defn ceo-compensation+
  "ceo compensation"
  [ticker]
  ((iex-stock-api-generator ceo-compensation! "resources/ceo-compensation.txt") ticker))

(defn insider-roster+
  "lists insiders" 
  [ticker]
  ((iex-stock-api-generator insider-roster! "resources/insider-roster.txt") ticker))

(defn splits+
  "reports stock splits" 
  [ticker]
  ((iex-stock-api-generator splits! "resources/splits.txt") ticker))

(defn fund-ownership+
  "tries text file, calls api if txt fails" 
  [ticker]
  ((iex-stock-api-generator fund-ownership! "resources/fund-ownership.txt") ticker))

(defn cash-flow+
  "tries text file, calls api if txt fails" 
  [ticker]
  ((iex-stock-api-generator cash-flow! "resources/cash-flow.txt") ticker))

(defn institutional-ownership+
  "tries text file, calls api if txt fails" 
  [ticker]
  ((iex-stock-api-generator institutional-ownership! "resources/institutional-ownership.txt") ticker))

(defn advstatfridge []
  "use in advstatlocal"
  (let [
        s  (slurp "resources/advstats.txt")
        l  (str/split s #"\n\n")
        l2 (map (fn [s] [(symbol (first (str/split s #" "))) s]) l)
        l3 (into [] l2)
        hm (into (sorted-map) l3)
       ]
    (fn [t field]
      (let [tt (-> (str t) (str/upper-case) (symbol))]
        (if (get hm tt)
          (second (str/split (util/xtract (get hm tt) field) #":"))
          (str "888"))))))

(def advstatlocal (advstatfridge))

(defn company!
  "returns raw response body for a call to company API"
  ([ticker] ;get the full response body
    (let [
          base-url "https://cloud.iexapis.com/"
          version "beta"
          endpoint-path (str "/stock/" ticker "/company")
          query-string (str "?token=" (:iexcloud util/keychain))
          u (str base-url version endpoint-path query-string)
          hm (try (client/get u {:cookie-policy :standard}) (catch Exception e 1))
          s (try (:body hm) (catch Exception e 2))
         ]
      s))
  ([ticker field] ;just get the field of interest
    "calls (company ticker), then extract the relevant field from the result"
    (let [s (company! ticker)]
      (util/xtract s field)))
)

(defn putfridge! [ticker] ;store raw text to flat file
  "appends raw company string to fridge flat file for given ticker"
	(let [good (company! ticker)]
	  (if (= good nil)
     0
     (spit "resources/fridge.txt" (str good "\n\n") :append true))))

(defn putfridge2!! [t]
  (if-let [good (advstats! t)]
    (let [f (fn [x] (-> x (str) (str/upper-case) (str " " good "\n\n")))] 
      (spit "resources/advstats.txt" (f t) :append true)
      good)
    nil))

(defn advstatfridge+ []
  "use in advstatfast"
  (let [
        sp   #(str/split % #" ")
        sp2  #(str/split % #"\n\n")
        f    (fn [s] 
               (-> (sp s) (first)
                 (symbol) ((fn [x] [x s]))))
        *hm* (->> 
               (-> 
                 "resources/advstats.txt" 
                 (slurp)
                 (sp2)
               )
               (map f)
               (into [])
               (into (sorted-map))
               (atom)
             )
       ]

    (fn [t field]
      (let [
            tt (util/tfmt t)
            f  (fn [x] (-> (util/xtract x field) (str/split #":") (second)))
           ]
        (if-let [s (get @*hm* tt)]
          (f s)
          (if-let [ss (putfridge2!! tt)]
            (do
              (swap! *hm* #(assoc % tt ss))
              (f ss)
            )
            nil)
) ) ) ) )

(def advstatfast+ (advstatfridge+))

(defn maketable 
  "creates fresh html file, table of biotech companies, see (biomktcapsorted)"
  [data funct fname]
    (spit (str fname ".htm")
      (str
        "<html><head></head><body><center><table border=\"1\" cellpadding=\"4\">"
        (reduce
          (fn [s1 s2] (str s1 s2))
          (map funct data))
        "</table></center></body></html>"))
    true)

(defn companydata [] ;fridge for snatch.
  (let [
        tf      (fn [rs] (util/xtract rs "symbol"))
        newmap2 (->> (-> (slurp "resources/companies.txt")
                         (str/split #"\n\n"))
                     (map (fn [s] [(-> s (tf) (symbol)) s]))
                     (into {}))
       ]
    (fn [t] (get newmap2 (symbol (str/upper-case t)))) ;becomes snatch
) )

(def snatch (companydata))

(defn isin? [x l] (> (count (filter #(= % x) l)) 0))

(defn fmt-helper [t, f, fun]
  (format "%,d"
    (util/parsint
      (fun (str/upper-case (str t)) f))))

(defn keystats! 
  "returns raw response body for a call to key API"
  [t]
  (let [
        tt (util/tfmt t)
        base-url "https://cloud.iexapis.com/"
        version "beta"
        endpoint-path (str "/stock/" tt "/stats/")
        query-string (str "?token=" (:iexcloud util/keychain))
        u (str base-url version endpoint-path query-string)
        hm (try (client/get u {:cookie-policy :standard}) (catch Exception e 1))
        s (try (:body hm) (catch Exception e 2))
       ]
    s)
)

(defn get-mktcap
"gets fresh mkt cap frem keystats only costs one point"
[t]
  (let [
        tt (util/tfmt t)
        base-url "https://cloud.iexapis.com/"
        version "beta"
        endpoint-path (str "/stock/" tt "/stats/marketcap")
        query-string (str "?token=" (:iexcloud util/keychain))
        u (str base-url version endpoint-path query-string)
        hm (try (client/get u {:cookie-policy :standard}) (catch Exception e 1))
        s (try (:body hm) (catch Exception e 2))
       ]
    (when @*showcost* (println "just 1 point"))
    (if s s 0))
)

(defn freshnup [fridge]
  "updates mug.fixtures/biomktcaps"
  []
"to do")

(defn keystat-helper! [t, f]
  (fmt-helper t f keystats!))
  
(defn advstat-helper+ [t, f] 
  (fmt-helper t f advstatfast+)) 
  
(defn advstat-helper [t, f]
  (fmt-helper t f advstatlocal))
  
#_(defn biotechs []
  (let [
        f?  (fn [t]  (= (util/xtract t "industry") "Biotechnology"))
        tf  (fn [t] (util/xtract t "symbol"))
       ]
    (->> (-> (slurp "resources/companies.txt")
             (str/split #"\n\n"))
         (filter f?)
         (map tf)
         (map symbol))))
    
#_(defn biopharmas []
  (->> (-> (slurp "resources/biopharmas.txt")
           (str/split #"\n"))
       (map symbol)))

; i need to update this function
; perhaps i should define the following as an 
; anonymous function within the one that follows
#_(defn freshbiomktcapsorted!! []
  (reverse 
    (sort-by second 
      (into [] 
        (map (fn [s] [(util/tfmt s) (keystats! s "marketcap")]) 
          (biopharmas))))))

(defn biomktcapsorted []
  (let [
        f1 (fn [s] (str/split s #" "))
        f2 (fn [[x y]] [(util/tfmt x) (util/stonum y)])
       ]
    (->> (-> (mug.fixtures/biomktcaps)
             (str/split #"\n"))
         (map f1)
         (map f2))))

; i need to update this function
#_(defn biomktcapsorted!! []
  (let [
        f "resources/biomktcapsorted.txt"
        f1 (fn [[x y]] (str x " " y "\n"))
        f2 (fn [x y] (str x y))
       ]
    (->> (freshbiomktcapsorted!!)
         (map f1)
         (reduce f2)
         (spit f))
    (biomktcapsorted)))

(defn highpass 
  "high pass marketcap filter, pass into util/maketable as data"
  [coll cutoff]
  (let [f (fn [[_ mktcp]] (> mktcp (* 1e0 cutoff)))]
    (filter f coll)
) )

(defn lowpass 
  "high pass marketcap filter, pass into util/maketable as data"
  [coll cutoff]
  (let [f (fn [[_ mktcp]] (< mktcp (* 1e0 cutoff)))]
    (filter f coll)
) )

#_(defn putfridge5! [t] ;needs repair
  (if-let [good (keystats! t)]
    (let [tt (-> t (str) (str/upper-case) (keyword))] 
      (spit "resources/keystats.txt" (str tt " " good "\n\n") :append true)
      good)
    nil))

#_(defn keystatfridge+ [] ;needs repair
  "use in keystatfast+"
  (let [
        sp   #(str/split % #" ")
        sp2  #(str/split % #"\n\n")
        f    (fn [s] 
               (-> (sp s) (first)
                 (kfmt) ((fn [x] [x s]))))
        *hm* (->> 
               (-> 
                 "resources/advstats.txt" 
                 (slurp)
                 (sp2)
               )
               (map f)
               (into [])
               (into (sorted-map))
               (atom)
             )
       ]

    (fn [t field]
      (let [
            tt (kfmt t)
            f  (fn [x] (-> (util/xtract x field) (str/split #":") (second)))
           ]
        (if-let [s (get @*hm* tt)]
          (f s)
          (if-let [ss (putfridge5! tt)]
            (do
              (swap! *hm* #(assoc % tt ss))
              (f ss)
            )
            nil)
) ) ) ) )

#_(def keystatfast+ (keystatfridge+)) ;needs repair


