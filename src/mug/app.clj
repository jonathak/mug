(ns mug.app
  (:import Crap Fart Jama.Matrix Jama.SingularValueDecomposition)
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [mug.util :as util]
            [alpaca.core :as alp]
            [alphav.core :as alphav]
            [clojure.test :as test]
  )
  (:gen-class))

;------------------------------------------------------------------------------------;

(defn d-array [vec] 
  "convert clojure vector to java array of type double"
  (into-array vec))

(defn d-vec [arr]
  "convert java array of doubles into clojure vector"
  (let [
        n   (alength arr)
        vec (range n)
       ]
    (into []
      (for [i (range n)] 
        (aget arr i)))))

(defn javacrap  [] (Crap/fart " ..from iexcloud.core/javacrap.. "))
(defn javacrap2 [] (Crap/matic (into-array [1. 2. 3. 4. 5. 6. 7. 8. 9.])))
(defn javacrap3 [] (d-vec (Crap/hole (d-array [1. 2. 3.]))))
(defn javacrap4 [] (d-vec (Svd/hole (d-array [1. 2. 3.]))))
(defn javacrap5 [] (d-vec (Svd/uuu (d-array [1. 2. -5. -2. 1. 3. 3. 1. -1.]) 3)))

(def m      [[1. 2. -5.][-2. 1. 3.][3. 1. -1.]] )
(def m-ser  {:columnwise (d-array (flatten m)) :by (Long. (count (first m)))} )
(def uu     (d-vec (Svd/uuu (:columnwise m-ser) (:by m-ser))))
(def u      (->> uu (partition (:by m-ser)) (map vec) (vec)))

;------------------------------------------------------------------------------------;

(defn  industries  [] 
  (let [
        raw (-> (slurp "resources/companies.txt")
                (str/split #"\n\n"))
       ]
    (set (map #(util/xtract %1 "industry") raw))))

(defn  cname       [t] (util/xtract (util/snatch t) "companyName"))
(defn  industry    [t] (util/xtract (util/snatch t) "industry"))
(defn  website     [t] (util/xtract (util/snatch t) "website"))
(defn  description [t] (util/xtract (util/snatch t) "description"))
(defn  ceo         [t] (util/xtract (util/snatch t) "CEO"))
(defn  sector      [t] (util/xtract (util/snatch t) "sector"))
(defn  employees   [t] (util/xtract (util/snatch t) "employees"))

;please switch these to keystats+ when keystats+ is repaired
(defn  sharesoutstanding+ [t] (util/advstat-helper+ t "sharesOutstanding"))
(defn  publicfloat+       [t] (util/advstat-helper+ t "float"))
(defn  avg30Volume+       [t] (util/advstat-helper+ t "avg30Volume"))
(def  v+ avg30Volume+)
(defn  day200mvgavg+      [t] (util/advstat-helper+ t "day200MovingAvg"))
(defn  day50mvgavg+       [t] (util/advstat-helper+ t "day50MovingAvg"))
;(defn  nextearn+          [t] (util/advstatfast+  t "nextEarningsDate"))
(defn  beta+              [t] (util/advstatfast+  t "beta"))
(defn  marketcap+         [t] (util/advstat-helper+  t "marketcap"))


(defn  cash+       [t] (util/advstat-helper+ t "totalCash"))
(defn  debt+       [t] (util/advstat-helper+ t "currentDebt"))
(defn  revenue+    [t] (util/advstat-helper+ t "revenue"))
(defn  gross+      [t] (util/advstat-helper+ t "grossProfit"))
(defn  ebitda+     [t] (util/advstat-helper+ t "EBITDA"))
(defn  dbtoeqty+   [t] (util/advstatfast+ t "debtToEquity"))

(defn  price!      [t] (util/price! t))

(defn  cash       [t] (util/advstat-helper t "totalCash"))
(defn  debt       [t] (util/advstat-helper t "currentDebt"))
(defn  revenue    [t] (util/advstat-helper t "revenue"))
(defn  gross      [t] (util/advstat-helper t "grossProfit"))
(defn  ebitda     [t] (util/advstat-helper t "EBITDA"))
(defn  dbtoeqty   [t] (util/advstatlocal t "debtToEquity"))

;------------------------------------------------------------------------------------;

(defn refresh "refreshes ticker in advstats.txt file"
  [t]
  (util/putfridge2!! t))

(defn sl "symbol lookup"
  [bait]
    (if-let [raw (util/symbol-lookup (str/upper-case bait))]
      (if-let [data (-> raw
                        (str/replace #", " "_")
                        (str/replace #" " "_")
                        (str/replace #"\"" "")
                        (str/replace #":" " ")
                        (str/replace #"," " ")
                        (str/replace #"\s\d+\w" " n")
                        (read-string))]
        (if-let [[{x 'symbol} _] data] 
          x
          data)
        raw)
      "0"))

(defn cc "ceo-compensation"
  [t]
    (if-let [raw (util/ceo-compensation+ t)]
      (-> raw
        (str/replace #"^[A-Z]+ " "")
        (str/replace #":\"\"" ":\"0\"")
        (str/replace #", " "_")
        (str/replace #" " "_")
        (str/replace #"\"" "")
        (str/replace #":" " ")
        (str/replace #"," " ")
        (read-string)
        ('salary))
      0))

(defn ir "insider roster"
  [t]
    (->>
      (-> (util/insider-roster+ t)
          (str/replace #"^[A-Z]+ " "")
          (str/replace #" " "_")
          (str/replace #"\"" "")
          (str/replace #":" " ")
          (str/replace #"," " ")
          (read-string))
      (map #(str (% 'entityName) "\n"))
      (reduce #(str %1 %2))))

(defn db "deep book"
  [t]
    (-> (util/deepbook t)
        (str/replace #"\"" "")
        (str/replace #":" " ")
        (str/replace #"," " ")
        (read-string)))

(defn ^String sp "splits"
  {:updated '20190906}
  [t]
   (if (util/valid-ticker? t)
    (let [booger (fn [[x]] (if x [x] [{'exDate 'none 'ratio 'none}]))]
      (->>
        (-> (util/splits+ t)
            (str/replace #"^[A-Za-z]+\s" "")
            (str/replace #"\"" "")
            (str/replace #":" " ")
            (str/replace #"," " ")
            (str/replace #"-" "")
            (str/replace #"for" "/")
            (read-string)
            (booger))
        (map #(str (% 'exDate) " " (% 'ratio) "\n"))
        (reduce #(str %1 %2))))
    ;else not valid ticker
    "_")
)

(defn  fo "fund ownership" 
  [t] 
    (let [
          raw (util/fund-ownership+ t)
          clo (util/raw-to-clojure raw)
          s   (->> (map #(str (util/year ('report_date %)) " " 
                              (util/dayofyear ('report_date %)) " " 
                              ('entityProperName %)) 
                      clo)
                (sort) (reverse) (reduce #(str %1 "\n" %2)))
         ]
      (spit (str "resources/" t "_fo.txt") s)
      s))

(defn  cff "cash flow financing" 
  [t] 
  (let [hm (-> t (util/cash-flow+) (util/raw-to-clojure))]
    (if (> (count hm) 0)
        (->> (if (= [] ('cashflow hm))
                 [{'reportDate '999 'cashFlowFinancing 9999}]
                 ('cashflow hm))
             (map (fn [x] (str ('reportDate x) " " (-> ('cashFlowFinancing x)
                                                       (#(if (= % 'null) 0 %))
                                                       (/ 1e4) (int) (/ 1e2)))) )
            (reduce #(str %1 "\n" %2)) )
        "")))

(defn io "institutional-ownership"
  [t]
  (let [
          w (fn [x] (-> x (/ 314712e5) (+ 1970) (int) (str)))
        clo (-> (util/institutional-ownership+ t)
                (util/raw-to-clojure)
            )
          s (->> (map #(str (w ('reportDate %)) " " ('entityProperName %)) clo)
                 (sort)
                 (reverse)
                 (reduce #(str %1 "\n" %2)))
       ]
    s))

(defn news "news"
  [t]
  (let [
          w (fn [x] (-> x (/ 314712e5) (+ 1970) (int) (str)))
         ff (fn [x] (-> x (str/replace #"(\"|\:|\,)" "")))
         fx (fn [x] (w (read-string (ff x))))
         fy ff
         fz ff
        clo (->> (-> (util/news! t)
                     (str/split #"datetime")
                     (rest)
                 )
                 (map (fn [x] (str/split x #"(headline|source|summary)")))
                 (map (fn [x] (-> x (into []) (pop))))
                 (map (fn [[x y z]] [(fx x) (fy y) (fz z)]))
            )
       ]
    (->> clo
         (map (fn [[x y z]] (str x " " y)))
         (reduce (fn [x y] (str x "\n\n" y)))
    )))


(defn pl "println abbreviation"
  [s]
  (println s))

(def lp pl) ; pl alias


(defn av-prices [] true)


(defn print-all-prices 
  ;"provides candlestick data with volume to txt file"
  ;{:post [(instance? Boolean %)]}
    ([t b]
      (let [
            nn  (str "resources//" (str/upper-case t) "_a_" b ".txt")
            ff  (java.io.File. nn)
            hm  {"1Min?" 60  "5Min?" 300 "15Min?" 900 "1D?" 86400}
           ]
        (if (.exists ff)
          (let [
                f  (fn [s] (if s (first (str/split s #" ")) "0"))
                fp (fn [s] (-> (read-string s) (* 1000) (util/datetime) (str/replace #"(\s|:)" "_")))
                ;fp2 (fn [s] (-> (read-string s) (- 1546232400) (/ (or (hm b) 86400)) (+ 0.5) (int)))
                fp2 (fn [s] (-> (read-string s) (- 1546289100) (/ (or (hm b) 86400)) (+ 0.5) (int)))
                f3 (fn [s] (str (fp2 (f s)) " " (fp (f s)) " " s))
                fnoth (fn [x] x) ;useful when debugging
               ]
            (->> (-> (slurp nn) (str/split #"\"t\":") (rest))
                 (map #(str/replace % #"\}\,\{" "\n"))
                 (map #(str/replace % #"\,\"(o|h|l|c|v)\"\:" " "))
                 (map f3)
                 (reduce (fn [x y] (str x y)))
                 (spit (str "resources//" (str/upper-case t) "_a_" b ".p")))
            true) 
          ;else
          (if (alp/putfridge3 t b)
            (if (.exists (java.io.File. nn))
                (print-all-prices t b)
                false)
            false))))
    ([t]
      (print-all-prices t "1D?")))

(defn freshprices! 
  ([t b]
    (alp/putfridge3 t b)
    (print-all-prices t b))
  ([t]
    (freshprices! t "1D?")))


(defn normalized-gap-moves 
  ;"normalized-gap-moves (open2/close1 or -close1/open2)"
    ([t b]
      (let [
            nn  (str "resources//" (str/upper-case t) "_a_" b ".txt")
            ff  (java.io.File. nn)
            hm  {"1Min?" 60  "5Min?" 300 "15Min?" 900 "1D?" 86400}
           ]
        (if (.exists ff)
          (let [
                f             (fn [s] (if s (first (str/split s #" ")) "0"))
                fp            (fn [s] (-> (read-string s) (* 1000) (util/datetime) (str/replace #"(\s|:)" "_")))
                fp2           (fn [s] (-> (read-string s) (- 1546289100) (/ (or (hm b) 86400)) (+ 0.5) (int)))
                f3            (fn [s] (str (fp2 (f s)) " " (fp (f s)) " " s))
                fo            (fn [[_ _ _ o _ _ _ _]] o)
                fc            (fn [[_ _ _ _ _ _ c _]] c)

                mrs           ;modified read-string
                              (fn [ss] 
                                (let [sn (read-string ss)] 
                                  (if (= (type sn) java.lang.Double)
                                      sn
                                      (if (= (type sn) java.lang.Long)
                                          (* 1.0 sn)
                                          77777.777))))
 
                ft            (fn [s] (-> (str/split s #" ") 
                                          ((fn [[xx yy]] 
                                             (let [x (mrs xx)
                                                   y (mrs yy)]
                                               (if (or (= x 0.0) (= y 0.0))
                                                   88888.888
                                                   (/ (- y x) x)))))))
           
                periods       (->> (-> (slurp nn) (str/split #"\"t\":") (rest))
                                   (map #(str/replace % #"\}\,\{" "\n"))
                                   (map #(str/replace % #"\,\"(o|h|l|c|v)\"\:" " "))
                                   (map f3))
                [_ & futures] periods 
                opens         (map fo (map #(str/split % #" ") futures))
                closes        (map fc (map #(str/split % #" ") (drop-last periods)))
                combined      (util/zip-strings closes opens)
                transformed   (map ft combined)
                result        (util/zip-strings (map str/trim-newline futures) transformed)
               ]
            (do (spit (str "resources//" (str/upper-case t) "_g_" b ".p") (reduce #(str %1 "\n" %2) result)) 
                [combined transformed])) 
          ;else
          (if (alp/putfridge3 t b)
            (if (.exists (java.io.File. nn))
                (normalized-gap-moves t b)
                false)
            false))))
    ([t]
      (normalized-gap-moves t "1D?")))


(defn volume-spikes 
  "sorted vector of 5 largest volume periods"
  [t]
  (print-all-prices t)
  (let [
        b    "1D?"
        gulp (slurp (str "resources//" (str/upper-case t) "_a_" b ".p")) ;string
        fl   (fn [s] (-> s (str/split #" "))) ;string to list
        my-* (fn [v c] (* (read-string v) (read-string c)))
        fv (fn [s]  
             (-> (str/split s #" ")
                 ((fn [[_ _ _ _ _ _ c v]] (my-* v c)))))
        recent? (fn [s] (or (= s "2019") (= s "2018")))
        year? (fn [s] (-> s (str/split #" ") (second) (reverse) ((fn [[a b c d & e]] (str d c b a))) ))
       ]
    (->> (str/split gulp #"\n")
         (filter (fn [s] (recent? (year? s)))) ;don't want anything too old
         (sort-by fv)
         (reverse)
         (take 5)
         (map (fn [s] (str s " " (fv s))))
         (map (fn [s] (str/split s #" ")))
         (map (fn [[_ d _ _ _ _ _ _ vc]] [d vc]))
         (map (fn [[d vc]] [(str/replace d #"_00_00_00_E(D|S)T" "") vc]))
         (map (fn [[d vc]] [d (read-string vc)]))
         (map (fn [[d vc]] [d (int (/ vc 1e6))]))
         (map (fn [[d vc]] (str d " " vc)))
         (reduce #(str % "\n" %2))
    )
  )
)

(defn top-volume-spike [t]
  (->> (str/split (volume-spikes t) #"\n")
       (map #(str/split % #" "))
       (map second)
       (map read-string)
       (apply max)
       (str)
))


(defn movements 
  "sorted vector of 5 largest normalized gap moves"
  [t]
  (normalized-gap-moves t "1D?")
  (let [
        b    "1D?"
        gulp (slurp (str "resources//" (str/upper-case t) "_g_" b ".p")) ;string
        fl   (fn [s] (-> s (str/split #" "))) ;string to list
        fngm (fn [s] (->> (fl s) (last) (read-string) ((fn [x] (if (< x 0) (- 0 x) x))))) ;string to ngm float (java.lang Comparable)
        recent? (fn [s] (or (= s "2019") (= s "2018")))
        year? (fn [s] (-> s (str/split #" ") (second) (reverse) ((fn [[a b c d & e]] (str d c b a))) ))
       ]
    (->> (str/split gulp #"\n")
         (filter (fn [s] (recent? (year? s)))) ;don't want anything too old
         (sort-by fngm)
         (reverse)
         (take 5)
         (map (fn [s] 
                (let [flr (fl s)]
                  (str (str/replace (second flr) #"00_00_00_E(D|S)T_" "") 
                       "\t" 
                       (-> (last flr)
                           (read-string)
                           (min 10.0)
                           (* 100.0)
                           (int)
                       )
                       " %"))))
         (reduce #(str % "\n" %2))
    )
  )
)

(defn tot-abs-movements
  "scalar version of movements"
  [t]
  (let [
        fabs (fn [x] (if (< x 0.0) (- 0 x) (+ 0 x)))
       ]
    (->> (str/split (movements t) #"\n")
         (map (fn [s] (str/split s #"\t")))
         (map second)
         (map read-string)
         (map fabs)
         (reduce +)
         (* 0.2)
         (int)
    )
  )
)

;------------------------------------------------------------------------------------;

(defn- funct+ 
  "for use in top etc."
  [[tic cap]]
  (str "<tr>
          <td>" (int (/ cap 1e6)) "</td>
          <td>" (/ (util/parsint (revenue+ tic)) 1) "</td>
          <td>" (/ (util/parsint (avg30Volume+ tic)) 1) "</td>
          <td>" (/ (util/parsint (day50mvgavg+ tic)) 1) "</td>
          <td>" (* (/ (util/parsint (avg30Volume+ tic)) 1) (/ (util/parsint (day50mvgavg+ tic)) 1)) "</td>
          <td>" tic "</td>
          <td> <a href=\"" (website tic) "\">" (website tic) "</a></td>
        </tr>"))

(defn- funct0+ 
  "for use in top etc."
  [[tic cap]]
  (if (= 0 (/ (util/parsint (revenue+ tic)) 1))
    (str "<tr>
            <td>" (int (/ cap 1e6)) "</td>
            <td>" (/ (util/parsint (revenue+ tic)) 1) "</td>
            <td>" (/ (util/parsint (avg30Volume+ tic)) 1) "</td>
            <td>" (/ (util/parsint (day50mvgavg+ tic)) 1) "</td>
            <td>" (-> (* (/ (util/parsint (avg30Volume+ tic)) 1) (/ (util/parsint (day50mvgavg+ tic)) 1)) 
                      (/ 1e4) (int) (/ 1e2))"</td>
            <td>" tic "</td>
            <td> <a href=\"" (website tic) "\">" (website tic) "</a></td>
          </tr>")
    ""))

(defn- funct 
  "for use in top etc."
  [[tic cap]]
  (str "<tr>
          <td>" (int (/ cap 1e6)) "</td>
          <td>" (/ (util/parsint (revenue tic)) 1) "</td>
          <td>" tic "</td>
          <td> <a href=\"" (website tic) "\">" (website tic) "</a></td>
        </tr>"))

(defn top+ [num]
  "creates fresh html file, table of biotech companies"
  (util/maketable (first (partition num (util/biomktcapsorted))) funct+ "out"))

(defn graballcompanies! []
  "refreshes the entire flatfile database!"
  (util/doseq-interval util/putfridge! (util/iex-symbols!) 20))

(defn all [] ; used in window
    (util/biomktcapsorted))

(defn norevenues+ [] 
  (filter (fn [[t m]] (= (revenue+ t) "0")) (all)))

(defn window 
  ([data low high]
    (util/lowpass (util/highpass data low) high))
  ([low high]
    (window (all) low high)))

(defn window-htm [low high]
  (util/maketable (window low high) funct+ "out"))

(defn window0-htm [low high]
  (util/maketable (window low high) funct0+ "out"))

(defn graballadvstats!! []
  "refreshes entire flatfile database - very very expensive!"
  (util/doseq-interval util/putfridge2!! (map first (window 25 2500)) 20))

(defn numticks "NUMber of TICKerS in mktcap range l h"
  [l h]
    (count (window l h)))

#_(defn get_bio_prices! []
  "writes price history file for each ticker!"
  (util/doseq-interval alp/putfridge3 (util/biopharmas) 500))

(defn rates [t]
  (let [
        n  (str "resources//" (str/upper-case t) "_p.txt")
        f  (java.io.File. n)
       ]
    (if (.exists f)
      (let [
            s  (slurp (str "resources//" (str/upper-case t) "_p.txt"))
            l  (rest (str/split s #"\"t\":"))
            l2 (map #(str/split % #"\"c\":") l)
            f  (fn [s] (if s (first (str/split s #",")) "0"))
            fx (fn [s] (- (int (/ (read-string s) 86400.0)) 16590))
            fy (fn [s] (read-string s))
            f2 (fn [[x y]] [(fx (f x)) (fy (f y))])
            l3 (map f2 l2)
            lp (rest l3) 
            lm (reverse (rest (reverse l3)))
            l4 (map vector lm lp)
            l5 (map (fn [[[w x] [y z]]] (str y " " (- z x) "\n")) l4)
            s2 (reduce (fn [x y] (str x y)) l5)
           ]
        (spit (str "resources//" t "_r.p") s2))
      (do
        (alp/putfridge3 t)
        (rates t)))))

;------------------------------------------------------------------------------------;


