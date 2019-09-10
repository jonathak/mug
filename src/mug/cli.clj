(ns mug.cli
  (:require [mug.core :refer :all ]
            [mug.app   :as app]
            [mug.util  :as util]
            [alphav.core    :as alphav]
            [sec.core       :as sec]
            [clojure.string :as str]
            [clojure.java.browse :refer [browse-url]])
  (:use [mug.core])
  (:gen-class))

(defn -main
  "functions associated with the iexcloud api"
  [& args]
  (println "\n\n")
  (println 
"
Welcome to Mug!  
******************

(For help, enter .h (including the dot) at the prompt >)\n")
  (declare top)
  (top))

(def rs read-string) ;abbreviation

(defn check-all-true [list-of-booleans] 
  (if (> (count list-of-booleans) 0)
      (let [x (first list-of-booleans)]
        (if (or (= x nil) (= x false))
            false
            (if (= 1 (count list-of-booleans))
                true
                (check-all-true (rest list-of-booleans)))))
      ;else
      true))

(def ^:dynamic *inventory* (atom []))
(def ^:dynamic   *name*    (atom ""))
(def ^:dynamic  *pname*    (atom ""))
(def ^:dynamic  *fridge*   (atom {}))
(def ^:dynamic   *from*    (atom (fn [])))
(def ^:dynamic *bag-buffer* (atom []))

(defn industry?
  "returns fulltext industry name given industry abbreviation"
  [short-form]
  (let [
        i-a (->> (-> (slurp "resources/industry-abbreviations.txt")
                     (str/split #"\n"))
                  (map #(str/split % #"\t"))
                  (map (fn [[a b]] [(util/tfmt a) b]))
                  (into {}))
       ]
    (get i-a (util/tfmt short-form))))

(defn industry-tickers!
  "gets all tickers in a given industry (using abbreviation)"
  [indust-abbrev]
  (let [
        indust  (industry? indust-abbrev)
        i-test? (fn [x] (= indust (i x)))
       ]
  (filter i-test? (util/iex-symbols!))))

(defn industry-tickers
  "gets all tickers in a given industry (using abbreviation)"
  [indust-abbrev]
  (let [
        indust  (industry? indust-abbrev)
        i-test? (fn [x] (= indust (i x)))
       ]
  (filter i-test? (map symbol (str/split (slurp "resources/iexsymbols.txt") #"\n")))))

(defn top []
  (declare catch-all)
  (declare shmelp)
  (declare help)
  (declare quitt)
  (declare sku)
  (declare bag)
  (declare window)
  (declare fill-bag)
  (declare add-to-bag)
  (declare drop-from-bag)
  (declare new-universe)
  (swap! *from* (fn [_] top))
  (if-let [cmd (do (print (str "top" "> ")) (flush) (read-line))]
    (case (-> cmd (str/split #" ") (first))
          ".h"  (if (= 2 (count (str/split cmd #" "))) 
                    (if (= "cmd" (second (str/split cmd #" "))) 
                        (shmelp)
                        (help))
                    (help))

          ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))
                    
          ".q"  (quitt)

          ".w"  (window cmd)
          ".b"  (fill-bag cmd)
          ".a"  (add-to-bag cmd)
          ".d"  (drop-from-bag cmd)

          ".p"  (do (println ".p not yet implimented") (@*from*))

          ".sl" (do (println (if (= 2 (count (str/split cmd #" ")))
                        (let [[x y] (str/split cmd #" ")]
                          (sl y))
                        "none"))
                    (@*from*))

          ".l"  (do (if (> (count @*fridge*) 0)
                        (println (reduce #(str %1 "\n" %2) (map
                                                             (fn [k] (str (count (get @*fridge* k)) "\t" k))
                                                             (keys @*fridge*))))
                        (println "fridge is empty."))
                        (top))

          ".noisy"  (do (util/verbose) (top))
          ".quiet"  (do (util/concise) (top))

          ".nu"     (new-universe)

          (catch-all cmd help) )))

(defn new-universe []
  (declare u-help)
  (declare add-industry)
  (swap! *from* (fn [_] new-universe))
  (if-let [cmd     (do (print (str "universe" "> ")) (flush) (read-line))]
    (let  [
           abbrev  (fn [q] (->> (str/split q #" +") (map first) (reduce str)))
                                                 
           wrapper (fn [x] (str (abbrev x) "\t" x))
          ]
      (case (-> cmd (str/split #" ") (first))
            ".h"  (u-help)
            ".q"  (quitt)
            ".u"  (top)
            ".li" (do (-> (slurp "resources/industry-abbreviations.txt")
                          (println))
                      (@*from*))
            ".ai" (do (println "this may take a minute...")
                      (add-industry cmd))
          (catch-all cmd help)))))

(defn add-industry [cmd]
  (let [
        [_ i-abbrev] (str/split cmd #" +") 
        tics (industry-tickers (util/tfmt i-abbrev))
       ]
    (doseq [tic tics]
      (swap! *inventory* (fn [inv] (conj inv [tic (k-mkt tic)]))))
    (swap! *name* (fn [_] "fresh"))
    (bag)
))

(defn sku [t]
  (declare shmelp)
  (declare quitt)
  (declare bag)
  (declare window)
  ;(swap! *from* (fn [_] (sku t)))
  (if-let [cmd (do (print (str "." @*name* "> ")) (flush) (read-line))]
    (do (case (-> cmd (str/split #" ") (first))
              ".u"  (do (swap! *name* (fn [_] @*pname*)) (@*from*))
              ".q"  (quitt)

              ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))

              ".l"  (do (if (> (count @*fridge*) 0)
                            (println (reduce #(str %1 "\n" %2) (keys @*fridge*)))
                            (println "fridge is empty."))
                        (sku t))

              ".w"  (window cmd)

              (if (cname cmd)
                  (do  (swap! *name* (fn [_] (util/tfmt cmd)))
                       (sku @*name*))
                  (if (contains? @*fridge* cmd)
                      (do (swap! *inventory* (fn [_] (get @*fridge* cmd)))
                          (swap! *name* (fn [_] cmd))
                          (bag))
                      (do (println (case cmd
                      ".verbose"  (do (util/verbose))
                      ".concise"  (do (util/concise))
                       ".mov"     (mov t)
                       ".mvs"     (mvs t) 
                       ".p"       (p t)
                       ".pr"      (prices t)
                       ".pr 15m"  (prices t "15Min?")
                       ".pr 5m"   (prices t "5Min?")
                       ".pr 1m"   (prices t "1Min?")
                       ".fpr"     (fprices t)
                       ".fpr 15m" (fprices t "15Min?")
                       ".fpr 5m"  (fprices t "5Min?")
                       ".fpr 1m"  (fprices t "1Min?")
                       ".ngm"     (ngm t)
                       ".cff"     (cffd t)
                       ".mkt"     (mkt t)
                       ".cname"   (cname t)
                       ".i"       (i t)
                       ".web"     (web t)
                       ".oweb"    (oweb t)
                       ".yahoo"   (yahoo t)
                       ".sec"     (sec t)
                       ".desc"    (desc t)
                       ".ceo"     (ceo t)
                       ".s"       (s t)
                       ".emp"     (emp t)
                       ".so"      (so t)
                       ".pf"      (pf t)
                       ".v"       (v t)
                       ".b"       (b t)
                       ".c"       (c t)
                       ".d"       (d t)
                       ".r"       (r t)
                       ".g"       (g t)
                       ".e"       (e t)
                       ".d2e"     (d2e t)
                       ".ir"      (ir t)
                       ".db"      (db t)
                       ".sp"      (sp t)
                       ".fo"      (fo t)
                       ".io"      (io t)
                       ".cc"      (cc t)
                       ".refresh" (app/refresh t)
                       ".h"       (shmelp)
                       "")) (when (not (= ".q" cmd)) (sku t)) ))) ))))

(defn bag []
  (declare blelp)
  (declare quitt)
  (declare catch-all)
  (declare window)
  (declare fill-bag)
  (declare add-to-bag)
  (declare drop-from-bag)
  (declare sort-table)
  (swap! *from* (fn [_] bag))
  (if-let [cmd (do (print (str ":" @*name* "> ")) (flush) (read-line))]
    (case (-> cmd (str/split #" ") (first))

          ".p"        (do (println ".p not yet implimented") (@*from*))

          ".verbose"  (do (util/verbose) (bag))
          ".concise"  (do (util/concise) (bag))

          ".u"  (top)
          ".q"  (quitt)

          ".h"   (blelp)
          ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))

          ".w"  (window cmd)
          ".b"  (fill-bag cmd)
          ".a"  (add-to-bag cmd)
          ".d"  (drop-from-bag cmd)

          ".count" (do (println (count @*inventory*)) (bag))

          ".l"  (do (if (> (count @*inventory*) 0)
                        (println (reduce #(str %1 "\n" %2) (map #(str (first %) "\t" (cname (first %))) @*inventory*)))
                        (println "no inventory."))
                    (bag))

          ".n"  (do (if (> (count @*fridge*) 0)
                        (println (reduce #(str %1 "\n" %2) (keys @*fridge*)))
                        (println "fridge is empty."))
                    (bag))

          ".s" (let [
                     retry     (fn [] (do (println "usage: '.s <name>") (flush) (bag)))
                     is-ticker (fn [n] (do (println (str n " is a ticker.")) (flush) (bag)))
                     bb (str/split cmd #" ")
                    ]
                 (if (= (count bb) 2)
                     (let [name (second bb)] 
                       (if (cname name)
                           (is-ticker name)
                           (do (swap! *fridge* (fn [fridge] (assoc fridge name @*inventory*)))
                               (swap! *name* (fn [_] name))
                               (bag))))
                     (retry)))

          ".map" (let [xs (rest (str/split cmd #" "))
                        s (if (= (count xs) 1)
                              (str "mug.core/" (first xs))
                              (reduce #(str %1 " " %2)  xs))
                        f (eval (read-string s))] 
                   (doseq [t @*inventory*]
                     (println (str (first t) "\t" (f (first t)))))
                   (bag))

          ".m" (let [xs (rest (str/split cmd #" "))
                     fun (fn [x] (eval (read-string (str "mug.core/" x))))
                     eat (fn [x] (let [y (str/split x #"!")] 
                                   (if (= (count y) 2)
                                       (let [[a b] y
                                                fa (fun (first y))
                                                fb (fun (second y))]
                                         (fn [booger] (-> (/ (float (fa booger)) (float (fb booger))) (* 100.0) (int) (/ 100.0) (max 0.0))))
                                       (fun x))))
                     pop (fn [x] (let [y (str/split x #"\*")] 
                                   (if (= (count y) 2)
                                       (let [[a b] y
                                                fa (fun (first y))
                                                fb (fun (second y))]
                                         (fn [booger] (-> (* (float (fa booger)) (float (fb booger))) (* 100.0) (int) (/ 100.0) (max 0.0))))
                                       (fun x))))
                     funs (map eat xs)] 
                 (if (> (count @*inventory*) 0)
                     (let [header  (str "t\t" (reduce #(str % "\t" %2) xs))
                           newline (fn [t] (str (first t) (reduce str (map (fn [f] (str "\t" (f (first t)))) funs))))] 
                       (println header)
                       (swap! *bag-buffer* (fn [_] [header]))
                       (doseq [t @*inventory*]
                         (println (newline t))
                         (swap! *bag-buffer* (fn [bag-so-far] (conj bag-so-far (newline t))))))
                     (println "need something to map."))
                 (bag))

          ".srt" (do (sort-table) (bag))

          (catch-all cmd blelp) )))

(defn catch-all [cmd helpp]
  (let [cmdlist (str/split cmd #" ")]
    (if (cname (first cmdlist))
        (if (= 1 (count cmdlist))
            (do (swap! *pname* (fn [_] @*name*))
                (swap! *name* (fn [_] (util/tfmt cmd)))
                (sku cmd))
            (if (= 2 (count cmdlist))
                (let [[t c] cmdlist] (do (println ((eval (read-string (str "mug.core/" c))) t)) (flush) (@*from*)))
                (do (println "too many arguments?") (flush) (@*from*))))
        (if (contains? @*fridge* cmd)
            (do (swap! *inventory* (fn [_] (get @*fridge* cmd)))
                (swap! *pname* (fn [_] @*name*))
                (swap! *name* (fn [_] cmd))
                (bag))
            (do (println (str "\n No such name: " cmd)) (helpp))))))

(defn window [cmd]
  (let [
        retry (fn [] (do (println "usage: 'w low high") (flush) (@*from*)))
        bb    (str/split cmd #" ")
       ]
    (if (= (count bb) 3)
        (let [[_ l h] bb] 
          (if (and l h)
              (let [ll (rs l)
                    hh (rs h)]
                (if (and (= (type ll) java.lang.Long) (= (type hh) java.lang.Long) (> hh ll))
                    (do (swap! *inventory* (fn [_] (w ll hh)))
                        (swap! *name* (fn [_] "fresh"))
                        (swap! *fridge* (fn [fridge] (assoc fridge "fresh" @*inventory*))) 
                        (w ll hh)
                        (bag))
                    (retry)))
              (retry)))
        (retry))))

(defn fill-bag [cmd]
  (let [
        retry  (fn [] (do (println "usage: 'b tic1 tic2 ...") (flush) (top)))
        retry2 (fn [] (do (println "did you enter an incorrect ticker?") (flush) (top)))
        bb    (str/split cmd #" ")
       ]
    (if (> (count bb) 1)
        (let [[_ & tics] bb] 
          (if (check-all-true (map t? tics))
              (do (swap! *inventory* (fn [_] (map (fn [tic] [(util/tfmt tic) (mkt tic)]) tics)))
                  (swap! *name* (fn [_] "fresh"))
                  (swap! *fridge* (fn [fridge] (assoc fridge "fresh" @*inventory*)))
                  (bag))
               (retry2)))
        (retry))))

(defn add-to-bag [cmd]
  (let [
        retry  (fn [] (do (println "usage: 'a tic1 tic2 ...") (flush) (top)))
        retry2 (fn [] (do (println "did you enter an incorrect ticker?") (flush) (top)))
        bb    (str/split cmd #" ")
       ]
    (if (> (count bb) 1)
        (let [[_ & tics] bb] 
          (if (check-all-true (map t? tics))
              (do (swap! *inventory* (fn [old] (concat old (map (fn [tic] [(util/tfmt tic) (mkt tic)]) tics))))
                  (swap! *name* (fn [_] "fresh"))
                  (swap! *fridge* (fn [fridge] (assoc fridge "fresh" @*inventory*)))
                  (bag))
               (retry2)))
        (retry))))

(defn drop-from-bag [cmd]
  (let [
        retry  (fn [] (do (println "usage: 'd tic1") (flush) (top)))
        retry2 (fn [] (do (println "did you enter an incorrect ticker?") (flush) (top)))
        bb    (str/split cmd #" ")
       ]
    (if (= (count bb) 2)
        (let [[_ tic] bb] 
          (if (t? tic)
              (do (swap! *inventory* (fn [old] (remove (fn [[x y]] (util/t=? x tic)) old)))
                  (swap! *name* (fn [_] "fresh"))
                  (swap! *fridge* (fn [fridge] (assoc fridge "fresh" @*inventory*)))
                  (bag))
               (retry2)))
        (retry))))

(defn sort-table []
  (let [
        [head & body] @*bag-buffer*
        f (fn [s] (->> (str/split s #"\t") (second) (read-string)))
       ]
    (println head)
    (doseq [s (reverse (sort-by f body))]
      (println s))))


(defn u-help 
  "commands for creating and limiting the universe of companies"
  []
  (do (println 
"
 .h            this help message
 .ai abc       limit the universe to specific industries
 .li           list all industries
 .q            quit Mug!
 .u            up to top level
"
  ) (flush))
  (@*from*))


(defn help []
  (do (print "
 .h            this help message
 .doc          open Mud documentation
 .h cmd        list of ticker-specific commands
 .sl <cname>   symbol lookup by company name
 .noisy        (state: show external data-usage alerts)
 .quiet        (state: hide external data-usage alerts)
 .w low high   create un-named set, window of mkt caps
 .b            bag, create a set (bag) by listing tickers
 .a            add a ticker or tickers to the set (bag)
 .d            delete a ticker from the set (bag)
 <ticker> h    result of ticker-specific commands 
 <ticker> <cmd>  evaluates command (cmd) of name
 .l            list named sets and pairs
 .p <t> <t>    create un-named pair (not yet implimented)
 <name>        load symbol or named set or pair
 .q            to quit.\n\n") (flush))
  (top)
)

(defn shmelp []
  (do (print 
"\n <name>        load symbol or named set or pair 
 .h            this help message
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .q            to quit.
 .cname company name
 .bas   basket
 .pr     prices 1D
 .pr x   prices 15m 5m 1m
 .fpr    fresh prices 1D
 .fpr x  fresh prices 15m 5m 1m
 .ngm   normalized gap moves
 .i     industry
 .web   website
 .oweb  open website
 .desc  description
 .ceo   ceo
 .s     sector
 .emp   employees
 .so    sharesoutstanding
 .pf    publicfloat
 .v     avg30Volume
 .b     beta
 .mkt   marketcap
 .c     cash
 .d     debt
 .r     revenue
 .g     gross
 .e     ebitda
 .d2e   dbtoeqty
 .ir    insider roster
 .db    deep book
 .sp    splits
 .fo    fund ownership
 .cff   cash flow financing
 .io    institutional-ownership
 .cc    ceo-compensation\n") (flush))
  (@*from*)
)

(defn blelp []
  (do (print "
 <name>        load symbol or named set or pair 
 <name> <cmd>  evaluates command (cmd) of name
 .h            this help message
 .doc          open Mud documentation
 .h cmd        list of commands of the type (cmd ticker)
 .sl <cname>   symbol lookup by company name
 <ticker> l    lists ticker-specific commands
 .w low high   create un-named set, window of mkt caps
 .b            bag, create a set (bag) by listing tickers
 .a            add a ticker or tickers to the set (bag)
 .d            delete a ticker from the set (bag)
 .u            up to top
 .doc          Mug documentation
 .count       companies in set (bag)
 .map         displays table of single attribute by ticker for set
 .m           same as .map but accepts multiple attributes
 .srt         sorts (reverse) .map-generated or .m-generated table by first data column
 .p <t> <t>    create un-named pair (not yet implimented)
 .noisy        (state: show data usage points)
 .quiet        (state: hide data usage points)
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .s <n>        name the current set or pair
 .q            to quit.\n\n") (flush))
  (bag)
)

(defn quitt [] 
  (do (println "Thanks for using Mug.\nGoodbye.")))

