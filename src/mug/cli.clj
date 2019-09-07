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
  (swap! *from* (fn [_] top))
  (if-let [cmd (do (print (str "top" "> ")) (flush) (read-line))]
    (case (-> cmd (str/split #" ") (first))
          ".h"  (if (= 2 (count (str/split cmd #" "))) 
                    (if (= "cmd" (second (str/split cmd #" "))) 
                        (shmelp)
                        (help))
                    (help))
                    
          ".q"  (quitt)

          ".w"  (window cmd)
          ".b"  (fill-bag cmd)
          ".a"  (add-to-bag cmd)
          ".d"  (drop-from-bag cmd)

          ".l"  (do (if (> (count @*fridge*) 0)
                        (println (reduce #(str %1 "\n" %2) (keys @*fridge*)))
                        (println "fridge is empty."))
                        (top))

          ".verbose"  (do (util/verbose) (top))
          ".concise"  (do (util/concise) (top))

          (catch-all cmd help) )))

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
  (swap! *from* (fn [_] bag))
  (if-let [cmd (do (print (str ":" @*name* "> ")) (flush) (read-line))]
    (case (-> cmd (str/split #" ") (first))

          ".verbose"  (do (util/verbose) (bag))
          ".concise"  (do (util/concise) (bag))

          ".u"  (top)
          ".q"  (quitt)

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
                     (do (println (str "t\t" (reduce #(str % "\t" %2) xs)))
                         (doseq [t @*inventory*]
                           (println (str (first t) (reduce str (map (fn [f] (str "\t" (f (first t)))) funs))))))
                     (println "need something to map."))
                 (bag))

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
                        (wh ll hh)
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


(defn help []
  (do (print "
 <name>        load symbol or named set or pair 
 <name> <cmd>  evaluates command (cmd) of name
 .h            this help message
 .h cmd        list of commands 
 .l            list named sets and pairs
 .w low high   create un-named set, window of mkt caps
 .b            bag, create a set (bag) by listing tickers
 .a            add a ticker or tickers to the set (bag)
 .d            delete a ticker from the set (bag)
 .p <t> <t>    create un-named pair
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
 .h            this help message
 .n            list named sets and pairs
 .l            list members of loaded set or pair
 .w low high   create un-named set
 .p <t> <t>    create un-named pair
 .s <n>        name the current set or pair
 .q            to quit.\n\n") (flush))
  (bag)
)

(defn quitt [] 
  (do (println "Thanks for using Mug.\nGoodbye.")))

