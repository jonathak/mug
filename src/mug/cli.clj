(ns mug.cli
  (:require [mug.core :refer :all ]
            [mug.app        :as app]
            [mug.util       :as util]
            [mug.help       :as hlp]
            [alphav.core    :as alphav]
            [sec.core       :as sec]
            [clojure.string :as str]
            [clojure.java.browse :refer [browse-url]])
  (:use [mug.core])
  (:gen-class))

(defn -main
" a command line interface to extensive uniquely summarized
 public financial equity data mixed with analytical tools.
 requires individual API acces to multiple financial data
 providors including iexcloud.io alpaca.markets and
 alphavantage.co."
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
(def ^:dynamic *subset* (atom []))
(def ^:dynamic *universe*  (atom []))
(def ^:dynamic *history* (atom []))
(def ^:dynamic *prior* (atom ""))

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
  (declare quitt)
  (declare sku)
  (declare bag)
  (declare window)
  (declare fill-bag)
  (declare add-to-bag)
  (declare drop-from-bag)
  (declare edit-universe)
  (declare show-universe)
  (swap! *from* (fn [_] top))
  (if-let [cmd (do (print (str "top" "> ")) (flush) (read-line))]
    (do (swap! *history* (fn [x] (conj @*history* cmd)))
        (case (-> cmd (str/split #" ") (first))

          "\\"  (do (swap! *prior* (fn [_] (peek @*history*))) (top))

          ".h"  (do (print hlp/t-help) (top))

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

          ".noisy"  (do (util/verbose) (println "ok i will let you know when you consume points") (top))
          ".quiet"  (do (util/concise) (println "ok i will be quite about point consumption") (top))

          ".eu"     (edit-universe)
          ".su"     (show-universe)

          (catch-all cmd) ))))

(defn show-universe [] 
  (do (if (> (count @*universe*) 0)
        (println
          (reduce
            (fn [x y] (str x "\n" y)) 
            @*universe*))
        (println "the universe is empty"))
      (@*from*)))

(defn edit-universe []
  (declare add-industry)
  (swap! *from* (fn [_] edit-universe))
  (if-let [cmd     (do (print (str "universe" "> ")) (flush) (read-line))]
    (let  [
           abbrev  (fn [q] (->> (str/split q #" +") (map first) (reduce str)))
                                                 
           wrapper (fn [x] (str (abbrev x) "\t" x))
          ]
      (swap! *history* (fn [x] (conj @*history* cmd)))
      (case (-> (str @*prior* cmd) (str/split #" ") (first))
            "\\"  (do (print (peek @*history*)) 
                        ;(swap! *prior* (fn [x] (peek @*history*)))
                        ;(swap! *history* (fn [x] (pop x)))
                  )

            ".h"  (do (print hlp/u-help) (edit-universe))
            ".q"  (quitt)
            ".u"  (top)
            ".li" (do (-> (slurp "resources/industry-abbreviations.txt")
                          (println))
                      (@*from*))

            ".si" (do (->> (-> (slurp "resources/industry-abbreviations.txt")
                               (str/split #"\n"))
                           (filter 
                             (fn [x]
                                (re-find 
                                  (re-pattern 
                                    (str "(?ix) " (second
                                                    (str/split cmd #" +"))))
                                  x)))
                           ((fn [y] (if (= y '()) '("none") y)))
                           (reduce #(str % "\n" %2))
                           (println))
                      (@*from*))

            ".cl" (do (swap! *universe* (fn [_] []))
                      (println "the universe is empty")
                      (@*from*))

            ".ai" (do (println "this may take a minute...")
                      (add-industry cmd))

            ".su" (do (show-universe))

            ".cu" (@*from*)

          (catch-all cmd)))))

(defn add-industry [cmd]
  (let [
        [_ i-abbrev] (str/split cmd #" +") 
        tics (industry-tickers (util/tfmt i-abbrev))
       ]
    (doseq [tic tics]
      (swap! *universe* (fn [inv] (conj inv tic))))
    (@*from*)
))

(defn sku [t]
  (declare quitt)
  (declare bag)
  (declare window)
  ;(swap! *from* (fn [_] (sku t)))
  (if-let [cmd (do (print (str "." @*name* "> ")) (flush) (read-line))]
    (do (swap! *history* (fn [x] (conj @*history* cmd)))
        (case (-> (str @*prior* cmd) (str/split #" ") (first))
              "\\"  (do (print (peek @*history*)) 
                        ;(swap! *prior* (fn [x] (peek @*history*)))
                        ;(swap! *history* (fn [x] (pop x)))
            )
                        
              ".u"  (do (swap! *name* (fn [_] @*pname*)) (@*from*))
              ".q"  (quitt)

              ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))

              ".l"  (do (if (> (count @*fridge*) 0)
                            (println (reduce #(str %1 "\n" %2) (keys @*fridge*)))
                            (println "fridge is empty."))
                        (sku t))

              ".w"  (window cmd)
              ".h"  (let [choice (do (print hlp/s-help-sub) 
                                     (flush)
                                     (read-line))]
                      (case (first choice)
                            \1 (do (print hlp/s-help-sub-quant) (sku t))
                            \2 (do (print hlp/s-help-sub-web) (sku t))
                            \3 (do (print hlp/s-help-sub-find) (sku t))
                            \4 (do (print hlp/s-help-sub-info) (sku t))
                            \5 (do (print hlp/s-help-sub-history) (sku t))
                            \6 (do (print hlp/s-help-sub-nav) (sku t))
                            (sku t)))

              (if (cname (str @*prior* cmd))
                  (do  (swap! *name* (fn [_] (util/tfmt (str @*prior* cmd))))
                       (sku @*name*))
                  (if (contains? @*fridge* cmd)
                      (do (swap! *inventory* (fn [_] (get @*fridge* (str @*prior* cmd))))
                          (swap! *name* (fn [_] (str @*prior* cmd)))
                          (bag))
                      (do (println (case (str @*prior* cmd)
                      ".verbose"  (do (util/verbose))
                      ".concise"  (do (util/concise))
                       ".mov"     (mov t)
                       ".mvs"     (mvs t) 
                       ".vs"      (vs t)
                       ".tvs"     (tvs t)
                       ".p"       (p t)
                       ".pr"      (prices t)
                       ".pr 15m"  (prices t "15Min?")
                       ".pr 5m"   (prices t "5Min?")
                       ".pr 1m"   (prices t "1Min?")
                       ".fpr"     (fprices t)
                       ".fpr 15m" (fprices t "15Min?")
                       ".fpr 5m"  (fprices t "5Min?")
                       ".fpr 1m"  (fprices t "1Min?")
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
                       ".zc"      (zipcode t)
                       ".refresh" (app/refresh t)
                       "")) (when (not (= ".q" (str @*prior* cmd))) (sku t)) ))) ))))

(defn bag []
  (declare quitt)
  (declare catch-all)
  (declare window)
  (declare fill-bag)
  (declare add-to-bag)
  (declare drop-from-bag)
  (declare sort-table)
  (declare sort-table-quiet)
  (declare keep-top)
  (swap! *from* (fn [_] bag))
  (if-let [cmd (do (print (str ":" @*name* "> ")) (flush) (read-line))]
    (do (swap! *history* (fn [x] (conj @*history* cmd)))
        (case (-> (str @*prior* cmd) (str/split #" ") (first))

              "\\"  (do (print (peek @*history*)) 
                        ;(swap! *prior* (fn [x] (peek @*history*)))
                        ;(swap! *history* (fn [x] (pop x)))
                    )

          ".p"      (do (println ".p not yet implimented") (@*from*))

          ".cmds"   (do (println hlp/cmds)
                        (bag))

          ".noisy"  (do (util/verbose) (bag))
          ".quiet"  (do (util/concise) (bag))

          ".u"  (top)
          ".q"  (quitt)

          ".h"  (let [choice (do (print hlp/b-help-sub) 
                                 (flush)
                                 (read-line))]
                      (case (first choice)
                            \1 (do (print hlp/b-help-bag) (bag))
                            \2 (do (print hlp/b-help-table) (bag))
                            \3 (do (print hlp/b-help-nav) (bag))
                            (bag)))


          ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))

          ".w"  (window (str @*prior* cmd))
          ".b"  (fill-bag (str @*prior* cmd))
          ".a"  (add-to-bag (str @*prior* cmd))
          ".d"  (drop-from-bag (str @*prior* cmd))

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
                     bb (str/split (str @*prior* cmd) #" ")
                    ]
                 (if (= (count bb) 2)
                     (let [name (second bb)] 
                       (if (cname name)
                           (is-ticker name)
                           (do (swap! *fridge* (fn [fridge] (assoc fridge name @*inventory*)))
                               (swap! *name* (fn [_] name))
                               (bag))))
                     (retry)))

          ".map" (let [xs (rest (str/split (str @*prior* cmd) #" "))
                        s (if (= (count xs) 1)
                              (str "mug.core/" (first xs))
                              (reduce #(str %1 " " %2)  xs))
                        f (eval (read-string s))] 
                   (doseq [t @*inventory*]
                     (println (str (first t) "\t" (f (first t)))))
                   (bag))

          ".m" (let [xs (rest (str/split (str @*prior* cmd) #" "))
                     function? (fn [s] 
                                 (util/in? 
                                   ["emp" "so" "pf" "v" "b" "mkt" "c" "d" "r" "g" "e" "d2e" "cc" "mov" "mvs" "vs" "tvs" "web" "i" "s" "ceo" "cff" "zc"] 
                                   s))
                     fun (fn [x] (if (function? x)
                                     (eval (read-string (str "mug.core/" x)))
                                     (fn [_] 999)))
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

          ".kt" (do (sort-table-quiet) 
                    (if (or (= [] @*bag-buffer*) (= [nil] @*bag-buffer*))
                        (println "you first needto use the .m command to create a table")
                        (if (and (= 2 (count (str/split cmd #" +")))
                                 (= java.lang.Long (type (read-string (second (str/split cmd #" +"))))))
                            (keep-top (read-string (second (str/split cmd #" +")))) 
                            (keep-top 8)))
                    (bag)
                )

          ".kb" (do (bag))

          (catch-all (str @*prior* cmd)) ))))

(defn keep-top [num] 
  (swap! *inventory* (fn [_]
    (->> (take num (rest @*bag-buffer*))
         (map (fn [s] (first (str/split s #"\t"))))
         (map (fn [t] [t (mkt t)])))))
  ;(bag)
)

(defn catch-all [cmd]
  (let [cmdlist (str/split cmd #" ")]
    (if (cname (first cmdlist))
        (if (= 1 (count cmdlist))
            (do (swap! *pname* (fn [_] @*name*))
                (swap! *name* (fn [_] (util/tfmt cmd)))
                (sku cmd))
            (if (= 2 (count cmdlist))
                (let [[t c] cmdlist] (do (println (try
                                                    ((eval (read-string (str "mug.core/" (if (= c "cff") 
                                                                                             "cffd" 
                                                                                             c)))) 
                                                     t)
                                                    (catch java.lang.RuntimeException e "did you mispell a function?"))) 
                                         (flush) (@*from*)))
                (do (println "too many arguments?") (flush) (@*from*))))
        (if (contains? @*fridge* cmd)
            (do (swap! *inventory* (fn [_] (get @*fridge* cmd)))
                (swap! *pname* (fn [_] @*name*))
                (swap! *name* (fn [_] cmd))
                (bag))
            (do (if (= (first cmd) \.)
                    (println (str "\n that was not a valid command \n use .h for help "))
                    (if (= (str/replace cmd #" +" "") "") 
                        (@*from*)
                        (println (str "\n Did you just mispell a ticker? \n Commands start with '.' "))))
                (@*from*))))))

(defn window [cmd]
  (let [
        retry (fn [] (do (println "usage: .w low high") (flush) (@*from*)))
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

(defn sort-table-quiet []
  (let [subset []
        [head & body] @*bag-buffer*
        f (fn [s] (->> (str/split s #"\t") (second) (read-string)))
       ]
    (swap! *subset* (fn [_] [head]))
    (doseq [s (reverse (sort-by f body))]
      (swap! *subset* (fn [ss] (conj ss s))))
    (swap! *bag-buffer* (fn [_] @*subset*))))

(defn quitt [] 
  (do (println "Thanks for using Mug.\nGoodbye.")))

