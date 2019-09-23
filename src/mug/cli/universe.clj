(in-ns 'mug.cli)

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
  (filter i-test? (iex/iex-symbols!))))

(defn industry-tickers
  "gets all tickers in a given industry (using abbreviation)"
  [indust-abbrev]
  (let [
        indust  (industry? indust-abbrev)
        i-test? (fn [x] (= indust (i x)))
       ]
  (filter i-test? (map symbol (str/split (slurp "resources/iexsymbols.txt") #"\n")))))

(defn show-universe [] 
  (do (if (> (count @*universe*) 0)
        (println
          (reduce
            (fn [x y] (str x "\n" y)) 
            @*universe*))
        (println "the universe is empty"))
      (@*from*)))

(defn add-industry [cmd]
  (let [
        [_ i-abbrev] (str/split cmd #" +") 
        tics (industry-tickers (util/tfmt i-abbrev))
       ]
    (doseq [tic tics]
      (swap! *universe* (fn [inv] (conj inv tic))))
    (@*from*)
))

(defn universe-case 
  [cmd] 
  (case (-> (str cmd) (str/split #" ") (first))

            ".h"  (do (print hlp/u-help) (universe))
            ".q"  (quitt)
            ".u"  (@*from*)
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

          (catch-all cmd)))