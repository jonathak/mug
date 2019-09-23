(in-ns 'mug.cli)

(defn make-pair "for comparing two companies"
  [t1 t2]
  (declare pair)
  (swap! *pair* (fn [_] [(util/tfmt t1) (util/tfmt t2)]))
  (do (combine-rates t1 t2))
  (pair)
)

(defn pair-case 
  [cmd]
  (let [[a b c] (-> cmd (str/split #" +"))]
        (case a
          ".h"      (do (print hlp/p-help) (@*from*))
          ".u"      (top)
          ".prices" (do (fprices (first @*pair*)) (fprices (second @*pair*)) (pair)) ;freshprices
          ".rates"  (do (rates (first @*pair*)) (rates (second @*pair*)) (pair)) ;rates
          ".align"  (do (combine-rates (first @*pair*) (second @*pair*) (pair)))
          ".q"      (quitt)
          ".s" (let [
                     retry     (fn [] (do (println "usage: '.s <name>") (flush) (@*from*)))
                     is-ticker (fn [n] (do (println (str n " is a ticker.")) (flush) (@*from*)))
                     bb        (str/split (str cmd) #" ")
                    ]
                 (if (= (count bb) 2)
                     (let [name (second bb)] 
                       (if (cname name)
                           (is-ticker name)
                           (do (swap! *cooler* (fn [cooler] (assoc cooler name @*inventory*)))
                               (swap! *name* (fn [_] name))
                               (@*from*))))
                     (retry)))
          (@*from*))))