(in-ns 'mug.cli)

(defn top-case [cmd]
   (case (-> cmd (str/split #" ") (first))

          ".h"  (do (print hlp/t-help) (top))

          ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))
                    
          ".q"  (quitt)

          ".w"  (window cmd)
          ".b"  (fill-bag cmd)
          ".a"  (add-to-bag cmd)
          ".d"  (drop-from-bag cmd)

          ".p"  (let [[a b c] (str/split cmd #" ")]
                  (make-pair b c) 
                  (@*from*))

          ".sl" (do (println (if (= 2 (count (str/split cmd #" ")))
                        (let [[x y] (str/split cmd #" ")]
                          (app/sl y))
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

          ".eu"     (universe)
          ".su"     (show-universe)

          ".dow"    (do (println "this might take a few seconds ... ")
                        ;(dow) 
                        ;(swap! *name* (fn [_] "dow")) 
                        ;(swap! *fridge* (fn [fridge] (assoc fridge "dow" @*inventory*)))
                        (bag))

          (catch-all cmd)))
