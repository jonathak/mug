(in-ns 'mug.cli)

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

(defn catch-all [cmd]
  (let [cmdlist (str/split cmd #" ")]
    (if (cname (first cmdlist))
        (if (= 1 (count cmdlist))
            (do (swap! *priorname* (fn [_] @*name*))
                (swap! *name* (fn [_] (util/tfmt cmd)))
                (sku cmd))
            (if (= 2 (count cmdlist))
                (let [[t c] cmdlist] 
                  (do 
                    (println 
                      (try ((eval 
                              (read-string 
                                (str "mug.core/" 
                                  (if (= c "cff") "cffd" c)))) t)
                           (catch java.lang.RuntimeException e "did you mispell a function?"))) 
                    (flush) 
                    (@*from*)))
                (do (println "too many arguments?") (flush) (@*from*))))
        (if (contains? @*fridge* cmd)
            (do (swap! *inventory* (fn [_] (get @*fridge* cmd)))
                (swap! *priorname* (fn [_] @*name*))
                (swap! *name* (fn [_] cmd))
                (bag))
            (do (if (= (first cmd) \.)
                    (println (str "\n that was not a valid command \n use .h for help "))
                    (if (= (str/replace cmd #" +" "") "") 
                        (@*from*)
                        (println (str "\n Did you just mispell a ticker? \n Commands start with '.' "))))
                (@*from*))))))

