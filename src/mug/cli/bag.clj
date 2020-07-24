(in-ns 'mug.cli)

(defn sort-table 
  "used in kt and dt"
  []
  (let [
        [head & body] @*bag-buffer*
        f (fn [s] (->> (str/split s #"\t") (second) (read-string)))
       ]
    (swap! *index* (fn [_] 0))
    (println (str "0\t" head))
    (doseq [s (reverse (sort-by f body))]
      (swap! *index* (fn [i] (+ 1 i)))
      (println (str @*index* "\t" s)))))

(defn sort-table-quiet 
 "used in kt and dt"
  []
  (let [subset []
        [head & body] @*bag-buffer*
        f (fn [s] (->> (str/split s #"\t") (second) (read-string)))
       ]
    (swap! *subset* (fn [_] [head]))
    (doseq [s (reverse (sort-by f body))]
      (swap! *subset* (fn [ss] (conj ss s))))
    (swap! *bag-buffer* (fn [_] @*subset*))))

(defn keep-top 
  "keep-or-drop-top helper"
  [num] 
  (swap! *inventory* (fn [inventory]
    (let [f (fn [t] (->> t (symbol) (get (into {} inventory))))]                 
      (->> (take num (rest @*bag-buffer*))
           (map (fn [s] (str/split s #"\t")))
           (map (fn [[t x]] [(symbol t) (f t)]))
      )))))

(defn drop-top 
  "keep-or-drop-top helper"
  [num] 
  (swap! *inventory* (fn [inventory]
    (let [m (fn [t] (->> t (symbol) (get (into {} inventory))))]
      (->> (drop num (rest @*bag-buffer*))
           (map (fn [s] (str/split s #"\t")))
           (map (fn [[t x]] [(symbol t) (m t)])))))))

(defn keep-or-drop-top 
  "used in bag kt and dt: func is keep-top or drop-top"
  [cmd func]
  (declare bag)
  (do (sort-table-quiet) 
      (if (or (= [] @*bag-buffer*) (= [nil] @*bag-buffer*))
          (println "you first needto use the .m command to create a table")
          (if (and (= 2 (count (str/split cmd #" +")))
                   (= java.lang.Long (type (read-string (second (str/split cmd #" +"))))))
              (func (read-string (second (str/split cmd #" +")))) 
              (func 8)))
      (bag)))

(defn window [cmd]
  (let [
        rs read-string
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
        retry  (fn [] (do (println "usage: 'b tic1 tic2 ...") (flush) (@*from*)))
        retry2 (fn [] (do (println "did you enter an incorrect ticker?") (flush) (@*from*)))
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

(defn bag-case 
  [cmd] 
  (case (-> (str cmd) (str/split #" ") (first))

          ".p"      (let [[a b c] (str/split cmd #" ")]
                      (make-pair b c) 
                      (@*from*))

          ".cmds"   (do (println hlp/cmds)
                        (@*from*))

          ".noisy"  (do (util/verbose) (@*from*))
          ".quiet"  (do (util/concise) (@*from*))

          ".u"  (top)
          ".q"  (quitt)

          ".h"  (let [choice (do (print hlp/b-help-sub) 
                                 (flush)
                                 (read-line))]
                      (case (first choice)
                            \1 (do (print hlp/b-help-bag) (@*from*))
                            \2 (do (print hlp/b-help-table) (@*from*))
                            \3 (do (print hlp/b-help-nav) (@*from*))
                            (bag)))


          ".doc" (do (browse-url "./doc/mug.pdf") (@*from*))

          ".w"  (window (str cmd))
          ".b"  (fill-bag (str cmd))
          ".a"  (add-to-bag (str cmd))
          ".d"  (drop-from-bag (str cmd))

          ".count" (do (println (count @*inventory*)) (@*from*))

          ".l"  (do (if (> (count @*inventory*) 0)
                        (println (reduce #(str %1 "\n" %2) (map #(str (first %) "\t" (cname (first %))) @*inventory*)))
                        (println "no inventory."))
                    (@*from*)
                )

          ".n"  (do (if (> (count @*fridge*) 0)
                        (println (reduce #(str %1 "\n" %2) (keys @*fridge*)))
                        (println "fridge is empty."))
                    (@*from*))

          ".s" (let [
                     retry     (fn [] (do (println "usage: '.s <name>") (flush) (@*from*)))
                     is-ticker (fn [n] (do (println (str n " is a ticker.")) (flush) (@*from*)))
                     bb        (str/split (str cmd) #" ")
                    ]
                 (if (= (count bb) 2)
                     (let [name (second bb)] 
                       (if (cname name)
                           (is-ticker name)
                           (do (swap! *fridge* (fn [fridge] (assoc fridge name @*inventory*)))
                               (swap! *name* (fn [_] name))
                               (@*from*))))
                     (retry)))

          ".map" (let [xs (rest (str/split (str cmd) #" "))
                        s (if (= (count xs) 1)
                              (str "mug.core/" (first xs))
                              (reduce #(str %1 " " %2)  xs))
                        f (eval (read-string s))] 
                   (doseq [t @*inventory*]
                     (println (str (first t) "\t" (f (first t)))))
                   (@*from*)
                 )

          ".m" (let [xs (rest (str/split (str cmd) #" "))
                     function? (fn [s] 
                                 (util/in? 
                                   ["emp" "so" "pf" "v" "b" "mkt" "c" "d" "r" "g" "e" "d2e" "cc" "mov" "mvs" "mvx" "mvd" "bw" "vs" "tvs" "web" "i" "s" "ceo" "cff" "zc" "cname" "p"] 
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
                       (swap! *index* (fn [_] 0))
                       (println (str @*index* "\t" header))
                       (swap! *bag-buffer* (fn [_] [header]))
                       (doseq [t @*inventory*]
                         (swap! *index* (fn [i] (+ 1 i)))
                         (println (str @*index* "\t" (newline t)))
                         (swap! *bag-buffer* (fn [bag-so-far] (conj bag-so-far (newline t))))))
                     (println "need something to map."))
                 (@*from*)
               )

          ".srt" (do (sort-table) (@*from*)
                 )

          ".kt" (do (sort-table-quiet) 
                    (keep-or-drop-top cmd keep-top)
                    (@*from*)
                )

          ".dt" (do (sort-table-quiet) 
                    (keep-or-drop-top cmd drop-top)
                    (@*from*)
                )

          (catch-all (str cmd))))
