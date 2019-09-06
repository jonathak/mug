(ns mug.examples
  (:import Crap Fart Jama.Matrix Jama.SingularValueDecomposition)
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [mug.util :as util]
            [alpaca.core :as alp]
            [clojure.test :as test]
            [mug.core :as core]
  )
  (:gen-class))

()

(defn -main
  "functions associated with the iexcloud api"
  [& args]
  (println "use lein repl, test with lein test"))
