(defproject mug "0.3.0-cli version"
  :description "cli interface to iexcloud api, alpaca api, alphavantage api"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [clj-http "3.10.0"]
                ]
  :java-source-paths ["src/java" "src/java/Jama"]
  :main mug.cli
  :aot [mug.cli]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
