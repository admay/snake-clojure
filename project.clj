(defproject snake "1.0.0"
  :description "Snake in Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot snake.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
