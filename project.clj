(defproject clojure-genetic-programming "0.1.0-SNAPSHOT"
  :description "A small genetic programming experiment in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "GPL v3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot galapagos.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
