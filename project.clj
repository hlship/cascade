(defproject cascade "0.2-SNAPSHOT"
  :description "Simple, fast, easy web applications in idiomatic Clojure"
  :url "http://github.com/hlship/cascade"
  :source-path "src/main/clojure"
  :resources-path "src/main/resources"
  :test-path "src/test/clojure"
  :dev-resources-path "src/test/resources"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/algo.monads "0.1.0"]
                 [compojure "0.6.5"]]
  :dev-dependencies [[ring/ring-jetty-adapter "0.3.11"]
                     [swank-coljure "1.3.1"]
                     [midje "1.1.1"]])
