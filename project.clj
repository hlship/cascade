(def ring-version "0.3.11")

(defproject cascade "0.2-SNAPSHOT"
  :description "Simple, fast, easy web applications in idiomatic Clojure"
  :url "http://github.com/hlship/cascade"
  :source-path "src/main/clojure"
  :resources-path "src/main/resources"
  :test-path "src/test/clojure"
  :dev-resources-path "src/test/resources"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [ring/ring-core ~ring-version]
                 [ring/ring-jetty-adapter ~ring-version]]
  :dev-dependencies [[ring/ring-devel ~ring-version]])