(defproject cascade "0.2-SNAPSHOT"
  :description "Simple, fast, easy web applications in idiomatic Clojure"
  :url "http://github.com/hlship/cascade"
  :warn-on-reflection true
  :dependencies [[org.clojure/clojure "1.3.0"]
                    [clj-json "0.4.3"]
                 [compojure "0.6.5"]]
  :dev-dependencies [[ring/ring-jetty-adapter "0.3.11"]
                     [hiccup "0.3.7"]
                     [swank-clojure "1.3.1"]
                     [midje "1.1.1"]
                     [lein-ring "0.4.6"]]
  :ring {:handler test-app/app})
