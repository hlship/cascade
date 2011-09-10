(def jetty-version "7.0.0.RC4")
(def slf4j-version "1.5.2")

(defproject cascade "0.2-SNAPSHOT"
  :description "Simple, fast, easy web applications in idiomatic Clojure"
  :url "http://github.com/hlship/cascade"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.slf4j/slf4j-api ~slf4j-version]
                 [org.slf4j/slf4j-log4j12 ~slf4j-version]
                 [org.eclipse.jetty/jetty-server ~jetty-version]
                 [org.eclipse.jetty/jetty-servlet ~jetty-version]]
  :dev-dependencies [[org.easymock/easymock "2.5.1"]
                     [org.seleniumhq.selenium.server/selenium-server "1.0.3" :classifier "standalone"]])