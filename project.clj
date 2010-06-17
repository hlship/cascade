(defproject com.howardlewisship.cascade/cascade-core "1.0.0-SNAPSHOT"
  :description "Simple, fast, easy web applications in idiomatic Clojure"
  :url "http://github.com/hlship/cascade/"
  :dependencies 
    [[org.clojure/clojure "1.1.0"]
     [org.clojure/clojure-contrib "1.1.0"]
     [org.slf4j/slf4j-api "1.5.2"]
     
     [org.slf4j/slf4j-log4j12 "1.5.2"]
     [log4j/log4j "1.2.14"]]
  :dev-dependencies
    [[org.eclipse.jetty/jetty-server "7.0.0.RC4"]
     [org.eclipse.jetty/jetty-servlet "7.0.0.RC4"]     
     [org.easymock/easymock "2.5.1"]
     [org.seleniumhq.selenium.client-drivers/selenium-java-client-driver "1.0.3"]
     ; oops, need the right classification of this, not yet supported
     [org.seleniumhq.selenium.server/selenium-server "1.0.3"] 
     [org.seleniumhq.selenium.server/selenium-server-coreless "1.0.3"]]
  :aot :all
  :warn-on-reflection true
  :source-path "src/main/clojure"
  :test-path "src/test/clojure"
  :resources-path "src/main/resources"
  :jar-dir "target")
