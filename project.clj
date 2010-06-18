(defproject com.howardlewisship.cascade/cascade-core "1.0.0-SNAPSHOT"
  :description "Simple, fast, easy web applications in idiomatic Clojure"
  :url "http://github.com/hlship/cascade/"
  :dependencies 
    [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
     [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
     [org.slf4j/slf4j-api "1.5.2"]     
     [org.slf4j/slf4j-log4j12 "1.5.2"]
     [log4j/log4j "1.2.14"]]
  :dev-dependencies
    [[org.eclipse.jetty/jetty-server "7.0.0.RC4"]
     [org.eclipse.jetty/jetty-servlet "7.0.0.RC4"]     
     [org.easymock/easymock "2.5.1"]
     [org.seleniumhq.selenium.server/selenium-server "1.0.3" :classifier "standalone"]]
  :aot :all  ;  cascade.filter
  :main cascade.version
  :warn-on-reflection true
  :source-path "src/main/clojure"
  :test-path "src/test/clojure"
  :resources-path "src/main/resources"
  :test-resources-path "src/test/resources"
  :compile-path "target/classes")
