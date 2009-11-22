; Copyright 2009 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns cascade.test-integration
  (:import
    (com.thoughtworks.selenium Selenium DefaultSelenium HttpCommandProcessor)
    (org.openqa.selenium.server RemoteControlConfiguration SeleniumServer))    
  (:use
    (clojure test)
    (cascade jetty)))

(def #^Selenium selenium)

(defn start-selenium-server
  []
  (doto (SeleniumServer.) .start))

(defn start-selenium-client
  []
  (let [cp (HttpCommandProcessor. 
            "localhost" 
            (RemoteControlConfiguration/DEFAULT_PORT)
            "*firefox"
            "http://localhost:9090/")
        client (DefaultSelenium. cp)]
    (.start client)
    client))                    
          
(defn run-selenium-fixture
  [f]
  (let [jetty (start-jetty-server "src/test/webapp" 9090 'app1.views)
        ss (start-selenium-server)
        client (start-selenium-client)]
    (try
      (binding [selenium client]
        (f))
      (finally
        (.stop client)
        (.stop ss)
        (.stop jetty)))))
        
(use-fixtures :once run-selenium-fixture)

(defn open
  "Open the root page of the application."
  []
  (.open selenium "http://localhost:9090"))

(defn wait-for-page-to-load
  "Wait up to 15 seconds for the page to load after the previous operation."
  []
  (.waitForPageToLoad selenium "15000"))
  
(defn click-link
  "Click a link based on its label."
  [label]
  (.click selenium (str "link=" label))
  (wait-for-page-to-load))

(deftest test-index-page
  (open)
  (is (= (.getTitle selenium) "Test Application"))
  (click-link "Simple Output")
  (is (= (.getTitle selenium) "It works!")))