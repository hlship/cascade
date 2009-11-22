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

(defn is-title
  [expected]
  (is (= (.getTitle selenium) expected)))
  
(defn is-text
  [locator expected]
  (is (= (.getText selenium locator) expected)))

(deftest test-index-page
  (open)
  (is-title "Test Application")
  (click-link "Simple Output")
  (is-title "It works!")
  (click-link "index")
  (is-title "Test Application"))
  
(deftest simple-actions
  (open)
  (click-link "Simple Actions")
  (is-title "Current Count")
  (is-text "current" "0")
  (click-link "here to increment")
  (is-text "current" "1")
  (click-link "here to decrement")
  (is-text "current" "0")
  (click-link "here to reset")
  (is-text "current" "1"))
  
(deftest simple-javascript
  (open)
  (click-link "Simple JavaScript")
  (is-text "message" "ready event did fire."))
  
(deftest context-image
  (open)
  (click-link "Context Image")
  (is-title "Local Image")
  (let [path (.getAttribute selenium "//img[1]/@src")]
    ; TODO: Read content directly and compare to on-disk.
    (is (.endsWith path "/images/clojure-logo.png"))))
            
(deftest exception-report
  (open)
  (click-link "Forced Exception")
  (is-title "An unexpected exception has occurred.")
  (.isTextPresent selenium "Divide by zero")
  (.isTextPresent selenium "java.lang.ArithmeticException"))
  
              