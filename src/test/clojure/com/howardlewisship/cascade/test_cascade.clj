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

(ns com.howardlewisship.cascade.test-cascade
(:use 
  com.howardlewisship.cascade
    (com.howardlewisship.cascade dom)
    (com.howardlewisship.cascade.internal utils)
    (clojure.contrib test-is duck-streams)))

(defn render
  [dom]
  (with-out-str (render-xml dom *out*)))
 
(defn minimize-ws [string]
  (.replaceAll string "\\s+" " "))
  
 
(defn render-test
  [view-fn name env]
  (let [input-path (str "expected/" name ".xml")
        expected (slurp* (find-classpath-resource input-path))
        trimmed-expected (minimize-ws expected)
        dom (view-fn env)
        rendered (render dom)
        trimmed-render (minimize-ws rendered)]
      (is (= trimmed-render trimmed-expected))))


(defview simple-view
  [env] 
  :p [ (env :message) ])

(deftest simple-defview
	(render-test simple-view "simple-defview" {:message "Embedded Template"}))
	
(defview attributes-view
  [env]
  :p {:id "outer"} [
    :em {:id (env :inner) } [
      (env :message)
     ]
     "\r"
     :hr
     (env :copyright)
  ])	
	
(deftest attribute-rendering
  (render-test	attributes-view "attribute-rendering" {:message "Nested Text" 
    :copyright "(c) 2009 HLS"
    :inner "frotz" }))
	