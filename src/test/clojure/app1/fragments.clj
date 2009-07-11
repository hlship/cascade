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

(ns app1.fragments
  (:use com.howardlewisship.cascade
        com.howardlewisship.cascade.dom))

(defn echo
  [env params]
  (struct-map dom-node
    :type :text
    :value (str (params :value))))
    
(defn tloop
  "Loop fragment used in testing (so as not to conflict with an eventual framework loop fragment)."
  [env params]
  (let [source (params :source)
        value-key (params :value)]
        (apply concat (for [value source]
                         (render-body (assoc env value-key value))))))

(defn mimic
  "Mimics an arbitrary element."
  [env params]
  (struct-map dom-node 
    :type :element 
    :name (keyword (params :element))
    :attributes (-> env :fragment-token :attributes) 
    :content (render-body env)))