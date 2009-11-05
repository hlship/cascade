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

(ns #^{:doc "Cached results of invariant view fragments"} 
  cascade.view-cache
  (:use
    (cascade logging)))
  
(def cache (atom {}))

(defn update-view-cache
  "Updates the cache (using swap!) then returns the value."
  [key value]
  (swap! cache assoc key value)
  value) 

(defmacro read-view-cache
  "Reads the cache for a particular key. Returns the cached value if present. If the cached value is nil,
  then invokes the supplier function (with no arguments) to produce the value to cache, then
  uses swap! to store the value into the cache for later invocations."
  [key form]
  `(or (get @cache ~key) (update-view-cache ~key ~form))) 

(defn clear-view-cache
  "Clears the cache. This is used when a view namespace is invalidated (changes on disk). Perhaps in the future,
  we'll relate which keys are associated with which namespaces and be more careful about what we clear."
  []
  (debug "Clearing view cache.")
  (reset! cache {}))
