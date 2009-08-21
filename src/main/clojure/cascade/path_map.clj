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

; Contains the basic dispatchers

(ns #^{:doc "Manages path prefixs to views and functions."}
  cascade.path-map
  (:use 
    (cascade config)
    (cascade.internal utils)))
      
(defn add-fn-to-dispatch-map
  "Maps a path to a view or action function. The path string is relative to the context root and is extracted from the
  :path meta-data key."
  [function]
  (let [path (^function :path)]
    (if path
      (let [key (split-path path)]
        ; TODO: check for conflict on key
        (assoc-in-config [:mapped-functions key] function)))))
                  
(defn find-mappings
  "Given the request path (as a split path, or vector of terms), finds all mapped functions that can apply to
  the request path. The return value is a seq of pairs; each pair is a split path to match against the request path,
  and a corresponding function. The seq is sorted in descending order by path count."
  [split-path]
  (let [mapped-functions (read-config :mapped-functions)
       sorted (sort (fn [[path1 _] [path2 _]] (- (count path2) (count path1))) mapped-functions)]
    (filter (fn [[path function]] (= (take (count path) split-path) path)) sorted)))
        
     