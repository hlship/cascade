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

(ns #^{:doc "Maps path prefixs to view, action and dispatcher functions"}
  cascade.path-map
  (:require
    (clojure.contrib [str-utils2 :as s2]))
  (:use 
    (cascade change-tracker logging config urls func-utils fail)))
      
(def #^ClassLoader context-class-loader (.getContextClassLoader (Thread/currentThread)))
      
(def tracked-namespaces (atom #{}))      
      
(def cascade-type-to-virtual-folder { :view "view" :action "action"})      

(defn to-source-path
	"Converts a namespace to a path (on the classpath) to the source file for
	 the namespace."
	[n]
	(let [ns-str (name (ns-name n))
			  converted (s2/replace (s2/replace ns-str \- \_) \. \/)]
		  (str converted ".clj")))

(defn start-tracking
	[n]
	(let [source-path (to-source-path n)
			  source-url (.getResource context-class-loader source-path)]
		(track-resource source-url
			(fn []
				; Unmark it as tracked (to force a later start-tracking call).
				(swap! tracked-namespaces disj n)
				; Now, reload the namespace.
				(debug "Reloading namespace %s" (ns-name n))
				(require :reload (ns-name n))))))
				
(defn add-tracked-namespace
	"Adds a namespace to those that are tracked for reload on change (if not already tracked)."
	[n]
	(swap! tracked-namespaces
		(fn [tracked]
			(if-not (contains? tracked n)
				(do 
					(start-tracking n)
					(conj tracked n))
				tracked))))
      
(defn add-function-to-config
  [selector path function]
  (let [key (split-path path)]
    ; TODO: check for conflict on key        
    (assoc-in-config [selector key] function)))
    
(defn find-matching-functions
  "Given the request path (as a split path, i.e. a vector of terms), finds all mapped functions in the configuration
  (within the selector key) that can apply to
  the request path. The return value is a seq of pairs; each pair is a split path to match against the request path,
  and a corresponding function. The seq is sorted in descending order by path count."  
  [selector split-path]    
  (let [mapped-functions (read-config selector)
       sorted (sort (fn [[path1 _] [path2 _]] (- (count path2) (count path1))) mapped-functions)]
    (filter (fn [[path function]] (= (take (count path) split-path) path)) sorted)))
      
(defn add-mapped-function
  "Maps a path to a view or action function. The path string is relative to the context root and is extracted from the
  :path meta-data key. Does nothing if the extracted path is nil."
  [function]
  (let [path (^function :path)]
  	(add-tracked-namespace (^function :ns))
    (if path
      (add-function-to-config :mapped-functions path function))))
      
(defn path-for-function*
  "Given the meta data for a function, computes the path (as a string, relative to the context root)
  for that function."
  [fn-meta]
  (let [type (get fn-meta :cascade-type)
        folder (get cascade-type-to-virtual-folder type)
        fn-path (get fn-meta :path)
        qname (qualified-function-name-from-meta fn-meta)]
    (fail-if (or (nil? type) (nil? folder))
     "Function %s is neither a view function nor an action function." qname)
    (cond
      ;; TODO: The user-supplied path may need some doctoring. It should not start with or end
      ;; with a slash.
      (not (nil? fn-path)) fn-path

      ;; Go from type to folder
      
      true (str folder "/" qname))))
              
(defmacro path-for-function
  "Calculates the path to a given view or action function. The result is a path string,
  relative to the context root. If the function defines :path meta-data, that it used, otherwise
  an appropriate path is constructed within the virtual /view or /action folder."
  [function]
  `(path-for-function* (meta (var ~function))))
     
