; Copyright 2009, 2010 Howard M. Lewis Ship
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
; implied. See the License for the specific language governing permissions
; and limitations under the License.

(ns
  #^{:doc "Internal private utilities"}
  cascade.internal.utils
  (:import 
    [java.util.regex Matcher MatchResult]
    [clojure.lang IFn])
  (:use
    [clojure pprint]
    [clojure.contrib str-utils]))

(declare find-namespace-resource)

(defn ppstring
  "Pretty-print a collection to a string."
  [coll]
  (with-out-str (pprint coll)))

(defn find-classpath-resource
  "Finds a resource on the classpath (as a URL) or returns nil if not found."
  [path]
  (.. (Thread/currentThread) getContextClassLoader (getResource path)))

(defn find-namespace-resource
  "Given a namespace (or a symbol identifying a namespace),
  locates a resource relative to the namespace, or nil if not found."
  [namespace path]
  (let [ns-str (name (ns-name namespace))
        ns-path (.. ns-str (replace \. \/) (replace \- \_))]
    (find-classpath-resource (str ns-path "/" path))))

(defn to-seq
  "Converts the object to a sequence (a vector) unless it is already sequential."
  [obj]
  (if (sequential? obj) obj [obj]))

(defn function?
  "Returns true if an object is (or acts as) a Clojure function?"
  [obj]
  (instance? IFn obj))

(defn apply-until-non-nil
  "Works through a sequence of functions, apply the argseq to each of them until a function
  returns a non-nil value"
  [functions argseq]
  (first (remove nil? (map #(apply % argseq) functions))))

(defn blank?
  [#^String s]
  (or
    (nil? s)
    (= 0 (.length s))))
