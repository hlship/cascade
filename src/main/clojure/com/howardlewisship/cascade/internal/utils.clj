; Copyright 2009 Howard M. Lewis Ship
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

(ns com.howardlewisship.cascade.internal.utils
  (:use
   clojure.contrib.str-utils))

(declare find-namespace-resource)

(defn find-classpath-resource
  "Finds a resource on the classpath (as a URL) or returns nil if not found. Optionally takes
  a symbol and evaluates the path relative to the symbol's namespace."
  ([path]
   (.. (Thread/currentThread) getContextClassLoader (getResource path)))
  ([symbol path]
   (let [ns (:ns (meta (resolve symbol)))]
     (find-namespace-resource ns path))))

(defn find-namespace-resource
  "Given a namespace (or a symbol identifying a namespace),
  locates a resource relative to the namespace, or nil if not found."
  [namespace path]
  (let [ns-str (name (ns-name namespace))
        ns-path (.. ns-str (replace \. \/) (replace \- \_))]
    (find-classpath-resource (str ns-path "/" path))))


(defn to-str-list
  [coll]
  "Creates a comma-seperated list from the collection, or returns \"(none\")
  if the collection is null or empty."
  (if (empty? coll)
    "(none)"
    (str-join ", " coll)))

(defn first-non-nil
  "Returns the first non-nil value from the collection."
  [coll]
  (first (remove nil? coll)))

; hiredman on IRC doesn't think this is correct, but using (intern)
; doesn't fit my needs. Need to evaluated the expression in the context
; of the namespace, not just bind the value into the namespace.

(defmacro eval-in-namespace
  "Switches to a namespace (identified by the ns symbol) to evaluate the given forms."
  [ns & forms]
  `(let [initial-ns# (ns-name *ns*)]
    (try
      (require ~ns)
      (in-ns ~ns)
      ~@(for [f forms] `(eval '~f))
      (finally (in-ns initial-ns#)))))
