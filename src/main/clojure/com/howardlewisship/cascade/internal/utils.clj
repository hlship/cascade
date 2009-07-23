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
  (:import (java.util.regex Matcher MatchResult)
           (clojure.lang IFn))
  (:use
    com.howardlewisship.cascade.config
    clojure.contrib.str-utils))

(defn fail
  "A quick way to throw a RuntimeException."
  [#^String msg]
  (throw (RuntimeException. msg)))

(defn fail-unless
  "Throws a runtime exception if the condition is false."
  [condition message]
  (or condition (fail message)))

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
  "Creates a comma-seperated list from the collection, or returns \"(none\")
if the collection is null or empty."
  [coll]
  (if (empty? coll)
    "(none)"
    (str-join ", " coll)))

(defn- to-seq
  "Converts the object to a sequence (a vector) unless it is already sequential."
  [obj]
  (if (sequential? obj) obj [obj]))

(defn function?
  "Returns true if an object is (or acts as) a Clojure function?"
  [obj]
  (instance? IFn obj))

(defn create-chain
  "Function factory for building a chain control structure. The parameters passed to the
  returned function are
  passed to every step function in the chain.  Step functions are defined via the keys of the
  :chain key of the global configuration. Each step can be a function (that should match the arity
  of the overally chain), or can be a keyword or symbol used to identify another
  step int the chain, or can be a vector of steps. In this way, chains can be easily
  composed."
  [selector]
  (fn [& params]
    (loop [queue (to-seq (-> configuration :chains selector))]
        (let [current (first queue)
              remaining (rest queue)]
          (cond
            (empty? queue) nil
            (nil? current) (recur remaining)
            (sequential? current) (recur (concat current remaining))
            (or (symbol? current) (keyword? current)) (recur (cons (-> configuration :chains current) remaining))
            (function? current) (let [result (apply current params)]
                                  (if (nil? result)
                                      (recur remaining)
                                      result)))))))
;; TODO: better error reporting when an element from the queue is a symbol or other non-function object.
;; Also, symbol does implement IFn just to keep things confused.


