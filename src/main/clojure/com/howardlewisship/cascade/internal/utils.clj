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
  (:import (java.util.regex Matcher MatchResult))
  (:use
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

(defn first-non-nil
  "Returns the first non-nil value from the collection."
  [coll]
  (first (remove nil? coll)))

(defn remove-matches
  "Lazily removes from the seq (of maps) where the key matches the value."
  [key value seq]
  (remove #(= (get % key) value) seq))

(defn filter-matches
  "Lazily filters from the seq (of maps) where the key matches the value."
  [key value seq]
  (filter #(= (get % key) value) seq))

(defn read-single-form
  "Reads a single form from a string. Throws a RuntimeException if the string contains more than a single form."
  [expression-string]
  (with-in-str expression-string
    (let [eof-marker (Object.)
          read-once #(read *in* false eof-marker)
          result (read-once)
          extra (read-once)]
      ; The first read should exhaust the input, and the extra should be the marker.
      (if (= extra eof-marker)
        result
        (throw (RuntimeException. (format "Input expression '%s' should contain only a single form." expression-string)))))))

(defn re-partition-matches
  "Partitions an input string using a regular expression. The result is a list of two element vectors (pairs).
  Each pair consists of a text value (that is, non matching text) and a java.util.regex.MatchResult. In some cases,
  one value inside a pair may be nil."
  [re #^String s]
  (let [#^Matcher matcher (re-matcher re s)]
    (loop [#^Integer last-pos 0
           result []]
      (if (.find matcher)

        (let [match-result (.toMatchResult matcher)
              text (.substring s last-pos (.start match-result))]
          (recur (.end match-result) (conj result [text match-result])))

        ; Else, no match, add a final pair for any remaining text after
        ; the last match.

        (conj result [(.substring s last-pos) nil])))))

(defn re-map
  "Transforms a string by using a regular expression. The string is matched against the expression. Matching sections have the
MatchResult passed to the match-fn, non-matching sections have the non-matching text passed to the text-fn. The return value
is the sequence of results from the two functions. Nil values
 returned from the functions are removed. If the text function is omitted, identity is used."
  ([re s match-fn]
    (re-map re s identity match-fn))
  ([re s text-fn match-fn]
    (let [partitioned (re-partition-matches re s)
          invoker (fn [[text match-result]] [(and text (text-fn text)) (and match-result (match-fn match-result))])
          processed (map invoker partitioned)]
      (remove nil? (apply concat processed)))))

(defn blank?
  "Is the provided string nil or the empty string?"
  [#^String s]
  (or (nil? s)
      (= 0 (.length s))))