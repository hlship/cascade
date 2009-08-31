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

(ns cascade.internal.utils
  (:import (java.util.regex Matcher MatchResult)
           (clojure.lang IFn))
  (:require
    (clojure.contrib [str-utils2 :as s2]))  
  (:use
    (cascade config logging urls)
    (clojure.contrib str-utils pprint)))

(defn fail
  "A quick way to throw a RuntimeException."
  [#^String msg]
  (throw (RuntimeException. msg)))

(defmacro fail-unless
  "Throws a runtime exception if the condition is false. The message is generated from the format
  and additional arguments and is only evaluated if a failure occurs."
  [condition fmt & args ]
  `(if-not ~condition (fail (format ~fmt ~@args))))

(defmacro fail-if
  "Throws a runtime exception if the condition is true.  The message is generated from the format
  and additional arguments and is only evaluated if a failure occurs."
  [condition fmt & args]
  `(if ~condition (fail (format ~fmt ~@args))))

(declare find-namespace-resource)

(defn ppstring 
  "Pretty-print a collection to a string."
  [coll]
  (with-out-str (pprint coll)))

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

(defn to-seq
  "Converts the object to a sequence (a vector) unless it is already sequential."
  [obj]
  (if (sequential? obj) obj [obj]))

(defn function?
  "Returns true if an object is (or acts as) a Clojure function?"
  [obj]
  (instance? IFn obj))

(defn expand-function-list
  "Expands a configured function list into a sequence of actual functions. The configuration-key will be
  :chains or :pipelines, to identify where inside @configuration we search for functions. The values
  can be a keyword or symbol or an array of keywords or symbols."
  [configuration-key selector]
  (loop [result []
         queue (to-seq selector)]
    (let [current (first queue)
          remaining (rest queue)]
      (cond
        (empty? queue) result
        (nil? current) (recur result remaining)
        (sequential? current) (recur result (concat current remaining))
        (or (symbol? current) (keyword? current))
          (recur result (cons (read-config configuration-key current) remaining))
        (function? current) (recur (conj result current) remaining)))))
    
(defn apply-until-non-nil
  "Works through a sequence of functions, apply the argseq to each of them until a function
  returns a non-nil value"
  [functions argseq]
  (first (remove nil? (map #(apply % argseq) functions))))

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
    ; TODO: We do this pretty late in case someone's been changing @configuration
    ; but it might be nice to cache this rather than compute it each time.
    (apply-until-non-nil (expand-function-list :chains selector) params)))
    
(defn create-pipeline
  "A pipeline is like a chain, but the individual functions (called filters) that
  are each passed an extra parameter (the first
  parameter) which is the next function in the pipeline. The final function in the pipeline is passed
  a bridge to an ordinary function, called the terminator. In this way, each function can control
  parameters, return values and exception behavior for functions further down the pipeline (in AOP terms, \"around advice\"). Returns a function
  with the same arity as the terminator. Nil return values from filter functions, or the terminator, have
  no special meaning."
  [selector terminator]
  (assoc-in-config [:pipelines selector]
    (fn [& params]
      (loop [bridge terminator
             queue (reverse (expand-function-list :filters selector))]
         (if (empty? queue)
           ; So, the outer function simply passes the params to the outermost bridge,
           ; whose arity should match the arity of the terminator. 
           ; The bridge may in fact be the terminator.
           (apply bridge params)

           ; Build a new bridge that invokes the current function passing the
           ; current bridge to it as the first parameter.
         
           (recur (fn [& args]
             (apply (first queue) bridge args)) (rest queue)))))))

(defn call-pipeline
  "Calls into a pipeline, identified by its selector (a keyword). The pipeline is expected to be found
  inside the :pipelines configuration map."
  [selector & args]
  (apply (read-config :pipelines selector) args))

(defn qualified-function-name-from-meta
  [fn-meta]
  (str (ns-name (fn-meta :ns)) "/"  (fn-meta :name)))
  

(defn qualified-function-name
   "Accesses the meta-data for a function to extract its name and namespace, concatinated and
   returned as a string."
  [f]
  (qualified-function-name-from-meta ^f))
  
(defn blank?
  [#^String s]
  (or 
    (nil? s)
    (= 0 (.length s))))
    
(defn split-path
  "Splits path (a string) on slash characters, returning a vector of the results. Leading slashes and doubled slashes are
  ignored (that is, empty names in the result are removed)."
  [#^String path]
  (let [names (.split #"/" path)]
    (vec (remove blank? (seq names)))))

(defn add-query-parameter
  [#^StringBuilder builder key value]
  (doto builder
    (.append (to-url-string key))
    (.append "=")
    (.append (to-url-string value))))

(defn construct-absolute-path
  "Converts a link map into a absolute path, including query parameters."  
  [context-path link-map]
  (loop [#^StringBuilder sb (doto (StringBuilder.)
                              (.append context-path)
                              (.append "/")
                              (.append (link-map :path)))
         sep "?"
         param-pairs (-> link-map :parameters)]
    (if (empty? param-pairs)
      (.toString sb)
      (do
        (.append sb sep)
        (add-query-parameter sb ((first param-pairs) 0) ((first param-pairs) 1))
        (recur sb "&" (next param-pairs))))))          

(defn link-map-from-path
  "Constructs a link map a path, extra path info, and optional query parameters. Applications should
  use the link-map macro instead."
  [fn-path extra-path-info query-parameters]
  {
    :path (if (empty? extra-path-info)
            fn-path
            (str fn-path "/" (s2/join "/" (map str extra-path-info))))
    :parameters query-parameters
  })         