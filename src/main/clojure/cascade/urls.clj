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

(ns #^{:doc "Functions used when encoding data into portions of a URL"} 
  cascade.urls
  (:import 
    (clojure.lang Keyword Symbol)
    (javax.servlet.http HttpServletRequest))
  (:require
    (clojure.contrib [str-utils2 :as s2]))    
  (:use 
    (cascade config fail logging)
    (cascade.internal utils)
    (clojure.contrib seq-utils)))
  
(defmulti to-url-string
  "Used to encode a value for inclusion as a query parameter value, or as extra path data.
  Dispatches on the class of the single parameter."
  class)
  
;; TODO: Encode quotes and such inside the string.  
(defmethod to-url-string String [s] s)
(defmethod to-url-string Number [#^Number n] (.toString n))

;;  Assumes that keyword and symbols names are already URL safe
(defmethod to-url-string Keyword [kw] (name kw))
(defmethod to-url-string Symbol [sym] (name sym))

(defn parse-int [s] (Integer/parseInt s))

(doseq [[k f] [[:int #'parse-int] 
               [:str #'str]]]
  (assoc-in-config [:url-parser k] f))

(defn get-parser-fn 
  "Converts a parser into a parser function, looking for it in configuration (under :url-parser) if a keyword,
  or passing it through (a symbol of inline function) otherwise."
  [parser]
  (if (keyword? parser)
    (read-config :url-parser parser)
    parser))
          
(defn parse-url-value
  "Used internally to parse a URL value. Nil values stay nil, but others are subject to exceptions
  if improperly formed. The parser-fn is passed the value if not nil."
  [value parser-fn]
  (if-not (nil? value) (parser-fn value)))

(defn parse-extra-path-value
  "Used internally to parse a positional value from the extra path in the URL. The value extacted from the extra-path (a seq of strings) is
  passed through the parser function. Indexes outside the bounds of the extra-path seq are treated as nil."
  [extra-path index parser-fn]
  (parse-url-value (nth extra-path index nil) parser-fn))
        
(defn parse-query-parameter-value
  "Parses a value specified as a query parameter. The parameter-name is the string used to identify the parameter, and the extracted parameter value
  is passed through the parser function."
  [#^HttpServletRequest request parameter-name parser-fn]
  (parse-url-value (.getParameter request parameter-name) parser-fn))
        
(defn create-positional-binding
  [extra-path-symbol bound-symbol parser index]
  `[~bound-symbol (parse-extra-path-value ~extra-path-symbol ~index ~(get-parser-fn parser))])

(defn construct-positional-bindings
  [env-symbol pairs]
  (if-not (empty? pairs)
    (let [extra-path (gensym "extra-path")
          setup `[~extra-path (-> ~env-symbol :cascade :extra-path)]
          bindings (mapcat (fn [[bound-symbol parser] index] (create-positional-binding extra-path bound-symbol parser index))
                           pairs
                           (iterate inc 0))]
      (concat setup bindings))))

(defn create-query-parameter-binding
  [request-symbol bound-symbol parser param-keyword]
  `[~bound-symbol (parse-query-parameter-value ~request-symbol ~(name param-keyword) ~(get-parser-fn parser))])

(defn construct-query-parameter-bindings
  [env-symbol pairs]
  (if-not (empty? pairs)
    (let [request-symbol (gensym "request")
          setup `[~request-symbol (-> ~env-symbol :servlet-api :request)]
          ;; TODO: More verification of the pairs; second value must be a vector, must have two values, first value must be a keyword.
          bindings (mapcat (fn [[bound-symbol [param-keyword parser]]] (create-query-parameter-binding request-symbol bound-symbol parser param-keyword)) pairs)]
      (concat setup bindings))))

(defmacro parse-url
  "Handles the extraction and conversion of data encoded into the URL as extra path information or as query parameters."
  [env-symbol symbol-mappings & forms]
  (if (empty? symbol-mappings)
    `(do ~@forms)
    (do
      (fail-unless (vector? symbol-mappings) "parse-url expects a vector of symbol mappings.")
      (fail-unless (even? (count symbol-mappings)) "parse-url requires an even number of symbol mappings.")

      (let [symbol-pairs (partition 2 symbol-mappings)
            [query-parameter-pairs positional-pairs] (separate #(vector? (nth % 1)) symbol-pairs)
            positional-bindings (construct-positional-bindings env-symbol positional-pairs)
            query-parameter-bindings (construct-query-parameter-bindings env-symbol query-parameter-pairs)]
        `(let [~@positional-bindings ~@query-parameter-bindings] ~@forms)))))

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