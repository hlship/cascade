; Copyright 2009, 2010, 2011 Howard M. Lewis Ship
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

(ns ^{:doc "Utilities for converting an exception into a structure that can be rendered"}
  cascade.exception
  (:import
    (java.lang Throwable StackTraceElement))
  (:require
    (clojure.contrib [str-utils2 :as s2]))
  (:use
    clojure.stacktrace
    cascade
    (cascade.internal utils)))

;; Identifies the properties of Throwable that are excluded from each exception-map's set of properties

(def throwable-properties (keys (bean (Throwable.))))

(defn class-name-for-element
  "Returns the CSS class name for a stack frame element, or nil.  Useful values are :c-omitted-frame
   or :c-usercode-frame."
  [^StackTraceElement element]
  (lcond
    :let [class-name (.getClassName element)]
    (or
      (.startsWith class-name "clojure.lang.")
      (.startsWith class-name "sun.")
      (.startsWith class-name "java.lang.reflect.")) :c-omitted-frame))

(defn convert-clojure-frame
  "Converts a stack frame into DOM nodes representing the Clojure namespace and function name(s),
  or returns nil if the stack frame is not for a Clojure call frame."
  [class-name method-name]
  (when (contains? #{"invoke" "doInvoke"} method-name)
    (let [[namespace-name & raw-function-ids] (s2/split class-name #"\$")
          function-ids (map #(nth (first (re-seq #"(\w+)__\d+" %)) 1 nil) raw-function-ids)
          function-names (map #(s2/replace % \_ \-) function-ids)]
      (if-not (empty? raw-function-ids)
        (template
          namespace-name "/" (s2/join "/" function-names)
          :span { :class :c-omitted } [ " " class-name "." method-name])))))

(defn transform-stack-frame
  [^StackTraceElement element]
  {
    :element element
    :method-name
      (let [file-name (.getFileName element)
            line-number (.getLineNumber element)
            class-name (.getClassName element)
            method-name (.getMethodName element)]
        (template
          (or
            (convert-clojure-frame class-name method-name)
            (str class-name "." method-name))
          " "
          (cond
            (.isNativeMethod element) (template :em [ "(Native Method)" ])
            (and (not (nil? file-name)) (<= 0 line-number)) (str "(" file-name ":" line-number ")")
            (not (nil? file-name)) (str "(" file-name ")")
            :else (template :em [ "(Unknown Source)" ]))))
      :class-name (class-name-for-element element)
  })

(defn transform-stack-trace
  "Transforms a primitive array of StackTraceElements into individual maps;
  :method-name is a seq of DOM nodes to describe the method and location
  :element is the original StackTraceElement
  :class-name is a keyword (may be nil) used when rendering the frame as an :li element"
  [elements]
  (map transform-stack-frame (seq elements)))

(defn expand-exception-stack
  "Expands a simple exception into a seq of exception maps, representing the stack of exceptions (the first or outer
  exceptions wrap the later, inner, deeper exceptions). Each map has keys :class-name, :message, :stack-trace and :properties.
  :class-name is the name of the exception class, :message is the message associated with the exception,
  :stack-trace is via transform-stack-trace (and will only be present in the last, or deepest, exception map),
  and :properties is a map of additional JavaBean properties of the map that should be presented to the user. The values
  of the :properties map are Java objects, not necesarilly strings, and will need further transformation to be
  presented."
  [^Throwable thrown-exception]
  (loop [^Throwable current thrown-exception
         stack []]
    (let [bean-properties (bean current)
          next-exception (.getCause current)
          is-deepest (nil? next-exception)
          exception-map { :class-name (.. current getClass getName)
                          :message (.getMessage current)
                          :properties (apply dissoc bean-properties throwable-properties) }]
      (if is-deepest
        (conj stack (assoc exception-map :stack-trace (transform-stack-trace (.getStackTrace current))))
        (recur next-exception (conj stack exception-map))))))