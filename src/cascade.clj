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

(ns
  cascade
  "Core functions and macros used when implementing Cascade views"
  (:use
    [cascade dom]
    [cascade.internal viewbuilder parse-functions]
    ring.util.response))

(defmacro markup
  "Defines a block of the template DSL, which is converted into code that renders a seq of DOM nodes."
  [& forms]
  (parse-markup forms))

(defmacro defview
  "Defines a Cascade view function, which uses an embedded template. A view function may have a doc string and meta data
preceding the parameters vector. The function's forms are an implicit inline block. The function returns a Ring response
map, consisting of a single key, :body, consisting of the DOM nodes rendering by the implicit template."
  [& forms]
  (let [[fn-name fn-params template-forms] (parse-function-def forms)]
    `(defn ~fn-name ~(or (meta fn-name) {}) ~fn-params
      (~response (markup ~@template-forms)))))

(defmacro defragment
  "Defines a Cascade view fragment function, which uses an embedded template. A fragment function may have a doc string and meta data
preceding the parameters vector. The function's forms are an implicit inline block. The function returns a seq of DOM nodes."
  [& forms]
  (let [[fn-name fn-params template-forms] (parse-function-def forms)]
    `(defn ~fn-name ~(or (meta fn-name) {}) ~fn-params
      (markup ~@template-forms))))

(defmacro block
  "Encapsulates a block of template forms as a function with parameters, typically used as
  a callback. The function, when invoked, returns a seq of DOM nodes."
  [fn-params & template-forms]
  `(fn ~fn-params (markup ~@template-forms)))

(defmacro markup-for
  "Executes a for list comprehension on the bindings, with the  forms evaluated as an implicit markup block."
  [bindings & markup-forms]
  `(combine (for ~bindings (markup ~@markup-forms))))

(def linebreak
  "A DOM text node for a line break."
  (text-node "\r"))

(defn raw
  "Wraps a string as a Static DOM node, but does not do any filtering of the value."
  [s]
  (raw-node s))

(defn <!--
  "Creates a comment DOM node."
  [comment]
  (comment-node comment))
