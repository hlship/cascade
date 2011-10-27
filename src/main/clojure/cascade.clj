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
    [compojure core]
    [cascade dom]
    [cascade.internal viewbuilder parse-functions]))

(defmacro template
  "Defines a block of the template DSL, which is converted into code that renders a seq of DOM nodes."
  [& forms]
  (parse-embedded-template forms))

(defmacro defview
  "Defines a Cascade view function, which uses an embedded template. A view function may have a doc string and meta data
  preceding the parameters vector. The function's forms are an implicit inline block."
  [& forms]
  (let [[fn-name fn-params template-forms] (parse-function-def forms)]
    `(defn ~fn-name ~(or (meta fn-name) {}) ~fn-params
      (template ~@template-forms))))

(defmacro block
  "Encapsulates a block of template forms as a function with parameters, typically used as
  a callback. The function, when invoked, returns a seq of DOM nodes."
  [fn-params & template-forms]
  `(fn ~fn-params (template ~@template-forms)))

(defmacro template-for
  "Executes a for list comprehension on the bindings, with the template forms evaluated as an implicit template."
  [bindings & template-forms]
  `(combine (for ~bindings (template ~@template-forms))))

(def ^{:doc "A DOM text node for a line break."}
  linebreak
  (text-node "\r"))

(defn raw
  "Wraps a string as a Static DOM node, but does not do any filtering of the value."
  [s]
  (raw-node s))

(defn <!--
  "Creates a comment DOM node."
  [comment]
  (comment-node comment))

(def application-version
  "The application version, which is incorporated into the URLs for assets." nil)

; Should Asset extend Renderable?
(defprotocol Asset
  "Represent a server-side resource so that it can be exposed efficiently to the client."
  (exists [asset] "Returns true if the asset exists and is readable.")
  (content-type [asset] "Returns the MIME content type for the Asset.")
  (^InputStream content [asset] "Returns the content of the Asset as a stream of bytes.")
  (^String client-url [asset] "Returns an absolute URL to the Asset."))

(defn file-asset-handler [req] nil)

(defn wrap-exception-handling
  "Middleware for standard Cascade exception reporting; exceptions are caught and reported using the Cascade
  exception report view."
  [handler]
  ; Just a placeholder for now
  handler)

(defn initialize-assets
  "Initializes asset handling for Cascade. This sets an application version (a value incorporated into URLs, which
should change with each new deployment. Named arguments:
:virtual-folder (default \"assets\")
  The root folder under which assets will be exposed to the client.
:public-folder (default \"public\")
  The file system folder under which file assets are stored."
  [application-version & {:keys [virtual-folder public-folder]
                          :or {virtual-folder "asset"
                               public-folder "public"}}]
  (set! cascade/application-version application-version)
  (let [root (str "/" virtual-folder "/" application-version)]
    (wrap-exception-handling
      (routes
        (GET (str root "/file/*") file-asset-handler)))))

(defn wrap-html
  "Ring middleware that wraps a handler so that the return value from the handler (a seq of DOM nodes)
is serialized to HTML (as lazy seq of strings)."
  [handler]
  (fn [req]
    (->
      (handler req)
      serialize-html)))