; Copyright 2011 Howard M. Lewis Ship
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

(ns cascade.import
  "Support for importing assets and JavaScript initialization code into the document, during the DOM rendering phase."
  (:use
    cascade
    [cascade dom asset])
  (:require
    [clojure.string :as s2]
    [clj-json.core :as json]))

(def ^{:dynamic true}
  *active-imports*
  "Stores a per-thread map used to track what's been imported."
  nil)

(defn wrap-setup-active-imports [handler]
  "Binds the *active-imports* var so that it may be updated by the downstream handlers."
  (fn [req]
    (binding [*active-imports* {:stylesheets [] :javascript []}]
      (handler req))))

(defn add-if-not-present
  [list value]
  (if (contains? list value)
    list
    ; Dependent on the list being a vector that conj-es at the end
    (conj list value)))

(defn import-into-keyed-list
  "Updates that map in *active-imports*, adding the value to the list storted under the
given key, if not already present.  Returns nil."
  [key value]
  (set! *active-imports* (update-in *active-imports* [key] add-if-not-present value))
  nil)

(defn import-stylesheet
  "Imports a stylesheet for a CSS asset. The stylesheet may be specified as an asset. Alternately, a stylesheet factory
function and a path passed to that factory may be provided (which reads nicely). Returns nil."
  ; TODO: Naming and explicit ordering!
  ([stylesheet-asset]
    (import-into-keyed-list :stylesheets stylesheet-asset))
  ([factory-fn asset-path]
    (import-stylesheet (factory-fn asset-path))))

(defn import-module
  "Imports a module by module name. Returns nil"
  [module-name]
  (import-into-keyed-list :javascript [module-name]))

(defn javascript
  "Imports initialization JavaScript that will be executed when the page loads.
module-name
  Name of module exposing the initialization function. The module should return a JavaScript object that maps names to functions.
initializer-fn-name
  Name of a function exposed by the module that will be invoked.
arguments
  Arguments passed to the client-side function. The arguments will be converted to JSON before being streamed to the client. Typically,
  a single value (a string, or a JSON Object) is passed."
  [module-name initializer-fn-name & arguments]
  ; TODO: this may change a bit when we implement Ajax partial rendering.
  (import-into-keyed-list :javascript [module-name initializer-fn-name arguments]))

(defn to-element-node
  "Converts an asset into a <link> element node."
  [asset]
  (element-node :link {:rel :stylesheet :type "text/css" :href asset} nil))

(defn add-stylesheet-nodes
  [dom-nodes stylesheet-assets]
  ; TODO: optimize when no assets
  (extend-dom dom-nodes [[[:html :head :script] :before]
                         [[:html :head :link] :before]
                         [[:html :head] :bottom]] (map to-element-node stylesheet-assets)))

(defn apply-response-transformation
  [handler key transform-fn]
  (fn [req]
    (let [response (handler req)]
      (and response
        (update-in response [:body] transform-fn (*active-imports* key))))))

(defn wrap-import-stylesheets
  "Middleware that expects the rendered body to be a seq of DOM nodes. The DOM nodes are
post-processed to add new <link> elements for any imported stylesheets."
  [handler]
  (apply-response-transformation handler :stylesheets add-stylesheet-nodes))

(defn add-javascript
  "Updates the DOM with new <script> nodes to support imported JavaScript modules and JavaScript initializations."
  [dom-nodes javascript]
  (if (empty? javascript)
    dom-nodes
    (let [just-module-names (map first (filter #(= (count %) 1) javascript))
          initializations (filter #(> (count %) 1) javascript)]
      (->
        dom-nodes
        (extend-dom [[[:html :head] :bottom]]
          (markup
            :script {:src (classpath-asset "cascade/require-jquery.js")}
            :script [
            "require.config({ baseUrl: '"
            (build-client-url :classpath "")
            "' });\n"
            (if-not (empty? just-module-names)
              (markup
                "require(["
                (->>
                  (map #(str \' % \') just-module-names)
                  (s2/join ", "))
                "]);\n"))
            (if-not (empty? initializations)
              (markup
                "require(['cascade/init'], function(module) {\n"
                "  module.pageInit("
                (json/generate-string initializations) ");\n"
                "});\n"))
            ]))))))

(defn wrap-import-javascript
  "Middleware that handles imported JavaScript modules and JavaScript initialization."
  [handler]
  (apply-response-transformation handler :javascript add-javascript))

(defn wrap-imports
  "Wraps a request-to-DOM-nodes handler with support for imports."
  [handler]
  (->
    handler
    wrap-import-javascript
    wrap-import-stylesheets
    wrap-setup-active-imports))
