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
    [clojure.string :as s2]))

(defn wrap-install-cascade-atom-into-request [handler]
  "Wraps the handler with a new handler that installs the :cascade key into the request.
The value is an atom containing a map of the data needed to track imports."
  (fn [req]
    (handler (assoc req :cascade (atom {:stylesheets [] :modules []})))))

(defn add-if-not-present
  [list value]
  (if (contains? list value)
    list
    ; Dependent on the list being a vector that conj-es at the end
    (conj list value)))

(defn import-in-cascade-key
  ([req key value]
    (swap! (req :cascade) update-in [key] add-if-not-present value)))

(defn import-stylesheet
  "Imports a stylesheet for a CSS asset.  Returns nil."
  ; TODO: Naming and explicit ordering!
  [req stylesheet-asset]
  (import-in-cascade-key req :stylesheets stylesheet-asset)
  nil)

(defn import-module
  "Imports a module by module name. Returns nil"
  [req module-name]
  (import-in-cascade-key req :modules module-name)
  nil)

(defn to-element-node
  "Converts an asset into a <link> element node."
  [asset]
  (element-node :link {:rel :stylesheet :type "text/css" :href asset} nil))

(defn add-stylesheet-nodes
  [req dom-nodes stylesheet-assets]
  ; TODO: optimize when no assets
  (extend-dom dom-nodes [[[:html :head :script] :before]
                         [[:html :head :link] :before]
                         [[:html :head] :bottom]] (map to-element-node stylesheet-assets)))

(defn apply-response-transformation
  [handler key transform-fn]
  (fn [req]
    (let [response (handler req)]
      (and response
        (update-in response [:body] #(transform-fn req % (-> req :cascade deref key)))))))

(defn wrap-import-stylesheets
  "Middleware that expects the rendered body to be a seq of DOM nodes. The DOM nodes are
post-processed to add new <link> elements for any imported stylesheets."
  [handler]
  (apply-response-transformation handler :stylesheets add-stylesheet-nodes))

(defn add-module-requires-javascript
  [req dom-nodes module-names]
  (if (empty? module-names)
    dom-nodes
    (->
      dom-nodes
      (extend-dom [[[:html :head] :bottom]]
        (template
          :script {:src (classpath-asset "cascade/require-jquery.js")}
          :script [
          "require.config({ baseUrl: '"
          (build-client-url :module "")
          "' });\n"
          "require(["
          (->>
            (map #(str \' % \') module-names)
            (s2/join ", "))
          "]);\n"
          ])))))

(defn wrap-import-modules
  "Middleware that handles imported modules."
  [handler]
  (apply-response-transformation handler :modules add-module-requires-javascript))

(defn wrap-imports
  "Wraps a request-to-DOM-nodes handler with support for imports."
  [handler]
  (->
    handler
    wrap-import-modules
    wrap-import-stylesheets
    wrap-install-cascade-atom-into-request))
