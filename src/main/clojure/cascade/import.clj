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
    [cascade dom]))

(def wrap-install-cascade-atom-into-request [handler]
  "Wraps the handler with a new handler that installs the :cascade key into the request.
The value is an atom containing a map of the data needed to track imports."
  (fn [req]
    (handler (assoc req :cascade (atom {:stylesheets [] :js-inits []})))))

(defn add-if-not-present
  [list value]
  (if (contains? list value)
    list
    ; Dependent on the list being a vector that conj-es at the end
    conj list value))

(defn import-in-cascade-key
  ([req key value]
    (swap! (req :cascade) update-in [key] add-if-not-present value)))

(defn import-stylesheet
  "Imports a stylesheet for a CSS asset.  Returns the request object."
  ; TODO: Naming and explicit ordering!
  [req stylesheet-asset]
  (import-in-cascade-key req :stylesheets stylesheet-asset))

(defn add-stylesheet-nodes
  [dom-nodes stylesheet-assets]

(def wrap-import-stylesheets
  "Middleware around a handler that consumes a request and returns a seq of DOM nodes (or nil). The DOM nodes are
post-processed to add new <link> tags for any imported stylesheets."
  [handler]
  (fn [req]
    (let [dom-nodes (handler req)]
      (if dom-nodes
        (add-stylesheet-nodes dom-nodes (-> req :cascade deref :stylesheets))))))
