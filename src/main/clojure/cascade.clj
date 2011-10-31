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
  (:import
    [java.io File FileInputStream BufferedInputStream InputStream]
    [java.util Calendar Date])
  (:require
    [ring.util [response :as ring] [mime-type :as mime-type]]
    [ring.middleware.file-info :as file-info])
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

; This may change into a value passed as a key in a filter, or perhaps a rebindable var.
(def asset-configuration
  "The atom containing the asset configuration, needed to create Asset and handle asset requests.
This should only be changed at startup, by (initialize-assets)."
  (atom nil))

; Should Asset extend Renderable?
(defprotocol Asset
  "Represent a server-side resource so that it can be exposed efficiently to the client."
  (^String file-name [asset] "Returns just the name of the Asset.")
  (^InputStream content [asset] "Returns the content of the Asset as a stream of bytes, or null if the Asset does not exist.")
  (^String client-url [asset] "Returns an absolute URL to the Asset."))

(deftype FileAsset [file url]
  Asset
  (file-name [asset] (.getName file))
  (content [asset]
    (if (.canRead file)
      (-> (FileInputStream. file) BufferedInputStream.)))
  (client-url [asset] url)

  ; Each "implementation" of Asset should also extend ToAttributeValueString
  ; There isn't a proper way to express this in Clojure, though I think there
  ; should be.

  ToAttributeValueString
  (to-attribute-value-string [asset] (client-url asset)))

(defn file-asset
  "Creates an Asset representing a file in the application's public folder, as configured in (initialize-assets)."
  [path]
  (let [file-path (str (:public-folder @asset-configuration) "/" path)
        file (File. file-path)
        client-url (str (:file-path @asset-configuration) path)]
    (->FileAsset file client-url)))

(defn get-content-type [asset]
  (let [name (file-name asset)
        dotx (.lastIndexOf name ".")
        extension (.substring name (inc dotx))
        content-type (-> @asset-configuration :file-extensions (get extension))]
    (or content-type "text/plain")))

(defn generic-asset-handler
  "Processes the Asset, returning a Ring response (including content type) if the Asset exists, or nil. In the future,
the will also encompass GZIP compression of the asset, and perhaps in-memory caching."
  [asset]
  (let [stream (content asset)]
    (if stream
      (->
        (ring/response stream)
        (ring/header "Expires" (@asset-configuration :expiration))
        (ring/content-type (get-content-type asset))))))

(defn wrap-exception-handling
  "Middleware for standard Cascade exception reporting; exceptions are caught and reported using the Cascade
exception report view."
  [handler]
  ; Just a placeholder for now
  handler)

(defn- now-plus-ten-years
  "Returns a string representation of the current Date plus ten years, used for setting expiration date of assets."
  []
  (let [format (file-info/make-http-format)]
    (->>
      (doto (Calendar/getInstance)
        (.add Calendar/YEAR 10))
      .getTime
      (.format format))))

(defn file-handler
  "Inteprets the path as the pat to a file asset in the configured public folder."
  [path]
  (generic-asset-handler (file-asset path)))

(defn asset-dispatch-handler
  "Internal dispatcher for asset requests. Returns the response from the handler, or nil if
no handler matches the name.
dispatch
    Maps keywords for the asset domain (eg., :file) to the handler
name
    Keyword used to locate handler
path
    String path passed to the handler.
  "
  [dispatch name path]
  (let [handler (dispatch name)]
    (and handler (handler path))))

(defn initialize-assets
  "Initializes asset handling for Cascade. This sets an application version (a value incorporated into URLs, which
should change with each new deployment. Named arguments:
:virtual-folder (default \"assets\")
The root folder under which assets will be exposed to the client.
:public-folder (default \"public\")
The file system folder under which file assets are stored. May be an absolute path, should not end with a slash.
:file-extensions
Additional file-extension to MIME type mappings, beyond the default set."
  [application-version & {:keys [virtual-folder public-folder file-extensions]
                          :or {virtual-folder "assets"
                               public-folder "public"}}]
  (let [root (str "/" virtual-folder "/" application-version)
        dispatch {:file file-handler}]
    (reset! asset-configuration
      {:application-version application-version
       :expiration (now-plus-ten-years)
       :public-folder public-folder
       :virtual-folder virtual-folder
       :file-path (str root "/file/")
       :file-extensions (merge mime-type/default-mime-types file-extensions)})
    (printf "Initialized asset access at virtual folder %s\n" root)
    (wrap-exception-handling
      (GET [(str root "/:domain/:path") :path #".*"]
        [domain path] (asset-dispatch-handler dispatch (keyword domain) path)))))

(defn wrap-html
  "Ring middleware that wraps a handler so that the return value from the handler (a seq of DOM nodes)
is serialized to HTML (as lazy seq of strings)."
  [handler]
  (fn [req]
    (->
      (handler req)
      serialize-html)))

(defview stylesheet
  "Creates a <link> element for a stylesheet. The resource should be either a String or an Asset."
  [resource]
  :link {:rel :stylesheet :type "text/css" :href resource})