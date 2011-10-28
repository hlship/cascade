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
    [java.io File FileInputStream BufferedInputStream InputStream])
  (:require
    [ring.util.response :as ring])
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

(def default-file-extension-to-mime-type
  "Default set of mappings from file extensions (as keywords) to corresponding MIME type."
  {
    :js "text/javascript"
    :css "text/css"
    })

; Should Asset extend Renderable?
(defprotocol Asset
  "Represent a server-side resource so that it can be exposed efficiently to the client."
  (^InputStream content [asset] "Returns the content of the Asset as a stream of bytes, or null if the Asset does not exist.")
  (^String client-url [asset] "Returns an absolute URL to the Asset."))

(extend-type cascade.Asset
  ToAttributeValueString
  (to-attribute-value-string [asset] (client-url asset)))

(defrecord FileAsset [path]
  Asset
  (content [asset]
    (let [file-path (str (:public-folder @asset-configuration) "/" path)
          file (File. file-path)]
      (if (.canRead file)
        (-> (FileInputStream. file) BufferedInputStream.))))
  (client-url [asset]
    (str (:file-path @asset-configuration) path)))

(defn file-asset
  "Creates an Asset representing a file in the application's public folder, as configured in (initialize-assets)."
  [path]
  (->FileAsset path))

(defn get-content-type [asset]
  (let [path (:path asset)
        dotx (.lastIndexOf path ".")
        extension (keyword (.substring path (inc dotx)))
        content-type (-> @asset-configuration :file-extension extension)]
    (or content-type "text/plain")))

(defn generic-asset-handler
  "Processes the Asset, returning a Ring response (including content type) if the Asset exists, or nil. In the future,
the will also encompass GZIP compression of the asset, and perhaps in-memory caching."
  [asset]
  (let [stream (content asset)]
    (if stream
      (-> (ring/response stream)
        (ring/content-type (get-content-type asset))))))

; TODO: perhaps we should create a single handler for the virtual folder, then delegate out to it by domain
; ("file", "classpath")
(defn create-file-asset-handler
  [path-prefix]
  (fn [req]
    (let [uri (:uri req)
          path (.substring uri (.length path-prefix))]
      (generic-asset-handler (->FileAsset path)))))

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
The file system folder under which file assets are stored. May be an absolute path, should not end with a slash.
:file-extensions
Additional file-extension to MIME type mappings, beyond the default set."
  [application-version & {:keys [virtual-folder public-folder file-extensions]
                          :or {virtual-folder "assets"
                               public-folder "public"}}]
  (let [root (str "/" virtual-folder "/" application-version)
        file-path (str root "/file/")]
    (reset! asset-configuration
      {:application-version application-version
       :public-folder public-folder
       :virtual-folder virtual-folder
       :file-path file-path
       :file-extensions (merge default-file-extension-to-mime-type file-extensions)})
    (printf "Initialized file system resource access to directory '%s' at %s\n" public-folder file-path)
    (wrap-exception-handling
      (routes
        (GET (str file-path "*") [] (create-file-asset-handler file-path))))))

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