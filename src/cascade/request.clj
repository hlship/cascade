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

(ns cascade.request
  "Functions used to initialize a Cascade application and process requests."
  (:import
    [java.util Calendar Date])
  (:require
    [ring.util [response :as ring] [mime-type :as mime-type]]
    [ring.middleware.file-info :as file-info])
  (:use
    [compojure core]
    [cascade dom asset import exception]))

(defn asset-handler
  "Internal handler for asset requests. Returns the response from the handler, or nil if
no handler matches the name.
asset-factories
  Maps keywords for the asset domain (eg., :file or :classpath) to the factory function (which takes a path)
domain
  Keyword used to locate factory function
path
  String path passed to the handler."
  [asset-factories domain path]
  (let [factory (domain asset-factories)
        asset (and factory (factory path))
        stream (and asset (content-stream asset))]
    (if stream
      (->
        (ring/response stream)
        (ring/header "Expires" (@asset-configuration :expiration))
        (ring/content-type (get-content-type asset))))))


(defn- now-plus-ten-years
  "Returns a string representation of the current Date plus ten years, used for setting expiration date of assets."
  []
  (let [format (file-info/make-http-format)]
    (->>
      (doto (Calendar/getInstance)
        (.add Calendar/YEAR 10))
      .getTime
      (.format format))))

(def placeholder-routes
  "Returns a placeholder request handling function that always returns nil."
  (routes))

(defn wrap-serialize-html
  "Wraps a handler that produces a seq of DOM nodes (e.g., one created via defview or template) so that
the returned dom-nodes are converted into a seq of strings (the markup to be streamed to the client)."
  [handler]
  (fn [req]
    (let [response (handler req)]
      (and response
        (update-in response [:body] serialize-html)))))

(defn wrap-html-markup
  "Wraps the handler (which renders a request to DOM nodes) with full rendering support, including imports."
  [handler]
  (->
    handler
    wrap-imports
    wrap-serialize-html))

(defn render-report-exception
  [req exception]
  ((->
    (fn [req]
      (exception-report req exception))
    wrap-imports
    wrap-serialize-html) req))


(defn wrap-exception-reporting
  "Middleware for standard Cascade exception reporting; exceptions are caught and reported using the Cascade
exception report view."
  [handler]
  (fn [req]
    (try
      (handler req)
      (catch Exception e (render-report-exception req e)))))

(defn initialize
  "Initializes asset handling for Cascade. This sets an application version (a value incorporated into URLs, which
  should change with each new deployment. Named arguments:
:html-routes
  Routes that produce full-page rendered HTML markup. The provided handlers should render the request to a seq
  of DOM nodes.
:virtual-folder (default \"assets\")
  The root folder under which assets will be exposed to the client.
:public-folder (default \"public\")
  The file system folder under which file assets are stored. May be an absolute path, should not end with a slash.
:file-extensions
  Additional file-extension to MIME type mappings, beyond the default set (defined by ring.util.mime-type/default-mime-types).
:asset-factories
  Additional asset dispatcher mappings. Keys are domain keywords, values are functions that accept a path within that domain.
  The functions should construct and return a cascade.asset/Asset. Default support for the :file and :classpath domains."
  [application-version & {:keys [virtual-folder public-folder file-extensions asset-factories html-routes]
                          :or {virtual-folder "assets"
                               public-folder "public"}}]
  (let [root (str "/" virtual-folder "/" application-version)
        asset-factories (merge {:file file-asset
                                :classpath classpath-asset} asset-factories)]
    (reset! asset-configuration
      {:application-version application-version
       :expiration (now-plus-ten-years)
       :public-folder public-folder
       :assets-folder root
       :file-extensions (merge mime-type/default-mime-types file-extensions)})
    (printf "Initialized asset access at virtual folder %s\n" root)
    (->
      (routes
        (GET [(str root "/:domain/:path") :path #".*"]
          [domain path] (asset-handler asset-factories (keyword domain) path))
        (wrap-html-markup (or html-routes placeholder-routes)))
      wrap-exception-reporting)))