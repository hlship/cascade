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
    [cascade dom asset]))

(defn wrap-exception-handling
  "Middleware for standard Cascade exception reporting; exceptions are caught and reported using the Cascade
exception report view.  *NOT YET IMPLEMENTED*"
  [handler]
  ; Just a placeholder for now
  handler)

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

(defn initialize
  "Initializes asset handling for Cascade. This sets an application version (a value incorporated into URLs, which
should change with each new deployment. Named arguments:
:virtual-folder (default \"assets\")
  The root folder under which assets will be exposed to the client.
:public-folder (default \"public\")
  The file system folder under which file assets are stored. May be an absolute path, should not end with a slash.
:file-extensions
  Additional file-extension to MIME type mappings, beyond the default set (defined by ring.util.mime-type/default-mime-types).
:asset-factories
  Additional asset dispatcher mappings. Keys are domain keywords, values are functions that accept a path within that domain.
The functions should construct and return a cascade/Asset."
  [application-version & {:keys [virtual-folder public-folder file-extensions asset-factories]
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
    (wrap-exception-handling
      (GET [(str root "/:domain/:path") :path #".*"]
        [domain path] (asset-handler asset-factories (keyword domain) path)))))

(defn wrap-html
  "Ring middleware that wraps a handler so that the return value from the handler (a seq of DOM nodes)
is serialized to HTML (as lazy seq of strings)."
  [handler]
  (fn [req]
    (->
      (handler req)
      serialize-html)))