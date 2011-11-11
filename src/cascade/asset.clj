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

(ns cascade.asset
  "Defines basic types of assets (resources exposed to the client browser)."
  (:use
    [cascade dom])
  (:import
    [java.io File FileInputStream BufferedInputStream InputStream]
    [java.net URL]))

; This may change into a value passed as a key in a filter, or perhaps a rebindable var.
(def asset-configuration
  "The atom containing the asset configuration, needed to create Asset and handle asset requests.
This should only be changed at startup, by (cascade.request/initialize)."
  (atom nil))

(defprotocol Asset
  "Represent a server-side resource so that it can be exposed efficiently to the client."
  (^String file-name [asset] "Returns just the name of the Asset.")
  (^InputStream content-stream [asset] "Returns the content of the Asset as a stream of bytes, or null if the Asset does not exist.")
  (^String client-url [asset] "Returns an absolute URL to the Asset."))

(defrecord FileAsset [^URL file url]
  Asset
  (file-name [asset] (.getName file))
  (content-stream [asset]
    (if (.canRead file)
      (-> (FileInputStream. file) BufferedInputStream.)))
  (client-url [asset] url)

  ; Each "implementation" of Asset should also extend ToAttributeValueString
  ; There isn't a proper way to express this in Clojure, though I think there
  ; should be.

  ToAttributeValueString
  (to-attribute-value-string [asset] url))

(defn build-client-url
  "Builds a complete client URL for an asset.
domain
  Keyword for the asset domain (either :file or :classpath, for the built-in domains)
path
  Path within the domain to the asset, consisting of a series of names seperated by slashes (but not starting with one)."
  [domain path]
  (str (:assets-folder @asset-configuration) "/" (name domain) "/" path))

(defn file-asset
  "Creates an Asset representing a file in the application's public folder, as configured in (initialize)."
  [path]
  (let [file-path (str (:public-folder @asset-configuration) "/" path)
        file (File. file-path)
        client-url (build-client-url :file path)]
    (->FileAsset file client-url)))

(defrecord ClasspathAsset [name ^URL resource-url url]
  Asset
  (file-name [asset] name)
  (content-stream [asset]
    (and resource-url (-> (.openStream resource-url) BufferedInputStream.)))
  (client-url [asset] url)

  ToAttributeValueString
  (to-attribute-value-string [asset] url))

(defn classpath-to-url
  "Given a path on the classpath, obtains the URL for the resource."
  [path]
  (-> (Thread/currentThread) .getContextClassLoader (.getResource path)))

(defn path-to-file
  "Given a path name, finds the last slash character, and returns the file name that follows it."
  [path]
  (last (.split path "/")))

(defn is-safe
  "Returns false if the path ends in \".class\", and true otherwise."
  [path]
  (not (re-find #"\.class$" path)))

(defn classpath-asset
  "Creates an Asset representing a file on the classpath."
  [path]
  (let [file-name (path-to-file path)
        resource-url (and (is-safe path) (classpath-to-url path))
        client-url (build-client-url :classpath path)]
    (->ClasspathAsset file-name resource-url client-url)))

(defn get-content-type [asset]
  (let [name (file-name asset)
        dotx (.lastIndexOf name ".")
        extension (.substring name (inc dotx))
        content-type (-> @asset-configuration :file-extensions (get extension))]
    (or content-type "text/plain")))



