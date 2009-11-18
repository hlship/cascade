; Copyright 2009 Howard M. Lewis Ship
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
  #^{:doc "Asset management"}
  cascade.asset
  (:import
    (java.net URL URLConnection)
    (javax.servlet ServletContext)
    (javax.servlet.http HttpServletRequest HttpServletResponse))
  (:require
    (clojure.contrib [str-utils2 :as s2]))
  (:use
    (clojure.contrib duck-streams)
    (cascade config fail path-map logging)))

(assoc-in-config :application-version
  (Long/toHexString (System/nanoTime)))

(assoc-in-config :asset-blacklist
  [#"\.class$", #"\.clj$"])

(def ten-years-from-now
  (+ (System/currentTimeMillis) (* 1000 60 60 24 365 10)))

;; Assets represents files stored either under the web context or stored within the classpath.
;; Identifying an asset will eventually incorporate a locale-specific search. Ultimately,
;; assets will be (optionally) GZip compressed, for clients which support GZip compression.
;; Once an asset is located, it is represented as a map, with keys:
;;   :type (:context or :classpath)
;;   :path string -- path relative to the "root" (the web context, or the root of the classpath), no leading slash
;; TODO: Extra keys for date time modified, file size, mime type?
;; TODO: lots of caching

(defn is-allowed-path
  "Apply blacklist rules to see if the path is allowed (not on the blacklist)."
  [path]
  (not-any? #(re-find % path) (read-config :asset-blacklist)))

(defn fail-if-blacklisted
  "Throws RuntimeException if the path is on the blacklist."
  [path]
  (fail-unless (is-allowed-path path) "Asset '%s' is on the blacklist." path))

(defn get-classpath-asset
  "Locates an asset on the classpath. Throws RuntimeException if the asset does not exist.
  Returns an asset map. The path is relative to the classpath root and should not
  start with a slash."
  [path]
  (let [asset-url (.getResource context-class-loader path)]
    (fail-if (nil? asset-url) "Asset '%s' not found on the classpath." path)
    (fail-if-blacklisted path)
    { :type :classpath
      :path path
    }))

(defn get-context-asset
  "Locates an asset in the web application context. Throws RuntimeException if the asset
  does not exist. Returns an asset map. The path is relative to the web context root and should
  not start with a slash."
  [path]
  (let [#^ServletContext context (read-config :servlet-context)
        asset-url (.getResource context (str "/" path))]
    (fail-if (nil? asset-url) "Asset '%s' not found in the context." path)
    { :type :context
      :path path
    }))

(assoc-in-config [:asset-resolver :classpath] #'get-classpath-asset)

(assoc-in-config [:asset-resolver :context] #'get-context-asset)

(defn get-asset
  "Gets an asset within a domain (defined by type, :context or :classpath). Throws RuntimeException if
  the asset can not be found, or if the asset is blacklisted. The behavior of this method can be
  extended via the :asset-resolver configuration key, which is a map to function that take an env and a path."
  [type path]
  ((read-config [:asset-resolver type]) path))

(defn construct-asset-path
  [env folder asset-map]
  (let [#^HttpServletRequest request (-> env :servlet-api :request)
        context-path (.getContextPath request)]
    (format "%s/asset/%s/%s/%s" context-path folder (read-config :application-version) (asset-map :path))))        
  
(defmulti to-asset-path
  "Converts an asset map into a URL path that can be used by the client to obtain the contents of the asset."
  #(:type %2))

(defmethod to-asset-path :classpath
  [env asset-map]
  (construct-asset-path env "classpath" asset-map))

(defmethod to-asset-path :context
  [env asset-map]
  (construct-asset-path env "context" asset-map))

(defn get-mime-type
  "Determine the MIME type of the path."
  [path]
  (let [#^ServletContext context (read-config :servlet-context)]
    (or
      (.getMimeType context path)
      ;; TODO: internal configuration lookup
      "application/octet-stream")))

(defn open-output-stream-for-asset
  "Configures the response in preperation for writing content."
  [#^HttpServletResponse response #^URL asset-url #^String mime-type]
  (let [#^URLConnection connection (.openConnection asset-url)
        last-modified (.getLastModified connection)
        content-length (.getContentLength connection)]
    (if-not (zero? content-length)
      (.setContentLength response content-length))
    (doto response
      (.setDateHeader "Last-Modified" last-modified)
      (.setDateHeader "Expires"  ten-years-from-now)
      (.setContentType mime-type))
    (.getOutputStream response)))
                 
(defn asset-request-dispatcher
  "Processes a request for an asset. The domain-name is used in exceptions. url-provider is a function passed
  an asset-path string that returns a URL for the asset, or nil if not found."
  [env domain-name url-provider]
  (let [#^HttpServletResponse response (-> env :servlet-api :response)
        [_ _ application-version & asset-path-terms] (-> env :cascade :split-path)
        asset-path (s2/join "/" asset-path-terms)
        ; TODO: Should we just ignore bad requests, let the container send a 404?
        _ (fail-if (nil? application-version) "Invalid %s asset URL." domain-name)
        _ (fail-unless (= application-version (read-config :application-version)) "Incorrect application version.")
        asset-url (url-provider asset-path)
        _ (fail-if (nil? asset-url) "Could not locate %s asset %s." domain-name asset-path)
        mime-type (get-mime-type asset-path)]
    (with-open [output-stream (open-output-stream-for-asset response asset-url mime-type)]  
      (with-open [input-stream (.openStream asset-url)]
        (copy input-stream output-stream)
        (.flush output-stream))))
  ; If we got this far, we copied the contents (or failed, throwing an exception)
  true)

(defn classpath-asset-dispatcher
  [env]
  (asset-request-dispatcher env "classpath" 
    (fn [asset-path]
      (fail-if-blacklisted asset-path)
      (.getResource context-class-loader asset-path))))

(defn context-asset-dispatcher
  [env]
  (asset-request-dispatcher env "context"
    (fn [asset-path]
      ; TODO: check for WEB-INF
      (let [#^ServletContext context (-> env :servlet-api :context)]
        (.getResource context (str "/" asset-path))))))
        
(add-function-to-config :dispatchers "asset/classpath" #'classpath-asset-dispatcher)
(add-function-to-config :dispatchers "asset/context" #'context-asset-dispatcher)
