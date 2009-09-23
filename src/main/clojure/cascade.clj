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

(ns cascade
  (:import
    (javax.servlet.http HttpServletRequest HttpServletResponse))
  (:use
    (cascade path-map fail urls logging)
    (cascade.internal utils viewbuilder parse-functions)))
  
(defmacro template
  "Defines a block of the template DSL, which is converted into code that renders a seq of DOM nodes."
  [& forms]
  (parse-embedded-template forms))
    
(defmacro defview
  "Defines a Cascade view function, which uses an embedded template. A view function may have a doc string and meta data
  preceding the parameters vector. The parameters vector is followed by an optional vector to establish URL bindings. 
  The function's forms are an implicit inline block."
  [& forms]
  (let [[fn-name fn-params fn-bindings template-forms] (parse-function-def forms)
        env-symbol (first fn-params)
        full-meta (merge ^fn-name {:cascade-type :view})]
    `(add-mapped-function (defn ~fn-name 
                                ~full-meta
                                ~fn-params 
                                (parse-url ~env-symbol ~fn-bindings (template ~@template-forms))))))
 
(defmacro defaction
  "Defines a Cascade action function.  An action function may have a doc string and meta data
  preceding the parameters vector. The forms are interpreted normally (not as an embedded template)."
  [& forms]
  (let [[fn-name fn-params fn-bindings forms] (parse-function-def forms)
        env-symbol (first fn-params)
        full-meta (merge ^fn-name {:cascade-type :action})]
    `(add-mapped-function (defn ~fn-name 
                                ~full-meta 
                                ~fn-params 
                                (parse-url ~env-symbol ~fn-bindings ~@forms)))))

(defmacro block
  "Encapsulates a block of template forms as a function with parameters, typically used as
  a callback. The function, when invoked, returns a seq of DOM nodes."
  [fn-params & template-forms]
    `(fn ~fn-params (template ~@template-forms)))

(def #^{:doc "A DOM text node for a line break."}
  linebreak
  (text-node "\r"))

(defmacro link-map-from-function
  "Creates a link map from a function reference, extra path info (as a seq), optional extra query parameters (as a map)."
  [function extra-path-info query-parameters]
  `(link-map-from-path (path-for-function ~function) ~extra-path-info ~query-parameters))
        
(defn link-path
  "Converts a link map into an absolute path (but not a complete URL). The returned path string
  will include any query parameters and extra path info from the link map, and will have
  been encoded by the HttpServletResponse. Uses standard keys from the env map."
  [env link-map]
  (let [#^HttpServletRequest request (-> env :servlet-api :request)
        #^HttpServletResponse response (-> env :servlet-api :response)
        context-path (.getContextPath request)
        link-path (construct-absolute-path context-path link-map)]
    (.encodeURL response link-path)))
                  
(defmacro link
  "Creates a link to a view or action function. Additional path info data may be specified (as a seq of
  data items),
  as well as query parameters (as a map whose keys are strings or keywords and whose values are converted to strings.).
  Uses standard keys from the env map. The resulting link is returned as a string."
  ([env function]
    (link env function nil))
  ([env function extra-path-info]
    (link env function extra-path-info nil))
  ([env function extra-path-info query-parameters]
    `(link-path ~env (link-map-from-function ~function ~extra-path-info ~query-parameters))))

(defmacro render-link
  "Creates a hyperlink with an href as a link to a view or action function.
  Following the function, is an optional vector of extra path information, then
  an optional map of query parameters. Additional forms after that form an implicit template."
  [env function & forms]
  (let [[extra-path query-parameters template-forms] (parse-render-link-forms forms)]
    `(template :a { :href (link ~env ~function ~extra-path ~query-parameters) } [ ~@template-forms ])))

(defn send-redirect
  "Sends a redirect to the client using a link map created by the link macro. Returns true."
  [env #^String link-path]
  (let [#^HttpServletResponse response (-> env :servlet-api :response)]
       (.sendRedirect response link-path)
       true))
       