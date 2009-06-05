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

(ns com.howardlewisship.cascade.view-manager
  (:use
   com.howardlewisship.cascade.internal.utils
   com.howardlewisship.cascade.dom
   com.howardlewisship.cascade.config
   com.howardlewisship.cascade.internal.parser))

; A fragment function takes two parameters: env and params.  A view function is simply a wrapper
; around a fragment function that takes just the env and supplys nil for the params.

; Seems like there may be a monadic option for this assembly that might help with passing
; the namespace and other details around.

; Note: we use "namespace" when refering to a Clojure namepace (usually represented as a symbol), and
; "ns" to refer to a XML namespace (ns-uri and ns-prefix, typically).

; The URI for a fragment

(def fragment-uri "cascade")

(def fragment-cache (atom {}))
(def view-cache (atom {}))

(declare get-fragment)

; A render function takes a single parameter (the environment) and returns a collection
; of DOM nodes that can be transformed (not yet implemented) and then streamed. Any returned nil values
; are filtered out.

(defn- combine-render-funcs
  "Combines a number of render functions together to form a composite render function. "
  [funcs]
  ; TODO: optimize for # of funcs (0, 1, many)
  (fn combined
    [env params]
    (remove nil?
      (apply concat
        (for [f funcs]
          (f env params))))))

(defn- construct-attributes
  "Convert attribute tokens into attribute DOM nodes."
  [attribute-tokens]
  (for [token attribute-tokens]
    (struct-map dom-node
      :type :attribute
      :ns-uri (token :ns-uri)
      :name (token :name)
      :value (token :value))))

(defn- create-render-body-fn
  "Creates a function that takes a single env parameter and invokes the provided function
  to render the body, using the params of the enclosing/invoking fragment function."
  [body-combined container-params]
  (fn [env]
    (body-combined env container-params)))

(defn- remove-cascade-namespaces
  [ns-uri-to-prefix]
  (dissoc ns-uri-to-prefix fragment-uri))

(defn- wrap-fn-as-fragment-fn
  "Convert a function of no parameters into a function that accepts fragment parameters (env and params)."
  [f]
  (fn [env params] (f)))

(defn- wrap-dom-node-as-fragment-fn
  "Wraps a static DOM node as a fragment function (returning the DOM node in a vector)."
  [dom-node]
  (let [fn-result [dom-node]]
    (wrap-fn-as-fragment-fn (fn [] fn-result))))


; to-fragment-fn exists to convert parsed nodes (from the parser namespace) into fragment rendering functions.
; Many of these functions ignore their env and params arguments and return fixed DOM node values. Others are
; more involved and dynamic.

(defmulti to-fragment-fn #(:type %2))

(defmethod to-fragment-fn :text
  [namespace parsed-node]
  (wrap-dom-node-as-fragment-fn (struct-map dom-node :type :text :value (-> parsed-node :token :value))))

(defmethod to-fragment-fn :comment
  [namespace parsed-node]
  (wrap-dom-node-as-fragment-fn (struct-map dom-node :type :comment :value (parsed-node :text))))

(defn- create-fragment-renderer
  [namespace element-node]
  (let [body (element-node :body)
        token (element-node :token)
        element-uri (token :ns-uri)
        element-name (token :tag)
        element-ns-uri-to-prefix (element-node :ns-uri-to-prefix)
        body-as-funcs (map (partial to-fragment-fn namespace) body)
        body-combined (combine-render-funcs body-as-funcs)
        attributes (construct-attributes (element-node :attributes))]

    ; The fragment-renderer is responsible for providing parameters
    ; to the included fragment. Parameters aren't implemented yet.

    (fn fragment-renderer [env params]
                          ; TODO: Error if a fragment element defines any namespace besides cascade.
                          (let [frag-func (get-fragment (name element-name))
                                inner-params [] ; TODO: evaluate parameters
                                body-renderer (create-render-body-fn body-combined params)
                                ; TODO: rebuild token, stripping from :attributes any parameters
                                frag-env (merge env {:element-token token
                                                     :render-body body-renderer})]
                            (frag-func frag-env inner-params)))))

(defn- create-static-element-renderer
  [namespace element-node]
  (let [body (element-node :body)
        token (element-node :token)
        element-uri (token :ns-uri)
        element-name (token :tag)
        element-ns-uri-to-prefix (element-node :ns-uri-to-prefix)
        body-as-funcs (map (partial to-fragment-fn namespace) body)
        body-combined (combine-render-funcs body-as-funcs)
        attributes (construct-attributes (element-node :attributes))]
    (fn static-element-renderer [env params]
                                [(struct-map dom-node
                                  :type :element
                                  :ns-uri element-uri
                                  :ns-uri-to-prefix (remove-cascade-namespaces element-ns-uri-to-prefix)
                                  :name element-name
                                  ; currently assuming that attributes are "static" but
                                  ; that will change ... though we should seperate "static" from "dynamic"
                                  :attributes attributes
                                  ; TODO: there might be a way to identify that a static element has only static
                                  ; content, in which case the body can itself be computed statically
                                  :content (body-combined env params))])))

(defmethod to-fragment-fn :element
  [namespace element-node]
  ; TODO: handle elements in the cascade namespace specially
  ; TODO: check for cascade namespace attributes
  (let [element-uri (-> element-node :token :ns-uri)]

    (if (= fragment-uri element-uri)
      (create-fragment-renderer namespace element-node)
      (create-static-element-renderer namespace element-node))))

(defn parse-and-create-fragment
  "Parses a cascade template file and creates a fragment function from it."
  [namespace src]
  (let [nodes (parse-template src)
        funcs (map (partial to-fragment-fn namespace) nodes)]
    (combine-render-funcs funcs)))

(defn parse-and-create-view
  "Parses a cascade template file and creates a view function from it."
  [namespace src]
  (let [frag-func (parse-and-create-fragment namespace src)]
    (fn view-equivalent-of-fragment [env] (frag-func env nil))))

(defn- find-name-in-namespace
  [name namespace]
  (let [found (get (ns-interns (the-ns namespace)) name)]
    ; what may be found is a Var wrapping a function, we want
    ; the function
    (and found (deref found))))

(defn- search-namespaces
  [name namespaces]
  (first-non-nil (map (partial find-name-in-namespace name) namespaces)))


(defn- create-from-template
  "Searches for a template file (with a .cml extension) as a classpath resource in one of the namespaces,
creating a function (using the factory) if found. If not found, throws RuntimeException."
  [name namespaces factory-fn]
  (let [file (str name ".cml")
        template-fn (first-non-nil
      (for [namespace namespaces]
        (if-let [src (find-namespace-resource namespace file)]
          (factory-fn namespace src))))]

    (or
      template-fn
      (throw (RuntimeException. (format "Could not locate template '%s' in any of namespaces %s."
        file
        (to-str-list (map ns-name namespaces))))))))

(defn- find-or-create-fn
  "Searches for an existing function in any of the namespaces, or creates a function from a bare template."
  [name namespaces factory-fn]
  (or
    (search-namespaces name namespaces)
    (create-from-template name namespaces factory-fn)))

(defn- get-or-create-cached-fn
  "Gets a view or fragment function from a cache, or finds it as a real function,
or uses the factory function to create it dynamically (from a template)."
  [name cache config-key factory-fn]
  (if-let [existing (get @cache name)]
    existing
    (let [namespaces (get configuration config-key)
          created (find-or-create-fn name namespaces factory-fn)]
      (swap! cache assoc name created)
      created)))

(defn get-fragment
  "Gets a fragment function with a given string name. Fragment functions expect an env and a params and return a seq of render nodes."
  [name]
  (get-or-create-cached-fn name fragment-cache :fragment-namespaces parse-and-create-fragment))

(defn get-view
  "Gets a fragment function with a given string name. View functions expect an env and return a seq of render nodes."
  [name]
  (get-or-create-cached-fn name view-cache :view-namespaces parse-and-create-view))