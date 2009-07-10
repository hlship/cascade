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
  (:import (java.util.regex MatchResult))
  (:use
    (com.howardlewisship.cascade.internal utils parser)
    (com.howardlewisship.cascade dom config)
    clojure.contrib.with-ns))

; A fragment function takes two parameters: env and params.  A view function is simply a wrapper
; around a fragment function that takes just the env and supplys nil for the params.

; Seems like there may be a monadic option for this assembly that might help with passing
; the namespace and other details around.

; Note: we use "namespace" when refering to a Clojure namepace (usually represented as a symbol), and
; "ns" to refer to a XML namespace (ns-uri and ns-prefix, typically).

; This module is getting somewhat large and perhaps needs to be split into sub-modules. I think a module for the
; building side (i.e., to-fragment-fn and everything related) can be split out.

; We currently have two different DOMs; thus template XML -> XML tokens
; -> parsed DOM nodes -> Clojure function -> rendered DOM nodes -> markup stream 

; The URI for a fragment

(def fragment-uri "cascade")

(def expansion-re #"\$\{(.*?)\}")

; TODO: Eventually, when we have (defview) and (deffragment), we may need two levels of cache:
; for the fragment & view functions derived from templates, and for the fragment & view functions
; defined by the (defview) and (deffragment) macros.

(def fragment-cache (atom {}))
(def view-cache (atom {}))

(declare get-fragment to-value-fn)

; A render function takes a single parameter (the environment) and returns a collection
; of DOM nodes that can be transformed (not yet implemented) and then streamed. Any returned nil values
; are filtered out.

(defn- to-dom-node-seq
  [any]
  "Converts the result of a render function to a seq as needed."
  (cond
    (nil? any) nil

    (sequential? any) any

    (map? any) [any]
    
    ; TODO: All a string or number to render as itself. Maybe convert this to a multifunction.

    true (throw (RuntimeException. (format "A render function returned %s. Render functions should return nil, a seq of DOM nodes, or a single DOM node."
    (pr-str any))))))

(defn- combine-render-funcs
  "Combines a number of render functions together to form a composite render function. "
  [funcs]
  ; TODO: optimize for # of funcs (0, 1, many)
  (fn combined
    [env params]
    (remove nil? (apply concat (for [f funcs] (to-dom-node-seq (f env params)))))))

(defn- static-text-to-value-fn
  [text]
  (if (blank? text)
    nil
    (fn [_ _] text)))

(defn- match-result-to-value-fn
  [namespace #^MatchResult match-result]
  (to-value-fn namespace (.group match-result 1)))

(defn- to-attribute-fn
  "Converts an attribute value (a string) into a function that generates the value for the attribute
  (from env and params)."
  [namespace attribute-value]
  (let [match-fn (partial match-result-to-value-fn namespace)
        value-fns (re-map expansion-re attribute-value static-text-to-value-fn match-fn)]
    (fn attribute-fn [env params]
      (apply str (map #(% env params) value-fns)))))

(defn- create-attributes-gen-fn
  "Creates a function that accepts env and params and returns a sequence of
attribute dom-nodes. Handles expansions in each attribute value."
  [namespace attribute-tokens]
  (let [pairs (for [token attribute-tokens]
    ; create a pair consisting of a dom-node skeleton,
    ; plus a function to generate the attribute value
    [(struct-map dom-node
      :type :attribute
      :ns-uri (token :ns-uri)
      :name (token :name)) (to-attribute-fn namespace (token :value))])]
    (fn attributes-gen [env params]
      (for [[dom-node-skel attribute-fn] pairs]
        (assoc dom-node-skel :value (attribute-fn env params))))))

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
  "Convert a function of no parameters into a fragment function (that takes env and params)."
  [f]
  (fn [_ _] (f)))

(defn- wrap-dom-node-as-fragment-fn
  "Wraps a static DOM node as a fragment function (returning the DOM node in a vector)."
  [dom-node]
  (let [fn-result [dom-node]]
    (wrap-fn-as-fragment-fn (fn [] fn-result))))

(defn to-value-fn
  "Converts a string expression into a function. The function will take two parameters,
env and params, return a single value."
  [namespace expression-string]
  (let [expression-form (read-single-form expression-string)]
    ; TODO: control over function's parameter names
    (binding [*ns* (the-ns namespace)]
      (eval (list 'fn ['env 'params] expression-form)))))


(defn- convert-dynamic-attributes-to-param-gen-fn
  "Converts attribute tokens into a function that accepts env and params and produces a new params (that can be passed
to a subordinate fragment)."
  [namespace tokens]
  (let [name-fn-pairs
        (for [token (filter-matches :ns-uri fragment-uri tokens)]
          (let [{name :name value :value} token]
             [name (to-value-fn namespace value)]))]
    (fn provide-subordinate-params
      [env params]
      ; Invoke each function using the provided env and params
      (let [invoked-pairs (for [[name value-fn] name-fn-pairs] [name (value-fn env params)])]
        ; Then build up the final params map
        (reduce (fn [map [key value]] (assoc map key value)) {} invoked-pairs)))))

(defn- static-text-to-render-fn
  "Converts simple static text to a render function."
  [text]
  (if (blank? text)
    nil
    (fn static-text [_ _]
      (struct-map dom-node :type :text :value text))))

(defn- expression-to-render-fn
  "Converts an expression string and a namespace to a render function."
  [namespace expression-str]
  (let [value-fn (to-value-fn namespace expression-str)]
    (fn expression-to-text [env params]
      (struct-map dom-node :type :text :value (str (value-fn env params))))))

; to-fragment-fn exists to convert parsed nodes (from the parser namespace) into fragment rendering functions.
; Many of these functions ignore their env and params arguments and return fixed DOM node values. Others are
; more involved and dynamic.

(defmulti to-fragment-fn
  "Converts any kind of parsed DOM node into a fragment function (which takes env and params and returns
a collection of renderable DOM nodes)."
  #(:type %2))

(defmethod to-fragment-fn :text
  [namespace parsed-node]
  (let [text (-> parsed-node :token :value)
        match-to-render-fn (fn [#^MatchResult match-result] (expression-to-render-fn namespace (.group match-result 1)))
        render-fns (re-map expansion-re text static-text-to-render-fn match-to-render-fn)]
    (fn combined [env params]
      (for [f render-fns] (f env params)))))

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
        params-gen-fn (convert-dynamic-attributes-to-param-gen-fn namespace (element-node :attributes))]

    ; The fragment-renderer is responsible for providing parameters
    ; to the subordinate fragment. The fragment renderer receives its container's
    ; environment and parameters and uses those to build an environment and parameters
    ; for the fragment.

    (fn fragment-renderer
      [container-env container-params]
      ; TODO: Error if a fragment attribute defines any namespace besides cascade.
      (let [fragment-fn (get-fragment (name element-name))
            frag-params (params-gen-fn container-env container-params)
            body-renderer (create-render-body-fn body-combined container-params)
            ; TODO: rebuild token, stripping from :attributes any parameters
            frag-env (merge container-env {:fragment-token token
                                           :container-env container-env
                                           :render-body body-renderer})]
        (fragment-fn frag-env frag-params)))))

(defn- create-static-element-renderer
  [namespace element-node]
  (let [body (element-node :body)
        token (element-node :token)
        element-uri (token :ns-uri)
        element-name (token :tag)
        element-ns-uri-to-prefix (element-node :ns-uri-to-prefix)
        body-as-funcs (map (partial to-fragment-fn namespace) body)
        body-combined (combine-render-funcs body-as-funcs)
        attributes-gen-fn (create-attributes-gen-fn namespace (element-node :attributes))]
    (fn static-element-renderer [env params]
      (struct-map dom-node
        :type :element
        :ns-uri element-uri
        :ns-uri-to-prefix (remove-cascade-namespaces element-ns-uri-to-prefix)
        :name element-name
        :attributes (attributes-gen-fn env params)
        ; TODO: there might be a way to identify that a static element has only static
        ; content, in which case the body can itself be computed statically
        :content (body-combined env params)))))

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
  (let [mappings (ns-interns (the-ns namespace))
        found (get mappings (symbol name))]
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
      (throw (RuntimeException.
        (format "Could not locate function %s or template '%s' in %s %s."
          name file
          (if (= 1 (count namespaces)) "namespace" "namespaces")
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
  "Gets a fragment function with a given string name. Fragment functions expect an env and a params and return a seq of rendered DOM nodes."
  [name]
  (get-or-create-cached-fn name fragment-cache :fragment-namespaces parse-and-create-fragment))

(defn get-view
  "Gets a fragment function with a given string name. View functions expect an env and return a seq of rendered DOM nodes."
  [name]
  (get-or-create-cached-fn name view-cache :view-namespaces parse-and-create-view))