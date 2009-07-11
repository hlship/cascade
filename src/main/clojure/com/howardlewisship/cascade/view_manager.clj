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
; -> parsed DOM nodes -> Clojure function -> rendered DOM nodes -> markup stream. 

; The URI for a fragment

(def cascade-namespace-uri "cascade")

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

(defn to-dom-node-seq
  [any]
  "Converts the result of a render function to a seq as needed."
  (cond
    (nil? any) nil

    (sequential? any) any

    ; A map is assumed to be a DOM node, wrap it in a vector
    (map? any) [any]
    
    ; TODO: Allow a string or number to render as itself. Maybe convert this to a multifunction.

    true (throw (RuntimeException. (format "A render function returned %s. Render functions should return nil, a seq of DOM nodes, or a single DOM node."
    (pr-str any))))))

(defn combine-render-funcs
  "Combines a number of render functions together to form a composite render function. "
  [funcs]
  ; TODO: optimize for # of funcs (0, 1, many)
  (fn do-render-combined
    [env params]
    (remove nil? (apply concat (for [f funcs] (to-dom-node-seq (f env params)))))))

(defn static-text-to-value-fn
  [text]
  (if (blank? text)
    nil
    (fn [_ _] text)))

(defn match-result-to-value-fn
  [namespace #^MatchResult match-result]
  (to-value-fn namespace (.group match-result 1)))

(defn to-attribute-value-eval-fn
  "Converts an attribute value (a string) into a function that generates the value for the attribute
  (from env and params)."
  [namespace attribute-value]
  (let [match-fn (partial match-result-to-value-fn namespace)
        value-fns (re-map expansion-re attribute-value static-text-to-value-fn match-fn)]
    (fn do-convert-attribute [env params]
     ; TODO: Exception catch & report, tie to a location.
      (apply str (map #(% env params) value-fns)))))

(defn create-attributes-gen-fn
  "Creates a function that accepts env and params and returns a sequence of
attribute dom-nodes. Handles expansions in each attribute value."
  [namespace attribute-tokens]
  (let [pairs (for [token attribute-tokens]
    ; create a pair consisting of a dom-node skeleton,
    ; plus a function to generate the attribute value
    [(struct-map dom-node ; dom-node skeleton for the attribute
      :type :attribute
      :ns-uri (token :ns-uri)
      :name (token :name)) 
      
      ; function that generates the final :value
      (to-attribute-value-eval-fn namespace (token :value))])]
    (fn do-create-attributes [env params]
      (for [[dom-node-skel value-fn] pairs]
        (assoc dom-node-skel :value (value-fn env params))))))

(defn create-render-body-fn
  "Creates a function that takes a single env parameter and invokes the provided function
to render the body, using the params of the enclosing/invoking fragment function."
  [body-combined container-env container-params]
  ; Re-associate the :render-body function of the container, so that if we hit 
  ; another render-body directive, it is the container's body that gets rendered, not the
  ; fragment's.
  (fn do-render-body [env]
    (body-combined (assoc env :render-body (container-env :render-body)) container-params)))

(defn remove-cascade-namespaces
  [ns-uri-to-prefix]
  (dissoc ns-uri-to-prefix cascade-namespace-uri))

(defn wrap-fn-as-fragment-fn
  "Convert a function of no parameters into a fragment function (that takes env and params)."
  [f]
  (fn [_ _] (f)))

(defn wrap-dom-node-as-fragment-fn
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


(defn convert-dynamic-attributes-to-param-gen-fn
  "Converts attribute tokens into a function that accepts env and params and produces a new params (that can be passed
to a subordinate fragment)."
  [namespace tokens]
  (let [name-fn-pairs
        (for [token (filter-matches :ns-uri cascade-namespace-uri tokens)]
          (let [{name :name value :value} token]
             [name (to-value-fn namespace value)]))]
    (fn do-provide-params
      [env params]
      ; Invoke each function using the provided env and params
      (let [invoked-pairs (for [[name value-fn] name-fn-pairs] [name (value-fn env params)])]
        ; Then build up the final params map
        (reduce (fn [map [key value]] (assoc map key value)) {} invoked-pairs)))))

(defn static-text-to-render-fn
  "Converts simple static text to a render function."
  [text]
  (if (blank? text)
    nil
    (fn do-static-text [_ _]
      (struct-map dom-node :type :text :value text))))

(defn expression-to-render-fn
  "Converts an expression string and a namespace to a render function."
  [namespace expression-str]
  (let [value-fn (to-value-fn namespace expression-str)]
    (fn do-expression-to-dom-node [env params]
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
    (fn do-render-text-with-expansions [env params]
      (for [f render-fns] (f env params)))))

(defmethod to-fragment-fn :comment
  [namespace parsed-node]
  (wrap-dom-node-as-fragment-fn (struct-map dom-node :type :comment :value (parsed-node :text))))

(defn create-fragment-renderer
  [namespace element-node]
  (let [body (element-node :body)
        token (element-node :token)
        element-name (token :tag)
        element-ns-uri-to-prefix (element-node :ns-uri-to-prefix)
        body-as-funcs (map (partial to-fragment-fn namespace) body)
        body-combined (combine-render-funcs body-as-funcs)
        attributes (element-node :attributes)
        params-gen-fn (convert-dynamic-attributes-to-param-gen-fn namespace attributes)
        static-attributes (remove #(= cascade-namespace-uri (% :ns-uri)) attributes)
        attributes-gen-fn (create-attributes-gen-fn namespace static-attributes)]

    ; The fragment-renderer is responsible for providing parameters
    ; to the subordinate fragment. The fragment renderer receives its container's
    ; environment and parameters and uses those to build an environment and parameters
    ; for the fragment.

    (fn do-render-fragment
      [container-env container-params]
      (let [fragment-fn (get-fragment (name element-name))
            frag-params (params-gen-fn container-env container-params)
            frag-attributes (attributes-gen-fn container-env container-params)
            body-renderer (create-render-body-fn body-combined container-env container-params)
            frag-env (merge container-env {:fragment-token (assoc token :attributes frag-attributes)
                                           :render-body body-renderer})]
        (fragment-fn frag-env frag-params)))))

(defn create-render-body-renderer
  "Creates a renderer for the render-body directive." 
  [namespace element-node]
  ; TODO check that there are no attributes, no child nodes
  (fn do-render-body [env params]
    ((env :render-body) env)))

(defn create-cascade-namespace-element-renderer
  "Creates a renderer function from an element already determined to be in the cascade namespace; this is typically a fragment, 
but may also be a built-in directive such as render-body."
  [namespace element-node]
  (let [element-name (-> element-node :token :tag)]
    (cond
      (= element-name :render-body) (create-render-body-renderer namespace element-node)
      true (create-fragment-renderer namespace element-node))))

(defn create-static-element-renderer
  [namespace element-node]
  ; TODO: Ensure that no attributes are in cascade namespace.
  (let [body (element-node :body)
        token (element-node :token)
        element-uri (token :ns-uri)
        element-name (token :tag)
        element-ns-uri-to-prefix (element-node :ns-uri-to-prefix)
        body-as-funcs (map (partial to-fragment-fn namespace) body)
        body-combined (combine-render-funcs body-as-funcs)
        attributes-gen-fn (create-attributes-gen-fn namespace (element-node :attributes))]
    (fn do-render-static-element [env params]
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
  (let [element-uri (-> element-node :token :ns-uri)]
    (if (= cascade-namespace-uri element-uri)
      (create-cascade-namespace-element-renderer namespace element-node)
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
    (fn do-view-wrapper [env] (frag-func env nil))))

(defn find-name-in-namespace
  [name namespace]
  (let [mappings (ns-interns (the-ns namespace))
        found (get mappings (symbol name))]
    ; what may be found is a Var wrapping a function, we want
    ; the function
    (and found (deref found))))

(defn search-namespaces
  [name namespaces]
  (first-non-nil (map (partial find-name-in-namespace name) namespaces)))


(defn create-from-template
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

(defn find-or-create-fn
  "Searches for an existing function in any of the namespaces, or creates a function from a bare template."
  [name namespaces factory-fn]
  (or
    (search-namespaces name namespaces)
    (create-from-template name namespaces factory-fn)))

(defn get-or-create-cached-fn
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