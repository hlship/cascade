(ns com.howardlewisship.cascade.view-manager
    (:use com.howardlewisship.cascade.dom
          com.howardlewisship.cascade.internal.parser))

; A render function takes a single parameter (the environment) and returns a seq
; of DOM nodes that can be transformed and then streamed. Any returned nil values
; are filtered out.

(defn- combine-render-funcs
  "Combines a number of render functions together to form a composite render function. "
  [funcs]
  (fn combined [env]
      (remove nil? (concat (for [f funcs] (f env))))))

(defn- construct-attributes
  "Convert attribute tokens into attribute DOM nodes."
  [attribute-tokens]
  (for [token attribute-tokens]
       (struct-map dom-node
                   :type :attribute
                   :ns-uri (token :ns-uri)
                   :name (token :name)
                   :value (token :value))))

(defmulti to-render-func :type
  "Converts a parsed template node into a function that will render some number of DOM nodes.")

(defmethod to-render-func :text [parsed-node]
  (fn [env] [(struct-map dom-node
                         :type :text :value (-> parsed-node :token :value))]))

(defmethod to-render-func :element [parsed-node]
  ; TODO: handle elements in the cascade namespace specially
  ; TODO: check for cascade namespace attributes
  (let [body (parsed-node :body)
        token (parsed-node :token)
        body-as-funcs (map to-render-func body)
        body-combined (combine-render-funcs body-as-funcs)
        attributes (construct-attributes (node :attributes))]
       (fn element-node-renderer [env]
           [(struct-map dom-node
                        :type :element
                        :ns-uri (token :ns-uri)
                        :name (token :token)
                        ; currently assuming that attributes are "static" but
                        ; that will change ... though we should seperate "static" from "dynamic"
                        :attributes attributes
                        ; TODO: there might be a way to identify that a static element has only static
                        ; content, in which case the body can itself be computed statically
                        :content (body-combined env))])))


(defn create-view
  "Parses a source file and creates a view function from it."
  [src]
  (let [root-node (parse-template src)]
       (to-render-func root-node)))