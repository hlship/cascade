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

(ns com.howardlewisship.cascade.test-parser
  (:use clojure.contrib.test-is
   clojure.contrib.pprint
   com.howardlewisship.cascade.internal.parser))

(def #^{:private true} base "src/test/resources/com/howardlewisship/cascade/internal/")

(defn- extract-attributes [element-node]
  (let [attrs (element-node :attributes)]
    (reduce (fn combine [map attr-token] (assoc map (attr-token :name) (attr-token :value)))
      {}
      attrs)))

(defn- extract-ns-attributes [element-node]
  (let [attrs (element-node :attributes)]
    (reduce (fn combine [map attr-token] (assoc map {:ns-uri (attr-token :ns-uri)
                                                     :name (attr-token :name)}
                                                    (attr-token :value)))
      {}
      attrs)))

(defn expect-attributes [element-node attr-map]
  (is (= (extract-attributes element-node) attr-map)))

(defn expect-ns-attributes [element-node attr-map]
  (is (= (extract-ns-attributes element-node) attr-map)))

(deftest trival-document
  ; parse template may eventually return a list of nodes
  ; to reflect stuff besides the root element
  (let [[root] (parse-template (str base "root-only.xml"))]
    (is (= (root :type) :element))
    (is (not (nil? (root :token))))
    (is (= (-> root :token :tag) :root-node))
    (is (= (-> root :token :ns-uri) ""))
    (is (nil? (root :attributes)))
    (is (nil? (root :body)))))

(deftest single-attribute
  (let [[root] (parse-template (str base "single-attribute.xml"))]
    (expect-attributes root {:wife "suzy"})))

(deftest multiple-attributes
  (let [[root] (parse-template (str base "multiple-attributes.xml"))]
    (expect-attributes root {:x "20" :y "30"})))

(deftest nested-elements
  (let [[root] (parse-template (str base "nested-elements.xml"))
        body (root :body)
        pre-text (nth body 0)
        nested (nth body 1)
        post-text (nth body 2)
        nested-body (nested :body)
        inner-text (nth nested-body 0)]
    (expect-attributes root {:x "20" :y "30"})
    (is (= (-> nested :token :tag) :nested))
    (expect-attributes nested {:z "40"})
    (dorun
      (map #(is (= (.trim (-> %1 :token :value)) %2))
        [pre-text inner-text post-text]
        ["Pre-nested text", "Inner nested text", "Post-nested text"]))))

(deftest namespaced-elements
  (let [[root] (parse-template (str base "namespaced-elements.xml"))
        body (root :body)
        plain (nth body 1)
        nested1 (nth body 3)]

    (is (nil? (root :attributes)))
    (is (= (-> root :token :ns-uri) "cascade"))
    (is (= (root :ns-uri-to-prefix) {"cascade" "c"}))

    (is (= (-> plain :token :ns-uri) ""))

    (is (= (-> nested1 :token :ns-uri) "cascade"))

    (expect-ns-attributes nested1 {
      {:name :x :ns-uri ""} "20"
      {:name :y :ns-uri "cascade"} "30"
      })))