(ns com.howardlewisship.cascade.test-parser
    (:use clojure.contrib.test-is
          com.howardlewisship.cascade.parser))

(def #^{:private true} base "src/test/resources/com/howardlewisship/cascade/internal/")

(defn- extract-attributes [element-node]
  (let [attrs (element-node :attributes)]
       (reduce (fn combine [map attr-token] (assoc map (attr-token :name) (attr-token :value)))
               {}
               attrs)))

(defn expect-attributes [element-node attr-map]
  (is (= (extract-attributes element-node) attr-map)))

(deftest test-add-to-key-list
         (let [initial {}
               first-add (add-to-key-list initial :foo 1)
               second-add (add-to-key-list first-add :foo 2)]
              (is (= first-add {:foo [1]}))
              (is (= second-add {:foo [1 2]}))))

(deftest trival-document
         ; parse template may eventually return a list of nodes
         ; to reflect stuff besides the root element
         (let [root (parse-template (str base "root-only.xml"))]
              (is (= (root :type) :element))
              (is (not (nil? (root :token))))
              (is (= (-> root :token :tag) :root-node))
              (is (nil? (root :attributes)))
              (is (nil? (root :body)))))

(deftest single-attribute
         (let [root (parse-template (str base "single-attribute.xml"))]
              (expect-attributes root {:wife "suzy"})))

(deftest multiple-attributes
         (let [root (parse-template (str base "multiple-attributes.xml"))]
              (expect-attributes root {:x "20" :y "30"})))

(deftest nested-elements
         (let [root (parse-template (str base "nested-elements.xml"))
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