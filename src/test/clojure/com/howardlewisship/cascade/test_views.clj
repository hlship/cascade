(ns com.howardlewisship.cascade.test-views
    (:use clojure.contrib.test-is
          clojure.contrib.pprint
          com.howardlewisship.cascade.dom
          com.howardlewisship.cascade.view-manager))

(def #^{:private true} base "src/test/resources/")

(deftest simple-view
         (let [view (parse-and-create-view (str base "simple-view.xml"))
               dom (view {})
               output (with-out-str (render-xml dom *out*))]
              (print output)))