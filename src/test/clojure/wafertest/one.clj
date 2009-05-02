(ns wafertest.one
    (:use com.howardlewisship.cascade com.howardlewisship.cascade.parser clojure.xml))

(set! *warn-on-reflection* true)

(def t (parse-view-fragment-template "src/test/resources/wafertest/one/views/simple.xml"))

(println "parsed function: " t)
(println "generated DOM:   " (t nil))
(println "Markup:\n" (dom-to-string (t nil)))


(println "ns.xml: " (parse "src/test/resources/wafertest/one/views/ns.xml"))
