(ns wafertest.one
    (:use com.howardlewisship.wafer com.howardlewisship.wafer.parser))

(def t (parse-view-fragment-template "src/test/resources/wafertest/one/views/simple.xml"))

(println "parsed function: " t)
(println "generated DOM:   " (t nil))
(println "Markup:\n" (dom-to-string (t nil)))
         

