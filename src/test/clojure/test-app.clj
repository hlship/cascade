(set! *warn-on-reflection* true)

(require '(com.howardlewisship.cascade.internal dispatcher)
         'app1.views)
(use 'com.howardlewisship.cascade.jetty)

(pr (run-jetty "src/test/webapp"))