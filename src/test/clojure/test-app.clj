(set! *warn-on-reflection* true)

(require '(cascade.internal dispatcher)
         'app1.views)
(use 'cascade.jetty)

(pr (run-jetty "src/test/webapp"))