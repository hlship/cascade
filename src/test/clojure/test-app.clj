(set! *warn-on-reflection* true)

(require 'app1.views)
(use 'cascade.jetty)

(pr (run-jetty "src/test/webapp"))