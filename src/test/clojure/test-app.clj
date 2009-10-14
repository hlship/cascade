(set! *warn-on-reflection* true)

(use 'cascade.jetty)

(pr (run-jetty "src/test/webapp" 8080 'app1.views))