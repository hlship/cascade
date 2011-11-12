(ns main
  (:use test-app ring.adapter.jetty))

(run-jetty app {:port 8080})