(ns org.ego-test
  (:require [org.ego.stanza-test]
            [org.ego.xml-test])
  (:use [clojure.contrib.test-is]))
   
(run-tests 'org.ego.stanza-test 'org.ego.xml-test)