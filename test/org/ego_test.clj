(ns org.ego-test
  (:require [org.ego.stanza-test]
            [org.ego.xml-test])
  (:use [clojure.contrib.test-is]))
        
(deftest test-common
  (testing "alter-nil"))
   
(run-tests 'org.ego.stanza-test 'org.ego.xml-test)