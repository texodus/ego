(ns org.ego-test
  (:gen-class)
  (:use [clojure.contrib.test-is]))
        
(deftest test-common
  (testing "alter-nil"))
   
(run-tests 'org.ego.stanza-test)