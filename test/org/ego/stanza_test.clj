(ns org.ego.stanza-test
  (:gen-class)
  (:use [org.ego.stanza]
        [clojure.contrib.test-is]))
 
(deftest test-common
  (testing "alter-nil"
    (let [temp-ref (ref {:a 1})]
      (is (= (dosync (alter-nil temp-ref assoc :a 2)) nil))
      (is (= (:a @temp-ref))))))
