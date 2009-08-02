(ns org.ego.xml-test
  (:use [org.ego.xml]
        [clojure.contrib.test-is]))
 
(deftest test-parse
  (testing "parse"
    (is (nil? (process :connect "test")))
    (is (empty? (process :upstream "test" "<te")))
    (is (empty? (process :upstream "test" "st")))
    (is (empty? (process :upstream "test" "s")))
    (is (nil? (process :connect "test2")))
    (is (= (first (process :upstream "test" ">"))
           {:state nil, :tag :start-element, :qname "tests", :attrs []}))
    (is (= [{:state nil, :tag :start-element, :qname "test", :attrs []} 
            {:state nil, :tag :end-element, :qname "test", :attrs []}]
           (process :upstream "test2" "<test></test>")))))


