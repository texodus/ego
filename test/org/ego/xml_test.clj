(ns org.ego.xml-test
  (:use [org.ego.xml]
        [clojure.contrib.test-is]))
 
(deftest test-parse
  (testing "parse - basic interleave"
    (is (nil? (process :connect "test")))
    (is (empty? (process :upstream "test" "<te")))
    (is (empty? (process :upstream "test" "st")))
    (is (empty? (process :upstream "test" "s")))
    (is (nil? (process :connect "test2")))
    (is (empty? (process :upstream "test2" "<te")))
    (is (= (first (process :upstream "test" ">"))
           {:state nil, :tag :start-element, :qname "tests", :attrs []}))
    (is (= (process :upstream "test2" "st></test>")
           [{:state nil, :tag :start-element, :qname "test", :attrs []} 
            {:state nil, :tag :end-element, :qname "test", :attrs []}]))
    (is (nil? (process :disconnect "test")))
    (is (nil? (process :disconnect "test2"))))
  (testing "parse - attributes"
    (is (nil? (process :connect "test")))
    (is (= (first (process :upstream "test" "<test name='x'>"))
           {:state nil, :tag :start-element, :qname "test", :attrs [["name" "x"]]}))
    (is (nil? (process :connect "test2")))
    (is (= (first (process :upstream "test2" "</test name='x' val=\"value\">"))
           {:state nil, :tag :end-element, :qname "test", :attrs [["val" "value"] ["name" "x"]]}))))
           

