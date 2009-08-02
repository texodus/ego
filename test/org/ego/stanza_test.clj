(ns org.ego.stanza-test
  (:use [org.ego.stanza]
        [clojure.contrib.test-is]))
 
(deftest test-common
  (testing "alter-nil"
    (let [temp-ref (ref {:a 1})]
      (is (= (dosync (alter-nil temp-ref assoc :a 2)) nil))
      (is (= (:a @temp-ref)))))
  (testing "process - simple interleave"
    (is (nil? (process :connect "test")))
    (is (empty? (process :upstream "test" {:state nil, :tag :start-element, :qname "test", :attrs []})))
    (is (= (process :upstream "test" {:state nil, :tag :end-element, :qname "test", :attrs []})
           [{:tag :test, :attrs {}, :content nil}]))
    (is (nil? (process :disconnect "test")))))

