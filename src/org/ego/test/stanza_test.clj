(ns org.ego.test.stanza-test
  (:gen-class)
  (:use [org.ego.stanza]
[clojure.contrib.test-is]))
 
(deftest test-common
  (testing "Common"

;(defn pair-off
;  [x y]
;  (map #(apply vector %)
;       (partition 2 (interleave x y))))

; (is (= (pair-off [1 2 3 4] [:a :b :c :d]) [[1 :a] [2 :b] [3 :c] [4 :d]]))
; (is (= (pair-off [] []) []))
; (is (= (pair-off [1 2 3] []) []))
; (is (= (pair-off [] [1 2 3]) []))

  (let [temp-ref (ref {:a 1})]
    (is (= (dosync (alter-nil temp-ref assoc :a 2)) nil))
    (is (= (:a @temp-ref)))
    )

  (testing "XML parser"
	 
  (testing "Channel Handler"))))
 
(defn -main
  []
  (run-tests 'org.ego.test.stanza-test))