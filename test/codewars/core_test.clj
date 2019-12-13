(ns codewars.core-test
  (:require [clojure.test :refer :all]
            [codewars.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(defn test-assert [act exp]
  (is (= act exp)))

(deftest a-test1
  (testing "thirt"
   (test-assert(thirt 8529), 79)
   (test-assert(thirt 85299258), 31)
   (test-assert(thirt 5634), 57)
   (test-assert(thirt 1111111111), 71)
   (test-assert(thirt 987654321), 30)
))

(deftest a-test2
  (testing "Test 1"
    (def ur ["BBAR 150", "CDXE 515", "BKWR 250", "BTSQ 890", "DRTY 600"])
    (def vr ["A" "B" "C" "D"])
    (def res [["A" 0] ["B" 1290] ["C" 515] ["D" 600]])
    (is (= (stock-list ur vr) res))))

(deftest a-test3
  (testing "Test 3"
    (is (= (evaporator 10 10 10) 22))))
