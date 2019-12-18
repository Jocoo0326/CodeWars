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

(deftest test-accum
  (testing "Accum, Basic tests"
    (is (= (accum "ZpglnRxqenU"), "Z-Pp-Ggg-Llll-Nnnnn-Rrrrrr-Xxxxxxx-Qqqqqqqq-Eeeeeeeee-Nnnnnnnnnn-Uuuuuuuuuuu"))
    (is (= (accum "NyffsGeyylB"), "N-Yy-Fff-Ffff-Sssss-Gggggg-Eeeeeee-Yyyyyyyy-Yyyyyyyyy-Llllllllll-Bbbbbbbbbbb"))
    (is (= (accum "MjtkuBovqrU"), "M-Jj-Ttt-Kkkk-Uuuuu-Bbbbbb-Ooooooo-Vvvvvvvv-Qqqqqqqqq-Rrrrrrrrrr-Uuuuuuuuuuu"))
    (is (= (accum "EvidjUnokmM"), "E-Vv-Iii-Dddd-Jjjjj-Uuuuuu-Nnnnnnn-Oooooooo-Kkkkkkkkk-Mmmmmmmmmm-Mmmmmmmmmmm"))
    (is (= (accum "HbideVbxncC"), "H-Bb-Iii-Dddd-Eeeee-Vvvvvv-Bbbbbbb-Xxxxxxxx-Nnnnnnnnn-Cccccccccc-Ccccccccccc"))
))

(deftest a-test1
  (testing "partlist"
    (test-assert (partlist ["I", "wish", "I", "hadn't", "come"]),
      '[("I", "wish I hadn't come"), ("I wish", "I hadn't come"), ("I wish I", "hadn't come"), ("I wish I hadn't", "come")])
    (test-assert (partlist ["cdIw", "tzIy", "xDu", "rThG"]),
      '[("cdIw", "tzIy xDu rThG"), ("cdIw tzIy", "xDu rThG"), ("cdIw tzIy xDu", "rThG")])
    (test-assert (partlist ["vJQ", "anj", "mQDq", "sOZ"]),
      '[("vJQ", "anj mQDq sOZ"), ("vJQ anj", "mQDq sOZ"), ("vJQ anj mQDq", "sOZ")])
))

(deftest test-create-phone-number
  (are [arr exp] (= (create-phone-number arr) exp)
    [1 2 3 4 5 6 7 8 9 0] "(123) 456-7890"
    [1 1 1 1 1 1 1 1 1 1] "(111) 111-1111"
    [4 7 8 1 5 7 9 9 7 1] "(478) 157-9971"
    [7 8 0 2 2 1 7 5 1 3] "(780) 221-7513"))

(deftest example-tests
  (are [xs answer] (= (find-odd xs) answer)
    [20 1 -1 2 -2 3 3 5 5 1 2 4 20 4 -1 -2 5] 5
    [1 1 2 -2 5 2 4 4 -1 -2 5] -1
    [20 1 1 2 2 3 3 5 5 4 20 4 5] 5
    [10] 10
    [1 1 1 1 1 1 10 1 1 1 1] 10
    [5 4 3 2 1 5 4 3 2 10 10] 1
    ))

(deftest test-prod-fib
  (is (= (product-fib 4895) [55 89 true]))
  (is (= (product-fib 5895) [89 144 false])))

(deftest test-bouncing-balls
  (is (= (bouncing-balls 3 0.66 1.5) 3))
  (is (= (bouncing-balls 30 0.66 1.5) 15)))

(deftest test-play-pass
  (is (= (play-pass "I LOVE YOU!!!" 1) "!!!vPz fWpM J"))
  (is (= (play-pass "MY GRANMA CAME FROM NY ON THE 23RD OF APRIL 2015" 2)
         "4897 NkTrC Hq fT67 GjV Pq aP OqTh gOcE CoPcTi aO")))

(deftest test-hamming
  (doseq [n (range 20) :let [hams [1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36]
                             ham (nth hams n)]]
    (is (= ham (hamming n)))))
