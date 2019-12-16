(ns codewars.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello World!"))

(defn -main
  "main"
  [& args]
  (println "hello codewars"))

(defn power [base exponent]
  (if (zero? exponent)
    1
    (loop [cnt exponent
           acc 1]
      (if (zero? cnt)
        acc
        (recur (dec cnt) (*' acc base))))))

(defn row-sum-odd-numbers [row-num]
  ;; your code here
  (power row-num 3))

(defn thirt [n]
  ;; your code here
  (loop [nr n]
    (let [tmp (->> nr
                   pr-str
                   (map #(- (int %) 48))
                   reverse
                   (map * (cycle [1 10 9 12 3 4]))
                   (reduce +))]
      (if (= tmp nr)
        tmp
        (recur tmp)))))

(defn stock-list [list-of-books list-of-cat]
  ;; your code
  (if (every? seq [list-of-books list-of-cat])
    (let [m (->> list-of-cat
                 (mapv #(vector % 0))
                 flatten
                 (apply hash-map))
          rmap (reduce
                (fn [acc item]
                  (let [cat (->> item first str)
                        num (->> item (re-find #"\d+") Integer/parseInt)]
                    (if (contains? acc cat)
                      (update acc cat + num)
                      acc)))
                m
                list-of-books)]
      (map #(vector % (rmap %)) list-of-cat))
    []))

(defn evaporator [content, evap_per_day, threshold]
  (count
   (take-while
    #(> % threshold)
    (iterate #(* % (/ (- 100 evap_per_day) 100)) 100))))

(defn accum [s]
  (clojure.string/join "-"
                       (map-indexed
                        (fn [idx elm]
                          (clojure.string/capitalize
                           (apply str
                                  (repeat (inc idx) elm)))) s)))

(defn partlist [arr]
  (mapv
   #(vector (clojure.string/join " " (take % arr))
            (clojure.string/join " " (drop % arr)))
   (range 1 (count arr))))

(defn create-phone-number [nums]
  (let [[f3 rest] (split-at 3 nums)
        [f36 l4] (split-at 3 rest)]
    (apply str "(" (apply str f3) ") " (apply str f36) "-" l4)))

(defn find-odd [xs]
  (ffirst (filter (fn [[k v]] (odd? v))
                 (frequencies xs))))

(defn find-odd [xs] (reduce bit-xor xs))


