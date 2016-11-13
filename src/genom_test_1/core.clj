(ns genom-test-1.core)

(def test-pop (list [1 2 3] [4 5 6] [7 8 9]))

;;                0   1   2   3   4   5   6   7   8   9
(def distances [[ 0   28  57  72  81  85  80  113 89  80  ] ; 0
                [ 28  0   28  45  54  57  63  85  63  63  ] ; 1
                [ 57  28  0   20  30  28  57  57  40  57  ] ; 2
                [ 72  45  20  0   10  20  72  45  20  45  ] ; 3
                [ 81  54  30  10  0   22  81  41  10  41  ] ; 4
                [ 85  57  28  20  22  0   63  28  28  63  ] ; 5
                [ 80  63  57  72  81  63  0   80  89  113 ] ; 6
                [ 113 85  57  45  41  28  80  0   40  80  ] ; 7
                [ 89  63  40  20  10  28  89  40  0   40  ] ; 8
                [ 80  63  57  45  41  63  113 80  40  0   ]]);9

(defn get-distance [a b]
  "returns distance between city a and b"
  (get-in distances [a b]))

(defn calcualte-fitness [genome]
  "reutrns the total distance travelled for a given genome"
  (reduce + (map #(apply get-distance %) (partition 2 1 genome))))

(defn generate-genome [genome-size]
  (shuffle (range genome-size)))

(defn generate-population [genome-size population-size]
  (repeatedly population-size #(generate-genome genome-size)))

(defn random-unique-numbers [n]
  (distinct (repeatedly #(rand-int n))))

(take 2 (random-unique-numbers 5))

(def genome-pairs (apply map vector (repeatedly 2 #(generate-genome 5))))
genome-pairs

(def genome-1 (first (apply map vector genome-pairs)))
genome-1
(def genome-2 (second (apply map vector genome-pairs)))
genome-2

(defn cross-over [genome-1 genome-2]
  (apply map vector (let [genome-pairs (map vector genome-1 genome-2)]
    (map #(if (some (partial = (reverse %)) genome-pairs)
          (vec (reverse %))
          %) genome-pairs))))

(defn breed-population [population breedings-amount]
  ;;pick two random creatures and breed them
  (mapcat identity (repeatedly breedings-amount (fn [] apply cross-over (take 2
        (map #(nth population %)
             (random-unique-numbers (count population))))))))
             ;; (distinct (repeatedly #(rand-int (count population))))))))))

(defn mutate [mutations-amount genome]
  (apply (partial assoc genome)
     (mapcat (fn [[gen1 gen2]]
         [gen1 (genome gen2) gen2 (genome gen1)])
  (take mutations-amount
        (partition 2
                   (distinct (repeatedly
                               #(rand-int (count genome)))))))))

(def test-genome-1 (generate-genome 9))
test-genome-1
(def mutated-genome-test-1 (mutate 1 test-genome-1))
mutated-genome-test-1
(calcualte-fitness test-genome-1)
(calcualte-fitness mutated-genome-test-1)

([1 2 3] 1)

(defn tournament-selection [population survivours]
  ;;pick two and check who is better according to fitness. Calculate probability
  (map #(vector % (calcualte-fitness (nth population %))) (take 2 (distinct (repeatedly #(rand-int (count population))))))
  )

(defn tourn-second-try [population survivours]
  (take 2 (distinct (repeatedly #(rand-int (count population)))))
  )
(tourn-second-try test-pop 1)


test-pop
(map #(calcualte-fitness %) test-pop)
(tournament-selection test-pop 1)

(def test-genome-1 (generate-genome 5))
(def test-genome-2 (generate-genome 5))
test-genome-1
test-genome-2

(calcualte-fitness test-genome-1)
(calcualte-fitness test-genome-2)

(defn rand-prob [fitness-1 fitness-2]
  (let [fitness-sum (+ fitness-1 fitness-2)]
    (if (< (rand-int fitness-sum) fitness-1)
      fitness-1
      fitness-2)))

(frequencies (repeatedly 10000 #(rand-prob 3 7)))



