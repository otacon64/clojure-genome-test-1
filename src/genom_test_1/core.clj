(ns genom-test-1.core)
(use 'clojure.repl)

;;
;; matrix with distances for cities
;;

(def
  ^{:doc "distances matrix for the cities"}
  ;;           0   1   2   3   4   5   6   7   8   9
  distances [[ 0   28  57  72  81  85  80  113 89  80  ] ; 0
             [ 28  0   28  45  54  57  63  85  63  63  ] ; 1
             [ 57  28  0   20  30  28  57  57  40  57  ] ; 2
             [ 72  45  20  0   10  20  72  45  20  45  ] ; 3
             [ 81  54  30  10  0   22  81  41  10  41  ] ; 4
             [ 85  57  28  20  22  0   63  28  28  63  ] ; 5
             [ 80  63  57  72  81  63  0   80  89  113 ] ; 6
             [ 113 85  57  45  41  28  80  0   40  80  ] ; 7
             [ 89  63  40  20  10  28  89  40  0   40  ] ; 8
             [ 80  63  57  45  41  63  113 80  40  0   ]]);9

(defn
  ^{:doc "returns distance between city a and b"}
  get-distance [a b]
  (get-in distances [a b]))


(defn
  ^{:doc "reutrns the total distance travelled for a given genome"}
  calcualte-fitness [genome]
  (reduce + (map #(apply get-distance %) (partition 2 1 genome))))

(defn
  ^{:doc "returns randomly generated sequence of unique numbers"}
  generate-genome [genome-size]
  (shuffle (range genome-size)))

(defn
  ^{:doc "returns random population of size population-size"}
  generate-population [genome-size population-size]
  (repeatedly population-size #(generate-genome genome-size)))

(defn
  ^{:doc "returns lazy-seq of unique random integers with max value of n"}
  random-unique-numbers [n]
  (distinct (repeatedly #(rand-int n))))

(defn
  ^{:doc "performs cyclic cross-over of genome-1 and genome-2"}
  cross-over [genome-1 genome-2]
  (apply map vector (let [genome-pairs (map vector genome-1 genome-2)]
    (map #(if (some (partial = (reverse %)) genome-pairs)
          (vec (reverse %))
          %) genome-pairs))))


(defn
  ^{:doc "breeds the population"}
  breed-population [population breedings-amount]
  ;;pick two random creatures and breed them
  (mapcat identity (repeatedly breedings-amount (fn [] apply cross-over (take 2
        (map #(nth population %)
             (random-unique-numbers (count population))))))))
             ;; (distinct (repeatedly #(rand-int (count population))))))))))



(defn
  ^{:doc "mutates genome with mutations-amount random mutations"}
  mutate [mutations-amount genome]
  (apply (partial assoc genome)
     (mapcat (fn [[gen1 gen2]]
         [gen1 (genome gen2) gen2 (genome gen1)])
  (take mutations-amount
        (partition 2
                   (distinct (repeatedly
                               #(rand-int (count genome)))))))))



(defn
  ^{:doc "returns winning genome based on random chance with
  chances inversely  to their fitnesses"}
  rand-prob [genome-1 genome-2]
  (let [ fitness-1 (calcualte-fitness genome-1)
         fitness-2 (calcualte-fitness genome-2)
         fitness-sum (+ fitness-1 fitness-2)]
    (if (>= (rand-int fitness-sum) fitness-1)
      genome-1
      genome-2)))







