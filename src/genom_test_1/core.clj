(ns genom-test-1.core
  (:require [clojure.pprint :refer [pprint print-table]]))
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


(defn
  ^{:doc "breeds the population"}
  breed-population [population breedings-amount]
  ;;pick two random creatures and breed them
  (mapcat identity (repeatedly breedings-amount (fn [] apply cross-over (take 2
        (map #(nth population %)
             (random-unique-numbers (count population))))))))
             ;; (distinct (repeatedly #(rand-int (count population))))))))))



;; reduce works like this:
;; reduce f val [x1 x2 x3]
;; (f (f (f val x1) x2) x3)

;; if I have [10 1 0.5]
;; I want to have [10 11 11.5]
;; 0 + 10 = 10
;; 10 + 1 = 11
;; 11 + 0.5 = 11.5

;; val + x1 = res1
;; res1 + x2 = res2
;; res2 + x3 = res3

;; res should be a vector [10 11 11.5]
;; res1 [10]
;; res2 [10 11]
;; res3 [10 11 11.5]

;; conj add a value to the end of vector
;; (conj coll x)
;; (conj res (+ last-val-res xi))

;; f should be...
;;

(doc generate-population)

(def test-pop (generate-population 5 5))
test-pop

;; generate fitness of each genome
(def fitnesses (mapv calcualte-fitness test-pop))
fitnesses

;; get processed vector
(def rulete-position (reduce proc-pop [] fitnesses))
rulete-position

;; zip rulete-position to population
(def pop-map (map
       #(zipmap [:val :rulete :fitness] [%1 %2 %3])
  test-pop rulete-position fitnesses))
pop-map

(print-table pop-map)

;; get genome by rulete position....
;; if random value is lower or equal to rulete then we have our guy

(->> pop-map last :rulete)
()

(defn return-random-rulete [population-map]
  (let [rulete-value (rand-int (-> population-map last :rulete inc))]
    (first (filter
             #(<= rulete-value (% :rulete))
             population-map))))

(return-random-rulete pop-map)


(apply max-key :rulete pop-map)

(defn proc-pop [acc x]
  (conj acc
        ((fnil #(+ x %) 0) (peek acc))))

(reduce proc-pop [] [1 2 3])

(proc-pop [] 1)

(let [x 1]
  (fnil #(+ x) 0))



(map #(hash-map :val %) test-pop)

(let [val-sum (reduce + test-pop)]
  val-sum)













