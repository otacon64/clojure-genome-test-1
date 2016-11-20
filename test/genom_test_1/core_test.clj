(ns genom-test-1.core-test
  (:require [genom-test-1.core :refer :all] :reload)
  (:use expectations))


(expect nil? nil)

;; test for get-distance
(expect 0 (get-distance 1 1))
(expect 72 (get-distance 0 3))
(expect 113 (get-distance 0 7))
(expect 113 (get-distance 7 0))

;; series of tests for genomes
(def test-genome-1
  [1 4 2 0 3 8 7 6 5 9])
(def test-genome-2
  [7 9 4 1 5 8 3 2 0 6])

;; tests for calcualte-fitness
;; fitness for test-genome-1:
;; [1 4] + [4 2] + [2 0] + [0 3]
;; + [3 8] + [8 7] + [7 6] + [6 5]
;;  + [5 9] =
;; 54 + 30 + 57 + 72 + 20 + 40 +
;; 80 + 63 + 63 = 416
(def fitness-test-genome-1
  (reduce +
                [(get-distance 1 4)
                (get-distance 4 2)
                (get-distance 2 0)
                (get-distance 0 3)
                (get-distance 3 8)
                (get-distance 8 7)
                (get-distance 7 6)
                (get-distance 6 5)
                (get-distance 5 9)]))

(expect (reduce + [54 30 57 72 20 40 80 63 63])
        fitness-test-genome-1)

(expect fitness-test-genome-1
        (calcualte-fitness test-genome-1))

;; cross-over tests
test-genome-1
test-genome-2
(cross-over test-genome-1 test-genome-2)

;; rand-prob test
(defn calculate-freq [amount genome-1 genome-2]
  (->> (repeatedly amount #(rand-prob
                             genome-1
                             genome-2))
       (frequencies)

       ((fn [m]
         (zipmap (keys m)
                 (map #(float (/ % amount))
                      (vals m)))))))


(defn theoretical-freq [genome-1 genome-2]
  (->> [genome-1 genome-2]
     (map calcualte-fitness)
     ((fn [[fit1 fit2]]
       (let [fit-sum (+ fit1 fit2)]
       (list (float (/ fit2 fit-sum))
             (float (/ fit1 fit-sum))))))
       (zipmap [genome-1 genome-2])))

(defn check-tol [amount tol genome-1 genome-2]
  (->> [(theoretical-freq genome-1 genome-2)
      (calculate-freq amount genome-1 genome-2)]
     (apply merge-with #(- %1 %2))
     (vals)
     (map #(< (Math/abs %) tol))))

(expect '(true true)
        (check-tol 10000 0.008
                   (generate-genome 10)
                   (generate-genome 10)))
