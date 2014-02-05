;5-Gon         A  
;               \  
;                B    C
;              /  \  /
;             J     D
;            / \   / 
;           I   H- F - E 
;                \ 
;                 G
;
;
;10 nodes arranged in three node lines to form a pentagon. 
;Each node assigned a number 1-10. 
;Each Line total is equal to the rest.
;
;Problem- 
;Find the Largest 16 digit number that completes a fivegon after rotating around the smallest outer spoke
;
;ABD
;CDF
;EFH
;GHJ
;IJB

(ns pe68.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defrecord fivegon [a b c d e f g h i j])

(defn str-to-num [s]
  (Integer. (re-find  #"\d+" s )))

(defn new-fg [fg]
  (apply #(fivegon. %1 %2 %3 %4 %5 %6 %7 %8 %9 %10) fg))

;show lines and totals
(defn print-lines [fg]
  (let [l1 (println "abd "  (+ (:a fg) (:b fg) (:d fg)))
        l2 (println "cdf "  (+ (:c fg) (:d fg) (:f fg)))
        l3 (println "efh "  (+ (:e fg) (:f fg) (:h fg)))
        l4 (println "ghj "  (+ (:g fg) (:h fg) (:j fg)))
        l5 (println "ijb "  (+ (:i fg) (:j fg) (:b fg)))
        line (println "-----------")]))


;Are all nodes are unique and do the lines have equal sums?
(defn lines-equal? [fg]
  (= (+ (:a fg) (:b fg) (:d fg))
     (+ (:c fg) (:d fg) (:f fg))
     (+ (:e fg) (:f fg) (:h fg))
     (+ (:g fg) (:h fg) (:j fg))
     (+ (:i fg) (:j fg) (:b fg))))

;squish digits into a single number
(defn convert-num [x y z]
  (str-to-num (str x y z)))

;find the index of the line to pivot. Use only the outer number
(defn find-pivot [fg]
  (let [l (list (:a fg) (:c fg) (:e fg) (:g fg) (:i fg))]
    (.indexOf l (apply min l))))

;convert node digits to composite numbers for entire fivegon
(defn fg-to-list [fg]
  (let [abd (convert-num (:a fg) (:b fg) (:d fg))
        cdf (convert-num (:c fg) (:d fg) (:f fg))
        efh (convert-num (:e fg) (:f fg) (:h fg))
        ghj (convert-num (:g fg) (:h fg) (:j fg))
        ijb (convert-num (:i fg) (:j fg) (:b fg))]
    (list abd cdf efh ghj ijb)))

;Perform all actions on a fivegon. Convert answer to string.
(defn process-fivegon [fg]
  (let [fl (fg-to-list fg)
        pivot (find-pivot fg)
        n (count fl)
        rotated (flatten (list (nthrest fl pivot) (drop-last (- n pivot) fl)))]
    (str (first rotated) (second rotated) (nth rotated 2) (nth rotated 3) (nth rotated 4))))

(def a (map inc (range 10)))
(def c [6 5 10 3 9 1 8 4 7 2])
(def fg (new-fg a))
(def jb (new-fg c))
(def results (filter lines-equal? (map new-fg (combo/permutations a))))
(def res-strings (map process-fivegon results))
(def candidates (filter #(= 16 (count %)) res-strings))

; Largest 16 digit number that completes a fivegon after rotating around the smallest outer spoke
;ANSWER=6531031914842725 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
