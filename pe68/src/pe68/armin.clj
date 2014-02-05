(ns osmos.core
  (:gen-class))

;numbers from file get read as chars, convert them to numbers
(defn to-numbers [line]
  (map read-string (re-seq #"[\d.]+" line)))

(defrecord state [armin motes cost])

(def small (.split #"\n" (slurp "/u/epsystem/jbeaumon/code/clojure/osmos/src/osmos/small-osmos.in")))
(def large (.split #"\n" (slurp "/u/epsystem/jbeaumon/code/clojure/osmos/src/osmos/large-osmos.in")))
(def input small)
(def nTestCases (first input))
(def tests (map to-numbers (rest input)))

;find out how many motes we must add for the armin to make the next jump
(defn count-adds [armin mote n-adds]
  (if (> armin mote)
    n-adds
    (recur (+ armin (dec armin)) mote (inc n-adds))))

(defn add-mote [state]
  (let [armin (:armin state) motes (:motes state) cost (:cost state)]
  (state. armin (conj motes (dec armin)) (inc cost))))

(defn del-mote [state]
  (let [armin (:armin state) motes (:motes state) cost (:cost state)]
  (state. armin (butlast motes)(inc cost))))

;consume the smallest motes until we reach an impasse
(defn eat [state]
  (let [armin (:armin state) motes (:motes state) cost (:cost state)]
  ;armin cannot consume next mote.
    (if (or (empty? motes) (> (first motes) armin))
      state
    (recur (state. (+ armin (first motes)) (rest motes) cost)))))

;eat and add motes until we find the best score
(defn get-cost [state case-no]
  (let [n-state (eat state)
        armin (:armin n-state)
        motes (:motes n-state)
        cost (:cost n-state)
        worst (count motes)]
  (println n-state)  
  (cond 
    (zero? worst)
      (println (str "Case #" case-no": " cost))
    (= worst 1)
      (println (str "Case #" case-no": " (inc cost)))
    (<= worst (count-adds armin (first motes) 0))
      (println (str "Case #" case-no": " (+ worst cost)))
    :else (get-cost (add-mote n-state) case-no))))

(defn do-test [t case-no]
  (if (empty? t)
    nil
  (let [arm (ffirst t)
        mts (second t)
        st (state. arm mts 0)
        c (get-cost st case-no)]
    (recur (nthrest t 2) (inc case-no)))))

;(map #(let [t (take 2 %) arm (ffirst t) motes (second t) s (state. a motes 0)] (get-cost s 1)) tests)

(defn -main
  [& args]
  (alter-var-root #'*read-eval* (constantly false))
  (do-test tests 1)
)
