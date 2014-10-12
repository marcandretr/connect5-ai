(ns connect5-ai.core
  (:require [clojure.core.async :as casync]
            clojure.set)
  (:gen-class))

;(defn -main [& args])

(def adjacent-transform
  (for [c1 (range -2 3)
        c2 (range -2 3)]
    [c1 c2]))

(defn in-bounds?
  [grid-w grid-h [x y :as point]]
  (not (or
         (< x 0)
         (< y 0)
         (>= x grid-w)
         (>= y grid-h))))

(defn gen-map-from-charmat
  "Generates map representation of blabla"
  [charmat ret-map line-idx]
  (if (empty? charmat)
    ret-map
    (recur
      (rest charmat)
      (reduce
        (fn [col [col-idx charac]]
          (case charac
            1 (assoc col :1 (cons [line-idx col-idx] (:1 col)))
            2 (assoc col :2 (cons [line-idx col-idx] (:2 col)))
            col))
        ret-map
        (map-indexed vector (first charmat)))
      (inc line-idx))))


(defn heuristic
  "Processes the heuristic for a state"
  [grid-w grid-h is-first-player state])

(defn gen-surrounding-points
  [grid-w grid-h [x y :as point]]
  (filter
    (partial in-bounds? grid-w grid-h)
    (map
      #(map + point %)
      adjacent-transform)))

(defn gen-children-points
  [parent-state grid-w grid-h]

  (let [all-parent-points-union (clojure.set/union (parent-state :1) (parent-state :2))]

    ;Possible points
    (clojure.set/difference
      (reduce
        (fn [col1 el1] (clojure.set/union col1 (set (gen-surrounding-points grid-w grid-h el1))))
        #{}
        all-parent-points-union)
      all-parent-points-union)))

(defn gen-children-states
  "Swiggity Swag"
  [grid-w grid-h is-first-player state]

  (let [player-to-gen (if (> (count (state :1)) (count (state :2))) :2 :1)
        children-positions (gen-children-points state grid-w grid-h)
        ]
    ; Todo: Started from the bottom now we here
    ))

(defn minimax-decision
  [grid-w grid-h is-first-player state]

  (println grid-w grid-h is-first-player state)
  (if (and
        is-first-player
        (empty? (state :1)))
    [(long (/ grid-w 2)) (long (/ grid-h 2))]

    [2 3]
    ;(gen-successors grid-w grid-h is-first-player state)
    ))

(defn -getNextMove
  "Gets the next best move"
  [charmat timeout]
  (let [charmat-seq (lazy-seq charmat)
        state (gen-map-from-charmat charmat-seq {:1 #{} :2 #{}} 0)
        is-first-player (even? (+ (count (state :1)) (count (state :2))))]

    (minimax-decision (count (first charmat-seq))
                      (count charmat-seq)
                      is-first-player
                      state)))

