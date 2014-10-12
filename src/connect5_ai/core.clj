(ns connect5-ai.core
  (:require [clojure.core.async :as async]
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
  [grid-w grid-h is-first-player state timeout]

  (println grid-w grid-h is-first-player state)
  (if (and
        is-first-player
        (empty? (state :1)))
    ; Return a vector
    ; If the board is empty and we are player 1
    [(long (/ grid-w 2)) (long (/ grid-h 2))]
    ; Otherwise


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
                      state
                      timeout)))

(defn chantest
  []

  (let [c (async/chan (async/sliding-buffer 1))
        heuristic-c (async/chan (async/sliding-buffer 1))
        timeout-chan (async/timeout 2000)]
    (async/go
      ; Compute layers and feed them to the channel as long as it's opened.
      (loop [x 0]

        (if ;(not last-chan-op-result)
            (clojure.core.async.impl.protocols/closed? heuristic-c)
          (println "Chan c was closed. Has stopped digging down layers")
          (do
            ;(Thread/sleep (rand-int 300))
            (println "Computed configurations of layer " x)
            (async/>! c x)
            (recur (inc x))))))
    (letfn [(chan-taker [] (async/alts!! [timeout-chan c] :priority true))]
      (async/go

      (loop [[result source] (chan-taker)]
        (if (not result)
          (do
            (println "Chan h was closed. Has stopped minimaxing"))
          (do
            (println "Started Calculating heuristic val of layer " result)
            ;(Thread/sleep (rand-int 600))
            (println "Done    Calculating heuristic val of layer " result)
            (async/>! heuristic-c result)
            (recur (chan-taker)))))))
    ; Let's block on the timeout channel
    (async/<!! timeout-chan)
    ; We must stop this madness, close those chans!
    (async/close! c)
    (async/close! heuristic-c)
    ; We return the last value found in the heuristic-channel (If there is none, we're damned. We be blocking here)
    ; Todo: Use alts! to get a value and not say here forever.
    (async/<!! heuristic-c)))

