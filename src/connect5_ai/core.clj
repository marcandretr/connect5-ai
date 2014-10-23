(ns connect5-ai.core
  (:require [clojure.core.async :as async]
            clojure.set
            clojure.data.priority-map)
  (:gen-class))

;(defn -main [& args])

(def adjacent-transform
  (for [c1 (range -1 2)
        c2 (range -1 2)]
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
            1 (assoc col true (cons [line-idx col-idx] (col true)))
            2 (assoc col false (cons [line-idx col-idx] (col false)))
            col))
        ret-map
        (map-indexed vector (first charmat)))
      (inc line-idx))))


(defn heuristic
  "Processes the heuristic for a state"
  [grid-w grid-h is-first-player state]
  (rand-int 5))

(defn gen-surrounding-points
  [grid-w grid-h [x y :as point]]
  (filter
    (partial in-bounds? grid-w grid-h)
    (map
      #(map + point %)
      adjacent-transform)))

(defn gen-children-points
  [parent-state grid-w grid-h]

  (let [all-parent-points-union (clojure.set/union (parent-state true) (parent-state false))]

    ;Possible points
    (clojure.set/difference
      (reduce
        (fn [col1 el1] (clojure.set/union col1 (set (gen-surrounding-points grid-w grid-h el1))))
        #{}
        all-parent-points-union)
      all-parent-points-union)))

(defn is-terminal-state
  ""
  [state]
  false)

(defn get-value-for-state
  ""
  [state]
  (rand-int 5))

(defn generate-world-from-position
  ""
  [state position is-first-player]
  (assoc state is-first-player (cons position (state is-first-player))))

(defn generate-successor-states
  ""
  [state grid-width grid-height is-first-player]
  (let [possible-moves (gen-children-points state grid-width grid-height)
        possible-states (set (map #(generate-world-from-position state % is-first-player) possible-moves))]
    (reduce #(assoc %1 %2 (heuristic grid-width grid-height is-first-player %2))
            (clojure.data.priority-map/priority-map)
            possible-states)))


(defn minimax-decision
  [grid-w grid-h is-first-player state timeout]

  (println grid-w grid-h is-first-player state)
  (if (and
        is-first-player
        (empty? (state true)))
    ; Return a vector
    ; If the board is empty and we are player 1
    [(long (/ grid-w 2)) (long (/ grid-h 2))]
    ; Otherwise


    ;(gen-successors grid-w grid-h is-first-player state)
    ))

(defn- negamax-inner
  ""
  [state alpha beta timeout is-first-player max-depth grid-width grid-height]
  (if (is-terminal-state state)
    [(get-value-for-state state) []]
    (if (= max-depth 0)
      [(heuristic grid-width grid-height is-first-player state) []]
        (loop [successor-states (generate-successor-states state grid-width grid-height is-first-player)
               new-alpha alpha
               new-beta beta
               best Double/NEGATIVE_INFINITY
               best-move []]
          (if (empty? successor-states)
            (if (empty? (state true))
              [best [(long (/ grid-width 2)) (long (/ grid-height 2))]]
                [best best-move])
            (let [[successor-state _] (first successor-states)
                  [v-temp _] (negamax-inner successor-state (- new-beta) (- new-alpha) timeout (not is-first-player) (- max-depth 1) grid-width grid-height)
                  v (- v-temp)]
              (if (> v best)
                (let [new-best v
                      _ (println (type successor-state))
                      new-best-move (first (clojure.set/difference (set (successor-state is-first-player)) (set (state is-first-player))))]
                  (if (> new-best new-alpha)
                    (let [ret-alpha new-best]
                      (if (>= ret-alpha new-beta)
                        [new-best new-best-move]
                        (recur (rest successor-states) ret-alpha new-beta new-best new-best-move)))
                    (recur (rest successor-states) new-alpha new-beta new-best new-best-move)))
                (recur (rest successor-states) new-alpha new-beta best best-move))))))))

(defn negamax
  ""
  [is-first-player state timeout grid-width grid-height]
  ; Return if timeout
  (let [[negamax-value move] (negamax-inner state Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY timeout is-first-player 1 grid-width grid-height)]
    (if is-first-player
      move
      (second (negamax-inner state Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY timeout is-first-player 1 grid-width grid-height)))))

(defn -getNextMove
  "Gets the next best move"
  [charmat timeout]
  (let [charmat-seq (lazy-seq charmat)
        state (gen-map-from-charmat charmat-seq {true #{} false #{}} 0)
        is-first-player (even? (+ (count (state true)) (count (state false))))
        grid-width (count (first charmat-seq))
        grid-height (count charmat-seq)]

    ;(minimax-decision grid-w
    ;                  grid-h
    ;                  is-first-player
    ;                  state
    ;                  timeout)
    (into [] (negamax is-first-player
             state
             timeout
             grid-width
             grid-height))))

(defn chantest
  []

  (let [c (async/chan (async/sliding-buffer 1))
        heuristic-c (async/chan (async/sliding-buffer 1))
        timeout-chan (async/timeout 2000)]
    (async/go
      ; Compute layers and feed them to the channel as long as it's opened.
      (loop [x 0]

        (if                                                 ;(not last-chan-op-result)
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

