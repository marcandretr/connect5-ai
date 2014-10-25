(ns connect5-ai.core
  (:require [clojure.core.async :as async]
            clojure.set
            clojure.data.priority-map
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(def width-heuristic-calculation 5)

(defn gen-heuristic-dictionary
  "Generate the dictionnary of heuristics values"
  [depth]
  (apply merge (for [x '(:min :max :wall :empty)]
                 (if (= depth 0)
                   {x 0}
                   {x (gen-heuristic-dictionary (dec depth))}))))

(defn calc-positive-heuristic
  ""
  [list-to-check player opponent]
  (let [list-to-check (conj list-to-check :empty)]
  (if (and (= (.indexOf (rest list-to-check) opponent) -1) (= (.indexOf (rest list-to-check) :wall) -1))
    (let [count-in-list (reduce #(+ %1 (if (= %2 player) 1 0)) 0 list-to-check)]
      (if (< count-in-list 4)
        count-in-list
        1000))
    0)))

(defn calc-heuristic
  ""
  [list-to-check player opponent]
  (- (calc-positive-heuristic list-to-check player opponent)
     (calc-positive-heuristic (reverse list-to-check) opponent player)))

(defn fill-heuristics-dict
  ""
  []
  (reduce #(assoc-in %1 [%2] (calc-heuristic %2 :max :min))
          (gen-heuristic-dictionary width-heuristic-calculation)
          (apply clojure.math.combinatorics/cartesian-product
                 (for [_ (range width-heuristic-calculation)] [:min :max :wall :empty]))))

(def heuristic-dict (fill-heuristics-dict))


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

(defn default-assoc
  [col mkey element]

  (let [new-val (if (contains? col mkey)
                  (conj (col mkey) element)
                  (conj #{} element))]
    (assoc col mkey new-val)))

(defn line-index-from-point
  [[x y :as point]
   grid-width]
  {:h  x
   :v  y
   :d1 (- x y)
   :d2 (- (dec grid-width) x y)}
  )

(defn gen-lines
  [points grid-width]

  (loop [points points
         h {}
         v {}
         d1 {}
         d2 {}]

    (if (empty? points)
      {:h  h
       :v  v
       :d1 d1
       :d2 d2}
      (let [point (first points)
            [x y] point]
        (let [indexes (line-index-from-point point grid-width)]
          (recur
            (rest points)
            (default-assoc h (indexes :h) x)
            (default-assoc v (indexes :v) y)
            (default-assoc d1 (indexes :d1) x)
            (default-assoc d2 (indexes :d2) x)))))))        ; Todo: if it shits bricks, look here closely.

(defn build-all-lines-from-state
  [state grid-w]
  {:max (gen-lines (state true) grid-w)
   :min (gen-lines (state false) grid-w)})

(defn get-value-for-point
  [{friend :max foe :min}
   [x y :as point]
   [grid-w grid-h :as grid-dimensions]]
  (assert (in-bounds? grid-w grid-h point) "Requested point is not in bounds. This is some serious thing homie.")

  (let [indexes (line-index-from-point point grid-w)]
    (map (fn [[i1 a] [i2 b]]
           (assert (= i1 i2) (str "Map is not mixing the same lines - " i1 " with " i2))
           (let [line-index (indexes i1)
                 la (or (a line-index) #{})
                 lb (or (b line-index) #{})]
             (for [i (range -5 6)

                   :let [realpoint (case i1
                                     :v (+ i y)
                                     (+ i x))]]
               (cond
                 (or
                   (< realpoint 0)
                   (>= realpoint (case i1
                                   :h grid-w
                                   :v grid-h
                                   :d1 (cond
                                         (= line-index 0) (min grid-w grid-h)
                                         (< grid-w grid-h) (if (< line-index 0)
                                                             (if (<= (- grid-w grid-h) line-index)
                                                               grid-w
                                                               (+ grid-h line-index))
                                                             (- grid-w line-index))
                                         (>= grid-w grid-h) (if (< line-index 0)
                                                              (+ grid-h line-index)
                                                              (if (>= (- grid-w grid-h) line-index)
                                                                grid-h
                                                                (- grid-w line-index))))
                                   :d2 (cond
                                         (= line-index 0) (min grid-w grid-h)
                                         (< grid-w grid-h) (if (< line-index 0)
                                                             (if (<= (- grid-w grid-h) line-index)
                                                               grid-w
                                                               (+ grid-h line-index))
                                                             (- grid-w line-index))
                                         (>= grid-w grid-h) (if (< line-index 0)
                                                              (+ grid-h line-index)
                                                              (if (>= (- grid-w grid-h) line-index)
                                                                grid-h
                                                                (- grid-w line-index))))))) :wall
                 (contains? la realpoint) :max
                 (contains? lb realpoint) :min
                 :else :empty))))
           friend foe)))

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

          (if                                               ;(not last-chan-op-result)
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

