(ns connect5-ai.core
  (:require [clojure.core.async :as async]
            clojure.set
            clojure.data.priority-map
            [clojure.math.combinatorics :as combo])
  (:gen-class))

(def width-heuristic-calculation 5)
(def directors {:h  (map #(do [%1 0]) (range -5 6))
                :v  (map #(do [0 %1]) (range -5 6))
                :d1 (map #(do [%1 %1]) (range -5 6))
                :d2 (map #(do [%1 (- %1)]) (range -5 6))})

(defn gen-heuristic-dictionary
  "Generate the dictionnary of heuristics values"
  [depth]
  (apply merge (for [x '(:min :max :wall :empty)]           ;
                 (if (= depth 0)
                   {x 0}
                   {x (gen-heuristic-dictionary (dec depth))}))))

(defn calc-positive-heuristic
  ""
  [list-to-check player opponent]

  (if
    (and
      (= (.indexOf list-to-check opponent) -1)
      (= (.indexOf list-to-check :wall) -1))
    (let [count-in-list (reduce #(+ %1 (if (= %2 player) 1 0)) 0 list-to-check)]
      (if (< count-in-list 3)
        (Math/pow 2 count-in-list)
        1000))
    0))

(defn calc-heuristic
  ""
  [list-to-check player opponent]

  (+
    (calc-positive-heuristic list-to-check player opponent))


  (- (calc-positive-heuristic list-to-check player opponent)
     (calc-positive-heuristic (reverse list-to-check) opponent player)))

(defn fill-heuristics-dict
  ""
  []
  (reduce #(assoc-in %1 %2 (calc-heuristic %2 :max :min))
          (gen-heuristic-dictionary width-heuristic-calculation)
          (apply clojure.math.combinatorics/cartesian-product
                 (for [_ (range width-heuristic-calculation)] [:min :max :wall :empty]))))

(defn generate-valid-keys
  ""
  [depth]
  (let [half-keys (filter
                    #(loop [keys %
                            wall-done false]
                      (if (empty? keys)
                        true
                        (if (and (not wall-done) (not (= (first keys) :wall)))
                          (recur (rest keys) true)
                          (if (and wall-done (= (first keys) :wall))
                            false
                            (recur (rest keys) wall-done)))))
                    (combo/selections [:min :max :empty :wall] (/ depth 2)))]
    (for [k1 half-keys k2 half-keys] (concat k1 (reverse k2)))))

(def heuristic-dict (fill-heuristics-dict))

(defn assoc-in!
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  {:added  "1.0"
   :static true}
  [m [k & ks] v]
  (if ks
    (assoc! m k (assoc-in! (get m k) ks v))
    (assoc! m k v)))

(defn compute-path-value
  [path]
  (loop [left-part (reverse (take 5 path))
         right-part (take-last 5 path)

         ; States
         prev-left-is-dead false
         prev-right-is-dead false
         prev-left-direct-streak-is-dead false
         prev-right-direct-streak-is-dead false

         prev-left-streak-type (first left-part)
         prev-left-streak-count 0
         prev-left-partial-streak-count 0
         prev-left-direct-streak 0

         prev-right-streak-type (first right-part)
         prev-right-streak-count 0
         prev-right-partial-streak-count 0
         prev-right-direct-streak 0]
    (if (or
          ;(prn prev-left-direct-streak prev-left-streak-type prev-right-streak-type prev-right-direct-streak)
          ;(prn prev-left-is-dead prev-right-is-dead)
          (empty? left-part)
          (and prev-left-is-dead prev-right-is-dead))
      (if (and (not= prev-left-streak-type prev-right-streak-type)
               (not= prev-left-streak-type :wall)
               (not= prev-right-streak-type :wall))
        ; Types differents
        (let [opl (if (= :min prev-left-streak-type) + +)
              opr (if (= :min prev-right-streak-type) + +)]
          (+
            (opl
              (cond
                ;(prn prev-left-partial-streak-count prev-right-partial-streak-count) -3332
                (or (= :wall prev-left-streak-type) (= :empty prev-left-streak-type)) 0

                (= 4 prev-left-direct-streak) Double/POSITIVE_INFINITY
                (and
                  (= 3 prev-left-direct-streak)
                  (< 0 prev-left-partial-streak-count)
                  (< 0 prev-right-partial-streak-count)
                  (= 0 prev-right-direct-streak)) 50000

                ;(and
                ;  (= 3 prev-left-direct-streak)
                ;  (< 0 prev-left-partial-streak-count)
                ;  (or (= 0 prev-right-direct-streak)
                ;      (and (< 0 prev-right-direct-streak)
                ;           (not= prev-left-streak-type prev-right-streak-type)))) 50000

                (<= 4 (+ prev-left-streak-count prev-left-partial-streak-count)) (+ prev-left-direct-streak prev-left-streak-count)
                :else 0))

            (opr
              (cond
                ;(prn prev-left-partial-streak-count prev-right-partial-streak-count) -333
                (or (= :wall prev-right-streak-type) (= :empty prev-right-streak-type)) 0
                (= 4 prev-right-direct-streak) Double/POSITIVE_INFINITY
                (and
                  (= 3 prev-right-direct-streak)
                  (< 0 prev-right-partial-streak-count)
                  (< 0 prev-left-partial-streak-count)
                  (= 0 prev-left-direct-streak)) 50000

                ;(and
                ;  (= 3 prev-right-direct-streak)
                ;  (< 0 prev-right-partial-streak-count)
                ;  (or (= 0 prev-left-direct-streak)
                ;      (and (< 0 prev-left-direct-streak)
                ;           (not= prev-left-streak-type prev-right-streak-type)))) 50000 ;

                (<= 4 (+ prev-right-streak-count prev-right-partial-streak-count)) (+ prev-right-direct-streak prev-right-streak-count)
                :else 0))
            ))
        ; Types identiques ou mur
        (let [op (if (or (= :min prev-left-streak-type) (= :min prev-right-streak-type)) + +)]
          (op (cond
                (= :wall prev-left-streak-type prev-right-streak-type) 0 ;
                (= 4 (+ prev-left-direct-streak prev-right-direct-streak)) (if (or
                                                                                 (= prev-left-streak-type :max)
                                                                                 (= prev-left-streak-type :min)
                                                                                 (= prev-right-streak-type :max)
                                                                                 (= prev-right-streak-type :min))
                                                                             Double/POSITIVE_INFINITY
                                                                             0) ;
                (and (= 3 (+ prev-left-direct-streak prev-right-direct-streak))
                     (>= prev-right-partial-streak-count 1)
                     (>= prev-left-partial-streak-count 1)  ;
                     )
                50000
                (<= 5 (+ prev-left-direct-streak prev-right-direct-streak)) 0

                :else (+ prev-left-direct-streak
                         prev-left-streak-count
                         prev-right-direct-streak
                         prev-right-streak-count)))))

      (let [left (first left-part)
            right (first right-part)
            left-is-dead (or prev-left-is-dead (= left :wall) (not (or (= left :empty) (= prev-left-streak-type :empty) (= left prev-left-streak-type))))
            right-is-dead (or prev-right-is-dead (= right :wall) (not (or (= right :empty) (= prev-right-streak-type :empty) (= right prev-right-streak-type))))

            left-direct-streak-is-dead (or prev-left-direct-streak-is-dead
                                           (= left :empty)
                                           (= left :wall)
                                           (not= left prev-left-streak-type))
            right-direct-streak-is-dead (or prev-right-direct-streak-is-dead
                                            (= right :empty)
                                            (= right :wall)
                                            (not= right prev-right-streak-type))


            left-streak-type (if (= :empty prev-left-streak-type) left prev-left-streak-type)
            right-streak-type (if (= :empty prev-right-streak-type) right prev-right-streak-type)

            left-streak-count (cond (or left-is-dead (= left :empty)) prev-left-streak-count
                                    :else (inc prev-left-streak-count))
            right-streak-count (cond (or right-is-dead (= right :empty)) prev-right-streak-count
                                     :else (inc prev-right-streak-count))

            left-partial-streak-count (cond (or left-is-dead (not= left :empty)) prev-left-partial-streak-count
                                            :else (inc prev-left-partial-streak-count))

            right-partial-streak-count (cond (or right-is-dead (not= right :empty)) prev-right-partial-streak-count
                                             :else (inc prev-right-partial-streak-count)) ;

            left-direct-streak (if left-direct-streak-is-dead prev-left-direct-streak (inc prev-left-direct-streak))
            right-direct-streak (if right-direct-streak-is-dead prev-right-direct-streak (inc prev-right-direct-streak))

            ]
        (recur
          (rest left-part)
          (rest right-part)

          ; States
          left-is-dead
          right-is-dead

          left-direct-streak-is-dead
          right-direct-streak-is-dead

          left-streak-type
          left-streak-count
          left-partial-streak-count
          left-direct-streak

          right-streak-type
          right-streak-count
          right-partial-streak-count
          right-direct-streak)))))

(defn build-heuristic-dict-with-awesomeness
  []
  (persistent!
    (reduce
      (fn [col path]
        (assoc-in! col path (compute-path-value path)))
      (transient {})
      []))
  )

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
            1 (assoc col true (conj (col true) [col-idx line-idx]))
            2 (assoc col false (conj (col false) [col-idx line-idx]))
            col))
        ret-map
        (map-indexed vector (first charmat)))
      (inc line-idx))))


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
        (fn [col1 el1]
          (clojure.set/union
            col1
            (set
              (gen-surrounding-points grid-w grid-h el1))))
        #{}
        all-parent-points-union)
      all-parent-points-union)))

(defn default-assoc
  [col mkey element]

  (let [new-val (if (contains? col mkey)
                  (conj (col mkey) element)
                  (conj #{} element))]
    (assoc col mkey new-val)))

(defn line-index-from-point
  [[x y :as point]
   grid-width]
  {:v  x
   :h  y
   :d1 (- x y)
   :d2 (- (dec grid-width) x y)}
  )

(defn gen-lines
  [points grid-w grid-h]

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
        (let [indexes (line-index-from-point point grid-w)]
          (recur
            (rest points)
            (default-assoc h (indexes :h) x)
            (default-assoc v (indexes :v) y)
            (default-assoc d1 (indexes :d1) x)
            (default-assoc d2 (indexes :d2) x)))))))        ; Todo: if it shits bricks, look here closely.

(defn build-all-lines-from-state
  [state grid-w grid-h is-first-player]

  {:max (gen-lines (state is-first-player) grid-w grid-h)
   :min (gen-lines (state (not is-first-player)) grid-w grid-h)}

  ;{:max (gen-lines (state true) grid-w grid-h)
  ; :min (gen-lines (state false) grid-w grid-h)}

  ;
  )

(defn get-value-for-point
  [{max-lines :max min-lines :min}
   [x y :as point]
   [grid-w grid-h :as grid-dimensions]]
  (assert (in-bounds? grid-w grid-h point) "Requested point is not in bounds. This is some serious thing homie.")

  (let [indexes (line-index-from-point point grid-w)
        paths (map (fn [[linetype-max max-lines-for-type] [linetype-min min-lines-for-type]]
                     (assert (= linetype-max linetype-min) (str "Map is not mixing the same lines - " linetype-max " with " linetype-min))
                     (let [line-index (indexes linetype-max)
                           max-line (or (max-lines-for-type line-index) #{})
                           min-line (or (min-lines-for-type line-index) #{})
                           ]

                       (map
                         (fn [[nx ny :as new-point]]
                           (let [index-in-line (if (= linetype-max :v) ny nx)]
                             (cond
                               (not (in-bounds? grid-w grid-h new-point)) :wall
                               (contains? max-line index-in-line) :max
                               (contains? min-line index-in-line) :min
                               :else :empty)))
                         (map (partial map + point) (directors linetype-max)))))
                   max-lines min-lines)]

    (reduce + (map (fn [path]
                     (assert (= :empty (nth path 5)))
                     (compute-path-value path))
                   paths))))

(defn get-value-for-state
  ""
  [grid-w grid-h state]

  (get-value-for-point (state :lines)
                       (state :last-move)
                       [grid-w grid-h]))

(defn generate-world-from-position
  ""
  [grid-w grid-h state position is-first-player]
  (let [state (assoc state :last-move position)
        state (assoc state :lines (build-all-lines-from-state state grid-w grid-h is-first-player))
        tstate (transient state)]

    (assoc! tstate is-first-player (conj (state is-first-player) position))
    (assoc! tstate :heuristic-result (- (get-value-for-state grid-w ;TODO remove the minus cause this was a test (except if heuristic is good like that... then leave it alone)
                                                             grid-h
                                                             state)))
    (persistent! tstate)))

(defn generate-successor-states
  ""
  [state grid-width grid-height is-first-player]
  (let [possible-moves (gen-children-points state grid-width grid-height)
        possible-states (set
                          (map
                            #(generate-world-from-position grid-width grid-height state % is-first-player)
                            possible-moves))]
    (reduce
      (fn [col item]
        (assoc col item (item :heuristic-result)))
      (clojure.data.priority-map/priority-map-by <)
      possible-states)))

(defn is-terminal-state?
  ""
  [state]

  (or (= Double/POSITIVE_INFINITY (state :heuristic-result))
      (= Double/NEGATIVE_INFINITY (state :heuristic-result))))

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

(defn- is-it-a-win?
  ""
  [grid-w grid-h state]
  (let [point (state :last-move)
        max-lines ((state :lines) :max)
        min-lines ((state :lines) :min)
        indexes (line-index-from-point point grid-w)
        paths (map (fn [[linetype-max max-lines-for-type] [linetype-min min-lines-for-type]]
                     (assert (= linetype-max linetype-min) (str "Map is not mixing the same lines - " linetype-max " with " linetype-min))
                     (let [line-index (indexes linetype-max)
                           max-line (or (max-lines-for-type line-index) #{})
                           min-line (or (min-lines-for-type line-index) #{})
                           ]

                       (map
                         (fn [[nx ny :as new-point]]
                           (let [index-in-line (if (= linetype-max :v) ny nx)]
                             (cond
                               (not (in-bounds? grid-w grid-h new-point)) :wall
                               (contains? max-line index-in-line) :max
                               (contains? min-line index-in-line) :min
                               :else :empty)))
                         (map (partial map + point) (directors linetype-max)))))
                   max-lines min-lines)]
    (boolean (some true? (map (fn [path]
                       (let [parts (partition 5 1 path)]
                         (reduce #(or %1
                                     (apply (partial = :max) %2))
                                 false
                                 parts)
                         )) paths)))
    ))

(defn- negamax-inner
  ""
  [state alpha beta timeout is-first-player max-depth grid-width grid-height]

  ;(prn is-first-player max-depth)
  ;(if (= max-depth 3) (clojure.pprint/pprint state))

  (if (empty? (state true))
    {:best-move [(long (/ grid-width 2)) (long (/ grid-height 2))]}

    (if (is-terminal-state? state)
      (let [is-a-win (is-it-a-win? grid-width grid-height state)
            _ (prn is-a-win (state :last-move))]
        (if is-a-win
          (let [who-wins? (if is-a-win
                            (not is-first-player)
                            nil)]
            {:best-move  (state :last-move)
             :best-value (state :heuristic-result)
             :who-win?   who-wins?})




          (if (= max-depth 0)
            {:best-move  (state :last-move)
             :best-value (state :heuristic-result)
             :who-win?   nil}

            (loop
              [sucessors (generate-successor-states state grid-width grid-height (not is-first-player)) ; Flip is first player.
               best {:best-value Double/NEGATIVE_INFINITY
                     :best-move  []
                     :who-win    nil}
               alpha alpha]
              ;(if (and (= 3 max-depth)
              ;         (not-empty sucessors))
              ;  (clojure.pprint/pprint
              ;    [((first (first sucessors)) :last-move)
              ;     ((first (first sucessors)) :heuristic-result)]))
              (if (empty? sucessors)
                best
                (let [{child-best-move :best-move tmp-child-best-value :best-value who-win? :who-win?} (negamax-inner (first (first sucessors))
                                                                                                                      (- beta)
                                                                                                                      (- alpha)
                                                                                                                      timeout
                                                                                                                      (not is-first-player)
                                                                                                                      (dec max-depth)
                                                                                                                      grid-width
                                                                                                                      grid-height)
                      child-best-value (- tmp-child-best-value)]
                  (if (and (= child-best-value (best :best-value) Double/POSITIVE_INFINITY)
                           (not= who-win? nil))
                    {:best-value child-best-value :best-move child-best-move :who-win? who-win?}
                    (if (and (= (best :best-value) Double/POSITIVE_INFINITY)
                             (< child-best-value (best :best-value)))
                      best
                      (if (> child-best-value (best :best-value))
                        (let [best {:best-value child-best-value :best-move child-best-move :who-win? who-win?}]
                          (if (> (best :best-value) alpha)
                            (let [alpha (best :best-value)]
                              (if (and (>= alpha beta)
                                       (or (not= child-best-value Double/POSITIVE_INFINITY)
                                           (not= who-win? nil)))
                                best
                                (recur (rest sucessors) best alpha)))
                            (recur (rest sucessors) best alpha)))
                        (recur (rest sucessors) best alpha))))))))



          ))
      (if (= max-depth 0)
        {:best-move  (state :last-move)
         :best-value (state :heuristic-result)
         :who-win?   nil}

        (loop
          [sucessors (generate-successor-states state grid-width grid-height (not is-first-player)) ; Flip is first player.
           best {:best-value Double/NEGATIVE_INFINITY
                 :best-move  []
                 :who-win    nil}
           alpha alpha]
          ;(if (and (= 3 max-depth)
          ;         (not-empty sucessors))
          ;  (clojure.pprint/pprint
          ;    [((first (first sucessors)) :last-move)
          ;     ((first (first sucessors)) :heuristic-result)]))
          (if (empty? sucessors)
            best
            (let [{child-best-move :best-move tmp-child-best-value :best-value who-win? :who-win?} (negamax-inner (first (first sucessors))
                                                                                                                  (- beta)
                                                                                                                  (- alpha)
                                                                                                                  timeout
                                                                                                                  (not is-first-player)
                                                                                                                  (dec max-depth)
                                                                                                                  grid-width
                                                                                                                  grid-height)
                  child-best-value (- tmp-child-best-value)]
              (if (and (= child-best-value (best :best-value) Double/POSITIVE_INFINITY)
                       (not= who-win? nil))
                {:best-value child-best-value :best-move child-best-move :who-win? who-win?}
                (if (and (= (best :best-value) Double/POSITIVE_INFINITY)
                         (< child-best-value (best :best-value)))
                  best
                  (if (> child-best-value (best :best-value))
                    (let [best {:best-value child-best-value :best-move child-best-move :who-win? who-win?}]
                      (if (> (best :best-value) alpha)
                        (let [alpha (best :best-value)]
                          (if (and (>= alpha beta)
                                   (or (not= child-best-value Double/POSITIVE_INFINITY)
                                       (not= who-win? nil)))
                            best
                            (recur (rest sucessors) best alpha)))
                        (recur (rest sucessors) best alpha)))
                    (recur (rest sucessors) best alpha)))))))))))

(defn negamax
  ""
  [is-first-player state timeout grid-width grid-height]
  ; TODO Return if timeout
  (let [nega-result (negamax-inner state
                                   Double/NEGATIVE_INFINITY
                                   Double/POSITIVE_INFINITY
                                   timeout
                                   (not is-first-player)
                                   3                        ; Steps
                                   grid-width grid-height)]
    (prn "Negamax-value" nega-result)
    (nega-result :best-move)))

(defn -getNextMove
  "Gets the next best move"
  [charmat timeout]
  (let [charmat-seq (lazy-seq charmat)
        state (gen-map-from-charmat charmat-seq {true #{} false #{}} 0)
        is-first-player (even? (+ (count (state true)) (count (state false))))
        grid-width (count (first charmat-seq))
        grid-height (count charmat-seq)]

    (let [decision (negamax is-first-player
                            state
                            timeout
                            grid-width
                            grid-height)]
      (prn "Decision: " decision)
      (into [] decision))))
(defn chantest
  []
  (let [c (async/chan (async/sliding-buffer 1))
        heuristic-c (async/chan (async/sliding-buffer 1))
        timeout-chan (async/timeout 1999)]
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

