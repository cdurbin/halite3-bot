(ns hlt.scoring
  (:require
   [hlt.utils :refer :all]
   [hlt.game :refer :all]
   [hlt.custom-game :refer :all]
   [hlt.collisions :refer :all]))

(defn shortest-path-old
  "Returns the directions to take for the shortest path and least halite used between start and end."
  [world start end]
  (let [{:keys [width height]} world
        end-location (select-keys end [:x :y])]
    (loop [potential-directions SURROUNDING_DIRECTIONS
           location (select-keys start [:x :y])
           dropoff-distance (:dropoff-distance (get-location world start STILL))
           total-cost 0
           directions nil]
      (if (= location end-location)
        {:directions directions :total-cost total-cost}
        (let [distance-maps (map (fn [direction]
                                   (let [cell (get-location world location direction)
                                         distance (distance-between width height cell end-location)]
                                     {:distance distance
                                      :direction direction
                                      :halite (:halite cell)
                                      :dropoff-distance (:dropoff-distance cell)
                                      :location (select-keys cell [:x :y])}))
                                 potential-directions)
              winner (first (sort (compare-by :distance asc :halite asc) distance-maps))
              cost-multiplier (if (> (:dropoff-distance winner) dropoff-distance)
                                2
                                1)
              potential-directions (remove #(= (:direction winner) (get opposite-direction %))
                                           potential-directions)
              total-cost (long (+ total-cost (Math/floor (* MOVE_COST cost-multiplier (:halite winner)))))]
          (recur potential-directions
                 (:location winner)
                 (:dropoff-distance winner)
                 total-cost
                 (conj directions (:direction winner))))))))

(defn shortest-path
  "Returns the directions to take for the shortest path and least halite used between start and end."
  [world start end]
  (let [{:keys [width height]} world
        end-location (select-keys end [:x :y])]
    (loop [potential-directions SURROUNDING_DIRECTIONS
           location (select-keys start [:x :y])
           halite-from-last-cell (:cell-halite start)
           ; dropoff-distance (:dropoff-distance (get-location world start STILL))
           total-cost 0
           directions nil]
      (if (= location end-location)
        {:directions directions :total-cost total-cost}
        (let [distance-maps (map (fn [direction]
                                   (let [cell (get-location world location direction)
                                         distance (distance-between width height cell end-location)]
                                     {:distance distance
                                      :direction direction
                                      :halite (:halite cell)
                                      ; :dropoff-distance (:dropoff-distance cell)
                                      :location (select-keys cell [:x :y])}))
                                 potential-directions)
              winner (first (sort (compare-by :distance asc :halite asc) distance-maps))
              cost-multiplier 1
              potential-directions (remove #(= (:direction winner) (get opposite-direction %))
                                           potential-directions)
              total-cost (long (+ total-cost (Math/floor (* MOVE_COST cost-multiplier halite-from-last-cell))))]
          (recur potential-directions
                 (:location winner)
                 (:halite winner)
                 total-cost
                 (conj directions (:direction winner))))))))

(defn calculate-movement-cost
  "Returns the cost to move from one location to another."
  [world start end]
  (:total-cost (shortest-path world start end)))

(defn score-mining
  "Provides a score for choosing the given cell with regards to mining. All values should
  be greater than or equal to 0."
  [world ship cell]
  (let [capacity (get-capacity ship)
        mined-amount (min capacity (get-gather-amount cell))]
    (if (> (- capacity mined-amount) 50)
      mined-amount
      (let [halite-taken (min capacity (* GATHER_AMOUNT (:halite cell)))
            remaining-halite (- (:halite cell halite-taken))]
        (- mined-amount (* MOVE_COST remaining-halite))))))

(defn score-inspiration
  "Provides a score for choosing the given cell with regards to inspiration. Positive means
  an expected gain from inspiration and negative means an expected enemy gain from inspiration."
  [world ship cell]
  (get-inspire-delta-by-move world ship cell))

(defn score-movement
  "Provides a score for moving to the given cell. All values will be less than or equal to 0."
  [world ship cell]
  (* -1 (calculate-movement-cost world ship cell)))

(defn score-movement-towards-base
  "Provides a score for moving to the given cell. All values will be less than or equal to 0.
  TODO - don't punish waiting for a cell blocked by my own ships, DO punish staying still when
  waiting on a block from an enemy.
  TODO - plan route all the way back to the base instead of just one move closer."
  [world ship cell]
  (let [current-cell (get-location world ship STILL)
        current-distance (:dropoff-distance current-cell)
        new-distance (:dropoff-distance cell)
        distance-score (if (< current-distance new-distance)
                         -250
                         (if (= current-distance new-distance)
                           -100
                           150))
        movement-cost (if (= (select-keys current-cell [:x :y]) (select-keys cell [:x :y]))
                        0
                        (* -1 MOVE_COST (:halite cell)))]
    (+ distance-score movement-cost)))

(defn ram-danger-score
  [world ship cell]
  (let [ship-in-cell (:ship cell)]
    (if (= (:my-id world) (:owner ship-in-cell))
      -2000
      (let [enemy-ships (filter #(and (not= (:my-id world) (:owner %))
                                      (not (ghost-ship? %)))
                                (get-ships-in-cells world (conj (get-in cell [:neighbors 1]) cell)))
            scores (map #(int (score-collision world ship % cell)) enemy-ships)
            low-score (when (seq scores)
                        (apply min scores))
            low-score (if (or (nil? low-score)
                              (two-player? world))
                        low-score
                        (+ low-score (* 0.25 (get-value-of-a-ship world))))]
        (if (nil? low-score)
          0
          low-score)))))

(defn score-collect-move
  "Provides an overall score for a move attempting to try to collect halite."
  [world ship cell]
  ; [world ship cell mining-target]
  (let [collision-score (ram-danger-score world ship cell)
        mining-score (score-mining world ship cell)
        inspiration-score (score-inspiration world ship cell)
        movement-score (score-movement world ship cell)
        total (+ (min 0 collision-score) mining-score inspiration-score movement-score)]
    ; (flog world ship (str "Collect score:" total))
    total))

(defn score-target-move
  "Provides an overall score for a move attempting to try to collect halite from the target."
  [world ship cell target]
  ; [world ship cell mining-target]
  (let [collision-score (ram-danger-score world ship cell)
        inspiration-score (score-inspiration world ship cell)
        ; _ (log "cell" cell "target" target)
        movement-score (score-movement world cell target)
        total (+ (min 0 collision-score) inspiration-score movement-score)]
    ; (flog world ship (str "Target score:" total))
    total))

(defn score-dropoff-move
  "Provides an overall score for a move attempting to try to dropoff halite."
  [world ship cell]
  ; [world ship cell mining-target]
  (let [collision-score (ram-danger-score world ship cell)
        inspiration-score (score-inspiration world ship cell)
        movement-score (score-movement-towards-base world ship cell)
        total (+ (min 0 collision-score) inspiration-score movement-score)]
    ; (flog world ship (str "Dropoff score:" total))
    total))
