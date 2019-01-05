(ns hlt.core
  "Core functionality for Halite 3."
  (:require
   [cheshire.core :as json]
   [clojure.set :as set]
   [clojure.data :as data]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [hlt.utils :refer :all]
   [hlt.game :refer :all]
   [hlt.custom-game :refer :all]
   [hlt.dropoffs :refer :all]
   [hlt.collisions :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def DROP_OFF_AMOUNT 950)
(def DROP_OFF_AMOUNT_EARLY 950)
(def DROP_OFF_AMOUNT_LATE 650)
(def NUM_EARLY_SHIPS 9)
(def BACK_TO_GATHER_AMOUNT 650)
(def MIN_DROPOFF_DISTANCE 4)
(def PERCENT_TOP_CELLS 12)
; (def PERCENT_TOP_CELLS 20)
(def TOP_SCORE_DELTA 200)
; (def TURNS_TO_START_CRASHING 8)

(def last-spawn-turn-pct
  {2 0.52
   4 0.70})

(def LAST_TURN_DROPOFF_PCT 0.80)
(def MIN_SHIPS_BEFORE_IGNORE_GHOST 45)
(def MAX_TURNS_EVALUATE 5)

(def halite-burn-map
  {2 {32 16
      40 14
      48 12
      56 8
      64 8}
   4 {32 12
      40 12
      48 14
      56 14
      64 16}})

(def min-per-spawn-ship
  {2 {32 810
      40 820
      48 830
      56 840
      64 850}
   4 {32 610
      40 640
      48 700
      56 880
      64 860}})


(def DELTA_CARRY 500)
(def MAX_REWINDS 22)
(def NUM_BAN_TURNS 7)

(def get-steal-amount-by-map-size
  {32 1.0
   40 0.65
   48 0.35
   56 0.1
   64 0.0})

(defn get-steal-amount
  "Returns the amount I think I can possibly crash and take. Used to determine how many ships to
  build."
  [world]
  (let [{:keys [total-other-ship-halite width]} world]
    (if (two-player? world)
      0
      (* total-other-ship-halite (get-steal-amount-by-map-size width)))))

(defn want-to-spawn?
  "Returns true if player wants to spawn a turtle."
  [world]
  (and (enough-spawn-halite? world)
       (let [{:keys [turn last-dropoff-turn players my-player cells num-players total-halite
                     total-ship-count total-other-ship-halite last-spawn-turn width turns-left]} world
             my-ship-count (count (:ships my-player))
             my-num-dropoffs (inc (count (:dropoffs my-player)))
             amount-i-can-steal (get-steal-amount world)
             my-share (/ (+ amount-i-can-steal total-halite) (inc my-ship-count))]
         (or (and (two-player? world)
                  (<= my-ship-count (- total-ship-count my-ship-count))
                  (> turns-left (+ 35 TURNS_TO_START_CRASHING)))
             (and (< turn last-spawn-turn)
                  (or (> (/ (+ amount-i-can-steal total-halite) total-ship-count)
                         (get-in min-per-spawn-ship [num-players width]))
                      (> my-share (* 1.25 num-players (get-in min-per-spawn-ship [num-players width])))))))))

(defn can-spawn?
  "Returns true if player can spawn a turtle."
  [world shipyard]
  (and (enough-spawn-halite? world)
       (safe-location? world nil shipyard)))

(defn should-move?
  "Returns true if the ship should move."
  [world ship dropoff-location]
  (and (can-move? world ship)
       (not= (select-keys dropoff-location [:x :y]) (select-keys ship [:x :y]))
       (let [{:keys [num-players width]} world
             cell (get-in world [:cells (select-keys ship [:x :y])])
             halite-burn (get-in halite-burn-map [num-players width])]
             ; halite-burn (if (= :collect (:mode ship))
             ;               MAX_HALITE_BURN_COLLECT
             ;               MAX_HALITE_BURN_DROPOFF)]
         (or (>= (:halite ship) MAX_HALITE_CARRY)
             (:target ship)
             (ram-danger? world ship cell)
             (>= (:dropoff-distance cell) (- (:turns-left world) TURNS_TO_START_CRASHING))
             (<= (Math/floor (* (:halite cell) MOVE_COST)) halite-burn)
             (some? (seq (filter #(should-ram? world ship %) (get-surrounding-cells world ship))))
             (< (+ (Math/floor (* (:halite cell) MOVE_COST))
                   (apply min (map #(get-inspire-delta-by-move world ship %)
                                   (get-surrounding-cells world ship))))
                0)))))

(defn get-mode
  "Returns whether we are moving to drop off or collecting"
  [ship cell turns-left drop-off-amount]
  (if (or (>= (:dropoff-distance cell) (- turns-left TURNS_TO_START_CRASHING))
          (>= (:halite ship) drop-off-amount)
          (and (= :dropoff (:mode ship))
               (>= (:halite ship) BACK_TO_GATHER_AMOUNT)))
          ; (and (>= (:halite ship) BACK_TO_GATHER_AMOUNT)
          ;      (<= (:dropoff-distance cell) MIN_DROPOFF_DISTANCE)))
    :dropoff
    :collect))

(def FLOW_DISTANCE 15)

(defn get-top-cell-target
  "Returns the target to move after."
  [world ship]
  (let [{:keys [width height top-cells uninspired-cells dropoff-locations move-towards-dropoff?
                good-dropoffs]} world
        cells
              (if (and (seq dropoff-locations)
                       move-towards-dropoff?
                       (<= (apply min (map #(distance-between width height ship %) dropoff-locations))
                           FLOW_DISTANCE))
                (concat dropoff-locations good-dropoffs)
                (if (:motivated ship)
                  (concat top-cells good-dropoffs)
                  (concat good-dropoffs uninspired-cells)))
        ; cells (if (:motivated ship) top-cells uninspired-cells)
        field-comparison (if (:motivated ship) :score :uninspired-score)
        closest-target (first (sort (compare-by :distance asc field-comparison desc)
                                    (map #(assoc % :distance (distance-between width height ship %))
                                         cells)))]
    closest-target))

(defn get-uninspired-cell-target
  "Returns the target to move after."
  [world ship]
  (let [{:keys [width height top-cells uninspired-cells]} world
        cells uninspired-cells
        field-comparison :uninspired-score
        closest-target (first (sort (compare-by :distance asc field-comparison desc)
                                    (map #(assoc % :distance (distance-between width height ship %))
                                         cells)))]
    closest-target))

(defn get-best-direction
  "Returns the best direction to move to get to a target."
  [world ship target cells]
  (let [{:keys [width height]} world
        closest-target (first (sort (compare-by :cost asc :distance asc :halite asc)
                                    (map #(let [distance (distance-between width height target %)]
                                            (assoc %
                                                   :distance distance
                                                   :cost (+ (* 0.1 MOVE_COST (:halite %))
                                                            (get-inspire-delta-by-move world ship %)
                                                            (- (* 2000 (/ 1 (+ distance 0.5)))))))
                                         cells)))]
    (:direction closest-target)))

;; TODO I don't think anything should actually call this
(defn get-best-gather-direction
  "Returns the best direction to move to get to a target."
  [world ship target cells]
  (let [{:keys [width height]} world
        closest-target (first (sort (compare-by :cost asc :distance asc :halite desc)
                                    (map #(let [distance (distance-between width height target %)]
                                            (assoc %
                                                   :distance distance
                                                   :cost (+ (* 0.1 MOVE_COST (:halite %))
                                                            (get-inspire-delta-by-move world ship %)
                                                            (- (* 2000 (/ 1 (+ distance 0.5)))))))
                                         cells)))]
    (:direction closest-target)))

(defn shortest-path
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

(defn calculate-movement-cost
  "Returns the cost to move from one location to another."
  [world start end]
  (:total-cost (shortest-path world start end)))

(defn turns-to-full-mining
  "Returns the number of turns it will take to fully mine a cell."
  [world ship cell]
  ;; Took a long time for this log message
  ;; (log "Turns to full mining for ship " ship "and cell" cell)
  (let [
        moving-turns (distance-between (:width world) (:height world) ship cell)
        ; moving-turns (inc (distance-between (:width world) (:height world) ship cell))
         ;; TODO replace this approximate with actual
        movement-cost (calculate-movement-cost world ship cell)
        distance-cost 0]
        ; distance-cost (* DISTANCE_COST (- (:dropoff-distance cell) current-distance))]
    (loop [cell-halite (:halite cell)
           halite-carried (- (:halite ship) movement-cost distance-cost)
           turns (+ 1 moving-turns (if (< halite-carried 0) 1 0))]
      (let [gained-this-turn (Math/ceil (* GATHER_AMOUNT
                                           (+ cell-halite
                                              ; (get-bonus (assoc cell :halite cell-halite)))))
                                              (get-ship-bonus ship (assoc cell :halite cell-halite)))))
            halite-carried (+ halite-carried gained-this-turn)
            cell-halite (* CELL_HALITE_LEFT_BEHIND cell-halite)]
        (if (or (>= turns MAX_TURNS_EVALUATE)
                (>= halite-carried DROP_OFF_AMOUNT))
          {:turns turns
           :last-turn-gain gained-this-turn
           :halite-carried (min MAX_HALITE_CARRY halite-carried)}
          (recur cell-halite halite-carried (inc turns)))))))

(defn should-mine-cell?
  "Returns true if I should try to mine a cell."
  [world ship cell location]
  (let [{:keys [my-id turns-left]} world]
    (if (or (two-player? world))
            ; (little-halite-left? world MIN_CRASH_FOR_HALITE)
            ; (< (:turns-left world) CRASH_TURNS_LEFT))
      (or (nil? (:ship cell))
          (not= my-id (-> cell :ship :owner)))
      (safe-location? world ship location))))

(defn get-collect-move
  "Returns a move to collect as much halite as possible."
  [world ship]
  (let [{:keys [my-shipyard try-to-spawn?]} world
        directions (if (= 0 (:dropoff-distance (get-location world ship STILL)))
                       SURROUNDING_DIRECTIONS
                       ALL_DIRECTIONS)
        surrounding-cells (map #(assoc (get-location world ship %) :direction %) directions)
        ram-cell (first (filter #(and
                                      ;; (not (ghost-ship? (:ship %)))
                                      (should-ram? world ship %))
                                surrounding-cells))]
    ; (if ram-cell
    ;   (do (log "I am going to ram with ship " ship "and cell" (select-keys ram-cell [:x :y]))
    ;       (flog world ram-cell (format "Ramming with ship %d" (:id ship)) :green)
    ;       (assoc ram-cell :ship ship :reason "Ramming ship."))
      (let [safe-cells (filter #(safe-location? world ship %) (if try-to-spawn?
                                                                (remove #(= (select-keys my-shipyard [:x :y])
                                                                            (select-keys % [:x :y]))
                                                                        surrounding-cells)
                                                                surrounding-cells))
            safe-cells (if (and (empty? safe-cells) (two-player? world))
                         ; (filter #(only-other-ships? world ship %) surrounding-cells)
                         (filter #(safe-ignoring-ghost-ships? world ship %) surrounding-cells)
                         safe-cells)]
        (if (:target ship)
          (let [best-direction (get-best-direction world ship (:target ship) safe-cells)
                best-direction (or best-direction STILL)]
            (log "Target is " (select-keys (:target ship) [:x :y]) "and best direction" best-direction)
            (flog world (:target ship) (format "Ship %d existing target" (:id ship)) :orange)
            {:ship ship
             :target (:target ship)
             :direction best-direction
             :reason (str "Moving to target" (select-keys (:target ship) [:x :y :halite]))})
          (let [current-cell (get-location world ship STILL)
                nearby-cells (for [location (conj (-> current-cell :neighbors :inspiration) current-cell)
                                   :let [cell (get-location world location STILL)]
                                   :when (should-mine-cell? world ship cell location)
                                   :let [mining-info (turns-to-full-mining world ship cell)]]
                               (merge mining-info cell))
                target (first (sort (compare-by :turns asc :halite-carried desc :dropoff-distance desc)
                                    nearby-cells))
                mined-this-turn (* GATHER_AMOUNT (+ (:halite current-cell)
                                                    (get-bonus current-cell)))
                target (if (and target
                                (>= (:turns target) MAX_TURNS_EVALUATE)
                                (seq (:top-cells world))
                                ; (> mined-this-turn (:last-turn-gain target))
                                (<= mined-this-turn 25)
                                (<= (+ (:halite target) (get-bonus target)) 100)
                                ; (<= (:last-turn-gain target) 10)
                                (< (+ (:score target) TOP_SCORE_DELTA)
                                   (:min-top-cell-score world)))
                         nil
                         target)]
            (if (and target
                     ; (= (select-keys ship [:x :y]) (select-keys target [:x :y]))
                     (some #{STILL} (map :direction safe-cells))
                     (>= mined-this-turn (:last-turn-gain target)))
              {:ship ship
               :direction STILL
               :reason (str "collect more from current cell than last turn gain of target." (select-keys target [:x :y]))}
              (if target
                (let [
                      ; safe-cells (remove #(= STILL (:direction %)) safe-cells)
                      best-direction (get-best-gather-direction world ship target safe-cells)
                      best-direction (or best-direction STILL)]
                  (log "Nearby Target is " (select-keys target [:x :y :halite]) "and best direction" best-direction)
                  {:ship ship
                   :direction best-direction
                   :reason (str "Moving to best target in my nearby cells" (select-keys target [:x :y :halite]))})
                ;; Need to choose a new target
                (if ram-cell
                  (do (log "I am going to ram with ship " (:id ship) "and cell" (select-keys ram-cell [:x :y]))
                      (flog world ram-cell (format "Ramming with ship %d" (:id ship)) :green)
                      (assoc ram-cell :ship ship :reason "Ramming ship."))
                  (let [target (get-top-cell-target world ship)
                        ; safe-cells (remove #(= STILL (:direction %)) safe-cells)
                        best-direction (get-best-gather-direction world ship target safe-cells)
                        best-direction (or best-direction STILL)]
                    (log "Target is " target "and best direction" best-direction)
                    (flog world target (format "Chose new target for %d" (:id ship)) :yellow)
                    {:ship (assoc ship :target target)
                     :target target
                     :direction best-direction
                     :reason (str "There were no good targets so I picked" (select-keys target [:x :y :halite]))})))))))))

(defn get-dropoff-move
  "Returns a move towards a dropoff site."
  [world ship]
  (let [banned-cells (:banned-cells world)
        surrounding-cells (for [direction ALL_DIRECTIONS]
                            (assoc (get-location world ship direction) :direction direction))
        ram-cell (first (filter #(and
                                      (not (ghost-ship? (:ship %)))
                                      (should-ram? world ship %))
                                surrounding-cells))]
   ; (if ram-cell
   ;   (do (log "I am going to ram with ship " ship "and cell" (select-keys ram-cell [:x :y]))
   ;       (flog world ram-cell (format "Ramming with ship %d" (:id ship)) :green)
   ;       (assoc ram-cell :ship ship :reason "Ramming ship."))
     (let [
           allowed-cells (remove #(banned-cell? % banned-cells) surrounding-cells)
           safe-cells (filter #(safe-location? world ship %) allowed-cells)
           best-choice (->> safe-cells
                            (map #(assoc % :cost (+ (* 0.1 MOVE_COST (:halite %))
                                                    (get-inspire-delta-by-move world ship %)
                                                    (- (* 2000 (/ 1 (+ 0.5 (:next-dropoff-distance %))))))))
                            (sort (compare-by :cost asc :next-dropoff-distance asc))
                            ; (sort (compare-by :cost asc :dropoff-distance asc :halite asc))
                            first)
           ; safe-cells (if (and (> (:my-ship-count world) MIN_SHIPS_BEFORE_IGNORE_GHOST)
           ;                     (or (nil? best-choice)
           ;                         (= STILL (:direction best-choice))))
           ;              (filter #(safe-ignoring-ghost-ships? world ship %) surrounding-cells)
           ;              safe-cells)
           ; blocked? (blocked-by-enemy? world best-choice ship surrounding-cells)
           blocked? (blocked-by-enemy? world best-choice ship)
           best-choice (if (and blocked? (= STILL (:direction best-choice)))
                         (->> safe-cells
                              (map #(assoc % :cost (+ (* 0.1 MOVE_COST (:halite %))
                                                      (get-inspire-delta-by-move world ship %)
                                                      (- (* 2000 (/ 1 (+ 0.5 (:next-dropoff-distance %))))))))
                              (sort (compare-by :cost asc :next-dropoff-distance asc))
                              ; (sort (compare-by :cost asc :dropoff-distance asc :halite asc))
                              first)
                         best-choice)
           best-choice (when best-choice
                         (assoc best-choice :ship ship))
           banned-cells (when (and blocked? best-choice)
                          (assoc banned-cells
                                 (select-keys (get-location world ship STILL) [:x :y]) NUM_BAN_TURNS))]
           ; best-choice (->> safe-cells
           ;                  (sort (compare-by :dropoff-distance asc :halite asc))
           ;                  first)
           ; best-choice (when best-choice
           ;               (assoc best-choice :ship ship))]
       ; (log "Safe cells for dropoff move are: " safe-cells)
       (if (and best-choice blocked?)
         (assoc best-choice :reason "best dropoff choice, but had to ban a direction"
                :banned-cells banned-cells)
         (if best-choice
           (assoc best-choice :reason "best dropoff choice")
           {:ship ship :direction STILL :reason "dropoff couldn't find a good best choice"})))))

(defn get-move
  "Returns a move direction"
  [world ship]
  (log "Deciding on move for ship:" (:id ship))
  (if-not (should-move? world ship nil)
    {:ship ship :direction STILL :reason "Should move returned false."}
    (if (= :collect (:mode ship))
      (get-collect-move world ship)
      (get-dropoff-move world ship))))

(defn get-evade-move
  "Returns a move to try and escape."
  [world ship]
  (let [{:keys [my-id]} world
        surrounding-cells (map #(assoc (get-location world ship %) :direction %)
                               SURROUNDING_DIRECTIONS)
        open-cells (filter #(and (not (at-enemy-dropoff? world %))
                                 (or (nil? (:ship %))
                                     (ghost-ship? (:ship %))))
                           surrounding-cells)
        _ (log "Turn " (:turn world) "open cells are" open-cells)
        open-cells (map #(assoc % :cost (get-inspire-delta-by-move world ship %)) open-cells)
        best-cell (first (sort (compare-by :cost asc :dropoff-distance asc :halite asc) open-cells))]
    (if best-cell
      (assoc best-cell :ship ship :reason "Evading to an open cell." :color :red)
      (let [enemy-cells (filter #(and (not= my-id (-> % :ship :owner))
                                      (not (at-enemy-dropoff? world %)))
                                surrounding-cells)
            enemy-cells-with-ship-carry (map #(assoc % :ship-carry (-> % :ship :halite))
                                             enemy-cells)
            best-cell (first (sort (compare-by :ship-carry desc :dropoff-distance asc :halite asc)
                                   enemy-cells-with-ship-carry))]
        (if best-cell
          (assoc best-cell :ship ship :reason "Can't evade - crashing into random enemy." :color :red)
          {:ship ship :direction STILL :reason "Impossible." :color :red})))))

(defn get-moves-and-world
  "Returns moves and an updated world."
  [world ships]
  (reduce (fn [{:keys [world moves]} ship]
            (let [move (get-move world ship)
                  target (:target move)
                  direction (:direction move)
                  banned-cells (or (:banned-cells move)
                                   (:banned-cells world))
                  location (get-location world ship direction)
                  sitting-duck? (and (= STILL direction)
                                     (can-move? world ship)
                                     (ram-danger? world ship location))
                  move (if sitting-duck?
                         (get-evade-move world ship)
                         move)
                  target (:target move)
                  direction (:direction move)
                  location (get-location world ship direction)
                  colliding-ship (get-colliding-ship world location)
                  ;; Note the colliding ship is not always the cause when forcing to stay still
                  prior-colliding-ship (when colliding-ship
                                         (get-colliding-ship world (get-location world colliding-ship STILL)))
                  _ (when colliding-ship
                      (log "CDD: collision at location " location "calculating move for ship" ship
                           "which decided on direction " direction "causing it to hit" colliding-ship
                           "but that was because of " prior-colliding-ship))
                  move (assoc move :location location :collision colliding-ship
                                   :pre-collision prior-colliding-ship)
                  world (add-ship-to-cell world ship location)
                  ; updated-cells (add-ship-to-cell (:cells world)
                  ;                                 (merge ship (select-keys location [:x :y]))
                  ;                                 location)
                  my-ships (-> world :my-player :ships)
                  updated-ships (conj (remove-item ship my-ships) (:ship move))
                  updated-top-cells (if target
                                      (remove (fn [cell]
                                                (= (select-keys target [:x :y])
                                                   (select-keys cell [:x :y])))
                                              (:top-cells world))
                                      (:top-cells world))
                  moves (conj moves move)]
              {:world (assoc world
                             :top-cells updated-top-cells
                             :my-player (assoc (:my-player world) :ships updated-ships)
                             :banned-cells banned-cells)
               :moves moves}))
          {:world world}
          ships))

(defn score-cell
  "Sets a score for a cell."
  [world cell]
  ; (log "Scoring cell:" cell)
  (let [surrounding-cells (get-three-range-cells world cell)]
    [(+ (* 1.25 (+ (get-bonus cell) (:halite cell)))
        (reduce + (map #(+ (:halite %) (get-bonus %)) surrounding-cells)))
     ; (+ (* 4 (+ (get-bonus cell) (:halite cell))
     ;     (reduce + (map #(+ (:halite %) (get-bonus %)) surrounding-cells))))]))
     (+ (* 1.25 (:halite cell))
        (reduce + (map :halite surrounding-cells)))]))

(defn decorate-cells
  "Adds dropoff distance and a score to each cell. Handles multiple shipyards and choose the min distance."
  [world cells-to-update shipyards next-dropoffs]
  (let [dropoffs shipyards
        {:keys [width height]} world]
    (into {}
      (for [cell cells-to-update
            :let [min-distance (first (sort (map #(distance-between width height cell %) dropoffs)))
                  next-distance (apply min min-distance (if (seq next-dropoffs)
                                                          (map #(distance-between width height cell %)
                                                               next-dropoffs)
                                                          [INFINITY]))
                  [score uninspired-score] (score-cell world cell)]]
                  ; enemy-side-count (get-surrounded-enemy-count world cell)]]
                  ; uninspired-score (- uninspired-score (* 100 min-distance))]]
        [(select-keys cell [:x :y]) (assoc cell
                                           :dropoff-distance min-distance
                                           :next-dropoff-distance next-distance
                                           :score score
                                           :uninspired-score uninspired-score)]))))
                                           ; :surrounded-enemy-count enemy-side-count)]))))

(defn decorate-ship
  "Adds extra keys to a ship that are useful."
  [world ship ship-location-map last-round-ships drop-off-amount]
  (let [{:keys [total-halite total-ship-count num-players width]} world
        ;; TODO performance improvement
        can-ignore-motivate? (> (/ total-halite total-ship-count) (get-in min-per-spawn-ship
                                                                          [num-players width]))
        cell (get-location world ship STILL)
        last-round-ship (get last-round-ships (:id ship))
        last-mode (:mode last-round-ship)
        mode (if (and (= :dropoff last-mode)
                      (> (:halite ship) BACK_TO_GATHER_AMOUNT))
               :dropoff
               (get-mode ship cell (:turns-left world) drop-off-amount))
        num-surrounding-ships 0]
        ; surrounding-sites (get-surrounding-cells world ship)
        ; num-surrounding-ships (count (keep #(get ship-location-map (select-keys % [:x :y]))
        ;                                    surrounding-sites))]
    (log "Ship " ship "decided on mode" mode)
    (assoc ship
           :mode mode
           :motivated true
           :cell-halite (:halite cell)
           :target (:target last-round-ship)
           :neighbor-count num-surrounding-ships
           :dropoff-distance (:dropoff-distance cell))))

(def MIN_SHIPS_TO_RAM_GHOST 20)

(defn predict-enemy-ship-locations
  "Returns an updated world with ships at all predicted locations."
  [world ship-location-map]
  (let [{:keys [players my-id width turns-left my-ship-count]} world
        other-players (filter #(not= (:player-id %) my-id)
                              players)
        ; other-dropoffs (flatten (map :dropoffs other-players))
        other-ships (flatten (map :ships other-players))]
        ; my-ships (:ships my-player)]
    (log "Predict ships with other-ships" other-ships) ; "and other dropoffs" other-dropoffs)
    (reduce (fn [updated-world ship]
              ; (log "Reduce called with ship" ship " and Updated world " updated-world)
              (let [cell (get-location updated-world ship STILL)
                    updated-world (assoc-in updated-world
                                            [:cells (select-keys cell [:x :y]) :ship] ship)
                    surrounding-cells (when (or (little-halite-left? world MIN_CRASH_FOR_HALITE)
                                                (<= turns-left CRASH_TURNS_LEFT)
                                                (and (> my-ship-count MIN_SHIPS_TO_RAM_GHOST)
                                                     (< width 50)))
                                        (get-surrounding-cells world cell))]
                    ; surrounding-cells (when (can-move? updated-world ship)
                    ;                     (get-surrounding-cells updated-world cell))
                    ; surrounding-cells (get-surrounding-cells updated-world cell)]
                (reduce (partial add-ghost-to-cell ship)
                        updated-world
                        surrounding-cells)))
            world
            other-ships)))
            ; (concat other-dropoffs other-ships))))

(defn get-top-cells
  "Returns the top pct cells by score"
  [world pct]
  (let [{:keys [cells width height ship-location-map my-id turns-left]} world
        num-cells-to-return (Math/floor (* width height pct 0.01))
        cells (vals cells)
        cells (remove #(when-let [ship (get ship-location-map (select-keys % [:x :y]))]
                         (not= my-id (:owner ship)))
                            ; (< (:halite %) 1000))
                      cells)
        ; best-cells (take num-cells-to-return (sort (compare-by :score desc) cells))
        ; best-cells (if (two-player? world)
        ;              cells
        ;              (filter #(safe-location? world {:halite 1000} %) cells))
        ; cells (filter #(safe-location? world {assoc % :halite 1000} %)
        ;               cells)]

        ; cells (filter #(> (:halite %) 150)
        ;               (vals cells))
        best-cells (if (or (two-player? world)
                           (little-halite-left? world MIN_CRASH_FOR_HALITE)
                           (< turns-left CRASH_TURNS_LEFT))
                     cells
                     (filter (fn [cell]
                               (let [nearby-ships (get-five-range-ships world cell)]
                                 (when (< (count nearby-ships) 6)
                                   cell)))
                             cells))]
    [(take num-cells-to-return (sort (compare-by :score desc) (remove #(get ship-location-map (select-keys % [:x :y]))
                                                                      best-cells)))
     (take num-cells-to-return (sort (compare-by :uninspired-score desc) best-cells))]))

(defn remove-bad-targets
  "If my current cell is better than my target - get rid of my target."
  [world dropoff-location]
  (let [{:keys [top-cells ship-location-map width height]} world
        top-cell-locations (map #(select-keys % [:x :y]) top-cells)
        my-ships (-> world :my-player :ships)
        ships-with-targets (filter :target my-ships)
        dropoff-x-y (select-keys dropoff-location [:x :y])
        ships-with-targets (remove #(= dropoff-x-y (select-keys % [:x :y]))
                                   ships-with-targets)
        ships-to-remove-target (filter (fn [ship]
                                         (let [ship-location (select-keys ship [:x :y])
                                               target-location (select-keys (:target ship) [:x :y])]
                                           (or (= target-location ship-location)
                                               (get ship-location-map target-location)
                                               (<= (distance-between width height ship-location target-location) 15)
                                               (better-cell? (get-location world ship STILL)
                                                             (get-location world (:target ship) STILL))
                                               (some (set [(select-keys ship [:x :y])])
                                                     top-cell-locations))))
                                       ships-with-targets)]
    ; (log "Turn" (:turn world) "Ships with targets were " (pr-str ships-with-targets))
    (reduce blank-out-target
            world
            ships-to-remove-target)))


(defn remove-one-turn-from-banned-cells
  "Cells are banned for a set number of turns."
  [world banned-cells]
  (let [{:keys [ship-location-map my-id]} world]
    (into {}
          (for [[location turns] banned-cells
                :let [turns (dec turns)]
                :when (and (> turns 0)
                           (some? (first (filter #(let [ship (:ship %)]
                                                    (and ship
                                                         ; (not= GHOST (:id ship))
                                                         (not= my-id (:owner ship))))
                                                 (get-two-range-cells world (get-location world location STILL))))))]
            [location turns]))))

; (defn get-value-of-a-ship
;   "Note - I will want to change this at some point to take into account the average halite gained
;   per turn divided by the number of ships. For now just base on total halite on the map and
;   the number of ships."
;   [world]
;   (/ (:total-halite world) (:total-ship-count world)))
;
; (defn get-cost-of-wasted-turn
;   "Returns the cost of wasting a turn by staying STILL."
;   [world]
;   (/ (:total-halite world) (:total-ship-count world)))
;
; (def odds-of-collision-still 0.2)
; (def odds-of-collision-moving 0.1)
;
; (defn get-ships-in-cells
;   "Returns ships from cells."
;   [world locations]
;   (keep :ship (map #(get-location world % STILL) locations)))
;
; (defn play-out-fight
;   "Figure out who will collect what from a fight based on looking out up to 7 turns..."
;   [world cell my-ship their-ship]
;   (let [;; Include the inspiration bonus. Assume if I'm inspired they are too.
;         dropped-halite (* (+ (:halite my-ship) (:halite their-ship))
;                           (if (inspired-cell? world cell)
;                             3
;                             1))]
;     (loop [remaining-halite dropped-halite
;            iteration 1
;            total-halite 0
;            ships (remove #(or (= (:id my-ship) (:id %))
;                               (= (:id their-ship) (:id %)))
;                          (get-ships-in-cells (get-in cell [:neighbors 1])))]
;       (if (or (<= remaining-halite 0)
;               (> iteration 7))
;         {:total total-halite :leftover remaining-halite}
;         (let [[my-carry-capacity other-carry-capacity] (get-carrying-capacity world ships)
;               delta (long (- my-carry-capacity other-carry-capacity))
;               delta (if (> (Math/abs delta) remaining-halite)
;                       (if (> delta 0)
;                         remaining-halite
;                         (* -1 remaining-halite))
;                       delta)
;               remaining-halite (- remaining-halite delta)
;               total-halite (long (+ total-halite delta))]
;           (recur remaining-halite
;                  (inc iteration)
;                  total-halite
;                  (get-ships-in-cells (get-in cell [:neighbors (inc iteration)]))))))))
;
; (defn spoils-of-war
;   "Returns a value of the net halite change from a collision. This could get complicated to
;   track correctly, but is probably one of the most important things to get right in this game."
;   [world my-ship cell their-ship]
;   (let [{:keys [total leftover]} (play-out-fight world cell my-ship their-ship)]
;     (if (= 0 leftover)
;       total
;       (let [my-closest-base (get-closest-cell world cell (:my-dropoffs world))
;             their-closest-base (get-closest-cell world cell (:enemy-dropoffs world))]
;         (if (< (:distance my-closest-base) (:distance their-closest-base))
;           (+ total (* 0.5 leftover))
;           (if (= (:distance my-closest-base) (:distance their-closest-base))
;             total
;             (- total (* 0.5 leftover))))))))
;
; (defn score-collision
;   "Provides a score for choosing the given cell with regards to collision. Positive means
;   an expected gain from a collision and negative means an expected loss from a collision."
;   [world ship cell]
;   (if-let [other-ship (:ship cell)]
;     (if (= (:my-id world) (:owner other-ship))
;       (- (* -2 (get-value-of-a-ship world)) (:halite ship) (:halite other-ship))
;       (let [collisions-odds (if (ghost-ship? (:ship cell))
;                               0.1
;                               0.2)]
;         (if (two-player? world)
;           (* collisions-odds (spoils-of-war world ship cell other-ship))
;           (+ (get-value-of-a-ship world)
;              (* collisions-odds (spoils-of-war world ship cell other-ship))))))
;     0))


(defn score-mining
  "Provides a score for choosing the given cell with regards to mining. All values should
  be greater than or equal to 0."
  [world ship cell]
  0)

(defn score-inspiration
  "Provides a score for choosing the given cell with regards to inspiration. Positive means
  an expected gain from inspiration and negative means an expected enemy gain from inspiration."
  [world ship cell]
  (get-inspire-delta-by-move world ship cell))

(defn score-movement
  "Provides a score for moving to the given cell. All values will be less than or equal to 0."
  [world ship cell]
  (let [current-cell (get-location world ship STILL)]
    (if (= (select-keys current-cell [:x :y]) (select-keys cell [:x :y]))
      0
      (* -1 MOVE_COST (:halite current-cell)))))

(defn score-movement-towards-base
  "Provides a score for moving to the given cell. All values will be less than or equal to 0."
  [world ship cell]
  (let [current-cell (get-location world ship STILL)]
    (if (= (select-keys current-cell [:x :y]) (select-keys cell [:x :y]))
      0
      (* -1 MOVE_COST (:halite current-cell)))))

(defn score-collect-move
  "Provides an overall score for a move attempting to try to collect halite."
  [world ship cell mining-target]
  (let [collision-score (score-collision world ship cell)
        mining-score (score-mining world ship cell)
        inspiration-score (score-inspiration world ship cell)
        movement-score (score-movement world ship cell)
        total (+ collision-score mining-score inspiration-score movement-score)]
    (flog world ship (str "Collect score:" total))
    total))

(defn score-dropoff-move
  "Provides an overall score for a move attempting to try to dropoff halite."
  [world ship cell mining-target]
  (let [collision-score (score-collision world ship cell)
        inspiration-score (score-inspiration world ship cell)
        movement-score (score-movement-towards-base world ship cell)
        total (+ collision-score inspiration-score movement-score)]
    (flog world ship (str "Dropoff score:" total))
    total))



        ;; Don't think this one makes sense to add (at least yet)
        ;; target-score (score-moving-toward-target world ship cell)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main loop
(defn -main
  "Main"
  [& args]
  (setup-logging args)
  (let [world (load-world)
        {:keys [my-shipyard cells width height num-players my-id other-shipyards]} world
        cells (map #(add-neighbors world %) (vals cells))
        cells (decorate-cells world cells [my-shipyard] nil)
        last-turn (total-turns height width)
        last-spawn-turn (* last-turn (get last-spawn-turn-pct num-players))
        last-dropoff-turn (* last-turn LAST_TURN_DROPOFF_PCT)
        world (assoc world
                     :last-turn last-turn :last-spawn-turn last-spawn-turn
                     :last-dropoff-turn last-dropoff-turn)]
    (println bot-name)
    (loop [cells cells
           last-round-ships nil
           last-round-other-player-ships nil
           last-dropoff-location nil
           last-dropoff-locations nil
           banned-cells nil]
      (let [world (build-world-for-round (assoc world :cells cells) last-round-other-player-ships
                                         TURNS_TO_START_CRASHING)
            {:keys [ship-location-map potential-locations updated-cells my-player turns-left
                    players turn cells other-players
                    potential-locations other-player-ships]} world
            score-potential-locations (mapcat #(get-locations-in-inspiration-range world %)
                                              updated-cells)
            score-potential-locations (set (concat score-potential-locations
                                                   (mapcat #(get-locations-in-inspiration-range world %)
                                                           potential-locations)))
            ;; ## IMPORTANT - must be done prior to scoring cells
            world (if (> (:turns-left world) TURNS_TO_START_CRASHING)
                    (predict-enemy-ship-locations world ship-location-map)
                    world)
            score-potential-cells (map #(get-location world % STILL) score-potential-locations)
            build-dropoff? (should-build-dropoff? world last-dropoff-location)
            max-halite-dropoff (if (seq last-dropoff-locations)
                                 (apply max (map :halite last-dropoff-locations))
                                 500)
            move-towards-dropoff? (and (seq last-dropoff-locations)
                                       build-dropoff?
                                       (or (> (count (:ships my-player)) 25)
                                           (> (:halite my-player)
                                              (- DROPOFF_COST max-halite-dropoff))))
            updated-cell-map (decorate-cells world
                                             score-potential-cells
                                             (conj (:dropoffs my-player) my-shipyard)
                                             (when move-towards-dropoff?
                                               last-dropoff-locations))
            cells (merge cells updated-cell-map)
            _ (doseq [cell (filter :inspired (vals cells))]
                (flog world (select-keys cell [:x :y]) "Inspired")) ;:yellow))
            world (assoc world :cells cells)
            [top-cells uninspired-cells] (get-top-cells world PERCENT_TOP_CELLS)
            min-top-cell-score (if (empty? top-cells)
                                 0
                                 (->> top-cells
                                     (map :score)
                                     (apply min)))
            min-uninspired-score (if (empty? uninspired-cells)
                                   0
                                   (->> uninspired-cells
                                       (map :uninspired-score)
                                       (apply min)))
            drop-off-amount (if (< (count (:ships my-player)) NUM_EARLY_SHIPS)
                              DROP_OFF_AMOUNT_EARLY
                              (if (and (not (two-player? world))
                                       (or (little-halite-left? world MIN_CRASH_FOR_HALITE)
                                           (<= turns-left CRASH_TURNS_LEFT)))
                                DROP_OFF_AMOUNT_LATE
                                DROP_OFF_AMOUNT))
            decorated-ships (map #(decorate-ship world % ship-location-map last-round-ships
                                                 drop-off-amount)
                                 (:ships my-player))
            my-player (assoc my-player :ships decorated-ships)
            good-dropoffs (get-good-dropoffs world)
            world (assoc world
                         :good-dropoffs good-dropoffs
                         :my-player my-player
                         :top-cells top-cells
                         :uninspired-cells uninspired-cells
                         :min-top-cell-score min-top-cell-score
                         :min-uninspired-score min-uninspired-score)
            build-dropoff-distance (get-dropoff-distance (count (:dropoffs my-player)))
            dropoff-location (when (and last-dropoff-location
                                        (>= (:dropoff-distance (get-location world last-dropoff-location STILL))
                                            build-dropoff-distance))
                               last-dropoff-location)
            dropoff-location last-dropoff-location
            dropoff-locations (choose-dropoff-locations world dropoff-location)
            world (assoc world :dropoff-locations dropoff-locations)
            _ (doseq [dl dropoff-locations]
                (flog-color world dl "Chosen dropoff" :blue))
            ; dropoff-location (first dropoff-locations)
            ; _ (when dropoff-location
            ;     (flog world dropoff-location (format "Dropoff location selected - score: %s, uninspired score: %s."
            ;                                          (str (:score dropoff-location))
            ;                                          (str (:uninspired-score dropoff-location)))
            ;                                  :blue))
            ; changed-dropoff? (not= (select-keys last-dropoff-location [:x :y])
            ;                        (select-keys dropoff-location [:x :y]))
            ; world (if changed-dropoff?
            ;         (unassign-dropoff-moves world last-dropoff-location)
            ;         world)
            ; world (assoc world :dropoff-locations dropoff-locations)
            world (remove-bad-targets world last-dropoff-location)
            my-player (:my-player world)
            ; build-dropoff? (should-build-dropoff? world last-dropoff-location)
            halite-to-save (if build-dropoff?
                             (- DROPOFF_COST (apply max 500 (map :halite dropoff-locations)))
                             0)
            world (assoc world
                         :reserve halite-to-save
                         :move-towards-dropoff? (and move-towards-dropoff? build-dropoff?))

            try-to-spawn? (want-to-spawn? world)

            banned-cells (remove-one-turn-from-banned-cells world banned-cells)
            _ (doseq [cell (keys banned-cells)]
                (flog world cell "Banned cell" :green))
            world (assoc world :banned-cells banned-cells :try-to-spawn? try-to-spawn?)

            world (ignore-enemy-ships-on-base world)
            dropoff-ship (when build-dropoff?
                           (choose-dropoff-ship world))
            world (if (and build-dropoff? (nil? dropoff-ship) (seq dropoff-locations))
                    (assign-dropoff-moves world)
                    world)
            dropoff-location (when-not dropoff-ship dropoff-location)
            my-player (:my-player world)

            stuck-ships (get-stuck-ships world (remove #(= (:id dropoff-ship) (:id %))
                                                       (:ships my-player))
                                         dropoff-location)
            other-ships (get-my-ships-that-can-move stuck-ships my-player)
            other-ships (remove #(= (:id dropoff-ship) (:id %)) other-ships)
            collecting-ships (sort (compare-by :cell-halite desc :halite desc :dropoff-distance desc)
                                   (filter #(= :collect (:mode %)) other-ships))
            dropoff-ships (sort (compare-by :dropoff-distance asc :halite desc)
                                (filter #(= :dropoff (:mode %)) other-ships))
            {:keys [world moves]} (get-moves-and-world world (concat stuck-ships
                                                                     dropoff-ships
                                                                     collecting-ships))
            [world moves] (if (> (:turns-left world) TURNS_TO_START_CRASHING)
                            (unwind-collisions world moves get-moves-and-world MAX_REWINDS)
                            [world moves])
            world (update-world-for-dropoff-ship world dropoff-ship)
            spawn-command (get-spawn-command (and try-to-spawn? (can-spawn? world my-shipyard)))
            dropoff-command (get-dropoff-command dropoff-ship)
            cells-without-ships (into {}
                                  (map (fn [[k v]]
                                         [k (dissoc v :ship)])
                                       cells))
            last-round-ships (into {}
                                   (map (fn [ship]
                                          [(:id ship) ship])
                                        (-> world :my-player :ships)))]

        (log "Players" (pr-str players))
        (doseq [move moves]
          (if (:color move)
            (flog world (:ship move) (:reason move) (:color move))
            (flog world (:ship move) (:reason move))))
        (log "My output:" (str spawn-command (string/join " " (map generate-move-command moves))))
        (log "Stuck ships:" (map :id stuck-ships))
        (log "Turn " turn "logging all moves.")
        (println (str spawn-command " "
                      dropoff-command " "
                      (string/join " " (map generate-move-command moves))))
        (recur cells-without-ships last-round-ships other-player-ships dropoff-location dropoff-locations
               (:banned-cells world))))))
