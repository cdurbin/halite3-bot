(ns hlt.old-core
  "My old bot."
  (:require
   [cheshire.core :as json]
   [clojure.set :as set]
   [clojure.data :as data]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [hlt.utils :refer :all]
   [hlt.game :refer :all]
   [hlt.custom-game :refer :all]
   [hlt.dropoffs :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def DROP_OFF_AMOUNT 950)
(def DROP_OFF_AMOUNT_EARLY 800)
(def DROP_OFF_AMOUNT_LATE 550)
(def NUM_EARLY_SHIPS 9)
(def BACK_TO_GATHER_AMOUNT 650)
(def MIN_DROPOFF_DISTANCE 4)
(def PERCENT_TOP_CELLS 12)
(def TOP_SCORE_DELTA 200)
(def TURNS_TO_START_CRASHING 8)

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

(def MIN_CRASH_FOR_HALITE 350)
(def CRASH_TURNS_LEFT 25)
(def DELTA_CARRY 500)
(def FOUR_PLAYER_HALITE_TO_CRASH 250)

(defn ram-makes-sense?
  "Returns true if I should even consider ramming."
  [world]
  true)

(defn get-cost-to-ram
  "Returns the cost for the ship to ram another one."
  [world ship cell]
  (let [current-cell (get-location world ship STILL)]
    (+
       ; (:halite ship)
       (* MOVE_COST (:halite current-cell))
       (* GATHER_AMOUNT (+ (:halite current-cell) (get-bonus current-cell)))
       (if (two-player? world)
         (get-inspire-delta-by-move world ship cell)
         (if (or (little-halite-left? world MIN_CRASH_FOR_HALITE) (< (:turns-left world) CRASH_TURNS_LEFT))
           0
           (/ (:total-halite world) (:total-ship-count world)))))))

(defn ram-makes-sense-based-on-carry-capacity?
  "Returns true if it makes sense."
  [my-carry-amount other-carry-amount my-ship their-ship]
  (and (> my-carry-amount
          (+ (:halite their-ship) (get-bonus their-ship)))
       (> my-carry-amount
          (+ (:halite my-ship) (get-bonus my-ship)))))
       ; (> my-carry-amount (+ DELTA_CARRY other-carry-amount))))

(def NUM_BAN_TURNS 7)

(defn should-ram-aggressive?
  "Returns true if I should intentionally try to crash into an enemy."
  [world my-ship cell]
  (let [{:keys [my-id]} world
        ship-in-cell (:ship cell)
        their-ship (when (and ship-in-cell
                              (not= my-id (:owner ship-in-cell))
                              (not= GHOST (:owner ship-in-cell)))
                     ship-in-cell)]
    (when their-ship
      (log "should-ram?: my-ship " (:id my-ship) "their ship" (:id their-ship)))
    (and their-ship
         (not (at-enemy-dropoff? world cell))
         (or
             (let [[my-carry-amount other-carry-amount] (get-six-range-carrying-capacity
                                                         world cell my-ship their-ship)
                   cost-to-ram (get-cost-to-ram world my-ship cell)]
               (and
                    ; (> (:halite their-ship) (:halite my-ship))
                    (> my-carry-amount other-carry-amount)
                    (< cost-to-ram (* GATHER_AMOUNT (+ (:halite their-ship) (get-bonus their-ship))))
                    (least-halite-ramming-ship? world cell my-ship)
                    (or (two-player? world)
                        (and (ram-makes-sense-based-on-carry-capacity?
                              my-carry-amount other-carry-amount my-ship their-ship)
                             (> (:halite their-ship) (:halite my-ship))
                             (> (:halite their-ship) FOUR_PLAYER_HALITE_TO_CRASH)))))))))

(defn all-out-ram?
  "Returns true if I should get really aggressive with ramming."
  [world]
  (let [{:keys [total-halite total-ship-count turns-left]} world]
    (or (two-player? world))))
        ; (little-halite-left? world)
        ; (<= turns-left CRASH_TURNS_LEFT))))

(defn should-ram?
  "Returns true if I should intentionally try to crash into an enemy."
  [world my-ship cell]
  (if (all-out-ram? world)
    (should-ram-aggressive? world my-ship cell)
  ; (when (ram-makes-sense? world)
    (let [{:keys [my-id]} world
          ship-in-cell (:ship cell)
          their-ship (when (and ship-in-cell
                                (not= my-id (:owner ship-in-cell))
                                (not= GHOST (:owner ship-in-cell)))
                       ship-in-cell)]
      (when their-ship
        (log "should-ram?: my-ship " (:id my-ship) "their ship" (:id their-ship)))
      (and their-ship
           (not (at-enemy-dropoff? world cell))
           (and (or (two-player? world)
                    (let [[my-carry-amount other-carry-amount] (get-two-range-carrying-capacity
                                                                world cell my-ship their-ship)]
                      (> my-carry-amount other-carry-amount)))

                (let [[my-carry-amount other-carry-amount] (get-six-range-carrying-capacity
                                                            world cell my-ship their-ship)
                      cost-to-ram (get-cost-to-ram world my-ship cell)]
                  (and
                       ;; (> (:halite their-ship) (:halite my-ship))
                       (> my-carry-amount other-carry-amount)
                       (or (and (two-player? world)
                                (or (:inspired cell)
                                    (> (:halite their-ship) (:halite my-ship)))
                                (> my-carry-amount (+ other-carry-amount (:halite my-ship))))
                           (> my-carry-amount (+ other-carry-amount cost-to-ram)))
                       (< cost-to-ram (min my-carry-amount
                                           (+ (:halite their-ship)
                                              (get-bonus their-ship)
                                              (get-bonus my-ship))))
                       (least-halite-ramming-ship? world cell my-ship)
                       (or
                           (two-player? world)
                           (and (ram-makes-sense-based-on-carry-capacity?
                                 my-carry-amount other-carry-amount my-ship their-ship))))))))))

(def RAM_DANGER_MIN_HALITE 0)

(defn ram-danger?
  "Returns true if my ship might get rammed."
  [world ship cell]
  (let [{:keys [total-halite total-ship-count turns-left my-id]} world]
    (and ship
         (> turns-left TURNS_TO_START_CRASHING)
         (> (:dropoff-distance cell) 0)
         (>= (:halite ship) RAM_DANGER_MIN_HALITE)
         (let [surrounding-cells (get-surrounding-cells world cell)
               enemies-in-reach (keep #(when (and %
                                                  (not= GHOST (:id %))
                                                  (not= my-id (:owner %))
                                                  (can-move? world %))
                                                   ; (< (:halite %) (:halite ship)))
                                         %)
                                      (map :ship surrounding-cells))
               enemy-ship (:ship cell)
               enemy-in-cell (when (and enemy-ship
                                        (not= GHOST (:id ship))
                                        (not= my-id (:owner ship)))
                               enemy-ship)
               enemies-in-reach (if enemy-in-cell
                                  (conj enemies-in-reach enemy-in-cell)
                                  enemies-in-reach)]
           ; (log "CDD: turn" (:turn world) "cell" (dissoc cell :neighbors) "enemies-in-reach" enemies-in-reach "for ship" ship)
           (and (or (surrounded-on-three-sides-and-four-player? world cell)
                    (seq enemies-in-reach))
                (or
                    (let [nearby-ships (get-six-range-ships world cell)
                          my-id (-> world :my-player :player-id)
                          my-ships (filter #(= my-id (:owner %)) nearby-ships)
                          my-carry-amount (- (reduce + (map #(get-capacity %) my-ships)) (:halite ship))
                          other-ships (remove #(= my-id (:owner %)) nearby-ships)
                          ; other-carry-amount (- (reduce + (map #(get-capacity %) other-ships)) (:halite their-ship))                      other-ships (remove #(= my-id (:owner %)) nearby-ships)
                          other-carry-amount (reduce + (map #(get-capacity %) other-ships))]
                      ; (log "Ram danger check for ship" ship "and cell" (dissoc cell :neighbors) "my ship count" (count my-ships)
                           ; "other ship count" (- (count nearby-ships) (count my-ships))]
                      (or (at-enemy-dropoff? world cell)
                          ; (>= (count other-ships) (count my-ships))
                          (and (< my-carry-amount other-carry-amount)
                               (or (surrounded-on-three-sides-and-four-player? world cell)
                                   (< (apply min MAX_HALITE_CARRY (map :halite enemies-in-reach))
                                      (:halite ship))))))
                    (and
                         ;; (not (two-player? world))
                         (let [nearby-ships (get-two-range-ships world cell)
                               my-id (-> world :my-player :player-id)
                               my-ships (filter #(= my-id (:owner %)) nearby-ships)
                               my-carry-amount (- (reduce + (map #(get-capacity %) my-ships)) (:halite ship))
                               other-ships (remove #(= my-id (:owner %)) nearby-ships)
                               ; other-carry-amount (- (reduce + (map #(get-capacity %) other-ships)) (:halite their-ship))                      other-ships (remove #(= my-id (:owner %)) nearby-ships)
                               other-carry-amount (reduce + (map #(get-capacity %) other-ships))]
                           ; (log "Ram danger check for ship" ship "and cell" (dissoc cell :neighbors) "my ship count" (count my-ships)
                                ; "other ship count" (- (count nearby-ships) (count my-ships))]
                           (or
                               ; (>= (count other-ships) (count my-ships))
                               (at-enemy-dropoff? world cell)
                               (and (< my-carry-amount other-carry-amount)
                                    (or (surrounded-on-three-sides-and-four-player? world cell)
                                        (< (apply min MAX_HALITE_CARRY (map :halite enemies-in-reach))
                                           (:halite ship)))))))))))))

(defn safe-location?
  "Returns true if I can move to the location without crashing."
  [world ship location]
  (let [cell (get (:cells world) (select-keys location [:x :y]))
        other-ship (:ship cell)
        safe? (or (and (<= (:turns-left world) TURNS_TO_START_CRASHING)
                       (= 0 (:dropoff-distance cell)))
                  (and (not (ram-danger? world ship cell))
                       (or (and (nil? other-ship)
                                (<= (:dropoff-distance cell) (:turns-left world)))
                           (should-ram? world ship cell))))]
    safe?))

(defn safe-ignoring-ghost-ships?
  "Returns true if I can move the location without crashing into my own ships."
  [world ship location]
  (let [cell (get (:cells world) (select-keys location [:x :y]))]
    (or (and (nil? (:ship cell))
             (<= (:dropoff-distance cell) (:turns-left world))
             (not (ram-danger? world ship cell)))
        (and (< (:turns-left world) TURNS_TO_START_CRASHING)
             (= 0 (:dropoff-distance cell)))
        (and (ghost-ship? (:ship cell))
             (not (at-enemy-dropoff? world cell))
                 ; (not (ram-danger? world ship cell)))
             (<= (:dropoff-distance cell) (:turns-left world))))))

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

(defn should-spawn?
  "Returns true if player can spawn a turtle."
  [world shipyard constants]
  (when (and (enough-spawn-halite? world)
             (safe-location? world nil shipyard))
    (let [{:keys [turn last-dropoff-turn players my-player cells num-players total-halite
                  total-ship-count total-other-ship-halite last-spawn-turn width turns-left]} world
          my-ship-count (count (:ships my-player))
          my-num-dropoffs (inc (count (:dropoffs my-player)))
          amount-i-can-steal (get-steal-amount world)
          my-share (/ (+ amount-i-can-steal total-halite) (inc my-ship-count))]
      (or (and (two-player? world)
               (<= my-ship-count (- total-ship-count my-ship-count))
               (> turns-left (+ 20 TURNS_TO_START_CRASHING)))
          (and (< turn last-spawn-turn)
               (or (> (/ (+ amount-i-can-steal total-halite) total-ship-count)
                      (get-in min-per-spawn-ship [num-players width]))
                   (> my-share (* 1.25 num-players (get-in min-per-spawn-ship [num-players width])))))))))

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
               (>= (:halite ship) BACK_TO_GATHER_AMOUNT))
          (and (>= (:halite ship) BACK_TO_GATHER_AMOUNT)
               (<= (:dropoff-distance cell) MIN_DROPOFF_DISTANCE)))
    :dropoff
    :collect))

(defn get-top-cell-target
  "Returns the target to move after."
  [world ship]
  (let [{:keys [width height top-cells uninspired-cells]} world
        cells (if (:motivated ship) top-cells uninspired-cells)
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

(defn get-collect-move
  "Returns a move to collect as much halite as possible."
  [world ship]
  (let [directions (if (= 0 (:dropoff-distance (get-location world ship STILL)))
                       SURROUNDING_DIRECTIONS
                       ALL_DIRECTIONS)
        surrounding-cells (map #(assoc (get-location world ship %) :direction %) directions)
        ram-cell (first (filter #(and
                                      ;; (not (ghost-ship? (:ship %)))
                                      (should-ram? world ship %))
                                surrounding-cells))]
    (if ram-cell
      (do (log "I am going to ram with ship " ship "and cell" (select-keys ram-cell [:x :y]))
          (flog world ram-cell (format "Ramming with ship %d" (:id ship)) :green)
          (assoc ram-cell :ship ship :reason "Ramming ship."))
      (let [safe-cells (filter #(safe-location? world ship %) surrounding-cells)
            safe-cells (if (and (empty? safe-cells) (two-player? world))
                         ; (filter #(only-other-ships? world ship %) surrounding-cells)
                         (filter #(safe-ignoring-ghost-ships? world ship %) surrounding-cells)
                         safe-cells)]
        (if (:target ship)
          (let [best-direction (get-best-direction world ship (:target ship) safe-cells)
                best-direction (or best-direction STILL)]
            (log "Target is " (:target ship) "and best direction" best-direction)
            (flog world (:target ship) (format "Ship %d existing target" (:id ship)) :orange)
            {:ship ship
             :target (:target ship)
             :direction best-direction
             :reason (str "Moving to target" (dissoc (:target ship) :neighbors))})
          (let [current-cell (get-location world ship STILL)
                nearby-cells (for [location (conj (-> current-cell :neighbors :inspiration) current-cell)
                                   :let [cell (get-location world location STILL)]
                                   :when (safe-location? world ship cell)
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
                     (some #{STILL} (map :direction safe-cells))
                     (>= mined-this-turn (:last-turn-gain target)))
              {:ship ship
               :direction STILL
               :reason "collect more from current cell than last turn gain of target."}
              (if target
                (let [best-direction (get-best-gather-direction world ship target safe-cells)
                      best-direction (or best-direction STILL)]
                  (log "Nearby Target is " (dissoc target :neighbors) "and best direction" best-direction)
                  {:ship ship
                   :direction best-direction
                   :reason (str "Moving to best target in my nearby cells" (dissoc target :neighbors))})
                ;; Need to choose a new target
                (let [target (get-top-cell-target world ship)
                      best-direction (get-best-gather-direction world ship target safe-cells)
                      best-direction (or best-direction STILL)]
                  (log "Target is " target "and best direction" best-direction)
                  (flog world target (format "Chose new target for %d" (:id ship)) :yellow)
                  {:ship (assoc ship :target target)
                   :target target
                   :direction best-direction
                   :reason (str "There were no good targets so I picked" (dissoc target :neighbors))})))))))))

(defn get-dropoff-move
  "Returns a move towards a dropoff site."
  [world ship]
  (let [banned-cells (:banned-cells world)
        surrounding-cells (for [direction ALL_DIRECTIONS]
                            (assoc (get-location world ship direction) :direction direction))
        allowed-cells (remove #(banned-cell? % banned-cells) surrounding-cells)
        safe-cells (filter #(safe-location? world ship %) allowed-cells)
        best-choice (->> safe-cells
                         (map #(assoc % :cost (+ (* 0.1 MOVE_COST (:halite %))
                                                 (get-inspire-delta-by-move world ship %)
                                                 (- (* 2000 (/ 1 (+ 0.5 (:dropoff-distance %))))))))
                         (sort (compare-by :cost asc :dropoff-distance asc))
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
                                                   (- (* 2000 (/ 1 (+ 0.5 (:dropoff-distance %))))))))
                           (sort (compare-by :cost asc :dropoff-distance asc))
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
        {:ship ship :direction STILL :reason "dropoff couldn't find a good best choice"}))))

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
                  updated-cells (add-ship-to-cell (:cells world) ship location)
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
                             :cells updated-cells
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
  (let [surrounding-cells (get-two-range-cells world cell)]
    [(+ (* 1.25 (+ (get-bonus cell) (:halite cell)))
        (reduce + (map #(+ (:halite %) (get-bonus %)) surrounding-cells)))
     ; (+ (* 4 (+ (get-bonus cell) (:halite cell))
     ;     (reduce + (map #(+ (:halite %) (get-bonus %)) surrounding-cells))))]))
     (+ (* 1.25 (:halite cell))
        (reduce + (map :halite surrounding-cells)))]))

(defn decorate-cells
  "Adds dropoff distance and a score to each cell. Handles multiple shipyards and choose the min distance."
  [world cells-to-update shipyards]
  (let [dropoffs shipyards
        {:keys [width height]} world]
    (into {}
      (for [cell cells-to-update
            :let [min-distance (first (sort (map #(distance-between width height cell %) dropoffs)))
                  [score uninspired-score] (score-cell world cell)
                  enemy-side-count (get-surrounded-enemy-count world cell)]]
                  ; uninspired-score (- uninspired-score (* 100 min-distance))]]
        [(select-keys cell [:x :y]) (assoc cell
                                           :dropoff-distance min-distance
                                           :score score
                                           :uninspired-score uninspired-score
                                           :surrounded-enemy-count enemy-side-count)]))))

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
  (let [{:keys [cells width height ship-location-map]} world
        num-cells-to-return (Math/floor (* width height pct 0.01))
        cells (vals cells)
        cells (remove #(get ship-location-map (select-keys % [:x :y]))
                            ; (< (:halite %) 1000))
                      cells)
        ; best-cells (take num-cells-to-return (sort (compare-by :score desc) cells))
        best-cells (if (two-player? world)
                     cells
                     (filter #(safe-location? world {:halite 1000} %) cells))
        ; cells (filter #(safe-location? world {assoc % :halite 1000} %)
        ;               cells)]

        ; cells (filter #(> (:halite %) 150)
        ;               (vals cells))
        best-cells cells]
    [(take num-cells-to-return (sort (compare-by :score desc) best-cells))
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

(def MAX_REWINDS 30)

(defn unwind-collisions-old
  "Any time there is a collision try to back out moves until there is no longer a collision."
  [world moves]
  (loop [iteration 0
         updated-world world
         updated-moves moves]
    (let [colliding-ships (get-colliding-ships updated-world updated-moves)]
      (log "Turn" (:turn world) "Iteration " iteration "Colliding ships are" (mapv :id colliding-ships))
      (if (or (empty? colliding-ships) (>= iteration MAX_REWINDS))
        [updated-world updated-moves]
        (let [{:keys [world moves]} (remove-moves-with-collisions
                                     updated-world updated-moves colliding-ships)
              all-hitters (set (keep :hitter colliding-ships))
              ; _ (log "All hitter ids" (map :id all-hitters))
              ; _ (log "All hitters" all-hitters)
              all-causes (set (keep :cause colliding-ships))
              ; _ (log "All causes" (map :id all-causes))
              all-pre-causes (set (keep :pre-cause colliding-ships))
              ; _ (log "All pre-causes" (map :id all-pre-causes))
              ; _ (log "All causes" all-causes)
              all-hitters (set/difference all-hitters all-causes all-pre-causes)
              all-causes (set/difference all-causes all-pre-causes)
              ; _ (log "Set difference" (map :id all-hitters))
              {world :world new-moves :moves} (get-moves-and-world world (concat all-hitters all-causes all-pre-causes))
             ; _ (log "Old moves (which should have removed new move IDs)." (map generate-move-command moves))
              next-round-of-moves (concat moves new-moves)]
          ; (log "CDD: New moves:" new-moves)
          ; (log "CDD: New moves with collisions" (filter :collision new-moves))
          ; (log "CDD: All remaining moves with collisions" (filter :collision next-round-of-moves))
          ; (log "New moves:" (map generate-move-command new-moves))
          (recur (inc iteration) world next-round-of-moves))))))

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

(defn -main
  "Main"
  [& args]
  (when (= "-log" (first args))
    (reset! log-stuff true)
    (init-flog))
  (let [constants (json/parse-string (read-line))
        [num-players my-id] (map #(Integer/parseInt %)
                                 (string/split (read-line) #" "))
        shipyards (doall (for [player (range num-players)
                               :let [[id x y] (string/split (read-line) #" ")]]
                           {:player-id (Integer/parseInt id)
                            :x (Integer/parseInt x)
                            :y (Integer/parseInt y)}))
        my-shipyard (first (filter #(= my-id (:player-id %)) shipyards))
        other-shipyards (remove #(= my-id (:player-id %)) shipyards)
        [width-str height-str] (string/split (read-line) #" ")
        width (Integer/parseInt width-str)
        height (Integer/parseInt height-str)
        cells (load-cells height)
        initial-world {:height height :width width :cells cells}
        cells (map #(add-neighbors initial-world %) (vals cells))
        cells (build-cell-map cells)
        cells (decorate-cells initial-world
                              (vals cells)
                              [my-shipyard])
        last-turn (total-turns height width)
        last-spawn-turn (* last-turn (get last-spawn-turn-pct num-players))
        last-dropoff-turn (* last-turn LAST_TURN_DROPOFF_PCT)]
    (println bot-name)
    (log constants)
    (log "Num players" num-players)
    (log "My player id" my-id)
    (log "Shipyards" (pr-str shipyards))
    (log (format "Width %s, height %s" width height))
    (loop [cells cells
           last-round-ships nil
           last-round-other-player-ships nil
           last-dropoff-location nil
           banned-cells nil]
      (let [
            turn (Integer/parseInt (read-line))
            _ (log "Turn" turn)
            ; _ (log "Last round ships" last-round-ships)
            turns-left (- last-turn turn)
            players (doall (for [i (range num-players)]
                             (load-player)))
            my-player (first (filter #(= my-id (:player-id %)) players))
            other-players (remove #(= my-id (:player-id %)) players)
            updated-cells (load-updated-cells)
            cells (merge-updated-cells cells updated-cells)
            total-halite (reduce + (map :halite (vals cells)))
            total-ship-count (reduce + (map #(count (:ships %))
                                            players))
            total-ship-count (inc total-ship-count)
            total-other-ship-halite (- (reduce + (map :halite (mapcat :ships players)))
                                       (reduce + (map :halite (:ships my-player))))
            enemy-dropoffs (concat other-shipyards (mapcat :dropoffs other-players))
            world {:width width :height height :players players :my-player my-player :cells cells
                   :turn turn :last-spawn-turn last-spawn-turn :turns-left turns-left
                   :last-dropoff-turn last-dropoff-turn :my-shipyard my-shipyard
                   :num-players num-players :my-ship-count (count (:ships my-player))
                   :total-halite total-halite :total-ship-count total-ship-count
                   :enemy-dropoffs enemy-dropoffs :total-other-ship-halite total-other-ship-halite
                   :my-id my-id}
            ship-location-map (build-ship-location-map world (> turns-left TURNS_TO_START_CRASHING))
            world (assoc world :ship-location-map ship-location-map)
            ; update-inspiration-cells (find-potentially-updated-inspiration-cells world old-ship-locations)
            ; nearby-cells (map #(inspire-cell world % ship-location-map) nearby-cells)
            ; nearby-cells (build-cell-map (map #(inspire-cell world % ship-location-map) nearby-cells))

            other-players (remove #(= (:player-id %) (:player-id my-player))
                                  (:players world))
            other-player-ships (mapcat :ships other-players)
            changed-locations (get-changed-locations-for-ships last-round-other-player-ships
                                                               other-player-ships)
            potential-locations (set (mapcat #(get-locations-in-inspiration-range world %)
                                             changed-locations))

            ; _ (log "Changed locations are:" changed-locations)
            ; _ (log "Potential locations are:" potential-locations)
            inspire-update-cells (map #(inspire-cell world % ship-location-map) potential-locations)
            cells (combine-cells inspire-update-cells cells)
            world (assoc world :cells cells)
            world (if (> turns-left TURNS_TO_START_CRASHING)
                    (predict-enemy-ship-locations world ship-location-map)
                    world)
            score-potential-locations (mapcat #(get-locations-in-inspiration-range world %)
                                              updated-cells)
            score-potential-locations (set (concat score-potential-locations
                                                   (mapcat #(get-locations-in-inspiration-range world %)
                                                           potential-locations)))
            ; _ (log "Locations that could have a different score are:" (pprint/pprint score-potential-locations logger))
            score-potential-cells (map #(get-location world % STILL) score-potential-locations)
            updated-cell-map (decorate-cells world
                                             score-potential-cells
                                             (conj (:dropoffs my-player) my-shipyard))
            optimized-cells (merge cells updated-cell-map)

            ; original-cells (decorate-cells {:height height :width width :cells cells}
            ;                                (vals cells)
            ;                                (conj (:dropoffs my-player) my-shipyard))
            ;
            ; differences (for [cell (vals cells)
            ;                   :let [optimized-cell (get optimized-cells (select-keys cell [:x :y]))
            ;                         original-cell (get original-cells (select-keys cell [:x :y]))]
            ;                   :when (not= optimized-cell original-cell)]
            ;               (str "Cells" (select-keys cell [:x :y]) "was a problem with score:"
            ;                    (:score optimized-cell) "Vs score:" (:score original-cell)))
            ;               ; (data/diff optimized-cell original-cell))
            ;
            ; _ (log "Differences are: " differences)
            ; _ (log "Differences are: " (pprint/pprint differences logger))

            cells optimized-cells
            ; _ (when (= turn 6)
            ;     (System/exit 1))
            _ (doseq [cell (filter :inspired (vals cells))]
                (flog world (select-keys cell [:x :y]) "Inspired")) ;:yellow))

            ; _ (log "Turn " turn "Inspired cells are:" (map #(select-keys % [:x :y])
            ;                                                (filter :inspired (vals cells))))
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
            world (assoc world
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
            dropoff-locations (choose-dropoff-locations world dropoff-location)
            dropoff-location (first dropoff-locations)
            _ (when dropoff-location
                (flog world dropoff-location (format "Dropoff location selected - score: %s, uninspired score: %s."
                                                     (str (:score dropoff-location))
                                                     (str (:uninspired-score dropoff-location)))
                                             :blue))
            changed-dropoff? (not= (select-keys last-dropoff-location [:x :y])
                                   (select-keys dropoff-location [:x :y]))
            world (if changed-dropoff?
                    (unassign-dropoff-moves world last-dropoff-location)
                    world)
            world (assoc world :dropoff-locations dropoff-locations)
            world (remove-bad-targets world last-dropoff-location)
            my-player (:my-player world)
            build-dropoff? (should-build-dropoff? world)
            halite-to-save (if build-dropoff?
                             (- DROPOFF_COST (apply max 500 (map :halite dropoff-locations)))
                             0)
            world (assoc world :reserve halite-to-save)
            ; world (if (> turns-left TURNS_TO_START_CRASHING)
            ;         (predict-enemy-ship-locations world ship-location-map)
            ;         world)
            ; _ (doseq [cell (filter #(ghost-ship? (:ship %))
            ;                        (vals (:cells world)))]
            ;     (flog world cell "GHOST" :white))
            ; _ (log "After predictions here are the enemy ships on the map"
            ;        (filterv #(not= (:player-id my-player) (:owner %))
            ;                 (->> world
            ;                      :cells
            ;                      vals
            ;                      (keep :ship))))
            banned-cells (remove-one-turn-from-banned-cells world banned-cells)
            _ (doseq [cell (keys banned-cells)]
                (flog world cell "Banned cell" :green))
            world (assoc world :banned-cells banned-cells)
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
            ; surrounded-ships (filter #(surrounded-ship? world % ship-location-map) other-ships)
            ; other-ships (get-my-ships-that-can-move (concat stuck-ships surrounded-ships) my-player)
            ; other-ships (remove #(= (:id dropoff-ship) (:id %)) other-ships)
            ; other-ships (map (fn [ship]
            ;                    (assoc ship :mode (get-mode ship
            ;                                                (get-location world ship STILL)
            ;                                                (:turns-left world))))
            ;                  other-ships)
            ; collecting-ships (sort (compare-by :cell-halite desc)
            ;                        (filter #(= :collect (:mode %)) other-ships))
            collecting-ships (sort (compare-by :halite desc)
                                   (filter #(= :collect (:mode %)) other-ships))
            ; collecting-ships (sort (compare-by :dropoff-distance desc :cell-halite desc)
            ;                        (filter #(= :collect (:mode %)) other-ships))
            dropoff-ships (sort (compare-by :dropoff-distance asc :halite desc)
                                (filter #(= :dropoff (:mode %)) other-ships))

            ;; XXX not sure if I want to do this yet
            other-ships (sort (compare-by :halite desc) other-ships)
            {:keys [world moves]} (get-moves-and-world world (concat stuck-ships
                                                                     ;; surrounded-ships
                                                                     ; other-ships))
                                                                     dropoff-ships
                                                                     collecting-ships))
            [world moves] (if (> (:turns-left world) TURNS_TO_START_CRASHING)
                            (unwind-collisions-old world moves)
                            [world moves])
            world (update-world-for-dropoff-ship world dropoff-ship)
            spawn-command (get-spawn-command (should-spawn? world my-shipyard constants))
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
        ; (log "Surrounded ships:" surrounded-ships)
        (log "Cells with ships are:" (map #(select-keys % [:x :y])
                                          (filter :ship (-> world :cells vals))))
        (log "Turn " turn "logging all moves.")
        (println (str spawn-command " "
                      dropoff-command " "
                      (string/join " " (map generate-move-command moves))))
        (recur cells-without-ships last-round-ships other-player-ships dropoff-location
               (:banned-cells world))))))

(comment
 (let [world {:cells {{:x 1 :y 2} {:x 1 :y 3 :halite 55}
                      {:x 3 :y 5} {:x 3 :y 5 :ship "oh my" :halite 122}}}
       locations {"n" {:x 1 :y 2}
                  "s" {:x 3 :y 5}}]
   (keep (fn [[k v]]
           (when (safe-location? world nil v)
             k))
         locations)))
