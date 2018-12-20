(ns hlt.core
  "Core functionality for Halite 3."
  (:require
   [cheshire.core :as json]
   [clojure.set :as set]
   [clojure.data :as data]
   [clojure.string :as string]
   [clojure.pprint :as pprint]
   [hlt.utils :refer :all]
   [hlt.game :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def GHOST "ghost")

(def DROP_OFF_AMOUNT 950)
(def DROP_OFF_AMOUNT_EARLY 800)
(def DROP_OFF_AMOUNT_LATE 550)
(def NUM_EARLY_SHIPS 9)
(def BACK_TO_GATHER_AMOUNT 650)
(def MIN_DROPOFF_DISTANCE 4)
; (def MAX_HALITE_BURN_COLLECT 8)
; (def MAX_HALITE_BURN_DROPOFF 8)
(def PERCENT_TOP_CELLS 12)
(def TOP_SCORE_DELTA 200)
(def TURNS_TO_START_CRASHING 8)
; (def LAST_TURN_SPAWN_PCT 0.70)

(def last-spawn-turn-pct
  {2 0.52
   4 0.70})

(def LAST_TURN_DROPOFF_PCT 0.80)

(def MIN_SHIPS_BEFORE_IGNORE_GHOST 45)

(def MAX_TURNS_EVALUATE 5)
(def CELL_HALITE_LEFT_BEHIND 0.75)

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

(def ship-counter
  "Used to decide whether a ship should ignore inspiration in its move calculations."
  (atom 0))

; (defn NEARBY_SHIP_RANGE 6)

(defn get-six-range-ships
  "Returns any ships nearby a location (within inspiration range)."
  [world location]
  (let [cell (get-location world location STILL)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2])
                                 (get-in cell [:neighbors 3])
                                 (get-in cell [:neighbors 4])
                                 (get-in cell [:neighbors 5])
                                 (get-in cell [:neighbors 6]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-one-range-ships
  "Returns any ships nearby a location (within inspiration range)."
  [world location]
  (let [cell (get-location world location STILL)
        nearby-locations (get-in cell [:neighbors 1])]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-two-range-ships
  "Returns any ships nearby a location (within inspiration range)."
  [world location]
  (let [cell (get-location world location STILL)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-seven-range-ships
  "Returns any ships nearby a location (within inspiration range)."
  [world location]
  (let [cell (get-location world location STILL)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2])
                                 (get-in cell [:neighbors 3])
                                 (get-in cell [:neighbors 4])
                                 (get-in cell [:neighbors 5])
                                 (get-in cell [:neighbors 6])
                                 (get-in cell [:neighbors 7]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn most-ships-around-cell?
  "Returns true if I have more ships around a cell (or there are no nearby ships)."
  [world cell]
  (let [my-id (:my-id world)
        nearby-ships (get-six-range-ships world cell)
        my-ships (filter #(= my-id (:owner %)) nearby-ships)
        my-ship-count (count my-ships)
        other-ship-count (- (count nearby-ships) (count my-ships))]
    (when (or (= 0 other-ship-count)
              (> my-ship-count other-ship-count))
      (log (format "Turn %d - cell %s I had %d ships and they had %d ships."
                   (:turn world)
                   (select-keys cell [:x :y])
                   my-ship-count
                   other-ship-count)))
    (or (= 0 other-ship-count)
        (> my-ship-count other-ship-count))))

(defn not-terrible-dropoff?
  "Returns true if I have more ships around a cell (or there are no nearby ships)."
  [world cell]
  (let [my-id (:my-id world)
        nearby-ships (get-six-range-ships world cell)
        my-ships (filter #(= my-id (:owner %)) nearby-ships)
        my-ship-count (count my-ships)
        other-ship-count (- (count nearby-ships) (count my-ships))]
    (or (> my-ship-count 5)
        (< (- other-ship-count my-ship-count) 3))))

(def MIN_CRASH_FOR_HALITE 350)
(def CRASH_TURNS_LEFT 25)
(def DELTA_CARRY 500)
(def FOUR_PLAYER_HALITE_TO_CRASH 250)

(defn get-ship-bonus
  "Returns the bonus for a ship which can suppress inspiration and a cell."
  [ship cell]
  (if (:motivated ship)
    (get-bonus cell)
    0))

(defn little-halite-left?
  "Returns true if there is not much halite left per ship."
  [world]
  (<= (/ (:total-halite world) (:total-ship-count world)) MIN_CRASH_FOR_HALITE))

(defn ram-makes-sense?
  "Returns true if I should even consider ramming."
  [world]
  true)
  ; (let [{:keys [total-halite total-ship-count turns-left]} world]
  ;   (or (two-player? world)
  ;       (little-halite-left? world)
  ;       (<= turns-left CRASH_TURNS_LEFT))))

(defn best-ramming-ship?
  "Returns true if the ship is the best potential ship to ram a target with."
  [world cell ship]
  (let [{:keys [ship-location-map my-id]} world
        surrounding-cells (get-surrounding-cells world cell)
        surrounding-ships (keep #(get ship-location-map (select-keys % [:x :y]))
                                surrounding-cells)
        my-surrounding-ships (filter #(and (= my-id (:owner %))
                                           (can-move? world %))
                                     surrounding-ships)]
    (<= (:halite ship) (apply min MAX_HALITE_CARRY (map :halite my-surrounding-ships)))))

(defn get-nearby-carrying-capacity
  "Returns a tuple of my nearby carrying capacity and enemy nearby carrying capacity."
  [world cell my-ship their-ship]
  (let [my-id (:my-id world)
        nearby-ships (get-six-range-ships world cell)
        my-ships (filter #(= my-id (:owner %)) nearby-ships)
        my-collecting-ships (filter #(<= (:halite %) 750) my-ships)
        my-carry-amount (- (reduce + (map #(get-capacity %) my-collecting-ships)) (get-capacity my-ship))
        other-ships (remove #(= my-id (:owner %)) nearby-ships)
        other-carry-amount (- (reduce + (map #(get-capacity %) other-ships)) (get-capacity their-ship))]
    [my-carry-amount other-carry-amount]))

(defn get-two-range-carrying-capacity
  "Returns a tuple of my nearby carrying capacity and enemy nearby carrying capacity."
  [world cell my-ship their-ship]
  (let [my-id (:my-id world)
        nearby-ships (get-two-range-ships world cell)
        my-ships (filter #(= my-id (:owner %)) nearby-ships)
        my-collecting-ships (filter #(<= (:halite %) 750) my-ships)
        my-carry-amount (- (reduce + (map #(get-capacity %) my-collecting-ships)) (get-capacity my-ship))
        other-ships (remove #(= my-id (:owner %)) nearby-ships)
        other-carry-amount (- (reduce + (map #(get-capacity %) other-ships)) (get-capacity their-ship))]
    [my-carry-amount other-carry-amount]))

(defn get-inspire-opponent-count
  "Returns the number of ships with a different owner within inspiration range."
  [world ship]
  (let [{:keys [ship-location-map]} world
        cell (get-location world ship STILL)
        locations (-> cell :neighbors :inspiration)
        ships (keep ship-location-map locations)]
    (count (filter #(not= (:owner ship) (:owner %)) ships))))

(defn get-extra-halite
  "Returns the extra halite a ship would receive if the cell was inspired next round."
  [world ship]
  (let [capacity (get-capacity ship)
        cell (get-location world ship STILL)
        halite-gained-this-turn (* GATHER_AMOUNT (+ (:halite cell) (get-bonus cell)))
        halite-next-round (* CELL_HALITE_LEFT_BEHIND (:halite cell))
        capacity (- capacity halite-gained-this-turn)]
    (if (> capacity 0)
      (min capacity (* GATHER_AMOUNT 2 halite-next-round))
      0)))

(defn get-opponent-extra-inspire-by-move
  "Returns the extra inspire that all opponents gain by moving to this cell."
  [world original-cell cell]
  (let [{:keys [ship-location-map my-id]} world
        potential-inspire-add-locations (get-in original-cell [:neighbors 5])
        potential-inspire-add-locations (filter (fn [loc]
                                                  (some (set [loc])
                                                        (get-in cell [:neighbors 4])))
                                                potential-inspire-add-locations)
        ships (keep ship-location-map potential-inspire-add-locations)
        opponent-ships (remove #(= my-id (:owner %)) ships)
        now-inspired-ships (filter #(= 1 (get-inspire-opponent-count world %)) opponent-ships)]
    (reduce + (map #(get-extra-halite world %) now-inspired-ships))))

(defn get-opponent-lost-inspire-by-move
  "Returns the inspire that all opponents will lose by moving to this cell."
  [world original-cell cell]
  (let [{:keys [ship-location-map my-id]} world
        potential-inspire-subtract-locations (get-in cell [:neighbors 5])
        potential-inspire-subtract-locations (filter (fn [loc]
                                                       (some (set [loc])
                                                             (get-in original-cell [:neighbors 4])))
                                                     potential-inspire-subtract-locations)
        ships (keep ship-location-map potential-inspire-subtract-locations)
        opponent-ships (remove #(= my-id (:owner %)) ships)
        not-inspired-ships (filter #(= 2 (get-inspire-opponent-count world %)) opponent-ships)]
    (reduce + (map #(get-extra-halite world %) not-inspired-ships))))

(defn get-inspire-delta-by-move
  "Returns the amount of inspire this move would add or subtract."
  [world ship cell]
  ; (if-not (two-player? world)
  ;   0
  (let [original-cell (get-location world ship STILL)
        cost (- (get-opponent-extra-inspire-by-move world original-cell cell)
                (get-opponent-lost-inspire-by-move world original-cell cell))]
    (if (not= 0 cost)
      (do (log "Turn:" (:turn world) "There's a move to reduce the inspiration - cost was" cost "and cell" (select-keys cell [:x :y]))
          (flog world cell (str "Inspiration cost:" cost) :brown)
          (if (> (Math/abs ^Integer cost) 5)
            cost
            0))
      cost)))

; (defn get-cost-to-ram
;   "Returns the cost for the ship to ram another one."
;   [world ship cell]
;   (let [current-cell (get-location world ship STILL)]
;     (+ (* MOVE_COST (:halite current-cell))
;        (* GATHER_AMOUNT (+ (:halite current-cell) (get-bonus current-cell)))
;        (get-inspire-delta-by-move world ship cell))))
;
; (defn ram-makes-sense-based-on-carry-capacity?
;   "Returns true if it makes sense."
;   [my-carry-amount other-carry-amount their-ship]
;   (> my-carry-amount
;     (+ (max DELTA_CARRY (:halite their-ship)) other-carry-amount)))

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
         (if (or (little-halite-left? world) (< (:turns-left world) CRASH_TURNS_LEFT))
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

(defn banned-cell?
  "Returns true if the cell has been banned for dropoff ships."
  [cell cells]
  (let [cell-location (select-keys cell [:x :y])]
    (get cells cell-location)))

(defn ghost-ship?
  "Returns true if there isn't a ship in this cell on this turn."
  [ship]
  (= (:id ship) GHOST))

(defn blocked-by-enemy?
  "Returns true if my target direction is blocked by an enemy ship."
  [world chosen-cell ship]
  (let [my-id (:my-id world)
        current-cell (get-location world ship STILL)]
    (when (or (nil? chosen-cell) (>= (:dropoff-distance chosen-cell) (:dropoff-distance current-cell)))
      (let [cells (get-cells-within-two-range world ship)
            surrounding-ships (filter :ship cells)]
        (some? (first (filter #(and (not= my-id (-> % :ship :owner))
                                    (< (:dropoff-distance %) (:dropoff-distance current-cell)))
                              surrounding-ships)))))))

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
             (let [[my-carry-amount other-carry-amount] (get-nearby-carrying-capacity
                                                         world cell my-ship their-ship)
                   cost-to-ram (get-cost-to-ram world my-ship cell)]
               (and
                    ; (> (:halite their-ship) (:halite my-ship))
                    (> my-carry-amount other-carry-amount)
                    (< cost-to-ram (* GATHER_AMOUNT (+ (:halite their-ship) (get-bonus their-ship))))
                    (best-ramming-ship? world cell my-ship)
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

                (let [[my-carry-amount other-carry-amount] (get-nearby-carrying-capacity
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
                       (best-ramming-ship? world cell my-ship)
                       (or
                           (two-player? world)
                           (and (ram-makes-sense-based-on-carry-capacity?
                                 my-carry-amount other-carry-amount my-ship their-ship))))))))))

(defn get-surrounded-enemy-count
  "Returns number of enemies that could surround this cell."
  [world cell]
  (let [my-id (:my-id world)
        enemy-in-reach (for [s-cell (get-surrounding-cells world cell)
                             :let [ships (get-one-range-ships world s-cell)
                                   ; _ (log "Two range ships are:" ships)
                                   enemy-ships (filter #(and (not= my-id (:owner %))
                                                             (not (ghost-ship? %)))
                                                       ships)]
                             :when (seq enemy-ships)]
                         enemy-ships)
        ret-value
                  (apply max 0
                    (map count
                         (for [i (nth enemy-in-reach 0 [{:id :fake}])
                               j (nth enemy-in-reach 1 [{:id :fake}])
                               k (nth enemy-in-reach 2 [{:id :fake}])
                               l (nth enemy-in-reach 3 [{:id :fake}])]
                           (remove #(= :fake %)
                                   (set [(:id i) (:id j) (:id k) (:id l)])))))]
    ret-value))

; (defn surrounded-on-three-sides?
;   "Returns true if I could be surrounded on three sides."
;   [world cell]
;   (let [surrounded? (>= (:surrounded-enemy-count cell) 3)]
;     (when surrounded?
;       (flog-color world cell (str "Surrounded on" (:surrounded-enemy-count cell) " sides") :brown))
;     surrounded?))

(defn surrounded-on-three-sides?
  "Returns true if I could be surrounded on three sides."
  [world cell]
  (if (two-player? world)
    false
    (>= (:surrounded-enemy-count cell) 3)))

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
           (and (or (surrounded-on-three-sides? world cell)
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
                               (or (surrounded-on-three-sides? world cell)
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
                                    (or (surrounded-on-three-sides? world cell)
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

(defn only-other-ships?
  "Returns true if I can move to the location without crashing into my own ships."
  [world ship location]
  (let [cell (get (:cells world) (select-keys location [:x :y]))
        my-ships (-> world :my-player :ships)]
    (or (and (nil? (:ship cell))
             (<= (:dropoff-distance cell) (:turns-left world))
             (not (ram-danger? world ship cell)))
        (and (< (:turns-left world) TURNS_TO_START_CRASHING)
             (= 0 (:dropoff-distance cell)))
        (and (nil? (some (set [(-> cell :ship :id)]) (map :id my-ships)))
             (<= (:dropoff-distance cell) (:turns-left world))
             (not (ram-danger? world ship cell))))))

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

(defn enough-spawn-halite?
  "Returns true if I have enough halite to spawn a ship."
  [world constants]
  (let [{:keys [my-player reserve]} world]
    (>= (:halite my-player) (+ (get constants "NEW_ENTITY_ENERGY_COST") reserve))))

(defn can-spawn?
  "Returns true if player can spawn a turtle."
  [world shipyard constants]
  (when (and (enough-spawn-halite? world constants)
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

(defn generate-move-command
  "Returns a move command"
  [move]
  (log "Move for ship " (-> move :ship :id) "was direction" (:direction move) "because:" (:reason move))
  (format (str "%s %d %s") MOVE (-> move :ship :id) (:direction move)))

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

             ; (>= (:halite ship) DROP_OFF_AMOUNT)

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

; (defn decorate-move-cost
;   "Adds a movement cost for moving to a cell."
;   [world ship cell]
;   (assoc cell :cost (+ (* MOVE_COST (:halite cell))
;                        (get-inspire-delta-by-move world ship cell))))

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
                ; nearby-cells (for [location (conj (:larger-neighbors current-cell) current-cell)])
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

(defn add-ship-to-cell
  "Adds a ship to a given cell."
  [cells ship location]
  (log "ASTC: " (:id ship) "at location" (select-keys location [:x :y]))
  (assoc-in cells [(select-keys location [:x :y]) :ship] ship))

(defn get-stuck-ships
  "Returns ships that don't have enough halite to move."
  [world ships dropoff-location]
  (remove #(can-move? world %) ships))
  ; (remove #(should-move? world % dropoff-location) ships))

(defn get-ships-that-can-move
  "Returns ships that can move."
  [stuck-ships my-player]
  (remove #(some (set [(:id %)]) (map :id stuck-ships))
          (:ships my-player)))

(defn get-colliding-ship
  "Returns a ship that I am colliding with at the current location."
  [world location]
  (get-in world [:cells (select-keys location [:x :y]) :ship]))

(defn get-spawn-command
  "Generate spawn command."
  [world my-shipyard constants]
  (if (can-spawn? world my-shipyard constants)
    GENERATE
    ""))

(defn get-dropoff-command
  "Generate dropoff-command"
  [dropoff-ship]
  (if dropoff-ship
    (format "%s %d" CONSTRUCT (:id dropoff-ship))
    ""))

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
  (let [surrounding-cells (get-cells-within-two-range world cell)]
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

(defn build-cell-map
  "Returns a cell map from the given values."
  [cells]
  (into {}
    (for [cell cells]
      [(select-keys cell [:x :y]) cell])))

(defn build-moves-map
  "Returns a moves map with the key as the ID of the moving ship."
  [moves]
  (into {}
    (for [move moves]
      [(-> move :ship :id) move])))

(defn get-should-motivate
  "Returns true if I should be motivated or false otherwise."
  [ship]
  true)
  ; (let [new-count (inc @ship-counter)
  ;       motivated? (> new-count 1)
  ;       new-count (if (= 12 new-count)
  ;                   0
  ;                   new-count)]
  ;   (reset! ship-counter new-count)
  ;   motivated?))

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
        motivated (:motivated last-round-ship)
        motivated (if (nil? motivated)
                    (get-should-motivate ship)
                    motivated)
        motivated (if can-ignore-motivate? motivated true)
        mode (if (and (= :dropoff last-mode)
                      (> (:halite ship) BACK_TO_GATHER_AMOUNT))
               :dropoff
               (get-mode ship cell (:turns-left world) drop-off-amount))
        num-surrounding-ships 0]
        ; surrounding-sites (get-surrounding-cells world ship)
        ; num-surrounding-ships (count (keep #(get ship-location-map (select-keys % [:x :y]))
        ;                                    surrounding-sites))]
    (log "Ship " ship "decided on mode" mode "and motivated:" motivated)
    (assoc ship
           :mode mode
           :motivated motivated
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
                    surrounding-cells (when (or (little-halite-left? world)
                                                (<= turns-left CRASH_TURNS_LEFT)
                                                (and (> my-ship-count MIN_SHIPS_TO_RAM_GHOST)
                                                     (< width 50)))
                                        (get-surrounding-cells world cell))]
                    ; surrounding-cells (when (can-move? updated-world ship)
                    ;                     (get-surrounding-cells updated-world cell))
                    ; surrounding-cells (get-surrounding-cells updated-world cell)]
                (reduce (fn [updated-world cell]
                          (let [ship-at-location (get ship-location-map (select-keys cell [:x :y]))]
                            (if (and ship-at-location (not= (:id ship) (:id ship-at-location)))
                              updated-world
                              (assoc-in updated-world
                                        [:cells (select-keys cell [:x :y]) :ship]
                                        (assoc ship :id GHOST)))))
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

(defn build-ship-location-map
  "Hashmap with keys of x, y to a value of the ship."
  [world]
  (let [{:keys [players turns-left]} world
        all-ships (if (> turns-left TURNS_TO_START_CRASHING)
                    (mapcat :ships players)
                    (-> world :my-player :ships))]
    (into {}
          (for [ship all-ships]
            [(select-keys ship [:x :y]) ship]))))

(defn surrounded-ship?
  "Returns true if all directions around the ship are occupied."
  [world ship ship-location-map]
  (let [surrounding-sites (get-surrounding-cells world ship)
        num-surrounding-ships (count (keep #(get ship-location-map (select-keys % [:x :y]))
                                           surrounding-sites))]
    (if (= 4 num-surrounding-ships)
      (do
          (log "Ship:" (:id ship) "is surrounded!")
          true)
      false)))

(defn ignore-enemy-ships-on-base
  "Pretend enemy ships on my shipyard or dropoffs don't matter."
  [world]
  (let [{:keys [my-shipyard my-player my-id]} world
        bases (conj (:dropoffs my-player) my-shipyard)]
        ; my-ship-ids (map :id (:ships my-player))]
    (reduce (fn [updated-world base]
              (let [cell (get-location updated-world base STILL)]
                (if (and (:ship cell)
                         (not= (-> cell :ship :owner) my-id))
                  (do (log "I'm ignoring a ship on my base on turn " (:turn updated-world))
                      (assoc-in updated-world [:cells (select-keys cell [:x :y]) :ship] nil))
                  updated-world)))
            world
            bases)))

(defn find-closest-ship
  "Returns the closest ship to given target from a list of ships"
  [world target ships]
  (let [distance-maps (for [ship ships
                            :let [distance (distance-between (:width world) (:height world) ship target)]]
                        {:distance distance :ship ship :halite (:halite ship)})]
    (first (sort (compare-by :distance asc :halite asc) distance-maps))))

(defn find-closest-n-ships
  "Returns the closest n ships to the given target from a list of ships"
  [world target ships n]
  (let [distance-maps (for [ship ships
                            :let [distance (distance-between (:width world) (:height world) ship target)]]
                        {:distance distance :ship ship :halite (:halite ship)})]
    (map :ship (take n (sort (compare-by :distance asc :halite asc) distance-maps)))))

(defn better-cell?
  "Returns true if cell1 is better than cell2."
  [cell1 cell2]
  (> (+ (:halite cell1) (get-bonus cell1))
     (+ (:halite cell2) (get-bonus cell2))))

(defn blank-out-target
  "Returns a world with the target for a ship removed."
  [world ship]
  (let [my-ships (-> world :my-player :ships)
        _ (flog world
                (:target ship)
                (format "Blanking out target %s for ship %d"
                        (select-keys (:target ship) [:x :y])
                        ; (:target ship)
                        (:id ship))
                :purple)
        ship (assoc ship :target nil)
        my-ships (conj (remove-item ship my-ships) ship)]
    (assoc world :my-player (assoc (:my-player world) :ships my-ships))))

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

(def MAX_TARGET_DISTANCE 6)

(defn map-targets-to-ships
  "Gives targets for each of the ships. TBD: Any target with a mapped ship gets removed from top
  cells."
  [world]
  world)
  ; (let [{:keys [top-cells my-player]} world
  ;       my-collecting-ships (filter #(= :collect (:mode %)) (:ships my-player))
  ;       targets (take (Math/floor (/ (count my-collecting-ships) 12)) top-cells)]
  ;   (log "The top targets are: " targets)
  ;   (reduce (fn [updated-world target]
  ;             (let [top-cells (:top-cells updated-world)
  ;                   my-ships (-> updated-world :my-player :ships)
  ;                   my-potential-ships (filter #(and (= :collect (:mode %))
  ;                                                    (nil? (:target %))
  ;                                                    (better-cell? target (get-location world % STILL)))
  ;                                              my-ships)
  ;                   {:keys [ship distance]} (find-closest-ship world target my-potential-ships)]
  ;               ; (log "Find closest ship returned ship" ship "distance" distance)
  ;               (if (and ship (< distance MAX_TARGET_DISTANCE))
  ;                 (let [closest-ship (assoc ship :target target)
  ;                       ; top-cells (remove-item target top-cells)
  ;                       my-ships (conj (remove-item closest-ship my-ships) closest-ship)]
  ;                   ; (log "My updated ships after targets are: " my-ships)
  ;                   (assoc updated-world
  ;                          :top-cells top-cells
  ;                          :my-player (assoc (:my-player updated-world) :ships my-ships)))
  ;                 updated-world)))
  ;           world
  ;           targets)))

(defn inspired-cell?
  "Returns true if the cell is inspired."
  [world cell ship-location-map]
  (let [my-id (:my-id world)]
    (loop [inspire-count 0
           remaining-cells (-> cell :neighbors :inspiration)]
      (let [ship (get ship-location-map (first remaining-cells))
            inspire-count (+ inspire-count (if (and ship (not= (:owner ship) my-id)) 1 0))]
        (if (>= inspire-count INSPIRE_SHIPS_NEEDED)
          true
          (if (empty? (rest remaining-cells))
            false
            (recur inspire-count (rest remaining-cells))))))))

(defn inspire-cell
  "Adds a bonus halite to a cell if the cell would be inspired based on the current enemy ship
  locations."
  [world location ship-location-map]
  (let [cell (get-location world location STILL)
        inspired? (inspired-cell? world cell ship-location-map)]
    (assoc cell :inspired inspired?)))

(defn get-changed-locations-for-ships
  "Returns any locations which have had a ship move into or out of."
  [old-ships new-ships]
  (let [old-locations (set (map #(select-keys % [:x :y]) old-ships))
        new-locations (set (map #(select-keys % [:x :y]) new-ships))
        moved-away-from (set/difference old-locations new-locations)
        moved-to (set/difference new-locations old-locations)]
    (concat moved-away-from moved-to)))

(defn get-locations-in-inspiration-range
  "Returns x and y coords for any location within inspiration range of the current location."
  [world location]
  (let [cell (get-location world location STILL)]
    (conj (-> cell :neighbors :inspiration)
          (select-keys location [:x :y]))))

(defn get-colliding-ships
  "Returns IDs of all my ships that will collide with each other based on the given moves."
  [world moves]
  (let [my-id (:my-id world)]
    (keep (fn [move]
            (when (= my-id (-> move :collision :owner))
              {:hitter (:ship move)
               :cause (:collision move)
               :pre-cause (:pre-collision move)
               :location (:location move)}))
          moves)))

(defn remove-moves-with-collisions
  "Removes any moves that resulted in a collision and updates the world and moves accordingly."
  [world moves colliding-ships]
  (log "RMWC colliding-ships are" colliding-ships)
  (reduce (fn [{:keys [world moves]} collision]
            (let [location (:location collision)
                  relevant-moves (filter #(or (= (-> % :ship :id) (-> collision :hitter :id))
                                              (= (-> % :ship :id) (-> collision :cause :id))
                                              (= (-> % :ship :id) (-> collision :pre-cause :id)))
                                         moves)
                  relevant-locations (map #(select-keys (:location %) [:x :y])
                                          relevant-moves)
                  world (reduce (fn [world location]
                                  (assoc-in world [:cells location :ship] nil))
                                world
                                relevant-locations)
                  ; world (assoc-in world [:cells (select-keys location [:x :y]) :ship] nil)
                  moves (remove #(or (= (-> % :ship :id) (-> collision :hitter :id))
                                     (= (-> % :ship :id) (-> collision :cause :id))
                                     (= (-> % :ship :id) (-> collision :pre-cause :id)))
                                moves)]
              {:world world :moves moves}))
          {:world world :moves moves}
          colliding-ships))

(def MAX_REWINDS 30)

(defn unwind-collisions
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
              _ (log "All hitter ids" (map :id all-hitters))
              ; _ (log "All hitters" all-hitters)
              all-causes (set (keep :cause colliding-ships))
              _ (log "All causes" (map :id all-causes))
              all-pre-causes (set (keep :pre-cause colliding-ships))
              _ (log "All pre-causes" (map :id all-pre-causes))
              ; _ (log "All causes" all-causes)
              all-hitters (set/difference all-hitters all-causes all-pre-causes)
              all-causes (set/difference all-causes all-pre-causes)
              _ (log "Set difference" (map :id all-hitters))
              {world :world new-moves :moves} (get-moves-and-world world (concat all-hitters all-causes all-pre-causes))
             ; _ (log "Old moves (which should have removed new move IDs)." (map generate-move-command moves))
              next-round-of-moves (concat moves new-moves)]
          (log "CDD: New moves:" new-moves)
          (log "CDD: New moves with collisions" (filter :collision new-moves))
          (log "CDD: All remaining moves with collisions" (filter :collision next-round-of-moves))
          ; (log "New moves:" (map generate-move-command new-moves))
          (recur (inc iteration) world next-round-of-moves))))))

(defn add-neighbors
  "Adds all cells within four distance of a cell."
  [world cell]
  (let [one-range-neighbors (get-locations world cell exactly-one-range-possibilities)
        two-range-neighbors (get-locations world cell exactly-two-range-possibilities)
        three-range-neighbors (get-locations world cell exactly-three-range-possibilities)
        four-range-neighbors (get-locations world cell exactly-four-range-possibilities)
        five-range-neighbors (get-locations world cell exactly-five-range-possibilities)
        six-range-neighbors (get-locations world cell exactly-six-range-possibilities)
        seven-range-neighbors (get-locations world cell exactly-seven-range-possibilities)
        ; inspiration-neighbors (concat one-range-neighbors two-range-neighbors three-range-neighbors
        ;                               four-range-neighbors)
        inspiration-neighbors (get-locations-within-four-range world cell)
        neighbors {1 one-range-neighbors
                   2 two-range-neighbors
                   3 three-range-neighbors
                   4 four-range-neighbors
                   5 five-range-neighbors
                   6 six-range-neighbors
                   7 seven-range-neighbors
                   :inspiration inspiration-neighbors}]
    (assoc cell :neighbors neighbors)))

  ; (let [neighbors (get-locations-within-four-range world cell)
  ;       exactly-five-range-neighbors (get-locations-at-five-range world cell)]
  ;   (assoc cell :neighbors neighbors :larger-neighbors (concat neighbors exactly-five-range-neighbors))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dropoff management
(def FIRST_BUILD_DROPOFF_DISTANCE 11)
(def BUILD_DROPOFF_DISTANCE 15)
(def MAX_DROPOFF_LOCATION_DISTANCE 26)
(def NUM_POTENTIAL_DROPOFFS 1)
; (def MIN_DROPOFF_SCORE 12500)
(def MIN_DROPOFF_SCORE 4400)
(def MIN_SHIPS_PER_DROPOFF 10)
(def MIN_SHIPS_FOR_FIRST_DROPOFF 15)

(def min-per-ship-to-build-dropoff
  {2 {32 1410
      40 1410
      48 1410
      56 1110
      64 1110}
   4 {32 380
      40 380
      48 350
      56 330
      64 350}})

(def MAX_MOVE_TO_DROPOFF_DISTANCE 10)

(defn choose-dropoff-locations
  "Given a bunch of choices for dropoff locations. Choose the one(s) that could be the most
  valuable."
  [world last-dropoff-location]
  (let [num-dropoffs (count (-> world :my-player :dropoffs))
        build-dropoff-distance (if (> num-dropoffs 0)
                                 BUILD_DROPOFF_DISTANCE
                                 FIRST_BUILD_DROPOFF_DISTANCE)
        last-dropoff-location (get-location world last-dropoff-location STILL)]
    (if (and last-dropoff-location
             ; (> (:score last-dropoff-location) (- MIN_DROPOFF_SCORE 2000))
             (> (:uninspired-score last-dropoff-location) (- MIN_DROPOFF_SCORE 1000))
             (>= (:dropoff-distance last-dropoff-location) build-dropoff-distance))
      [last-dropoff-location]
      (let [{:keys [top-cells]} world
            nearby-sites (filter #(and (<= build-dropoff-distance
                                           (:dropoff-distance %)
                                           MAX_DROPOFF_LOCATION_DISTANCE)
                                       ; (>= (:score %) MIN_DROPOFF_SCORE)
                                       (>= (:uninspired-score %) MIN_DROPOFF_SCORE)
                                       (not-terrible-dropoff? world %))
                                 top-cells)
            nearby-sites (if (seq nearby-sites)
                           nearby-sites
                           (filter #(and (<= build-dropoff-distance
                                             (:dropoff-distance %))
                                         (>= (:uninspired-score %) MIN_DROPOFF_SCORE)
                                         (not-terrible-dropoff? world %))
                                   top-cells))
            - (log (format "Turn %d potential dropoff locations %s" (:turn world)
                         ; (pr-str (take NUM_POTENTIAL_DROPOFFS (map #(select-keys % [:x :y]) (sort (compare-by :score desc) nearby-sites))))
                           (pr-str (take NUM_POTENTIAL_DROPOFFS (map #(select-keys % [:x :y]) (sort (compare-by :uninspired-score desc) nearby-sites))))))
            ; (take NUM_POTENTIAL_DROPOFFS (sort (compare-by :score desc) nearby-sites))
            dropoffs (take NUM_POTENTIAL_DROPOFFS (sort (compare-by :dropoff-distance asc :uninspired-score desc) nearby-sites))
            ;; TODO work with multiple dropoffs later
            dropoff (first dropoffs)
            {:keys [ship distance]} (when dropoff
                                      (find-closest-ship world dropoff (-> world :my-player :ships)))]
        (when (and ship (< distance MAX_MOVE_TO_DROPOFF_DISTANCE))
          [dropoff])))))

(defn should-build-dropoff?
  "Returns true if it makes sense for me to build a dropoff."
  [world]
  (let [{:keys [turn last-dropoff-turn players my-player cells num-players dropoff-locations width]} world
        total-halite (reduce + (map :halite (vals cells)))
        total-ship-count (reduce + (map #(count (:ships %))
                                        players))
        total-ship-count (inc total-ship-count)
        my-ship-count (count (:ships my-player))
        my-num-dropoffs (inc (count (:dropoffs my-player)))]
    (log "Total halite:" total-halite "total-ship-count" total-ship-count)
    (log "Calculation of halite per ship:" (int (/ total-halite total-ship-count)))
    (log "Turn is " turn "last dropoff-turn is" last-dropoff-turn)
    (and  (< turn last-dropoff-turn)
          (> my-ship-count (+ MIN_SHIPS_FOR_FIRST_DROPOFF
                              (* MIN_SHIPS_PER_DROPOFF (dec my-num-dropoffs))))
          (or (seq dropoff-locations)
              (> (/ total-halite total-ship-count) (get-in min-per-ship-to-build-dropoff [num-players width]))))))

; (def NUM_DROPOFF_SHIPS 6)

(def NEXT_DROPOFF_DISTANCE 5)
(defn assign-dropoff-moves
  "Returns moves to go towards a dropoff. For now just always assume there is one dropoff."
  [world]
  (let [{:keys [my-player dropoff-locations]} world
        location (first dropoff-locations)
        my-collecting-ships (filter #(= :collect (:mode %)) (:ships my-player))
        ; num-dropoff-ships (Math/ceil (/ (inc (count (:ships my-player))) 7))
        num-dropoff-ships 1
        ships (find-closest-n-ships world location my-collecting-ships num-dropoff-ships)
        ships (filter #(>= (:dropoff-distance %) NEXT_DROPOFF_DISTANCE) ships)]
    (log (format "Turn %d Assigning ships %s to target %s" (:turn world) (pr-str (map :id ships)) (select-keys location [:x :y])))
    (reduce (fn [updated-world ship]
              (let [my-ships (-> updated-world :my-player :ships)
                    ship (assoc ship :target location)
                    my-ships (conj (remove-item ship my-ships) ship)]
                (assoc updated-world :my-player (assoc (:my-player updated-world) :ships my-ships))))
            world
            ships)))

(defn unassign-dropoff-moves
  "Remove all moves that were set up to move to a dropoff because either the dropoff is no
  longer valid or has been created."
  [world location]
  (let [my-ships (-> world :my-player :ships)
        dropoff-ships (filter #(= (select-keys location [:x :y])
                                  (select-keys (:target %) [:x :y]))
                              my-ships)]
    (reduce blank-out-target
            world
            dropoff-ships)))

(def FAR_DROPOFF 25)
(def AUTO_BUILD_DROPOFF 1500)

(defn choose-dropoff-ship-orig
  "Returns the ship that should build a dropoff."
  [world ships]
  (let [{:keys [top-cells my-player]} world
        num-dropoffs (count (:dropoffs my-player))
        build-dropoff-distance (if (> num-dropoffs 0)
                                 BUILD_DROPOFF_DISTANCE
                                 FIRST_BUILD_DROPOFF_DISTANCE)
        top-locations (map #(select-keys % [:x :y]) top-cells)
        ships (filter (fn [ship]
                        (and (>= (+ (:halite my-player) (:halite ship) (:cell-halite ship))
                                 DROPOFF_COST)
                             (or (> (:cell-halite ship) AUTO_BUILD_DROPOFF)
                                 (and (not (at-enemy-dropoff? world ship))
                                      (some (set [(select-keys ship [:x :y])])
                                            top-locations)))))
                      ships)
        ships (filter (fn [ship]
                        (or (> (:cell-halite ship) AUTO_BUILD_DROPOFF)
                            (and (or (> (:dropoff-distance ship) FAR_DROPOFF)
                                     (> (:score (get-location world ship STILL))
                                        (* MIN_DROPOFF_SCORE 1.5)))
                                 (not-terrible-dropoff? world ship))))
                      ships)
        furthest-ship (first (sort (compare-by :dropoff-distance desc) ships))
        cell (when furthest-ship
               (get-location world furthest-ship STILL))]
    (when (and furthest-ship
               (> (:dropoff-distance furthest-ship) build-dropoff-distance))
               ; (> (:score cell) MIN_DROPOFF_SCORE))
      furthest-ship)))

(def NEARBY_DROPOFF_HALITE 5000)

(defn enough-nearby-ship-halite
  "Returns true if my ships closeby have enough halite carried to warrant a new dropoff."
  [world cell]
  (let [{:keys [my-id]} world
        nearby-ships (get-seven-range-ships world cell)
        my-nearby-ships (filter #(= my-id (:owner %)) nearby-ships)]
    (>= (reduce + (map :halite my-nearby-ships))
        NEARBY_DROPOFF_HALITE)))

(def REQUIRED_NEARBY_HALITE 5100)

(defn enough-nearby-gather-halite
  "Returns true if there is enought halite to gather to warrant a new dropoff."
  [world cell]
  (let [nearby-cells (get-cells world cell (concat exactly-one-range-possibilities
                                                   exactly-two-range-possibilities
                                                   exactly-three-range-possibilities
                                                   exactly-four-range-possibilities
                                                   exactly-five-range-possibilities
                                                   exactly-six-range-possibilities
                                                   exactly-seven-range-possibilities))]
    (>= (reduce + (map :halite nearby-cells))
        REQUIRED_NEARBY_HALITE)))

; (def USEFUL_DROPOFF_DISTANCE 18)

; (def USEFUL_DROPOFF_DISTANCE FIRST_BUILD_DROPOFF_DISTANCE)
(def USEFUL_DROPOFF_DISTANCE 16)

(defn useful-dropoff-location
  "Returns true if I could use a dropoff here."
  [world]
  (let [{:keys [my-player]} world
        my-ships (:ships my-player)
        potential-ships (filter (fn [ship]
                                  (and (>= (:dropoff-distance ship) USEFUL_DROPOFF_DISTANCE)
                                       (>= (+ (:halite my-player) (:halite ship) (:cell-halite ship))
                                           DROPOFF_COST)
                                       (not (at-enemy-dropoff? world ship))
                                       (enough-nearby-ship-halite world ship)
                                       (enough-nearby-gather-halite world ship)))
                                my-ships)]
    (first (sort (compare-by :dropoff-distance desc) potential-ships))))

(defn choose-dropoff-ship
  "Returns a ship that should build a dropoff."
  [world]
  (let [{:keys [dropoff-locations my-player]} world
        dropoff-locations (map #(select-keys % [:x :y]) dropoff-locations)]
    (if (empty? dropoff-locations)
      (if-let [second-choice (choose-dropoff-ship-orig world (:ships my-player))]
        second-choice
        (useful-dropoff-location world))
      (let [ships (filter (fn [ship]
                            (and (not (at-enemy-dropoff? world ship))
                                 (>= (+ (:halite my-player) (:halite ship) (:cell-halite ship))
                                     DROPOFF_COST)
                                 (some (set [(select-keys ship [:x :y])])
                                       dropoff-locations)))
                          (:ships my-player))]
        (first (sort (compare-by :score desc) ships))))))

(defn update-world-for-dropoff-ship
  "Updates player halite info based on built dropoff."
  [world ship]
  (if (nil? ship)
    world
    (let [player (:my-player world)
          dropoff-cost (- DROPOFF_COST
                          (+ (:halite ship) (:cell-halite ship)))
          halite (- (:halite player) (max 0 dropoff-cost))]
      (assoc world :reserve 0 :my-player (assoc player :halite halite)))))

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
                                                 (get-cells-within-two-range world location)))))]
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
            ship-location-map (build-ship-location-map world)
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
                                       (or (little-halite-left? world)
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
            build-dropoff-distance (if (> (count (:dropoffs my-player)) 0)
                                     BUILD_DROPOFF_DISTANCE
                                     FIRST_BUILD_DROPOFF_DISTANCE)
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
            world (map-targets-to-ships world)
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
            other-ships (get-ships-that-can-move stuck-ships my-player)
            other-ships (remove #(= (:id dropoff-ship) (:id %)) other-ships)
            ; surrounded-ships (filter #(surrounded-ship? world % ship-location-map) other-ships)
            ; other-ships (get-ships-that-can-move (concat stuck-ships surrounded-ships) my-player)
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
                            (unwind-collisions world moves)
                            [world moves])
            world (update-world-for-dropoff-ship world dropoff-ship)
            spawn-command (get-spawn-command world my-shipyard constants)
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
