(ns hlt.collisions
  "All the collision logic."
  (:require
   [hlt.utils :refer :all]
   [hlt.game :refer :all]
   [hlt.custom-game :refer :all]))

;; TODO Get these parameters back in the main code
(def MIN_CRASH_FOR_HALITE 350)
(def CRASH_TURNS_LEFT 25)
(def FOUR_PLAYER_HALITE_TO_CRASH 250)
(def TURNS_TO_START_CRASHING 8)
(def RAM_DANGER_MIN_HALITE 0)

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
       (* MOVE_COST (get current-cell :halite 0))
       (* GATHER_AMOUNT (+ (get current-cell :halite 0) (get-bonus current-cell)))
       (if (two-player? world)
         (get-inspire-delta-by-move world ship cell)
         (if (or (little-halite-left? world MIN_CRASH_FOR_HALITE)
                 (< (:turns-left world) CRASH_TURNS_LEFT))
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

(defn should-ram-old?
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

(defn ram-danger-old?
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


(defn get-value-of-a-ship
  "Note - I will want to change this at some point to take into account the average halite gained
  per turn divided by the number of ships. For now just base on total halite on the map and
  the number of ships."
  [world]
  (/ (:total-halite world) (:total-ship-count world)))

(defn get-cost-of-wasted-turn
  "Returns the cost of wasting a turn by staying STILL."
  [world]
  (/ (:total-halite world) (:total-ship-count world)))

(def odds-of-collision-still 0.2)
(def odds-of-collision-moving 0.1)

(defn get-ships-in-cells
  "Returns ships from cells."
  [world locations]
  (keep #(get (:ship-location-map world) (select-keys % [:x :y]))
        locations))

(defn play-out-fight
  "Figure out who will collect what from a fight based on looking out up to 7 turns..."
  [world cell my-ship their-ship]
  (let [;; Include the inspiration bonus. Assume if I'm inspired they are too.
        multiplier (if (inspired-cell? world cell) 3 1)
        dropped-halite (* (+ (:halite my-ship) (:halite their-ship))
                          multiplier)]
    (loop [remaining-halite dropped-halite
           iteration 1
           total-halite 0
           delta-carry 0
           ships (remove #(or (= (:id my-ship) (:id %))
                              (= (:id their-ship) (:id %)))
                         (get-ships-in-cells world (conj (get-in cell [:neighbors 1]) cell)))]
      (if (or (<= remaining-halite 20)
              (> iteration 5))
        {:total total-halite :leftover remaining-halite}
        (let [ships (remove ghost-ship? ships)
              [my-carry-capacity other-carry-capacity] (get-carrying-capacity world ships)
              delta (long (- my-carry-capacity other-carry-capacity))
              delta (+ delta-carry delta)
              gathered-one-round (if (= 0 delta)
                                   0
                                   (min (Math/abs delta) (* GATHER_AMOUNT multiplier remaining-halite)))
              delta-carry (if (> delta 0)
                            (- delta gathered-one-round)
                            (+ delta gathered-one-round))
              ; delta (if (> (Math/abs delta) remaining-halite)
              ;         (if (> delta 0)
              ;           remaining-halite
              ;           (* -1 remaining-halite))
              ;         delta)
              remaining-halite (- remaining-halite gathered-one-round)
              total-halite (if (> delta 0)
                             (long (+ total-halite gathered-one-round))
                             (long (- total-halite gathered-one-round)))]
          (log (format (str "Turn: %d, iteration %d, cell %s, ships: %s, my-carry-capacity: %d, "
                            "other-carry-capacity: %d, delta %d, delta-carry %d, gathered-one-round %d, "
                            "total-halite (aka score): %d")
                       (:turn world)
                       iteration
                       (select-keys cell [:x :y])
                       (pr-str ships)
                       my-carry-capacity
                       other-carry-capacity
                       (long delta)
                       (long delta-carry)
                       (long gathered-one-round)
                       (long total-halite)))
          (recur remaining-halite
                 (inc iteration)
                 total-halite
                 delta-carry
                 (get-ships-in-cells world (get-in cell [:neighbors (inc iteration)]))))))))

(defn spoils-of-war
  "Returns a value of the net halite change from a collision. This could get complicated to
  track correctly, but is probably one of the most important things to get right in this game."
  [world my-ship cell their-ship]
  (if (= 0 (:dropoff-distance cell))
    (:halite their-ship)
    (let [{:keys [total leftover]} (play-out-fight world cell my-ship their-ship)]
      ; (flog world cell (str "SOW: total" total "leftover" leftover))
      (if (<= 0 leftover)
        total
        (let [my-closest-base (get-closest-cell world cell (-> world :my-player :dropoffs))
              their-closest-base (get-closest-cell world cell (:enemy-dropoffs world))]
          (if (< (:distance my-closest-base) (:distance their-closest-base))
            (+ total (* 0.5 leftover))
            (if (= (:distance my-closest-base) (:distance their-closest-base))
              total
              (- total (* 0.5 leftover)))))))))

(defn score-collision
  "Provides a score for choosing the given cell with regards to collision. Positive means
  an expected gain from a collision and negative means an expected loss from a collision."
  [world ship other-ship cell]
  ; (log "Turn " (:turn world) "SC: cell is" (select-keys cell [:ship :x :y]))
  (if (= (:my-id world) (:owner other-ship))
    (- (* -2 (get-value-of-a-ship world)) (:halite ship) (:halite other-ship))
    (let [collisions-odds (if (ghost-ship? (:ship cell))
                            0.1
                            0.2)
          battle-diff (spoils-of-war world ship cell other-ship)
          battle-diff (- (+ battle-diff (:halite other-ship)) (:halite ship))]
      ; (if (two-player? world)
      (* collisions-odds battle-diff))))
        ; (- (* collisions-odds battle-diff)
           ; (get-value-of-a-ship world)))))

(defn ram-danger-new?
  ""
  [world ship cell]
  (when (and ship (not= 0 (:dropoff-distance cell)))
    (let [enemy-ships (filter #(and (not= (:my-id world) (:owner %))
                                    (not (ghost-ship? %)))
                              (get-ships-in-cells world (get-in cell [:neighbors 1])))
          scores (map #(int (score-collision world ship % cell)) enemy-ships)
          low-score (when (seq scores)
                      (apply min scores))
          low-score (if (or (nil? low-score)
                            (two-player? world))
                      low-score
                      (- low-score (* 0.50 (get-value-of-a-ship world))))]

      ; (flog world cell "RD: enemy-ships" enemy-ships)
      (when (seq scores)
        (if (< low-score 0)
          (flog-color world cell (str "Scores:" (pr-str scores)) :brown)
          (flog-color world cell (str "Scores:" (pr-str scores)) :yellow))
        (< low-score 100)))))

(defn should-ram-new?
  [world ship cell]
  (when ship
    (when-let [other-ship (:ship cell)]
      (let [score (int (score-collision world ship other-ship cell))
            score (if (two-player? world)
                    score
                    (- score (get-value-of-a-ship world)))]
        (when (> score 0)
          (flog-color world cell (str "Score:" score) :green))
        (> score 20)))))

(def ram-danger-function
  "Maps players and map size to the version of the ram function to use."
  {2 {32 ram-danger-old?
      40 ram-danger-old?
      48 ram-danger-old?
      56 ram-danger-old?
      64 ram-danger-old?}
   4 {32 ram-danger-new?
      40 ram-danger-new?
      48 ram-danger-new?
      56 ram-danger-new?
      64 ram-danger-new?}})

(def should-ram-function
  "Maps players and map size to the version of the ram function to use."
  {2 {32 should-ram-old?
      40 should-ram-old?
      48 should-ram-old?
      56 should-ram-old?
      64 should-ram-old?}
   4 {32 should-ram-new?
      40 should-ram-new?
      48 should-ram-new?
      56 should-ram-old?
      64 should-ram-old?}})

(defn should-ram?
  [world ship cell]
  (let [{:keys [num-players width]} world
        should-ram-fn (get-in should-ram-function [num-players width])]
    (should-ram-fn world ship cell)))

(defn ram-danger?
  [world ship cell]
  (let [{:keys [num-players width]} world
        ram-danger-fn (get-in ram-danger-function [num-players width])]
    (ram-danger-fn world ship cell)))

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
