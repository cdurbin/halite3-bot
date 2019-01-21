(ns hlt.custom-game
  "Functions that should never change - just rely on some custom structures
  specific to the way I structure my bot and the world."
  (:require
   [clojure.set :as set]
   [hlt.utils :refer :all]
   [hlt.game :refer :all]
   [hlt.map-analysis :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

(def GHOST "ghost")

(defn get-surrounding-cells
  "Returns cells within one range of my location."
  [world location]
  (map #(get (:cells world) %)
       (if (:neighbors location)
         (get-in location [:neighbors 1])
         (get-in (get-cell world location) [:neighbors 1]))))

(defn get-two-range-cells
  "Returns cells within two range of my location."
  [world cell]
  (when (nil? (get-in cell [:neighbors 1]))
    (/ 0 0))
  (map #(get (:cells world) %)
       (concat (get-in cell [:neighbors 1])
               (get-in cell [:neighbors 2]))))

(defn get-three-range-cells
  "Returns cells within three range of my location."
  [world cell]
  (when (nil? (get-in cell [:neighbors 1]))
    (/ 0 0))
  (map #(get (:cells world) %)
       (concat (get-in cell [:neighbors 1])
               (get-in cell [:neighbors 2])
               (get-in cell [:neighbors 3]))))

(defn get-four-range-cells
  "Returns cells within three range of my location."
  [world cell]
  (when (nil? (get-in cell [:neighbors 1]))
    (/ 0 0))
  (map #(get (:cells world) %)
       (concat (get-in cell [:neighbors 1])
               (get-in cell [:neighbors 2])
               (get-in cell [:neighbors 3])
               (get-in cell [:neighbors 4]))))

(defn get-five-range-cells
  "Returns cells within seven range of my location."
  [world cell]
  (when (nil? (get-in cell [:neighbors 1]))
    (/ 0 0))
  (map #(get (:cells world) %)
       (concat (get-in cell [:neighbors 1])
               (get-in cell [:neighbors 2])
               (get-in cell [:neighbors 3])
               (get-in cell [:neighbors 4])
               (get-in cell [:neighbors 5]))))

(defn get-seven-range-cells
  "Returns cells within seven range of my location."
  [world cell]
  (when (nil? (get-in cell [:neighbors 1]))
    (/ 0 0))
  (map #(get (:cells world) %)
       (concat (get-in cell [:neighbors 1])
               (get-in cell [:neighbors 2])
               (get-in cell [:neighbors 3])
               (get-in cell [:neighbors 4])
               (get-in cell [:neighbors 5])
               (get-in cell [:neighbors 6])
               (get-in cell [:neighbors 7]))))

(defn get-one-range-ships
  "Returns any ships nearby a location (within one range)."
  [world location]
  (let [cell (get-cell world location)
        nearby-locations (get-in cell [:neighbors 1])]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-two-range-ships
  "Returns any ships nearby a location (within two range)."
  [world location]
  (let [cell (get-cell world location)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-three-range-ships
  "Returns any ships nearby a location (within three range)."
  [world location]
  (let [cell (get-cell world location)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2])
                                 (get-in cell [:neighbors 3]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-four-range-ships
  "Returns any ships nearby a location (within four range)."
  [world location]
  (let [cell (get-cell world location)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2])
                                 (get-in cell [:neighbors 3])
                                 (get-in cell [:neighbors 4]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-five-range-ships
  "Returns any ships nearby a location (within four range)."
  [world location]
  (let [cell (get-cell world location)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2])
                                 (get-in cell [:neighbors 3])
                                 (get-in cell [:neighbors 4])
                                 (get-in cell [:neighbors 5]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-six-range-ships
  "Returns any ships nearby a location (within inspiration range)."
  [world location]
  (let [cell (get-cell world location)
        nearby-locations (concat (get-in cell [:neighbors 1])
                                 (get-in cell [:neighbors 2])
                                 (get-in cell [:neighbors 3])
                                 (get-in cell [:neighbors 4])
                                 (get-in cell [:neighbors 5])
                                 (get-in cell [:neighbors 6]))]
    (keep (fn [loc]
            (get (:ship-location-map world) loc))
          (conj nearby-locations (select-keys location [:x :y])))))

(defn get-seven-range-ships
  "Returns any ships nearby a location (within inspiration range)."
  [world location]
  (let [cell (get-cell world location)
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

(defn get-ship-bonus
  "Returns the bonus for a ship which can suppress inspiration and a cell."
  [ship cell]
  (if (:motivated ship)
    (get-bonus cell)
    0))

(defn little-halite-left?
  "Returns true if there is not much halite left per ship."
  [world amount]
  (<= (/ (:total-halite world) (:total-ship-count world)) amount))

(defn least-halite-ramming-ship?
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

(defn get-six-range-carrying-capacity
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

(defn get-carrying-capacity
  "Returns a tuple of my nearby carrying capacity and enemy nearby carrying capacity."
  [world ships]
  (let [my-id (:my-id world)
        my-ships (filter #(= my-id (:owner %)) ships)
        my-carry-amount (reduce + (map #(get-capacity %) my-ships))
        other-ships (remove #(= my-id (:owner %)) ships)
        other-carry-amount (reduce + (map #(get-capacity %) other-ships))]
    [my-carry-amount other-carry-amount]))

(defn get-inspire-opponent-count
  "Returns the number of ships with a different owner within inspiration range."
  [world ship]
  (let [{:keys [ship-location-map]} world
        cell (get-cell world ship)
        locations (-> cell :neighbors :inspiration)
        ships (keep ship-location-map locations)]
    (count (filter #(not= (:owner ship) (:owner %)) ships))))

(defn get-extra-halite
  "Returns the extra halite a ship would receive if the cell was inspired next round."
  [world ship]
  (let [capacity (get-capacity ship)
        cell (get-cell world ship)
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
  (if-not (two-player? world)
    0
    (let [original-cell (get-cell world ship)]
      (if (= (select-keys original-cell [:x :y]) (select-keys cell [:x :y]))
        0
        (let [cost (- (get-opponent-extra-inspire-by-move world original-cell cell)
                      (get-opponent-lost-inspire-by-move world original-cell cell))]
          (if (not= 0 cost)
            (do (log "Turn:" (:turn world) "There's a move to reduce the inspiration - cost was" cost "and cell" (select-keys cell [:x :y]))
                (flog world cell (str "Inspiration cost:" cost) :brown)
                (if (> (Math/abs ^Integer cost) 5)
                  cost
                  0))
            cost))))))

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
        current-cell (get-cell world ship)]
    (when (or (nil? chosen-cell) (>= (:dropoff-distance chosen-cell) (:dropoff-distance current-cell)))
      (let [cells (get-two-range-cells world current-cell)
            surrounding-ships (filter :ship cells)]
        (some? (first (filter #(and (not= my-id (-> % :ship :owner))
                                    (< (:dropoff-distance %) (:dropoff-distance current-cell)))
                              surrounding-ships)))))))

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
  (>= (:surrounded-enemy-count cell) 3))

(defn surrounded-on-three-sides-and-four-player?
  "Returns true if I could be surrounded on three sides and four player game."
  [world cell]
  (if (two-player? world)
    false
    (>= (:surrounded-enemy-count cell) 3)))

(defn generate-move-command
  "Returns a move command"
  [move]
  (log "Move for ship " (-> move :ship :id) "was direction" (:direction move) "because:" (:reason move))
  (format (str "%s %d %s") MOVE (-> move :ship :id) (:direction move)))

(comment
 (let [abc {:a {:b 1 :c 2} :d 1}]
   (update abc :a dissoc :c)))

(defn add-ship-to-cell
  "Adds a ship to a given cell."
  [world ship location]
  (log "ASTC: " (:id ship) "at location" (select-keys location [:x :y]))
  (let [old-location (select-keys ship [:x :y])
        new-location (select-keys location [:x :y])
        change? (not= old-location new-location)
        perform-dissoc? (and change?
                             (= (:id ship) (get-in world [:updated-ship-location-map old-location :id])))]
    (as-> world world
          (assoc-in world [:cells new-location :ship] ship)
          (if perform-dissoc?
            (update world :updated-ship-location-map dissoc old-location)
            world)
          (if change?
            (assoc-in world [:updated-ship-location-map new-location] ship)
            world))))

(defn get-stuck-ships
  "Returns ships that don't have enough halite to move."
  [world ships dropoff-location]
  (remove #(can-move? world %) ships))

(defn get-my-ships-that-can-move
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
  [spawn?]
  (if spawn?
    GENERATE
    ""))

(defn get-dropoff-command
  "Generate dropoff-command"
  [dropoff-ship]
  (if dropoff-ship
    (format "%s %d" CONSTRUCT (:id dropoff-ship))
    ""))

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

(defn add-ghost-to-cell
  "Adds a ghost ship to a given cell if there is no ship there."
  [ship world cell]
  (let [ship-at-location (get (:ship-location-map world) (select-keys cell [:x :y]))]
    (if (and ship-at-location (not= (:id ship) (:id ship-at-location)))
      world
      (assoc-in world
                [:cells (select-keys cell [:x :y]) :ship]
                (assoc ship :id GHOST)))))

(defn build-ship-location-map
  "Hashmap with keys of x, y to a value of the ship."
  [world all-ships?]
  (let [{:keys [players turns-left]} world
        ships (if all-ships?
                (mapcat :ships players)
                (-> world :my-player :ships))]
    (into {}
          (for [ship ships]
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
              (let [cell (get-cell updated-world base)]
                (if (and (:ship cell)
                         (not= (-> cell :ship :owner) my-id))
                  (do (log "I'm ignoring a ship on my base on turn " (:turn updated-world))
                      (assoc-in updated-world [:cells (select-keys cell [:x :y]) :ship] nil))
                  updated-world)))
            world
            bases)))

(defn get-closest-cell
  "Returns the closest cell to a target from a list of cells."
  [world target cells]
  (let [distance-maps (for [cell cells
                            :let [distance (distance-between (:width world) (:height world) cell target)]]
                        {:distance distance :cell cell})]
    (first (sort (compare-by :distance asc) distance-maps))))

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

(defn inspired-cell?
  "Returns true if the cell is inspired."
  [world cell]
  (let [{:keys [my-id ship-location-map]} world]
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
  [world location]
  (let [cell (get-cell world location)
        inspired? (inspired-cell? world cell)]
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
  (let [cell (get-cell world location)]
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

(defn add-neighbors
  "Adds all cells within seven distance of a cell."
  [world cell]
  (let [one-range-neighbors (get-locations world cell exactly-one-range-possibilities)
        two-range-neighbors (get-locations world cell exactly-two-range-possibilities)
        three-range-neighbors (get-locations world cell exactly-three-range-possibilities)
        four-range-neighbors (get-locations world cell exactly-four-range-possibilities)
        five-range-neighbors (get-locations world cell exactly-five-range-possibilities)
        six-range-neighbors (get-locations world cell exactly-six-range-possibilities)
        seven-range-neighbors (get-locations world cell exactly-seven-range-possibilities)
        inspiration-neighbors (concat one-range-neighbors two-range-neighbors three-range-neighbors
                                      four-range-neighbors)
        neighbors {1 one-range-neighbors
                   2 two-range-neighbors
                   3 three-range-neighbors
                   4 four-range-neighbors
                   5 five-range-neighbors
                   6 six-range-neighbors
                   7 seven-range-neighbors
                   :inspiration inspiration-neighbors}]
    (assoc cell :neighbors neighbors)))

(defn add-quadrant
  "Adds quadrant info to a cell."
  [world cell]
  (assoc cell :quadrant (cell->quadrant (select-keys cell [:x :y]))))

(defn build-world-for-round
  "Builds up the world for the current round."
  [world last-round-other-player-ships turns-to-start-crashing]
  (let [{:keys [last-turn my-id cells other-shipyards width height my-shipyard num-players
                last-spawn-turn last-dropoff-turn quadrant-distances valid-quadrants]} world
        turn (Integer/parseInt (read-line))
        _ (log "Turn" turn)
        ; _ (log "Last round ships" last-round-ships)
        turns-left (- last-turn turn)
        players (doall (for [i (range num-players)
                             :let [player (load-player)]]
                         (update player :ships (fn [ships]
                                                 (map #(assoc % :quadrant (cell->quadrant (select-keys % [:x :y])))
                                                      ships)))))
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
               :my-id my-id :updated-cells updated-cells :other-players other-players
               :quadrant-distances quadrant-distances :valid-quadrants valid-quadrants}
        ship-location-map (build-ship-location-map world (> turns-left turns-to-start-crashing))
        world (assoc world :ship-location-map ship-location-map :updated-ship-location-map ship-location-map)
        other-player-ships (mapcat :ships other-players)
        changed-locations (get-changed-locations-for-ships last-round-other-player-ships
                                                           other-player-ships)
        potential-locations (set (mapcat #(get-locations-in-inspiration-range world %)
                                         changed-locations))
        ; _ (log "Changed locations are:" changed-locations)
        ; _ (log "Potential locations are:" potential-locations)
        inspire-update-cells (map #(inspire-cell world %) potential-locations)
        cells (combine-cells inspire-update-cells cells)]
    ;     quadrant-metrics (get-quadrant-metrics (assoc world :cells cells))]
    ; (log "QM:" quadrant-metrics)
    (assoc world :updated-cells updated-cells :potential-locations potential-locations
          :other-player-ships other-player-ships :cells cells)))
          ; :quadrant-metrics quadrant-metrics)))

(defn unwind-collisions
  "Any time there is a collision try to back out moves until there is no longer a collision."
  [world moves moves-fn max-rewinds]
  (loop [iteration 0
         updated-world world
         updated-moves moves]
    (let [colliding-ships (get-colliding-ships updated-world updated-moves)]
      (log "Turn" (:turn world) "Iteration " iteration "Colliding ships are" (mapv :id colliding-ships))
      (if (or (empty? colliding-ships) (>= iteration max-rewinds))
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
              {world :world new-moves :moves} (moves-fn world (concat all-hitters all-causes all-pre-causes))
             ; _ (log "Old moves (which should have removed new move IDs)." (map generate-move-command moves))
              next-round-of-moves (concat moves new-moves)]
          ; (log "CDD: New moves:" new-moves)
          ; (log "CDD: New moves with collisions" (filter :collision new-moves))
          ; (log "CDD: All remaining moves with collisions" (filter :collision next-round-of-moves))
          ; (log "New moves:" (map generate-move-command new-moves))
          (recur (inc iteration) world next-round-of-moves))))))
