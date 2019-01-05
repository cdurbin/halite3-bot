(ns hlt.game
  "Functions that should almost never change. Just the fundamental rules of the game."
  (:require
   [clojure.string :as string]
   [cheshire.core :as json]
   [hlt.utils :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
(def NORTH "n")
(def SOUTH "s")
(def EAST "e")
(def WEST "w")
(def STILL "o")
(def GENERATE "g")
(def CONSTRUCT "c")
(def MOVE "m")

(def MOVE_COST 0.1)
(def GATHER_AMOUNT 0.25)
(def CELL_HALITE_LEFT_BEHIND 0.75)
(def MAX_HALITE_CARRY 1000)
(def SHIP_COST 1000)
(def DROPOFF_COST 4000)
(def INSPIRED_BONUS 2)
(def INSPIRE_SHIPS_NEEDED 2)

(def ALL_DIRECTIONS [NORTH SOUTH EAST WEST STILL])
(def SURROUNDING_DIRECTIONS [NORTH SOUTH EAST WEST])

(defn load-player
  "Returns a player's info for the turn."
  []
  (let [[player num-ships num-dropoffs halite] (map #(Integer/parseInt %)
                                                    (string/split (read-line) #" "))
        ships (doall (for [i (range num-ships)
                           :let [[id x y halite] (map #(Integer/parseInt %)
                                                      (string/split (read-line) #" "))]]
                       {:id id :x x :y y :halite halite :owner player}))
        dropoffs (doall (for [i (range num-dropoffs)
                              :let [[id x y] (map #(Integer/parseInt %)
                                                  (string/split (read-line) #" "))]]
                          {:id id :x x :y y}))]
    (log "Dropoffs are: " (pr-str dropoffs))
    {:player-id player
     :num-ships num-ships
     :num-dropoffs num-dropoffs
     :ships ships
     :dropoffs dropoffs
     :halite halite}))

(defn load-cells
  "Loads the cells at the beginning of the game."
  [height]
  (let [map-info (doall
                  (for [row (range height)
                        :let [info (read-line)
                              values (remove string/blank? (string/split info #" "))
                              values (map-indexed (fn [idx v]
                                                    {:halite (Integer/parseInt v) :x idx})
                                                  values)]
                        value values]
                    (merge value {:y row})))]
    (into {}
      (for [cell map-info]
        [(select-keys cell [:x :y]) (assoc cell :inspired false)]))))

(defn normalize
  "Returns a normalized map value."
  [value max-value]
  (mod value max-value))

(defn get-x-and-y
  "Returns {:x x :y y} for the provided reference point and direction."
  [ref-point direction max-x max-y]
  (condp = direction
    STILL (select-keys ref-point [:x :y])
    NORTH {:x (:x ref-point) :y (normalize (dec (:y ref-point)) max-y)}
    SOUTH {:x (:x ref-point) :y (normalize (inc (:y ref-point)) max-y)}
    WEST {:x (normalize (dec (:x ref-point)) max-x) :y (:y ref-point)}
    EAST {:x (normalize (inc (:x ref-point)) max-x) :y (:y ref-point)}))

(defn get-location
  "Returns map information for a particular cell."
  [world ref-point direction]
  (let [{:keys [width height cells]} world
        x-and-y (get-x-and-y ref-point direction width height)]
    ; (log "Get location for " ref-point "and direction" direction "is: " (get cells x-and-y))
    (get cells x-and-y)))

(defn load-updated-cells
  "Loads cell updates for a turn."
  []
  (let [num-updated-cells (Integer/parseInt (read-line))]
    (log "Num num-updated-cells" num-updated-cells)
    (doall (for [i (range num-updated-cells)
                 :let [[x y halite] (map #(Integer/parseInt %)
                                         (string/split (read-line) #" "))]]
             {:x x :y y :halite halite}))))

    ; (log "Updated cells" updated-cells)

(defn merge-updated-cells
  "Merges in the updated cells."
  [original-cell-map updated-cells]
  (reduce (fn [cell-map cell]
            (assoc-in cell-map [(select-keys cell [:x :y]) :halite] (:halite cell)))
          original-cell-map
          updated-cells))

(defn at-dropoff?
  "Returns true if I am at a dropoff."
  [world cell]
  (let [{:keys [my-shipyard my-player]} world
        bases (conj (:dropoffs my-player) my-shipyard)]
    (some? (first (filter #(= (select-keys % [:x :y])
                              (select-keys cell [:x :y]))
                          bases)))))

(defn get-bonus
  "Returns the bonus for a cell."
  [cell]
  (if (:inspired cell)
    (* INSPIRED_BONUS (:halite cell))
    0))

(defn ^Long get-gather-amount
  "Returns the gather amount for a cell."
  [cell]
  (Math/ceil (* GATHER_AMOUNT (+ (:halite cell) (get-bonus cell)))))

(defn can-move?
  "Returns true if the ship can move."
  [world ship]
  (let [cell (get-in world [:cells (select-keys ship [:x :y])])]
    (>= (:halite ship) (Math/floor (* (:halite cell) MOVE_COST)))))

(defn get-capacity
  "Returns how much halite a ship can add before it is full."
  [ship]
  (- MAX_HALITE_CARRY (:halite ship)))

(defn at-enemy-dropoff?
  "Returns true if the location is an enemy dropoff."
  [world cell]
  (let [x-and-y (select-keys cell [:x :y])]
    (some? (first (filter #(= x-and-y (select-keys % [:x :y]))
                          (:enemy-dropoffs world))))))

(defn distance-between
  "Returns the distance between two cells."
  [width height cell1 cell2]
  (let [x-dist (Math/abs ^Integer (- (:x cell1) (:x cell2)))
        y-dist (Math/abs ^Integer (- (:y cell1) (:y cell2)))
        x-dist (if (> x-dist (/ width 2))
                (- width x-dist)
                x-dist)
        y-dist (if (> y-dist (/ width 2))
                (- height y-dist)
                y-dist)]
    (+ x-dist y-dist)))

(def opposite-direction
  "Map of opposite directions"
  {NORTH SOUTH
   SOUTH NORTH
   EAST WEST
   WEST EAST})

(def exactly-one-range-possibilities
  "A collection of all the one range possible cells."
  [[NORTH]
   [EAST]
   [SOUTH]
   [WEST]])

(def exactly-two-range-possibilities
  "A collection of all the two range possible cells."
  [[NORTH NORTH]
   [EAST NORTH]
   [EAST EAST]
   [EAST SOUTH]
   [SOUTH SOUTH]
   [WEST NORTH]
   [WEST SOUTH]
   [WEST WEST]])

(def exactly-three-range-possibilities
  "A collection of all the three range possible cells."
  [[NORTH NORTH NORTH]
   [SOUTH SOUTH SOUTH]
   [EAST EAST EAST]
   [WEST WEST WEST]
   [NORTH NORTH EAST]
   [EAST EAST NORTH]
   [EAST EAST SOUTH]
   [SOUTH SOUTH EAST]
   [WEST WEST NORTH]
   [WEST NORTH NORTH]
   [WEST WEST SOUTH]
   [WEST SOUTH SOUTH]])

(def exactly-four-range-possibilities
  "A collection of all the four range possible cells."
  [[NORTH NORTH NORTH NORTH]
   [SOUTH SOUTH SOUTH SOUTH]
   [EAST EAST EAST EAST]
   [WEST WEST WEST WEST]
   [NORTH NORTH NORTH EAST]
   [NORTH NORTH NORTH WEST]
   [NORTH NORTH EAST EAST]
   [NORTH NORTH WEST WEST]
   [NORTH EAST EAST EAST]
   [NORTH WEST WEST WEST]
   [SOUTH SOUTH SOUTH WEST]
   [SOUTH SOUTH SOUTH EAST]
   [SOUTH SOUTH WEST WEST]
   [SOUTH SOUTH EAST EAST]
   [SOUTH WEST WEST WEST]
   [SOUTH EAST EAST EAST]])

(def exactly-five-range-possibilities
  "A collection of all the four range possible cells."
  [[NORTH NORTH NORTH NORTH NORTH]
   [SOUTH SOUTH SOUTH SOUTH SOUTH]
   [EAST EAST EAST EAST EAST]
   [WEST WEST WEST WEST WEST]
   [NORTH NORTH NORTH NORTH EAST]
   [NORTH NORTH NORTH EAST EAST]
   [NORTH NORTH NORTH NORTH WEST]
   [NORTH NORTH NORTH WEST WEST]
   [NORTH NORTH EAST EAST EAST]
   [NORTH NORTH WEST WEST WEST]
   [NORTH EAST EAST EAST EAST]
   [NORTH WEST WEST WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH WEST]
   [SOUTH SOUTH SOUTH WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH EAST]
   [SOUTH SOUTH SOUTH EAST EAST]
   [SOUTH SOUTH WEST WEST WEST]
   [SOUTH SOUTH EAST EAST EAST]
   [SOUTH WEST WEST WEST WEST]
   [SOUTH EAST EAST EAST EAST]])

(def exactly-six-range-possibilities
  "A collection of all the exactly 6 range possible cells."
  [[NORTH NORTH NORTH NORTH NORTH NORTH]
   [SOUTH SOUTH SOUTH SOUTH SOUTH SOUTH]
   [EAST EAST EAST EAST EAST EAST]
   [WEST WEST WEST WEST WEST WEST]
   [NORTH NORTH NORTH NORTH NORTH EAST]
   [NORTH NORTH NORTH NORTH EAST EAST]
   [NORTH NORTH NORTH EAST EAST EAST]
   [NORTH NORTH NORTH NORTH NORTH WEST]
   [NORTH NORTH NORTH NORTH WEST WEST]
   [NORTH NORTH NORTH WEST WEST WEST]
   [NORTH NORTH EAST EAST EAST EAST]
   [NORTH NORTH WEST WEST WEST WEST]
   [NORTH EAST EAST EAST EAST EAST]
   [NORTH WEST WEST WEST WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH SOUTH WEST]
   [SOUTH SOUTH SOUTH SOUTH WEST WEST]
   [SOUTH SOUTH SOUTH WEST WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH SOUTH EAST]
   [SOUTH SOUTH SOUTH SOUTH EAST EAST]
   [SOUTH SOUTH SOUTH EAST EAST EAST]
   [SOUTH SOUTH WEST WEST WEST WEST]
   [SOUTH SOUTH EAST EAST EAST EAST]
   [SOUTH WEST WEST WEST WEST WEST]
   [SOUTH EAST EAST EAST EAST EAST]])

(def exactly-seven-range-possibilities
  "A collection of all the exactly 7 range possible cells."
  [[NORTH NORTH NORTH NORTH NORTH NORTH NORTH]
   [SOUTH SOUTH SOUTH SOUTH SOUTH SOUTH SOUTH]
   [EAST EAST EAST EAST EAST EAST EAST]
   [WEST WEST WEST WEST WEST WEST WEST]
   [NORTH NORTH NORTH NORTH NORTH NORTH EAST]
   [NORTH NORTH NORTH NORTH NORTH EAST EAST]
   [NORTH NORTH NORTH NORTH EAST EAST EAST]
   [NORTH NORTH NORTH EAST EAST EAST EAST]
   [NORTH NORTH NORTH NORTH NORTH NORTH WEST]
   [NORTH NORTH NORTH NORTH NORTH WEST WEST]
   [NORTH NORTH NORTH NORTH WEST WEST WEST]
   [NORTH NORTH NORTH WEST WEST WEST WEST]
   [NORTH NORTH EAST EAST EAST EAST EAST]
   [NORTH NORTH WEST WEST WEST WEST WEST]
   [NORTH EAST EAST EAST EAST EAST EAST]
   [NORTH WEST WEST WEST WEST WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH SOUTH SOUTH WEST]
   [SOUTH SOUTH SOUTH SOUTH SOUTH WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH WEST WEST WEST]
   [SOUTH SOUTH SOUTH WEST WEST WEST WEST]
   [SOUTH SOUTH SOUTH SOUTH SOUTH SOUTH EAST]
   [SOUTH SOUTH SOUTH SOUTH SOUTH EAST EAST]
   [SOUTH SOUTH SOUTH SOUTH EAST EAST EAST]
   [SOUTH SOUTH SOUTH EAST EAST EAST EAST]
   [SOUTH SOUTH WEST WEST WEST WEST WEST]
   [SOUTH SOUTH EAST EAST EAST EAST EAST]
   [SOUTH WEST WEST WEST WEST WEST WEST]
   [SOUTH EAST EAST EAST EAST EAST EAST]])

(defn get-end-location
  "Returns the last location following a list of directions."
  [world cell directions]
  (let [{:keys [width height]} world]
    (loop [next-cell cell
           directions directions]
      (if (seq directions)
        (recur (get-x-and-y next-cell (first directions) width height) (rest directions))
        next-cell))))

(defn get-locations
  "Returns all locations based on a cell and a collection of directions."
  [world cell directions-coll]
  (for [directions directions-coll]
    (get-end-location world cell directions)))

(defn total-turns
  "Returns total turns before the game is over."
  [height width]
  (+ 400 (* (/ (- height 32) 8) 25)))

(defn two-player?
  "Returns true if it is a two player game."
  [world]
  (= 2 (:num-players world)))

; (defn enough-spawn-halite?
;   "Returns true if I have enough halite to spawn a ship."
;   [world constants]
;   (let [{:keys [my-player reserve]} world]
;     (>= (:halite my-player) (+ (get constants "NEW_ENTITY_ENERGY_COST") reserve))))

(defn enough-spawn-halite?
  "Returns true if I have enough halite to spawn a ship."
  [world]
  (let [{:keys [my-player reserve]} world]
    (>= (:halite my-player) (+ SHIP_COST reserve))))

(defn load-world
  "Loads the inital world before the start of the first round."
  []
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
        cells (load-cells height)]
    {:num-players num-players :my-id my-id :my-shipyard my-shipyard :width width :height height
     :cells cells :other-shipyards other-shipyards}))
