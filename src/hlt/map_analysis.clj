(ns hlt.map-analysis
  "Functions that analyze the map. Note that quadrants need to work for map sizes up to 64 by 64.
  So on smaller maps there will be gaps between quadrant numbers.

  TODO:
  1.) I need to set the expected quadrant for each ship (maybe as part of picking the mode).

  Dropoff ships should set their quadrant to the quadrant of their closest dropoff. For collecting
  ships I'll want to look at the immediate mining capabilities (maybe current cell and one cell in
  every direction). Compare the amount I can mine from nearby cells to all of the other quadrants.
  If I can do better closeby, then stay. Otherwise move to the best quadrant.
  Perform the quadrant check in multiple sweeps. First assign all the enemy ships. I could assign
  enemies the same way I assign my own ships. For my ships: First assign of the dropoff ships. Then
  assign all the ships that are mining their nearby cells to their current quadrants. Then assign
  all the other ships sorted by the ships with the most halite.

  2.) Continue to choose dropoffs the same way for now. I should take quadrants into account soon.
  TBD the best way to incorporate quadrants.

  Performance concerns:
  I should calculate quadrant stats once per turn. Ideally I don't need to map over all of the
  cells in order to perform all the metrics calculations.

  Useful metrics:
  1.) average gather amount per quadrant cell
  2.) average gather amount per ship per turn in the quadrant
  3.) sorted gather amounts (so if there's N ships -
  what halite can I expect to gather for the N + 1 ship.)
  4.) prediction of when the quadrant will be 'mostly' inspired
  5.) sorted gather amounts of a cell plus it's directly surrounding cells (5 total turns)
  6.) predict how much halite will be left to gather N turns from now

  Questions: Do I need to track updates to a quadrant mid-turn?

  Algorithm - at beginning of turn assume all ships will stay in their current quadrants. Then
  update to
  "
  (:require
   [hlt.game :refer :all]
   [hlt.utils :refer :all]
   [clojure.java.io :as io]
   [clojure.edn :as edn])
  (:gen-class))

(set! *warn-on-reflection* true)

(def cells-per-quadrant 64)
; (def cells-per-quadrant 16)
(def num-quadrants 64)
; (def num-quadrants 256)
(def inverse-cells-per-quadrant (double (/ 1 cells-per-quadrant)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup quadrants
(def cell->quadrant
  "Map from {:x :y} to a quadrant."
  (into {}
    (for [x (range 64)
          y (range 64)]
      [{:x x :y y}
       (+ (int (/ x 8))
          (* 8 (int (/ y 8))))])))

; (def cell->quadrant
;   "Map from {:x :y} to a quadrant."
;   (into {}
;     (for [x (range 64)
;           y (range 64)]
;       [{:x x :y y}
;        (+ (int (/ x 4))
;           (* 4 (int (/ y 4))))])))

(def quadrant->cells
  "Map from quadrant to the cells in that quadrant."
  (into {}
    (for [q (range num-quadrants)]
      [q (keep (fn [[k v]]
                 (when (= q v)
                   k))
               cell->quadrant)])))

(def map-size->valid-quadrants
  "Map width to valid quadrants."
  (into {}
    (for [width [32 40 48 56 64]]
      [width (set
              (keep (fn [[cell quadrant]]
                      (when (and (< (:x cell) width)
                                 (< (:y cell) width))
                        quadrant))
                    cell->quadrant))])))

(defn calculate-cell-and-quadrant->distance
  "Maps a cell to the distance to each other quadrant."
  [world]
  (let [{:keys [width height]} world]
    (into {}
      (for [x (range width)
            y (range height)
            quadrant (range num-quadrants)
            :when (some #{quadrant} (map-size->valid-quadrants width))
            :let [distances (map #(distance-between width height {:x x :y y} %)
                                 (get quadrant->cells quadrant))]]
        [{:x x :y y :quadrant quadrant} (apply min distances)]))))

(defn load-cell-and-quadrant->distance
  "Loads in cell and quadrant distance from a file (pre-calculated in a file)."
  [width]
  (edn/read-string (slurp (io/resource (str width ".edn")))))
  ; (edn/read-string (slurp (io/resource (str width "-small.edn")))))

(comment
 (def from-file (load-cell-and-quadrant->distance 64))
 (get valid-quadrants 32)
 (def the-test (calculate-cell-and-quadrant->distance {:width 32 :height 32}))
 (def the-64-test (calculate-cell-and-quadrant->distance {:width 64 :height 64}))
 (spit "32-small.edn" (calculate-cell-and-quadrant->distance {:width 32 :height 32}))
 (spit "40-small.edn" (calculate-cell-and-quadrant->distance {:width 40 :height 40}))
 (spit "48-small.edn" (calculate-cell-and-quadrant->distance {:width 48 :height 48}))
 (spit "56-small.edn" (calculate-cell-and-quadrant->distance {:width 56 :height 56}))
 (spit "64-small.edn" (calculate-cell-and-quadrant->distance {:width 64 :height 64}))
 (map (fn [c]
        [c (distance-between 32 32 {:x 0 :y 0} c)])
      (get quadrant->cells 2))
 (get the-test {:x 0 :y 0 :quadrant 10})
 (get from-file {:x 0 :y 0 :quadrant 33})
 (first from-file)
 (get quadrant->cells 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def NUM_CELLS_TO_AVG 10)

;;; Metrics
(defn avg-gather-amount-per-cell
  "Returns average gather amount per quadrant cell."
  [world cells]
  (/ (reduce + (map get-gather-amount (take NUM_CELLS_TO_AVG (sort (compare-by :score desc) cells))))
     NUM_CELLS_TO_AVG))
     ; (count cells)))

(defn avg-gather-amount-per-ship
  "Returns average gather amount per quadrant cell."
  [world cells ships]
  ; (/ (reduce + (map get-gather-amount cells))
  (/ (reduce + (map get-gather-amount (take NUM_CELLS_TO_AVG (sort (compare-by :score desc) cells))))
    (max 1 (count ships))))

(defn get-top-scoring-cell
  "Returns the top cell from a list of cells."
  [cells]
  (first (sort (compare-by :score desc) cells)))

(defn get-cells-by-quadrant
  "Returns a map of quadrants as keys and all the cells within each quadrant as the values."
  [world]
  (group-by :quadrant (vals (:cells world))))

(defn get-ships-by-quadrant
  "Returns a map of ships by quadrant."
  [world]
  (group-by :quadrant (mapcat :ships (:players world))))

(defn-timed get-quadrant-metrics
  "Returns all of the relevant metrics for all quadrants."
  [world]
  (let [quadrant-cells (get-cells-by-quadrant world)
        quadrant-ships (get-ships-by-quadrant world)]
    (reduce (fn [metrics quadrant]
              (let [quadrant-num (key quadrant)
                    cells (val quadrant)
                    total-halite (reduce + (map :halite cells))
                    total-bonus (reduce + (map get-bonus cells))
                    ship-count (count (get quadrant-ships quadrant-num))
                    avg-gather-per-cell (avg-gather-amount-per-cell world cells)
                    per-turn-gather (* ship-count avg-gather-per-cell)
                    ; avg-gather-per-ship (avg-gather-amount-per-ship world cells
                    ;                                                 (get quadrant-ships quadrant-num))
                    metrics-for-quadrant {
                                          :avg-gather-per-cell avg-gather-per-cell
                                          :per-turn-gather per-turn-gather
                                          ; :avg-gather-per-ship avg-gather-per-ship
                                          :total-halite total-halite
                                          :total-bonus total-bonus
                                          ;; TODO guessing at an inspiration in N turns is probably
                                          ;; important
                                          :ship-count ship-count
                                          :top-scoring-cell (get-top-scoring-cell cells)}]
                (assoc metrics quadrant-num metrics-for-quadrant)))
            {}
            quadrant-cells)))

(defn update-quadrant-metrics
  "Updates a quadrant-metrics based on a ship changing quadrants."
  [metrics old-quadrant new-quadrant]
  (let [new-quadrant-metrics (get metrics new-quadrant)
        new-quadrant-ships (inc (:ship-count new-quadrant-metrics))
        new-per-turn-gather (* new-quadrant-ships (:avg-gather-per-cell new-quadrant-metrics))
        new-quadrant-metrics (assoc new-quadrant-metrics
                                    :ship-count new-quadrant-ships
                                    :per-turn-gather new-per-turn-gather)
        old-quadrant-metrics (get metrics old-quadrant)
        old-quadrant-ships (dec (:ship-count old-quadrant-metrics))
        old-per-turn-gather (* old-quadrant-ships (:avg-gather-per-cell old-quadrant-metrics))
        old-quadrant-metrics (assoc old-quadrant-metrics
                                    :ship-count old-quadrant-ships
                                    :per-turn-gather old-per-turn-gather)]
    (assoc metrics old-quadrant old-quadrant-metrics new-quadrant new-quadrant-metrics)))

(defn get-turns-for-needed-halite
  "Returns the number of turns it takes to gather the required halite."
  [world needed-halite quadrant-num]
  (let [metrics (get-in world [:quadrant-metrics quadrant-num])]
    ; (log "Metrics are:" metrics "Quadrant number is:" quadrant-num)
    ; (log "QMS:" (pr-str (:quadrant-metrics world)))
    (/ needed-halite (max 1 (:avg-gather-per-cell metrics)))))

(defn get-best-quadrant
  "Returns the best quadrant to try to gather from based on the needed-halite"
  [world location needed-halite potential-quadrants max-turns]
  ; (log "Potential quadrants were:" (pr-str potential-quadrants))
  (let [quadrants (map (fn [{:keys [quadrant distance]}]
                         {:quadrant quadrant
                          :distance distance
                          :turns (+ distance (get-turns-for-needed-halite world needed-halite quadrant))})
                       potential-quadrants)]
    ; (log "Quadrants are:" (pr-str quadrants))
    (:quadrant (first (sort (compare-by :turns asc :distance asc) quadrants)))))
