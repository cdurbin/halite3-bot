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
  "
  (:require
   [hlt.game :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)
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

(def quadrant->cells
  "Map from quadrant to the cells in that quadrant."
  (into {}
    (for [q (range 64)]
      [q (keep (fn [[k v]]
                 (when (= q v)
                   k))
               cell->quadrant)])))

(def valid-quadrants
  "Map width to valid quadrants."
  (into {}
    (for [width [32 40 48 56 64]]
      [width (set
              (keep (fn [[cell quadrant]]
                      (when (and (< (:x cell) width)
                                 (< (:y cell) width))
                        quadrant))
                    cell->quadrant))])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Metrics
(defn avg-gather-amount-per-cell
  "Returns average gather amount per quadrant cell."
  [world cells]
  (/ (reduce + (map get-gather-amount cells))
     (count cells)))

(defn get-quadrant-metrics
  "Returns all of the relevant metrics for a quadrant."
  [world quadrant]
  (let [{:keys [cells]} world
        quadrant-cells (filter #(= quadrant (:quadrant %)) (vals cells))
        avg-gather-per-cell (avg-gather-amount-per-cell world quadrant-cells)]))

(comment
 (get valid-quadrants 64))
