(ns hlt.dropoffs
  "Functions related to choosing dropoff locations."
  (:require
   [hlt.utils :refer :all]
   [hlt.game :refer :all]
   [hlt.custom-game :refer :all])
  (:gen-class))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dropoff management
(def FIRST_BUILD_DROPOFF_DISTANCE 15)
(def BUILD_DROPOFF_DISTANCE 15)
(def MAX_DROPOFF_LOCATION_DISTANCE 40)
; (def NUM_POTENTIAL_DROPOFFS 1)
(def MIN_DROPOFF_SCORE 6500)
; (def MIN_DROPOFF_SCORE 5100)

(def MIN_SHIPS_PER_DROPOFF 13)
; (def MIN_SHIPS_FOR_FIRST_DROPOFF 10)
; (def MIN_SHIPS_FOR_FIRST_DROPOFF_TWO_PLAYER 10)
(def MAX_MOVE_TO_DROPOFF_DISTANCE 10)

(def min-ships-for-first-dropoff
  {2 {32 10
      40 10
      48 10
      56 12
      64 11}
   4 {32 9
      40 9
      48 11
      56 10
      64 11}})

(def FAR_DROPOFF 25)
(def AUTO_BUILD_DROPOFF 2500)

(def NEXT_DROPOFF_DISTANCE 5)
(def NEARBY_DROPOFF_HALITE 5000)
; (def NEARBY_DROPOFF_HALITE 2100)

(def REQUIRED_NEARBY_HALITE 5100)
(def USEFUL_DROPOFF_DISTANCE 16)

(def min-per-ship-to-build-dropoff
  {2 {32 5
      40 5
      48 5
      56 5
      64 5}
   4 {32 5
      40 5
      48 5
      56 5
      64 5}})
  ; {2 {32 1410
  ;     40 1410
  ;     48 1410
  ;     56 1110
  ;     64 1110}
  ;  4 {32 380
  ;     40 380
  ;     48 350
  ;     56 330
  ;     64 350}})

(defn get-dropoff-distance
  "Returns the number of cells between dropoffs to decide what to build."
  [num-dropoffs]
  (if (> num-dropoffs 0)
    BUILD_DROPOFF_DISTANCE
    FIRST_BUILD_DROPOFF_DISTANCE))

(defn not-terrible-dropoff?
  "Returns true if I have more ships around a cell (or there are no nearby ships)."
  [world cell]
  (let [my-id (:my-id world)
        nearby-ships (get-six-range-ships world cell)
        my-ships (filter #(= my-id (:owner %)) nearby-ships)
        my-ship-count (count my-ships)
        other-ship-count (- (count nearby-ships) (count my-ships))]
    ; (or (> my-ship-count 7)
    (>= my-ship-count other-ship-count)))

; (defn choose-dropoff-locations
;   "Given a bunch of choices for dropoff locations. Choose the one(s) that could be the most
;   valuable."
;   [world last-dropoff-location]
;   (let [{:keys [my-player reserve]} world
;         num-dropoffs (count (:dropoffs my-player))
;         build-dropoff-distance (if (> num-dropoffs 0)
;                                  BUILD_DROPOFF_DISTANCE
;                                  FIRST_BUILD_DROPOFF_DISTANCE)
;         last-dropoff-location (get-cell world last-dropoff-location)]
;     (if (and last-dropoff-location
;              ; false ;; see what happens if I let my ships build anywhere
;              (not (at-enemy-dropoff? world last-dropoff-location))
;              (not-terrible-dropoff? world last-dropoff-location))
;              ; (> (:score last-dropoff-location) (- MIN_DROPOFF_SCORE 2000))
;              ; (> (:uninspired-score last-dropoff-location) (- MIN_DROPOFF_SCORE 1000))
;              ; (>= (:dropoff-distance last-dropoff-location) build-dropoff-distance))
;       [last-dropoff-location]
;       ; (let [sites (conj (get-cells-within-two-range world last-dropoff-location) last-dropoff-location)
;       ;       best-site (first (sort (compare-by :uninspired-score desc :dropoff-distance desc) sites))]
;       ;   [best-site])
;       (let [{:keys [top-cells]} world
;             nearby-sites (filter #(and (<= build-dropoff-distance
;                                            (:dropoff-distance %)
;                                            MAX_DROPOFF_LOCATION_DISTANCE)
;                                        ; (>= (:score %) MIN_DROPOFF_SCORE)
;                                        (>= (:uninspired-score %) MIN_DROPOFF_SCORE)
;                                        (not-terrible-dropoff? world %))
;                                  top-cells)
;             nearby-sites (if (seq nearby-sites)
;                            nearby-sites
;                            (filter #(and (<= build-dropoff-distance
;                                              (:dropoff-distance %))
;                                          (>= (:uninspired-score %) MIN_DROPOFF_SCORE)
;                                          (not-terrible-dropoff? world %))
;                                    top-cells))
;             - (log (format "Turn %d potential dropoff locations %s" (:turn world)
;                          ; (pr-str (take NUM_POTENTIAL_DROPOFFS (map #(select-keys % [:x :y]) (sort (compare-by :score desc) nearby-sites))))
;                            (pr-str (take NUM_POTENTIAL_DROPOFFS (map #(select-keys % [:x :y]) (sort (compare-by :uninspired-score desc) nearby-sites))))))
;             ; (take NUM_POTENTIAL_DROPOFFS (sort (compare-by :score desc) nearby-sites))
;             dropoffs (take NUM_POTENTIAL_DROPOFFS (sort (compare-by :dropoff-distance asc :uninspired-score desc) nearby-sites))
;             ;; TODO work with multiple dropoffs later
;             dropoff (first dropoffs)
;             {:keys [ship distance]} (when dropoff
;                                       (find-closest-ship world dropoff (-> world :my-player :ships)))]
;         (when (and ship (< distance MAX_MOVE_TO_DROPOFF_DISTANCE))
;           [dropoff])))))

(defn custom-dropoff-score
  "Scores a dropoff. Takes the score, uninspired-score, dropoff distance,
  number of my nearby ships into account."
  [world dropoff]
  (let [{:keys [score uninspired-score dropoff-distance]} dropoff
        my-id (:my-id world)
        nearby-ships (get-six-range-ships world dropoff)
        my-nearby-count (count (filter #(= my-id (:owner %)) nearby-ships))]
    (+ (* 0.25 score) (* 0.75 uninspired-score) (* 250 my-nearby-count)
       (- (* 100 dropoff-distance)))))

; (def NUM_POTENTIAL_DROPOFFS 7)

; (def num-potential-dropoffs
;   {2 {32 5
;       40 6
;       48 7
;       56 7
;       64 7}
;    4 {32 5
;       40 5
;       48 7
;       56 6
;       64 20}})

(def num-potential-dropoffs
  {2 {32 5
      40 5
      48 5
      56 17
      64 17}
   4 {32 8
      40 8
      48 8
      56 5
      64 17}})

(defn choose-best-dropoffs
  "Returns the best dropoff from a list of dropoff locations."
  [world dropoffs]
  (let [{:keys [width num-players]} world
        dropoffs-with-scores (map #(assoc % :custom-score (custom-dropoff-score world %))
                                  dropoffs)]
    (take (get-in num-potential-dropoffs [num-players width])
          (sort (compare-by :custom-score desc) dropoffs-with-scores))))

(defn choose-dropoff-locations
  "Given a bunch of choices for dropoff locations. Choose the one(s) that could be the most
  valuable."
  [world last-dropoff-location]
  ; (let [{:keys [my-player reserve]} world
  ;       num-dropoffs (count (:dropoffs my-player))
  ;       build-dropoff-distance (if (> num-dropoffs 0)
  ;                                BUILD_DROPOFF_DISTANCE
  ;                                FIRST_BUILD_DROPOFF_DISTANCE)
  ;       last-dropoff-location (get-cell world last-dropoff-location)]
  ;   (if (and last-dropoff-location
  ;            ; false ;; see what happens if I let my ships build anywhere
  ;            (not (at-enemy-dropoff? world last-dropoff-location))
  ;            (not-terrible-dropoff? world last-dropoff-location))
  ;            ; (> (:score last-dropoff-location) (- MIN_DROPOFF_SCORE 2000))
  ;            ; (> (:uninspired-score last-dropoff-location) (- MIN_DROPOFF_SCORE 1000))
  ;            ; (>= (:dropoff-distance last-dropoff-location) build-dropoff-distance))
  ;     [last-dropoff-location]
      ; (let [sites (conj (get-cells-within-two-range world last-dropoff-location) last-dropoff-location)
      ;       best-site (first (sort (compare-by :uninspired-score desc :dropoff-distance desc) sites))]
      ;   [best-site])
  (let [{:keys [top-cells uninspired-cells my-player]} world
        num-dropoffs (count (:dropoffs my-player))
        build-dropoff-distance (if (> num-dropoffs 0)
                                 BUILD_DROPOFF_DISTANCE
                                 FIRST_BUILD_DROPOFF_DISTANCE)
        nearby-sites (filter #(and (<= build-dropoff-distance
                                       (:dropoff-distance %)
                                       MAX_DROPOFF_LOCATION_DISTANCE)
                                   ; (>= (:score %) MIN_DROPOFF_SCORE)
                                   (>= (:uninspired-score %) MIN_DROPOFF_SCORE)
                                   (not-terrible-dropoff? world %))
                             uninspired-cells)
        nearby-sites (if (seq nearby-sites)
                       nearby-sites
                       (filter #(and (<= build-dropoff-distance
                                         (:dropoff-distance %))
                                     (>= (:uninspired-score %) MIN_DROPOFF_SCORE)
                                     (not-terrible-dropoff? world %))
                               uninspired-cells))]
    (when (seq nearby-sites)
      (choose-best-dropoffs world nearby-sites))))
    ;     - (log (format "Turn %d potential dropoff locations %s" (:turn world)
    ;                  ; (pr-str (take NUM_POTENTIAL_DROPOFFS (map #(select-keys % [:x :y]) (sort (compare-by :score desc) nearby-sites))))
    ;                    (pr-str (take NUM_POTENTIAL_DROPOFFS (map #(select-keys % [:x :y]) (sort (compare-by :uninspired-score desc) nearby-sites))))))
    ;     ; (take NUM_POTENTIAL_DROPOFFS (sort (compare-by :score desc) nearby-sites))
    ;     dropoffs (take NUM_POTENTIAL_DROPOFFS (sort (compare-by :dropoff-distance asc :uninspired-score desc) nearby-sites))
    ;     ;; TODO work with multiple dropoffs later
    ;     dropoff (first dropoffs)
    ;     {:keys [ship distance]} (when dropoff
    ;                               (find-closest-ship world dropoff (-> world :my-player :ships)))]
    ; (when (and ship (< distance MAX_MOVE_TO_DROPOFF_DISTANCE))
    ;   [dropoff])))

(defn should-build-dropoff?
  "Returns true if it makes sense for me to build a dropoff."
  [world last-dropoff-location]
  ; (if last-dropoff-location
  (let [{:keys [turn last-dropoff-turn players my-player cells num-players dropoff-locations width]} world
        total-halite (reduce + (map :halite (vals cells)))
        total-ship-count (reduce + (map #(count (:ships %))
                                        players))
        total-ship-count (inc total-ship-count)
        my-ship-count (count (:ships my-player))
        my-num-dropoffs (inc (count (:dropoffs my-player)))
        first-dropoff-ships (get-in min-ships-for-first-dropoff [num-players width])]
    (log "Total halite:" total-halite "total-ship-count" total-ship-count)
    (log "Calculation of halite per ship:" (int (/ total-halite total-ship-count)))
    (log "Turn is " turn "last dropoff-turn is" last-dropoff-turn)
    (and
          (if last-dropoff-location
            (< turn (+ 10 last-dropoff-turn))
            (< turn last-dropoff-turn))
          (> my-ship-count (+ first-dropoff-ships
                              (* MIN_SHIPS_PER_DROPOFF (dec my-num-dropoffs))))
          (or (seq dropoff-locations)
              (> (/ total-halite total-ship-count) (get-in min-per-ship-to-build-dropoff [num-players width]))))))

; (def NUM_DROPOFF_SHIPS 6)
(defn assign-dropoff-moves
  "Returns moves to go towards a dropoff. For now just always assume there is one dropoff."
  [world]
  world)
  ; (let [{:keys [my-player dropoff-locations]} world
  ;       location (first dropoff-locations)
  ;       my-collecting-ships (filter #(= :collect (:mode %)) (:ships my-player))
  ;       ; num-dropoff-ships (Math/ceil (/ (inc (count (:ships my-player))) 7))
  ;       num-dropoff-ships 1
  ;       ships (find-closest-n-ships world location my-collecting-ships num-dropoff-ships)
  ;       ships (filter #(>= (:dropoff-distance %) NEXT_DROPOFF_DISTANCE) ships)]
  ;   (log (format "Turn %d Assigning ships %s to target %s" (:turn world) (pr-str (map :id ships)) (select-keys location [:x :y])))
  ;   (reduce (fn [updated-world ship]
  ;             (let [my-ships (-> updated-world :my-player :ships)
  ;                   ship (assoc ship :target location)
  ;                   my-ships (conj (remove-item ship my-ships) ship)]
  ;               (assoc updated-world :my-player (assoc (:my-player updated-world) :ships my-ships))))
  ;           world
  ;           ships)))

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
                                     (> (:score (get-cell world ship))
                                        (* MIN_DROPOFF_SCORE 1.5)))
                                 (not-terrible-dropoff? world ship))))
                      ships)
        furthest-ship (first (sort (compare-by :dropoff-distance desc) ships))
        cell (when furthest-ship
               (get-cell world furthest-ship))]
    (when (and furthest-ship
               (> (:dropoff-distance furthest-ship) build-dropoff-distance))
               ; (> (:score cell) MIN_DROPOFF_SCORE))
      furthest-ship)))

(defn enough-nearby-ship-halite
  "Returns true if my ships closeby have enough halite carried to warrant a new dropoff."
  [world cell]
  (let [{:keys [my-id]} world
        nearby-ships (get-seven-range-ships world cell)
        my-nearby-ships (filter #(= my-id (:owner %)) nearby-ships)]
    (>= (reduce + (map :halite my-nearby-ships))
        NEARBY_DROPOFF_HALITE)))

(defn enough-nearby-gather-halite
  "Returns true if there is enought halite to gather to warrant a new dropoff."
  [world cell]
  (let [nearby-cells (get-seven-range-cells world cell)]
    (>= (reduce + (map :halite nearby-cells))
        REQUIRED_NEARBY_HALITE)))

(defn useful-dropoff-location
  "Returns true if I could use a dropoff here."
  [world]
  (let [{:keys [my-player]} world
        my-ships (:ships my-player)
        potential-ships (filter (fn [ship]
                                  (let [cell (get-cell world ship)]
                                    (and (>= (:dropoff-distance ship) USEFUL_DROPOFF_DISTANCE)
                                         (>= (+ (:halite my-player) (:halite ship) (:cell-halite ship))
                                             DROPOFF_COST)
                                         (not (at-enemy-dropoff? world ship))
                                         (not-terrible-dropoff? world cell)
                                         (enough-nearby-ship-halite world cell)
                                         (enough-nearby-gather-halite world cell))))
                                my-ships)]
    (first (sort (compare-by :dropoff-distance desc) potential-ships))))

(defn choose-dropoff-ship
  "Returns a ship that should build a dropoff."
  [world]
  (let [{:keys [dropoff-locations my-player]} world
        dropoff-locations (map #(select-keys % [:x :y]) dropoff-locations)
        ships (filter (fn [ship]
                        (and (not (at-enemy-dropoff? world ship))
                             (>= (+ (:halite my-player) (:halite ship) (:cell-halite ship))
                                 DROPOFF_COST)
                             (some (set [(select-keys ship [:x :y])])
                                   dropoff-locations)))
                      (:ships my-player))]
    (log "Turn" (:turn world) "Ships at dropoff" (mapv :id ships))
    (if (or (empty? dropoff-locations) (empty? ships))
      (if-let [second-choice (choose-dropoff-ship-orig world (:ships my-player))]
        second-choice
        (useful-dropoff-location world))
      ; (let [ships (filter (fn [ship]
      ;                       (and (not (at-enemy-dropoff? world ship))
      ;                            (>= (+ (:halite my-player) (:halite ship) (:cell-halite ship))
      ;                                DROPOFF_COST)
      ;                            (some (set [(select-keys ship [:x :y])])
      ;                                  dropoff-locations)))
      ;                     (:ships my-player))]
        ; (first (sort (compare-by :score desc) ships))
      (first (sort (compare-by :score desc :dropoff-distance desc) ships)))))

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

(def GOOD_DROPOFF_GATHER 3000)

(defn good-dropoff?
  "Returns true if a dropoff is good."
  [world dropoff]
  (let [cell (get-cell world dropoff)
        surrounding-cells (get-three-range-cells world cell)
        nearby-gather (reduce + (map #(+ (:halite %) (get-bonus %)) surrounding-cells))]
    (> nearby-gather GOOD_DROPOFF_GATHER)))

(defn get-good-dropoffs
  "Returns a list of good dropoffs."
  [world]
  nil
  (let [my-dropoffs (-> world :my-player :dropoffs)]
    (filter #(good-dropoff? world %) my-dropoffs)))

(defn get-dropoff-advance-ships
  "Returns ships that are dropping off, but don't have to move back to an existing base in order
  to have enough halite to build a new dropoff."
  [world dropoff-ships]
  (let [{:keys [my-player dropoff-locations]} world
        halite-required (- DROPOFF_COST
                           (apply min 1000 (map :halite dropoff-locations))
                           (apply min 1000 (map :halite dropoff-ships)))]
    (if (>= (:halite my-player) halite-required)
      dropoff-ships
      (let [sorted-ships (sort (compare-by :dropoff-distance asc :halite desc) dropoff-ships)]
        (loop [remaining-ships sorted-ships
               halite (:halite my-player)]
          (when-let [next-ship (first remaining-ships)]
            (let [halite (+ halite (- (:halite next-ship) 80))]
              (if (>= halite halite-required)
                (rest remaining-ships)
                (recur (rest remaining-ships) halite)))))))))

(defn decorate-advance-dropoff-ships
  "Adds a key to indicate this is an advance dropoff ship."
  [advance-dropoff-ship dropoff-ships]
  (conj (remove-item advance-dropoff-ship dropoff-ships)
        (assoc advance-dropoff-ship :advance true)))
