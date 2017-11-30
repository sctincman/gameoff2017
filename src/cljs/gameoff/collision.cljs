(ns gameoff.collision
  (:require [gameoff.signals :as s]
            [gameoff.render.threejs.core :as render]
            [clojure.core.matrix :as m]))

(defprotocol ^:export IAABB
  (left [this])
  (right [this])
  (top [this])
  (bottom [this])
  (back [this])
  (front [this])
  (mins [this])
  (maxs [this]))

(defrecord ^:export AABB [center width height depth half-width half-height half-depth]
  IAABB
  (left [this] (- (first center) half-width))
  (right [this] (+ (first center) half-width))
  (bottom [this] (- (second center) half-height))
  (top [this] (+ (second center) half-height))
  (back [this] (- (nth center 2) half-depth))
  (front [this] (+ (nth center 2) half-depth))
  (mins [this] [(left this) (bottom this) (back this)])
  (maxs [this] [(right this) (top this) (front this)]))

(defrecord ^:export WABB [min-point max-point]
  IAABB
  (left [this] (first min-point))
  (right [this] (first max-point))
  (bottom [this] (nth min-point 2))
  (top [this] (nth max-point 2))
  (back [this] (second min-point))
  (front [this] (second max-point))
  (mins [this] min-point)
  (maxs [this] max-point))

(defn ^:export add-aabb [entity offset width height depth]
  (assoc entity :aabb
         (->AABB offset width height depth
                 (/ width 2.0) (/ height 2.0) (/ depth 2.0))))

(defn ^:export spatial?
  "Does component have a position and collision components."
  [entity]
  (and (some? (:position entity))
       (some? (:collidable entity))))

(defn update-wabbs
  "A horrible hack!"
  [world]
  (let [current-scene (get-in world [:scene :current-scene])
        scenes @(get-in world [:backend :scenes])
        box3 (js/THREE.Box3.)
        empty (js/THREE.Object3D.)]
    (reduce-kv (fn [world id entity]
                 (if-not (spatial? entity)
                   world
                   (if-let [root  (get-in scenes [current-scene :children id :root])]
                     (let [box3  (.setFromObject box3 root)
                           wabb (->WABB [(aget box3 "min" "x") (aget box3 "min" "y") (aget box3 "min" "z")]
                                        [(aget box3 "max" "x") (aget box3 "max" "y") (aget box3 "max" "z")])]
                       (assoc-in world [id :aabb] wabb))
                     world)))
               world
               world)))

(defn ^:export add-space
  "Spatial component. Stores a location based hash of entities with collision components."
  [world bucket-size]
  (let [inverse (/ 1 bucket-size)]
    (-> world
        (assoc-in [:space :bucket] [inverse inverse inverse])
        (assoc-in [:space :cells] (hash-map)))))

(defn pos->bucket-space
  [position bucket]
  (map int (m/mul position bucket)))

(defn aabb-buckets [space entity]
  (let [start-point (pos->bucket-space (m/add (:position entity)
                                        (mins (:aabb entity)))
                                 (:bucket space))
        end-point (pos->bucket-space (m/add (:position entity)
                                      (maxs (:aabb entity)))
                               (:bucket space))]
    (for [x (range (first start-point) (inc (first end-point)))
          y (range (second start-point) (inc (second end-point)))
          z (range (second start-point) (inc (second end-point)))]
      [x y z])))

(defn populate-cells
  "Buckets entities by their position and collisions"
  ([world]
   (let [bucket (get-in world [:space :bucket])]
     (reduce-kv (fn [cells id entity]
                  (if-not (and (spatial? entity) (some? (:aabb entity)))
                    cells
                    (let [start-point (pos->bucket-space (mins (get entity :aabb))
                                                         bucket)
                          end-point   (pos->bucket-space (maxs (get entity :aabb))
                                                         bucket)]
                      (reduce (fn [cells x]
                                (reduce (fn [cells y]
                                          (reduce (fn [cells z]
                                                    (update cells [x y z] conj id))
                                                  cells
                                                  (range (nth start-point 2) (inc (nth end-point 2)))))
                                        cells
                                        (range (second start-point) (inc (second end-point)))))
                              cells
                              (range (first start-point) (inc (first end-point)))))))
                (hash-map)
                world))))

(defn aabb-intersects? [first-aabb second-aabb]
  (and (some? first-aabb) (some? second-aabb)
       (and (<= (left first-aabb) (right second-aabb))
            (>= (right first-aabb) (left second-aabb)))
       (and (<= (bottom first-aabb) (top second-aabb))
            (>= (top first-aabb) (bottom second-aabb)))
       (and (<= (back first-aabb) (front second-aabb))
            (>= (front first-aabb) (back second-aabb)))))

(defn intersect-wabb [first-wabb second-wabb]
  (let [min-point (map max (mins first-wabb) (mins second-wabb))
        max-point (map min (maxs first-wabb) (maxs second-wabb))]
    (->WABB min-point max-point)))

(defn ^:export aabb-collision-pairs
  "Returns list of collision pairs"
  [world cells]
  (reduce-kv (fn [pairs cell children]
               (loop [first-key (first children)
                      the-rest (rest children)
                      pairs pairs]
                 (if (pos? (count the-rest))
                   (recur (first the-rest) (rest the-rest)
                          (reduce (fn [pairs second-key]
                                    (if (aabb-intersects? (get-in world [first-key :aabb])
                                                          (get-in world [second-key :aabb]))
                                      (update pairs first-key
                                              (fn [others]
                                                (set (conj others second-key))))
                                      pairs))
                                  pairs
                                  the-rest))
                   pairs)))
             {}
             cells))

(defn null-handler [prime second-key delta-t world]
  (println "Collision! " second-key)
  prime)

(defn wabb-solid-handler [prime second-key delta-t world]
  (if-let [mass-prime (get-in prime [:body :mass])]
    (let [mass-secondary (get-in world [second-key :body :mass] js/Infinity)
          overlap (intersect-wabb (:aabb prime) (get-in world [second-key :aabb]))
          dimensions (m/sub (maxs overlap) (mins overlap))
          weight-factor (- 1 (/ mass-prime (+ mass-prime mass-secondary)))
          direction (m/normalise (m/sub (:position prime)
                                        (get-in world [:second-key :position])))
          displacement (m/mul weight-factor (m/mul direction
                                                   (m/dot direction
                                                          dimensions)))]
      (update prime :position m/add displacement))
    prime))

(defn handle-collision
  [primary-key secondary-key world delta-t]
  (let [primary-entity (get world primary-key)
        primary-event (get-in world [primary-key :collision-handlers :internal] null-handler)
        secondary-event (get-in world [secondary-key :collision-handlers :external] null-handler)]
    (-> primary-entity
        (primary-event secondary-key delta-t world)
        (secondary-event primary-key delta-t world))))

(defn ^:export handle-collisions
  "Not sure yet"
  [world delta-t]
  (let [world (update-wabbs world)
        cells (populate-cells world)
        world (assoc-in world [:space :cells] cells)
        pairs (aabb-collision-pairs world cells)]
    (reduce-kv (fn [world primary-key collider-keys]
                 (reduce (fn [world collider-key]
                           (assoc world primary-key
                                   (handle-collision primary-key collider-key world delta-t)))
                         world
                         collider-keys))
               world
               pairs)))
