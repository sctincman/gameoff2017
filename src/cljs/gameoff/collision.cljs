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
  (back [this] (- (get center 2) half-depth))
  (front [this] (+ (get center 2) half-depth))
  (mins [this] [(left this) (bottom this) (back this)])
  (maxs [this] [(right this) (top this) (front this)]))

(defrecord ^:export WABB [min-point max-point]
  IAABB
  (left [this] (first min-point))
  (right [this] (first max-point))
  (bottom [this] (get min-point 2))
  (top [this] (get max-point 2))
  (back [this] (second min-point))
  (front [this] (second max-point))
  (mins [this] min-point)
  (maxs [this] max-point))

(defn ^:export add-aabb [entity offset width height depth]
  (assoc entity :aabb
         (->AABB offset width height depth
                 (/ width 2.0) (/ height 2.0) (/ depth 2.0))))

(defn update-wabbs
  "A horrible hack!"
  [world]
  (let [current-scene (get-in world [:scene :current-scene])
        scenes @(get-in world [:backend :scenes])
        box3 (js/THREE.Box3.)
        empty (js/THREE.Object3D.)]
    (reduce-kv (fn [world id entity]
                 (let [root  (get-in scenes [current-scene :children id :root] empty)
                       box3  (.setFromObject box3 root)
                       wabb (->WABB [(aget box3 "min" "x") (aget box3 "min" "y") (aget box3 "min" "z")]
                                    [(aget box3 "max" "x") (aget box3 "max" "y") (aget box3 "max" "z")])]
                   (assoc-in world [id :aabb] wabb)))
               world
               world)))

(defn ^:export add-space
  "Spatial component. Stores a location based hash of entities with collision components."
  [world bucket-size]
  (let [inverse (/ 1 bucket-size)]
    (-> world
        (assoc-in [:space :bucket] [inverse inverse inverse])
        (assoc-in [:space :cells] (hash-map)))))

(defn pos->bucket
  [space position]
  (map int (m/dot position
                  (:bucket space))))

(defn ^:export spatial?
  "Does component have a position and collision components."
  [entity]
  (and (some? (:position entity))
       (some? (:aabb entity))) ;change to general collision, or make `collidable?` call
  )

(defn aabb-buckets [space entity]
  (let [start-point (pos->bucket space (m/add (:position entity)
                                              (mins (:aabb entity))))
        end-point (pos->bucket space (m/add (:position entity)
                                            (maxs (:aabb entity))))]
    (for [x (range (first start-point) (inc (first end-point)))
          y (range (second start-point) (inc (second end-point)))
          z (range (second start-point) (inc (second end-point)))]
      [x y z])))

(defn populate-space
  "Buckets entities by their position and collisions"
  ([space world]
   (assoc space :cells
          (reduce-kv (fn [cells id entity]
                       (if (spatial? entity)
                         
                         (let [start-point (pos->bucket space (m/add (:position entity)
                                                                     (mins (get entity :aabb))))
                               end-point (pos->bucket space (m/add (:position entity)
                                                                   (maxs (get entity :aabb))))]
                           (reduce (fn [cells x]
                                     (reduce (fn [cells y]
                                               (reduce (fn [cells z]
                                                         (update cells [x y z] conj id))
                                                       cells
                                                       (range (get start-point 2) (inc (get end-point 2)))))
                                             cells
                                             (range (second start-point) (inc (second end-point)))))
                                   cells
                                   (range (first start-point) (inc (first end-point)))))
                         cells))
                     (hash-map)
                     world)))
  ([world]
   (update world :space populate-space world)))

(defn aabb-intersects? [first-aabb second-aabb]
  (and (some? first-aabb) (some? second-aabb)
       (and (<= (left first-aabb) (right second-aabb))
            (>= (right first-aabb) (left second-aabb)))
       (and (<= (bottom first-aabb) (top second-aabb))
            (>= (top first-aabb) (bottom second-aabb)))
       (and (<= (back first-aabb) (front second-aabb))
            (>= (front first-aabb) (back second-aabb)))))

(defn translate-aabb
  [aabb position]
  (update aabb :center m/add position))

(defn ^:export aabb-collision-pairs
  "Returns list of collision pairs"
  [space world]
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
             (:cells space)))

(defn null-handler [prime second-key delta-t world]
  prime)

(defn handle-collision
  [world primary-key secondary-key delta-t]
  (let [primary-entity (get world primary-key)
        primary-event (get-in world [primary-key :collision-handlers :external] null-handler)
        secondary-event (get-in world [secondary-key :collision-handlers :external] null-handler)]
    (-> primary-entity
        (primary-event secondary-key delta-t world)
        (secondary-event primary-key delta-t world))))

(defn ^:export handle-collisions
  "Not sure yet"
  [world delta-t]
  (let [space (populate-space (get world :space) world)
        pairs (aabb-collision-pairs space world)]
    (reduce-kv (fn [world primary-key collider-keys]
                 (reduce (fn [world collider-key]
                           (handle-collision world primary-key collider-key delta-t))
                         world
                         collider-keys))
               world
               pairs)))
