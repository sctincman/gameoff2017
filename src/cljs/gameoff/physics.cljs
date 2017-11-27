(ns gameoff.physics
  (:require [gameoff.signals :as s]
            [clojure.core.matrix :as m]))

(defrecord ^:export BodyComponent [mass velocities accelerations])

;;hmm body will need all forces, not just movement, hack for just movement now
(defn ^:export body [entity mass speed]
  (assoc entity :body (->BodyComponent mass {:forces [0 0 0]} {:forces [0 0 0]})))


;;have body listen to a forces signal
;;foldp over body/time to propagate?
(defn physical? [entity]
  (and (:body entity)
       (:position entity)))

(defn accelerate [body delta-t]
  (let [acceleration (reduce m/add [0 0 0]
                             (vals (get-in body [:body :accelerations])))]    
    (update-in body [:body :velocities]
               (fn [velocities]
                 (update velocities :forces
                         m/add
                         (m/mmul delta-t acceleration))))))

(defn velocitate [body delta-t]
  (let [velocity (reduce m/add [0 0 0]
                         (vals (get-in body[:body :velocities])))]    
    (update-in body [:position] m/add (m/mmul delta-t velocity))))

(defn propagate
  ([body delta-t]
   (if (physical? body)
     (-> body
         (accelerate delta-t)
         (velocitate delta-t))
     body))
  ([delta-t]
   (fn [xform]
     (fn
       ([] (xform))
       ([world] (xform world))
       ([world entity]
        (let [[id body] (first entity)]
          (xform world {id (propagate body delta-t)})))))))

(defn ^:export update-bodies
  [entities delta-t]
  (reduce-kv (fn [entities id entity]
               (if (physical? entity)
                 (assoc entities id (propagate entity delta-t))
                 entities))
             entities
             entities))
