(ns gameoff.physics
  (:require [gameoff.signals :as s]
            [clojure.core.matrix :as m]))

(defrecord ^:export BodyComponent [mass velocities accelerations a-total v-total])

;;hmm body will need all forces, not just movement, hack for just movement now
(defn ^:export body [entity mass speed]
  (assoc entity :body (->BodyComponent mass {:forces [0 0 0]} {:forces [0 -0.00005 0]} [0 0 0] [0 0 0])))


;;have body listen to a forces signal
;;foldp over body/time to propagate?
(defn physical? [entity]
  (and (:body entity)
       (:position entity)))

(defn accelerate [body delta-t]
  (let [acceleration (reduce m/add [0 0 0]
                             (vals (get-in body [:body :accelerations])))]
    (-> body
        (assoc-in [:body :a-total] acceleration)
        (update-in [:body :velocities]
                   (fn [velocities]
                     (update velocities :forces
                             m/add
                             (m/mmul delta-t acceleration)))))))

(defn velocitate [body delta-t]
  (let [velocity (reduce m/add [0 0 0]
                         (vals (get-in body[:body :velocities])))]
    (-> body
        (assoc-in [:body :v-total] velocity)
        (update-in [:position] m/add (m/mmul delta-t velocity)))))

(defn propagate [body delta-t]
  (if (physical? body)
    (-> body
        (accelerate delta-t)
        (velocitate delta-t))
    body))

(defn ^:export step [world delta-t]
  (reduce-kv (fn [world id entity]
               (update world id propagate delta-t))
             world world))
