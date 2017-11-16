(ns gameoff.physics
  (:require [gameoff.signals :as s]
            [gameoff.vector :as v]))

(defrecord ^:export BodyComponent [velocity acceleration])

;;hmm body will need all forces, not just movement, hack for just movement now
(defn ^:export body [entity mass speed]
  (let [movement-state (:movement entity)
        velocity-signal (s/foldp (fn [velocity movement]
                                   (condp = (:state movement)
                                     :moving-forward {:x speed, :y 0.0, :z 0.0}
                                     :moving-backward {:x (- speed), :y 0.0, :z 0.0}
                                     :standing {:x 0.0, :y 0.0, :z 0.0}
                                     nil velocity))
                                 {:x 0.0, :y 0.0, :z 0.0}
                                 movement-state)
        acceleration-signal (s/signal {:x 0.0, :y 0.0, :z 0.0} "accel")]
    (assoc entity :body
           (->BodyComponent velocity-signal acceleration-signal))))


;;have body listen to a forces signal
;;foldp over body/time to propagate?
(defn physical? [entity]
  (and (:body entity)
       (:position entity)))

(defn accelerate [body delta-t]
  (s/propagate (get-in body [:body :velocity])
               (v/add (s/value (get-in body [:body :velocity]))
                     (v/scale delta-t
                             (s/value (get-in body [:body :acceleration])))))
  body)

(defn propagate
  ([body delta-t]
   (if (physical? body)
     (-> body
         (update :position
                 v/add
                 (v/scale delta-t (s/value (get-in body [:body :velocity]))))
         (accelerate delta-t))
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
