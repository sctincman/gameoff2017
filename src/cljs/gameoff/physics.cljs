(ns gameoff.physics
  (:require [gameoff.signals :as s]
            [clojure.core.matrix :as m]))

(defrecord ^:export BodyComponent [velocity acceleration])

;;hmm body will need all forces, not just movement, hack for just movement now
(defn ^:export body [entity mass speed]
  (let [movement-state (:movement entity)
        roation-state (:rotation entity)
        velocity-signal (s/foldp (fn [velocity movement]
                                   (let [heading [1.0 0.0 0.0]]
                                     (condp = (:state movement)
                                       :moving-forward (m/mmul speed heading)
                                       :moving-backward (m/mmul (- speed) heading)
                                       :standing [0.0 0.0 0.0]
                                       nil velocity)))
                                 [0.0 0.0 0.0]
                                 movement-state)
        acceleration-signal (s/signal [0.0 0.0 0.0] "accel")]
    (assoc entity :body
           (->BodyComponent velocity-signal acceleration-signal))))


;;have body listen to a forces signal
;;foldp over body/time to propagate?
(defn physical? [entity]
  (and (:body entity)
       (:position entity)))

(defn accelerate [body delta-t]
  (s/propagate (get-in body [:body :velocity])
               (m/add (s/value (get-in body [:body :velocity]))
                      (m/mmul delta-t
                             (s/value (get-in body [:body :acceleration])))))
  body)

(defn propagate
  ([body delta-t]
   (if (physical? body)
     (-> body
         (update :position
                 m/add
                 (m/mmul delta-t (s/value (get-in body [:body :velocity]))))
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
