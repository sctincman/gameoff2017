(ns gameoff.quaternion
  (:require [clojure.core.matrix :as m]))

(defn ^:export axis-angle->q [axis angle]
  (let [half-angle (/ angle 2)
        s (Math/sin half-angle)]
    (conj (m/mmul axis s)
          (Math/cos half-angle))))

(defn ^:export qmul
  ([qa qb]
   (let [[qax qay qaz qaw] qa
         [qbx qby qbz qbw] qb]
     [(+ (* qax qbw)    (* qaw qbx)     (* qay qbz)  (- (* qaz qby)))
      (+ (* qay qbw)    (* qaw qby)     (* qaz qbx)  (- (* qax qbz)))
      (+ (* qaz qbw)    (* qaw qbz)     (* qax qby)  (- (* qay qbx)))
      (+ (* qaw qbw) (- (* qax qbx)) (- (* qay qby)) (- (* qaz qbz)))]))
  ([qa qb qc]
   (qmul qa (qmul qb qc))))

(defn ^:export length [q]
  (Math.sqrt (reduce (fn [acc i]
                       (+ acc (* i i)))
                     0
                     q)))

(defn ^:export conjugate [q]
  (let [[x y z w] q]
    [(- x) (- y) (- z) w]))

(defn ^:export normalize [q]
  (let [l (length q)]
    (if (= l 0)
      [0 0 0 1]
      (let [il (/ 1 l)]
        (map #(* % il) q)))))

(defn ^:export inverse [q]
  (normalize (conjugate q)))
