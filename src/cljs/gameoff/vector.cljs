(ns gameoff.vector)

(def ^:export zero {:x 0, :y 0, :z 0})

(defn ^:export vector
  ([x] (vector [x 0 0]))
  ([x y] (vector [x y 0]))
  ([x y z] {:x x, :y y, :z z}))

(defn add
  ([u v]
   {:x (+ (:x u) (:x v))
    :y (+ (:y u) (:y v))
    :z (+ (:z u) (:z v))})
  ([u v w]
   {:x (+ (:x u) (:x v) (:x w))
    :y (+ (:y u) (:y v) (:y w))
    :z (+ (:z u) (:z v) (:y w))}))

(defn scale [a v]
  {:x (* a (:x v))
   :y (* a (:y v))
   :z (* a (:y v))})

(defn dot [u v]
  {:x (* (:x u) (:x v))
   :y (* (:y u) (:y v))
   :z (* (:z u) (:z v))})

;;TODO defprotocol and records for vector math
;;; TODODO  use a real vector library
