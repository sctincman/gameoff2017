(ns gameoff.render.threejs.camera
  (:require [gameoff.render.core :as render]
            [gameoff.vector :as v]
            [cljsjs.three]))

(defrecord ^:export ThreeJSCamera [object]
  render/IRenderable
  (prepare [this entity]
    (when (some? (:position entity))
      (set! (.-x (.-position object))
            (get-in entity [:position :x]))
      (set! (.-y (.-position object))
            (get-in entity [:position :y]))
      (set! (.-z (.-position object))
            (get-in entity [:position :z])))))

(defn ^:export ThreeJSPerspectiveCamera [fov aspect near far]
  (let [camera (js/THREE.PerspectiveCamera. fov aspect near far)
        entity (-> {}
                   (assoc :position (v/vector 0 0 700))
                   (assoc :rotation nil)
                   (assoc :camera {:type :perspective}))]
    (update entity :renders conj
            (->ThreeJSCamera camera))))

(defn ^:export ThreeJSOrthoCamera [left right top bottom near far]
  (let [camera (js/THREE.OrthographicCamera. left right top bottom near far)
        entity (-> {}
                   (assoc :position (v/vector 0 0 700))
                   (assoc :rotation nil)
                   (assoc :camera {:type :orthographic}))]
    (update entity :renders conj
            (->ThreeJSCamera camera))))

