(ns gameoff.render.threejs.camera
  (:require [gameoff.render.core :as render]
            [gameoff.vector :as v]
            [cljsjs.three]))

;; ...needs to be updated to fit new paradigm in threejs/core.cljs
(defn ^:export ThreeJSPerspectiveCamera [fov aspect near far]
  (let [camera (js/THREE.PerspectiveCamera. fov aspect near far)]
    {:position (v/vector 0 0 700)
     :rotation v/zero
     :render {:type [:camera :perspective]}}))

(defn ^:export ThreeJSOrthoCamera [left right top bottom near far]
  (let [camera (js/THREE.OrthographicCamera. left right top bottom near far)]
    {:position (v/vector 0 0 700)
     :rotation v/zero
     :render {:type [:camera :orthographic]}}))

