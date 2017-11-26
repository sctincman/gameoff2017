(ns gameoff.render.threejs.camera
  (:require [gameoff.render.core :as render]
            [cljsjs.three]))

;; ...needs to be updated to fit new paradigm in threejs/core.cljs
(defn ^:export ThreeJSPerspectiveCamera [fov aspect near far]
  (let [camera (js/THREE.PerspectiveCamera. fov aspect near far)]
    {:position [0.0 0.0 700.0]
     :rotation [0.0 0.0 0.0]
     :render {:type [:camera :perspective]}}))

(defn ^:export ThreeJSOrthoCamera [left right top bottom near far]
  (let [camera (js/THREE.OrthographicCamera. left right top bottom near far)]
    {:position [0.0 0.0 700.0]
     :rotation [0.0 0.0 0.0]
     :render {:type [:camera :orthographic]}}))

