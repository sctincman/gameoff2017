(ns gameoff.render
  (:require [cljsjs.three]
            [reagent.core :as reagent]
            [gameoff.signals :as signals]))

(defn ^:export frames
  "Returns a signal that triggers when a new frame needs to be rendered, with the value of the absolute time. CLJS uses `requestAnimationFrame`. The request id of the call is stored in the properties map atom as `:request-id`."
  []
  (let [out-signal (signals/signal (system-time) "frames")]
    (letfn [(callback [time]
              (signals/propagate out-signal time)
              (let [request-id (.requestAnimationFrame js/window callback)]
                (swap! (get out-signal :properties)
                       assoc :request-id request-id)))]
      (callback (system-time))
      out-signal)))

(def view-angle 75)
(def aspect 1)
(def near 0.1)
(def far 1000)

(defn create-renderer
  ([]
   (doto (js/THREE.WebGLRender. {:antialias true})
     (.setPixelRatio js/window.devicePixelRatio)
     (.setSize 500 500)))
  ([element]
   (doto (js/THREE.WebGLRenderer. #js {:canvas element :antialias true})
     (.setPixelRatio js/window.devicePixelRatio)
     (.setSize 500 500))))

(defn init-renderer
  [state canvas]
  (let [scene    (js/THREE.Scene.)
        p-camera (js/THREE.PerspectiveCamera.
                  view-angle aspect near far)
        box      (js/THREE.BoxGeometry.
                  200 200 200)
        mat      (js/THREE.MeshBasicMaterial.
                  (js-obj "color" 0xbbbbbb
                          "wireframe" true))
        mesh     (js/THREE.Mesh. box mat)
        renderer (create-renderer canvas)
        backend  {:scene scene
                  :camera p-camera
                  :mesh mesh
                  :renderer renderer}]
    
    ;;Change the starting position of cube and camera
    (aset p-camera "name" "p-camera")
    (aset p-camera "position" "z" 500)
    (aset mesh "rotation" "x" 45)
    (aset mesh "rotation" "y" 0)

    ;;Add camera, mesh and box to scene and then that to DOM node.
    (.add scene p-camera)
    (.add scene mesh)

    (assoc state :backend backend)))

(defn js-renderer
  ([state] (js-renderer state js/document.body))
  ([state parent]
   (let [r (create-renderer)]
     (.appendChild parent (.-domElement r)))))
