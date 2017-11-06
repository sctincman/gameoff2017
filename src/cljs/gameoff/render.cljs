(ns gameoff.render
  (:require [cljsjs.three]
            [reagent.core :as reagent]))

(def view-angle 75)

(def aspect 1)

(def near 0.1)

(def far 1000)

(defn create-renderer [element]
  (doto (js/THREE.WebGLRenderer. #js {:canvas element :antialias true})
    (.setPixelRatio js/window.devicePixelRatio)
    (.setSize 500 500)))

(defn reagent-renderer [attributes camera scene tick]
  (let [requested-animation (atom nil)]
    (reagent/create-class
     {:display-name "threejs-canvas"
      :reagent-render
      (fn threejs-canvas-render []
        [:canvas attributes])
      :component-did-mount
      (fn threejs-canvas-did-mount [this]
        (let [e (reagent/dom-node this)
              r (create-renderer e)]
          ((fn animate []
             (tick)
             (.render r scene camera)
             (reset! requested-animation (.requestAnimationFrame js/window animate))))))
      :component-will-unmount
      (fn [this]
        (.cancelAnimationFrame js/window @requested-animation))})))

(defn renderer [state]

  ;;First initiate the basic elements of a THREE scene
  (let [scene    (js/THREE.Scene.)
        p-camera (js/THREE.PerspectiveCamera.
                    view-angle aspect near far)
        box      (js/THREE.BoxGeometry.
                    200 200 200)
        mat      (js/THREE.MeshBasicMaterial.
                    (js-obj "color" 0xff0000
                            "wireframe" true))
        mesh     (js/THREE.Mesh. box mat)]

    ;;Change the starting position of cube and camera
    (aset p-camera "name" "p-camera")
    (aset p-camera "position" "z" 500)
    (aset mesh "rotation" "x" 45)
    (aset mesh "rotation" "y" 0)

    ;;Add camera, mesh and box to scene and then that to DOM node.
    (.add scene p-camera)
    (.add scene mesh)
    ;(.appendChild js/document.body (.-domElement renderer))

    (defn animate []
      (aset mesh "rotation" "y" (+ 0.01 (.-y (.-rotation mesh)))))
    (reagent-renderer nil p-camera scene animate)))
