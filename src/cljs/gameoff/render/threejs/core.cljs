(ns gameoff.render.threejs.core
  (:require [gameoff.render.core :as render]
            [gameoff.render.threejs.sprite :as sprite]
            [gameoff.render.threejs.texture :as texture]
            [cljsjs.three]))

(defrecord ThreeJSRenderComponent [backend]
  render/IRenderable
  (prepare [this entity]
    (when (some? (:position entity))
      (set! (.-x (.-position (:object backend)))
            (get-in entity [:position :x]))
      (set! (.-y (.-position (:object backend)))
            (get-in entity [:position :y]))
      (set! (.-z (.-position (:object backend)))
            (get-in entity [:position :z])))))

(defn ^:export create-program [gl vertex-source fragment-source]
  (let [program (.createProgram gl)
        vertex-shader (.createShader gl (.-VERTEX_SHADER gl))
        fragment-shader (.createShader gl (.-FRAGMENT_SHADER gl))]
    (.shaderSource gl vertex-shader vertex-source)
    (.shaderSource gl fragment-shader fragment-source)
    (.compileShader gl vertex-shader)
    (.compileShader gl fragment-shader)
    (.attachShader gl program vertex-shader)
    (.attachShader gl program fragment-shader)
    (.linkProgram gl program)
    program))

(defrecord ^:export ThreeJSBackend [renderer scene objects]
  render/IRenderBackend
  (add-to-backend [this renderable]
    (.add scene (get renderable :object))
    renderable)
  (render [this entities camera]
    (let [entities (doall (render/prepare-scene this (render/animate entities 0.0)))
          cam (:object (first (:renders (get entities camera))))]
      (.render renderer scene cam)
      entities))
  (prepare-scene [this entities]
    (reduce-kv (fn [entities id entity]
                 (when (render/renderable? entity)
                   (loop [render (first (:renders entity))
                          the-rest (rest (:renders entity))]
                     (render/prepare render entity)
                     (when-not (empty? the-rest)
                       (recur (first the-rest) (rest the-rest)))))
                 entities)
               entities
               entities))
  (test-cube [this]
    (let [geometry (js/THREE.BoxGeometry. 200 200 200)
          material (js/THREE.MeshStandardMaterial. (js-obj "color" 0xff0040 "wireframe" false))
          mesh (js/THREE.Mesh. geometry material)]
      (.add scene mesh)
      (->ThreeJSRenderComponent {:object mesh, :material material, :geometry geometry})))
  (create-sprite [this texture]
    (let [base-texture (if (satisfies? render/ITextureAtlas texture)
                         (:texture texture)
                         texture)
          js-texture (:texture base-texture) 
          material (js/THREE.SpriteMaterial.)
          sprite (js/THREE.Sprite. material)]
      (set! (.-map material) js-texture)
      (set! (.-lights material) true)
      (set! (.-needsUpdate material) true)
      (set! (.-x (.-scale sprite)) (:width base-texture))
      (set! (.-y (.-scale sprite)) (:height base-texture))
      (.add scene sprite)
      (sprite/->ThreeJSSprite sprite texture nil 1.0 1.0)))
  (create-sprite2 [this texture]
    (let [vertices (js/Float32Array.
                    [-0.5, -0.5,  0.0,
                      0.5, -0.5,  0.0,
                      0.5,  0.5,  0.0,
                     -0.5,  0.5,  0.0])
          uvs (js/Float32Array.
               [0.0, 0.0,
                1.0, 0.0,
                1.0, 1.0,
                0.0, 1.0])
          indices (js/Uint16Array.
                   [0, 1, 2,
                    0, 2, 3])
          base-texture (if (satisfies? render/ITextureAtlas texture)
                         (:texture texture)
                         texture)
          js-texture (:texture base-texture)
          geometry (js/THREE.BufferGeometry.)
          material (js/THREE.MeshBasicMaterial.
                    (js-obj "map" js-texture
                            "wireframe" false
                            "transparent" true))]
      (.addAttribute geometry "position" (js/THREE.BufferAttribute. vertices 3))
      (.addAttribute geometry "uv" (js/THREE.BufferAttribute. uvs 2))
      (.setIndex geometry (js/THREE.BufferAttribute. indices 1))
      (.computeBoundingBox geometry)
      (let [mesh (js/THREE.Mesh. geometry material)]
        (set! (.-x (.-scale mesh)) (:width base-texture))
        (set! (.-y (.-scale mesh)) (:height base-texture))
        (.add scene mesh)
        (->ThreeJSRenderComponent {:object mesh, :material material, :geometry geometry})))))

(defn- create-renderer
  ([]
   (doto (js/THREE.WebGLRender. {:antialias true})
     (.setPixelRatio js/window.devicePixelRatio)
     (.setSize 500 500)))
  ([element]
   (doto (js/THREE.WebGLRenderer. #js {:canvas element :antialias true})
     (.setPixelRatio js/window.devicePixelRatio)
     (.setSize 500 500))))

(defn ^:export init-renderer
  [state canvas]
  (let [scene    (js/THREE.Scene.)
        mat      (js/THREE.MeshBasicMaterial.
                  (js-obj "color" 0xbbbbbb
                          "wireframe" true))
        box      (js/THREE.BoxGeometry.
                  200 200 200)
        mesh     (js/THREE.Mesh. box mat)
        p-camera (js/THREE.PerspectiveCamera.
                  75 1 0.1 1000)
        renderer (create-renderer canvas)
        ;light (js/THREE.AmbientLight. 0xffffff)
        ;light2 (js/THREE.PointLight. 0xffffff 2 0)
        backend  {:scene scene
                  :camera p-camera
                  :mesh mesh
                  :renderer renderer}]
    
    (set! (.-background scene) (js/THREE.Color. 0x6c6c6c))
    (comment (.add scene light))
    (comment (.set (.-position light2) 300 300 400)
             (.add scene light2))

    (aset p-camera "name" "p-camera")
    (aset p-camera "position" "z" 500)
    (aset mesh "rotation" "x" 45)
    (aset mesh "rotation" "y" 0)
    (.add scene p-camera)
    (.add scene mesh)

    (assoc backend :obj (->ThreeJSBackend renderer scene []))))

(defn ^:export js-renderer
  ([state] (js-renderer state js/document.body))
  ([state parent]
   (let [r (create-renderer)]
     (.appendChild parent (.-domElement r)))))

(comment
  (defn ^:export load-texture! [loader [key uri] rest-uris resources start-func]
    (.load loader uri
           (fn [js-texture]
             (set! (.-magFilter js-texture) js/THREE.NearestFilter)
             (set! (.-needsUpdate js-texture) true)
             (let [accum (assoc resources key
                                (texture/->ThreeJSTexture
                                 js-texture
                                 (.-width (.-image js-texture))
                                 (.-height (.-image js-texture))))]
               (if (empty? rest-uris)
                 (start-func (create-threejs-backend!) accum)
                 (load-texture! loader (first rest-uris) (rest rest-uris) accum start-func)))))))

(comment
  (defn ^:export load-resources! [start-func]
    (let [loader (js/THREE.TextureLoader.)
          textures {:placeholder "assets/images/placeholder.png"
                    :deer "assets/images/deer.png"
                    :background "assets/images/test-background.png"
                    :forest-0 "assets/images/forest-0.png"
                    :forest-1 "assets/images/forest-1.png"
                    :forest-2 "assets/images/forest-2.png"
                    :forest-3 "assets/images/forest-3.png"}]
      (load-texture! loader (first textures) (rest textures) {} start-func))))
