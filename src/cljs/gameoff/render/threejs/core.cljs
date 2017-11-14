(ns gameoff.render.threejs.core
  (:require [gameoff.render.core :as render]
            [gameoff.render.threejs.sprite :as sprite]
            [gameoff.render.threejs.texture :as texture]
            [cljsjs.three]))

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

(defn- create-object
  [id desc]
  (let [geometry (js/THREE.BoxGeometry. 200 200 200)
        material (js/THREE.MeshStandardMaterial. (js-obj "color" 0xbbbbbb "wireframe" true))
        mesh (js/THREE.Mesh. geometry material)]
    {:mesh mesh, :material material, :geometry geometry}))

(defn- update-object
  [parent desc {mesh :mesh}]
  (when (some? (:position parent))
    (set! (.-x (.-position mesh))
          (get-in parent [:position :x]))
    (set! (.-y (.-position mesh))
          (get-in parent [:position :y]))
    (set! (.-z (.-position mesh))
          (get-in parent [:position :z])))
  (when (some? (:rotation parent))
    (set! (.-x (.-rotation mesh))
          (get-in parent [:rotation :x]))
    (set! (.-y (.-rotation mesh))
          (get-in parent [:rotation :y]))
    (set! (.-z (.-rotation mesh))
          (get-in parent [:rotation :z]))))

(defrecord ^:export ThreeJSBackend [renderer scene objects]
  render/IRenderBackend
  (render [this entities camera]
    (comment (let [cam (:object (first (:renders (get entities camera))))
                   entities (doall (render/prepare-scene this (render/animate entities 0.0)))
                   ]
               (.render renderer scene cam)
               entities)))
  (renderx [this camera]
    (fn [xform]
      (fn
        ([] (xform))
        ([result]
         (.render renderer scene camera)
         (xform result))
        ([result input]
         (let [[id entity] (first input)]
           (when (render/renderable? entity)
             (loop [[render-id render] (first (:renders entity))
                    the-rest (rest (:renders entity))]
               (when-not (contains? @objects (keyword id render-id))
                 (let [obj (create-object render-id render)]
                   (.add scene (:mesh obj))
                   (swap! objects assoc (keyword id render-id)
                          obj)))
               (update-object entity render (get @objects (keyword id render-id)))
               (when-not (empty? the-rest)
                 (recur (first the-rest) (rest the-rest))))))    
         (xform result input)))))
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
      (sprite/->ThreeJSSprite sprite texture nil 1.0 1.0))))

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
        p-camera (js/THREE.PerspectiveCamera.
                  75 1 0.1 1000)
        renderer (create-renderer canvas)
        ;light (js/THREE.AmbientLight. 0xffffff)
        ;light2 (js/THREE.PointLight. 0xffffff 2 0)
        backend  {:scene scene
                  :camera p-camera
                  :renderer renderer}]
    
    (set! (.-background scene) (js/THREE.Color. 0x6c6c6c))
    (comment (.add scene light))
    (comment (.set (.-position light2) 300 300 400)
             (.add scene light2))

    (aset p-camera "name" "p-camera")
    (aset p-camera "position" "z" 500)
    (.add scene p-camera)

    (assoc backend :obj (->ThreeJSBackend renderer scene (atom {})))))

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
