(ns gameoff.render.threejs.core
  (:require [gameoff.render.core :as render]
            [gameoff.render.threejs.sprite :as sprite]
            [gameoff.render.threejs.texture :as texture]
            [cljsjs.three]
            [cljsjs.three-examples.loaders.GLTFLoader]
            [cljsjs.three-examples.loaders.MTLLoader]
            [cljsjs.three-examples.loaders.OBJLoader2]))

(defn recurse-children
  [threejsobj levelstring]
  (let [children (aget threejsobj "children")]
    (println levelstring (aget threejsobj "name") (count children))
    (when (pos? (count children))
      (doseq [child children]
        (recurse-children child (str levelstring "+"))))))

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

(defn- resize-handler [backend]
  (let [canvas (aget (:renderer backend) "domElement")
        scenes (:scenes backend)]
    (fn [event]
      (println "Resize")
      (let [width (aget canvas "parentElement" "offsetWidth")
            height (aget canvas "parentElement" "offsetHeight")
            aspect (/ width height)
            cameras (map (fn [entry] (get (val entry) :root)) (get @scenes :cameras))]
        (.setSize (:renderer backend) width height)
        (when (pos? (count cameras))
          (loop [camera (first cameras)
                 the-rest (rest cameras)]
            (aset camera "aspect" aspect)
            (.updateProjectionMatrix camera)
            (when (pos? (count the-rest))
              (recur (first the-rest) (rest the-rest)))))))))

(defn ^:export load-gltf
  [backend path]
  (let [scenes (:scenes backend)
        gltf-loader (js/THREE.GLTFLoader.)]
    (.load gltf-loader path
           (fn [gltf]
             (let [cameras
                   (into {}
                         (map (fn [child]
                                {(keyword (str (aget child "name")))
                                 {:root child}})
                              (aget gltf "cameras")))
                   animations
                   (into {}
                         (map (fn [child]
                                {(keyword (str (aget child "name")))
                                 {:root child}})
                              (aget gltf "animations")))
                   loaded-scenes
                   (into {}
                         (map (fn [scene]
                                (let [mixer (js/THREE.AnimationMixer. scene)
                                      bbhelpers (js/THREE.Group.)]
                                  (.add scene (js/THREE.AmbientLight. 0xffffff))
                                  (set! (.-background scene) (js/THREE.Color. 0x6c6c6c))
                                  (.updateMatrixWorld scene true)
                                  (.updateMatrixWorld bbhelpers true)
                                  ;;(.add scene bbhelpers)
                                  {(keyword (str (aget scene "name")))
                                   {:root scene
                                    :mixer mixer
                                    :bounding-boxes bbhelpers
                                    :children
                                    (into {}
                                          (map (fn [child]
                                                 (.add bbhelpers (js/THREE.BoxHelper. child 0xff0000))
                                                 {(keyword (str (aget child "name")))
                                                  {:root child}})
                                               (aget scene "children")))}}))
                              (aget gltf "scenes")))]
               (swap! scenes (fn [scenes-map]
                               (let [current-animations (:animations scenes-map)
                                     current-cameras (:cameras scenes-map)]
                                 (-> scenes-map
                                     (assoc :animations (into animations current-animations))
                                     (assoc :cameras (into cameras current-cameras))
                                     (into loaded-scenes) ;for now, just overwrite, later try to do smart merge
                                     ))))
               ((resize-handler backend) nil)))
           (fn [b] "Progress event")
           (fn [c] (println "Failed to load " path)))
    backend))

(defn show-bounding-boxes
  ([world]
   (let [status (get world :show-bounding-boxes false)]
     (show-bounding-boxes world (not status))))
  ([world show?]
   (let [current-scene (get-in world [:scene :current-scene])
         scenes @(get-in world [:backend :scenes])
         bbs (get-in scenes [current-scene :bounding-boxes])]
     (println show?)
     (if show?
       (do ( println "Showing") (aset bbs "visisble" true))
       (do (println "hiding") (aset bbs "visisble" false)))
     (println "WAH")
     (assoc world :show-bounding-boxes show?))))

(def obj-loader (js/THREE.OBJLoader2.))
(def mtl-loader (js/THREE.MTLLoader.))

(defn- create-object
  [backend scene parent desc id]
  (println "Creating object " desc)
  (when-let [parent-root (get-in @(:scenes backend)
                                 [scene :children parent :root])]
    (if (= :obj (:type desc))
      (when (string? (:material desc))
        (let [pivot (js/THREE.Object3D.)]
          (swap! (:scenes backend) assoc-in [scene :children parent :children id]
                 {:root pivot :children {}})
          (.add parent-root pivot)
          (.setPath mtl-loader (:path desc))
          (.setCrossOrigin mtl-loader "anonymous")
          (.load mtl-loader (:material desc)
                 (fn [materials]
                   (.setSceneGraphBaseNode obj-loader pivot)
                   (.preload materials)
                   (.setMaterials obj-loader (aget materials "materials"))
                   (.setPath obj-loader (:path desc))
                   (.load obj-loader
                          (:geom desc)
                          (fn [event]
                            event))))))
      (let [geometry (js/THREE.BoxGeometry. 2 2 2)
            material (js/THREE.MeshStandardMaterial. (js-obj "color" 0x0bbbbb "wireframe" false))
            mesh (js/THREE.Mesh. geometry material)]
        (aset mesh "name" (name id))
        (.add parent-root mesh)
        (swap! (:scenes backend) assoc-in [scene :children parent :children id]
               {:root mesh :children {}})))))

(defn- update-object
  [entity mesh]
  (when-let [[x y z] (:position entity)]
    (.set (.-position mesh) x y z))
  (when-let [[x y z w] (:rotation entity)]
    (.set (.-quaternion mesh) x y z w)))

(defrecord ^:export ThreeJSBackend [renderer scenes]
  render/IRenderBackend
  (render-backend [this world delta-t]
    (let [scene (get-in world [:scene :current-scene] :default)
          camera (get-in world [:scene :camera] :default)]
      (when (some? (get @scenes scene))
        (doseq [[id entity] world]
          (when (render/renderable? entity)
            (when-let [obj (get-in @scenes [scene :children id :root])]
              (update-object entity obj))
            (when-not (empty? (:renders entity))
              (doseq [[render-id render-desc] (:renders entity)]
                (let [children (get-in @scenes [scene :children id :children])]
                  (if (contains? children render-id)
                    (when-let [obj (get-in children [render-id :root])]
                      ;; update animationclips/animationactions based on state
                      (update-object render-desc obj))
                    (create-object this scene id render-desc render-id)))))))
        (.update (get-in @scenes [scene :mixer]) (* 0.001 delta-t))
        (.render renderer
                 (get-in @scenes [scene :root])
                 (get-in @scenes [:cameras camera :root])))
      world)))

(defn- create-renderer
  ([]
   (doto (js/THREE.WebGLRenderer. #js {:antialias true})
     (.setPixelRatio js/window.devicePixelRatio)))
  ([element]
   (doto (js/THREE.WebGLRenderer. #js {:canvas element :antialias true})
     (.setPixelRatio js/window.devicePixelRatio))))

(defn- on-ready [backend]
  (fn [event]
    (when (= "complete" (aget js/document "readyState"))
      ((resize-handler backend) nil))))

(defn ^:export init-renderer
  [canvas]  
  (let [backend (->ThreeJSBackend (create-renderer canvas) (atom {}))]
    (.addEventListener js/window "resize" (resize-handler backend))
    backend))

(defn ^:export setup-scene
  [backend state]
  (if (some? (:include state))
    (load-gltf backend (:include state))
    (let [scene    (js/THREE.Scene.)
          camera (js/THREE.PerspectiveCamera.
                    75 1 0.1 1000)
          light (js/THREE.AmbientLight. 0xffffff)
          light2 (js/THREE.PointLight. 0xffffff 2 0)]
      (set! (.-background scene) (js/THREE.Color. 0x6c6c6c))
      (.add scene light)
      (.set (.-position light2) 200 200 700)
      (.add scene light2)
      (aset camera "name" "camera")
      (aset camera "position" "z" 10)
      (.add scene camera)
      (swap! (:scenes backend)
             (fn [scenes-map]
               (-> scenes-map
                   (assoc :default
                          {:root scene
                           :mixer (js/THREE.AnimationMixer.)
                           :children
                           {:sun {:root light}
                            :camera {:root camera}}})
                   (assoc :cameras
                          {:default {:root camera}}))))
      (.addEventListener js/document "readystatechange" (on-ready backend))
      ((resize-handler backend) nil)
      backend)))

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
