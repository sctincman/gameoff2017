(ns gameoff.render.threejs.texture
  (:require [gameoff.render.core :as render]
            [cljsjs.three]))

(defrecord ^:export ThreeJSTextureAtlas [texture submaps]
  render/ITexture
  (subtexture [this offset-x offset-y width height]
    (assoc this :texture (render/subtexture texture offset-x offset-y width height)))
  (magnification-filter [this filter]
    (assoc this :texture (render/magnification-filter texture filter)))
  (minification-filter [this filter]
    (assoc this :texture (render/minification-filter texture filter)))
  render/ITextureAtlas
  (defsub [this key offset-x offset-y width height]
    (let [sub (render/subtexture texture offset-x offset-y width height)]
      (assoc-in this [:submaps key] sub)))
  (getsub [this key]
    (get submaps key)))

(defrecord ^:export ThreeJSTexture [texture width height]
  render/ITexture
  (subtexture [this offset-x offset-y width height]
    (let [new-texture (.clone texture)]
      (set! (.-needsUpdate new-texture) true)
      (.set (.-offset new-texture)
            (/ offset-x (.-width (.-image new-texture)))
            (/ offset-y (.-height (.-image new-texture))))
      (when (or (neg? offset-x)
                (neg? offset-y))
        (set! (.-wrapS new-texture) js/THREE.MirroredRepeatWrapping)
        (set! (.-wrapT new-texture) js/THREE.MirroredRepeatWrapping))
      (.set (.-repeat new-texture)
            (/ width (.-width (.-image new-texture)))
            (/ height (.-height (.-image new-texture))))
      (-> this
          (assoc :texture new-texture)
          (assoc :width width)
          (assoc :height height))))
  
  (magnification-filter [this filter]
    (set! (.-magFilter texture)
          (condp = filter
            :linear js/THREE.LinearFilter
            :nearest js/THREE.NearestFilter))
    (set! (.-needsUpdate texture) true)
    this)
  (minification-filter [this filter]
    (set! (.-minFilter texture)
          (condp = filter
            :nearest js/THREE.NearestFilter
            :nearest-mip-nearest js/THREE.NearestMipMapNearestFilter
            :nearest-mip-linear js/THREE.NearestMipMapLinearFilter
            :linear js/THREE.LinearFilter
            :linear-mip-nearest js/THREE.LinearMipMapNearestFilter
            :linear-mip-linear js/THREE.LinearMipMapLinearFilter))
    (set! (.-needsUpdate texture) true)
    this)
  (width [this]
    (.-width (.-image texture)))
  (height [this]
    (.-height (.-image texture)))
  (splice [this offset width height]
    (let [texture (render/magnification-filter this :nearest)
          atlas (->ThreeJSTextureAtlas texture {})
          stride (int (/ (render/width this) width))
          rise   (int (/ (render/height this) height))]
      (reduce (fn [atlas key]
                (render/defsub atlas key
                  (+ (:x offset) (* width (:x key)))
                  (+ (:y offset) (* height (:y key)))
                  width height))
              atlas
              (for [x (range (- stride) stride)
                    y (range (- rise) rise)]
                {:x x, :y y})))))
