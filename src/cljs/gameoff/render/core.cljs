(ns gameoff.render.core
  (:require [gameoff.signals :as signals]))

(defn ^:export frames
  "Returns a signal that triggers when a new frame needs to be rendered, with the value of the absolute time. CLJS uses `requestAnimationFrame`. The request id of the call is stored in the properties map atom as `:request-id`."
  [& {:keys [out-signal]
      :or {out-signal (signals/signal (system-time) :prefix "frames")}}]
  (letfn [(callback [time]
            (signals/propagate out-signal time)
            (let [request-id (.requestAnimationFrame js/window callback)]
              (swap! (get out-signal :properties)
                     assoc :request-id request-id)))]
    (callback (system-time))
    out-signal))

(defprotocol ^:export ITexture
  "Abstract the details of a texture into this standard interface."
  (subtexture [this offset-x offset-y width height] "Returns a new texture that is a subsection of the parent.")
  (magnification-filter [this filter] "Sets the filter used when upscaling.")
  (minification-filter [this filter] "Sets the filter used when downscaling.")
  (splice [this offset width height] "Splices base texture regularly. Key'd by {x,y}. Negative indexes flip on the respective axis.")
  (width [this])
  (height [this]))

(defprotocol ^:export ITextureAtlas
  "Reference subtextures in a parent texture"
  (defsub [this key offset-x offset-y width height] "Assign a key to a subregion of the texture")
  (getsub [this key] "Fetch subtexture by key"))

(defprotocol ^:export ISprite
  (key-texture [this key])
  (set-texture [this texture])
  (scale [this value]))

(defprotocol ^:export IRenderBackend
  "Abstract the details of a backend into this standard interface."
  (render-backend [this world delta-t] "Using the list of entities, render and display the scene. Camera is the key of the camera entity to use for rendering."))

(defn ^:export render [world delta-t]
  (render-backend (:backend world) world delta-t))

(defprotocol ^:export ICamera
  "Abstract camera"
  (look-at [this point] "Rotate camera to look at a point"))

(defn ^:export animation-frames [animation]
  (signals/map (fn [frames]
           (first frames))
         (signals/foldp (fn [state step]
                    (rest state))
                  (cycle (:frames animation))
                  (signals/tick (:duration animation)))))

(defn ^:export add-animation [entity key animation]
  (assoc-in entity [:animations key]
            (animation-frames animation)))

(defn ^:export set-animation [entity key]
  (let [animation (get (:animations entity) key)]
    (if (some? animation)
      (assoc entity :current-frame animation)
      entity)))

(defn ^:export state-animate [entity states signal]
  (assoc entity :current-frame
         (signals/switch (signals/map (fn [state]
                            (get states (:state state)))
                          signal)
                   (:animations entity))))

(defn ^:export animate
  [entities delta-t]
  (reduce-kv (fn [entities id entity]
               (if (and (some? (:current-frame entity))
                        (not (empty? (:renders entity))))
                 (assoc entities id
                        (assoc entity :renders
                               (map (fn [renderer]
                                      (if (satisfies? ISprite renderer)
                                        (key-texture renderer (signals/value (:current-frame entity)))
                                        renderer))
                                    (:renders entity))))
                 entities))
             entities
             entities))

(defn renderable? [entity]
  (some? (and (:renders entity)
              (:position entity))))
