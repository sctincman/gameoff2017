(ns gameoff.render.threejs.sprite
  (:require [gameoff.render.core :as render]
            [gameoff.render.threejs.texture :as texture]
            [cljsjs.three]))

(defrecord ^:export ThreeJSTiling [object texture atlas-key scale-x scale-y]
  render/ISprite
  (set-texture [this texture]
    (let [js-texture (:texture texture)]
      (set! (.-map (.-material object)) js-texture)
      (set! (.-needsUpdate (.-material object)) true)
      (set! (.-x (.-scale object)) (:width texture))
      (set! (.-y (.-scale object)) (:height texture))
      (assoc this :texture texture)))
  (key-texture [this key]
    (if (satisfies? render/ITextureAtlas texture)
      (let [sub (render/getsub texture key)
            js-texture (:texture sub)]
        (set! (.-map (.-material object)) js-texture)
        (set! (.-needsUpdate (.-material object)) true)
        (set! (.-x (.-scale object)) (* scale-x (:width sub)))
        (set! (.-y (.-scale object)) (* scale-y (:height sub)))
        (assoc this :atlas-key key))
      this))
  (scale [this value]
    (let [sub (if (satisfies? render/ITextureAtlas texture)
                (render/getsub texture atlas-key)
                texture)]
      (set! (.-x (.-scale object)) (* value (:width sub)))
      (set! (.-y (.-scale object)) (* value (:height sub)))
      (-> this
          (assoc scale-x value)
          (assoc scale-y value))))
  render/IRenderable
  (prepare [this entity]
    (when (some? (:position entity))
      (set! (.-x (.-position (:object this)))
            (get-in entity [:position :x]))
      (set! (.-y (.-position (:object this)))
            (get-in entity [:position :y]))
      (set! (.-z (.-position (:object this)))
            (get-in entity [:position :z])))))

(defn ^:export create-tile [positions]
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
        offsets (js/Float32Array. positions)
        base-texture (if (satisfies? render/ITextureAtlas texture)
                       (:texture texture)
                       texture)
        js-texture (:texture base-texture)
        geometry (js/THREE.InstancedBufferGeometry.)
        
        material (js/THREE.MeshBasicMaterial.
                  (js-obj "map" js-texture
                          "wireframe" false
                          "transparent" true))]
    (.addAttribute geometry "position" (js/THREE.BufferAttribute. vertices 3))
    (.addAttribute geometry "uv" (js/THREE.BufferAttribute. uvs 2))
    (.addAttribute geometry "offset" (js/THREE.BufferAttribute. offsets 3))
    (.setIndex geometry (js/THREE.BufferAttribute. indices 1))
    (.computeBoundingBox geometry)
    (let [mesh (js/THREE.Mesh. geometry material)]
      (set! (.-x (.-scale mesh)) (:width base-texture))
      (set! (.-y (.-scale mesh)) (:height base-texture))
      (.add scene mesh)
      (render/->ThreeJSRenderComponent {:object mesh, :material material, :geometry geometry}))))

;;need to make a custom shader that adds offsets to position...

(def sprite-vertex-shader
  "precision highp float;

void main() {
	vUV = uvOffset + uv * uvScale;

	vec2 alignedPosition = position * scale;
	
	vec2 rotatedPosition;
	rotatedPosition.x = cos( rotation ) * alignedPosition.x - sin( rotation ) * alignedPosition.y;
	rotatedPosition.y = sin( rotation ) * alignedPosition.x + cos( rotation ) * alignedPosition.y;
	
	vec4 finalPosition;

	finalPosition = modelViewMatrix * vec4( 0.0, 0.0, 0.0, 1.0 );
	finalPosition.xy += rotatedPosition;
	finalPosition = projectionMatrix * finalPosition;
	
	gl_Position = finalPosition;
}")



(def sprite-fragment-shader
  "precision highp float;

uniform vec3 color;
uniform sampler2D map;
uniform float opacity;

uniform int fogType;
uniform vec3 fogColor;
uniform float fogDensity;
uniform float fogNear;
uniform float fogFar;
uniform float alphaTest;
	
varying vec2 vUV;
	
void main() {
	vec4 texture = texture2D( map, vUV );
	
	if ( texture.a < alphaTest ) discard;
	
	gl_FragColor = vec4( color * texture.xyz, texture.a * opacity );
	
	if ( fogType > 0 ) {
	
		float depth = gl_FragCoord.z / gl_FragCoord.w;
		float fogFactor = 0.0;
	
		if ( fogType == 1 ) {
			
			fogFactor = smoothstep( fogNear, fogFar, depth );
	
		} else {
	
			const float LOG2 = 1.442695;
			fogFactor = exp2( - fogDensity * fogDensity * depth * depth * LOG2 );
			fogFactor = 1.0 - clamp( fogFactor, 0.0, 1.0 );
			
		}
	
		gl_FragColor = mix( gl_FragColor, vec4( fogColor, gl_FragColor.w ), fogFactor );
	
	}
}")
