
(use (prefix sdl2 sdl2:)
     (prefix opengl-glew  gl:)
     (prefix gl-utils     gl-utils:)
     (prefix gl-math      gl-math:)
     miscmacros)

(import (prefix sdl2 sdl2:)
        (prefix opengl-glew gl:)
        (prefix gl-utils gl-utils:)
        (prefix gl-math gl-math:)
        miscmacros)

(include "sdl2-setup.scm")


;; ==================== utils ====================

(include "canvas.scm")


(define (p/fill out r g b a)
  (with-output-to-canvas
   out
   (gl:clear-color r g b a)
   (gl:clear gl:+color-buffer-bit+)))

(define (p/pset out   x y   r g b a)
  (with-texture
   (canvas-tex out)
   (gl:tex-sub-image-2d gl:+texture-2d+ ;; target
                        0               ;; level
                        x y 2 2         ;; x y w h
                        gl:+rgb+
                        gl:+unsigned-byte+
                        (gl-utils:->pointer (f32vector r g b   r g b   r g b   r g b))))
  (gl-utils:check-error))

;; (p/pset den  100 100  1 0 0 0)

;; (map canvas-tex (list den den2 vel vel2 divergence prs prs2))

(define p/add

  (let ((program-blue
         (create-program
          #f
          "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform vec2 InverseSize;
uniform float amount;

void main() {
 vec2 fragCoord = gl_FragCoord.xy;
 FragColor = clamp(texture(VelocityTexture, InverseSize * fragCoord) + amount, 0.0, 1.0);
}
")))
    (define $InverseSize (gl:get-uniform-location (program-id program-blue) "InverseSize"))
    (define $amount (gl:get-uniform-location (program-id program-blue) "amount"))

    (lambda (out source amount)
      (with-output-to-canvas
       out
       (with-program
        program-blue
        (gl:uniform2f $InverseSize (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
        (gl:uniform1f $amount amount)

        (gl:bind-texture gl:+texture-2d+ (canvas-tex source))
        (render-square))))))

(define p/advect
  (let ()
    (define prg
      (create-program
       #f
       "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform sampler2D SourceTexture;
//uniform sampler2D Obstacles;

uniform vec2 InverseSize;
uniform float TimeStep;
uniform float Dissipation;

void main() {
    vec2 fragCoord = gl_FragCoord.xy;
//    float solid = texture(Obstacles, InverseSize * fragCoord).x;
//    if (solid > 0) {
//        FragColor = vec4(0);
//        return;
//    }
//
    vec2 u = texture(VelocityTexture, InverseSize * fragCoord).xy;
    vec2 coord = InverseSize * (fragCoord - TimeStep * u);
    FragColor = Dissipation * texture(SourceTexture, coord);
}
"))
    (define VelocityTexture (gl:get-uniform-location (program-id prg) "VelocityTexture"))
    (define SourceTexture   (gl:get-uniform-location (program-id prg) "SourceTexture"))
    (define InverseSize     (gl:get-uniform-location (program-id prg) "InverseSize"))
    (define TimeStep        (gl:get-uniform-location (program-id prg) "TimeStep"))
    (define Dissipation     (gl:get-uniform-location (program-id prg) "Dissipation"))

    (lambda (out  velocity source timestep dissipation)
      (assert (= 2 (canvas-d velocity)))
      (with-output-to-canvas
       out
       (with-program
        prg

        (gl:uniform1i VelocityTexture 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

        (gl:uniform1i SourceTexture   1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex source))

        (gl:active-texture gl:+texture0+)

        (gl:uniform2f InverseSize     (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
        (gl:uniform1f TimeStep timestep)
        (gl:uniform1f Dissipation dissipation)
        (render-square))))))

(define p/splat
  (let ()
    (define prg
      (create-program
       #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform vec2 Point;
uniform float Radius;
uniform vec3 FillColor;

void main() {
    float d = distance(Point, gl_FragCoord.xy);
    if (d < Radius) {
        float a = (Radius - d) * 0.5;
        a = (d/Radius);//min(a, 1.0);
        FragColor = vec4(FillColor, a);
    } else {
        FragColor = vec4(0);
    }
}
"))
    (define Point     (gl:get-uniform-location (program-id prg) "Point"))
    (define Radius    (gl:get-uniform-location (program-id prg) "Radius"))
    (define FillColor (gl:get-uniform-location (program-id prg) "FillColor"))

    (lambda (out  x y radius  r g b)
      (with-output-to-canvas
       out
       (with-program
        prg

        (gl:uniform2f Point     x y)
        (gl:uniform1f Radius    radius)
        (gl:uniform3f FillColor r g b)

        (gl:enable gl:+blend+)
        ;; gl:+one-minus-src-alpha+ wont work on my fairphone2! why!?
        (gl:blend-func gl:+one+ gl:+one+)
        (render-square)
        (gl:disable gl:+blend+)
        )))))

(define p/divergence
  (let ()
    (define prg
      (create-program #f "
#version 300 es
precision mediump float;

out float FragColor;

uniform sampler2D Velocity;
uniform float HalfInverseCellSize;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring velocities:
    vec2 vN = texelFetchOffset(Velocity, T, 0, ivec2(0, 1)).xy;
    vec2 vS = texelFetchOffset(Velocity, T, 0, ivec2(0, -1)).xy;
    vec2 vE = texelFetchOffset(Velocity, T, 0, ivec2(1, 0)).xy;
    vec2 vW = texelFetchOffset(Velocity, T, 0, ivec2(-1, 0)).xy;

    FragColor = HalfInverseCellSize * (vE.x - vW.x + vN.y - vS.y);
}
"))
    (define Velocity            (gl:get-uniform-location (program-id prg) "Velocity"))
    (define HalfInverseCellSize (gl:get-uniform-location (program-id prg) "HalfInverseCellSize"))

    (lambda (out velocity)

      (with-output-to-canvas
       out
       (with-program
        prg

        (gl:uniform1i Velocity 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

        (gl:uniform1f HalfInverseCellSize (/ 0.5 1.25))
        (render-square))))))

(define p/jacobi
  (let ()

    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D Pressure;
uniform sampler2D Divergence;

uniform float Alpha;
uniform float InverseBeta;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    vec4 pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1));
    vec4 pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1));
    vec4 pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0));
    vec4 pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0));
    vec4 pC = texelFetch(Pressure, T, 0);

    vec4 bC = texelFetch(Divergence, T, 0);
    FragColor = (pW + pE + pS + pN + Alpha * bC) * InverseBeta;
}
"))

    (define Pressure    (gl:get-uniform-location (program-id prg) "Pressure"))
    (define Divergence  (gl:get-uniform-location (program-id prg) "Divergence"))
    (define Alpha       (gl:get-uniform-location (program-id prg) "Alpha"))
    (define InverseBeta (gl:get-uniform-location (program-id prg) "InverseBeta"))

    (lambda (out pressure divergence)
      (with-output-to-canvas
       out
       (with-program
        prg


        (define CellSize 1.25)

        (gl:uniform1i Divergence 1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex divergence))

        (gl:uniform1i Pressure 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

        (gl:uniform1f Alpha (* (- CellSize) CellSize))
        (gl:uniform1f InverseBeta 0.25)

        (render-square))))))

(define p/subtract-gradient
  (let ()
    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec2 FragColor;

uniform sampler2D Velocity;
uniform sampler2D Pressure;
uniform float GradientScale;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    float pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1)).r;
    float pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1)).r;
    float pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0)).r;
    float pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0)).r;
    float pC = texelFetch(Pressure, T, 0).r;

    // Use center pressure for solid cells:
    vec2 obstV = vec2(0);
    vec2 vMask = vec2(1);

    // Enforce the free-slip boundary condition:
    vec2 oldV = texelFetch(Velocity, T, 0).xy;
    vec2 grad = vec2(pE - pW, pN - pS) * GradientScale;
    vec2 newV = oldV - grad;
    FragColor = (vMask * newV) + obstV;  
}
"))

    

    (define Velocity (gl:get-uniform-location (program-id prg) "Velocity"))
    (define Pressure (gl:get-uniform-location (program-id prg) "Pressure"))
    (define GradientScale (gl:get-uniform-location (program-id prg) "GradientScale"))

    (lambda (out velocity pressure)
      (with-output-to-canvas
       out
       (with-program
        prg

        (define CellSize 1.25)

        (gl:uniform1i Pressure 1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

        (gl:uniform1i Velocity 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

        (gl:uniform1f GradientScale (/ 1.125 CellSize))

        (render-square))))))


(begin
  (define gridsize 512)
  (define vel        (create-canvas gridsize gridsize 2))
  (define vel2       (create-canvas gridsize gridsize 2))
  (define den        (create-canvas gridsize gridsize 1))
  (define den2       (create-canvas gridsize gridsize 1))
  (define divergence (create-canvas gridsize gridsize 3))
  (define prs        (create-canvas gridsize gridsize 1))
  (define prs2       (create-canvas gridsize gridsize 1)))


;; (p/fill vel 0 0 0 0)
;; (p/fill den 0 0 0 0)

;; (canvas-pixels den)
;; (canvas-pixels den2)
;; (canvas-pixels vel)
;; (canvas-pixels divergence)

(define (rain canvas r g b a #!optional (drops 100))
  (repeat drops
          (p/pset canvas
                  (random (canvas-w canvas))
                  (random (canvas-h canvas))
                  r g b a)))

;; (rain den 1 0 0 0)


(define visualize

  (let ()
    (define prg
      (create-program
       #f
       "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D DensityTexture;
uniform sampler2D VelocityTexture;
uniform vec2 InverseSize;

void main() {
 vec2 fragCoord = gl_FragCoord.xy;
 float density = texture(DensityTexture, InverseSize * fragCoord).x;
 vec2  vel     = texture(VelocityTexture, InverseSize * fragCoord).xy;
 FragColor = vec4(density, .5+.5*vel.x, .5+.5*vel.y, 0);
}
"))
    (define DensityTexture  (gl:get-uniform-location (program-id prg) "DensityTexture"))
    (define VelocityTexture (gl:get-uniform-location (program-id prg) "VelocityTexture"))
    (define InverseSize     (gl:get-uniform-location (program-id prg) "InverseSize"))

    (lambda (w h  den vel)
      (with-program
       prg
       
       (gl:uniform1i DensityTexture   0)
       (gl:active-texture gl:+texture0+)
       (gl:bind-texture   gl:+texture-2d+ (canvas-tex den))

       (gl:uniform1i VelocityTexture   1)
       (gl:active-texture gl:+texture1+)
       (gl:bind-texture   gl:+texture-2d+ (canvas-tex vel))
       (gl:active-texture gl:+texture0+)

       (gl:uniform2f InverseSize (/ 1 w) (/ 1 h))
       (render-square)))))


(define handle
  (let ((up #t))
    (lambda (event)
      (case (sdl2:event-type event)
        ((quit) (sdl2:quit!) (exit 0))
        ((window)
         (receive (w h) (sdl2:window-size window)
           (gl:viewport 0 0 w h)))
        ((key-down)
         (print "handling key" event)
         (when (eq? 'menu (sdl2:keyboard-event-sym event))
           (p/fill den 0 0 0 0)
           (p/fill vel 0 0 0 0)))
        ((mouse-motion)
         ;;(print "handling mouse-motion event: " event)
         (define mx (sdl2:mouse-motion-event-x event))
         (define my (sdl2:mouse-motion-event-y event))

         (unless up
           (receive (w h) (sdl2:window-size window)
             (let ((x (* (canvas-w den) (/ mx w)))
                   (y (* (canvas-h den) (- 1 (/ my h))))
                   (xx (* 0.2 (+ (sdl2:mouse-motion-event-xrel event)))) ;; r
                   (yy (* 0.2 (- (sdl2:mouse-motion-event-yrel event)))))
               (p/splat vel   x y 15
                        xx yy 0 ;; r g b
                        ))))
         (set! up (null? (sdl2:mouse-motion-event-state event))))
        ((mouse-button-up) (set! up #t))
        (else (print "unhandled " event))))))


(define core-iteration
  (let ((event (sdl2:make-event)))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))

      (gl:clear-color 0 0 0 0)
      (gl:clear gl:+color-buffer-bit+)

      (p/splat  den
                (* 0.5 (canvas-w den))
                (* 0.5 (canvas-h den))
                20
                1 0 0)
      (p/advect vel2 vel vel 1 0.9999) (canvas-swap! vel vel2)
      (p/advect den2 vel den 1 0.99) (canvas-swap! den den2)
      (p/divergence divergence vel)

      (p/fill prs 0 0 0 0)
      (repeat 20
              (p/jacobi prs2 prs divergence)
              (canvas-swap! prs prs2))

      (p/subtract-gradient vel2 vel prs) (canvas-swap! vel vel2) 


      (receive (w h) (sdl2:window-size window)
        (visualize w h  den vel))
      (sdl2:gl-swap-window! window))))

(define (gameloop proc)
  (define cm current-milliseconds)
  (let loop ((n 0) (t (cm)))
    (proc)
    (cond ((> (- (cm) t) 2000)
           (set! fps (inexact->exact (round (/ n (/ (- (cm) t) 1000)))))
           (print "fps " fps)
           (set! t (cm))
           (set! n 0)))
    (loop (add1 n) t)))

;; don't reach nrepl, so we have only 1 thread (yields fewer major GCs)
;;(gameloop core-iteration)

;; repl-friendly game-loop with fps counter
(define fps 0) ;; <-- access from your repl
(begin (handle-exceptions e void (thread-terminate! game-thread))
       (define game-thread ;; (thread-state game-thread)
         (thread-start!
          (lambda ()
            (gameloop (lambda ()
                        (core-iteration)
                        (thread-yield!)))))))


