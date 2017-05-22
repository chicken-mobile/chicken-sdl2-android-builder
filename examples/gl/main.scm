
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
(include "shaders.scm")


(begin
  (define gridsize 512)
  (define vel        (create-canvas gridsize gridsize 2))
  (define vel2       (create-canvas gridsize gridsize 2))
  (define den        (create-canvas gridsize gridsize 1))
  (define den2       (create-canvas gridsize gridsize 1))
  (define divergence (create-canvas gridsize gridsize 1))
  (define prs        (create-canvas gridsize gridsize 1))
  (define prs2       (create-canvas gridsize gridsize 1))
  (define obstacles  (create-canvas gridsize gridsize 3))

  (p/splat obstacles 0.75 0.38 0.10   1 0 0)
  (p/splat obstacles 0.75 0.62 0.10   1 0 0))


;; (p/fill vel 0 0 0 0)
;; (p/fill den 0 0 0 0)

;; (canvas-pixels den)
;; (canvas-pixels den2)
;; (canvas-pixels vel)
;; (canvas-pixels divergence)

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
uniform sampler2D PressureTexture;
uniform sampler2D ObstaclesTexture;
uniform vec2 InverseSize;

void main() {
 vec2 fragCoord = gl_FragCoord.xy;
 vec2  vel     =   (vec4(0.75,0.0,0.0,1.0)*texture(VelocityTexture, InverseSize * fragCoord)).xy;
 float density =   (vec4(0.25,0.0,0.0,1.0)*texture(DensityTexture, InverseSize * fragCoord)).x;
 float pressure =  (vec4(1.00,0.0,0.0,1.0)*texture(PressureTexture, InverseSize * fragCoord)).x;
 float obstacles = (vec4(1.00,0.0,0.0,1.0)*texture(ObstaclesTexture, InverseSize * fragCoord)).x;
 FragColor = vec4(length(vel), obstacles > 0.0 ? 0.0 : 0.5 + 10.0 * pressure, density, 0);
}
"))
    (let-program-locations
     prg (VelocityTexture DensityTexture PressureTexture ObstaclesTexture InverseSize)

     (lambda (w h  vel prs den obs)
       (with-program
        prg

        (gl:uniform1i ObstaclesTexture 3)
        (gl:active-texture gl:+texture3+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex obs))

        (gl:uniform1i PressureTexture  2)
        (gl:active-texture gl:+texture2+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex prs))

        (gl:uniform1i VelocityTexture  1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex vel))

        (gl:uniform1i DensityTexture   0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex den))

        (gl:uniform2f InverseSize (/ 1 w) (/ 1 h))
        (render-square))))))


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
             (let ((x (/ mx w))
                   (y (- 1 (/ my h)))
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

      (p/splat  den 0.5 0.5 20 1 0 0)

      (p/advect vel2 vel vel 1 0.9999) (canvas-swap! vel vel2)
      (p/advect den2 vel den 1 0.99)   (canvas-swap! den den2)
      (p/divergence divergence vel obstacles)

      (p/fill prs 0 0 0 0)
      (repeat 19
              (p/jacobi prs2 prs divergence obstacles)
              (canvas-swap! prs prs2))

      (p/subtract-gradient vel2 vel prs obstacles) (canvas-swap! vel vel2)


      (receive (w h) (sdl2:window-size window)
        (visualize w h  vel prs den obstacles))
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


