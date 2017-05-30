
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

(define (f32vector-sum d) (let loop ((i 0) (s 0)) (if (< i (f32vector-length d)) (loop (+ 1 i) (+ (f32vector-ref d i) s)) s)))

(begin
  (define gridsize 256)
  (define vel        (create-canvas gridsize gridsize 2)) (gc #t)
  (define vel2       (create-canvas gridsize gridsize 2)) (gc #t)
  (define den        (create-canvas gridsize gridsize 1)) (gc #t)
  (define den2       (create-canvas gridsize gridsize 1)) (gc #t)
  (define divergence (create-canvas gridsize gridsize 1)) (gc #t)
  (define prs        (create-canvas gridsize gridsize 1)) (gc #t)
  (define prs2       (create-canvas gridsize gridsize 1)) (gc #t)
  (define obstacles  (create-canvas gridsize gridsize 1)) (gc #t)
  (define wprs       (create-canvas gridsize gridsize 1)) (gc #t)
  (define wprs2      (create-canvas gridsize gridsize 1)) (gc #t)


  ;; (p/splat obstacles 0.75 0.38 0.10   1 0 0)
  ;; (p/splat obstacles 0.75 0.62 0.10   1 0 0)
  )

(define (reset!)
  (for-each (lambda (canvas) (p/fill canvas  0 0 0 0))
            (list vel den den2 prs prs2))
  (p/splat den 0.53 0.5 0.02   1000 0 0))

;; (canvas-pixels den)
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

#define PI 3.1415926538
#define PI2 (2.0*3.1415926538)

out vec4 FragColor;

uniform sampler2D DensityTexture;
uniform sampler2D VelocityTexture;
uniform sampler2D PressureTexture;
uniform sampler2D ObstaclesTexture;
uniform vec2 InverseSize;
uniform float Time;

float project(vec2 axis, vec2 x) {
  return dot(x, axis);
}

const float vel = 0.01;

void main() {
 ivec2 tsize = textureSize(VelocityTexture, 0);
 ivec2 tCoord = ivec2(InverseSize * vec2(tsize) * gl_FragCoord.xy);
 vec2 fragCoord = gl_FragCoord.xy;
 vec2  vel     =   (vec4(vel , vel,0.0,1.0)*texelFetch(VelocityTexture,  tCoord, 0)).xy;
 float density =   (vec4(0.02,0.00,0.0,1.0)*texelFetch(DensityTexture,   tCoord, 0)).x;
 float pressure =  (vec4(1.00,0.00,0.0,1.0)*texelFetch(PressureTexture,  tCoord, 0)).x;
 float obstacles = (vec4(0.20,0.00,0.0,1.0)*texelFetch(ObstaclesTexture, tCoord, 0)).x;

 //density = min(density, 1.0);
 //float off = mod(project(normalize(vel), vec2(fragCoord)), PI*2.0);
 //float t = off - mod(length(vel) * Time * 0.1, PI*2.0);
 //if(length(vel) > 100000.25) density *= .5 + 0.5*(sin(t));
 FragColor = vec4(length(vel)+obstacles, max(0.0,pressure - density) * 2.0, density , 0);
}
"))
    (let-program-locations
     prg (VelocityTexture DensityTexture PressureTexture ObstaclesTexture InverseSize Time)

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
        (gl:uniform1f Time (sdl2:get-ticks))
        (render-square))))))

(define mouse #f)
(define pause #f)
(define handle
  (let ((up #t))
    (lambda (event)
      (case (sdl2:event-type event)
        ((quit) (sdl2:quit!) (exit 0))
        ((window)
         (receive (w h) (sdl2:window-size window)
           (gl:viewport 0 0 w h)))
        ((key-up text-input))
        ((key-down)
         ;;(print "handling key" event)
         (case (sdl2:keyboard-event-sym event)
           ((menu)
            (p/fill den 0 0 0 0)
            (p/fill vel 0 0 0 0))
           ((c) (reset!))
           ((d) (print "density: " (floor (f32vector-sum (canvas-pixels den)))))
           ((return) (set! pause (not pause)) (print "paused: " pause))
           ((space)
            (define (pget c x y)
              ;;(define c vel)
              (define xx (inexact->exact (floor (* x (canvas-w c)))))
              (define yy (inexact->exact (floor (* y (canvas-h c)))))
              (define pix (canvas-pixels c))
              (define offset (+ (* (canvas-w c) (canvas-d c) yy) (* (canvas-d c) xx)))
              (case (canvas-d c)
                ((1) (vector (f32vector-ref pix offset)))
                ((2) (vector (f32vector-ref pix offset) (f32vector-ref pix (+ 1 offset))))
                (else #f)))
            (define x (vector-ref mouse 0))
            (define y (vector-ref mouse 1))
            (print "vel " (pget vel x y)
                   " den " (pget den x y)
                   " obs " (pget obstacles x y)
                   " div " (pget divergence x y)))))
        ((mouse-motion)
         ;;(print "handling mouse-motion event: " event)
         (define mx (sdl2:mouse-motion-event-x event))
         (define my (sdl2:mouse-motion-event-y event))

         (receive (w h) (sdl2:window-size window)
           (let  ((x (/ mx w))
                  (y (- 1 (/ my h))))
             (set! mouse (vector x y))
             (unless up
               (let ((xx (* 2 (+ (sdl2:mouse-motion-event-xrel event)))) ;; r
                     (yy (* 2 (- (sdl2:mouse-motion-event-yrel event)))))
                 (case (car (sdl2:mouse-motion-event-state event))
                   ((middle) (p/splat+ den x y 0.01  100 0 0)) ;; add "water"
                   ((right) (if (sdl2:scancode-pressed? 'lshift)
                                (p/splat obstacles x y 0.01   0 0 0)
                                (p/splat obstacles x y 0.01   1 0 0)))
                   ((left) (p/splat+ vel x y 0.01   (* 0.1 xx) (* 0.1 yy) 0)))))))
         (set! up (null? (sdl2:mouse-motion-event-state event))))
        ((mouse-button-up) (set! up #t))
        (else (print "unhandled " event))))))


;;(define pixvel (canvas-pixels vel))
;;(with-output-to-file "vel.f32" (lambda () (write pixvel)))

(define frames 0)
(define core-iteration
  (let ((event (sdl2:make-event))
        (dt 0.25)
        (iterations 32))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))
      (set! frames (add1 frames))

      ;; ==================== density step ====================
      ;;(p/diffuse den2 den obstacles 0.00001 0.1 20) (canvas-swap! den den2)
      (p/advect den2 vel den obstacles (* 2 dt) 1) (canvas-swap! den den2)

      ;; ==================== velocity step ====================
      ;;(p/fill vel 0 0 0 0)
      (p/splat vel 0.25 0.5 0.01   30 0 0)

      (p/project vel2 prs divergence vel obstacles 64) (canvas-swap! vel vel2)
      (p/advect  vel2 vel vel obstacles dt 1)          (canvas-swap! vel vel2)

      (p/project vel2 prs divergence vel obstacles 64) (canvas-swap! vel vel2)

      (receive (w h) (sdl2:window-size window)
        (visualize w h  vel den2 den obstacles))
      (sdl2:gl-swap-window! window))))

(define render
  (let ((event (sdl2:make-event)))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))
      (receive (w h) (sdl2:window-size window)
        (visualize w h  vel prs den obstacles))
      (sdl2:gl-swap-window! window))))


(define (core-iteration)
  (define dt 0.25)
  ;;(define old (f32vector-sum (canvas-pixels den)))
  ;;

  ;;(p/+ prs den 0)
  (p/advect-conserving den2 vel den obstacles dt) (canvas-swap! den den2)

  ;;(p/project vel2 prs divergence vel obstacles 8) (canvas-swap! vel vel2)
  ;;(p/advect den2 vel den obstacles 0.1 1) (canvas-swap! den den2)
  ;;(p/project vel2 prs divergence vel obstacles 8) (canvas-swap! vel vel2)

  (p/advect-conserving vel2 vel vel obstacles dt) (canvas-swap! vel vel2)
  ;;(p/advect vel2 vel vel obstacles 0.1 1) (canvas-swap! vel vel2)
  (p/subtract-gradient vel2 vel den obstacles 0.01) (canvas-swap! vel vel2)

  ;;(p/splat vel 0.5 0.5 0.01   -1 0 0)

  ;;(p/project vel2 prs divergence vel obstacles 20) (canvas-swap! vel vel2)
  ;;(p/* vel2 vel den) (canvas-swap! vel vel2)

  )

(reset!)

(define MUTEX (make-mutex))
(define (gameloop proc)
  (define cm current-milliseconds)
  (let loop ((n 0) (t (cm)))
    (dynamic-wind
      (lambda () (mutex-lock! MUTEX))
      (lambda () (unless pause (proc)))
      (lambda () (mutex-unlock! MUTEX)))
    (cond ((> (- (cm) t) 2000)
           (set! fps (inexact->exact (round (/ n (/ (- (cm) t) 1000)))))
           (file-write 0 (conc "fps " fps "\n"))
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
            (handle-exceptions
             e (raise e)
             (gameloop (lambda ()
                         (unless pause (core-iteration))
                         (render)
                         ;;(thread-sleep! 0.01)
                         (thread-yield!))))))))


(include "repl.scm")




