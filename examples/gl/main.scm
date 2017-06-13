
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

(define (f32vector-sum d)
  (let loop ((i 0) (s 0) (max most-negative-fixnum) (min most-positive-fixnum))
    (if (< i (f32vector-length d))
        (let ((w (f32vector-ref d i)))
          (loop (+ 1 i)
                (+ w s)
                (if (> w max) w max)
                (if (and (not (= w 0)) (< w min)) w min)))
        (values s max min))))

(begin
  (define gridsize 512)
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
  ;;(p/splat den 0.53 0.5 0.03   1 0 0)
  )

;; (canvas-pixels den)
;; (canvas-pixels vel)
;; (canvas-pixels divergence)

;; (rain den 1 0 0 0)


(include "visualize.scm")

(define mouse #f)
(define pause #f)
(define mradius 0.01)
(define handle
  (let ((up #t))
    (lambda (event)
      (case (sdl2:event-type event)
        ((quit) (sdl2:quit!) (exit 0))
        ((window)
         (receive (w h) (sdl2:window-size window)
           (gl:viewport 0 0 w h)))
        ((key-up text-input))
        ((mouse-wheel)
         (define wheel (sdl2:mouse-wheel-event-y event))
         (define x (vector-ref mouse 0))
         (define y (vector-ref mouse 1))
         (define r (if (sdl2:scancode-pressed? 'lctrl)
                       0.1
                       0.01))
         (define amount (if (sdl2:scancode-pressed? 'lshift)
                            0.1
                            0.01))
         (p/splat+ obstacles x y r (* amount wheel) 0 0))
        ((key-down)
         ;;(print "handling key" event)
         (case (sdl2:keyboard-event-sym event)
           ((menu)
            (p/fill den 0 0 0 0)
            (p/fill vel 0 0 0 0))
           ((c) (reset!))
           ((l) (canvas-pixels-from-ppm obstacles "./assets/island.ppm"))
           ((d) (print "density: " (floor (f32vector-sum (canvas-pixels den)))))
           ((return) (set! pause (not pause)) (print "paused: " pause))
           ((tab) (core-iteration))
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
               (let ((xx (* 0.01 (+ (sdl2:mouse-motion-event-xrel event)))) ;; r
                     (yy (* 0.01 (- (sdl2:mouse-motion-event-yrel event)))))
                 (case (car (sdl2:mouse-motion-event-state event))
                   ((right) (p/splat+ den x y 0.01  0.1 0 0)) ;; add "water"
                   ((middle)
                    (define r (if (sdl2:scancode-pressed? 'lctrl) 0.1 0.01))
                    (if (sdl2:scancode-pressed? 'lshift)
                        (p/splat obstacles x y r    0   0 0)
                        (p/splat obstacles x y r    0.5 0 0)))
                   ((left) (p/splat+ vel x y 0.01   (* 1 xx) (* 1 yy) 0)))))))
         (set! up (null? (sdl2:mouse-motion-event-state event))))
        ((mouse-button-up) (set! up #t))
        (else (print "unhandled " event))))))


;;(define pixvel (canvas-pixels vel))
;;(with-output-to-file "vel.f32" (lambda () (write pixvel)))

(define frames 0)

(define render
  (let ((event (sdl2:make-event)))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))
      (receive (w h) (sdl2:window-size window)
        (visualize w h  vel prs den obstacles))
      (sdl2:gl-swap-window! window))))


(define (core-iteration)
  (define dt 0.25)

  ;;(p/diffuse-conserving den2 den obstacles) (canvas-swap! den den2)
  (p/splat+ den 0.5 0.5 1.000  .00001 0 0)
  (p/advect-conserving den2 vel den obstacles dt) (canvas-swap! den den2)
  ;;(p/diffuse-conserving den2 den obstacles) (canvas-swap! den den2)
  (p/advect-conserving vel2 vel vel obstacles dt) (canvas-swap! vel vel2)
  (p/diffuse-conserving den2 den obstacles) (canvas-swap! den den2)
  (p/diffuse-conserving den2 den obstacles) (canvas-swap! den den2)
  (p/subtract-gradient vel2 vel den obstacles 0.01) (canvas-swap! vel vel2)
  ;;(p/diffuse-conserving vel2 vel obstacles) (canvas-swap! vel vel2)
  )

(reset!)

(define MUTEX (make-mutex))
(define (gameloop proc)
  (define cm current-milliseconds)
  (let loop ((n 0) (t (cm)))
    (dynamic-wind
      (lambda () (mutex-lock! MUTEX))
      proc
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




