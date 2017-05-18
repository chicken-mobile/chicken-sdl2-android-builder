;;; This should display a green square on red backgound.
;;; try it with:
;;; csi example.scm # <-- note no -s!
(use sdl2 miscmacros) ;; (import sdl2 miscmacros)

(define window (create-window! "sdl test" 0 0 640 640 '(opengl resizable)))
(define screen (window-surface window))

(define spot (make-rect 100 100 30 30))

(define (render)
  (fill-rect! screen spot (make-color 0 255 0)))

(define (handle event)
  (case (event-type event)
    ((window)
     (print "handling window event: " event)
     (set! screen (window-surface window))) ;; updates size
    ((mouse-motion)
     (print "handling mouse-motion event: " event)
     (set! (rect-x spot) (mouse-motion-event-x event))
     (set! (rect-y spot) (mouse-motion-event-y event)))
    (else (print "unhandled " event))))

(define core-iteration
  (let ((event (make-event)))
   (lambda ()
     (while* (poll-event! event) (handle it))
     (fill-rect! screen #f (make-color 255 0 0 255))
     (render)
     (update-window-surface! window))))

(define fps 0) ;; <-- access from your repl

(begin (handle-exceptions e void (thread-terminate! game-thread))
       (define game-thread ;; (thread-state game-thread)
         (thread-start!
          (lambda ()
            (define cm current-milliseconds)
            (let loop ((n 0) (t (cm)))
              (thread-yield!) ;; (thread-sleep! 0.2)
              (core-iteration)
              (cond ((> (- (cm) t) 2000)
                     (set! fps (inexact->exact (round (/ n (/ (- (cm) t) 1000)))))
                     (set! t (cm))
                     (set! n 0)))
              (loop (add1 n) t)))))
       (thread-quantum-set! game-thread 500000))
