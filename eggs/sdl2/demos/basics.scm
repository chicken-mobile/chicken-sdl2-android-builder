;; The contents of this demo file are made available under the CC0 1.0
;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;; http://creativecommons.org/publicdomain/zero/1.0/


;;; This is a demo program showing some basic SDL functionality.
;;;
;;; This program demonstrates the following concepts:
;;;
;;; - Initializing SDL
;;; - Creating and configuring a window
;;; - Using a record type to manage game scene objects
;;; - Creating surfaces, rects, and colors
;;; - Filling a surface with a color
;;; - Blitting one surface onto another
;;; - Updating the window surface
;;; - A basic event loop that responds to user input.
;;;
;;; Controls:
;;;
;;; - Mouse click / drag: Move Smiley 1.
;;; - Arrow keys: Move Smiley 2.
;;; - Space: Randomize Smiley colors.
;;; - V: Toggle verbose printing of events to console.
;;; - Escape, Q, or close button: Quit


(use (prefix sdl2 sdl2:)
     miscmacros)


;;; Initialize the parts of SDL that we need.
(sdl2:set-main-ready!)
(sdl2:init! '(video events joystick))

;; Automatically call sdl2:quit! when program exits normally.
(on-exit sdl2:quit!)

;; Call sdl2:quit! and then call the original exception handler if an
;; unhandled exception reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))


(printf "Compiled with SDL version ~A~N" (sdl2:compiled-version))
(printf "Running with SDL version ~A~N" (sdl2:current-version))
(printf "Using sdl2 egg version ~A~N" (sdl2:egg-version))



;;; Create a new window.
(define window
  (sdl2:create-window!
   "SDL Basics"                         ; title
   'centered  100                       ; x, y
   800  600                             ; w, h
   '(shown resizable)))                 ; flags

;;; Restrict the window from being made too small or too big, for no
;;; reason except to demonstrate this feature.
(set! (sdl2:window-maximum-size window) '(1024 768))
(set! (sdl2:window-minimum-size window) '(200 200))

(printf "Window position: ~A, size: ~A, max size: ~A, min size: ~A~N"
        (receive (sdl2:window-position window))
        (receive (sdl2:window-size window))
        (receive (sdl2:window-maximum-size window))
        (receive (sdl2:window-minimum-size window)))



;;; A record type for an object that has a surface and x/y coordinates
;;; representing the object's center.
(define-record-type obj
  (make-obj surface x y)
  obj?
  (surface obj-surface (setter obj-surface))
  (x       obj-x       (setter obj-x))
  (y       obj-y       (setter obj-y)))

;; Create rect the same size as obj's surface, centered on its x/y.
(define (obj-rect obj)
  (let ((w (sdl2:surface-w (obj-surface obj)))
        (h (sdl2:surface-h (obj-surface obj))))
    (sdl2:make-rect (- (obj-x obj) (/ w 2))
                   (- (obj-y obj) (/ h 2))
                   w
                   h)))

;;; Blit the obj's surface to the destination surface. The obj will be
;;; drawn centered on its x and y coordinates.
(define (draw-obj! obj dest)
  (sdl2:blit-surface! (obj-surface obj) #f dest (obj-rect obj)))



(define (make-random-color)
  ;; 50 is the minimum so that the color doesn't get too dark.
  (sdl2:make-color (+ 50 (random 175))
                  (+ 50 (random 175))
                  (+ 50 (random 175))
                  255))

;;; Make a new surface with a smiley face of the given color.
(define (make-smile-surf main-color)
  (let ((dest   (sdl2:make-surface 100 100 32))
        (shadow (sdl2:make-color 0 0 0 120)))
    ;; Draw the partially transparent black shadow
    (sdl2:fill-rect! dest (sdl2:make-rect 10 10 90 90) shadow)
    ;; Draw the head (using the main color)
    (sdl2:fill-rect! dest (sdl2:make-rect  0  0 90 90) main-color)
    ;; "Cut out" the eyes and mouth. Filling with a transparent color
    ;; replaces the alpha, it does not blend with the old color.
    (sdl2:fill-rects! dest
                     (list (sdl2:make-rect 25 20 10 20)
                           (sdl2:make-rect 55 20 10 20)
                           (sdl2:make-rect 15 50 10 10)
                           (sdl2:make-rect 25 60 40 10)
                           (sdl2:make-rect 65 50 10 10))
                     shadow)
    ;; Enable RLE for faster blitting.
    (sdl2:surface-rle-set! dest #t)
    dest))

;; Replace the object's surface with a new random color smiley face.
(define (randomize-smiley! obj)
  ;; Free the old surface. This is not strictly necessary, because
  ;; surfaces created with sdl2:make-surface are automatically garbage
  ;; collected. But freeing it immediately helps minimize garbage.
  (sdl2:free-surface! (obj-surface obj))

  ;; Now create and set the new surface.
  (set! (obj-surface obj) (make-smile-surf (make-random-color))))



;;; Create a couple smileys!
(define smiley1 (make-obj (make-smile-surf (make-random-color)) 300 300))
(define smiley2 (make-obj (make-smile-surf (make-random-color)) 500 300))



;;; Draw (or redraw) the entire scene. It would be more efficient to
;;; only redraw the parts of the scene that have changed, but since
;;; this is just a demo program we don't want to get too complex.
(define (draw-scene!)
  (let ((window-surf (sdl2:window-surface window)))
    ;; Clear the whole screen using a blue background color
    (sdl2:fill-rect! window-surf #f (sdl2:make-color 0 80 160))
    ;; Draw the smileys
    (draw-obj! smiley2 window-surf)
    (draw-obj! smiley1 window-surf)
    ;; Refresh the screen
    (sdl2:update-window-surface! window)))



;;; Simple event loop. It just repeats over and over (until the
;;; variable done is set to #t), getting a single event from SDL and
;;; then performing some actions depending on what kind of event it
;;; is. In a real program, your event loop would probably be more
;;; complex and better structured than this simple example.
(let ((done #f)
      (verbose? #f))
  (while (not done)
    (let ((ev (sdl2:wait-event!)))

      (when verbose?
        (print ev))

      (case (sdl2:event-type ev)
        ;; Window exposed, resized, etc.
        ((window)
         (draw-scene!))

        ;; User requested app quit (e.g. clicked the close button).
        ((quit)
         (set! done #t))

        ;; Joystick added (plugged in)
        ((joy-device-added)
         ;; Open the joystick so we start receiving events for it.
         (sdl2:joystick-open! (sdl2:joy-device-event-which ev)))

        ;; Mouse button pressed
        ((mouse-button-down)
         ;; Move smiley1 to the mouse position.
         (set! (obj-x smiley1) (sdl2:mouse-button-event-x ev))
         (set! (obj-y smiley1) (sdl2:mouse-button-event-y ev))
         (draw-scene!))

        ;; Mouse cursor moved
        ((mouse-motion)
         ;; If any button is being held, move smiley1 to the cursor.
         ;; This way it seems like you are dragging it around.
         (when (not (null? (sdl2:mouse-motion-event-state ev)))
           (set! (obj-x smiley1) (sdl2:mouse-motion-event-x ev))
           (set! (obj-y smiley1) (sdl2:mouse-motion-event-y ev))
           (draw-scene!)))

        ;; Keyboard key pressed.
        ((key-down)
         (case (sdl2:keyboard-event-sym ev)
           ;; Escape or Q quits the program
           ((escape q)
            (set! done #t))

           ;; V toggles verbose printing of events
           ((v)
            (if verbose?
                (begin
                  (print "Verbose OFF (events will not be printed)")
                  (set! verbose? #f))
                (begin
                  (print "Verbose ON (events will be printed)")
                  (set! verbose? #t))))

           ;; Space bar randomizes smiley colors
           ((space)
            (randomize-smiley! smiley1)
            (randomize-smiley! smiley2)
            (draw-scene!))

           ;; Arrow keys control smiley2
           ((left)
            (dec! (obj-x smiley2) 20)
            (draw-scene!))
           ((right)
            (inc! (obj-x smiley2) 20)
            (draw-scene!))
           ((up)
            (dec! (obj-y smiley2) 20)
            (draw-scene!))
           ((down)
            (inc! (obj-y smiley2) 20)
            (draw-scene!))))))))
