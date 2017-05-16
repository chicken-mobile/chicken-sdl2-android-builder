;; The contents of this demo file are made available under the CC0 1.0
;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;; http://creativecommons.org/publicdomain/zero/1.0/


(define +title+ "The Sea and the Stars")

;;; Change to #t for fullscreen mode.
(define *fullscreen?* #f)


;;; This is a demo program showing various 2D accelerated renderer
;;; features. It renders a scene of a starry night sky above a rolling
;;; ocean.
;;;
;;; This program demonstrates the following concepts:
;;;
;;; - Creating a window and renderer
;;; - Rendering to the screen
;;; - Rendering to a texture (render target)
;;; - Rendering to a surface (software renderer)
;;; - Converting a surface to a texture
;;; - Procedurally generating circle and gradient textures
;;; - Setting a texture's alpha mod, color mod, and blend mode
;;; - Setting the renderer draw color
;;; - Rendering points, lines, and rectangles
;;; - Procedurally animating a scene over time
;;;
;;; Controls:
;;;
;;; - Escape: Quit the program
;;; - C: Generate new constellations
;;; - Space: Pause or unpause animation
;;; - Left arrow: Decrease animation speed
;;; - Right arrow: Increase animation speed
;;;
;;; Creator: John Croisant


(use (prefix sdl2 sdl2:)
     miscmacros)

;;; Convenience aliases.
(define C sdl2:make-color)
(define P sdl2:make-point)
(define R sdl2:make-rect)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INITIALIZATION

;;; Initialize the parts of SDL that we need.
(sdl2:set-main-ready!)
(sdl2:init! '(video timer))

;;; Automatically call sdl2:quit! when program exits normally.
(on-exit sdl2:quit!)

;;; Call sdl2:quit! and then call the original exception handler if an
;;; unhandled exception reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))


;;; Request to use the best available render quality for scaling and
;;; rotating textures.
(sdl2:set-hint! 'render-scale-quality "best")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCENE SETTINGS

(define +screen-width+  800)
(define +screen-height+ 400)
(define +water-height+  100)
(define +sky-height+    (- +screen-height+ +water-height+))

(define +sky-rect+ (R 0 0 +screen-width+ +sky-height+))


(define +num-dim-stars+       3000)
(define +dim-star-group-size+ 50)
(define +dim-star-min-bright+ 0.0)
(define +dim-star-max-bright+ 0.35)


(define +constellation-min-stars+ 4)
(define +constellation-max-stars+ 7)

;;; The areas of the sky where constellations will appear.
(define +constellation-rects+
  (list (R (+ 50  (random 50)) (+ 10 (random 60)) 150 100)
        (R (+ 300 (random 50)) (+ 10 (random 60)) 100 200)
        (R (+ 500 (random 50)) (+ 10 (random 60)) 150 150)))


;;; Moon's base position (no rotation; 45 degrees above horizon)
(define +moon-base-pos+ (P (- (/ +screen-width+ 2) 150)
                           (- +sky-height+ 150)))
;;; Moon's pivot relative to base position.
(define +moon-pivot+ (P 150 150))
;;; Moon angle change per second.
(define +moon-speed+ 3.5)
;;; Number of moon craters. More is slower, but better looking.
(define +moon-craters+ 350)


(define +wave-block-w+ 17)     ; block width
(define +wave-height+  25)     ; height of peaks
(define +wave-period+  350)    ; space between peaks
(define +wave-speed+   1.5)    ; how fast waves move


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COLORS

(define +zenith-color+    (C  10  10  30))
(define +horizon-color+   (C  20  30  90))
(define +star-color+      (C 230 230 255))
(define +star-line-color+ (C  50  60  90))
(define +moon-color+      (C 220 220 220))
(define +wave-hl-color+   (C  80  90 120))

(define +white+           (C 255 255 255))
(define +black+           (C   0   0   0))

;;; Return a copy of the given color, but with different alpha.
(define (transparent color #!optional (alpha 0))
  (sdl2:color-set! (sdl2:copy-color color) #f #f #f alpha))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WINDOW / RENDERER

(define-values (*window* *renderer*)
  (sdl2:create-window-and-renderer!
   +screen-width+ +screen-height+
   (if *fullscreen?* '(fullscreen) '())))

(set! (sdl2:window-title *window*) +title+)

(set! (sdl2:render-draw-color *renderer*) +black+)
(sdl2:render-clear! *renderer*)

(set! (sdl2:render-viewport *renderer*)
      (R 0 0 +screen-width+ +screen-height+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY PROCEDURES

(define +pi+     3.141592653589793)
(define +2pi+    6.283185307179586)
(define +pi/2+   1.570796326794897)
(define +pi/180+ 0.017453292519943295)
(define +180/pi+ 57.29577951308232)

;;; Clamp the given numeric value to be between min and max.
(define (clamp x min max)
  (cond ((< x min) min)
        ((> x max) max)
        (else x)))

(define (smoothstep x edge0 edge1)
  (let ((x (clamp (/ (- x edge0) (- edge1 edge0))
                  0.0 1.0)))
    (* x x (- 3 (* 2 x)))))

;;; Returns the distance-squared between the two points. If you need
;;; the distance, call sqrt on the result.
(define (distance-sq x1 y1 x2 y2)
  (let ((dx (- x1 x2))
        (dy (- y1 y2)))
    (+ (* dx dx) (* dy dy))))


;;; Sine wave that ranges from 0 to 1 instead of -1 to 1.
(define (sin01 theta)
  (+ 0.5 (* 0.5 (sin theta))))


(define (move-point point x-offset y-offset)
  (P (+ x-offset (sdl2:point-x point))
     (+ y-offset (sdl2:point-y point))))

(define (move-rect rect x-offset y-offset)
  (R (+ x-offset (sdl2:rect-x rect))
     (+ y-offset (sdl2:rect-y rect))
     (sdl2:rect-w rect)
     (sdl2:rect-h rect)))

(define (scale-rect rect w-scale h-scale)
  (R (sdl2:rect-x rect)
     (sdl2:rect-y rect)
     (* w-scale (sdl2:rect-w rect))
     (* h-scale (sdl2:rect-h rect))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DRAW CIRCLE

;;; Return a new 32-bit surface containing a circle. fg and bg are the
;;; foreground and background colors. smooth is the distance to smooth
;;; the edges. This is an expensive operation, so don't call it every
;;; frame.
(define (draw-circle radius #!key (smooth 2)
                     (fg +white+) (bg (transparent fg)))
  (let ((size (ceiling (+ 2 (* radius 2))))
        (center (+ 1 radius))
        (edge0-sq (* (- radius smooth) (- radius smooth)))
        (edge1-sq (* radius radius)))
    (let ((surf (sdl2:make-surface size size 32)))
      (sdl2:lock-surface! surf)

      ;; Iterate through every pixel and calculate its color, based on
      ;; its distance from the center. (For speed of computation, this
      ;; uses distance squared instead of distance.)
      (do ((x 0 (add1 x)))
          ((>= x size))
        (do ((y 0 (add1 y)))
            ((>= y size))
          (let ((i (smoothstep (distance-sq x y center center)
                               edge0-sq edge1-sq)))
            (set! (sdl2:surface-ref surf x y)
                  (sdl2:color-lerp fg bg i)))))

      (sdl2:unlock-surface! surf)
      surf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDER GRADIENT

;;; Render a vertical gradient going from color1 (at the top) to
;;; color2 (at the bottom).
(define (render-vgradient! renderer rect color1 color2 bands)
  (let ((old-clip-rect (sdl2:render-clip-rect renderer))
        (step (/ 1.0 bands)))
    (receive (x y w h) (sdl2:rect->values rect)
      ;; Set the clip rect so we don't overflow the requested rect.
      (set! (sdl2:render-clip-rect renderer) rect)

      (let* ((band-height (+ 1 (ceiling (/ h bands))))
             (band (R 0 0 w band-height)))
        (do ((i 0.0 (+ i step)))
            ((> i 1.0))
          (sdl2:rect-set! band x (+ y (floor (* i h))))
          (set! (sdl2:render-draw-color renderer)
                (sdl2:color-lerp color1 color2 i))
          (sdl2:render-fill-rect! renderer band)))

      ;; Restore the original clip rect
      (set! (sdl2:render-clip-rect renderer) old-clip-rect))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SKY

;;; Generate a 1px-wide vertical gradient texture. The texture is
;;; created in a very roundabout way in order to demonstrate as much
;;; rendering functionality as possible.
(define (make-sky-texture renderer height)
  ;; Create a temporary surface, and a software renderer that draws
  ;; onto that surface.
  (let* ((surf (sdl2:make-surface 1 height 32))
         (surf-renderer (sdl2:create-software-renderer! surf)))
    ;; Render the vertical gradient to the surface.
    (render-vgradient! surf-renderer (R 0 0 1 height)
                       +zenith-color+ +horizon-color+ height)
    ;; Convert the surface to a texture (using the main renderer).
    (sdl2:create-texture-from-surface
     renderer surf)))

(define *sky-texture* (make-sky-texture *renderer* 40))
(set! (sdl2:texture-blend-mode *sky-texture*) 'blend)


;; Render the sky texture, stretched to fit the screen width.
(define (render-sky! renderer)
  (set! (sdl2:texture-alpha-mod *sky-texture*) 255)
  (sdl2:render-copy!
   renderer *sky-texture*
   #f (R 0 120 +screen-width+ (- +sky-height+ 120))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STARS

(define (generate-stars n rect)
  (receive (x y w h) (sdl2:rect->values rect)
    (list-tabulate n
      (lambda (_)
        (P (+ x (random w)) (+ y (random h)))))))

(define *dim-stars*
  (list-tabulate (inexact->exact
                  (round (/ +num-dim-stars+ +dim-star-group-size+)))
    (lambda (_)
      (generate-stars +dim-star-group-size+ +sky-rect+))))


(define (render-stars! renderer time star-groups min-bright max-bright)
  (for-each (lambda (star-group i)
              (render-star-group!
               renderer star-group
               (star-bright time i min-bright max-bright)))
            star-groups
            (iota (length star-groups))))

(define (render-star-group! renderer points brightness)
  (set! (sdl2:render-draw-color renderer)
        (sdl2:color-lerp +horizon-color+ +star-color+ brightness))
  ;; Render the stars (en masse)
  (sdl2:render-draw-points! renderer points))

;; Complicated math to make the stars have different brightnesses and
;; twinkle over time.
(define (star-bright time i min-bright max-bright)
  (let ((base (+ min-bright (* (- max-bright min-bright)
                               (/ (modulo i 13) 13))))
        (vary (* (sin (* 0.3 (modulo i 29)
                         (+ (* 29 i) time)))
                 (sin (* 0.5 (modulo i 13)
                         (+ (* 13 i) time))))))
    (* base (min 1.0 (+ 0.75 (* 0.5 vary))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTELLATIONS

(define (generate-constellations)
  (map (lambda (rect)
         (generate-stars
          (+ +constellation-min-stars+
             (random (- +constellation-max-stars+
                        +constellation-min-stars+)))
          rect))
       +constellation-rects+))

(define *constellations*
  (make-parameter (generate-constellations)))


(define (render-constellation! renderer time constellation)
  ;; Draw the lines between stars
  (set! (sdl2:render-draw-color renderer) +star-line-color+)
  (sdl2:render-draw-lines! renderer constellation)

  ;; Draw the stars (individually)
  (set! (sdl2:render-draw-color renderer) +star-color+)
  (for-each (lambda (point)
              (sdl2:render-draw-point!
               renderer (sdl2:point-x point) (sdl2:point-y point)))
            constellation))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOON

;;; A white circle on a transparent background.
(define +moon-body+
  (sdl2:create-texture-from-surface
   *renderer* (draw-circle 29 smooth: 3 fg: +white+)))
(set! (sdl2:texture-blend-mode +moon-body+) 'blend)

;;; A gray circle on a white background, with mod (multiply) blend
;;; mode. Rendering this texture will make a dark circle.
(define +moon-crater+
  (sdl2:create-texture-from-surface
   *renderer* (draw-circle 5 smooth: 3
                           fg: (C 245 245 240)
                           bg: +white+)))
(set! (sdl2:texture-blend-mode +moon-crater+) 'mod)

;;; A white circle on a black background, with add blend mode.
;;; Rendering this will make a bright circle.
(define +moon-glow+
  (sdl2:create-texture-from-surface
   *renderer* (draw-circle 20 smooth: 20
                           fg: +white+ bg: +black+)))
(set! (sdl2:texture-blend-mode +moon-glow+) 'add)


(define (make-moon-texture renderer)
  (let* ((moon-size (sdl2:texture-w +moon-body+))
         (texture (sdl2:create-texture
                   renderer 'rgba8888 'target moon-size moon-size)))
    (set! (sdl2:texture-blend-mode texture) 'blend)

    ;; Render to the texture.
    (set! (sdl2:render-target renderer) texture)

    ;; Draw the base shape (a big bright circle).
    (sdl2:render-copy!
     renderer +moon-body+
     #f (R 0 0 moon-size moon-size))

    ;; Draw some random craters (darker, smaller circles).
    (dotimes (i +moon-craters+)
      (let ((crater-size (round (* (sdl2:texture-w +moon-crater+)
                                   (+ 0.2 (/ (random 250) 100))))))
        (sdl2:render-copy!
         renderer +moon-crater+
         #f (R (random moon-size) (random moon-size)
               crater-size crater-size))))

    ;; Draw some highlight in the top left side.
    (set! (sdl2:texture-alpha-mod +moon-glow+) 30)
    (sdl2:render-copy!
     renderer +moon-glow+
     #f (R 0 0 (round (* 0.75 moon-size)) (round (* 0.75 moon-size))))

    ;; Reset render target.
    (set! (sdl2:render-target renderer) #f)

    ;; Return the completed moon texture.
    texture))


(define +moon-texture+ (make-moon-texture *renderer*))


;;; Rotation 0 means the base position, which is 45 degrees above the
;;; horizon. The rotation is adjusted -90 degrees so the moon starts
;;; below the horizon, for dramatic effect.
(define (moon-rotation time)
  (+ -90 (* +moon-speed+ time)))

(define (render-moon! renderer time)
  (let* ((x (sdl2:point-x +moon-base-pos+))
         (y (sdl2:point-y +moon-base-pos+))
         (size   (sdl2:texture-w +moon-texture+))
         (size/2 (round (/ (sdl2:texture-w +moon-texture+) 2))))
    ;; Render moon body
    (sdl2:render-copy-ex!
     renderer +moon-texture+
     #f (R (- x size/2) (- y size/2) size size)
     (moon-rotation time) +moon-pivot+
     ;; Flip horizontally and vertically, merely to demonstrate how.
     '(horizontal vertical))

    ;; Render moon glow
    (set! (sdl2:texture-alpha-mod +moon-glow+) 40)
    (sdl2:render-copy-ex!
     renderer +moon-glow+
     #f (R (- x size) (- y size) (* size 2) (* size 2))
     (moon-rotation time)
     (move-point +moon-pivot+ size/2 size/2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WAVES

(define +num-waves+ (inexact->exact (ceiling (/ +screen-width+ +wave-block-w+))))

(define *waves*
  (list-tabulate +num-waves+
    (lambda (i)
      (R (* i +wave-block-w+) 0 +wave-block-w+ 10))))

(define (update-wave! rect i num time y-offset)
  ;; This math is probably "wrong" but it looks okay.
  (let* ((theta (+ (* +2pi+ (/ (sdl2:rect-x rect) +wave-period+))
                   (* +wave-speed+ time)))
         (h  (+ y-offset +water-height+ (* +wave-height+ (sin01 theta))))
         (h* (round h)))
    (sdl2:rect-set! rect #f (- +screen-height+ h*) #f h*)))


(define (render-wave-gradient! renderer time rect)
  ;; Draw the sky gradient, partially transparent and flipped.
  (set! (sdl2:texture-alpha-mod *sky-texture*) 160)
  (sdl2:render-copy-ex! renderer *sky-texture*
                        #f rect 0.0 #f '(vertical)))


(define (render-wave-highlight! renderer time rect)
  (receive (x y w h) (sdl2:rect->values rect)
    ;; Adjust the highlight color based on how close the rect is to
    ;; its peak height.
    (set! (sdl2:render-draw-color renderer)
      (sdl2:color-lerp +zenith-color+ +wave-hl-color+
                       (max 0.4 (/ (- +sky-height+ y)
                                   +wave-height+))))

    (let ((hirect
           (R (+ x 4) (+ y 4) (- w 8)
              ;; Adjust the height using a sine wave, so it seems to undulate.
              (floor (+ (* 10 (sin (* 0.03 (+ x y time))))
                        (* h 0.25))))))
      ;; Draw the highlight rect outline.
      (sdl2:render-draw-rect! renderer hirect)
      ;; Draw a vertical line down the middle.
      (let* ((w/2 (floor (/ (sdl2:rect-w hirect) 2)))
             (line-x (+ (sdl2:rect-x hirect) w/2)))
        (sdl2:render-draw-line!
         renderer
         line-x (+ (sdl2:rect-y hirect) w/2)
         line-x (+ (sdl2:rect-y hirect) (sdl2:rect-h hirect) (- w/2)))))))


(define (render-waves! renderer time waves y-offset)
  ;; Update all the waves.
  (let ((num (length waves)))
    (for-each (cut update-wave! <> <> num time y-offset)
              waves
              (iota num)))

  ;; Draw the wave rects, en masse.
  (set! (sdl2:render-draw-color renderer) +zenith-color+)
  (sdl2:render-fill-rects! renderer waves)

  ;; Individually draw a gradient on each wave.
  (for-each (cut render-wave-gradient! renderer time <>)
            waves)

  ;; Draw a border around each wave rect, en masse.
  (set! (sdl2:render-draw-color renderer) +zenith-color+)
  (sdl2:render-draw-rects! renderer waves)

  ;; Individually draw a highlight near the top of each wave.
  (for-each (cut render-wave-highlight! renderer time <>)
            waves))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDER SCENE

(define (render-scene! renderer time)
  (set! (sdl2:render-draw-color renderer) +zenith-color+)
  (sdl2:render-fill-rect! renderer #f)

  (render-sky! renderer)
  (render-stars! renderer time *dim-stars*
                 +dim-star-min-bright+
                 +dim-star-max-bright+)

  (for-each (cut render-constellation! renderer time <>)
            (*constellations*))

  (render-moon! renderer time)
  (render-waves! renderer time *waves* 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENT HANDLING

(define *paused?*     (make-parameter #f))
(define *old-ticks*   (make-parameter (sdl2:get-ticks)))
(define *time*        (make-parameter 0.0))
(define *time-scale*  (make-parameter 1.0))

;;; Disable various irrelevant event types, to avoid wasted time and
;;; memory garbage from handling them.
(set! (sdl2:event-state 'text-editing)      #f)
(set! (sdl2:event-state 'text-input)        #f)
(set! (sdl2:event-state 'mouse-button-down) #f)
(set! (sdl2:event-state 'mouse-button-up)   #f)
(set! (sdl2:event-state 'mouse-motion)      #f)
(set! (sdl2:event-state 'mouse-wheel)       #f)
(set! (sdl2:event-state 'finger-down)       #f)
(set! (sdl2:event-state 'finger-up)         #f)
(set! (sdl2:event-state 'finger-motion)     #f)
(set! (sdl2:event-state 'multi-gesture)     #f)

(define (handle-event ev exit-main-loop!)
  (case (sdl2:event-type ev)
    ;; User requested app quit (e.g. clicked the close button).
    ((quit)
     (exit-main-loop! #t))

    ;; Keyboard key pressed
    ((key-down)
     (case (sdl2:keyboard-event-sym ev)
       ;; Escape quits the program
       ((escape)
        (exit-main-loop! #t))

       ;; C generates new random constellations.
       ((c)
        (set! (*constellations*) (generate-constellations)))

       ;; Space toggles pause
       ((space)
        (set! (*paused?*) (not (*paused?*))))

       ;; Left arrow decreases time scale.
       ((left)
        (dec! (*time-scale*) 0.5))
       ;; Right arrow increases time scale.
       ((right)
        (inc! (*time-scale*) 0.5))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN LOOP

(define (main-loop)
  ;; Create a continuation that can be called to exit the main loop.
  (let/cc exit-main-loop!
    ;; Loop forever (until exit-main-loop! is called).
    (while #t
      ;; Handle all pending events.
      (sdl2:pump-events!)
      (while (sdl2:has-events?)
        (handle-event (sdl2:poll-event!) exit-main-loop!))

      (let ((new-ticks (sdl2:get-ticks)))
        ;; Unless paused, update time (seconds) based on how many
        ;; ticks (ms) have passed since last frame, then re-render.
        (unless (*paused?*)
          (inc! (*time*)
                (* (*time-scale*)
                   (/ (- new-ticks (*old-ticks*))
                      1000)))
          (render-scene! *renderer* (*time*)))
        (set! (*old-ticks*) new-ticks))

      ;; Present (i.e. show) any changes to the screen.
      (sdl2:render-present! *renderer*)

      ;; Pause briefly to let the CPU rest.
      (sdl2:delay! 20))))


(main-loop)
