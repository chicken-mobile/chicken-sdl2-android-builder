
(use (prefix sdl2 sdl2:)
     (prefix opengl-glew  gl:)
     (prefix gl-utils     gl-utils:)
     (prefix gl-math      gl-math:)
     miscmacros)

(import (prefix sdl2 sdl2:)
        (prefix opengl-glew gl:))

(sdl2:set-main-ready!)
(sdl2:init! '(video timer))
(on-exit sdl2:quit!)

(define window (sdl2:create-window!
                "SDL2 + OpenGL Example"
                'undefined 'undefined 640 480
                '(opengl)))

;;; Use OpenGL 3.3
(sdl2:gl-attribute-set! 'context-profile-mask 'es)
(sdl2:gl-attribute-set! 'context-major-version 3)
(sdl2:gl-attribute-set! 'context-minor-version 0)

(define gl-context (sdl2:gl-create-context! window))

(assert (equal? window     (sdl2:gl-get-current-window)))
(assert (equal? gl-context (sdl2:gl-get-current-context)))

(gl:init)
(gl-utils:check-error)


(begin ;; eval this (M-C-x in emacs) to hot-swap the program into the running app

  (define vertex-shader-source "
#version 300 es
in vec2 position;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   c = color;
}")

  (define fragment-shader-source "
#version 300 es
in vec3 c;
out vec4 fragColor;
void main(){
  fragColor = vec4(c, 1.0);
}")

  (define program
    (gl-utils:make-program
     (list (gl-utils:make-shader gl:+vertex-shader+
                                 vertex-shader-source)
           (gl-utils:make-shader gl:+fragment-shader+
                                 fragment-shader-source)))))



(define square
  (gl-utils:make-mesh
   vertices: '(attributes:
               ((position #:float 2)
                (color #:unsigned-byte 3 normalized: #t))
               initial-elements:
               ((position . (-1 -1
                              1 -1
                              1  1
                             -1  1))
                (color . (255 0   0
                          0   255 0
                          0   0   255
                          255 0   255))))
   indices: '(type: #:ushort
              initial-elements: (0 1 2
                                 0 2 3))))

(gl-utils:mesh-make-vao!
 square `((position . ,(gl:get-attrib-location program "position"))
          (color    . ,(gl:get-attrib-location program "color"))))



;;; A matrix representing the screen projection.
(define projection-matrix
  (gl-math:perspective 640 480 0.1 100 70))

;;; A matrix representing the camera position and orientation.
(define view-matrix
  (gl-math:look-at (gl-math:make-point 1 0 3)
                   (gl-math:make-point 0 0 0)
                   (gl-math:make-point 0 1 0)))

;;; The model's current rotation around the Z axis.
(define *model-z-angle* 0.0)

;;; Calculate a matrix representing the model's transformation.
(define (calc-model-matrix)
  (gl-math:z-rotation *model-z-angle*))

;;; Calculate a matrix combining the model, view, and projection
;;; matrices.
(define (calc-mvp-matrix)
  (gl-math:m* projection-matrix
              (gl-math:m* view-matrix
                          (calc-model-matrix))))

(define (handle event)
  (case (sdl2:event-type event)
    ((mouse-motion)
     (print "handling mouse-motion event: " event))
    (else (print "unhandled " event))))


(define (render)
  (gl:use-program program)

  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP")
                        1 #f
                        (calc-mvp-matrix))

  (gl:bind-vertex-array (gl-utils:mesh-vao square))
  (gl:draw-elements
   (gl-utils:mode->gl (gl-utils:mesh-mode square))
   (gl-utils:mesh-n-indices square)
   (gl-utils:type->gl (gl-utils:mesh-index-type square))
   #f)

  (gl-utils:check-error)
  (gl:bind-vertex-array 0))

(define core-iteration
  (let ((event (sdl2:make-event)))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))
      (gl:clear (bitwise-ior gl:+color-buffer-bit+
                             gl:+depth-buffer-bit+))

      (set! *model-z-angle* (* (sdl2:get-ticks) 0.001))
      (render)
      (sdl2:gl-swap-window! window))))


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

