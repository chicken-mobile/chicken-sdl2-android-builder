;; The contents of this demo file are made available under the CC0 1.0
;; Universal Public Domain Dedication. See LICENSE-CC0.txt or visit
;; http://creativecommons.org/publicdomain/zero/1.0/


;;; This is a demo program showing how to integrate with OpenGL to
;;; create 3D graphics, using the opengl-glew, gl-utils, and gl-math
;;; eggs.
;;;
;;; Controls:
;;;
;;; - Click the close button to quit


(use (prefix sdl2 sdl2:)
     (prefix opengl-glew  gl:)
     (prefix gl-utils     gl-utils:)
     (prefix gl-math      gl-math:)
     miscmacros)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INITIALIZE SDL AND OPENGL

;; Initialize SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video timer))

;; Automatically call sdl2:quit! when program exits normally.
(on-exit sdl2:quit!)

;; Call sdl2:quit! and then call the original exception handler if an
;; unhandled exception reaches the top level.
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))


;;; Create an SDL window with the opengl flag.
(define window (sdl2:create-window!
                "SDL2 + OpenGL Example"
                'undefined 'undefined 640 480
                '(opengl)))

;;; Use OpenGL 3.3
(sdl2:gl-attribute-set! 'context-profile-mask 'core)
(sdl2:gl-attribute-set! 'context-major-version 3)
(sdl2:gl-attribute-set! 'context-minor-version 3)

;;; Configure basic OpenGL attributes
(sdl2:gl-attribute-set! 'red-size     5)
(sdl2:gl-attribute-set! 'green-size   5)
(sdl2:gl-attribute-set! 'blue-size    5)
(sdl2:gl-attribute-set! 'depth-size   16)
(sdl2:gl-attribute-set! 'doublebuffer 1)

;;; Configure multisampling (anti-aliasing)
(sdl2:gl-attribute-set! 'multisamplebuffers 1)
(sdl2:gl-attribute-set! 'multisamplesamples 4)

;;; Create the OpenGL context
(define gl-context (sdl2:gl-create-context! window))

(assert (equal? window (sdl2:gl-get-current-window)))
(assert (equal? gl-context (sdl2:gl-get-current-context)))


;;; Initialize OpenGL
(gl:init)
(gl-utils:check-error)


;;; For some reason, getting certain attributes causes an OpenGL
;;; error, at least on Mac OS X. This procedure attempts to get the
;;; value of the given attribute, but handles exceptions so that the
;;; program can continue.
(define (gl-attribute-safe attr)
  (condition-case
   (sdl2:gl-attribute attr)
   (e (exn sdl2)
      ((condition-property-accessor 'sdl2 'sdl-error) e))))

;;; The actual OpenGL settings may differ from the requested settings.
(printf
 "Actual OpenGL settings:
  red-size            ~A
  green-size          ~A
  blue-size           ~A
  depth-size          ~A
  doublebuffer        ~A
  multisamplebuffers  ~A
  multisamplesamples  ~A~%"
 (gl-attribute-safe 'red-size)
 (gl-attribute-safe 'green-size)
 (gl-attribute-safe 'blue-size)
 (gl-attribute-safe 'depth-size)
 (gl-attribute-safe 'doublebuffer)
 (gl-attribute-safe 'multisamplebuffers)
 (gl-attribute-safe 'multisamplesamples))


(printf "Drawable size: ~A~%"
        (receive (sdl2:gl-get-drawable-size window)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SHADERS / PROGRAM

(define vertex-shader-source "
#version 330
in vec2 position;
in vec3 color;
out vec3 c;
uniform mat4 MVP;

void main(){
   gl_Position = MVP * vec4(position, 0.0, 1.0);
   c = color;
}")

(define fragment-shader-source "
#version 330
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
                               fragment-shader-source))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MESH

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MATRICES

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RENDER

(define (render)
  (gl:use-program program)

  (gl:uniform-matrix4fv (gl:get-uniform-location program "MVP")
                        1 #f
                        (calc-mvp-matrix))

  (gl:bind-vertex-array (gl-utils:mesh-vao square))
  (gl:draw-elements-base-vertex
   (gl-utils:mode->gl (gl-utils:mesh-mode square))
   (gl-utils:mesh-n-indices square)
   (gl-utils:type->gl (gl-utils:mesh-index-type square))
   #f 0)

  (gl-utils:check-error)
  (gl:bind-vertex-array 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN LOOP

(define (main-loop)
  ;; Loop until the user clicks the close button.
  (while (not (sdl2:quit-requested?))
    (gl:clear (bitwise-ior gl:+color-buffer-bit+
                           gl:+depth-buffer-bit+))

    ;; Update the model's rotation based on how long the program has
    ;; been running.
    (set! *model-z-angle* (* (sdl2:get-ticks) 0.001))

    ;; Render the scene and update the window.
    (render)
    (sdl2:gl-swap-window! window)

    ;; Pause briefly to let the CPU rest.
    (sdl2:delay! 10)

    ;; Flush (clear) all old events so that they don't pile up.
    (sdl2:flush-events! 'first 'last)

    ;; Pump events so that we can detect whether the user has clicked
    ;; the close button.
    (sdl2:pump-events!)))

(main-loop)
