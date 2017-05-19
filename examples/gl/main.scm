
(use (prefix sdl2 sdl2:)
     (prefix opengl-glew  gl:)
     (prefix gl-utils     gl-utils:)
     (prefix gl-math      gl-math:)
     miscmacros)

(import (prefix sdl2 sdl2:)
        (prefix opengl-glew gl:)
        (prefix gl-utils gl-utils:)
        (prefix gl-math gl-math:))

(include "sdl2-setup.scm")


;; ==================== utils ====================

(define-syntax with-texture
  (syntax-rules ()
    ((_ id body ...)
     (dynamic-wind (lambda () (gl:bind-texture gl:+texture-2d+ id))
                   (lambda () body ...)
                   (lambda () (gl:bind-texture gl:+texture-2d+ 0))))))

;; ==================== program record ====================
(define-record program id)
(define (create-program vertex-shader fragment-shader)
  (let* ((vertex-shader (or vertex-shader "
#version 300 es
in vec2 position;
void main(){
   gl_Position = vec4(position, 0.0, 1.0);
}"))
         (id (gl-utils:make-program
              (list (if (string? vertex-shader)
                        (gl-utils:make-shader gl:+vertex-shader+ vertex-shader)
                        vertex-shader)
                    (if (string? fragment-shader)
                        (gl-utils:make-shader gl:+fragment-shader+ fragment-shader)
                        fragment-shader)))))

    (set-finalizer! (make-program id)
                    (lambda (p)
                      (print "deleting program " p)
                      (gl:delete-program (program-id p))))))

(define-syntax with-program
  (syntax-rules ()
    ((_ p body ...)
     (dynamic-wind (lambda () (gl:use-program (if (program? p) (program-id p) p)))
                   (lambda () body ...)
                   (lambda () (gl:use-program 0))))))


;; a canvas is basically a framebuffer with texture. you can render to
;; a canvas using its framebuffer, and you can read its output from
;; its texture.
(define-record canvas fb tex w h)

(define (create-canvas w h #!key (format gl:+rgb+) (type gl:+unsigned-byte+))
  (let ((tex (gl-utils:gen-texture))
        (fbo (gl-utils:gen-framebuffer)))

    (gl-utils:set-texture-properties tex)

    (with-texture
     tex
     (gl:tex-image-2d gl:+texture-2d+ 0 format w h 0 format type #f))

    (gl:bind-framebuffer gl:+framebuffer+ fbo)
    (gl:framebuffer-texture-2d gl:+framebuffer+ gl:+color-attachment0+
                               gl:+texture-2d+ tex 0)

    (set-finalizer! (make-canvas fbo tex w h)
                    (lambda (c)
                      (print "gc: canvas cleanup " c)
                      (gl-utils:delete-texture (canvas-tex c))
                      (gl-utils:delete-framebuffer (canvas-fb c))))))

(define (canvas-swap! a b)
  (let ((t (canvas-tex b))) (canvas-tex-set! b (canvas-tex a)) (canvas-tex-set! a t))
  (let ((t (canvas-fb  b))) (canvas-fb-set!  b (canvas-fb  a)) (canvas-fb-set!  a t))
  (let ((t (canvas-w   b))) (canvas-w-set!   b (canvas-w   a)) (canvas-w-set!   a t))
  (let ((t (canvas-h   b))) (canvas-h-set!   b (canvas-h   a)) (canvas-h-set!   a t)))

(define-syntax with-output-to-canvas
  (syntax-rules ()
    ((_ id body ...)
     (dynamic-wind (lambda () (gl:bind-framebuffer gl:+framebuffer+
                                              (if (canvas? id) (canvas-fb id) id)))
                   (lambda () body ...)
                   (lambda () (gl:bind-framebuffer gl:+framebuffer+ 0))))))




;; we really just want to incoke fragment shaders for all pixels. this
;; does something like that.
(define render-square
  (let ((square (gl-utils:make-mesh
                 vertices: '(attributes:
                             ((position #:float 2))
                             initial-elements:
                             ((position . (-1 -1
                                              1 -1
                                              1  1
                                              -1  1))))
                 indices: '(type: #:ushort
                                  initial-elements: (0 1 2
                                                       0 2 3)))))
    ;; normally, you'd do this: (gl:get-attrib-location (program-id program) "position")
    ;; 
    ;; but we have lots of different programs around. all of them use
    ;; the same vertex-shader, though, which even has only one
    ;; attribute. so we should be able to hardcode 0.
    (gl-utils:mesh-make-vao! square `((position . 0)))
    (lambda ()
      (gl:bind-vertex-array (gl-utils:mesh-vao square))
      (gl:draw-elements
       (gl-utils:mode->gl (gl-utils:mesh-mode square))
       (gl-utils:mesh-n-indices square)
       (gl-utils:type->gl (gl-utils:mesh-index-type square))
       #f))))


(define p/fade-out

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
 FragColor = texture(VelocityTexture, InverseSize * fragCoord) + amount;
}
")))

    (lambda (out source amount)
      (with-output-to-canvas
       out
       (with-program
        program-blue
        (gl:uniform2f (gl:get-uniform-location (program-id program-blue) "InverseSize")
                      (/ 1 (canvas-w out))
                      (/ 1 (canvas-h out)))
        (gl:uniform1f (gl:get-uniform-location (program-id program-blue) "amount")
                      amount)

        (gl:bind-texture gl:+texture-2d+ (canvas-tex source))
        (render-square))))))

(define (p/fill out r g b a)
  (with-output-to-canvas
   out
   (gl:clear-color r g b a)
   (gl:clear gl:+color-buffer-bit+)))

(define (p/pset out   x y   r g b)
  (with-texture
   (canvas-tex out)
   (gl:tex-sub-image-2d gl:+texture-2d+ ;; target
                        0               ;; level
                        x               ;; x
                        y               ;; y
                        1               ;; w
                        1               ;; h
                        gl:+rgb+
                        gl:+unsigned-byte+
                        (gl-utils:->pointer
                         (string (integer->char (min 255 (inexact->exact (floor (* 255.0 r)))))
                                 (integer->char (min 255 (inexact->exact (floor (* 255.0 g)))))
                                 (integer->char (min 255 (inexact->exact (floor (* 255.0 b))))))))))



(define c0 (create-canvas 512 512))
(define c1 (create-canvas 512 512))

;; ==================== read pixels
;; (define pixels (make-string (* 128 128 4) #\null))
;; (gl-utils:with-framebuffer
;;  frame_buffer
;;  (gl:read-pixels 0 0 128 128
;;                  gl:+rgb+
;;                  gl:+unsigned-byte+
;;                  (gl-utils:->pointer pixels)
;;                  ))


(define (handle event)
  (case (sdl2:event-type event)
    ((window)
     (receive (w h) (sdl2:window-size window)
       (gl:viewport 0 0 w h)))
    ((mouse-motion)
     ;;(print "handling mouse-motion event: " event)
     (define mx (sdl2:mouse-motion-event-x event))
     (define my (sdl2:mouse-motion-event-y event))

     (receive (w h) (sdl2:window-size window)
       (p/pset c0
               (* (canvas-w c0) (/ mx w))
               (* (canvas-h c0) (- 1 (/ my h)))
               255 255 0)))
    (else (print "unhandled " event))))


(define (render canvas)
  (p/fade-out (receive (w h) (sdl2:window-size window)
                (make-canvas 0 #f w h))
              canvas
              0)
  (gl-utils:check-error))


(define core-iteration
  (let ((event (sdl2:make-event)))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))

      (gl:clear-color 0 0 0 0)
      (gl:clear gl:+color-buffer-bit+)

      (p/fade-out c1 c0 -0.005)
      (canvas-swap! c0 c1)

      (render c0)
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

