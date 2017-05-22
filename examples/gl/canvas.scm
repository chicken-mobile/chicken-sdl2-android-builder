
(define-syntax with-texture
  (syntax-rules ()
    ((_ id body ...)
     (dynamic-wind (lambda () (gl:bind-texture gl:+texture-2d+ id))
                   (lambda () body ...)
                   (lambda () (gl:bind-texture gl:+texture-2d+ 0))))))


(define-syntax with-program
  (syntax-rules ()
    ((_ p body ...)
     (dynamic-wind (lambda () (gl:use-program (if (program? p) (program-id p) p)))
                   (lambda () body ...)
                   (lambda () (gl:use-program 0))))))


(define-syntax with-output-to-canvas
  (syntax-rules ()
    ((_ id body ...)
     (dynamic-wind (lambda () (gl:bind-framebuffer gl:+framebuffer+
                                              (if (canvas? id) (canvas-fb id) id)))
                   (lambda () body ...)
                   (lambda () (gl:bind-framebuffer gl:+framebuffer+ 0))))))


;; ==================== program record ====================
(define-record program id)
(define (create-program vertex-shader fragment-shader)

  (define (check-shader shader)
    (if (zero? shader) (error "problem with shader " shader)
        shader))

  (let* ((vertex-shader (or vertex-shader "
#version 300 es
in vec2 position;
void main(){
   gl_Position = vec4(position, 0.0, 1.0);
}"))
         (id (gl-utils:make-program
              (list (check-shader
                     (if (string? vertex-shader)
                         (gl-utils:make-shader gl:+vertex-shader+ vertex-shader)
                         vertex-shader))
                    (check-shader
                     (if (string? fragment-shader)
                         (gl-utils:make-shader gl:+fragment-shader+ fragment-shader)
                         fragment-shader))))))

    (set-finalizer! (make-program id)
                    (lambda (p)
                      (print "deleting program " p)
                      (gl:delete-program (program-id p))))))



;; a canvas is basically a framebuffer with texture. you can render to
;; a canvas using its framebuffer, and you can read its output from
;; its texture.
(define-record canvas fb tex w h d)
(define-record-printer canvas
  (lambda (x p)
    (display "#<canvas " p)
    (display (conc (canvas-w x) "x" (canvas-h x) "x" (canvas-d x)) p)
    (display ">" p)))

(define (create-canvas w h dimensions)

  (let ((tex (gl-utils:gen-texture))
        (fbo (gl-utils:gen-framebuffer)))

    (gl-utils:set-texture-properties tex wrap: gl:+clamp-to-edge+)

    (with-texture
     tex
     (case dimensions
       ((1) (gl:tex-image-2d gl:+texture-2d+ 0 gl:+r16f+    w h 0 gl:+red+  gl:+float+ #f))
       ((2) (gl:tex-image-2d gl:+texture-2d+ 0 gl:+rg16f+   w h 0 gl:+rg+   gl:+float+ #f))
       ((3) (gl:tex-image-2d gl:+texture-2d+ 0 gl:+rgb16f+  w h 0 gl:+rgb+  gl:+float+ #f))
       ((4) (gl:tex-image-2d gl:+texture-2d+ 0 gl:+rgba16f+ w h 0 gl:+rgba+ gl:+float+ #f))))

    (gl:bind-framebuffer gl:+framebuffer+ fbo)
    (gl:framebuffer-texture-2d gl:+framebuffer+ gl:+color-attachment0+
                               gl:+texture-2d+ tex 0)

    (define canvas
      (set-finalizer! (make-canvas fbo tex w h dimensions)
                      (lambda (c)
                        (print "gc: canvas cleanup " c)
                        (gl-utils:delete-texture (canvas-tex c))
                        (gl-utils:delete-framebuffer (canvas-fb c)))))

    (if (= gl:+framebuffer-complete+ (gl:check-framebuffer-status gl:+framebuffer+))
        canvas
        (error "incomplete framebuffer (probably format problem)" fbo))))




(define (canvas-swap! a b)
  (let ((t (canvas-tex b))) (canvas-tex-set! b (canvas-tex a)) (canvas-tex-set! a t))
  (let ((t (canvas-fb  b))) (canvas-fb-set!  b (canvas-fb  a)) (canvas-fb-set!  a t))
  (let ((t (canvas-w   b))) (canvas-w-set!   b (canvas-w   a)) (canvas-w-set!   a t))
  (let ((t (canvas-h   b))) (canvas-h-set!   b (canvas-h   a)) (canvas-h-set!   a t)))


(define (canvas-pixels canvas)
  (define px (make-f32vector (* (canvas-w canvas)
                                (canvas-h canvas)
                                (canvas-d canvas))
                             -1))
  (with-output-to-canvas
   canvas
   (gl:read-pixels 0 0 (canvas-w canvas) (canvas-h canvas)
                   (case (canvas-d canvas)
                     ((1) gl:+red+)
                     ((2) gl:+rg+)
                     ((3) gl:+rgb+)
                     ((4) gl:+rgba+)
                     (else (error "invalid dimensions " canvas)))
                   gl:+float+
                   (gl-utils:->pointer px)))
  px)


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


