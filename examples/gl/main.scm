
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

(begin

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
                     (lambda () (gl:bind-framebuffer gl:+framebuffer+ 0)))))))

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

  ;; case 1: glTexImage2D(GL_TEXTURE_2D, 0, GL_R16F,    width, height, 0, GL_RED, GL_HALF_FLOAT, 0); break;
  ;; case 2: glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16F,   width, height, 0, GL_RG, GL_HALF_FLOAT, 0); break;
  ;; case 3: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB16F,  width, height, 0, GL_RGB, GL_HALF_FLOAT, 0); break;
  ;; case 4: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, width, height, 0, GL_RGBA, GL_HALF_FLOAT, 0); break;
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


(define (p/fill out r g b a)
  (with-output-to-canvas
   out
   (gl:clear-color r g b a)
   (gl:clear gl:+color-buffer-bit+)))

(define (p/pset out   x y   r g b a)
  (with-texture
   (canvas-tex out)
   (gl:tex-sub-image-2d gl:+texture-2d+ ;; target
                        0               ;; level
                        x y 2 2         ;; x y w h
                        gl:+rgb+
                        gl:+unsigned-byte+
                        (gl-utils:->pointer (f32vector r g b   r g b   r g b   r g b))))
  (gl-utils:check-error))

;; (p/pset den  100 100  1 0 0 0)

;; (map canvas-tex (list den den2 vel vel2 divergence prs prs2))

(define p/add

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
 FragColor = clamp(texture(VelocityTexture, InverseSize * fragCoord) + amount, 0.0, 1.0);
}
")))
    (define $InverseSize (gl:get-uniform-location (program-id program-blue) "InverseSize"))
    (define $amount (gl:get-uniform-location (program-id program-blue) "amount"))

    (lambda (out source amount)
      (with-output-to-canvas
       out
       (with-program
        program-blue
        (gl:uniform2f $InverseSize (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
        (gl:uniform1f $amount amount)

        (gl:bind-texture gl:+texture-2d+ (canvas-tex source))
        (render-square))))))

(define p/advect
  (let ()
    (define prg
      (create-program
       #f
       "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D VelocityTexture;
uniform sampler2D SourceTexture;
//uniform sampler2D Obstacles;

uniform vec2 InverseSize;
uniform float TimeStep;
uniform float Dissipation;

void main() {
    vec2 fragCoord = gl_FragCoord.xy;
//    float solid = texture(Obstacles, InverseSize * fragCoord).x;
//    if (solid > 0) {
//        FragColor = vec4(0);
//        return;
//    }
//
    vec2 u = texture(VelocityTexture, InverseSize * fragCoord).xy;
    vec2 coord = InverseSize * (fragCoord - TimeStep * u);
    FragColor = Dissipation * texture(SourceTexture, coord);
}
"))
    (define VelocityTexture (gl:get-uniform-location (program-id prg) "VelocityTexture"))
    (define SourceTexture   (gl:get-uniform-location (program-id prg) "SourceTexture"))
    (define InverseSize     (gl:get-uniform-location (program-id prg) "InverseSize"))
    (define TimeStep        (gl:get-uniform-location (program-id prg) "TimeStep"))
    (define Dissipation     (gl:get-uniform-location (program-id prg) "Dissipation"))

    (lambda (out  velocity source timestep dissipation)
      (assert (= 2 (canvas-d velocity)))
      (with-output-to-canvas
       out
       (with-program
        prg

        (gl:uniform1i VelocityTexture 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

        (gl:uniform1i SourceTexture   1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex source))

        (gl:active-texture gl:+texture0+)

        (gl:uniform2f InverseSize     (/ 1 (canvas-w out)) (/ 1 (canvas-h out)))
        (gl:uniform1f TimeStep timestep)
        (gl:uniform1f Dissipation dissipation)
        (render-square))))))

(define p/splat
  (let ()
    (define prg
      (create-program
       #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform vec2 Point;
uniform float Radius;
uniform vec3 FillColor;

void main() {
    float d = distance(Point, gl_FragCoord.xy);
    if (d < Radius) {
        float a = (Radius - d) * 0.5;
        a = min(a, 1.0);
        FragColor = vec4(FillColor, a);
    } else {
        FragColor = vec4(0);
    }
}
"))
    (define Point     (gl:get-uniform-location (program-id prg) "Point"))
    (define Radius    (gl:get-uniform-location (program-id prg) "Radius"))
    (define FillColor (gl:get-uniform-location (program-id prg) "FillColor"))

    (lambda (out  x y radius  r g b)
      (with-output-to-canvas
       out
       (with-program
        prg

        (gl:uniform2f Point     x y)
        (gl:uniform1f Radius    radius)
        (gl:uniform3f FillColor r g b)

        (gl:enable gl:+blend+)
        ;; gl:+one-minus-src-alpha+ wont work on my fairphone2! why!?
        (gl:blend-func gl:+one+ gl:+one+)
        (render-square)
        (gl:disable gl:+blend+)
        )))))

(define p/divergence
  (let ()
    (define prg
      (create-program #f "
#version 300 es
precision mediump float;

out float FragColor;

uniform sampler2D Velocity;
uniform float HalfInverseCellSize;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring velocities:
    vec2 vN = texelFetchOffset(Velocity, T, 0, ivec2(0, 1)).xy;
    vec2 vS = texelFetchOffset(Velocity, T, 0, ivec2(0, -1)).xy;
    vec2 vE = texelFetchOffset(Velocity, T, 0, ivec2(1, 0)).xy;
    vec2 vW = texelFetchOffset(Velocity, T, 0, ivec2(-1, 0)).xy;

    FragColor = HalfInverseCellSize * (vE.x - vW.x + vN.y - vS.y);
}
"))
    (define Velocity            (gl:get-uniform-location (program-id prg) "Velocity"))
    (define HalfInverseCellSize (gl:get-uniform-location (program-id prg) "HalfInverseCellSize"))

    (lambda (out velocity)

      (with-output-to-canvas
       out
       (with-program
        prg

        (gl:uniform1i Velocity 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

        (gl:uniform1f HalfInverseCellSize (/ 0.5 1.25))
        (render-square))))))

(define p/jacobi
  (let ()

    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D Pressure;
uniform sampler2D Divergence;

uniform float Alpha;
uniform float InverseBeta;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    vec4 pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1));
    vec4 pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1));
    vec4 pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0));
    vec4 pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0));
    vec4 pC = texelFetch(Pressure, T, 0);

    vec4 bC = texelFetch(Divergence, T, 0);
    FragColor = (pW + pE + pS + pN + Alpha * bC) * InverseBeta;
}
"))

    (define Pressure    (gl:get-uniform-location (program-id prg) "Pressure"))
    (define Divergence  (gl:get-uniform-location (program-id prg) "Divergence"))
    (define Alpha       (gl:get-uniform-location (program-id prg) "Alpha"))
    (define InverseBeta (gl:get-uniform-location (program-id prg) "InverseBeta"))

    (lambda (out pressure divergence)
      (with-output-to-canvas
       out
       (with-program
        prg


        (define CellSize 1.25)

        (gl:uniform1i Divergence 1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex divergence))

        (gl:uniform1i Pressure 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

        (gl:uniform1f Alpha (* (- CellSize) CellSize))
        (gl:uniform1f InverseBeta 0.25)

        (render-square))))))

(define p/subtract-gradient
  (let ()
    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec2 FragColor;

uniform sampler2D Velocity;
uniform sampler2D Pressure;
uniform float GradientScale;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);

    // Find neighboring pressure:
    float pN = texelFetchOffset(Pressure, T, 0, ivec2(0, 1)).r;
    float pS = texelFetchOffset(Pressure, T, 0, ivec2(0, -1)).r;
    float pE = texelFetchOffset(Pressure, T, 0, ivec2(1, 0)).r;
    float pW = texelFetchOffset(Pressure, T, 0, ivec2(-1, 0)).r;
    float pC = texelFetch(Pressure, T, 0).r;

    // Use center pressure for solid cells:
    vec2 obstV = vec2(0);
    vec2 vMask = vec2(1);

    // Enforce the free-slip boundary condition:
    vec2 oldV = texelFetch(Velocity, T, 0).xy;
    vec2 grad = vec2(pE - pW, pN - pS) * GradientScale;
    vec2 newV = oldV - grad;
    FragColor = (vMask * newV) + obstV;  
}
"))

    

    (define Velocity (gl:get-uniform-location (program-id prg) "Velocity"))
    (define Pressure (gl:get-uniform-location (program-id prg) "Pressure"))
    (define GradientScale (gl:get-uniform-location (program-id prg) "GradientScale"))

    (lambda (out velocity pressure)
      (with-output-to-canvas
       out
       (with-program
        prg

        (define CellSize 1.25)

        (gl:uniform1i Pressure 1)
        (gl:active-texture gl:+texture1+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex pressure))

        (gl:uniform1i Velocity 0)
        (gl:active-texture gl:+texture0+)
        (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

        (gl:uniform1f GradientScale (/ 1.125 CellSize))

        (render-square))))))

(define vel        (create-canvas 512 512 2))
(define vel2       (create-canvas 512 512 2))
(define den        (create-canvas 512 512 1))
(define den2       (create-canvas 512 512 1))
(define divergence (create-canvas 512 512 3))
(define prs        (create-canvas 512 512 1))
(define prs2       (create-canvas 512 512 1))


;; (p/fill vel 0 0 0 0)
;; (p/fill den 0 0 0 0)

;; (canvas-pixels den)
;; (canvas-pixels den2)
;; (canvas-pixels vel)
;; (canvas-pixels divergence)

(define (rain canvas r g b a #!optional (drops 100))
  (repeat drops
          (p/pset canvas
                  (random (canvas-w canvas))
                  (random (canvas-h canvas))
                  r g b a)))

;; (rain den 1 0 0 0)

(define (fluid-iteration)
  ;;(p/pset   den   50  50       1 0 0 0)
  (p/splat  den  256 256 20    1 0 0)
  (p/advect vel2 vel vel 1 1) (canvas-swap! vel vel2)
  (p/advect den2 vel den 1 1) (canvas-swap! den den2)
  (p/divergence divergence vel)

  (p/fill prs 0 0 0 0)
  (repeat 16
          (p/jacobi prs2 prs divergence)
          (canvas-swap! prs prs2))

  (p/subtract-gradient vel2 vel prs) (canvas-swap! vel vel2) )


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
uniform vec2 InverseSize;

void main() {
 vec2 fragCoord = gl_FragCoord.xy;
 float density = texture(DensityTexture, InverseSize * fragCoord).x;
 vec2  vel     = texture(VelocityTexture, InverseSize * fragCoord).xy;
 FragColor = vec4(density, .5+.5*vel.x, .5+.5*vel.y, 0);
}
"))
    (define DensityTexture  (gl:get-uniform-location (program-id prg) "DensityTexture"))
    (define VelocityTexture (gl:get-uniform-location (program-id prg) "VelocityTexture"))
    (define InverseSize     (gl:get-uniform-location (program-id prg) "InverseSize"))

    (lambda (w h  den vel)
      (with-program
       prg
       
       (gl:uniform1i DensityTexture   0)
       (gl:active-texture gl:+texture0+)
       (gl:bind-texture   gl:+texture-2d+ (canvas-tex den))

       (gl:uniform1i VelocityTexture   1)
       (gl:active-texture gl:+texture1+)
       (gl:bind-texture   gl:+texture-2d+ (canvas-tex vel))
       (gl:active-texture gl:+texture0+)

       (gl:uniform2f InverseSize (/ 1 w) (/ 1 h))
       (render-square)))))


(define (handle event)
  (case (sdl2:event-type event)
    ((quit) (sdl2:quit!) (exit 0))
    ((window)
     (receive (w h) (sdl2:window-size window)
       (gl:viewport 0 0 w h)))
    ((key-down)
     (print "handling key" event))
    ((mouse-motion)
     ;;(print "handling mouse-motion event: " event)
     (define mx (sdl2:mouse-motion-event-x event))
     (define my (sdl2:mouse-motion-event-y event))

     (receive (w h) (sdl2:window-size window)
       (let ((x (* (canvas-w den) (/ mx w)))
             (y (* (canvas-h den) (- 1 (/ my h))))
             (xx (+ (sdl2:mouse-motion-event-xrel event))) ;; r
             (yy (- (sdl2:mouse-motion-event-yrel event))))
         (print xx "x" yy)
         (p/splat vel   x y 5
                  xx yy ;; g
                  0 ;; b
                  ))))
    (else (print "unhandled " event))))


(define (render canvas)
  (p/add (receive (w h) (sdl2:window-size window)
                (make-canvas 0 #f w h 3))
              canvas
              0)
  (gl-utils:check-error))

(define core-iteration
  (let ((event (sdl2:make-event)))
    (lambda ()
      (while* (sdl2:poll-event! event) (handle it))

      (gl:clear-color 0 0 0 0)
      (gl:clear gl:+color-buffer-bit+)

      (fluid-iteration)
      (receive (w h) (sdl2:window-size window)
       (visualize w h  den vel))
      (sdl2:gl-swap-window! window))))


(define fps 0) ;; <-- access from your repl
(begin (handle-exceptions e void (thread-terminate! game-thread))
       (define game-thread ;; (thread-state game-thread)
         (thread-start!
          (lambda ()
            (define cm current-milliseconds)
            (let loop ((n 0) (t (cm)))
              (thread-yield!) (thread-sleep! 0.01)
              (core-iteration)
              (cond ((> (- (cm) t) 2000)
                     (set! fps (inexact->exact (round (/ n (/ (- (cm) t) 1000)))))
                     (set! t (cm))
                     (set! n 0)))

              (loop (add1 n) t)))))
       (thread-quantum-set! game-thread 0))

