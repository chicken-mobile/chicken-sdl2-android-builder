
(begin
  (p/diffuse prs obstacles obstacles 0.0001 0.1 64)
  (canvas-swap! obstacles prs))

(define p/add-vel-when
  (let ()

    (define prg (create-program #f "
#version 300 es
precision mediump float;

out vec4 FragColor;

uniform sampler2D Velocity;
uniform sampler2D Source;

uniform vec2  Gravity;

void main() {
    ivec2 T = ivec2(gl_FragCoord.xy);
    vec4 vel = texelFetch(Velocity, T, 0);
    float den = texelFetch(Source, T, 0).x;

    FragColor = vel + vec4(den > 0.0 ? Gravity : vec2(0), 0.0, 0.0);
}
"))

    (let-program-locations
     prg (Velocity Source Gravity)

     (lambda (out velocity source gravity-x gravity-y)

       (with-program
        prg
        (with-output-to-canvas
         out

         (gl:uniform1i Source 1)
         (gl:active-texture gl:+texture1+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex source))

         (gl:uniform1i Velocity 0)
         (gl:active-texture gl:+texture0+)
         (gl:bind-texture   gl:+texture-2d+ (canvas-tex velocity))

         (gl:uniform2f Gravity gravity-x gravity-y)

         (render-square)))))))

(define (core-iteration)
  (define dt 0.25)
  (define iterations 4)
  ;;(define old (f32vector-sum (canvas-pixels den)))
  ;;

  ;;(p/+ prs den 0)

  ;;(p/project vel2 prs divergence vel obstacles 8) (canvas-swap! vel vel2)
  ;;(p/advect den2 vel den obstacles 0.1 1) (canvas-swap! den den2)
  ;;(p/project vel2 prs divergence vel obstacles 8) (canvas-swap! vel vel2)
  ;;(p/subtract-gradient vel2 vel den obstacles 0.05) (canvas-swap! vel vel2)
  ;;(p/advect-conserving den2 vel den obstacles 0.01) (canvas-swap! den den2)

  ;;(p/advect-conserving vel2 vel vel obstacles dt 1) (canvas-swap! vel vel2)
  (p/advect den2 vel den obstacles 0.1 1) (canvas-swap! den den2)
  (p/project2 vel2 prs divergence vel obstacles den iterations) (canvas-swap! vel vel2)
  (p/advect vel2 vel vel obstacles 0.1 1) (canvas-swap! vel vel2)

  ;;(p/subtract-gradient2 vel2 vel den obstacles 0.1) (canvas-swap! vel vel2)
  ;;(p/diffuse den2 den obstacles 0.0001 0.01 20) (canvas-swap! den den2)
  ;;(p/diffuse vel2 vel obstacles 0.001 0.1 20) (canvas-swap! vel vel2)
  ;;(p/splat vel 0.5 0.5 0.01   -1 0 0)
  ;;(p/add-vel-when vel2 vel den 0 -0.005) (canvas-swap! vel vel2)
  (p/project2 vel2 prs divergence vel obstacles den iterations) (canvas-swap! vel vel2)
  ;;(p/* vel2 vel den) (canvas-swap! vel vel2)

  )

(define (core-iteration)
  ;;(p/diffuse-conserving den2 den obstacles 0.01) (canvas-swap! den den2)
  (p/swe-advect vel2 vel den obstacles) (canvas-swap! vel vel2)
  (p/swe-dh den2 vel den obstacles) (canvas-swap! den den2)
  ;;(if(set! pause #t))
  ;;(define s (f32vector-sum (canvas-pixels den)))
  ;;(print s) (if (> s 1000) (set! pause #t))
  )

(begin
)

(define (reset!)
  (p/fill den 0 0 0 0)
  (p/fill vel 0 0 0 0)
  (p/splat den 0.5 0.5 0.02 1 0 0)
  )

(define obstacles2 (create-canvas (canvas-w obstacles) (canvas-h obstacles) (canvas-d obstacles)))
(define zero (create-canvas (canvas-w obstacles) (canvas-h obstacles) (canvas-d obstacles)))

(begin (p/diffuse-conserving obstacles2 obstacles zero)
       (canvas-swap! obstacles obstacles2))


;; crash: (define core-iteration #f)
(begin
  (p/fill obstacles 0 0 0 0)
  (p/splat obstacles 0.5 0.5 0.25    1 0 0)
  (p/splat obstacles 0.5 0.5 0.23    0 0 0)

  (p/fill den 0 0 0 0)
  (p/splat den 0.5 0.5 0.05    100 0 0))

(define (reset!)
  (p/fill den 0 0 0 0)
  (p/fill vel 0 0 0 0)
  (p/fill prs 0 0 0 0)

  ;; (p/splat vel 0.5 0.5 0.1      1   1 0)
  ;; (p/splat vel 0.65 0.65 0.13   0.1 0.1 0)
  ;; (p/splat vel 0.65 0.35 0.13   0.1 -0.1 0)
  ;; (p/splat vel 0.35 0.35 0.13   -0.1 -0.1 0)

  (p/splat den 0.5 0.5 0.05  100 0 0))

(begin
  (p/diffuse vel2 vel obstacles 0.00001 0.001) (canvas-swap! vel vel2))

(p/project)


(p/fill den 0 0 0 0)

(begin
  (define old (f32vector-sum (canvas-pixels den)))
  (p/advect-conserving den2 vel den 1) (canvas-swap! den den2)
  (print "delta: " (- old (f32vector-sum (canvas-pixels den)))))

(begin
  (thread-terminate! thread)
  (define thread
    (thread-start!
     (lambda () (let loop ()
             (print (map floor (receive (f32vector-sum (canvas-pixels den)))))
             (thread-sleep! 1)
             (loop))))))

;; (with-output-to-file "density.f32" (lambda () (write (canvas-pixels den))))

(define pix (canvas-pixels vel))

(define (section canvas x0 y0  w h)
  (define pixels (canvas-pixels canvas))
  (define W (canvas-w canvas))
  (define H (canvas-h canvas))
  (define D (canvas-d canvas))
  (define s (make-f32vector (* w h)))
  (define x1 (+ x0 w))
  (define y1 (+ y0 h))

  (define (pget x y)
    (f32vector-ref pixels (+ (* W D y) (* D x))))

  (do ((y 0 (add1 y)))
      ((>= y h))
    (do ((x 0 (add1 x)))
        ((>= x w))
      (f32vector-set! s
                      (+ (* y w) x)
                      (pget (+ x0 x) (+ y0 y)))))
  s)

(section vel 140 90 25 25)
(ttf:init!)
(use (prefix gl-type glt:))
(define face (glt:load-face "ComicNeue-Regular.otf" 10))

(glt:string-mesh "hello “world” «µπ»" face)
