
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
