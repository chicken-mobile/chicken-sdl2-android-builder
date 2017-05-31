;; the PPM ascii format is as intuitive as it
;; gets. https://en.wikipedia.org/wiki/Netpbm_format#PPM_example
;;
;; each pixel gets three ascii bytes: R G B
;; newlines are ignored (except in the header).
;; first pixel in file is top-left pixel in image.


;; read a ppm P3 ascii image into RGBA f32vector pixels suitable to
;; opengl.
(define (load-ppm filename #!optional (alpha 1))
  (with-input-from-file filename
    (lambda ()
      (assert (equal? "P3" (read-line)))
      (define w (read))
      (define h (read))
      (define max-color (read))
      (define result (make-f32vector (* w h 4)))

      (do ((y (- h 1) (sub1 y))) ;; upside down
          ((< y 0))
        (do ((x 0 (add1 x)))
            ((>= x w))
          (define r (read))
          (define g (read))
          (define b (read))
          (f32vector-set! result (+ (* y 4 w) (* x 4) 0) (/ r max-color))
          (f32vector-set! result (+ (* y 4 w) (* x 4) 1) (/ g max-color))
          (f32vector-set! result (+ (* y 4 w) (* x 4) 2) (/ b max-color))
          (f32vector-set! result (+ (* y 4 w) (* x 4) 3) alpha)))
      result)))

(define (canvas-pixels-from-ppm canvas filename)
  (canvas-pixels-set! canvas (load-ppm filename)))
