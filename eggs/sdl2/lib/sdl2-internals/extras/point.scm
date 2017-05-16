;;
;; chicken-sdl2: CHICKEN Scheme bindings to Simple DirectMedia Layer 2
;;
;; Copyright © 2013, 2015-2016  John Croisant.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


(export make-point            make-point*
        point-set!
        point->list           point->values
        point=?
        point-copy!           point-copy
        copy-point            copy-point*

        point-scale!          point-scale
        point-unscale!        point-unscale
        point-move!           point-move
        point-add!            point-add
        point-sub!            point-sub
        point-lerp!           point-lerp)


(define %%point-set!
  (foreign-lambda*
   void ((SDL_Point* p) (Sint32 x) (Sint32 y))
   "p->x = x; p->y = y;"))

(: %point-set!
   (sdl2:point* fixnum fixnum -> void))
(define (%point-set! point x y)
  (%%point-set! point (%point-x-guard x) (%point-y-guard y)))


(: make-point
   (#!optional fixnum fixnum -> sdl2:point))
(define (make-point #!optional (x 0) (y 0))
  (let ((point (alloc-point)))
    (%point-set! point x y)
    point))

(: make-point*
   (#!optional fixnum fixnum -> sdl2:point))
(define (make-point* #!optional (x 0) (y 0))
  (let ((point (alloc-point*)))
    (%point-set! point x y)
    point))


(: point-set!
   (sdl2:point* #!optional fixnum fixnum -> sdl2:point*))
(define (point-set! point #!optional x y)
  (if (and x y)
      (%point-set! point x y)
      (begin
        (when x (point-x-set! point x))
        (when y (point-y-set! point y))))
  point)


(: point->list
   (sdl2:point* -> (list fixnum fixnum)))
(define (point->list point)
  (list (point-x point)
        (point-y point)))

(: point->values
   (sdl2:point* -> fixnum fixnum))
(define (point->values point)
  (values (point-x point)
          (point-y point)))


(define (point=? point1 point2)
  (define foreign-equals
    (foreign-lambda*
     bool ((SDL_Point* p1) (SDL_Point* p2))
     "C_return(((p1->x == p2->x) && (p1->y == p2->y))
               ? 1 : 0);"))
  (foreign-equals point1 point2))


(define %point-copy!
  (foreign-lambda*
   void ((SDL_Point* src) (SDL_Point* dest))
   "*dest = *src;"))

(define (point-copy! src dest)
  (%point-copy! src dest)
  dest)

(define (point-copy point)
  (point-copy! point (alloc-point)))

;;; Deprecated, backward compatibility alias.
(define copy-point point-copy)

;;; Deprecated.
(define (copy-point* point)
  (point-copy! point (alloc-point*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; POINT MATH

(define-function-binding chickenSDL2_PointScale_i
  args: ((SDL_Point* p) (Sint32 scale) (SDL_Point* dest)))

(define-function-binding chickenSDL2_PointScale_d
  args: ((SDL_Point* p) (double scale) (SDL_Point* dest)))

(: point-scale!
   (sdl2:point* number #!optional sdl2:point*
    -> sdl2:point*))
(define (point-scale! p scale #!optional (dest p))
  (if (integer? scale)
      (chickenSDL2_PointScale_i p scale dest)
      (chickenSDL2_PointScale_d p scale dest))
  dest)

(: point-scale
   (sdl2:point* number -> sdl2:point*))
(define (point-scale p scale)
  (point-scale! p scale (alloc-point)))



(define-function-binding chickenSDL2_PointUnscale_i
  args: ((SDL_Point* p) (Sint32 scale) (SDL_Point* dest)))

(define-function-binding chickenSDL2_PointUnscale_d
  args: ((SDL_Point* p) (double scale) (SDL_Point* dest)))

(: point-unscale!
   (sdl2:point* number #!optional sdl2:point*
    -> sdl2:point*))
(define (point-unscale! p scale #!optional (dest p))
  (when (zero? scale) (error 'point-unscale! "Division by 0"))
  (if (integer? scale)
      (chickenSDL2_PointUnscale_i p scale dest)
      (chickenSDL2_PointUnscale_d p scale dest))
  dest)

(: point-unscale
   (sdl2:point* number -> sdl2:point*))
(define (point-unscale p scale)
  (when (zero? scale) (error 'point-unscale "Division by 0"))
  (point-unscale! p scale (alloc-point)))



(define-function-binding chickenSDL2_PointMove
  args: ((SDL_Point* p) (Sint32 dx) (Sint32 dy) (SDL_Point* dest)))

(: point-move!
   (sdl2:point* fixnum fixnum #!optional sdl2:point*
    -> sdl2:point*))
(define (point-move! p dx dy #!optional (dest p))
  (chickenSDL2_PointMove p dx dy dest)
  dest)

(: point-move
   (sdl2:point* fixnum fixnum -> sdl2:point*))
(define (point-move p x y)
  (point-move! p x y (alloc-point)))



(define-function-binding chickenSDL2_PointAdd
  args: ((SDL_Point* p1) (SDL_Point* p2) (SDL_Point* dest)))

(: point-add!
   (sdl2:point* sdl2:point* #!optional sdl2:point*
    -> sdl2:point*))
(define (point-add! p1 p2 #!optional (dest p1))
  (chickenSDL2_PointAdd p1 p2 dest)
  dest)

(: point-add
   (sdl2:point* sdl2:point* -> sdl2:point*))
(define (point-add p1 p2)
  (point-add! p1 p2 (alloc-point)))



(define-function-binding chickenSDL2_PointSub
  args: ((SDL_Point* p1) (SDL_Point* p2) (SDL_Point* dest)))

(: point-sub!
   (sdl2:point* sdl2:point* #!optional sdl2:point*
    -> sdl2:point*))
(define (point-sub! p1 p2 #!optional (dest p1))
  (chickenSDL2_PointSub p1 p2 dest)
  dest)

(: point-sub
   (sdl2:point* sdl2:point* -> sdl2:point*))
(define (point-sub p1 p2)
  (point-sub! p1 p2 (alloc-point)))



(define-function-binding chickenSDL2_PointLerp
  args: ((SDL_Point* p1) (SDL_Point* p2) (double t) (SDL_Point* dest)))

(: point-lerp!
   (sdl2:point* sdl2:point* float #!optional sdl2:point*
    -> sdl2:point*))
(define (point-lerp! p1 p2 t #!optional (dest p1))
  (chickenSDL2_PointLerp p1 p2 t dest)
  dest)

(: point-lerp
   (sdl2:point* sdl2:point* float -> sdl2:point*))
(define (point-lerp p1 p2 t)
  (point-lerp! p1 p2 t (alloc-point)))
