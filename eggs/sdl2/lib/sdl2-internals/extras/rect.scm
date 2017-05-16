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


(export make-rect             make-rect*
        rect-set!
        rect->list            rect->values
        rect-copy!            rect-copy
        copy-rect             copy-rect*

        rect-scale!           rect-scale
        rect-unscale!         rect-unscale
        rect-move!            rect-move
        rect-add-point!       rect-add-point
        rect-sub-point!       rect-sub-point
        rect-grow!            rect-grow
        rect-grow/center!     rect-grow/center
        rect-lerp!            rect-lerp
        rect-lerp-xy!         rect-lerp-xy)


(define %%rect-set!
  (foreign-lambda*
   void ((SDL_Rect* r) (Sint32 x) (Sint32 y) (Sint32 w) (Sint32 h))
   "r->x = x; r->y = y; r->w = w; r->h = h;"))

(: %rect-set!
   (sdl2:rect* fixnum fixnum fixnum fixnum -> void))
(define (%rect-set! rect x y w h)
  (%%rect-set! rect
               (%rect-x-guard x) (%rect-y-guard y)
               (%rect-w-guard w) (%rect-h-guard h)))


(: make-rect
   (#!optional fixnum fixnum fixnum fixnum -> sdl2:rect))
(define (make-rect #!optional (x 0) (y 0) (w 0) (h 0))
  (let ((rect (alloc-rect)))
    (%rect-set! rect x y w h)
    rect))

(: make-rect*
   (#!optional fixnum fixnum fixnum fixnum -> sdl2:rect))
(define (make-rect* #!optional (x 0) (y 0) (w 0) (h 0))
  (let ((rect (alloc-rect*)))
    (%rect-set! rect x y w h)
    rect))


(: rect-set!
   (sdl2:rect* #!optional fixnum fixnum fixnum fixnum -> sdl2:rect*))
(define (rect-set! rect #!optional x y w h)
  (if (and x y w h)
      (%rect-set! rect x y w h)
      (begin
        (when x (rect-x-set! rect x))
        (when y (rect-y-set! rect y))
        (when w (rect-w-set! rect w))
        (when h (rect-h-set! rect h))))
  rect)


(: rect->list
   (sdl2:rect* -> (list fixnum fixnum fixnum fixnum)))
(define (rect->list rect)
  (list (rect-x rect)
        (rect-y rect)
        (rect-w rect)
        (rect-h rect)))

(: rect->values
   (sdl2:rect* -> fixnum fixnum fixnum fixnum))
(define (rect->values rect)
  (values (rect-x rect)
          (rect-y rect)
          (rect-w rect)
          (rect-h rect)))


(define %rect-copy!
  (foreign-lambda*
   void ((SDL_Rect* src) (SDL_Rect* dest))
   "*dest = *src;"))

(define (rect-copy! src dest)
  (%rect-copy! src dest)
  dest)

(define (rect-copy rect)
  (rect-copy! rect (alloc-rect)))

;;; Deprecated, backward compatibility alias.
(define copy-rect rect-copy)

;;; Deprecated.
(define (copy-rect* rect)
  (rect-copy! rect (alloc-rect*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RECT MATH

(define-function-binding chickenSDL2_RectScale_i
  args: ((SDL_Rect* r) (Sint32 scale) (SDL_Rect* dest)))

(define-function-binding chickenSDL2_RectScale_d
  args: ((SDL_Rect* r) (double scale) (SDL_Rect* dest)))

(: rect-scale!
   (sdl2:rect* number #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-scale! r scale #!optional (dest r))
  (if (integer? scale)
      (chickenSDL2_RectScale_i r scale dest)
      (chickenSDL2_RectScale_d r scale dest))
  dest)

(: rect-scale
   (sdl2:rect* number -> sdl2:rect*))
(define (rect-scale r scale)
  (rect-scale! r scale (alloc-rect)))



(define-function-binding chickenSDL2_RectUnscale_i
  args: ((SDL_Rect* r) (Sint32 scale) (SDL_Rect* dest)))

(define-function-binding chickenSDL2_RectUnscale_d
  args: ((SDL_Rect* r) (double scale) (SDL_Rect* dest)))

(: rect-unscale!
   (sdl2:rect* number #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-unscale! r scale #!optional (dest r))
  (when (zero? scale) (error 'rect-unscale! "Division by 0"))
  (if (integer? scale)
      (chickenSDL2_RectUnscale_i r scale dest)
      (chickenSDL2_RectUnscale_d r scale dest))
  dest)

(: rect-unscale
   (sdl2:rect* number -> sdl2:rect*))
(define (rect-unscale r scale)
  (when (zero? scale) (error 'rect-unscale "Division by 0"))
  (rect-unscale! r scale (alloc-rect)))



(define-function-binding chickenSDL2_RectMove
  args: ((SDL_Rect* r) (Sint32 dx) (Sint32 dy) (SDL_Rect* dest)))

(: rect-move!
   (sdl2:rect* fixnum fixnum #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-move! r dx dy #!optional (dest r))
  (chickenSDL2_RectMove r dx dy dest)
  dest)

(: rect-move
   (sdl2:rect* fixnum fixnum -> sdl2:rect*))
(define (rect-move r x y)
  (rect-move! r x y (alloc-rect)))



(define-function-binding chickenSDL2_RectAddPoint
  args: ((SDL_Rect* r) (SDL_Point* p) (SDL_Rect* dest)))

(: rect-add-point!
   (sdl2:rect* sdl2:point* #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-add-point! r p #!optional (dest r))
  (chickenSDL2_RectAddPoint r p dest)
  dest)

(: rect-add-point
   (sdl2:rect* sdl2:point* -> sdl2:rect*))
(define (rect-add-point r p)
  (rect-add-point! r p (alloc-rect)))



(define-function-binding chickenSDL2_RectSubPoint
  args: ((SDL_Rect* r) (SDL_Point* p) (SDL_Rect* dest)))

(: rect-sub-point!
   (sdl2:rect* sdl2:point* #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-sub-point! r p #!optional (dest r))
  (chickenSDL2_RectSubPoint r p dest)
  dest)

(: rect-sub-point
   (sdl2:rect* sdl2:point* -> sdl2:rect*))
(define (rect-sub-point r p)
  (rect-sub-point! r p (alloc-rect)))



(define-function-binding chickenSDL2_RectGrow
  args: ((SDL_Rect* r) (Sint32 dw) (Sint32 dh) (SDL_Rect* dest)))

(: rect-grow!
   (sdl2:rect* fixnum fixnum #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-grow! r dw dh #!optional (dest r))
  (chickenSDL2_RectGrow r dw dh dest)
  dest)

(: rect-grow
   (sdl2:rect* fixnum fixnum -> sdl2:rect*))
(define (rect-grow r w h)
  (rect-grow! r w h (alloc-rect)))



(define-function-binding chickenSDL2_RectGrowCenter
  args: ((SDL_Rect* r) (Sint32 dw) (Sint32 dh) (SDL_Rect* dest)))

(: rect-grow!
   (sdl2:rect* fixnum fixnum #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-grow/center! r w h #!optional (dest r))
  (chickenSDL2_RectGrowCenter r w h dest)
  dest)

(: rect-grow/center
   (sdl2:rect* fixnum fixnum -> sdl2:rect*))
(define (rect-grow/center r w h)
  (rect-grow/center! r w h (alloc-rect)))



(define-function-binding chickenSDL2_RectLerp
  args: ((SDL_Rect* r1) (SDL_Rect* r2) (double t) (SDL_Rect* dest)))

(: rect-lerp!
   (sdl2:rect* sdl2:rect* float #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-lerp! r1 r2 t #!optional (dest r1))
  (chickenSDL2_RectLerp r1 r2 t dest)
  dest)

(: rect-lerp
   (sdl2:rect* sdl2:rect* float -> sdl2:rect*))
(define (rect-lerp r1 r2 t)
  (rect-lerp! r1 r2 t (alloc-rect)))



(define-function-binding chickenSDL2_RectLerpXY
  args: ((SDL_Rect* r1) (SDL_Rect* r2) (double t) (SDL_Rect* dest)))

(: rect-lerp-xy!
   (sdl2:rect* sdl2:rect* float #!optional sdl2:rect*
    -> sdl2:rect*))
(define (rect-lerp-xy! r1 r2 t #!optional (dest r1))
  (chickenSDL2_RectLerpXY r1 r2 t dest)
  dest)

(: rect-lerp-xy
   (sdl2:rect* sdl2:rect* float -> sdl2:rect*))
(define (rect-lerp-xy r1 r2 t)
  (rect-lerp-xy! r1 r2 t (alloc-rect)))
