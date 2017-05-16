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


(export make-color            make-color*
        color-set!
        color->list           color->values
        color=?
        color-copy!           color-copy
        copy-color            copy-color*
        color-scale!          color-scale
        color-mult!           color-mult
        color-add!            color-add
        color-sub!            color-sub
        color-lerp!           color-lerp

        make-colour           make-colour*
        colour-set!
        colour->list          colour->values
        colour=?
        colour-copy!          colour-copy
        copy-colour           copy-colour*
        colour-scale!         colour-scale
        colour-mult!          colour-mult
        colour-add!           colour-add
        colour-sub!           colour-sub
        colour-lerp!          colour-lerp)


(define %%color-set!
  (foreign-lambda*
   void ((SDL_Color* c) (Uint8 r) (Uint8 g) (Uint8 b) (Uint8 a))
   "c->r = r; c->g = g; c->b = b; c->a = a;"))

(: %color-set!
   (sdl2:color* fixnum fixnum fixnum fixnum -> void))
(define (%color-set! color r g b a)
  (%%color-set! color
                (%color-r-guard r) (%color-g-guard g)
                (%color-b-guard b) (%color-a-guard a)))


(: make-color
   (#!optional fixnum fixnum fixnum fixnum -> sdl2:color))
(define (make-color #!optional (r 0) (g 0) (b 0) (a 255))
  (let ((color (alloc-color)))
    (%color-set! color r g b a)
    color))

(: make-color*
   (#!optional fixnum fixnum fixnum fixnum -> sdl2:color))
(define (make-color* #!optional (r 0) (g 0) (b 0) (a 255))
  (let ((color (alloc-color*)))
    (%color-set! color r g b a)
    color))


(: color-set!
   (sdl2:color* #!optional fixnum fixnum fixnum fixnum -> sdl2:color*))
(define (color-set! color #!optional r g b a)
  (if (and r g b a)
      (%color-set! color r g b a)
      (begin
        (when r (color-r-set! color r))
        (when g (color-g-set! color g))
        (when b (color-b-set! color b))
        (when a (color-a-set! color a))))
  color)


(: color->list
   (sdl2:color* -> (list fixnum fixnum fixnum fixnum)))
(define (color->list color)
  (list (color-r color)
        (color-g color)
        (color-b color)
        (color-a color)))

(: color->values
   (sdl2:color* -> fixnum fixnum fixnum fixnum))
(define (color->values color)
  (values (color-r color)
          (color-g color)
          (color-b color)
          (color-a color)))


(define (color=? color1 color2)
  (define foreign-equals
    (foreign-lambda*
     bool ((SDL_Color* c1) (SDL_Color* c2))
     "C_return(((c1->r == c2->r) && (c1->g == c2->g) &&
                (c1->b == c2->b) && (c1->a == c2->a))
               ? 1 : 0);"))
  (foreign-equals color1 color2))


(define %color-copy!
  (foreign-lambda*
   void ((SDL_Color* src) (SDL_Color* dest))
   "*dest = *src;"))

(define (color-copy! src dest)
  (%color-copy! src dest)
  dest)

(define (color-copy color)
  (color-copy! color (alloc-color*)))

;;; Deprecated, backward compatibility alias.
(define copy-color color-copy)

;;; Deprecated.
(define (copy-color* color)
  (color-copy! color (alloc-color*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COLOR MATH

;;; Equivalent Scheme implementation, for reference.
;; (define (color-scale! color scale #!optional (dest color))
;;   (sdl2:color-set! dest
;;     (max 0 (min 255 (truncate (* scale (sdl2:color-r color)))))
;;     (max 0 (min 255 (truncate (* scale (sdl2:color-g color)))))
;;     (max 0 (min 255 (truncate (* scale (sdl2:color-b color)))))
;;     (max 0 (min 255 (truncate (* scale (sdl2:color-a color)))))))

(define-function-binding chickenSDL2_ColorScaleInt
  args: ((SDL_Color* c) (Uint8 scale) (SDL_Color* dest)))

(define-function-binding chickenSDL2_ColorScaleFloat
  args: ((SDL_Color* c) (float scale) (SDL_Color* dest)))

(: color-scale!
   (sdl2:color* number #!optional sdl2:color*
    -> sdl2:color*))
(define (color-scale! c scale #!optional (dest c))
  (let ((scale (max 0 (min 255 scale))))
    (if (integer? scale)
        (chickenSDL2_ColorScaleInt c scale dest)
        (chickenSDL2_ColorScaleFloat c scale dest)))
  dest)

(: color-scale
   (sdl2:color* number -> sdl2:color*))
(define (color-scale c scale)
  (color-scale! c scale (alloc-color)))


;;; Equivalent Scheme implementation, for reference.
;; (define (color-mult! color1 color2 #!optional (dest color1))
;;   (define (blend n1 n2 t) (+ n1 (* (- n2 n1) t)))
;;   (let-values (((r1 g1 b1 a1) (sdl2:color->values color1))
;;                ((r2 g2 b2 a2) (sdl2:color->values color2)))
;;     (sdl2:color-set! dest
;;       (blend r1 (* r1 r2 1/255) (/ a2 255))
;;       (blend g1 (* g1 g2 1/255) (/ a2 255))
;;       (blend b1 (* b1 b2 1/255) (/ a2 255))
;;       a1)))

(define-function-binding chickenSDL2_ColorMult
  args: ((SDL_Color* c1) (SDL_Color* c2) (SDL_Color* dest)))

(: color-mult!
   (sdl2:color* sdl2:color* #!optional sdl2:color*
    -> sdl2:color*))
(define (color-mult! c1 c2 #!optional (dest c1))
  (chickenSDL2_ColorMult c1 c2 dest)
  dest)

(: color-mult
   (sdl2:color* sdl2:color* -> sdl2:color*))
(define (color-mult c1 c2)
  (color-mult! c1 c2 (alloc-color)))


;;; Equivalent Scheme implementation, for reference.
;; (define (color-add! color1 color2 #!optional (dest color1))
;;   (let-values (((r1 g1 b1 a1) (sdl2:color->values color1))
;;                ((r2 g2 b2 a2) (sdl2:color->values color2)))
;;     (sdl2:color-set! dest
;;       (min 255 (+ r1 (* r2 a2 1/255)))
;;       (min 255 (+ g1 (* g2 a2 1/255)))
;;       (min 255 (+ b1 (* b2 a2 1/255)))
;;       a1)))

(define-function-binding chickenSDL2_ColorAdd
  args: ((SDL_Color* c1) (SDL_Color* c2) (SDL_Color* dest)))

(: color-add!
   (sdl2:color* sdl2:color* #!optional sdl2:color*
    -> sdl2:color*))
(define (color-add! c1 c2 #!optional (dest c1))
  (chickenSDL2_ColorAdd c1 c2 dest)
  dest)

(: color-add
   (sdl2:color* sdl2:color* -> sdl2:color*))
(define (color-add c1 c2)
  (color-add! c1 c2 (alloc-color)))


;;; Equivalent Scheme implementation, for reference.
;; (define (color-sub! color1 color2 #!optional (dest color1))
;;   (let-values (((r1 g1 b1 a1) (sdl2:color->values color1))
;;                ((r2 g2 b2 a2) (sdl2:color->values color2)))
;;     (sdl2:color-set! dest
;;       (max 0 (- r1 (* r2 a2 1/255)))
;;       (max 0 (- g1 (* g2 a2 1/255)))
;;       (max 0 (- b1 (* b2 a2 1/255)))
;;       a1)))

(define-function-binding chickenSDL2_ColorSub
  args: ((SDL_Color* c1) (SDL_Color* c2) (SDL_Color* dest)))

(: color-sub!
   (sdl2:color* sdl2:color* #!optional sdl2:color*
    -> sdl2:color*))
(define (color-sub! c1 c2 #!optional (dest c1))
  (chickenSDL2_ColorSub c1 c2 dest)
  dest)

(: color-sub
   (sdl2:color* sdl2:color* -> sdl2:color*))
(define (color-sub c1 c2)
  (color-sub! c1 c2 (alloc-color)))


(define-function-binding chickenSDL2_ColorLerp
  args: ((SDL_Color* c1) (SDL_Color* c2) (double t) (SDL_Color* dest)))

(: color-lerp!
   (sdl2:color* sdl2:color* float #!optional sdl2:color*
    -> sdl2:color*))
(define (color-lerp! c1 c2 t #!optional (dest c1))
  (chickenSDL2_ColorLerp c1 c2 t dest)
  dest)

(: color-lerp
   (sdl2:color* sdl2:color* float -> sdl2:color))
(define (color-lerp c1 c2 t)
  (color-lerp! c1 c2 t (alloc-color)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "COLOUR" ALIASES

(define make-colour           make-color)
(define make-colour*          make-color*)
(define colour-set!           color-set!)
(define colour->list          color->list)
(define colour->values        color->values)
(define colour=?              color=?)
(define colour-copy!          color-copy!)
(define colour-copy           color-copy)
(define copy-colour           copy-color)
(define copy-colour*          copy-color*)
(define colour-scale!         color-scale!)
(define colour-scale          color-scale)
(define colour-mult!          color-mult!)
(define colour-mult           color-mult)
(define colour-add!           color-add!)
(define colour-add            color-add)
(define colour-sub!           color-sub!)
(define colour-sub            color-sub)
(define colour-lerp!          color-lerp!)
(define colour-lerp           color-lerp)
