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


(export render-draw-blend-mode  render-draw-blend-mode-set!
        render-draw-blend-mode-raw
        render-draw-color       render-draw-color-set!
        render-draw-colour      render-draw-colour-set!

        render-clear!
        render-draw-line!       render-draw-lines!
        render-draw-point!      render-draw-points!
        render-draw-rect!       render-draw-rects!
        render-fill-rect!       render-fill-rects!)



(: render-draw-blend-mode
   (sdl2:renderer* -> symbol))
(define (render-draw-blend-mode renderer)
  (blend-mode->symbol (render-draw-blend-mode-raw renderer)))

(: render-draw-blend-mode-raw
   (sdl2:renderer* -> fixnum))
(define (render-draw-blend-mode-raw renderer)
  (with-temp-mem ((mode-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetRenderDrawBlendMode renderer mode-out)))
      (if (zero? ret-code)
          (pointer-u8-ref mode-out)
          (begin
            (free mode-out)
            (abort (sdl-failure "SDL_GetRenderDrawBlendMode" ret-code)))))))


(: render-draw-blend-mode-set!
   (sdl2:renderer* symbol -> void))
(define (render-draw-blend-mode-set! renderer blend-mode)
  (define (bad-mode-err x)
    (error 'render-draw-blend-mode-set!
           "Invalid blend mode" x))
  (let* ((mode-int (cond ((integer? blend-mode)
                          blend-mode)
                         (else
                          (symbol->blend-mode
                           blend-mode bad-mode-err))))
         (ret-code (SDL_SetRenderDrawBlendMode renderer mode-int)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetRenderDrawBlendMode" ret-code)))))

(set! (setter render-draw-blend-mode) render-draw-blend-mode-set!)



(: render-draw-color
   (sdl2:renderer* -> fixnum fixnum fixnum fixnum))
(define (render-draw-color renderer)
  (with-temp-mem ((r-out (%allocate-Uint8))
                  (g-out (%allocate-Uint8))
                  (b-out (%allocate-Uint8))
                  (a-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetRenderDrawColor
                     renderer r-out g-out b-out a-out)))
      (if (zero? ret-code)
          (values (pointer-u8-ref r-out)
                  (pointer-u8-ref g-out)
                  (pointer-u8-ref b-out)
                  (pointer-u8-ref a-out))
          (begin
            (free r-out)
            (free g-out)
            (free b-out)
            (free a-out)
            (abort (sdl-failure "SDL_GetRenderDrawColor" ret-code)))))))


(: render-draw-color-set!
   (sdl2:renderer* (or sdl2:color* (list-of fixnum)) -> void))
(define (render-draw-color-set! renderer rgba-or-color)
  (assert (or (list? rgba-or-color) (color? rgba-or-color)))
  (receive (r g b a) (if (list? rgba-or-color)
                         (values (list-ref rgba-or-color 0)
                                 (list-ref rgba-or-color 1)
                                 (list-ref rgba-or-color 2)
                                 (if (< 3 (length rgba-or-color))
                                     (list-ref rgba-or-color 3)
                                     255))
                         (values (color-r rgba-or-color)
                                 (color-g rgba-or-color)
                                 (color-b rgba-or-color)
                                 (color-a rgba-or-color)))
    (let ((ret-code (SDL_SetRenderDrawColor renderer r g b a)))
      (unless (zero? ret-code)
        (abort (sdl-failure "SDL_SetRenderDrawColor" ret-code))))))

(set! (setter render-draw-color) render-draw-color-set!)


;;; "Colour" aliases
(define render-draw-colour      render-draw-color)
(define render-draw-colour-set! render-draw-color-set!)



(: render-clear!
   (sdl2:renderer* -> void))
(define (render-clear! renderer)
  (let ((ret-code (SDL_RenderClear renderer)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderClear" ret-code)))))



(: render-draw-line!
   (sdl2:renderer* fixnum fixnum fixnum fixnum -> void))
(define (render-draw-line! renderer x1 y1 x2 y2)
  (let ((ret-code (SDL_RenderDrawLine renderer x1 y1 x2 y2)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderDrawLine" ret-code)))))


(: render-draw-lines!
   (sdl2:renderer*
    (or (list-of sdl2:point*) (vector-of sdl2:point*))
    -> void))
(define (render-draw-lines! renderer points)
  (with-temp-mem ((point-array (%points->array points)))
    (let ((ret-code (SDL_RenderDrawLines
                     renderer point-array
                     (if (vector? points)
                         (vector-length points)
                         (length points)))))
      (unless (zero? ret-code)
        (free point-array)
        (abort (sdl-failure "SDL_RenderDrawLines" ret-code))))))



(: render-draw-point!
   (sdl2:renderer* fixnum fixnum -> void))
(define (render-draw-point! renderer x y)
  (let ((ret-code (SDL_RenderDrawPoint renderer x y)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderDrawPoint" ret-code)))))


(: render-draw-points!
   (sdl2:renderer*
    (or (list-of sdl2:point*) (vector-of sdl2:point*))
    -> void))
(define (render-draw-points! renderer points)
  (with-temp-mem ((point-array (%points->array points)))
    (let ((ret-code (SDL_RenderDrawPoints
                     renderer point-array
                     (if (vector? points)
                         (vector-length points)
                         (length points)))))
      (unless (zero? ret-code)
        (free point-array)
        (abort (sdl-failure "SDL_RenderDrawPoints" ret-code))))))



(: render-draw-rect!
   (sdl2:renderer* (or sdl2:rect* boolean) -> void))
(define (render-draw-rect! renderer rect)
  (let ((ret-code (SDL_RenderDrawRect renderer rect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderDrawRect" ret-code)))))


(: render-draw-rects!
   (sdl2:renderer*
    (or (list-of sdl2:rect*) (vector-of sdl2:rect*))
    -> void))
(define (render-draw-rects! renderer rects)
  (with-temp-mem ((rect-array (%rects->array rects)))
    (let ((ret-code (SDL_RenderDrawRects
                     renderer rect-array
                     (if (vector? rects)
                         (vector-length rects)
                         (length rects)))))
      (unless (zero? ret-code)
        (free rect-array)
        (abort (sdl-failure "SDL_RenderDrawRects" ret-code))))))



(: render-fill-rect!
   (sdl2:renderer* (or sdl2:rect* boolean) -> void))
(define (render-fill-rect! renderer rect)
  (let ((ret-code (SDL_RenderFillRect renderer rect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_RenderFillRect" ret-code)))))


(: render-fill-rects!
   (sdl2:renderer*
    (or (list-of sdl2:rect*) (vector-of sdl2:rect*))
    -> void))
(define (render-fill-rects! renderer rects)
  (with-temp-mem ((rect-array (%rects->array rects)))
    (let ((ret-code (SDL_RenderFillRects
                     renderer rect-array
                     (if (vector? rects)
                         (vector-length rects)
                         (length rects)))))
      (unless (zero? ret-code)
        (free rect-array)
        (abort (sdl-failure "SDL_RenderFillRects" ret-code))))))
