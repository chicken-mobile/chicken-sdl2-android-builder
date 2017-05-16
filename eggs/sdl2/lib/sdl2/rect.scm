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


(export rect-empty?
        rect=?
        point-in-rect?
        enclose-points
        has-intersection?
        intersect-rect
        intersect-rect-and-line
        union-rect)


(define (rect-empty? rect)
  (SDL_RectEmpty rect))


(define (rect=? rect1 rect2)
  (SDL_RectEquals rect1 rect2))


(: point-in-rect?
   (sdl2:point* sdl2:rect* -> boolean))
(define-versioned (point-in-rect? point rect)
    libSDL-2.0.4+
  (SDL_PointInRect point rect))


(: enclose-points
   ((or (list-of sdl2:point*) (vector-of sdl2:point*))
    #!optional (or sdl2:rect* boolean) sdl2:rect*
    -> sdl2:rect boolean))
(define (enclose-points points #!optional clip-rect rect-out)
  (with-temp-mem ((point-array (%points->array points)))
    (let* ((rect-out (or rect-out (alloc-rect)))
           (any-enclosed? (SDL_EnclosePoints
                           point-array
                           (if (vector? points)
                               (vector-length points)
                               (length points))
                           clip-rect rect-out)))
      (values rect-out any-enclosed?))))


(define (has-intersection? rect1 rect2)
  (SDL_HasIntersection rect1 rect2))


(define (intersect-rect rect1 rect2 #!optional rect-out)
  (let* ((rect-out (or rect-out (alloc-rect)))
         (intersects? (SDL_IntersectRect rect1 rect2 rect-out)))
    (values rect-out intersects?)))


(define (intersect-rect-and-line rect x1 y1 x2 y2)
  (with-temp-mem ((x1-in-out (%allocate-Sint32))
                  (y1-in-out (%allocate-Sint32))
                  (x2-in-out (%allocate-Sint32))
                  (y2-in-out (%allocate-Sint32)))
    (pointer-s32-set! x1-in-out x1)
    (pointer-s32-set! y1-in-out y1)
    (pointer-s32-set! x2-in-out x2)
    (pointer-s32-set! y2-in-out y2)
    (let ((intersects? (SDL_IntersectRectAndLine
                        rect
                        x1-in-out y1-in-out
                        x2-in-out y2-in-out)))
      (values intersects?
              (pointer-s32-ref x1-in-out)
              (pointer-s32-ref y1-in-out)
              (pointer-s32-ref x2-in-out)
              (pointer-s32-ref y2-in-out)))))


(define (union-rect rect1 rect2 #!optional rect-out)
  (let ((rect-out (or rect-out (alloc-rect))))
    (SDL_UnionRect rect1 rect2 rect-out)
    rect-out))
