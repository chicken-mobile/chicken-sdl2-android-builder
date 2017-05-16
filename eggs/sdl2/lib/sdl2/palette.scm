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


(export palette-ref     palette-set!
        palette-colors  palette-colors-set!
        palette-colours palette-colours-set!)


;;; Sets the palette color at index i to a copy of the given
;;; sdl2:color. Throws an error if i is out of bounds.
(define (palette-set! palette i color)
  (assert-bounds i 0 (sub1 (palette-ncolors palette))
                 "index out of bounds" 'palette-set!)
  (SDL_SetPaletteColors palette color 1 i))


;;; Returns a copy of the palette color at index i, as a sdl2:color
;;; instance. Throws an error if i is out of bounds.
(define (palette-ref palette i)
  (assert-bounds i 0 (sub1 (palette-ncolors palette))
                 "index out of bounds" 'palette-ref)
  (let ((color (alloc-color)))
    (%read-color-array color (%palette-colors palette) i)
    color))

(set! (setter palette-ref)
      palette-set!)



;;; Set multiple colors in the palette. colors-vec must be a Scheme
;;; vector of sdl2:color instances. firstcolor specifies the first
;;; index in the palette to start setting. In other words, palette
;;; index firstcolor is set to the first color in colors-vec, palette
;;; index firstcolor+1 is set to the second color, and so on.
;;;
;;; If there are too many colors in the vector, the extra colors will
;;; be ignored and this procedure will return #f. If all given colors
;;; were set, this procedure will return #t.
(: palette-colors-set!
   (sdl2:palette*
    (vector-of sdl2:color*)
    #!optional fixnum
    -> boolean))
(define (palette-colors-set! palette colors-vec
                             #!optional (firstcolor 0))
  (assert (not (struct-null? palette)))
  (assert-bounds firstcolor 0 (sub1 (palette-ncolors palette))
                 "firstcolor out of bounds" 'palette-colors-set!)
  (with-temp-mem ((colors-array (%color-vector->array colors-vec)))
    (zero? (SDL_SetPaletteColors
            palette colors-array firstcolor
            (vector-length colors-vec)))))


;;; Returns a copy of all the colors in the palette, as a Scheme
;;; vector of sdl2:color instances.
(define (palette-colors palette)
  (%color-array->vector
   (%palette-colors palette)
   (palette-ncolors palette)))

(set! (setter palette-colors)
      (lambda (palette colors-vec)
        (palette-colors-set! palette colors-vec)))


(define palette-colours      palette-colors)
(define palette-colours-set! palette-colors-set!)
