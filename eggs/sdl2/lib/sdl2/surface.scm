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


(export free-surface!

        make-surface
        make-surface*
        create-rgb-surface*
        create-rgb-surface-from*

        convert-surface
        convert-surface*

        load-bmp
        load-bmp*
        load-bmp-rw
        load-bmp-rw*
        save-bmp!
        save-bmp-rw!

        lock-surface!
        unlock-surface!
        must-lock?

        blit-surface!
        blit-scaled!

        fill-rect!
        fill-rects!

        surface-ref  surface-set!
        surface-ref-raw

        surface-clip-rect   surface-clip-rect-set!
        surface-color-key   surface-color-key-set!
        surface-color-key-raw
        surface-colour-key  surface-colour-key-set!
        surface-colour-key-raw
        surface-alpha-mod   surface-alpha-mod-set!
        surface-blend-mode  surface-blend-mode-set!
        surface-blend-mode-raw
        surface-color-mod   surface-color-mod-set!
        surface-colour-mod  surface-colour-mod-set!
        surface-palette     surface-palette-set!
        surface-rle-set!

        rotate-surface-90
        rotate-surface-90*
        flip-surface
        flip-surface*)



(define (%map-color-for-surface color surface fn-name)
  (cond
   ((color? color)
    (SDL_MapRGBA (surface-format surface)
                 (color-r color)
                 (color-g color)
                 (color-b color)
                 (color-a color)))
   ((and (integer? color)
         (not (negative? color)))
    color)
   (else
    (error fn-name
           "invalid color (expected sdl2:color or nonnegative integer)"
           color))))


(define (%unmap-color-for-surface pixel surface)
  (assert (and (integer? pixel) (not (negative? pixel))))
  (with-temp-mem ((r-out (%allocate-Uint8))
                  (g-out (%allocate-Uint8))
                  (b-out (%allocate-Uint8))
                  (a-out (%allocate-Uint8)))
    (SDL_GetRGBA pixel
                 (surface-format surface)
                 r-out g-out b-out a-out)
    (make-color (pointer-u8-ref r-out)
                    (pointer-u8-ref g-out)
                    (pointer-u8-ref b-out)
                    (pointer-u8-ref a-out))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FREE SURFACE

(define (free-surface! surface)
  (assert (surface? surface))
  (SDL_FreeSurface surface)
  (%nullify-struct! surface)
  (void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE / CREATE SURFACE

;;; Convenient way to create a new surface of a given size and pixel
;;; depth (bits per pixel). The surface RGBA masks will be chosen
;;; automatically based on the requested depth. The surface will have
;;; a palette if depth <= 8. The surface will automatically be freed
;;; by the garbage collector. Returns #f if the surface could not be
;;; created (use get-error to find out why).
(define (make-surface width height depth)
  (let ((surface (make-surface* width height depth)))
    (when surface
      (set-finalizer! surface free-surface!))
    surface))


;;; Like make-surface, except the surface will NOT automatically
;;; be freed by the garbage collector. You must manually free the
;;; surface (e.g. using free-surface!) when you are done with it.
(define (make-surface* width height depth)
  (assert (and (integer? width)  (positive? width)))
  (assert (and (integer? height) (positive? height)))
  (assert (and (integer? depth)  (positive? depth)))
  (let ((masks (%surface-default-masks depth)))
    (create-rgb-surface* 0 width height depth
                         (list-ref masks 0)
                         (list-ref masks 1)
                         (list-ref masks 2)
                         (list-ref masks 3))))


(define (%surface-default-masks depth)
  (if (= depth 32)
      ;; For depth 32, we need to give explicit masks because there
      ;; seems to be no way to specify a default alpha mask. For red,
      ;; green, and blue, 0 means to use the default, but for alpha, 0
      ;; means no alpha channel.
      (if (= SDL_BYTEORDER SDL_BIG_ENDIAN)
          (list #xff000000
                #x00ff0000
                #x0000ff00
                #x000000ff)
          (list #x000000ff
                #x0000ff00
                #x00ff0000
                #xff000000))
      ;; For other depths, just use 0 to tell SDL to use defaults.
      (list 0 0 0 0)))



(define (create-rgb-surface* flags width height depth
                             rmask gmask bmask amask)
  (let ((surface (SDL_CreateRGBSurface flags width height depth
                                       rmask gmask bmask amask)))
    (if (and (surface? surface)
             (not (struct-null? surface)))
        surface
        (abort (sdl-failure "SDL_CreateRGBSurface" #f)))))


(define (create-rgb-surface-from* pixels width height depth pitch
                                  rmask gmask bmask amask)
  (let ((surface (SDL_CreateRGBSurfaceFrom
                  pixels width height depth pitch
                  rmask gmask bmask amask)))
    (if (and (surface? surface)
             (not (struct-null? surface)))
        surface
        (abort (sdl-failure "SDL_CreateRGBSurfaceFrom" #f)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERT SURFACE / PIXELS

(define (convert-surface surface format)
  (%autofree-struct!
   (convert-surface* surface format)
   free-surface!))


(define (convert-surface* surface format)
  (let ((surface (SDL_ConvertSurface surface format 0)))
    (if (and (surface? surface)
             (not (struct-null? surface)))
        surface
        (abort (sdl-failure "SDL_ConvertSurface" #f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD / SAVE BMP

;;; Loads a BMP image file from the given file path (a string), and
;;; returns a new surface containing the image. The surface will
;;; automatically be freed by the garbage collector. Returns #f if the
;;; image could not be loaded (use get-error to find out why).
(define (load-bmp filepath)
  (let ((surface (load-bmp* filepath)))
    (when surface
      (set-finalizer! surface free-surface!))
    surface))

;;; Like load-bmp, except the surface will NOT automatically be
;;; freed by the garbage collector. You must manually free the surface
;;; (e.g. using free-surface!) when you are done with it.
(define (load-bmp* filepath)
  (let ((surface (SDL_LoadBMP filepath)))
    (if (and (surface? surface)
             (not (struct-null? surface)))
        surface
        (abort (sdl-failure "SDL_LoadBMP" #f)))))


;;; Loads a BMP image file from the given sdl2:rwops, and returns a new
;;; surface containing the image. If close? is #t, rwops will be
;;; closed after reading. The surface will automatically be freed by
;;; the garbage collector. Returns #f if the image could not be loaded
;;; (use get-error to find out why).
(define (load-bmp-rw rwops #!optional close?)
  (let ((surface (load-bmp-rw* rwops close?)))
    (when surface
      (set-finalizer! surface free-surface!))
    surface))

;;; Like load-bmp-rw, except the surface will NOT automatically be
;;; freed by the garbage collector. You must manually free the surface
;;; (e.g. using free-surface!) when you are done with it.
(define (load-bmp-rw* rwops #!optional close?)
  (let ((surface (SDL_LoadBMP_RW rwops #f)))
    (when close?
      ;; Properly close and nullify the sdl2:rwops.
      (rw-close! rwops))
    (if (and (surface? surface)
             (not (struct-null? surface)))
        surface
        (abort (sdl-failure "SDL_LoadBMP_RW" #f)))))


(define (save-bmp! surface filepath)
  (let ((ret-code (SDL_SaveBMP surface filepath)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SaveBMP" ret-code)))))

(define (save-bmp-rw! surface rwops #!optional close?)
  (let ((ret-code (SDL_SaveBMP_RW surface rwops #f)))
    (when close?
      ;; Properly close and nullify the sdl2:rwops.
      (rw-close! rwops))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SaveBMP_RW" ret-code)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCK / UNLOCK

(define (lock-surface! surface)
  (let ((ret-code (SDL_LockSurface surface)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_LockSurface" ret-code)))))

(define (unlock-surface! surface)
  (SDL_UnlockSurface surface))

(define (must-lock? surface)
  (SDL_MUSTLOCK surface))

;;; Locks the surface (if needed), performs body, unlocks the surface
;;; (if needed), and returns value of the last body expression.
(define-syntax with-locked-surface
  (syntax-rules ()
    ((with-locked-surface surface body ...)
     (dynamic-wind
         (lambda ()
           (when (must-lock? surface)
             (lock-surface! surface)))
         (lambda ()
           body ...)
         (lambda ()
           (when (must-lock? surface)
             (unlock-surface! surface)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLIT

(define (blit-surface! src src-rect dst dst-rect)
  (let ((ret-code (SDL_BlitSurface src src-rect dst dst-rect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_BlitSurface" ret-code)))))

(define (blit-scaled! src src-rect dst dst-rect)
  (let ((ret-code (SDL_BlitScaled src src-rect dst dst-rect)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_BlitScaled" ret-code)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILL RECT(S)

;;; Fill one area of the surface with a color. rect can be an
;;; sdl2:rect, or #f to fill the whole surface. color can be an
;;; sdl2:color or an integer (e.g. a mapped color from map-rgba, or
;;; a palette index).
(define (fill-rect! surface rect color)
  (let* ((color-int (%map-color-for-surface
                     color surface 'fill-rect!))
         (ret-code (SDL_FillRect surface rect color-int)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_FillRect" ret-code)))))


;;; Fill multiple areas of the surface with a color. rects must be a
;;; list of sdl2:rects. color can be an sdl2:color or an integer (e.g. a
;;; mapped color from map-rgba, or a palette index).
(: fill-rects!
   (sdl2:surface*
    (or (list-of sdl2:rect*) (vector-of sdl2:rect*))
    (or sdl2:color* fixnum)
    -> void))
(define (fill-rects! surface rects color)
  (let ((color-int (%map-color-for-surface
                    color surface 'fill-rects!)))
    (with-temp-mem ((rect-array (%rects->array rects)))
      (let ((ret-code (SDL_FillRects
                       surface rect-array
                       (if (vector? rects)
                           (vector-length rects)
                           (length rects))
                       color-int)))
        (unless (zero? ret-code)
          (free rect-array)
          (abort (sdl-failure "SDL_FillRects" ret-code)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET / SET PIXEL

(define (%assert-surface-bounds surface x y fn-name)
  (assert-bounds x 0 (sub1 (surface-w surface))
                 "x coordinate out of bounds" fn-name)
  (assert-bounds y 0 (sub1 (surface-h surface))
                 "y coordinate out of bounds" fn-name))


;;; Set the pixel at the given x/y coordinates to the given sdl2:color
;;; or raw pixel value (like is returned by map-rgba). Throws an
;;; error if x or y is out of bounds for the surface size.
(define (surface-set! surface x y color)
  (%assert-surface-bounds surface x y 'surface-set!)
  (let ((pixel (%map-color-for-surface color surface 'surface-set!)))
    (with-locked-surface surface
      (chickenSDL2_SurfaceSetPixel surface x y pixel))))

;;; Returns a sdl2:color instance for the pixel at the given x/y
;;; coordinates in the surface. Throws an error if x or y is out of
;;; bounds for the surface size.
(define (surface-ref surface x y)
  (%assert-surface-bounds surface x y 'surface-ref)
  (receive (r g b a) (get-rgba
                      (surface-ref-raw surface x y)
                      (surface-format surface))
           (make-color r g b a)))

(set! (setter surface-ref)
      surface-set!)

;;; Returns a raw pixel value (like is returned by map-rgba) for
;;; the pixel at the given x/y coordinates in the surface. Throws an
;;; error if x or y is out of bounds for the surface size.
(define (surface-ref-raw surface x y)
  (%assert-surface-bounds surface x y 'surface-ref-raw)
  (with-locked-surface surface
    (chickenSDL2_SurfaceGetPixel surface x y)))

(set! (setter surface-ref-raw)
      surface-set!)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SURFACE PROPERTIES

(define (surface-clip-rect-set! surface rect)
  (SDL_SetClipRect surface rect))

(define (surface-clip-rect surface)
  (let ((rect-out (alloc-rect)))
    (SDL_GetClipRect surface rect-out)
    rect-out))

(set! (setter surface-clip-rect)
      surface-clip-rect-set!)



;;; Set the surface's color key. color can be an sdl2:color, or an
;;; integer (e.g. a mapped color from map-rgba), or #f to disable
;;; the color key.
(define (surface-color-key-set! surface color)
  (let ((ret-code
         (if color
             (SDL_SetColorKey surface #t
                              (%map-color-for-surface
                               color surface 'surface-color-key-set!))
             (SDL_SetColorKey surface #f 0))))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetColorKey" ret-code)))))

;;; Returns the surface's color key as an sdl2:color, or #f if the
;;; surface has no color key. Signals exception if an error occurred.
(define (surface-color-key surface)
  (with-temp-mem ((mapped-color-out (%allocate-Uint32)))
    (let ((ret-code (SDL_GetColorKey surface mapped-color-out)))
      (cond ((not (negative? ret-code))
             (%unmap-color-for-surface
              (pointer-u32-ref mapped-color-out)
              surface))
            ((= -1 ret-code)
             #f)
            (else
             (abort (sdl-failure "SDL_GetColorKey" ret-code)))))))

(set! (setter surface-color-key)
      surface-color-key-set!)

;;; Returns the surface's color key as a non-negative integer (a
;;; mapped color), or #f if the surface has no color key. Signals
;;; exception if an error occurred.
(define (surface-color-key-raw surface)
  (with-temp-mem ((mapped-color-out (%allocate-Uint32)))
    (let ((ret-code (SDL_GetColorKey surface mapped-color-out)))
      (cond ((not (negative? ret-code))
             (pointer-u32-ref mapped-color-out))
            ((= -1 ret-code)
             #f)
            (else
             (abort (sdl-failure "SDL_GetColorKey" ret-code)))))))

(set! (setter surface-color-key-raw)
      surface-color-key-set!)

(define surface-colour-key-set!  surface-color-key-set!)
(define surface-colour-key       surface-color-key)
(define surface-colour-key-raw   surface-color-key-raw)



(define (surface-alpha-mod-set! surface alpha)
  (let ((ret-code (SDL_SetSurfaceAlphaMod surface alpha)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetSurfaceAlphaMod" ret-code)))))

;;; Returns the surface's alpha mod as an integer in range [0, 255],
;;; or a negative integer if an error occurred.
(define (surface-alpha-mod surface)
  (with-temp-mem ((alpha-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetSurfaceAlphaMod surface alpha-out)))
      (if (zero? ret-code)
          (pointer-u8-ref alpha-out)
          (begin
            (free alpha-out)
            (abort (sdl-failure "SDL_GetSurfaceAlphaMod" ret-code)))))))

(set! (setter surface-alpha-mod)
      surface-alpha-mod-set!)



;;; Returns the surface's blend mode as a symbol (none, blend, add, or
;;; mod), or a negative integer if an error occurred.
(define (surface-blend-mode surface)
  (with-temp-mem ((mode-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetSurfaceBlendMode surface mode-out)))
      (if (zero? ret-code)
          (blend-mode->symbol (pointer-u8-ref mode-out))
          (begin
            (free mode-out)
            (abort (sdl-failure "SDL_GetSurfaceBlendMode" ret-code)))))))


(define (surface-blend-mode-raw surface)
  (with-temp-mem ((mode-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetSurfaceBlendMode surface mode-out)))
      (if (zero? ret-code)
          (pointer-u8-ref mode-out)
          (begin
            (free mode-out)
            (abort (sdl-failure "SDL_GetSurfaceBlendMode" ret-code)))))))


(define (surface-blend-mode-set! surface blend-mode)
  (define (bad-mode-err x)
    (error 'surface-blend-mode-set!
           "invalid surface blend mode" x))
  (let* ((mode-int (cond ((integer? blend-mode)
                          blend-mode)
                         (else
                          (symbol->blend-mode
                           blend-mode bad-mode-err))))
         (ret-code (SDL_SetSurfaceBlendMode surface mode-int)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetSurfaceBlendMode" ret-code)))))

(set! (setter surface-blend-mode)
      surface-blend-mode-set!)

(set! (setter surface-blend-mode-raw)
      surface-blend-mode-set!)



;;; Set the surface color mod. Accepts either an RGB list of 3
;;; integers (0-255), or an sdl2:color (alpha will be ignored).
(define (surface-color-mod-set! surface rgb-or-color)
  (assert (or (list? rgb-or-color) (color? rgb-or-color)))
  (receive (r g b) (if (list? rgb-or-color)
                       (values (list-ref rgb-or-color 0)
                               (list-ref rgb-or-color 1)
                               (list-ref rgb-or-color 2))
                       (values (color-r rgb-or-color)
                               (color-g rgb-or-color)
                               (color-b rgb-or-color)))
    (let ((ret-code (SDL_SetSurfaceColorMod surface r g b)))
      (unless (zero? ret-code)
        (abort (sdl-failure "SDL_SetSurfaceColorMod" ret-code))))))

;;; Returns the surface's color mod as 3 integers in the range [0,
;;; 255]. Signals an exception if an error occurred.
(define (surface-color-mod surface)
  (with-temp-mem ((r-out (%allocate-Uint8))
                  (g-out (%allocate-Uint8))
                  (b-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetSurfaceColorMod
                     surface r-out g-out b-out)))
      (if (zero? ret-code)
          (values (pointer-u8-ref r-out)
                  (pointer-u8-ref g-out)
                  (pointer-u8-ref b-out))
          (begin
            (free r-out)
            (free g-out)
            (free b-out)
            (abort (sdl-failure "SDL_GetSurfaceColorMod" ret-code)))))))

(set! (setter surface-color-mod)
      surface-color-mod-set!)

(define surface-colour-mod-set! surface-color-mod-set!)
(define surface-colour-mod      surface-color-mod)



(define (surface-palette-set! surface palette)
  (let ((ret-code (SDL_SetSurfacePalette surface palette)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetSurfacePalette" ret-code)))))

(define (surface-palette surface)
  (pixel-format-palette (surface-format surface)))

(set! (setter surface-palette)
      surface-palette-set!)



(define (surface-rle-set! surface rle?)
  (let ((ret-code (SDL_SetSurfaceRLE surface rle?)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetSurfaceRLE" ret-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIMPLE TRANSFORMATIONS

;;; Return a copy of the given surface rotated by the given number of
;;; 90 degree clockwise turns. If the given surface has a palette, the
;;; new surface will share the same palette.
(: rotate-surface-90
   (sdl2:surface* fixnum -> sdl2:surface))
(define (rotate-surface-90 surface turns)
  (set-finalizer! (rotate-surface-90* surface turns) free-surface!))

(: rotate-surface-90*
   (sdl2:surface* fixnum -> sdl2:surface))
(define (rotate-surface-90* surface turns)
  (assert (integer? turns))
  (let ((result (chickenSDL2_RotateSurface90 surface turns)))
    (if (and (surface? result) (not (struct-null? result)))
        result
        (abort (sdl-failure "chickenSDL2_RotateSurface90" #f)))))


;;; Return a copy of the given surface flipped on the X and/or Y axes.
;;; If the given surface has a palette, the new surface will share the
;;; same palette.
(: flip-surface
   (sdl2:surface* boolean boolean -> sdl2:surface))
(define (flip-surface surface flip-x? flip-y?)
  (set-finalizer! (flip-surface* surface flip-x? flip-y?) free-surface!))

(: flip-surface*
   (sdl2:surface* boolean boolean -> sdl2:surface))
(define (flip-surface* surface flip-x? flip-y?)
  (let ((result (chickenSDL2_FlipSurface surface flip-x? flip-y?)))
    (if (and (surface? result) (not (struct-null? result)))
        result
        (abort (sdl-failure "chickenSDL2_FlipSurface" #f)))))
