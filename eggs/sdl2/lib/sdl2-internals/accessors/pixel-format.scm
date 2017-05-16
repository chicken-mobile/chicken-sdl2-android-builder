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


(export pixel-format-format-raw
        pixel-format-format
        pixel-format-palette  pixel-format-palette-set!
        pixel-format-bits-per-pixel
        pixel-format-bytes-per-pixel
        pixel-format-rmask
        pixel-format-gmask
        pixel-format-bmask
        pixel-format-amask

        make-pixel-format
        make-pixel-format*)


(define-struct-field-accessors
  SDL_PixelFormat*
  pixel-format?
  ("format"
   type:   SDL_PixelFormatEnum
   getter: pixel-format-format-raw)
  ;; See below.
  ;; ("palette"
  ;;  type:   SDL_Palette*
  ;;  getter: pixel-format-palette)
  ("BitsPerPixel"
   type: Uint8
   getter: pixel-format-bits-per-pixel)
  ("BytesPerPixel"
   type: Uint8
   getter: pixel-format-bytes-per-pixel)
  ("Rmask"
   type: Uint32
   getter: pixel-format-rmask)
  ("Gmask"
   type: Uint32
   getter: pixel-format-gmask)
  ("Bmask"
   type: Uint32
   getter: pixel-format-bmask)
  ("Amask"
   type: Uint32
   getter: pixel-format-amask)
  ;; omitted: Rloss    (internal use)
  ;; omitted: Gloss    (internal use)
  ;; omitted: Bloss    (internal use)
  ;; omitted: Aloss    (internal use)
  ;; omitted: Rshift   (internal use)
  ;; omitted: Gshift   (internal use)
  ;; omitted: Bshift   (internal use)
  ;; omitted: Ashift   (internal use)
  ;; omitted: refcount (internal use)
  ;; omitted: next     (internal use)
  )


(define-enum-accessor
  getter: (pixel-format-format
           raw:   pixel-format-format-raw
           conv:  pixel-format-enum->symbol))


(define (pixel-format-palette-set! pixel-format palette)
  (let ((ret-code (SDL_SetPixelFormatPalette pixel-format palette)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetPixelFormatPalette" ret-code)))))

(define (pixel-format-palette pixel-format)
  (define getter (struct-field-getter
                  SDL_PixelFormat*
                  pixel-format?
                  SDL_Palette*
                  "palette"))
  (let ((palette (getter pixel-format)))
    (if (and (palette? palette)
             (not (struct-null? palette)))
        palette
        #f)))

(set! (setter pixel-format-palette)
      pixel-format-palette-set!)


(define (make-pixel-format #!optional (format 'unknown))
  (%autofree-struct!
   (make-pixel-format* format)
   free-pixel-format!))

(define (make-pixel-format* #!optional (format 'unknown))
  (define (err x)
    (error 'make-pixel-format* "invalid pixel format enum" x))
  (let* ((format-int (cond ((integer? format)
                            format)
                           (else
                            (symbol->pixel-format-enum
                             format err))))
         (pixel-format (SDL_AllocFormat format-int)))
    (if (struct-null? pixel-format)
        (abort (sdl-failure "SDL_AllocFormat" #f))
        pixel-format)))
