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


(export create-texture
        create-texture*
        create-texture-from-surface
        create-texture-from-surface*
        destroy-texture!

        query-texture
        query-texture-raw
        texture-format
        texture-access
        texture-w
        texture-h

        lock-texture-raw!
        unlock-texture!
        update-texture-raw!
        update-yuv-texture-raw! ;; SDL >= 2.0.1

        texture-alpha-mod   texture-alpha-mod-set!
        texture-blend-mode  texture-blend-mode-set!
        texture-color-mod   texture-color-mod-set!
        texture-colour-mod  texture-colour-mod-set!
        )


(: create-texture
   (sdl2:renderer* enum enum fixnum fixnum
    -> sdl2:texture))
(define (create-texture renderer format access w h)
  (set-finalizer! (create-texture* renderer format access w h)
                  destroy-texture!))

(: create-texture*
   (sdl2:renderer* enum enum fixnum fixnum
    -> sdl2:texture))
(define (create-texture* renderer format access w h)
  (let* ((format-int (if (integer? format)
                         format
                         (symbol->pixel-format-enum format)))
         (access-int (if (integer? access)
                         access
                         (symbol->texture-access access)))
         (texture (SDL_CreateTexture
                   renderer format-int access-int w h)))
    (if (and (texture? texture) (not (struct-null? texture)))
        texture
        (abort (sdl-failure "SDL_CreateTexture" #f)))))


(: create-texture-from-surface
   (sdl2:renderer* sdl2:surface* -> sdl2:texture))
(define (create-texture-from-surface renderer surface)
  (set-finalizer! (create-texture-from-surface* renderer surface)
                  destroy-texture!))

(: create-texture-from-surface*
   (sdl2:renderer* sdl2:surface* -> sdl2:texture))
(define (create-texture-from-surface* renderer surface)
  (let ((texture (SDL_CreateTextureFromSurface renderer surface)))
    (if (and (texture? texture) (not (struct-null? texture)))
        texture
        (abort (sdl-failure "SDL_CreateTextureFromSurface" #f)))))


(: destroy-texture!
   (sdl2:texture* -> void))
(define (destroy-texture! texture)
  (unless (struct-null? texture)
    (SDL_DestroyTexture texture)))



(: query-texture
   (sdl2:texture* -> symbol symbol fixnum fixnum))
(define (query-texture texture)
  (receive (format-int access-int w h) (query-texture-raw texture)
    (values (pixel-format-enum->symbol format-int)
            (texture-access->symbol access-int)
            w
            h)))

(: query-texture-raw
   (sdl2:texture* -> fixnum fixnum fixnum fixnum))
(define (query-texture-raw texture)
  (with-temp-mem ((format-out (%allocate-Uint32))
                  (access-out (%allocate-Sint32))
                  (w-out      (%allocate-Sint32))
                  (h-out      (%allocate-Sint32)))
    (let ((ret-code (SDL_QueryTexture
                     texture format-out access-out w-out h-out)))
      (if (zero? ret-code)
          (values (pointer-u32-ref format-out)
                  (pointer-s32-ref access-out)
                  (pointer-s32-ref w-out)
                  (pointer-s32-ref h-out))
          (begin
            (free format-out)
            (free access-out)
            (free w-out)
            (free h-out)
            (abort (sdl-failure "SDL_QueryTexture" ret-code)))))))


(: texture-format
   (sdl2:texture* -> symbol))
(define (texture-format texture)
  (with-temp-mem ((format-out (%allocate-Sint32)))
    (let ((ret-code (SDL_QueryTexture texture format-out #f #f #f)))
      (if (zero? ret-code)
          (pixel-format-enum->symbol (pointer-s32-ref format-out))
          (begin
            (free format-out)
            (abort (sdl-failure "SDL_QueryTexture" ret-code)))))))

(: texture-access
   (sdl2:texture* -> symbol))
(define (texture-access texture)
  (with-temp-mem ((access-out (%allocate-Sint32)))
    (let ((ret-code (SDL_QueryTexture texture #f access-out #f #f)))
      (if (zero? ret-code)
          (texture-access->symbol (pointer-s32-ref access-out))
          (begin
            (free access-out)
            (abort (sdl-failure "SDL_QueryTexture" ret-code)))))))

(: texture-w
   (sdl2:texture* -> fixnum))
(define (texture-w texture)
  (with-temp-mem ((w-out (%allocate-Sint32)))
    (let ((ret-code (SDL_QueryTexture texture #f #f w-out #f)))
      (if (zero? ret-code)
          (pointer-s32-ref w-out)
          (begin
            (free w-out)
            (abort (sdl-failure "SDL_QueryTexture" ret-code)))))))

(: texture-h
   (sdl2:texture* -> fixnum))
(define (texture-h texture)
  (with-temp-mem ((h-out (%allocate-Sint32)))
    (let ((ret-code (SDL_QueryTexture texture #f #f #f h-out)))
      (if (zero? ret-code)
          (pointer-s32-ref h-out)
          (begin
            (free h-out)
            (abort (sdl-failure "SDL_QueryTexture" ret-code)))))))



(: lock-texture-raw!
   (sdl2:texture* (or sdl2:rect* boolean) -> pointer fixnum))
(define (lock-texture-raw! texture rect)
  (with-temp-mem ((pitch-out (%allocate-Sint32)))
    (let* ((pixels** (make-pointer-vector 1))
           (ret-code (SDL_LockTexture
                      texture rect pixels** pitch-out)))
      (if (zero? ret-code)
          (values (pointer-vector-ref pixels** 0)
                  (pointer-s32-ref pitch-out))
          (begin
            (free pitch-out)
            (abort (sdl-failure "SDL_LockTexture" #f)))))))


(: unlock-texture!
   (sdl2:texture* -> void))
(define (unlock-texture! texture)
  (SDL_UnlockTexture texture))



(: update-texture-raw!
   (sdl2:texture* (or sdl2:rect* boolean) (or pointer locative) fixnum
    -> void))
(define (update-texture-raw! texture rect pixels pitch)
  (let ((ret-code (SDL_UpdateTexture texture rect pixels pitch)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_UpdateTexture" ret-code)))))


(: update-yuv-texture-raw!
   (sdl2:texture*
    (or sdl2:rect* boolean)
    (or pointer locative) fixnum
    (or pointer locative) fixnum
    (or pointer locative) fixnum
    -> void))
(define-versioned (update-yuv-texture-raw!
                   texture rect y-plane y-pitch
                   u-plane u-pitch v-plane v-pitch)
    libSDL-2.0.1+
  (let ((ret-code (SDL_UpdateYUVTexture
                   texture rect y-plane y-pitch
                   u-plane u-pitch v-plane v-pitch)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_UpdateYUVTexture" ret-code)))))



(: texture-alpha-mod
   (sdl2:texture* -> fixnum))
(define (texture-alpha-mod texture)
  (with-temp-mem ((alpha-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetTextureAlphaMod texture alpha-out)))
      (if (zero? ret-code)
          (pointer-u8-ref alpha-out)
          (begin
            (free alpha-out)
            (abort (sdl-failure "SDL_GetTextureAlphaMod" ret-code)))))))

(: texture-alpha-mod-set!
   (sdl2:texture* fixnum -> void))
(define (texture-alpha-mod-set! texture alpha)
  (let ((ret-code (SDL_SetTextureAlphaMod texture alpha)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetTextureAlphaMod" ret-code)))))

(set! (setter texture-alpha-mod) texture-alpha-mod-set!)



(: texture-blend-mode
   (sdl2:texture* -> symbol))
(define (texture-blend-mode texture)
  (with-temp-mem ((mode-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetTextureBlendMode texture mode-out)))
      (if (zero? ret-code)
          (blend-mode->symbol (pointer-u8-ref mode-out))
          (begin
            (free mode-out)
            (abort (sdl-failure "SDL_GetTextureBlendMode" ret-code)))))))


(: texture-blend-mode-raw
   (sdl2:texture* -> fixnum))
(define (texture-blend-mode-raw texture)
  (with-temp-mem ((mode-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetTextureBlendMode texture mode-out)))
      (if (zero? ret-code)
          (pointer-u8-ref mode-out)
          (begin
            (free mode-out)
            (abort (sdl-failure "SDL_GetTextureBlendMode" ret-code)))))))


(: texture-blend-mode-set!
   (sdl2:texture* enum -> void))
(define (texture-blend-mode-set! texture blend-mode)
  (define (bad-mode-err x)
    (error 'texture-blend-mode-set!
           "Invalid texture blend mode" x))
  (let* ((mode-int (cond ((integer? blend-mode)
                          blend-mode)
                         (else
                          (symbol->blend-mode
                           blend-mode bad-mode-err))))
         (ret-code (SDL_SetTextureBlendMode texture mode-int)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetTextureBlendMode" ret-code)))))

(set! (setter texture-blend-mode) texture-blend-mode-set!)
(set! (setter texture-blend-mode-raw) texture-blend-mode-set!)



(: texture-color-mod
   (sdl2:texture* -> fixnum fixnum fixnum))
(define (texture-color-mod texture)
  (with-temp-mem ((r-out (%allocate-Uint8))
                  (g-out (%allocate-Uint8))
                  (b-out (%allocate-Uint8)))
    (let ((ret-code (SDL_GetTextureColorMod
                     texture r-out g-out b-out)))
      (if (zero? ret-code)
          (values (pointer-u8-ref r-out)
                  (pointer-u8-ref g-out)
                  (pointer-u8-ref b-out))
          (begin
            (free r-out)
            (free g-out)
            (free b-out)
            (abort (sdl-failure "SDL_GetTextureColorMod" ret-code)))))))

(: texture-color-mod-set!
   (sdl2:texture* (or (list-of fixnum) sdl2:color) -> void))
(define (texture-color-mod-set! texture rgb-or-color)
  (assert (or (list? rgb-or-color) (color? rgb-or-color)))
  (receive (r g b) (if (list? rgb-or-color)
                       (values (list-ref rgb-or-color 0)
                               (list-ref rgb-or-color 1)
                               (list-ref rgb-or-color 2))
                       (values (color-r rgb-or-color)
                               (color-g rgb-or-color)
                               (color-b rgb-or-color)))
    (let ((ret-code (SDL_SetTextureColorMod texture r g b)))
      (unless (zero? ret-code)
        (abort (sdl-failure "SDL_SetTextureColorMod" ret-code))))))

(set! (setter texture-color-mod) texture-color-mod-set!)

(define texture-colour-mod-set! texture-color-mod-set!)
(define texture-colour-mod      texture-color-mod)
