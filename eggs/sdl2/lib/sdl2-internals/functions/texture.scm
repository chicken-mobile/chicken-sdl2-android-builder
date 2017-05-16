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


(export SDL_CreateTexture
        SDL_CreateTextureFromSurface
        SDL_DestroyTexture
        SDL_QueryTexture
        SDL_LockTexture
        SDL_UnlockTexture
        SDL_UpdateTexture
        SDL_GetTextureAlphaMod   SDL_SetTextureAlphaMod
        SDL_GetTextureBlendMode  SDL_SetTextureBlendMode
        SDL_GetTextureColorMod   SDL_SetTextureColorMod
        )


#+libSDL-2.0.1+
(begin
  (export SDL_UpdateYUVTexture)
  (define-function-binding SDL_UpdateYUVTexture
    return: (Sint32 zero-on-success)
    args: ((SDL_Texture*      texture)
           (SDL_Rect*-or-null rect)
           (Uint8*            Yplane)
           (Sint32            Ypitch)
           (Uint8*            Uplane)
           (Sint32            Upitch)
           (Uint8*            Vplane)
           (Sint32            Vpitch))))


(define-function-binding SDL_CreateTexture
  return: (SDL_Texture* texture)
  args: ((SDL_Renderer*        renderer)
         (SDL_PixelFormatEnum  format)
         (SDL_TextureAccess    access)
         (Sint32               w)
         (Sint32               h)))

(define-function-binding SDL_CreateTextureFromSurface
  return: (SDL_Texture* texture)
  args: ((SDL_Renderer* renderer)
         (SDL_Surface*  surface)))

(define-function-binding SDL_DestroyTexture
  args: ((SDL_Texture* texture)))


(define-function-binding SDL_QueryTexture
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture* texture)
         (Uint32*      format-out)
         (Sint32*      access-out)
         (Sint32*      w-out)
         (Sint32*      h-out)))


(define-function-binding SDL_LockTexture
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture*      texture)
         (SDL_Rect*-or-null rect)
         (pointer-vector    pixels)
         (Sint32*           pitch)))

(define-function-binding SDL_UnlockTexture
  args: ((SDL_Texture* texture)))


(define-function-binding SDL_UpdateTexture
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture*      texture)
         (SDL_Rect*-or-null rect)
         (c-pointer         pixels)
         (Sint32            pitch)))


(define-function-binding SDL_GetTextureAlphaMod
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture* texture)
         (Uint8*       alpha-out)))

(define-function-binding SDL_SetTextureAlphaMod
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture* texture)
         (Uint8        alpha)))


(define-function-binding SDL_GetTextureBlendMode
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture*   texture)
         (SDL_BlendMode* mode-out)))

(define-function-binding SDL_SetTextureBlendMode
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture*  texture)
         (SDL_BlendMode mode)))


(define-function-binding SDL_GetTextureColorMod
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture* texture)
         (Uint8*       r-out)
         (Uint8*       g-out)
         (Uint8*       b-out)))

(define-function-binding SDL_SetTextureColorMod
  return: (Sint32 zero-on-success)
  args: ((SDL_Texture* texture)
         (Uint8        r)
         (Uint8        g)
         (Uint8        b)))
