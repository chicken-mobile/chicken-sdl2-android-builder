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


(export SDL_GetPixelFormatName
        SDL_PixelFormatEnumToMasks
        SDL_MasksToPixelFormatEnum

        SDL_AllocFormat
        SDL_FreeFormat
        SDL_AllocPalette
        SDL_FreePalette

        SDL_SetPixelFormatPalette
        SDL_SetPaletteColors

        SDL_MapRGB
        SDL_MapRGBA
        SDL_GetRGB
        SDL_GetRGBA

        SDL_CalculateGammaRamp)


(define-function-binding SDL_GetPixelFormatName
  return: (c-string name)
  args: ((SDL_PixelFormatEnum format)))

(define-function-binding SDL_PixelFormatEnumToMasks
  return: (bool success?)
  args: ((SDL_PixelFormatEnum format)
         (Sint32* bpp)
         (Uint32* rmask-out)
         (Uint32* gmask-out)
         (Uint32* bmask-out)
         (Uint32* amask-out)))

(define-function-binding SDL_MasksToPixelFormatEnum
  return: (SDL_PixelFormatEnum format)
  args: ((Sint32 bpp)
         (Uint32 rmask)
         (Uint32 gmask)
         (Uint32 bmask)
         (Uint32 amask)))


(define-function-binding SDL_AllocFormat
  return: (SDL_PixelFormat* format)
  args: ((SDL_PixelFormatEnum pixel_format)))

(define-function-binding SDL_FreeFormat
  args: ((SDL_PixelFormat* format)))

(define-function-binding SDL_AllocPalette
  return: (SDL_Palette* palette)
  args: ((Sint32 ncolors)))

(define-function-binding SDL_FreePalette
  args: ((SDL_Palette* palette)))


(define-function-binding SDL_SetPixelFormatPalette
  return: (Sint32 zero-if-success)
  args: ((SDL_PixelFormat* format)
         (SDL_Palette* palette)))

(define-function-binding SDL_SetPaletteColors
  return: (Sint32 zero-if-success)
  args: ((SDL_Palette* palette)
         (SDL_Color* colors)
         (Sint32 firstcolor)
         (Sint32 ncolors)))


(define-function-binding SDL_MapRGB
  return: (Uint32 color)
  args: ((SDL_PixelFormat* format)
         (Uint8 r)
         (Uint8 g)
         (Uint8 b)))

(define-function-binding SDL_MapRGBA
  return: (Uint32 color)
  args: ((SDL_PixelFormat* format)
         (Uint8 r)
         (Uint8 g)
         (Uint8 b)
         (Uint8 a)))

(define-function-binding SDL_GetRGB
  args: ((Uint32 pixel)
         (SDL_PixelFormat* format)
         (Uint8* r-out)
         (Uint8* g-out)
         (Uint8* b-out)))

(define-function-binding SDL_GetRGBA
  args: ((Uint32 pixel)
         (SDL_PixelFormat* format)
         (Uint8* r-out)
         (Uint8* g-out)
         (Uint8* b-out)
         (Uint8* a-out)))


(define-function-binding SDL_CalculateGammaRamp
  args: ((float gamma)
         (Uint16* ramp-out)))
