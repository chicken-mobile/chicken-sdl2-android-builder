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


(export SDL_CreateRenderer
        SDL_CreateSoftwareRenderer
        SDL_CreateWindowAndRenderer
        SDL_DestroyRenderer
        SDL_GetRenderer
        SDL_GetRendererInfo
        SDL_GetRendererOutputSize
        SDL_GetNumRenderDrivers
        SDL_GetRenderDriverInfo
        SDL_RenderPresent
        SDL_RenderGetClipRect
        SDL_RenderSetClipRect
        SDL_RenderGetLogicalSize
        SDL_RenderSetLogicalSize
        SDL_RenderGetScale
        SDL_RenderSetScale
        SDL_RenderGetViewport
        SDL_RenderSetViewport
        SDL_RenderCopy
        SDL_RenderCopyEx
        SDL_RenderTargetSupported
        SDL_GetRenderTarget
        SDL_SetRenderTarget
        SDL_RenderReadPixels)


#+libSDL-2.0.4+
(begin
  (export SDL_RenderIsClipEnabled)
  (define-function-binding SDL_RenderIsClipEnabled
    return: (bool enabled?)
    args: ((SDL_Renderer* renderer))))


(define-function-binding SDL_CreateRenderer
  return: (SDL_Renderer* renderer)
  args: ((SDL_Window* window)
         (Sint32      index)
         (Uint32      flags)))


(define-function-binding SDL_CreateSoftwareRenderer
  return: (SDL_Renderer* renderer)
  args: ((SDL_Surface* surfate)))


;; This is a bit tricky because SDL_CreateWindowAndRenderer uses
;; pointer-pointers (SDL_Window** and SDL_Renderer**).
(define-function-binding* SDL_CreateWindowAndRenderer
  return: (Sint32 zero-on-success)
  args: ((Sint32 width)
         (Sint32 height)
         (Uint32 window_flags)
         (pointer-vector window_ptr_out)
         (pointer-vector renderer_ptr_out))
  body: "C_return(
           SDL_CreateWindowAndRenderer(
             width, height, window_flags,
             (SDL_Window**)window_ptr_out,
             (SDL_Renderer**)renderer_ptr_out));")


(define-function-binding SDL_DestroyRenderer
  args: ((SDL_Renderer* renderer)))


(define-function-binding SDL_GetRenderer
  return: (SDL_Renderer* renderer)
  args: ((SDL_Window* window)))


(define-function-binding SDL_GetRendererInfo
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_RendererInfo* info-out)))


(define-function-binding SDL_GetRendererOutputSize
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (Sint32* w-out)
         (Sint32* h-out)))


(define-function-binding SDL_GetNumRenderDrivers
  return: (Sint32 num-drivers))


(define-function-binding SDL_GetRenderDriverInfo
  return: (Sint32 zero-on-success)
  args: ((Sint32 index)
         (SDL_RendererInfo* info)))


(define-function-binding SDL_RenderPresent
  args: ((SDL_Renderer* renderer)))


(define-function-binding SDL_RenderGetClipRect
  args: ((SDL_Renderer* renderer)
         (SDL_Rect* rect-out)))

(define-function-binding SDL_RenderSetClipRect
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Rect*-or-null rect)))


(define-function-binding SDL_RenderGetLogicalSize
  args: ((SDL_Renderer* renderer)
         (Sint32* w-out)
         (Sint32* h-out)))

(define-function-binding SDL_RenderSetLogicalSize
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (Sint32 w)
         (Sint32 h)))


(define-function-binding SDL_RenderGetScale
  args: ((SDL_Renderer* renderer)
         (float* scale-x-out)
         (float* scale-y-out)))

(define-function-binding SDL_RenderSetScale
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (float scale-x)
         (float scale-y)))


(define-function-binding SDL_RenderGetViewport
  args: ((SDL_Renderer* renderer)
         (SDL_Rect* rect-out)))

(define-function-binding SDL_RenderSetViewport
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Rect*-or-null rect)))


(define-function-binding SDL_RenderCopy
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer*     renderer)
         (SDL_Texture*      texture)
         (SDL_Rect*-or-null srcrect)
         (SDL_Rect*-or-null dstrect)))

(define-function-binding SDL_RenderCopyEx
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer*      renderer)
         (SDL_Texture*       texture)
         (SDL_Rect*-or-null  srcrect)
         (SDL_Rect*-or-null  dstrect)
         (double             angle)
         (SDL_Point*-or-null center)
         (SDL_RendererFlip   flip)))


(define-function-binding SDL_RenderTargetSupported
  return: (bool supported?)
  args: ((SDL_Renderer* renderer)))

(define-function-binding SDL_GetRenderTarget
  return: (SDL_Texture* texture)
  args: ((SDL_Renderer* renderer)))

(define-function-binding SDL_SetRenderTarget
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer*        renderer)
         (SDL_Texture*-or-null texture)))


(define-function-binding SDL_RenderReadPixels
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer*     renderer)
         (SDL_Rect*-or-null rect)
         (Uint32            format)
         (c-pointer         pixels-out)
         (Sint32            pitch)))
