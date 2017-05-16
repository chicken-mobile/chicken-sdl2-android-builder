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


(export SDL_GetRenderDrawBlendMode
        SDL_SetRenderDrawBlendMode
        SDL_GetRenderDrawColor
        SDL_SetRenderDrawColor
        SDL_RenderClear
        SDL_RenderDrawLine
        SDL_RenderDrawLines
        SDL_RenderDrawPoint
        SDL_RenderDrawPoints
        SDL_RenderDrawRect
        SDL_RenderDrawRects
        SDL_RenderFillRect
        SDL_RenderFillRects)


(define-function-binding SDL_GetRenderDrawBlendMode
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         ((c-pointer "SDL_BlendMode") blend-mode-out)))

(define-function-binding SDL_SetRenderDrawBlendMode
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_BlendMode blend-mode)))


(define-function-binding SDL_GetRenderDrawColor
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (Uint8* r-out)
         (Uint8* g-out)
         (Uint8* b-out)
         (Uint8* a-out)))

(define-function-binding SDL_SetRenderDrawColor
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (Uint8 r)
         (Uint8 g)
         (Uint8 b)
         (Uint8 a)))


(define-function-binding SDL_RenderClear
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)))


(define-function-binding SDL_RenderDrawLine
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (Sint32 x1)
         (Sint32 y1)
         (Sint32 x2)
         (Sint32 y2)))

(define-function-binding SDL_RenderDrawLines
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Point* points)
         (Sint32 count)))


(define-function-binding SDL_RenderDrawPoint
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (Sint32 x)
         (Sint32 y)))

(define-function-binding SDL_RenderDrawPoints
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Point* points)
         (Sint32 count)))


(define-function-binding SDL_RenderDrawRect
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Rect*-or-null rect)))

(define-function-binding SDL_RenderDrawRects
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Rect* rects)
         (Sint32 count)))


(define-function-binding SDL_RenderFillRect
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Rect*-or-null rect)))

(define-function-binding SDL_RenderFillRects
  return: (Sint32 zero-on-success)
  args: ((SDL_Renderer* renderer)
         (SDL_Rect* rects)
         (Sint32 count)))
