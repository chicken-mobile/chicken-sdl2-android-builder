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


(export SDL_CreateWindow
        SDL_GetWindowFromID
        SDL_DestroyWindow

        SDL_UpdateWindowSurface
        SDL_UpdateWindowSurfaceRects

        SDL_ShowWindow
        SDL_HideWindow
        SDL_MaximizeWindow
        SDL_MinimizeWindow
        SDL_RaiseWindow
        SDL_RestoreWindow

        SDL_GetWindowBrightness
        SDL_GetWindowData
        SDL_GetWindowDisplayIndex
        SDL_GetWindowDisplayMode
        SDL_GetWindowFlags
        SDL_GetWindowGammaRamp
        SDL_GetWindowGrab
        SDL_GetWindowID
        SDL_GetWindowMaximumSize
        SDL_GetWindowMinimumSize
        SDL_GetWindowPixelFormat
        SDL_GetWindowPosition
        SDL_GetWindowSize
        SDL_GetWindowSurface
        SDL_GetWindowTitle

        SDL_SetWindowBordered
        SDL_SetWindowBrightness
        SDL_SetWindowData
        SDL_SetWindowDisplayMode
        SDL_SetWindowFullscreen
        SDL_SetWindowGammaRamp
        SDL_SetWindowGrab
        SDL_SetWindowIcon
        SDL_SetWindowMaximumSize
        SDL_SetWindowMinimumSize
        SDL_SetWindowPosition
        SDL_SetWindowSize
        SDL_SetWindowTitle

        SDL_WINDOWPOS_UNDEFINED_DISPLAY
        SDL_WINDOWPOS_ISUNDEFINED
        SDL_WINDOWPOS_CENTERED_DISPLAY
        SDL_WINDOWPOS_ISCENTERED)

#+libSDL-2.0.4+
(export SDL_GetGrabbedWindow)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CREATE / DESTROY WINDOW

(define-function-binding SDL_CreateWindow
  return: (SDL_Window* new-window)
  args: ((c-string title)
         (Sint32 x) (Sint32 y)
         (Sint32 w) (Sint32 h)
         (Uint32 flags)))

;;; TODO?: SDL_CreateWindowAndRenderer
;;; TODO?: SDL_CreateWindowFrom

(define-function-binding SDL_GetWindowFromID
  return: (SDL_Window* window-or-null)
  args: ((Uint32 id)))

(define-function-binding SDL_DestroyWindow
  args: ((SDL_Window* window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE WINDOW SURFACE

(define-function-binding SDL_UpdateWindowSurface
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)))

(define-function-binding SDL_UpdateWindowSurfaceRects
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (c-pointer rects)
         (Sint32 numrects)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WINDOW MANAGEMENT

(define-function-binding SDL_ShowWindow
  args: ((SDL_Window* window)))

(define-function-binding SDL_HideWindow
  args: ((SDL_Window* window)))

(define-function-binding SDL_MaximizeWindow
  args: ((SDL_Window* window)))

(define-function-binding SDL_MinimizeWindow
  args: ((SDL_Window* window)))

(define-function-binding SDL_RaiseWindow
  args: ((SDL_Window* window)))

(define-function-binding SDL_RestoreWindow
  args: ((SDL_Window* window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET WINDOW PROPERTIES

(define-function-binding SDL_GetWindowBrightness
  return: (float brightness)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowData
  return: (c-pointer data-value)
  args: ((SDL_Window* window)
         (c-string data-key)))

(define-function-binding SDL_GetWindowDisplayIndex
  return: (Sint32 display-index)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowDisplayMode
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (SDL_DisplayMode* mode-out)))

(define-function-binding SDL_GetWindowFlags
  return: (Uint32 flags)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowGammaRamp
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (Uint16* red-array-out)
         (Uint16* green-array-out)
         (Uint16* blue-array-out)))

(define-function-binding SDL_GetWindowGrab
  return: (bool grabbed?)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowID
  return: (Uint32 id)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowMaximumSize
  args: ((SDL_Window* window)
         (Sint32* w-out)
         (Sint32* h-out)))

(define-function-binding SDL_GetWindowMinimumSize
  args: ((SDL_Window* window)
         (Sint32* w-out)
         (Sint32* h-out)))

(define-function-binding SDL_GetWindowPixelFormat
  return: (Uint32 format)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowPosition
  args: ((SDL_Window* window)
         (Sint32* x-out)
         (Sint32* y-out)))

(define-function-binding SDL_GetWindowSize
  args: ((SDL_Window* window)
         (Sint32* w-out)
         (Sint32* h-out)))

(define-function-binding SDL_GetWindowSurface
  return: (SDL_Surface* surface-or-null)
  args: ((SDL_Window* window)))

(define-function-binding SDL_GetWindowTitle
  return: (c-string title)
  args: ((SDL_Window* window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SET WINDOW PROPERTIES

(define-function-binding SDL_SetWindowBordered
  args: ((SDL_Window* window)
         (bool bordered?)))

(define-function-binding SDL_SetWindowBrightness
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (float brightness)))

(define-function-binding SDL_SetWindowData
  return: (c-pointer previous-data-value)
  args: ((SDL_Window* window)
         (c-string data-key)
         (c-pointer data-value)))

(define-function-binding SDL_SetWindowDisplayMode
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (SDL_DisplayMode* mode)))

(define-function-binding SDL_SetWindowFullscreen
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (Uint32 fullscreen-flags)))

(define-function-binding SDL_SetWindowGammaRamp
  return: (Sint32 zero-if-success)
  args: ((SDL_Window* window)
         (Uint16* red-array)
         (Uint16* green-array)
         (Uint16* blue-array)))

(define-function-binding SDL_SetWindowGrab
  args: ((SDL_Window* window)
         (bool grabbed?)))

(define-function-binding SDL_SetWindowIcon
  args: ((SDL_Window* window)
         (SDL_Surface* surface)))

(define-function-binding SDL_SetWindowMaximumSize
  args: ((SDL_Window* window)
         (Sint32 max_w)
         (Sint32 max_h)))

(define-function-binding SDL_SetWindowMinimumSize
  args: ((SDL_Window* window)
         (Sint32 min_w)
         (Sint32 min_h)))

(define-function-binding SDL_SetWindowPosition
  args: ((SDL_Window* window)
         (Sint32 x)
         (Sint32 y)))

(define-function-binding SDL_SetWindowSize
  args: ((SDL_Window* window)
         (Sint32 w)
         (Sint32 h)))

(define-function-binding SDL_SetWindowTitle
  args: ((SDL_Window* window)
         (c-string title)))

#+libSDL-2.0.4+
(define-function-binding SDL_GetGrabbedWindow
  return: (SDL_Window* window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WINDOW POSITION C-MACROS

(define-function-binding* SDL_WINDOWPOS_UNDEFINED_DISPLAY
  return: (Uint32 value)
  args: ((Uint32 displayNum))
  body: "C_return(SDL_WINDOWPOS_UNDEFINED_DISPLAY(displayNum));")

(define-function-binding* SDL_WINDOWPOS_ISUNDEFINED
  return: (bool is-undefined?)
  args: ((Uint32 value))
  body: "C_return(SDL_WINDOWPOS_ISUNDEFINED(value));")

(define-function-binding* SDL_WINDOWPOS_CENTERED_DISPLAY
  return: (Uint32 value)
  args: ((Uint32 displayNum))
  body: "C_return(SDL_WINDOWPOS_CENTERED_DISPLAY(displayNum));")

(define-function-binding* SDL_WINDOWPOS_ISCENTERED
  return: (bool is-undefined?)
  args: ((Uint32 value))
  body: "C_return(SDL_WINDOWPOS_ISCENTERED(value));")
