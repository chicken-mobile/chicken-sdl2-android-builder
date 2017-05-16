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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VIDEO / DISPLAY MODE

(export SDL_VideoInit
        SDL_VideoQuit
        SDL_DisableScreenSaver
        SDL_EnableScreenSaver
        SDL_IsScreenSaverEnabled
        SDL_GetClosestDisplayMode
        SDL_GetCurrentDisplayMode
        SDL_GetCurrentVideoDriver
        SDL_GetDesktopDisplayMode
        SDL_GetDisplayBounds
        SDL_GetDisplayMode
        SDL_GetNumDisplayModes
        SDL_GetNumVideoDisplays
        SDL_GetNumVideoDrivers
        SDL_GetVideoDriver)


(define-function-binding SDL_VideoInit
  args: ((c-string driver-name)))

(define-function-binding SDL_VideoQuit)

(define-function-binding SDL_DisableScreenSaver)

(define-function-binding SDL_EnableScreenSaver)

(define-function-binding SDL_IsScreenSaverEnabled
  return: (bool enabled?))

(define-function-binding SDL_GetClosestDisplayMode
  return: (SDL_DisplayMode* closest)
  args: ((Sint32 display-index)
         (SDL_DisplayMode* mode)
         (SDL_DisplayMode* closest-out)))

(define-function-binding SDL_GetCurrentDisplayMode
  return: (Sint32 zero-if-success)
  args: ((Sint32 display-index) (SDL_DisplayMode* mode-out)))

(define-function-binding SDL_GetCurrentVideoDriver
  return: (c-string driver-name))

(define-function-binding SDL_GetDesktopDisplayMode
  return: (Sint32 zero-if-success)
  args: ((Sint32 display-index) (SDL_DisplayMode* mode-out)))

(define-function-binding SDL_GetDisplayBounds
  return: (Sint32 zero-if-success)
  args: ((Sint32 display-index) (SDL_Rect* rect-out)))

(define-function-binding SDL_GetDisplayMode
  return: (Sint32 zero-if-success)
  args: ((Sint32 display-index)
         (Sint32 mode-index)
         (SDL_DisplayMode* mode-out)))

(define-function-binding SDL_GetNumDisplayModes
  return: (Sint32 num-modes)
  args: ((Sint32 display-index)))

(define-function-binding SDL_GetNumVideoDisplays
  return: (Sint32 num-displays))

(define-function-binding SDL_GetNumVideoDrivers
  return: (Sint32 num-drivers))

(define-function-binding SDL_GetVideoDriver
  return: (c-string driver-name)
  args: ((Sint32 index)))
