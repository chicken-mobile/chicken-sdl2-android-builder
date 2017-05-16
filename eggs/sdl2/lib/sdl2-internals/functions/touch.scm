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


(export SDL_GetNumTouchDevices
        SDL_GetNumTouchFingers
        SDL_GetTouchDevice
        SDL_GetTouchFinger

        SDL_RecordGesture
        SDL_SaveDollarTemplate
        SDL_SaveAllDollarTemplates
        SDL_LoadDollarTemplates)


(define-function-binding SDL_GetNumTouchDevices
  return: (Sint32 num-touch-devices))

(define-function-binding SDL_GetNumTouchFingers
  return: (Sint32 num-touch-devices)
  args: ((SDL_TouchID touch-id)))

(define-function-binding SDL_GetTouchDevice
  return: (SDL_TouchID device-id)
  args: ((Sint32 index)))

(define-function-binding SDL_GetTouchFinger
  return: (SDL_Finger* finger-or-null)
  args: ((SDL_TouchID touch-id)
         (Sint32 index)))


(define-function-binding SDL_RecordGesture
  return: (bool success?)
  args: ((SDL_TouchID touch-id)))

(define-function-binding SDL_SaveDollarTemplate
  return: (bool success?)
  args: ((SDL_GestureID gesture-id)
         (SDL_RWops* dst)))

(define-function-binding SDL_SaveAllDollarTemplates
  return: (Sint32 num-templates-saved)
  args: ((SDL_RWops* dst)))

(define-function-binding SDL_LoadDollarTemplates
  return: (Sint32 num-templates-loaded)
  args: ((SDL_TouchID touch-id)
         (SDL_RWops* src)))
