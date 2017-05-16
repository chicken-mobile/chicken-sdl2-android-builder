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


(export init!
        init-subsystem!
        quit!
        quit-subsystem!
        was-init
        set-main-ready!

        clear-error!
        get-error
        set-error!

        get-platform

        screen-saver-enabled?
        screen-saver-enabled-set!

        has-clipboard-text?
        get-clipboard-text
        set-clipboard-text!

        current-version
        compiled-version
        version-at-least?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INITIALIZATION AND SHUTDOWN

(: init!
   (#!optional (or (list-of symbol) fixnum) -> void))
(define (init! #!optional (flags '(everything)))
  (let ((ret-code (SDL_Init (pack-init-flags flags))))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_Init" ret-code)))))

(: init-subsystem!
   ((or (list-of symbol) fixnum) -> void))
(define (init-subsystem! flags)
  (let ((ret-code (SDL_InitSubSystem (pack-init-flags flags))))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_InitSubSystem" ret-code)))))

(define (quit!)
  (SDL_Quit))

(define (quit-subsystem! flags)
  (SDL_QuitSubSystem (pack-init-flags flags)))

(define (was-init #!optional (flags '(everything)))
  (unpack-init-flags
   (SDL_WasInit (pack-init-flags flags))
   #t))

(define (set-main-ready!)
  (SDL_SetMainReady))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERROR HANDLING

(define (clear-error!)
  (SDL_ClearError))

(define (get-error)
  (SDL_GetError))

(define (set-error! message)
  (SDL_SetError message))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PLATFORM

(define (get-platform)
  (SDL_GetPlatform))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCREEN SAVER

(: screen-saver-enabled?
   (-> boolean))
(define (screen-saver-enabled?)
  (SDL_IsScreenSaverEnabled))

(: screen-saver-enabled-set!
   (boolean -> void))
(define (screen-saver-enabled-set! enabled?)
  (case enabled?
    ((#t) (SDL_EnableScreenSaver))
    ((#f) (SDL_DisableScreenSaver))
    (else (error 'screen-saver-enabled-set!
                 "not a boolean" enabled?))))

(set! (setter screen-saver-enabled?)
      screen-saver-enabled-set!)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIP BOARD

(: has-clipboard-text?
   (-> boolean))
(define (has-clipboard-text?)
  (SDL_HasClipboardText))

(: get-clipboard-text
   (-> string))
(define (get-clipboard-text)
  (or (SDL_GetClipboardText)
      (abort (sdl-failure "SDL_GetClipboardText" #f))))

(: set-clipboard-text!
   (string -> void))
(define (set-clipboard-text! text)
  (let ((ret-code (SDL_SetClipboardText text)))
    (unless (zero? ret-code)
      (abort (sdl-failure "SDL_SetClipboardText" ret-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VERSION

(define (current-version)
  (with-temp-mem ((major-out (%allocate-Uint8))
                  (minor-out (%allocate-Uint8))
                  (patch-out (%allocate-Uint8)))
    (%current-version major-out minor-out patch-out)
    (list (pointer-u8-ref major-out)
          (pointer-u8-ref minor-out)
          (pointer-u8-ref patch-out))))

(define (compiled-version)
  (with-temp-mem ((major-out (%allocate-Uint8))
                  (minor-out (%allocate-Uint8))
                  (patch-out (%allocate-Uint8)))
    (%compiled-version major-out minor-out patch-out)
    (list (pointer-u8-ref major-out)
          (pointer-u8-ref minor-out)
          (pointer-u8-ref patch-out))))

(define (version-at-least? x y z)
  (SDL_VERSION_ATLEAST x y z))
