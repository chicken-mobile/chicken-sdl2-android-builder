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
;;; WINDOW POS

(define-foreign-constants Uint32
  SDL_WINDOWPOS_UNDEFINED
  SDL_WINDOWPOS_CENTERED
  SDL_WINDOWPOS_UNDEFINED_MASK
  SDL_WINDOWPOS_CENTERED_MASK)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WINDOW FLAGS

(export window-flag->symbol
        symbol->window-flag
        pack-window-flags
        unpack-window-flags)


(define-versioned-enum-mappings
  type: SDL_WindowFlags
  value->symbol: (window-flag->symbol
                  table: %window-flag->symbol-table
                  hash:  number-hash
                  test:  =)
  symbol->value: (symbol->window-flag
                  table: %symbol->window-flag-table)
  (libSDL-2.0.0+
   (SDL_WINDOW_FULLSCREEN          fullscreen)
   (SDL_WINDOW_FULLSCREEN_DESKTOP  fullscreen-desktop)
   (SDL_WINDOW_OPENGL              opengl)
   (SDL_WINDOW_SHOWN               shown)
   (SDL_WINDOW_HIDDEN              hidden)
   (SDL_WINDOW_BORDERLESS          borderless)
   (SDL_WINDOW_RESIZABLE           resizable)
   (SDL_WINDOW_MINIMIZED           minimized)
   (SDL_WINDOW_MAXIMIZED           maximized)
   (SDL_WINDOW_INPUT_GRABBED       input-grabbed)
   (SDL_WINDOW_INPUT_FOCUS         input-focus)
   (SDL_WINDOW_MOUSE_FOCUS         mouse-focus)
   (SDL_WINDOW_FOREIGN             foreign))
  (libSDL-2.0.1+
   (SDL_WINDOW_ALLOW_HIGHDPI       allow-high-dpi))
  (libSDL-2.0.4+
   (SDL_WINDOW_MOUSE_CAPTURE       mouse-capture)))


(define-enum-mask-packer pack-window-flags
  symbol->window-flag)

(define-enum-mask-unpacker unpack-window-flags
  window-flag->symbol
  (append
   (list SDL_WINDOW_FULLSCREEN
         SDL_WINDOW_FULLSCREEN_DESKTOP
         SDL_WINDOW_OPENGL
         SDL_WINDOW_SHOWN
         SDL_WINDOW_HIDDEN
         SDL_WINDOW_BORDERLESS
         SDL_WINDOW_RESIZABLE
         SDL_WINDOW_MINIMIZED
         SDL_WINDOW_MAXIMIZED
         SDL_WINDOW_INPUT_GRABBED
         SDL_WINDOW_INPUT_FOCUS
         SDL_WINDOW_MOUSE_FOCUS
         SDL_WINDOW_FOREIGN)
   (cond-expand
     (libSDL-2.0.1+ (list SDL_WINDOW_ALLOW_HIGHDPI))
     (else '()))
   (cond-expand
     (libSDL-2.0.4+ (list SDL_WINDOW_MOUSE_CAPTURE))
     (else '()))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WINDOW EVENT IDS

(export window-event-id->symbol
        symbol->window-event-id)

(define-enum-mappings
  type: SDL_WindowEventID
  value->symbol: window-event-id->symbol
  symbol->value: symbol->window-event-id

  ((SDL_WINDOWEVENT_NONE          none)
   (SDL_WINDOWEVENT_SHOWN         shown)
   (SDL_WINDOWEVENT_HIDDEN        hidden)
   (SDL_WINDOWEVENT_EXPOSED       exposed)
   (SDL_WINDOWEVENT_MOVED         moved)
   (SDL_WINDOWEVENT_RESIZED       resized)
   (SDL_WINDOWEVENT_SIZE_CHANGED  size-changed)
   (SDL_WINDOWEVENT_MINIMIZED     minimized)
   (SDL_WINDOWEVENT_MAXIMIZED     maximized)
   (SDL_WINDOWEVENT_RESTORED      restored)
   (SDL_WINDOWEVENT_ENTER         enter)
   (SDL_WINDOWEVENT_LEAVE         leave)
   (SDL_WINDOWEVENT_FOCUS_GAINED  focus-gained)
   (SDL_WINDOWEVENT_FOCUS_LOST    focus-lost)
   (SDL_WINDOWEVENT_CLOSE         close)))
