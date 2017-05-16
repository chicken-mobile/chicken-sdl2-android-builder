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


(export display-mode-format-raw    display-mode-format-raw-set!
        display-mode-format        display-mode-format-set!
        display-mode-w             display-mode-w-set!
        display-mode-h             display-mode-h-set!
        display-mode-refresh-rate  display-mode-refresh-rate-set!
        %display-mode-driverdata
        make-display-mode
        make-display-mode*)


(define-struct-field-accessors
  SDL_DisplayMode*
  display-mode?
  ("format"
   type:   Uint32
   getter: display-mode-format-raw
   setter: display-mode-format-raw-set!
   guard:  (Uint32-guard "sdl2:display-mode field format"))
  ("w"
   type:   Sint32
   getter: display-mode-w
   setter: display-mode-w-set!
   guard:  (Sint32-guard "sdl2:display-mode field w"))
  ("h"
   type:   Sint32
   getter: display-mode-h
   setter: display-mode-h-set!
   guard:  (Sint32-guard "sdl2:display-mode field h"))
  ("refresh_rate"
   type:   Sint32
   getter: display-mode-refresh-rate
   setter: display-mode-refresh-rate-set!
   guard:  (Sint32-guard "sdl2:display-mode field refresh-rate"))
  ("driverdata"
   type:   c-pointer
   getter: %display-mode-driverdata))


(define-enum-accessor
  getter: (display-mode-format
           raw:   display-mode-format-raw
           conv:  pixel-format-enum->symbol)
  setter: (display-mode-format-set!
           raw:   display-mode-format-raw-set!
           conv:  symbol->pixel-format-enum))



(define (make-display-mode
         #!optional (format 'unknown) (w 0) (h 0) (refresh-rate 0))
  (%autofree-struct!
   (make-display-mode* format w h refresh-rate)
   free-display-mode!))

(define (make-display-mode*
         #!optional (format 'unknown) (w 0) (h 0) (refresh-rate 0))
  (let ((dm (alloc-display-mode*)))
    (display-mode-format-set! dm format)
    (display-mode-w-set! dm w)
    (display-mode-h-set! dm h)
    (display-mode-refresh-rate-set! dm refresh-rate)
    dm))
