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


(export keysym-scancode-raw  keysym-scancode-raw-set!
        keysym-sym-raw       keysym-sym-raw-set!
        keysym-mod-raw       keysym-mod-raw-set!

        keysym-scancode      keysym-scancode-set!
        keysym-sym           keysym-sym-set!
        keysym-mod           keysym-mod-set!

        make-keysym
        make-keysym*)


(define-struct-field-accessors
  SDL_Keysym*
  keysym?
  ("scancode"
   type:   SDL_Scancode
   getter: keysym-scancode-raw
   setter: keysym-scancode-raw-set!
   guard:  (Sint32-guard "sdl2:keysym field scancode"))
  ("sym"
   type:   SDL_Keycode
   getter: keysym-sym-raw
   setter: keysym-sym-raw-set!
   guard:  (Sint32-guard "sdl2:keysym field sym"))
  ("mod"
   type:   Uint16
   getter: keysym-mod-raw
   setter: keysym-mod-raw-set!
   guard:  (Uint16-guard "sdl2:keysym field mod")))


(define-enum-accessor
  getter: (keysym-scancode
           raw:   keysym-scancode-raw
           conv:  scancode->symbol)
  setter: (keysym-scancode-set!
           raw:   keysym-scancode-raw-set!
           conv:  symbol->scancode))


(define-enum-accessor
  getter: (keysym-sym
           raw:   keysym-sym-raw
           conv:  keycode->symbol)
  setter: (keysym-sym-set!
           raw:   keysym-sym-raw-set!
           conv:  symbol->keycode))


(define-enum-mask-accessor
  getter: (keysym-mod
           raw:    keysym-mod-raw
           unpack: unpack-keymods
           exact:  #f)
  setter: (keysym-mod-set!
           raw:    keysym-mod-raw-set!
           pack:   pack-keymods))


(define (make-keysym
         #!optional (scancode 'unknown) (sym 'unknown) (mod '()))
  (let ((keysym (alloc-keysym)))
    (keysym-sym-set! keysym sym)
    (keysym-scancode-set! keysym scancode)
    (keysym-mod-set! keysym mod)
    keysym))

(define (make-keysym*
         #!optional (scancode 'unknown) (sym 'unknown) (mod '()))
  (let ((keysym (alloc-keysym*)))
    (keysym-sym-set! keysym sym)
    (keysym-scancode-set! keysym scancode)
    (keysym-mod-set! keysym mod)
    keysym))
