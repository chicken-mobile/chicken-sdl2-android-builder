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


(export get-hint
        set-hint!
        clear-hints!)


(define-inline (%hint-name->str name fn-name)
  (if (string? name)
      name
      (symbol->hint
       name
       (lambda (x)
         (error fn-name "Unrecognized hint name" x)))))

(define-inline (%hint-priority->int priority fn-name)
  (case priority
    ((default)  SDL_HINT_DEFAULT)
    ((normal)   SDL_HINT_NORMAL)
    ((override) SDL_HINT_OVERRIDE)
    (else (if (integer? priority)
              priority
              (error fn-name
                     "Invalid hint priority"
                     priority)))))


(: get-hint
   ((or symbol string) -> (or string boolean)))
(define (get-hint name)
  (SDL_GetHint (%hint-name->str name 'get-hint)))


(: set-hint!
   ((or symbol string) string #!optional (or symbol fixnum)
    -> boolean))
(define (set-hint! name value #!optional (priority 'normal))
  (assert (string? value) "Hint value must be a string" value)
  (SDL_SetHintWithPriority
   (%hint-name->str name 'set-hint!)
   value
   (%hint-priority->int priority 'set-hint!)))


(: clear-hints!
   (-> void))
(define (clear-hints!)
  (SDL_ClearHints))
