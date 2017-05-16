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


(export rw-from-file
        rw-from-const-mem
        rw-from-mem

        rw-from-blob
        rw-from-string

        rw-close!)


(define (rw-from-file path mode)
  (let ((rwops (SDL_RWFromFile path mode)))
    (if (and (rwops? rwops) (not (struct-null? rwops)))
        rwops
        (abort (sdl-failure "SDL_RWFromFile" #f)))))

(define (rw-from-const-mem pointer size)
  (let ((rwops (SDL_RWFromConstMem pointer size)))
    (if (and (rwops? rwops) (not (struct-null? rwops)))
        rwops
        (abort (sdl-failure "SDL_RWFromConstMem" #f)))))

(define (rw-from-mem pointer size)
  (let ((rwops (SDL_RWFromMem pointer size)))
    (if (and (rwops? rwops) (not (struct-null? rwops)))
        rwops
        (abort (sdl-failure "SDL_RWFromMem" #f)))))


(define (rw-from-blob blob)
  (let ((rwops (SDL_RWFromMem
                (make-locative blob) (blob-size blob))))
    (if (and (rwops? rwops) (not (struct-null? rwops)))
        rwops
        (abort (sdl-failure "SDL_RWFromMem" #f)))))

(define (rw-from-string str)
  (let ((rwops (SDL_RWFromMem
                (make-locative str) (string-length str))))
    (if (and (rwops? rwops) (not (struct-null? rwops)))
        rwops
        (abort (sdl-failure "SDL_RWFromMem" #f)))))


(define (rw-close! rwops)
  (let ((ret-code (SDL_RWclose rwops)))
    (if (zero? ret-code)
        (%nullify-struct! rwops)
        (abort (sdl-failure "SDL_RWclose" ret-code)))))
