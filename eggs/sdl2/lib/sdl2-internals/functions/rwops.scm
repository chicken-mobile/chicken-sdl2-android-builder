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


(export SDL_AllocRW
        SDL_FreeRW

        SDL_RWFromConstMem
        SDL_RWFromFP
        SDL_RWFromFile
        SDL_RWFromMem

        SDL_RWclose
        SDL_RWread
        SDL_RWseek
        SDL_RWtell
        SDL_RWwrite

        SDL_ReadBE16
        SDL_ReadBE32
        SDL_ReadBE64
        SDL_ReadLE16
        SDL_ReadLE32
        SDL_ReadLE64

        SDL_WriteBE16
        SDL_WriteBE32
        SDL_WriteBE64
        SDL_WriteLE16
        SDL_WriteLE32
        SDL_WriteLE64)


(define-function-binding SDL_AllocRW
  return: (SDL_RWops* new-rwops))

(define-function-binding SDL_FreeRW
  args: ((SDL_RWops* rwops)))

(define-function-binding SDL_RWFromConstMem
  return: (SDL_RWops* new-rwops-or-null)
  args: (((const c-pointer) mem)
         (Sint32 size)))

(define-function-binding SDL_RWFromFP
  return: (SDL_RWops* new-rwops-or-null)
  args: ((c-pointer file-pointer)
         (bool autoclose)))

(define-function-binding SDL_RWFromFile
  return: (SDL_RWops* new-rwops-or-null)
  args: ((c-string path)
         (c-string mode)))

(define-function-binding SDL_RWFromMem
  return: (SDL_RWops* new-rwops-or-null)
  args: ((c-pointer mem)
         (Sint32 size)))


(define-function-binding SDL_RWclose
  return: (Sint32 zero-if-success)
  args: ((SDL_RWops* context)))

(define-function-binding SDL_RWread
  return: (size_t num-objects-read)
  args: ((SDL_RWops* context)
         (c-pointer ptr)
         (size_t size)
         (size_t maxum)))

(define-function-binding SDL_RWseek
  return: (Sint64 final-offset)
  args: ((SDL_RWops* context)
         (Sint64 offset)
         (SDL_RWopsWhenceEnum whence)))

(define-function-binding SDL_RWtell
  return: (Sint64 current-offset)
  args: ((SDL_RWops* context)))

(define-function-binding SDL_RWwrite
  return: (size_t num-objects-written)
  args: ((SDL_RWops* context)
         (c-pointer pts)
         (size_t size)
         (size_t num)))


(define-function-binding SDL_ReadBE16
  return: (Uint16 data)
  args: ((SDL_RWops* src)))

(define-function-binding SDL_ReadBE32
  return: (Uint32 data)
  args: ((SDL_RWops* src)))

(define-function-binding SDL_ReadBE64
  return: (Uint64 data)
  args: ((SDL_RWops* src)))

(define-function-binding SDL_ReadLE16
  return: (Uint16 data)
  args: ((SDL_RWops* src)))

(define-function-binding SDL_ReadLE32
  return: (Uint32 data)
  args: ((SDL_RWops* src)))

(define-function-binding SDL_ReadLE64
  return: (Uint64 data)
  args: ((SDL_RWops* src)))


(define-function-binding SDL_WriteBE16
  return: (bool success?)
  args: ((SDL_RWops* src)
         (Uint16 value)))

(define-function-binding SDL_WriteBE32
  return: (bool success?)
  args: ((SDL_RWops* src)
         (Uint32 value)))

(define-function-binding SDL_WriteBE64
  return: (bool success?)
  args: ((SDL_RWops* src)
         (Uint64 value)))

(define-function-binding SDL_WriteLE16
  return: (bool success?)
  args: ((SDL_RWops* src)
         (Uint16 value)))

(define-function-binding SDL_WriteLE32
  return: (bool success?)
  args: ((SDL_RWops* src)
         (Uint32 value)))

(define-function-binding SDL_WriteLE64
  return: (bool success?)
  args: ((SDL_RWops* src)
         (Uint64 value)))

