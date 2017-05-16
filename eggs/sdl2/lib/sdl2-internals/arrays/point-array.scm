;;
;; chicken-sdl2: CHICKEN Scheme bindings to Simple DipointMedia Layer 2
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
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIPOINT,
;; INDIPOINT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


(export %allocate-point-array
        %read-point-array     %write-point-array
        %point-array->list    %point-list->array
        %point-array->vector  %point-vector->array
        %points->array)


(define-allocator %allocate-point-array "SDL_Point")

(define-array-reader %read-point-array
  "SDL_Point*" SDL_Point*)
(define-array-writer %write-point-array
  "SDL_Point*" SDL_Point*)

(define-array->list %point-array->list
  %read-point-array (alloc-point))
(define-list->array %point-list->array
  %allocate-point-array %write-point-array)

(define-array->vector %point-array->vector
  %read-point-array (alloc-point))
(define-vector->array %point-vector->array
  %allocate-point-array %write-point-array)

(define-items->array %points->array
  %point-list->array
  %point-vector->array)
