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


(export rect?
        wrap-rect
        unwrap-rect
        %rect-pointer
        %rect-pointer-set!
        free-rect!
        alloc-rect*
        alloc-rect)

(define-struct-record-type
  sdl2:rect "SDL_Rect"
  pred:    rect?
  wrap:    wrap-rect
  unwrap:  unwrap-rect
  (pointer %rect-pointer
           %rect-pointer-set!))

(define-struct-memory-helpers
  "SDL_Rect"
  using: (wrap-rect
          rect?
          %rect-pointer
          %rect-pointer-set!)
  define: (free-rect!
           alloc-rect*
           #f)) ;; alloc-rect defined below

(define (alloc-rect)
  (wrap-rect
   (make-locative
    (make-blob (foreign-type-size "SDL_Rect")))))


(define-struct-record-printer sdl2:rect
  %rect-pointer
  show-address: #f
  (#f rect->list))
