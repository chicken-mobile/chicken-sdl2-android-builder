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


(export renderer-info-name
        renderer-info-flags
        renderer-info-flags-raw
        renderer-info-num-texture-formats
        renderer-info-texture-formats
        renderer-info-texture-formats-raw
        renderer-info-max-texture-width
        renderer-info-max-texture-height)


(define-struct-field-accessors
  SDL_RendererInfo*
  renderer-info?
  ("name"
   type:   c-string
   getter: renderer-info-name)
  ("flags"
   type: Uint32
   getter: renderer-info-flags-raw)
  ("num_texture_formats"
   type:   Uint32
   getter: renderer-info-num-texture-formats)
  ;; See below.
  ;; ("texture_formats"
  ;;  type:   "Uint32[16]"
  ;;  getter: renderer-info-texture-formats-raw)
  ("max_texture_width"
   type:   Sint32
   getter: renderer-info-max-texture-width)
  ("max_texture_height"
   type:   Sint32
   getter: renderer-info-max-texture-height))


(: renderer-info-texture-flags
   (sdl2:renderer-info* -> (list-of symbol)))
(define (renderer-info-flags info)
  (unpack-renderer-flags (renderer-info-flags-raw info)))


;;; texture_formats is a C array stored by value, not a pointer.
;;; CHICKEN can't return C arrays by value, so we must define a
;;; function that returns a pointer.

(: renderer-info-texture-formats-raw
   (sdl2:renderer-info* -> pointer))
(define (renderer-info-texture-formats-raw info)
  (define foreign-getter
    (foreign-lambda*
     Uint32* ((SDL_RendererInfo* info))
     "C_return( &(info->texture_formats) );"))
  (assert (renderer-info? info))
  (foreign-getter info))


(: renderer-info-texture-formats
   (sdl2:renderer-info* -> (list-of symbol)))
(define (renderer-info-texture-formats info)
  (assert (renderer-info? info))
  (let ((ptr (renderer-info-texture-formats-raw info)))
    (list-tabulate (renderer-info-num-texture-formats info)
      (lambda (i)
        (let* ((offset (* i 4)) ;; offset 4 bytes per index
               (value (pointer-u32-ref (pointer+ ptr offset))))
          (pixel-format-enum->symbol value))))))
