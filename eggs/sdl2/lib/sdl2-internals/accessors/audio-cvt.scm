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


(export audio-cvt-needed
        audio-cvt-src-format  audio-cvt-src-format-raw
        audio-cvt-dst-format  audio-cvt-dst-format-raw
        audio-cvt-rate-incr
        audio-cvt-buf-raw
        audio-cvt-len
        audio-cvt-len-cvt
        audio-cvt-len-mult
        audio-cvt-len-ratio)


(define-struct-field-accessors
  SDL_AudioCVT*
  audio-cvt?
  ("needed"
   type:   Sint32
   getter: audio-cvt-needed)
  ("src_format"
   type:   SDL_AudioFormat
   getter: audio-cvt-src-format-raw)
  ("dst_format"
   type:   SDL_AudioFormat
   getter: audio-cvt-dst-format-raw)
  ("rate_incr"
   type:   double
   getter: audio-cvt-rate-incr)
  ("buf"
   type:   Uint8*
   getter: audio-cvt-buf-raw)
  ("len"
   type:   Sint32
   getter: audio-cvt-len)
  ("len_cvt"
   type:   Sint32
   getter: audio-cvt-len-cvt)
  ("len_mult"
   type:   Sint32
   getter: audio-cvt-len-mult)
  ("len_ratio"
   type:   double
   getter: audio-cvt-len-ratio)
  ;; omitted: filters (internal use)
  ;; omitted: filter_index (internal use)
  )


(define-enum-accessor
  getter: (audio-cvt-src-format
           raw:   audio-cvt-src-format-raw
           conv:  audio-format-enum->symbol))

(define-enum-accessor
  getter: (audio-cvt-dst-format
           raw:   audio-cvt-dst-format-raw
           conv:  audio-format-enum->symbol))

