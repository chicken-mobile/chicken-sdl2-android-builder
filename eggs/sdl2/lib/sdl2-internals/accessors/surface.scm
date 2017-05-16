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


(export surface-format
        surface-w
        surface-h
        surface-pitch
        surface-pixels-raw
        surface-userdata-raw  surface-userdata-raw-set!
        surface-refcount      surface-refcount-set!)


(define-struct-field-accessors
  SDL_Surface*
  surface?
  ;; omitted: flags  (internal use)
  ("format"
   type:   SDL_PixelFormat*
   getter: surface-format)
  ("w"
   type:   Sint32
   getter: surface-w)
  ("h"
   type:   Sint32
   getter: surface-h)
  ("pitch"
   type:   Sint32
   getter: surface-pitch)
  ("pixels"
   type:   c-pointer
   getter: surface-pixels-raw)
  ("userdata"
   type:   c-pointer
   getter: surface-userdata-raw
   setter: surface-userdata-raw-set!)
  ;; omitted: locked     (internal use)
  ;; omitted: lock_data  (internal use)
  ;; omitted: clip_rect  (use SDL_GetClipRect / SDL_SetClipRect)
  ;; omitted: map        (internal use)
  ("refcount"
   type:   Sint32
   getter: surface-refcount
   setter: surface-refcount-set!
   guard:  (Sint32-guard "sdl2:surface field refcount")))
