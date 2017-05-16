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


(export audio-spec-freq          audio-spec-freq-set!
        audio-spec-format-raw    audio-spec-format-raw-set!
        audio-spec-format        audio-spec-format-set!
        audio-spec-channels      audio-spec-channels-set!
        audio-spec-silence
        audio-spec-samples       audio-spec-samples-set!
        audio-spec-size
        audio-spec-callback      audio-spec-callback-set!
        audio-spec-userdata-raw  audio-spec-userdata-raw-set!)


(define-struct-field-accessors
  SDL_AudioSpec*
  audio-spec?
  ("freq"
   type:   Sint32
   getter: audio-spec-freq
   setter: audio-spec-freq-set!
   guard:  (Sint32-guard "sdl2:audio-spec field freq"))
  ("format"
   type:   SDL_AudioFormat
   getter: audio-spec-format-raw
   setter: audio-spec-format-raw-set!)
  ("channels"
   type:   Uint8
   getter: audio-spec-channels
   setter: audio-spec-channels-set!
   guard:  (Uint8-guard "sdl2:audio-spec field channels"))
  ("silence"
   type:   Uint8
   getter: audio-spec-silence
   ;; no setter because value is calculated by SDL_OpenAudioDevice
   )
  ("samples"
   type:   Uint16
   getter: audio-spec-samples
   setter: audio-spec-samples-set!
   guard:  (Uint16-guard "sdl2:audio-spec field samples"))
  ("size"
   type:   Uint32
   getter: audio-spec-size
   ;; no setter because value is calculated by SDL_OpenAudioDevice
   )
  ("callback"
   type:   SDL_AudioCallback
   getter: audio-spec-callback
   setter: audio-spec-callback-set!)
  ("userdata"
   type:   c-pointer
   getter: audio-spec-userdata-raw
   setter: audio-spec-userdata-raw-set!))


(define-enum-accessor
  getter: (audio-spec-format
           raw:   audio-spec-format-raw
           conv:  audio-format-enum->symbol)
  setter: (audio-spec-format-set!
           raw:   audio-spec-format-raw-set!
           conv:  symbol->audio-format-enum))
