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


(export audio-format-enum->symbol
        symbol->audio-format-enum)


(define-foreign-constants int
  SDL_AUDIO_MASK_BITSIZE
  SDL_AUDIO_MASK_DATATYPE
  SDL_AUDIO_MASK_ENDIAN
  SDL_AUDIO_MASK_SIGNED
  SDL_MIX_MAXVOLUME
  SDL_AUDIO_ALLOW_FREQUENCY_CHANGE
  SDL_AUDIO_ALLOW_FORMAT_CHANGE
  SDL_AUDIO_ALLOW_CHANNELS_CHANGE
  SDL_AUDIO_ALLOW_ANY_CHANGE)


(define-enum-mappings
  type: SDL_AudioFormatEnum
  value->symbol: audio-format-enum->symbol
  symbol->value: symbol->audio-format-enum

  ((AUDIO_U8      u8)
   (AUDIO_S8      s8)
   (AUDIO_U16LSB  u16lsb)
   (AUDIO_S16LSB  s16lsb)
   (AUDIO_U16MSB  u16msb)
   (AUDIO_S16MSB  s16msb)
   (AUDIO_U16     u16)
   (AUDIO_S16     s16)
   (AUDIO_S32LSB  s32lsb)
   (AUDIO_S32MSB  s32msb)
   (AUDIO_S32     s32)
   (AUDIO_F32LSB  f32lsb)
   (AUDIO_F32MSB  f32msb)
   (AUDIO_F32     f32)
   (AUDIO_U16SYS  u16sys)
   (AUDIO_S16SYS  s16sys)
   (AUDIO_S32SYS  s32sys)
   (AUDIO_F32SYS  f32sys)))


(define-foreign-constants SDL_AudioStatus
  SDL_AUDIO_STOPPED
  SDL_AUDIO_PLAYING
  SDL_AUDIO_PAUSED)
