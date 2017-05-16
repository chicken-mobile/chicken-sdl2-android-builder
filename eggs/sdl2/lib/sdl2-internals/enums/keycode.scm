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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KEYCODE

(export keycode->symbol
        symbol->keycode)


(define-enum-mappings
  type: SDL_Keycode
  value->symbol: (keycode->symbol
                  %keycode->symbol-table)
  symbol->value: symbol->keycode

  ((SDLK_UNKNOWN             unknown)
   (SDLK_RETURN              return)
   (SDLK_ESCAPE              escape)
   (SDLK_BACKSPACE           backspace)
   (SDLK_TAB                 tab)
   (SDLK_SPACE               space)
   (SDLK_EXCLAIM             exclaim)
   (SDLK_QUOTEDBL            quote-dbl)
   (SDLK_HASH                hash)
   (SDLK_PERCENT             percent)
   (SDLK_DOLLAR              dollar)
   (SDLK_AMPERSAND           ampersand)
   (SDLK_QUOTE               quote)
   (SDLK_LEFTPAREN           left-paren)
   (SDLK_RIGHTPAREN          right-paren)
   (SDLK_ASTERISK            asterisk)
   (SDLK_PLUS                plus)
   (SDLK_COMMA               comma)
   (SDLK_MINUS               minus)
   (SDLK_PERIOD              period)
   (SDLK_SLASH               slash)
   (SDLK_0                   n-0)
   (SDLK_1                   n-1)
   (SDLK_2                   n-2)
   (SDLK_3                   n-3)
   (SDLK_4                   n-4)
   (SDLK_5                   n-5)
   (SDLK_6                   n-6)
   (SDLK_7                   n-7)
   (SDLK_8                   n-8)
   (SDLK_9                   n-9)
   (SDLK_COLON               colon)
   (SDLK_SEMICOLON           semicolon)
   (SDLK_LESS                less)
   (SDLK_EQUALS              equals)
   (SDLK_GREATER             greater)
   (SDLK_QUESTION            question)
   (SDLK_AT                  at)

   (SDLK_LEFTBRACKET         left-bracket)
   (SDLK_BACKSLASH           backslash)
   (SDLK_RIGHTBRACKET        right-bracket)
   (SDLK_CARET               caret)
   (SDLK_UNDERSCORE          underscore)
   (SDLK_BACKQUOTE           backquote)

   (SDLK_a                   a)
   (SDLK_b                   b)
   (SDLK_c                   c)
   (SDLK_d                   d)
   (SDLK_e                   e)
   (SDLK_f                   f)
   (SDLK_g                   g)
   (SDLK_h                   h)
   (SDLK_i                   i)
   (SDLK_j                   j)
   (SDLK_k                   k)
   (SDLK_l                   l)
   (SDLK_m                   m)
   (SDLK_n                   n)
   (SDLK_o                   o)
   (SDLK_p                   p)
   (SDLK_q                   q)
   (SDLK_r                   r)
   (SDLK_s                   s)
   (SDLK_t                   t)
   (SDLK_u                   u)
   (SDLK_v                   v)
   (SDLK_w                   w)
   (SDLK_x                   x)
   (SDLK_y                   y)
   (SDLK_z                   z)

   (SDLK_CAPSLOCK            caps-lock)

   (SDLK_F1                  f1)
   (SDLK_F2                  f2)
   (SDLK_F3                  f3)
   (SDLK_F4                  f4)
   (SDLK_F5                  f5)
   (SDLK_F6                  f6)
   (SDLK_F7                  f7)
   (SDLK_F8                  f8)
   (SDLK_F9                  f9)
   (SDLK_F10                 f10)
   (SDLK_F11                 f11)
   (SDLK_F12                 f12)

   (SDLK_PRINTSCREEN         print-screen)
   (SDLK_SCROLLLOCK          scroll-lock)
   (SDLK_PAUSE               pause)
   (SDLK_INSERT              insert)
   (SDLK_HOME                home)
   (SDLK_PAGEUP              page-up)
   (SDLK_DELETE              delete)
   (SDLK_END                 end)
   (SDLK_PAGEDOWN            page-down)
   (SDLK_RIGHT               right)
   (SDLK_LEFT                left)
   (SDLK_DOWN                down)
   (SDLK_UP                  up)

   (SDLK_NUMLOCKCLEAR        num-lock-clear)
   (SDLK_KP_DIVIDE           kp-divide)
   (SDLK_KP_MULTIPLY         kp-multiply)
   (SDLK_KP_MINUS            kp-minus)
   (SDLK_KP_PLUS             kp-plus)
   (SDLK_KP_ENTER            kp-enter)
   (SDLK_KP_1                kp-1)
   (SDLK_KP_2                kp-2)
   (SDLK_KP_3                kp-3)
   (SDLK_KP_4                kp-4)
   (SDLK_KP_5                kp-5)
   (SDLK_KP_6                kp-6)
   (SDLK_KP_7                kp-7)
   (SDLK_KP_8                kp-8)
   (SDLK_KP_9                kp-9)
   (SDLK_KP_0                kp-0)
   (SDLK_KP_PERIOD           kp-period)

   (SDLK_APPLICATION         application)
   (SDLK_POWER               power)
   (SDLK_KP_EQUALS           kp-equals)
   (SDLK_F13                 f13)
   (SDLK_F14                 f14)
   (SDLK_F15                 f15)
   (SDLK_F16                 f16)
   (SDLK_F17                 f17)
   (SDLK_F18                 f18)
   (SDLK_F19                 f19)
   (SDLK_F20                 f20)
   (SDLK_F21                 f21)
   (SDLK_F22                 f22)
   (SDLK_F23                 f23)
   (SDLK_F24                 f24)
   (SDLK_EXECUTE             execute)
   (SDLK_HELP                help)
   (SDLK_MENU                menu)
   (SDLK_SELECT              select)
   (SDLK_STOP                stop)
   (SDLK_AGAIN               again)
   (SDLK_UNDO                undo)
   (SDLK_CUT                 cut)
   (SDLK_COPY                copy)
   (SDLK_PASTE               paste)
   (SDLK_FIND                find)
   (SDLK_MUTE                mute)
   (SDLK_VOLUMEUP            volume-up)
   (SDLK_VOLUMEDOWN          volume-down)
   (SDLK_KP_COMMA            kp-comma)
   (SDLK_KP_EQUALSAS400      kp-equals-as400)

   (SDLK_ALTERASE            alt-erase)
   (SDLK_SYSREQ              sys-req)
   (SDLK_CANCEL              cancel)
   (SDLK_CLEAR               clear)
   (SDLK_PRIOR               prior)
   (SDLK_RETURN2             return2)
   (SDLK_SEPARATOR           separator)
   (SDLK_OUT                 out)
   (SDLK_OPER                oper)
   (SDLK_CLEARAGAIN          clear-again)
   (SDLK_CRSEL               crsel)
   (SDLK_EXSEL               exsel)

   (SDLK_KP_00               kp-00)
   (SDLK_KP_000              kp-000)
   (SDLK_THOUSANDSSEPARATOR  thousands-separator)
   (SDLK_DECIMALSEPARATOR    decimal-separator)
   (SDLK_CURRENCYUNIT        currency-unit)
   (SDLK_CURRENCYSUBUNIT     currency-subunit)
   (SDLK_KP_LEFTPAREN        kp-left-paren)
   (SDLK_KP_RIGHTPAREN       kp-right-paren)
   (SDLK_KP_LEFTBRACE        kp-left-brace)
   (SDLK_KP_RIGHTBRACE       kp-right-brace)
   (SDLK_KP_TAB              kp-tab)
   (SDLK_KP_BACKSPACE        kp-backspace)
   (SDLK_KP_A                kp-a)
   (SDLK_KP_B                kp-b)
   (SDLK_KP_C                kp-c)
   (SDLK_KP_D                kp-d)
   (SDLK_KP_E                kp-e)
   (SDLK_KP_F                kp-f)
   (SDLK_KP_XOR              kp-xor)
   (SDLK_KP_POWER            kp-power)
   (SDLK_KP_PERCENT          kp-percent)
   (SDLK_KP_LESS             kp-less)
   (SDLK_KP_GREATER          kp-greater)
   (SDLK_KP_AMPERSAND        kp-ampersand)
   (SDLK_KP_DBLAMPERSAND     kp-dbl-ampersand)
   (SDLK_KP_VERTICALBAR      kp-vertical-bar)
   (SDLK_KP_DBLVERTICALBAR   kp-dbl-vertical-bar)
   (SDLK_KP_COLON            kp-colon)
   (SDLK_KP_HASH             kp-hash)
   (SDLK_KP_SPACE            kp-space)
   (SDLK_KP_AT               kp-at)
   (SDLK_KP_EXCLAM           kp-exclam)
   (SDLK_KP_MEMSTORE         kp-mem-store)
   (SDLK_KP_MEMRECALL        kp-mem-recall)
   (SDLK_KP_MEMCLEAR         kp-mem-clear)
   (SDLK_KP_MEMADD           kp-mem-add)
   (SDLK_KP_MEMSUBTRACT      kp-mem-subtract)
   (SDLK_KP_MEMMULTIPLY      kp-mem-multiply)
   (SDLK_KP_MEMDIVIDE        kp-mem-divide)
   (SDLK_KP_PLUSMINUS        kp-plus-minus)
   (SDLK_KP_CLEAR            kp-clear)
   (SDLK_KP_CLEARENTRY       kp-clear-entry)
   (SDLK_KP_BINARY           kp-binary)
   (SDLK_KP_OCTAL            kp-octal)
   (SDLK_KP_DECIMAL          kp-decimal)
   (SDLK_KP_HEXADECIMAL      kp-hexadecimal)

   (SDLK_LCTRL               lctrl)
   (SDLK_LSHIFT              lshift)
   (SDLK_LALT                lalt)
   (SDLK_LGUI                lgui)
   (SDLK_RCTRL               rctrl)
   (SDLK_RSHIFT              rshift)
   (SDLK_RALT                ralt)
   (SDLK_RGUI                rgui)

   (SDLK_MODE                mode)

   (SDLK_AUDIONEXT           audio-next)
   (SDLK_AUDIOPREV           audio-prev)
   (SDLK_AUDIOSTOP           audio-stop)
   (SDLK_AUDIOPLAY           audio-play)
   (SDLK_AUDIOMUTE           audio-mute)
   (SDLK_MEDIASELECT         media-select)
   (SDLK_WWW                 www)
   (SDLK_MAIL                mail)
   (SDLK_CALCULATOR          calculator)
   (SDLK_COMPUTER            computer)
   (SDLK_AC_SEARCH           ac-search)
   (SDLK_AC_HOME             ac-home)
   (SDLK_AC_BACK             ac-back)
   (SDLK_AC_FORWARD          ac-forward)
   (SDLK_AC_STOP             ac-stop)
   (SDLK_AC_REFRESH          ac-refresh)
   (SDLK_AC_BOOKMARKS        ac-bookmarks)

   (SDLK_BRIGHTNESSDOWN      brightness-down)
   (SDLK_BRIGHTNESSUP        brightness-up)
   (SDLK_DISPLAYSWITCH       display-switch)
   (SDLK_KBDILLUMTOGGLE      kbd-illum-toggle)
   (SDLK_KBDILLUMDOWN        kbd-illum-down)
   (SDLK_KBDILLUMUP          kbd-illum-up)
   (SDLK_EJECT               eject)
   (SDLK_SLEEP               sleep)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KEYMOD

(export keymod->symbol
        symbol->keymod
        pack-keymods
        unpack-keymods)


(define-enum-mappings
  type: SDL_Keymod
  value->symbol: keymod->symbol
  symbol->value: symbol->keymod

  ((KMOD_NONE      none)

   (KMOD_LSHIFT    lshift)
   (KMOD_RSHIFT    rshift)
   (KMOD_SHIFT     shift)

   (KMOD_LCTRL     lctrl)
   (KMOD_RCTRL     rctrl)
   (KMOD_CTRL      ctrl)

   (KMOD_LALT      lalt)
   (KMOD_RALT      ralt)
   (KMOD_ALT       alt)

   (KMOD_LGUI      lgui)
   (KMOD_RGUI      rgui)
   (KMOD_GUI       gui)

   (KMOD_NUM       num)
   (KMOD_CAPS      caps)
   (KMOD_MODE      mode)))


(define-enum-mask-packer pack-keymods
  symbol->keymod)

(define-enum-mask-unpacker unpack-keymods
  keymod->symbol
  (list
   ;; omitted: KMOD_NONE
   KMOD_SHIFT  KMOD_LSHIFT  KMOD_RSHIFT
   KMOD_CTRL   KMOD_LCTRL   KMOD_RCTRL
   KMOD_ALT    KMOD_LALT    KMOD_RALT
   KMOD_GUI    KMOD_LGUI    KMOD_RGUI
   KMOD_NUM
   KMOD_CAPS
   KMOD_MODE))
