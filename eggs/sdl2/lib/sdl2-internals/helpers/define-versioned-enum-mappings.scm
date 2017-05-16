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


;;; Macro: define-versioned-enum-mappings
;;;
;;; Similar to define-enum-mappings, except it supports enums that are
;;; only available in a certain version of SDL and later.
;;;
;;; Defines and exports a group of related constants, and defines (but
;;; does not export) procedures to convert them to or from
;;; user-friendly symbols.
;;;
;;; The constants should be semantically related, and each value must
;;; be unique within the group. They do not need to be defined as an
;;; enum in C.
;;;
;;; Usage:
;;;
;;;   (define-enum-mappings
;;;     type: FOREIGN-TYPE
;;;     value->symbol: (VALUE->SYMBOL
;;;                     table: VALUE->SYMBOL-TABLE
;;;                     hash:  VALUE->SYMBOL-HASH-FN
;;;                     test:  VALUE->SYMBOL-TEST-FN)
;;;     symbol->value: (SYMBOL->VALUE
;;;                     table: SYMBOL->VALUE-TABLE)
;;;     (FEATURE
;;;      (CONSTANT-NAME SYMBOL)
;;;      ...)
;;;     ...)
;;;
;;; FOREIGN-TYPE must be an existing foreign type specifier that
;;; applies to all the constants' values.
;;;
;;; VALUE->SYMBOL is the name for a procedure to convert from an
;;; constant value to the corresponding user-friendly symbol. The
;;; procedure will be defined by this macro. The procedure will have
;;; one required argument, a constant value, and one optional
;;; argument, a procedure that will be called if the conversion fails
;;; (this can be used to throw an error or return a default value).
;;;
;;; VALUE->SYMBOL-TABLE is the name for a hash table that will be used
;;; to convert from a constant value to a symbol. The table will be
;;; defined by this macro.
;;;
;;; VALUE->SYMBOL-HASH-FN is the name of an existing procedure that
;;; will be used as the hashing function for VALUE->SYMBOL-TABLE. It
;;; should be appropriate for the type of constant. E.g. for integers
;;; use number-hash, for strings use string-hash.
;;;
;;; VALUE->SYMBOL-TEST-FN is the name of an existing procedure that
;;; will be used as the equivalence function for VALUE->SYMBOL-TABLE.
;;; It should be appropriate for the type of constant. E.g. for
;;; integers use =, for strings use string=?.
;;;
;;; SYMBOL->VALUE is the name for a procedure to convert from a
;;; user-friendly symbol to the corresponding constant value. The
;;; procedure will be defined by this macro. The procedure will have
;;; one required argument, a symbol, and one optional argument, a
;;; procedure that will be called if the conversion fails (this can be
;;; used to throw an error or return a default value).
;;;
;;; SYMBOL->VALUE-TABLE is the name for a hash table that will be used
;;; to convert from a symbol to the constant value. The table will be
;;; defined by this macro.
;;;
;;; Each FEATURE specifies a feature identifier indicating the minimum
;;; version number where the constants in that group are defined. If
;;; the feature is not registered, then the constant will not be
;;; defined, exported, or added to the tables. Usually FEATURE is of
;;; the form `libSDL-X.Y.Z+'.
;;;
;;; Each (CONSTANT-NAME SYMBOL) pair defines a one-to-one mapping
;;; between a constant and a user-friendly symbol.
;;;
;;; - CONSTANT-NAME is the name (as an unquoted symbol) of a constant
;;;   or enum value, exactly as it appears in C. This will also be the
;;;   name of the constant definition in Scheme.
;;;
;;; - SYMBOL is an unquoted symbol that users will use instead of the
;;;   constant. It should be lowercase and hyphen-separated.
;;;
;;; Example:
;;;
;;;   (define-versioned-enum-mappings
;;;     type: SDL_WindowFlags
;;;     value->symbol: (window-flag->symbol
;;;                     table: %window-flag->symbol-table
;;;                     hash:  number-hash
;;;                     test:  =)
;;;     symbol->value: (symbol->window-flag
;;;                     table: %symbol->window-flag-table)
;;;     (libSDL-2.0.0+
;;;      (SDL_WINDOW_FULLSCREEN          fullscreen)
;;;      (SDL_WINDOW_FULLSCREEN_DESKTOP  fullscreen-desktop))
;;;     (libSDL-2.0.1+
;;;      (SDL_WINDOW_ALLOW_HIGHDPI       allow-high-dpi))
;;;     (libSDL-2.0.4+
;;;      (SDL_WINDOW_MOUSE_CAPTURE       mouse-capture)))
;;;
(define-syntax define-versioned-enum-mappings
  (syntax-rules (type: value->symbol: table: hash: test: symbol->value:)
    ((define-versioned-enum-mappings
       type: FOREIGN-TYPE
       value->symbol: (VALUE->SYMBOL
                       table: VALUE->SYMBOL-TABLE
                       hash:  VALUE->SYMBOL-HASH
                       test:  VALUE->SYMBOL-TEST)
       symbol->value: (SYMBOL->VALUE
                       table: SYMBOL->VALUE-TABLE)
       (FEATURE (CONSTANT-NAME SYM) ...)
       ...)
     (begin
       (define VALUE->SYMBOL-TABLE
         (make-hash-table #:hash VALUE->SYMBOL-HASH
                          #:test VALUE->SYMBOL-TEST))
       (define SYMBOL->VALUE-TABLE
         (make-hash-table #:hash symbol-hash
                          #:test eq?))
       (define-enum-value->symbol-via-table
         VALUE->SYMBOL
         VALUE->SYMBOL-TABLE)
       (define-enum-symbol->value-via-table
         SYMBOL->VALUE
         SYMBOL->VALUE-TABLE)
       (cond-expand
         (FEATURE
          (define-foreign-constants FOREIGN-TYPE CONSTANT-NAME ...)
          (hash-table-set! VALUE->SYMBOL-TABLE CONSTANT-NAME 'SYM)
          ...
          (hash-table-set! SYMBOL->VALUE-TABLE 'SYM CONSTANT-NAME)
          ...)
         (else))
       ...))))


