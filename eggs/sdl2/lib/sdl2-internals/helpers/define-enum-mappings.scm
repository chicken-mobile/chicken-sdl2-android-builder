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


;;; Macro: define-enum-mappings
;;;
;;; Defines and exports a group of related integer constants, plus
;;; defines (but does not export) procedures to convert them to or
;;; from user-friendly symbols.
;;;
;;; The constants should be semantically related, and each integer
;;; value must be unique within the group. They do not need to be
;;; defined as an enum in C.
;;;
;;; This macro can define conversion procedures in one of two ways:
;;;
;;; 1. Without hash tables. This is best for most enum groups.
;;;
;;; 2. With hash tables. This is best for enum groups with more than
;;;    100 members (like keycode and scancode), or enum groups that
;;;    need to be modifiable after definition (e.g. to add more enums
;;;    based on the SDL version).
;;;
;;; If used in the second way, this macro also defines (but does not
;;; export) hash tables that are used by the conversion procedures.
;;;
;;; Usage (without hash tables):
;;;
;;;   (define-enum-mappings
;;;     type: FOREIGN-TYPE
;;;     value->symbol: VALUE->SYMBOL
;;;     symbol->value: SYMBOL->VALUE
;;;     ((CONSTANT-NAME SYMBOL)
;;;      ...))
;;;
;;; Usage (with hash tables):
;;;
;;;   (define-enum-mappings
;;;     type: FOREIGN-TYPE
;;;     value->symbol: (VALUE->SYMBOL
;;;                     VALUE->SYMBOL-TABLE)
;;;     symbol->value: (SYMBOL->VALUE
;;;                     SYMBOL->VALUE-TABLE)
;;;     ((CONSTANT-NAME SYMBOL)
;;;      ...))
;;;
;;; NOTE: The hash table can be enabled or disabled independently for
;;; either value->symbol, symbol->value, both, or neither. In fact, it
;;; is faster for symbol->value to NOT use a hash table, even for
;;; large enum groups.
;;;
;;; FOREIGN-TYPE must be an existing foreign type specifier that
;;; applies to all the constants' values.
;;;
;;; VALUE->SYMBOL is the name for a procedure to convert from an
;;; integer value to the corresponding user-friendly symbol. The
;;; procedure will be defined by this macro. The procedure will have
;;; one required argument, an integer value, and one optional
;;; argument, a procedure that will be called if the conversion fails
;;; (this can be used to throw an error or return a default value).
;;;
;;; VALUE->SYMBOL-TABLE (if specified) is the name for a hash table
;;; that will be used to convert from an integer value to a symbol.
;;; The table will be defined by this macro.
;;;
;;; SYMBOL->VALUE is the name for a procedure to convert from a
;;; user-friendly symbol to the corresponding integer value. The
;;; procedure will be defined by this macro. The procedure will have
;;; one required argument, a symbol, and one optional argument, a
;;; procedure that will be called if the conversion fails (this can be
;;; used to throw an error or return a default value).
;;;
;;; SYMBOL->VALUE-TABLE (if specified) is the name for a hash table
;;; that will be used to convert from a symbol to an integer value.
;;; The table will be defined by this macro.
;;;
;;; Each (CONSTANT-NAME SYMBOL) pair defines a one-to-one mapping
;;; between an integer constant and a user-friendly symbol.
;;;
;;; - CONSTANT-NAME is the name (as an unquoted symbol) of an integer
;;;   constant or enum value, exactly as it appears in C. This will
;;;   also be the name of the constant definition in Scheme.
;;;
;;; - SYMBOL is an unquoted symbol that users will use instead of the
;;;   integer constant. It should be lowercase and hyphen-separated.
;;;
;;; Examples:
;;;
;;;   ;; Without hash tables
;;;   (define-enum-mappings
;;;     type: Uint32
;;;     value->symbol: init-flag->symbol
;;;     symbol->value: symbol->init-flag
;;;     ((SDL_INIT_TIMER  timer)
;;;      (SDL_INIT_AUDIO  audio)
;;;      (SDL_INIT_VIDEO  video)))
;;;
;;;   ;; With hash tables
;;;   (define-enum-mappings
;;;     type: Uint32
;;;     value->symbol: (init-flag->symbol
;;;                     %init-flag->symbol-table)
;;;     symbol->value: (symbol->init-flag
;;;                     %symbol->init-flag-table)
;;;     ((SDL_INIT_TIMER  timer)
;;;      (SDL_INIT_AUDIO  audio)
;;;      (SDL_INIT_VIDEO  video)))
;;;
(define-syntax define-enum-mappings
  (syntax-rules (type: value->symbol: symbol->value:)
    ((define-enum-mappings
       type: foreign-type
       value->symbol: value->symbol-maybe-list
       symbol->value: symbol->value-maybe-list
       ((constant-name sym) ...))
     (begin
       (define-foreign-constants foreign-type constant-name ...)

       (%define-enum-mappings/expand-value->symbol
         value->symbol-maybe-list
         ((constant-name sym) ...))

       (%define-enum-mappings/expand-symbol->value
         symbol->value-maybe-list
         ((constant-name sym) ...))))))



(define-syntax %define-enum-mappings/expand-value->symbol
  (syntax-rules (type: value->symbol: symbol->value:)
    ;; With table name -- define table and via-table procedure.
    ((%define-enum-mappings/expand-value->symbol
      (value->symbol value->symbol-table)
      ((constant-name sym) ...))
     (begin
       (define-enum-value->symbol-table value->symbol-table
         ((constant-name sym) ...))
       (define-enum-value->symbol-via-table value->symbol
         value->symbol-table)))

    ;; Without table name -- define simple procedure.
    ((%define-enum-mappings/expand-value->symbol
      value->symbol
      ((constant-name sym) ...))
     (define-enum-value->symbol-simple value->symbol
       ((constant-name sym) ...)))))


(define-syntax %define-enum-mappings/expand-symbol->value
  (syntax-rules (type: symbol->value: symbol->value:)
    ;; With table name -- define table and via-table procedure.
    ((%define-enum-mappings/expand-symbol->value
      (symbol->value symbol->value-table)
      ((constant-name sym) ...))
     (begin
       (define-enum-symbol->value-table symbol->value-table
         ((constant-name sym) ...))
       (define-enum-symbol->value-via-table symbol->value
         symbol->value-table)))

    ;; Without table name -- define simple procedure.
    ((%define-enum-mappings/expand-symbol->value
      symbol->value
      ((constant-name sym) ...))
     (define-enum-symbol->value-simple symbol->value
       ((constant-name sym) ...)))))



(define-syntax define-enum-value->symbol-simple
  (syntax-rules ()
    ((define-enum-value->symbol value->symbol
       ((constant-name sym) ...))
     (define (value->symbol value #!optional not-found-callback)
       ;; Cannot use case, because each constant name must be
       ;; evaluated to get the integer value. If we could hardcode the
       ;; integers somehow, we could also use a case statement and
       ;; this would be very fast even for large enum groups.
       (select value
         ((constant-name) 'sym)
         ...
         (else (if not-found-callback
                   (not-found-callback value)
                   (error "invalid enum value" value))))))))


(define-syntax define-enum-symbol->value-simple
  (syntax-rules ()
    ((define-enum-symbol->value symbol->value
       ((constant-name sym) ...))
     (define (symbol->value symbol #!optional not-found-callback)
       (case symbol
         ((sym) constant-name)
         ...
         (else (if not-found-callback
                   (not-found-callback symbol)
                   (error "invalid enum symbol" symbol))))))))



(define-syntax define-enum-value->symbol-table
  (syntax-rules ()
    ((define-enum-value->symbol-table value->symbol-table
       ((constant-name sym) ...))
     (define value->symbol-table
       (alist->hash-table
        (list (cons constant-name 'sym) ...)
        test: =
        hash: number-hash)))))


(define-syntax define-enum-value->symbol-via-table
  (syntax-rules ()
    ((define-enum-value->symbol-via-table value->symbol
       value->symbol-table)
     (define (value->symbol value #!optional not-found-callback)
       (let ((symbol (hash-table-ref/default
                      value->symbol-table value #f)))
         (cond (symbol symbol)
               (not-found-callback (not-found-callback value))
               (#t (error "invalid enum value" value))))))))



(define-syntax define-enum-symbol->value-table
  (syntax-rules ()
    ((define-enum-symbol->value-table symbol->value-table
       ((constant-name sym) ...))
     (define symbol->value-table
       (alist->hash-table
        (list (cons 'sym constant-name) ...)
        test: eq?
        hash: symbol-hash)))))


(define-syntax define-enum-symbol->value-via-table
  (syntax-rules ()
    ((define-enum-symbol->value-via-table symbol->value
       symbol->value-table)
     (define (symbol->value symbol #!optional not-found-callback)
       (let ((value (hash-table-ref/default
                     symbol->value-table symbol #f)))
         (cond (value value)
               (not-found-callback (not-found-callback symbol))
               (#t (error "invalid enum symbol" symbol))))))))
