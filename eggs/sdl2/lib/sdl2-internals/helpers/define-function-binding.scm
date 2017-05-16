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


(export define-function-binding
        define-function-binding*)


;;; define-function-binding is the primary way of defining a low-level
;;; binding to a C function in this project. It does two things:
;;;
;;;   1. Creates a binding to a C function (using the standard CHICKEN
;;;      form, foreign-lambda).
;;;   2. Gives that binding a name in Scheme (using the standard
;;;      Scheme form, define).
;;;
;;; Usage:
;;;
;;;   (define-function-binding FUNC-NAME-SYMBOL
;;;     return: (RETURN-TYPE RETURN-SEMANTICS)  ; optional
;;;     args: ((ARG-TYPE ARG-NAME) ...))        ; optional
;;;
;;; FUNC-NAME-SYMBOL must be the exact name of the C function, as an
;;; unquoted symbol like SDL_Foo. It will also be the name of the
;;; Scheme definition.
;;;
;;; The `return: (...)` and/or `args: (...)` parts may be omitted, but
;;; if they are both provided, they must be provided in the correct
;;; order. In other words, they look like keyword arguments, but the
;;; order matters. This is to keep the macro implementation simple.
;;;
;;; If `return: (...)` is omitted, the function return type is void.
;;; If `args: (...)` is omitted, the function accepts no args.
;;;
;;; This macro exists to make function bindings more descriptive and
;;; self-documenting, and thus easier to maintain. It might also be
;;; used in the future to automatically generate API reference docs.
;;;
;;; RETURN-SEMANTICS should be a symbol that gives a hint about the
;;; meaning or significance of the return value. A good rule of thumb
;;; is, "If I were putting the return value into a variable, what
;;; might I call that variable?" RETURN-SEMANTICS has no effect on the
;;; code. It exists only to aid human understanding.
;;;
;;; ARG-NAME (for each arg) should be a symbol naming the argument.
;;; Usually this should be the same (or similar, but Scheme style) to
;;; the argument name listed in the SDL docs or headers. ARG-NAMEs
;;; have no effect on the code. They exist only to aid human
;;; understanding -- and to make it more obvious if you forgot an arg
;;; when defining the binding! :)
;;;
(define-syntax define-function-binding
  (syntax-rules (return: args:)
    ;; return type and args
    ((define-function-binding func-name-symbol
       return: (return-type return-semantics)
       args: ((arg-type arg-name) ...))
     (define func-name-symbol
       (foreign-lambda return-type func-name-symbol arg-type ...)))
    ;; no args
    ((define-function-binding func-name-symbol
       return: (return-type return-semantics))
     (define func-name-symbol
       (foreign-lambda return-type func-name-symbol)))
    ;; no return type (i.e. void)
    ((define-function-binding func-name-symbol
       args: ((arg-type arg-name) ...))
     (define func-name-symbol
       (foreign-lambda void func-name-symbol arg-type ...)))
    ;; no return type or args
    ((define-function-binding func-name-symbol)
     (define func-name-symbol
       (foreign-lambda void func-name-symbol)))))



;;; define-function-binding* is similar to define-function-binding,
;;; except that it is based on foreign-lambda*. So, it allows you to
;;; specify a custom function body as a string containing C code.
;;;
;;; Usage:
;;;
;;;   (define-function-binding* FUNC-NAME-SYMBOL
;;;     return: (RETURN-TYPE RETURN-SEMANTICS)    ; optional
;;;     args: ((ARG-TYPE ARG-NAME) ...)           ; optional
;;;     body: "C_return(foo);")
;;;
;;; This macro is often used for wrapping C macros in a function so
;;; that they can be called from Scheme. It is also used in special
;;; cases where a custom function body is useful, for example to do
;;; pointer stuff that would be much harder in Scheme.
;;;
;;; FUNC-NAME-SYMBOL will be the name of the Scheme definition. It is
;;; also usually the name of the C function that is being bound. But,
;;; unlike define-function-binding, it is not strictly necessary for
;;; it to be the name of a C function.
;;;
;;; As with define-function-binding, the `return: (...)` and/or `args:
;;; (...)` parts may be omitted, but if they are both provided, they
;;; must be provided in the correct order.
;;;
(define-syntax define-function-binding*
  (syntax-rules (return: args: body:)
    ;; return type and args
    ((define-function-binding* func-name-symbol
       return: (return-type return-semantics)
       args: ((arg-type arg-name) ...)
       body: body-string)
     (define func-name-symbol
       (foreign-lambda* return-type ((arg-type arg-name) ...) body-string)))
    ;; no args
    ((define-function-binding* func-name-symbol
       return: (return-type return-semantics)
       body: body-string)
     (define func-name-symbol
       (foreign-lambda* return-type () body-string)))
    ;; no return type (i.e. void)
    ((define-function-binding* func-name-symbol
       args: ((arg-type arg-name) ...)
       body: body-string)
     (define func-name-symbol
       (foreign-lambda* void ((arg-type arg-name) ...) body-string)))
    ;; no return type or args
    ((define-function-binding* func-name-symbol
       body: body-string)
     (define func-name-symbol
       (foreign-lambda* void () body-string)))))
