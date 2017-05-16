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
;;; UNIVERSAL STRUCT RECORD HELPERS
;;; These are helper procedures that work for all struct record types.

(export struct-eq?
        struct-null?
        %nullify-struct!
        %autofree-struct!)


;;; Returns #t if the objects refer to the same memory location.
;;; The objects can be struct records, pointers, locatives, or #f.
(define (struct-eq? obj1 obj2)
  (define (%address obj)
    (cond ((record-instance? obj)
           (%address (record-instance-slot obj 0)))
          ((or (pointer? obj) (locative? obj))
           (pointer->address obj))
          ((blob? obj)
           (pointer->address (make-locative obj)))
          ((not obj)
           0)
          (else
           (error 'struct-eq? "Unsupported object" obj))))
  (= (%address obj1) (%address obj2)))


;;; Returns #t if record is holding a null pointer (address 0).
;;; Assumes that the record's first slot holds its pointer, which is
;;; true for all struct record types in this library.
(define (struct-null? record)
  (let ((ptr (record-instance-slot record 0)))
    (and (pointer? ptr) (= 0 (pointer->address ptr)))))


;;; Sets the struct record's pointer to null (address 0). This is used
;;; when the struct is freed.
(define (%nullify-struct! record)
  (set! (record-instance-slot record 0)
        (address->pointer 0)))


;;; If given a record, set its finalizer to freer and return the
;;; record. Otherwise just return the given thing. This is useful for
;;; procedures that may return either a struct record or #f.
(define (%autofree-struct! maybe-record freer)
  (if (record-instance? maybe-record)
      (set-finalizer! maybe-record freer)
      maybe-record))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCT RECORD TYPE

;;; Defines a record type that wraps a pointer to a C struct, as well
;;; as some basic helper procedures.
;;;
;;; Usage:
;;;
;;;   (define-struct-record-type
;;;     RECORD-NAME "STRUCT-NAME-STR"
;;;     pred:    PRED?
;;;     wrap:    WRAPPER
;;;     unwrap:  UNWRAPPER
;;;     (POINTER POINTER-GETTER
;;;              POINTER-SETTER))
;;;
;;; RECORD-NAME is the name to use for the record type. In most cases
;;; it should start with "sdl2:" (even if the procedure names do not),
;;; to avoid name collisions with other libraries. The record type
;;; will be defined by this macro.
;;;
;;; "STRUCT-NAME-STR" is a string giving the name of the C struct that
;;; will be wrapped. It must exactly match the struct name as it
;;; appears in C.
;;;
;;; PRED? is the name to use for the record type predicate procedure,
;;; which returns #t when given an instance of the record type. The
;;; procedure will be defined by this macro.
;;;
;;; WRAPPER is the name to use for a procedure that creates a new
;;; record instance that wraps a pointer. The procedure will be
;;; defined by this macro.
;;;
;;; UNWRAPPER is the name to use for a procedure that will unwrap a
;;; record instance to get its pointer, but also accepts a pointer or
;;; #f. See define-struct-record-unwrapper (below). The procedure will
;;; be defined by this macro.
;;;
;;; POINTER is the slot name to use for the record type's first slot,
;;; which will hold a pointer (or locative). It is usually just called
;;; pointer, but the name doesn't really matter.
;;;
;;; POINTER-GETTER is the name to use for a procedure that will get
;;; the pointer that is wrapped by the record instance (UNWRAPPER is
;;; based on this procedure). The procedure will be defined by this
;;; macro.
;;;
;;; POINTER-SETTER is the name to use for a procedure that will set
;;; the pointer that is wrapped by the record instance. The procedure
;;; will be defined by this macro.
;;;
;;; Example:
;;;
;;;   (define-struct-record-type
;;;     sdl2:surface "SDL_Surface"
;;;     pred:    surface?
;;;     wrap:    wrap-surface
;;;     unwrap:  unwrap-surface
;;;     (pointer %surface-pointer
;;;              %surface-pointer-set!))
;;;
(define-syntax define-struct-record-type
  (syntax-rules (wrap: unwrap: pred:)
    ((define-struct-record-type
       record-name struct-name-str
       pred:    pred?
       wrap:    wrapper
       unwrap:  unwrapper
       (pointer pointer-getter
                pointer-setter!))
     (begin
       (define-record-type record-name
         (wrapper pointer)
         pred?
         (pointer pointer-getter pointer-setter!))
       (define-struct-record-unwrapper unwrapper
         struct-name-str pred? pointer-getter)))))


;;; Defines a procedure that returns a (possibly NULL) struct pointer
;;; when given an instance of the record type, a pointer, or #f.
;;;
;;; - If given an instance of the record type, extracts its pointer or
;;;   locative. (This pointer may be NULL.)
;;; - If given a pointer, locative, or #f, returns it as it is.
;;;   (CHICKEN treats #f as a null pointer.)
;;; - Otherwise, throws an error.
;;;
;;; The procedure is intended to be used with the CHICKEN foreign type
;;; system to auto-convert a Scheme value into a pointer.
(define-syntax define-struct-record-unwrapper
  (syntax-rules ()
    ((define-struct-record-unwrapper unwrapper
       struct-name record-pred? record-pointer)
     (define (unwrapper thing)
       (cond
        ((record-pred? thing)
         (record-pointer thing))
        ((or (pointer? thing) (locative? thing) (not thing))
         thing)
        (else
         (error (format "~S cannot convert ~S to ~A pointer"
                        'unwrapper thing struct-name))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCT MEMORY HELPERS


;;; Defines several procedures to help manage memory for a struct
;;; record (i.e. a record type that wraps a pointer to a C struct).
;;;
;;; Usage:
;;;
;;;   (define-struct-memory-helpers
;;;     "STRUCT-NAME"
;;;     using: (WRAPPER
;;;             RECORD-PRED?
;;;             RECORD-POINTER
;;;             RECORD-POINTER-SET!)
;;;     define: (FREER                  ; can be #f
;;;              ALLOCATOR              ; can be #f
;;;              ALLOCATOR-AUTOFREE))   ; can be #f
;;;
;;; "STRUCT-NAME" must be a string with the name of the C struct.
;;;
;;; The names in the "using:" section are existing procedures related
;;; to the record type. Usually you create these procedures with
;;; define-record-type (from SRFI 9).
;;;
;;; - WRAPPER must be the name of an existing procedure that creates
;;;   an instance of the record type by wrapping a pointer. The
;;;   procedure must take one argument, the pointer to wrap.
;;;
;;; - RECORD-PRED? must be the name of an existing procedure that
;;;   returns #t if given an instance of the record type.
;;;
;;; - RECORD-POINTER must be the name of an existing procedure that
;;;   gets the pointer of an instance of the record type.
;;;
;;; - RECORD-POINTER-SET! must be the name of an existing procedure
;;;   that sets the pointer of an instance of the record type.
;;;
;;; The names in the "define:" section are the names of new procedures
;;; that will be defined by this macro.
;;;
;;; - FREER is the name for a procedure that frees a struct record's
;;;   memory. See define-struct-record-freer (below). This name can be
;;;   #f, in which case the procedure will not be defined.
;;;
;;; - ALLOCATOR is the name for a procedure that allocates a struct
;;;   record that the caller must be manually free. See
;;;   define-struct-record-allocator (below). This name can be #f, in
;;;   which case the procedure will not be defined.
;;;
;;; - ALLOCATOR-AUTOFREE is the name for a procedure that allocates a
;;;   struct record that will free its memory automatically. See
;;;   define-struct-record-allocator-autofree (below). Requires both
;;;   FREER and ALLOCATOR. This name can be #f, in which case the
;;;   procedure will not be defined.
;;;
;;; Example:
;;;
;;;   ;; First, define the struct record type
;;;   (define-struct-record-type
;;;     sdl2:rect "SDL_Rect"
;;;     pred:    rect?
;;;     wrap:    wrap-rect
;;;     unwrap:  unwrap-rect
;;;     (pointer %rect-pointer
;;;              %rect-pointer-set!))
;;;
;;;   ;; Then, define the helpers
;;;   (define-struct-memory-helpers
;;;     "SDL_Rect"
;;;     using: (wrap-rect
;;;             rect?
;;;             %rect-pointer
;;;             %rect-pointer-set!)
;;;     define: (free-rect!
;;;              alloc-rect*
;;;              alloc-rect))
;;;
(define-syntax define-struct-memory-helpers
  (syntax-rules (using: define:)
    ((define-struct-memory-helpers
       struct-name
       using: (wrapper
               record-pred?
               record-pointer
               record-pointer-set!)
       define: (freer
                allocator
                allocator-autofree))
     (begin
       (macro-when freer
        (define-struct-record-freer freer
          record-pointer record-pointer-set!))
       (macro-when allocator
        (define-struct-record-allocator allocator
          struct-name wrapper))
       (macro-when allocator-autofree
        (define-struct-record-allocator-autofree allocator-autofree
          allocator freer))))))


;;; Define a procedure that frees the record's pointer. After freeing,
;;; it sets the record's pointer to the null pointer. Does nothing if
;;; the record's pointer is already the null pointer (so it is safe to
;;; call multiple times). Returns the record no matter what.
(define-syntax define-struct-record-freer
  (syntax-rules ()
    ((define-struct-record-freer freer
       record-pointer record-pointer-set!)
     (define (freer record)
       (unless (struct-null? record)
         (let ((ptr (record-pointer record)))
           (%nullify-struct! record)
           (unless (locative? ptr)
             (free ptr))))
       record))))


;;; Define a procedure that allocates a new struct instance, and
;;; returns a new record that holds a pointer to the new struct. The
;;; caller is responsible for freeing the struct when done with it.
(define-syntax define-struct-record-allocator
  (syntax-rules ()
    ((define-struct-record-allocator allocator
       struct-name wrapper)
     (define (allocator)
       (wrapper (allocate (foreign-type-size struct-name)))))))


;;; Define a procedure that allocates and wraps a new struct instance
;;; (using allocator) and also sets a finalizer so the struct will be
;;; automatically freed (using freer) immediately before the record is
;;; garbage collected.
(define-syntax define-struct-record-allocator-autofree
  (syntax-rules ()
    ((define-struct-record-allocator-autofree allocator-autofree
       allocator freer)
     (define (allocator-autofree)
       (set-finalizer! (allocator) freer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCT FIELD ACCESSORS


;;; Defines accessors (getters and setters) for many fields of a
;;; struct. This macro just calls define-struct-field-accessor many
;;; times. Usage:
;;;
;;;   (define-struct-field-accessors
;;;     STRUCT-POINTER-TYPE
;;;     RECORD-PRED?
;;;     ("FIELD-NAME"
;;;      type:   FIELD-TYPE
;;;      getter: FIELD-GETTER
;;;      setter: FIELD-SETTER      ; optional
;;;      guard:  FIELD-GUARD)      ; optional
;;;      ...)
;;;
;;; See define-struct-field-accessor (below) for more information
;;; about what the arguments mean.
;;;
;;; Example:
;;;
;;;   (define-struct-field-accessors
;;;     SDL_Rect*
;;;     rect?
;;;     ("x"
;;;      type:   Sint32
;;;      getter: rect-x
;;;      setter: rect-x-set!
;;;      guard:  (Sint32-guard "rect field x"))
;;;     ("y"
;;;      type:   Sint32
;;;      getter: rect-y
;;;      setter: rect-y-set!
;;;      guard:  (Sint32-guard "rect field y"))
;;;     ("w"
;;;      type:   Sint32
;;;      getter: rect-w
;;;      setter: rect-w-set!
;;;      guard:  (Sint32-guard "rect field w"))
;;;     ("h"
;;;      type:   Sint32
;;;      getter: rect-h
;;;      setter: rect-h-set!
;;;      guard:  (Sint32-guard "rect field h")))
;;;
(define-syntax define-struct-field-accessors
  (syntax-rules ()
    ((define-struct-field-accessors
       struct-pointer-type
       record-pred?
       (field-name field-key-args ...)
       ...)
     (begin
       (define-struct-field-accessor struct-pointer-type
         record-pred? field-name field-key-args ...)
       ...))))


;;; Defines accessors (a getter and a setter) for a single field of a
;;; struct. Usage:
;;;
;;;   (define-struct-field-accessor
;;;     STRUCT-POINTER-TYPE
;;;     RECORD-PRED?
;;;     "FIELD-NAME"
;;;     type:   FIELD-TYPE
;;;     getter: FIELD-GETTER
;;;     setter: FIELD-SETTER       ; optional
;;;     guard:  FIELD-GUARD)       ; optional
;;;
;;; STRUCT-POINTER-TYPE must be a foreign type specifier for a pointer
;;; to the struct.
;;;
;;; RECORD-PRED? must be the name of an existing procedure that
;;; returns #t for instances of the record type.
;;;
;;; "FIELD-NAME" must be a string giving the exact name of the field,
;;; as it appears in the C struct. You can access nested struct fields
;;; by using "." or "->" (as appropriate) in the field name. E.g.
;;; "foo.bar" would access the bar field of the foo field of this
;;; struct.
;;;
;;; FIELD-TYPE must be a foreign type specifier for the field.
;;;
;;; FIELD-GETTER and FIELD-SETTER are names for the getter and setter
;;; procedures that will be defined by this macro.
;;;
;;; FIELD-GUARD (if specified) must either be an expression that
;;; evaluates to a guard procedure, or #f to have no guard. The guard
;;; procedure will be used by the setter to validate any new value
;;; before it is set. The guard procedure must take one argument (the
;;; value to validate) and return a valid value (perhaps the same
;;; value that was given) or raise an error if the value is invalid.
;;; The guard can also perform conversion, e.g. to change an inexact
;;; integer to an exact one. If you don't care about validation or
;;; conversion, omit the guard clause or specify #f.
;;;
;;; You may omit both the setter and the guard, in which case no
;;; setter will be defined:
;;;
;;;   (define-struct-field-accessor
;;;     STRUCT-POINTER-TYPE
;;;     RECORD-PRED?
;;;     "FIELD-NAME"
;;;     type:   FIELD-TYPE
;;;     getter: FIELD-GETTER)
;;;
;;; If you specify a setter, the getter will be enhanced to work with
;;; generalized `set!` (SRFI 17). So, (set! (point-x my-point) 3) is
;;; equivalent to (point-x-set! my-point 3).
;;;
;;; Example:
;;;
;;;   (define-struct-field-accessor
;;;     SDL_Rect*
;;;     rect?
;;;     "x"
;;;     type:   Sint32
;;;     getter: rect-x
;;;     setter: rect-x-set!
;;;     guard:  (Sint32-guard "rect field x"))
;;;
(define-syntax define-struct-field-accessor
  (syntax-rules (type: getter: setter: guard:)
    ;; only getter
    ((define-struct-field-accessor
       struct-pointer-type
       record-pred?
       field-name
       type:   field-type
       getter: field-getter)
     (define field-getter
       (struct-field-getter struct-pointer-type record-pred?
                            field-type field-name)))

    ;; getter and setter, but no guard. Re-expand with guard: #f.
    ((define-struct-field-accessor
       struct-pointer-type
       record-pred?
       field-name
       type:   field-type
       getter: field-getter
       setter: field-setter)
     (define-struct-field-accessor
       struct-pointer-type
       record-pred?
       field-name
       type:   field-type
       getter: field-getter
       setter: field-setter
       guard:  #f))

    ;; getter, setter, and guard
    ((define-struct-field-accessor
       struct-pointer-type
       record-pred?
       field-name
       type:   field-type
       getter: field-getter
       setter: field-setter
       guard:  field-guard)
     (begin
       (define field-setter
         (struct-field-setter struct-pointer-type record-pred?
                              field-type field-name field-guard))
       (define field-getter
         (struct-field-getter struct-pointer-type record-pred?
                              field-type field-name))
       (set! (setter field-getter) field-setter)))))


;;; Creates (but does not define) a struct field getter.
;;; This is part of define-struct-field-accessor.
(define-syntax struct-field-getter
  (syntax-rules ()
    ((struct-field-getter struct-pointer-type record-pred?
                          field-type field-name)
     (let ((foreign-getter
            (foreign-lambda*-with-dynamic-body
             field-type ((struct-pointer-type obj))
             ("C_return(obj->~A);" field-name))))
       (lambda (record)
         (assert (record-pred? record))
         (foreign-getter record))))))


;;; Creates (but does not define) a struct field setter.
;;; This is part of define-struct-field-accessor.
(define-syntax struct-field-setter
  (syntax-rules ()
    ((struct-field-setter struct-pointer-type record-pred?
                          field-type field-name field-guard)
     (let ((guard field-guard)
           (foreign-setter
            (foreign-lambda*-with-dynamic-body
             void ((struct-pointer-type obj) (field-type val))
             ("obj->~A = val;" field-name))))
       (lambda (record value)
         (assert (record-pred? record))
         (macro-if field-guard
                   (foreign-setter record (guard value))
                   (foreign-setter record value)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RECORD PRINTER


;;; Defines a record printer for the given record type, using a
;;; standard format for chicken-sdl2 struct record types. The record
;;; printer determines how record instances are printed, for example
;;; in the REPL or when debugging.
;;;
;;; The format for printing is:
;;;
;;;   #<record-type address field-label: field-value ...>
;;;
;;; Record instances with a null pointer are printed like:
;;;
;;;   #<record-type NULL>
;;;
;;; Usage:
;;;
;;;   (define-struct-record-printer RECORD-TYPE
;;;     RECORD-POINTER
;;;     show-address: SHOW-ADDRESS?
;;;     (FIELD-LABEL FIELD-GETTER)       ; optional
;;;     ...)
;;;
;;; RECORD-TYPE must be the name of the record type. It must be
;;; exactly the same as was used with define-record-type.
;;;
;;; RECORD-POINTER must be the name of the procedure to get a record
;;; instance's pointer.
;;;
;;; SHOW-ADDRESS? must be #t or #f. It controls whether the struct's
;;; pointer address is shown (#t) or not shown (#f). This option does
;;; not affect whether "NULL" is shown when the pointer is null.
;;; (There is currently no way to turn that off.)
;;;
;;; FIELD-LABEL is a symbol or string specifying the label to show for
;;; a field value. Usually it is the name of a struct field, but it
;;; does not have to be. Alternatively, FIELD-LABEL can be #f, in
;;; which case the field value will be printed with no label.
;;;
;;; FIELD-GETTER is the name of an existing procedure that returns a
;;; value when given the record instance. Usually it is a field
;;; getter, but it could be a more complex procedure. The value
;;; returned by the procedure will be printed using write (i.e.
;;; strings will be printed in double quotes).
;;;
;;; Examples:
;;;
;;;   ;; Prints like: #<sdl2:surface 0x7fe369503670 w: 100 h: 200>
;;;   (define-struct-record-printer sdl2:surface
;;;     %surface-pointer
;;;     show-address: #t
;;;     (w surface-w)
;;;     (h surface-h))
;;;
;;;   ;; Prints like: #<sdl2:rect (1 2 3 4)>
;;;   (define-struct-record-printer sdl2:rect
;;;     %rect-pointer
;;;     show-address: #f
;;;     (#f rect->list))
;;;
(define-syntax define-struct-record-printer
  (syntax-rules (show-address:)
    ((define-struct-record-printer record-type
       record-pointer
       show-address: show-address?
       (field-label field-getter)
       ...)
     (define-record-printer (record-type record out)
       (display (sprintf "#<~A" 'record-type) out)
       (let ((address (pointer->address (record-pointer record))))
         (if (= 0 address)
             (display " NULL" out)
             (begin
               (macro-when show-address?
                 (display (sprintf " 0x~X" address) out))
               (begin
                 (macro-when field-label
                   (display (sprintf " ~A:" 'field-label) out))
                 (display (sprintf " ~S" (field-getter record)) out))
               ...)))
       (display ">" out)))))
