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
;;; MACROS


;;; define-event-type is used to define several functions related
;;; to an SDL event type (or group of related types).
;;;
;;; The functions defined are:
;;;
;;; - A predicate function that returns true for instances of the
;;;   event. (See define-event-type-pred.)
;;; - A record type printer for printing instances of the event.
;;;   (See register-event-type-printer.)
;;; - Getters and setters for each field of the event struct.
;;;   (See define-struct-field-accessor.)
;;;
;;; Some event types are related, e.g. SDL_KEYDOWN and SDL_KEYUP. They
;;; use the same C struct (and thus have the same fields), but the
;;; event type indicates some semantic difference. In such cases,
;;; define-event-type is called with multiple type-ids, and the
;;; predicate returns true for an event matching any of the type-ids.
;;;
;;; print: specifies what fields are shown in the record printer.
;;; The list can be empty, in which case the record printer will show
;;; only the event type.
;;;
(define-syntax define-event-type
  (syntax-rules (types: pred: print: type: getter: setter: guard:)
    ((define-event-type struct-name
       types: (type-id ...)
       pred:  pred?
       print: ((print-field print-field-getter) ...)
       (field-name field-options ...)
       ...)
     (begin
       (define-event-type-pred pred? (type-id ...))

       (register-event-type-printer
        (type-id ...)
        (print-field print-field-getter) ...)

       (define-struct-field-accessor
         SDL_Event*
         pred?
         field-name
         field-options ...)
       ...))))



;;; Defines a type predicate for a particular kind of event. The
;;; predicate function returns true when given an sdl2:event with any
;;; of the matching type IDs, otherwise returns false.
;;;
;;; This macro is part of define-event-type, and is usually not
;;; called directly.
(define-syntax define-event-type-pred
  (syntax-rules ()
    ((define-event-type-pred pred? (type-id ...))
     (define (pred? event)
       (and (event? event)
            (or (eq? type-id (event-type-raw event))
                ...))))))


;;; Registers a record printer for a particular kind of event. The
;;; record printer is a function that prints an object as text, e.g.
;;; "#<sdl2:event ...>". See the record printer functions below for
;;; more info.
;;;
;;; This macro is part of define-event-type, and is usually not
;;; called directly.
(define-syntax register-event-type-printer
  (syntax-rules ()
    ((register-event-type-printer
      (type-id ...)
      (print-field print-field-getter) ...)
     (let ((printer (lambda (event out)
                      (print-event
                       event out
                       (list (list 'print-field
                                   (print-field-getter event))
                             ...)))))
       (hash-table-set! event-printers type-id printer)
       ...))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RECORD PRINTER

;;; The record printer for the sdl2:event record type is unusual, since
;;; there are actually many different event structs, each with
;;; different fields. So, the sdl2:event record printer actually
;;; performs a type dispatch, to look up the printer function
;;; registered for the particular event type.

;;; The actual record printer for the sdl2:event record type.
(define-record-printer (sdl2:event event out)
  (if (struct-null? event)
      (display "#<sdl2:event NULL>" out)
      (let ((printer (hash-table-ref/default
                      event-printers
                      (event-type-raw event)
                      default-event-printer)))
        (printer event out))))

;;; Hash table holding the printers for each event type.
(define event-printers
  (make-hash-table
   test: = hash: number-hash size: 40))

;;; The default printer, in case there is no registered printer for an
;;; event type. It prints with no fields.
(define (default-event-printer event out)
  (print-event event out '()))

;;; The main logic for printing an event. fields must be a list of
;;; (field-name field-value) lists. It prints out like:
;;;
;;;   #<sdl2:event EVENT_TYPE field-name: field-value ...>
;;;
(define (print-event event out fields)
  (display "#<sdl2:event " out)
  (display (event-type event) out)
  (for-each (lambda (field)
              (display (sprintf " ~A: ~S" (car field) (cadr field))
                       out))
            fields)
  (display ">" out))
