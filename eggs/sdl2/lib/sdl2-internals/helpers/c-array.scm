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


;;; Define a procedure that allocates a block of memory large enough
;;; to hold items of a certain type. The resulting procedure's
;;; signature will be:
;;;
;;;   (NAME #!optional (n 1))
;;;
;;; It allocates enough memory to hold n items of the specified type,
;;; and returns a pointer to that memory. The memory must be manually
;;; freed (e.g. using free) when you are done with it.
;;;
;;; Usage:
;;;
;;;   (define-allocator NAME TYPE)
;;;
;;; NAME is the name of the procedure to define.
;;;
;;; TYPE is either a foreign type specifier, or a string containing
;;; the name of a C type.
;;;
;;; Examples:
;;;
;;;   (define-allocator %allocate-Sint32 Sint32)
;;;   (define-allocator %allocate-event-array "SDL_Event")
;;;
(define-syntax define-allocator
  (syntax-rules ()
    ((define-allocator name type)
     (define name
       (let ((size (foreign-type-size type)))
         (lambda (#!optional (n 1))
           (allocate (* size n))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C ARRAY READER / WRITER


;;; Defines a procedure which reads a single item from a C array, by
;;; copying its value into the given pointer. The resulting
;;; procedure's signature will be:
;;;
;;;   (NAME item-out array-pointer i)
;;;
;;; It copies the value in the i'th position of array-pointer into
;;; item-out. It has no return value.
;;;
;;; Usage:
;;;
;;;   (define-array-reader NAME
;;;     ARRAY-TYPE-STRING ITEM-TYPE)
;;;
;;; NAME is the name of the procedure to define.
;;;
;;; ARRAY-TYPE-STRING is a string containing a C code snippet
;;; specifying the pointer type of the C array.
;;;
;;; ITEM-TYPE is a foreign type specifier for the items that will be
;;; returned.
;;;
;;; Example:
;;;
;;;   (define-array-reader %read-event-array
;;;     "SDL_Event*" SDL_Event*)
;;;
(define-syntax define-array-reader
  (syntax-rules ()
    ((define-array-reader name
       array-type-string item-type)
     (define name
       (foreign-lambda*-with-dynamic-body
        void ((item-type out) (c-pointer array) (int i))
        ("*out = ((~A)array)[i];" array-type-string))))))


;;; Defines a procedure which writes a single item to a C array, by
;;; copying its value from the given pointer. The resulting
;;; procedure's signature will be:
;;;
;;;   (NAME item array-pointer i)
;;;
;;; It copies the value of item to the i'th position of array-pointer.
;;; It has no return value.
;;;
;;; Usage:
;;;
;;;   (define-array-writer NAME
;;;     ARRAY-TYPE-STRING ITEM-TYPE)
;;;
;;; NAME is the name of the procedure to define.
;;;
;;; ARRAY-TYPE-STRING is a string containing a C code snippet
;;; specifying the pointer type of the C array.
;;;
;;; ITEM-TYPE is a foreign type specifier for the items that will be
;;; returned.
;;;
;;; Example:
;;;
;;;   (define-array-writer %write-event-array
;;;     "SDL_Event*" SDL_Event*)
;;;
(define-syntax define-array-writer
  (syntax-rules ()
    ((define-array-writer name
       array-type-string item-type)
     (define name
       (foreign-lambda*-with-dynamic-body
        void ((item-type in) (c-pointer array) (int i))
        ("((~A)array)[i] = *in;" array-type-string))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C ARRAY <-> SCHEME LIST

;;; Defines a procedure which takes a pointer to a C array and a
;;; number N, and returns a Scheme list containing copies of the first
;;; N items in the C array. Bad things may occur if N exceeds the size
;;; of the C array, so be careful!
;;;
;;; Usage:
;;;
;;;   (define-array->list NAME
;;;     ALLOC-ITEM-EXPR ARRAY-READER)
;;;
;;; NAME is the name of the procedure to be defined.
;;;
;;; ARRAY-READER is the name of an existing procedure that reads an
;;; item from a C array. Usually ARRAY-READER is defined using
;;; define-array-reader.
;;;
;;; ALLOC-ITEM-EXPR is a Scheme expression used to allocate a new
;;; item. The allocated item's memory will be overwritten by a value
;;; from the array.
;;;
;;; Example:
;;;
;;;   (define-array->list %event-array->list
;;;     (alloc-event) %read-event-array)
;;;
(define-syntax define-array->list
  (syntax-rules ()
    ((define-array->list name
       array-reader
       alloc-item-expr)
     (define (name array-ptr n)
       (list-tabulate n
         (lambda (i)
           (let ((item alloc-item-expr))
             (array-reader item array-ptr i)
             item)))))))


;;; Defines a procedure which takes a Scheme list of items, and
;;; allocates and returns a pointer to a C array containing copies of
;;; the items in the list.
;;;
;;; Usage:
;;;
;;;   (define-list->array NAME
;;;     ARRAY-ALLOCATOR ARRAY-WRITER)
;;;
;;; NAME is the name of the procedure to be defined.
;;;
;;; ARRAY-ALLOCATOR is the name of a procedure which accepts a number
;;; N, and allocates and returns a pointer to enough memory to hold N
;;; items of the correct type. Usually ARRAY-ALLOCATOR is defined
;;; using define-allocator.
;;;
;;; ARRAY-WRITER is the name of an existing procedure that writes an
;;; item to a C array. Usually ARRAY-WRITER is defined using
;;; define-array-writer.
;;;
;;; Example:
;;;
;;;   (define-list->array %event-list->array
;;;     %allocate-event-array %write-event-array)
;;;
(define-syntax define-list->array
  (syntax-rules ()
    ((define-list->array name
       array-allocator array-writer)
     (define (name items)
       (let* ((n (length items))
              (array-ptr (array-allocator n)))
         (do ((items items (cdr items))
              (i 0 (+ i 1)))
             ((null? items))
           (array-writer (car items) array-ptr i))
         array-ptr)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C ARRAY <-> SCHEME VECTOR

;;; Defines a procedure which takes a pointer to a C array and a
;;; number N, and returns a Scheme vector containing copies of the
;;; first N items in the C array. Bad things may occur if N exceeds
;;; the size of the C array, so be careful!
;;;
;;; Usage:
;;;
;;;   (define-array->vector NAME
;;;     ALLOC-ITEM-EXPR ARRAY-READER)
;;;
;;; NAME is the name of the procedure to be defined.
;;;
;;; ARRAY-READER is the name of an existing procedure that reads an
;;; item from a C array. Usually ARRAY-READER is defined using
;;; define-array-reader.
;;;
;;; ALLOC-ITEM-EXPR is a Scheme expression used to allocate a new
;;; item. The allocated item's memory will be overwritten by a value
;;; from the array.
;;;
;;; Example:
;;;
;;;   (define-array->vector %event-array->vector
;;;     (alloc-event) %read-event-array)
;;;
(define-syntax define-array->vector
  (syntax-rules ()
    ((define-array->vector name
       array-reader
       alloc-item-expr)
     (define (name array-ptr n)
       (do ((items-vec (make-vector n))
            (i 0 (+ i 1)))
           ((= i n) items-vec)
         (let ((item alloc-item-expr))
           (array-reader item array-ptr i)
           (vector-set! items-vec i item)))))))


;;; Defines a procedure which takes a Scheme vector of items, and
;;; allocates and returns a pointer to a C array containing copies of
;;; the items in the vector.
;;;
;;; Usage:
;;;
;;;   (define-vector->array NAME
;;;     ARRAY-ALLOCATOR ARRAY-WRITER)
;;;
;;; NAME is the name of the procedure to be defined.
;;;
;;; ARRAY-ALLOCATOR is the name of a procedure which accepts a number
;;; N, and allocates and returns a pointer to enough memory to hold N
;;; items of the correct type. Usually ARRAY-ALLOCATOR is defined
;;; using define-allocator.
;;;
;;; ARRAY-WRITER is the name of an existing procedure that writes an
;;; item to a C array. Usually ARRAY-WRITER is defined using
;;; define-array-writer.
;;;
;;; Example:
;;;
;;;   (define-vector->array %event-vector->array
;;;     %allocate-event-array %write-event-array)
;;;
(define-syntax define-vector->array
  (syntax-rules ()
    ((define-vector->array name
       array-allocator array-writer)
     (define (name items-vec)
       (let* ((n (vector-length items-vec))
              (array-ptr (array-allocator n)))
         (do ((i 0 (+ i 1)))
             ((= i n) array-ptr)
           (array-writer (vector-ref items-vec i) array-ptr i)))))))



;;; Defines a procedure which takes a Scheme list or vector of items,
;;; and allocates and returns a pointer to a C array containing copies
;;; of the items. Basically, this macro combines procedures defined
;;; using define-list->array and define-vector->array.
;;;
;;; Usage:
;;;
;;;   (define-items->array NAME
;;;     LIST->ARRAY
;;;     VECTOR->ARRAY)
;;;
;;; NAME is the name of the procedure to be defined.
;;;
;;; LIST->ARRAY is the name of an existing procedure which takes a
;;; Scheme list of items and returns a pointer to a C array. Usually
;;; the procedure is defined using define-list->array, above.
;;;
;;; VECTOR->ARRAY is the name of an existing procedure which takes a
;;; Scheme vector of items and returns a pointer to a C array. Usually
;;; the procedure is defined using define-vector->array, above.
;;;
;;; Example:
;;;
;;;   (define-items->array %points->array
;;;     %point-list->array
;;;     %point-vector->array)
;;;
(define-syntax define-items->array
  (syntax-rules ()
    ((define-items->array name
       thing-list->array
       thing-vector->array)
     (define (name things #!optional fn-name)
       (cond
        ((list? things)
         (thing-list->array things))
        ((vector? things)
         (thing-vector->array things))
        (else
         (error fn-name "Cannot create C array from" things)))))))
