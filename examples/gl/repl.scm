
(use nrepl srfi-18)

;;(define MUTEX (make-mutex))

(nrepl 1234
       eval:
       (lambda (x)
         (dynamic-wind
           (lambda () (mutex-lock! MUTEX))
           (lambda () (eval x))
           (lambda () (mutex-unlock! MUTEX)))))



(print "heisann")

(define (foo x)
  (+ x 1))


(foo 4)
