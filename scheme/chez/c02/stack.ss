;;; Stack

(load "../lib/tests.ss")

(define make-stack
    (lambda ()
        ; data
        (let ([lst '()])
            ; method
            (lambda (msg . args)
                (cond 
                    [(eqv? msg 'empty?) (null? lst)]
                    [(eqv? msg 'push!) (set! lst (cons (car args) lst))]
                    [(eqv? msg 'top) (car lst)]
                    [(eqv? msg 'pop!) 
                        (let ([r (car lst)]) 
                            (set! lst (cdr lst))
                            r)]
                    [else "oops"])))))

(define stack1 (make-stack))
(define stack2 (make-stack))

(let ([stack1 (make-stack)]
    [stack2 (make-stack)])
    (run-test stack1 'empty?)
    (run-test stack2 'empty?)
    (run-test stack1 'push! 'b)
    (run-test stack2 'push! 'c)
    (run-test stack1 'top)
    (run-test stack2 'top)
    (run-test stack1 'pop!)
    (run-test stack1 'empty?))