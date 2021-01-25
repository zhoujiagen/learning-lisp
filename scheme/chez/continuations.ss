;;; Continuation
;;; keep track things when expression evaluation:
;;; (1) what to evaluate
;;; (2) what to do with the value - the continuation of a computation

(load "lib/tests.ss")

; call/cc
; must be passed a procedure p of 1 argument
; construct a concrete representation of the current continuation and pass it to p as k
; (the current continuation is represented by procedure k)
; (1) each time k is applied to a value, it return the value to the continuation of the call/cc application
; (2) if p returns without invoking k, the value returned by the procedure p becomes the value of application of call/cc
(let ([p (lambda (k) (* 5 4))]
    [p2 (lambda (k) (* 5 (k 4)))])
    (run-test call/cc p)
    (run-test call/cc p2)
    (run-test + 2 (call/cc p2)))

; example: nonlocal exit from a recursion
(define (product lst)
    (call/cc 
        (lambda (break)
            (let f ([lst lst])
                (cond
                    [(null? lst) 1]
                    [(= (car lst) 0) (break 0)] ; invoke continuation
                    [else (* (car lst) (f (cdr lst)))])))))

(run-tests product (list '(1 2 3 4 5) '(1 2 3 0 5)))


(let ([x (call/cc (lambda (k) k))])
    (display x) ; #<continuation>
    (newline)
    (display (x (lambda (ignore) "hi")))
    (newline))


; (((call/cc (lambda (k) k)) (lambda (x) x))
;     "HEY!")
(run-test ((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!")

(define retry #f)
(define factorial
    (lambda (x)
        (if (= x 0)
            (call/cc (lambda (k) (set! retry k) 1))
            (* x (factorial (- x 1))))))

; > (factorial 4)
; 24
; > (retry 1)
; 24
; > (retry 2)
; 48

;;; lwp: light-weight process

(define lwp-list '())

(define (lwp thunk)
    (set! lwp-list (append lwp-list (list thunk))))

(define (start)
    (let ([p (car lwp-list)])
        (set! lwp-list (cdr lwp-list))
        (p)))
    
(define (pause)
    (call/cc 
        (lambda (k)
            (lwp (lambda () (k #f)))
            (start))))

(let ()
    (lwp (lambda () (let f () (pause) (display "h") (f))))
    (lwp (lambda () (let f () (pause) (display "e") (f))))
    (lwp (lambda () (let f () (pause) (display "y") (f))))
    (lwp (lambda () (let f () (pause) (display "!") (f))))
    (lwp (lambda () (let f () (pause) (newline) (f))))
    (start))