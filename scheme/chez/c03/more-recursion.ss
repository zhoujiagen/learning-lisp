(load "../lib/tests.ss")

; ERROR: Exception: variable sum is not bound
; (let ([sum (lambda (lst)
;                 (if (null? lst)
;                     0
;                     (+ (car lst) (sum (cdr lst)))))])
;     (sum '(1 2 3 4 5)))

; pass itself
(let ([sum (lambda (sum lst)
                (if (null? lst)
                    0
                    (+ (car lst) (sum sum (cdr lst)))))])
    (display (sum sum '(1 2 3 4 5)))
    (newline))

;;; (letrec ((var expr) ...) body1 body2 ...)
;;; var ... are visible in body and expr ...
(letrec ([sum (lambda (lst)
                (if (null? lst)
                    0
                    (+ (car lst) (sum (cdr lst)))))])
    (display (sum '(1 2 3 4 5)))
    (newline))

; mutablly recursive procedures
(letrec ([even?
            (lambda (x)
                ;(begin (display "DEBUG: even? ") (display x) (newline))
                (or (= x 0)
                    (odd? (- x 1))))]
        [odd?
            (lambda (x)
                ;(begin (display "DEBUG: odd? ") (display x) (newline))
                (and (not (= x 0))
                    (even? (- x 1))))])
    (display (list (even? 20) (odd? 20)))
    (newline))

;;; named let
;;;(let name ((var expr) ...)
;;; body1 body2 ...)
(define list2?
    (lambda (x)
        (letrec ([race
                (lambda (h t)
                    (if (pair? h)
                        (let ([h (cdr h)])
                            (if (pair? h)
                                (and (not (eq? h t))
                                    (race (cdr h) (cdr t)))
                                (null? h)))
                        (null? h)))])
        (race x x))))

(run-test list2? '(1 2 3 4))


(define list3?
    (lambda (x)
        (let race ([h x] [t x])
            (if (pair? h)
                (let ([h (cdr h)])
                    (if (pair? h)
                        (and (not (eq? h t))
                            (race (cdr h) (cdr t)))
                        (null? h)))
                (null? h)))))

(run-test list3? '(1 2 3 4))

;;; recursive version(-r) v.s. iteration version(-i)

(define (factorial-r n)
    ;(let fact ([i n])
    (trace-let fact ([i n])
        (if (= i 0)
            1
            (* i (fact (- i 1))))))

(define (factorial-i n)
    ;(let fact ([i n] [acc 1])
    (trace-let fact ([i n] [acc 1])
        (if (= i 0)
            acc
            (fact (- i 1) (* i acc)))))

(define (fibonacci-r n)
    (let fib ([i n])
    ;(trace-let fib ([i n])
        (cond 
            [(= i 0) 0]
            [(= i 1) 1]
            [else (+ (fib (- i 1)) (fib (- i 2)))])))

(define (fibonacci-i n)
    (if (= n 0)
        0
        (let fib ([i n] [cur 1] [pre 0])
        ;(trace-let fib ([i n] [cur 1] [pre 0])
            (if (= i 1)
                cur
                (fib (- i 1) (+ cur pre) cur)))))

; trace and test
(let ([n 10])
    (run-test factorial-r n)
    (run-test factorial-i n)
    (run-test fibonacci-r n)
    (run-test fibonacci-i n))

;;; factor
(define (factor n)
    (let f ([n n] [fact 2])
        (cond 
            [(>= fact n) (list n)]
            [(integer? (/ n fact)) (cons fact (f (/ n fact) fact))]
            [else (f n (+ fact 1))])))

(run-tests factor '(0 1 12 3628800))
