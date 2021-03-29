;;; Assignments

(load "../lib/tests.ss")

;;; root of ax^2 + bx + c = 0
(define quadratic-formula
    (lambda (a b c)
        (let ([root1 0] [root2 0] [minusb 0] [radical 0] [divisor 0])
            (set! minusb (- 0 b))
            (set! radical (sqrt (- (* b b) (* 4 (* a c)))))
            (set! divisor (* 2 a))
            (set! root1 (/ (+ minusb radical) divisor))
            (set! root2 (/ (- minusb radical) divisor))
            (cons root1 root2))))

(run-test quadratic-formula 2 -4 6)

(define quadratic-formula2
    (lambda (a b c)
        (let ([minusb (- 0 b)]
            [radical (sqrt (- (* b b) (* 4 (* a c))))]
            [divisor (* 2 a)])
            (let ([root1 (/ (+ minusb radical) divisor)]
                [root2 (/ (- minusb radical) divisor)])
                (cons root1 root2)))))

(run-test quadratic-formula2 2 -4 6)

;;; Use assignments to implement procedures that must maintain some internal state.

(define next 0)
(define count
    (lambda ()
        (let ([v next])
            (set! next (+ next 1))
            v)))

(run-test count)
(run-test count)

; let-binding next
(define count2
    (let ([next 0])
        (lambda ()
            (let ([v next])
                (set! next (+ next 1))
                v))))

(run-test count2)
(run-test count2)        