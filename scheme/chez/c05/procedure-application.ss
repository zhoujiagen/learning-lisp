(load "../lib/out.ss")

(define first
    (lambda (lst)
        (apply (lambda (x . y) x) lst)))

(define (rest lst)
    (apply (lambda (x . y) y) lst))

(let ([x '(1 2 3 4)])
    (displayln (first x))   ; 1
    (displayln (rest x)))   ; (2 3 4)
