;;; use bracket

(let ([x +] [a 2] [b 3])
    (display (x a b))
    (newline))

(let ([double-cons (lambda (x) (cons x x))])
    (display (double-cons 'a))
    (newline))

(let ([double-any (lambda (f x) (f x x))])
    (display
        (list (double-any + 13)
            (double-any cons 'a)))
    (newline))