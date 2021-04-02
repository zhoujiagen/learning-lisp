(load "../lib/out.ss")

; continuation passing style

(letrec ([f (lambda (x) (cons 'a x))]
        [g (lambda (x) (cons 'b (f x)))]
        [h (lambda (x) (g (cons 'c x)))])
    (displayln (cons 'd (h '())))) ; (d b a c)

(letrec ([f (lambda (x k) (k (cons 'a x)))]
        [g (lambda (x k)
            (f x (lambda (v) (k (cons 'b v)))))]
        [h (lambda (x k) (g (cons 'c x) k))])
    (displayln (h '() (lambda (v) (cons 'd v))))) ; (d b a c)
