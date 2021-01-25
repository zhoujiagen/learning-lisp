(load "lib/tests.ss")

(define abs
    (lambda (x)
        (if (< x 0)
            (- 0 x)
            x)))

(run-tests abs '(-2 -1 0 1 2))