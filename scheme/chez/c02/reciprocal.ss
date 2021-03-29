(load "../lib/tests.ss")

(define reciprocal
    (lambda (n)
        (if (= n 0)
            "oops"
            (/ 1 n))))

(run-tests reciprocal '(-2 -1 0 1 2))