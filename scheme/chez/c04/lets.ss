(load "../lib/out.ss")

(let ([x (* 3.0 3.0)]
        [y (* 4.0 4.0)])
    (displayln (sqrt (+ x y)))) ; 5.0

(let* ([x (* 5.0 5.0)]
        [y (- x (* 4.0 4.0))])
    (displayln (sqrt y))); 3.0

(letrec ([sum (lambda (x)
                (if (zero? x)
                    0
                    (+ x (sum (- x 1)))))])
    (displayln (sum 5))) ; 15 

(letrec* ([sum (lambda (x)
                (if (zero? x)
                    0
                    (+ x (sum (- x 1)))))]
            [f (lambda () (cons n n-sum))]
            [n 15]
            [n-sum (sum n)])
    (displayln (f))) ; (15 . 120)