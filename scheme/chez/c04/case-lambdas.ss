(load "../lib/out.ss")

(define make-list
    (case-lambda
        [(n) (make-list n #f)]
        [(n x) 
         (do ([n n (- n 1)] [ls '() (cons x ls)])
            ((zero? n) ls))]))

(let ()
    (displayln (make-list 2))
    (displayln (make-list 2 0)))


(define substring2
    (case-lambda
        [(s) (substring2 s 0 (string-length s))]
        [(s start end) (substring s start end)]))

(let ()
    (displayln (substring2 "aaa"))
    (displayln (substring2 "aaa" 1 2)))