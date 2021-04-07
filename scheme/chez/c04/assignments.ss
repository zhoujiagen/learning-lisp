(load "../lib/out.ss")

(define flip-flop
    (let ([state #f]) ; internal state
        (lambda ()
            (set! state (not state))
            state)))

(let ()
    (displayln (flip-flop))
    (displayln (flip-flop))
    (displayln (flip-flop)))

(define memoize
    (lambda (proc)
        (let ([cache '()]) ; cache of calculation
            (lambda (x)
                (cond
                    [(assq x cache) => cdr]
                    [else 
                        (let ([ans (proc x)])
                            (set! cache (cons (cons x ans) cache))
                            ans)])))))
(let ()
    (define fibonacci
        (memoize 
            (lambda (n)
                (if (< n 2)
                    1
                    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))
    (displayln (fibonacci 100)))