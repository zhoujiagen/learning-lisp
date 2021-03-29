;;; thunk: zero-argument procedure

(load "../lib/tests.ss")

(define lazy
    ; t: thunk
    (lambda (t)
    ;(trace-lambda lazy (t)
        (let ([val #f] [flag #f])
            (lambda ()
                (if (not flag)
                    (begin 
                        (set! val (t))  ; application of thunk
                        (set! flag #t)))
                val))))

(define p
    (lazy (lambda ()
            (display "Ouch!")
            (newline)
            "got me")))

(run-test p)
(run-test p) ; call again