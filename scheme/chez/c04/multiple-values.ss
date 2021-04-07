(load "../lib/out.ss")


(let ()
    (displayln (let-values ([(a b) (values 1 2)]
                            [c (values 1 2 3)])
                    (list a b c))) ; (1 2 (1 2 3))
    (displayln (let*-values ([(a b) (values 1 2)]
                            [(a b) (values b a)])
                    (list a b)))) ; (2 1)
    