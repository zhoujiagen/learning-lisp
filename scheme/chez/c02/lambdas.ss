; (lambda (var ...) body1 body2 ...)
;
; (var ...) can be
; (1) a proper list of variables: (var1 ... varn)
; (2) a single varaible: varr;  rest
; (3) an improper list of variables: (var1 ... varn . varr); rest

; (1)
(let ([f (lambda (x y) (list x y))])
    (display (f 1 2))
    (newline))

; (2)
(let ([f (lambda x x)])
    (display (f 1 2 3 4))
    (newline))

; (3)
(let ([f (lambda (x . y) (list x y))])
    (display (f 1 2 3 4))
    (newline))
