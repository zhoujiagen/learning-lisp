(load "lib/tests.ss")
(load "lib/grades.ss")

(import (grades))

(let ()
    (display (gpa c a c b b))
    (newline)
    (display (gpa->grade 2.8))
    (newline))
