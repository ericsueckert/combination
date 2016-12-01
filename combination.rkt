;Eric Eckert
#lang racket

; calculates the combination of n and k using a memo to store previously calculated value
(define combm
  (letrec([memo null]
          [fact (lambda (n)
                  (if (= n 1)
                      1
                      (* n (fact (- n 1)))))]
          [f (lambda (n k);start of recursive function
               (let ([ans (assoc (list n k) memo)]); store input in (n k) list
                 (if ans ;check if answer was in the table
                     (cdr ans);if so return the answer
                     (let ([new-ans (/ (fact n) (* (fact k) (fact (- n k))))]); calculate value
                       (begin 
                         (set! memo (cons (cons (list n k) new-ans) memo));set newly calulated answer
                         new-ans)))))])
    f))
