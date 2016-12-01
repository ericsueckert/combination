;CSE 413 AU
;Perkins
;Eric Eckert
;Hw 4


#lang racket
(provide take)
(provide combm)
(provide red-blue)

;stream red-blue function pair
(define red-blue (lambda () (cons "red" blue-red)))
(define blue-red (lambda () (cons "blue" red-blue)))

;takes the first n elements of a stream
(define (take st n)
  (if (= n 0)
      '()
      (cons (car (st)) (take (cdr (st)) (- n 1)))))

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