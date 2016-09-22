#lang racket
(provide (all-defined-out))

(define program
  '(letp ((addone (x)) (+ x 1)) (apply (addone (1)))))

(define program2
  '(letp ((add3var (x y z)) (+ x (+ y z))) (apply (add3var (x y z)))))