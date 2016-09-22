#lang racket
(provide (all-defined-out))

(define myprogram
  '(+ 4
      ((gt (+ 3 (+ 4 5))
           ( (lt (+ 2 2) (+ 2 1))
             5
             6
             )
           )
       7
       8
       )
      )
  )

;; Test for eval
(define prog1
  '(let (x 1)
        (let (y (+ x 1))
    	     (+ x y)
        )
   )
)

;;Another test for Eval
(define prog2
  '(let (x 5)
     (let (y 10)
       (let (z ((gt x y) 1 0))
         (+ y y)))))

;;Another test for Eval
(define prog3
  '(let (x 5)
     (let (y (+ x 5))
       (let (x 100)
         (+ x y)))))

;;Yet another test for Eval
(define prog4
  '(let (x 1)
     (let (y (+ x 1))
       (let (x (+ y 2))
         x))))

(define prog5
  '(let (x 1)
     (+ x y)))

;Test for eval-new
(define prog-new
  '(let ((x 1) (y (+ x 1)))
     (+ x y)))


