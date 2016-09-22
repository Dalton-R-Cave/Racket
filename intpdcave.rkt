#lang racket
(require "program.rkt")
(provide (all-defined-out))

;length
;takes in a list and returns its length
(define length
  (lambda (lst)
    (cond
      [(null? lst) 0]
      [(not (list? lst)) 1]
      [else (+ 1 (length (cdr lst)))])))

;BCond
;takes in a snippet and return true if it is a B Condition
(define BCond
  (lambda (prog)
    (cond
      [(not(equal? (length prog) 3)) #f]
      [(and (and (equal? 'gt (car prog)) (Expr (cadr prog))) (Expr (cadr (cdr prog)))) #t]
      [(and (and (equal? 'lt (car prog)) (Expr (cadr prog))) (Expr (cadr (cdr prog)))) #t]
      [(and (and (equal? 'eq (car prog)) (Expr (cadr prog))) (Expr (cadr (cdr prog)))) #t]
      [else #f])))

;EvalBCond
;takes in a snippet and evaluates it if it as a BCond.  Returns false if it is not a BCond.
(define EvalBCond
  (lambda (prog)
    (cond
      [(not(equal? (length prog) 3)) #f]
      [(equal? 'lt (car prog)) (< (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [(equal? 'gt (car prog)) (> (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [(equal? 'eq (car prog)) (equal? (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [else #f])))

;CCond
;takes in a snippet and return true if it is a C condition
(define CCond
  (lambda (prog)
    (cond
      [(<(length prog) 1) '#f]
      [(BCond prog) #t]
      [(<(length prog) 2) '#f]
      [(and (equal? 'not (car prog)) (CCond (cadr prog))) #t]
      [(not(equal?(length prog) 3)) '#f]
      [(and (and (equal? 'or (car prog)) (CCond (cadr prog))) (CCond (cadr (cdr prog)))) #t]
      [(and (and (equal? 'and (car prog)) (CCond (cadr prog))) (CCond (cadr (cdr prog)))) #t]
      [else #f])))

;Op
;Takes in a snippet and return true if it is an operator
(define Op
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(equal? '+ prog) #t]
      [(equal? '- prog) #t]
      [(equal? '* prog) #t]
      [(equal? '/ prog) #t]
      [else #f])))

;CondExpr
;Takes in a snippet and return true if it is a Conditional Expression
(define CondExpr
  (lambda (prog)
    (cond
      [(not(equal? (length prog) 3)) #f]
      [(and (and (CCond (car prog)) (Expr (cadr prog))) (Expr (cadr (cdr prog)))) #t]
      [else #f])))

;EvalCondExpr
;Takes in a snippet and evaluates it as a Conditinal Expression
(define EvalCondExpr
  (lambda (prog)
    (cond
      [(not(equal? (length prog) 3)) #f]
      [(not (CondExpr prog)) #f]
      [(cond
        [(EvalCCond (car prog)) (EvalExpr (cadr prog))]
        [else (EvalExpr (cadr (cdr prog)))]
        )]
      [else #f])))

;EvalCCondExpr
;Takes in a snippet and evaluates it as a C Conditional Expression
(define EvalCCond
  (lambda (prog)
    (cond
      [(<(length prog) 1) #f]
      [(BCond prog) (EvalBCond prog)]
      [(<(length prog) 2) #f]
      [(and (equal? 'not (car prog)) (CCond (cadr prog))) (not (EvalCCond (cadr prog)))]
      [(not(equal? (length prog) 3)) #f]
      [(and (and (equal? 'and (car prog)) (CCond (cadr prog))) (CCond (cadr (cdr prog)))) (and (EvalCCond (cadr prog)) (EvalCCond (cadr (cdr prog))))]
      [(and (and (equal? 'or (car prog)) (CCond (cadr prog))) (CCond (cadr (cdr prog)))) (or (EvalCCond (cadr prog)) (EvalCCond (cadr (cdr prog))))]
      [else #f])))

;ArithExpr
;Takes in a snippet and return true if it is an Arithmetic Expression
(define ArithExpr
  (lambda (prog)
    (cond
      [(not(equal?(length prog) 3)) #f]
      [(and (and (Op (car prog)) (Expr (cadr prog))) (Expr (cadr (cdr prog)))) #t]
      [else #f])))

;EvalArithExpr
;Takes in a snippet and evaluates it as an Arithmetic Expression
(define EvalArithExpr
  (lambda (prog)
    (cond
      [(not(equal?(length prog) 3)) #f]
      [(equal? '+ (car prog)) (+ (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [(equal? '- (car prog)) (- (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [(equal? '* (car prog)) (* (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [(equal? '/ (car prog)) (/ (EvalExpr (cadr prog)) (EvalExpr (cadr (cdr prog))))]
      [else #f])))

;OpExpr
;Takes in a snippet and return true if it is an Operator Expression
(define OpExpr
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(ArithExpr prog) #t]
      [(CondExpr prog) #t]
      [else #f])))

;EvalOpExpr
;Takes in a snippet and evaluates it as an Operator Expression
(define EvalOpExpr
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(ArithExpr prog) (EvalArithExpr prog)]
      [(CondExpr prog) (EvalCondExpr prog)]
      [else #f])))

;Number
;Takes in a snippet and returns true if it is number
(define Number
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(number? prog) #t]
      [else #f])))

;Expr
;Takes in values and determines if they are expressions
(define Expr
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(Number prog) #t]
      [(OpExpr prog) #t]
      [else #f])))

;EvalExpr
;takes in a snippet and evaluates it
(define EvalExpr
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(Number prog) prog]
      [(OpExpr prog) (EvalOpExpr prog)]
      [else #f])))
  
;synchk
;takes in a program and returns true if it is valid syntax
(define synchk
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [else (Expr prog)])))

;eval
;takes in a program and returns the evaluation of it
(define eval
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(not(synchk prog)) #f]
      [else (EvalExpr prog)])))