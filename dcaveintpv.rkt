#lang racket
(require "program.rkt")
(require racket/trace)
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
    (lambda (envi)
    (cond
      [(not(equal? (length prog) 3)) #f]
      [(equal? ((EvalExpr (cadr prog))envi) '(Cannot Evaluate)) '(Cannot Evaluate)]
      [(equal? ((EvalExpr (cadr (cdr prog))) envi) '(Cannot Evaluate)) '(Cannot Evaluate)]
      [(equal? 'lt (car prog)) (< ((EvalExpr (cadr prog))envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [(equal? 'gt (car prog)) (> ((EvalExpr (cadr prog))envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [(equal? 'eq (car prog)) (equal? ((EvalExpr (cadr prog))envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [else #f]))))

;CCond
;takes in a snippet and return true if it is a C condition
(define CCond
  (lambda (prog)
    (cond
      [(<(length prog) 1) #f]
      [(BCond prog) #t]
      [(<(length prog) 2) #f]
      [(and (equal? 'not (car prog)) (CCond (cadr prog))) #t]
      [(not(equal?(length prog) 3)) #f]
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
    (lambda (envi)
    (cond
      [(not(equal? (length prog) 3)) #f]
      [(not (CondExpr prog)) #f]
      [(cond
        [(equal? ((EvalExpr (cadr prog))envi) '(Cannot Evaluate)) '(Cannot Evaluate)]
        [((EvalCCond (car prog))envi) ((EvalExpr (cadr prog)) envi)]
        [else ((EvalExpr (cadr (cdr prog)))envi)]
        )]
      [else #f]))))

;EvalCCondExpr
;Takes in a snippet and evaluates it as a C Conditional Expression
(define EvalCCond
  (lambda (prog)
    (lambda (envi)
    (cond
      [(<(length prog) 1) #f]
      [(BCond prog) ((EvalBCond prog) envi)]
      [(<(length prog) 2) #f]
      [(and (equal? 'not (car prog)) (CCond (cadr prog))) (not ((EvalCCond (cadr prog))envi))]
      [(not(equal? (length prog) 3)) #f]
      [(and (and (equal? 'and (car prog)) (CCond (cadr prog))) (CCond (cadr (cdr prog)))) (and ((EvalCCond (cadr prog))envi) ((EvalCCond (cadr (cdr prog)))envi))]
      [(and (and (equal? 'or (car prog)) (CCond (cadr prog))) (CCond (cadr (cdr prog)))) (or ((EvalCCond (cadr prog))envi) ((EvalCCond (cadr (cdr prog)))envi))]
      [else #f]))))

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
    (lambda (envi)
    (cond
      [(not(equal?(length prog) 3)) #f]
      [(equal? '(Cannot Evaluate) ((EvalExpr (cadr prog)) envi)) '(Cannot Evaluate)]
      [(equal? '(Cannot Evaluate) ((EvalExpr (cadr (cdr prog))) envi)) '(Cannot Evaluate)]
      [(equal? '+ (car prog)) (+ ((EvalExpr (cadr prog)) envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [(equal? '- (car prog)) (- ((EvalExpr (cadr prog)) envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [(equal? '* (car prog)) (* ((EvalExpr (cadr prog)) envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [(equal? '/ (car prog)) (/ ((EvalExpr (cadr prog)) envi) ((EvalExpr (cadr (cdr prog)))envi))]
      [else #f]))))

;VarAssign
;Takes in a snippet and return true if it is contains a variable and an expression
(define VarAssign
  (lambda (prog)
    (cond
      [(not(equal?(length prog) 2)) #f]
      [(and (Symbol (car prog)) (Expr (cadr prog)))]
      [else #f])))

;VarAssignSeq
;Takes in a sequence of variable assignement and returns true if they are syntactically correct
(define VarAssignSeq
  (lambda (prog)
    (cond
      [(null? prog) #t]
      [(VarAssign (car prog)) (VarAssignSeq (cdr prog))]
      [else #f])))

;VarAssign-new
;Takes in a snippet and return true if it contains a variable and an expression or multiple sets
(define VarAssign-new
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(list? (car prog)) (VarAssignSeq prog)]
      [else (VarAssign prog)])))

;LetExpr
;Takes in a snippet and return true if it is a Let Expression
(define LetExpr
  (lambda (prog)
    (cond
      [(not(equal?(length prog) 3)) #f]
      [(and (and (equal? 'let (car prog)) (VarAssign (cadr prog))) (Expr (cadr (cdr prog))))]
      [else #f])))

;LetExpr-new
;Takes in a snippet and return true if it is a Let Expression with or without multiple variables
(define LetExpr-new
  (lambda (prog)
    (cond
      [(LetExpr prog) #t]
      [(not(equal?(length prog) 3)) #f]
      [(and (and (equal? 'let (car prog)) (VarAssign-new (cadr prog))) (Expr-new (cadr (cdr prog))))]
      [else #f])))

;OpExpr
;Takes in a snippet and return true if it is an Operator Expression
(define OpExpr
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(ArithExpr prog) #t]
      [(CondExpr prog) #t]
      [(LetExpr prog) #t]
      [else #f])))

;OpExpr-new
;Takes in a snippet and return true if it is an operator expression (allows for multivariable assignment)
(define OpExpr-new
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(ArithExpr prog) #t]
      [(CondExpr prog) #t]
      [(LetExpr-new prog) #t]
      [else #f])))

;EvalLetExpr
;Takes in a snippet of code and adds the variable to the environment
(define EvalLetExpr
  (lambda (prog)
    (lambda (envi)
      ((EvalExpr(cadr (cdr prog))) ((AssignVar (cadr prog)) envi)))))

;EvalLetExpr-new
;Takes in a snippet and add the variable or variables to the environment
(define EvalLetExpr-new
  (lambda (prog)
    (lambda (envi)
      ((EvalExpr-new(cadr (cdr prog))) ((AssignVar-new (cadr prog)) envi)))))

;AssignVar
;Takes in a varAssign Expression and adds it to the environemtn
(define AssignVar
  (lambda (newVar)
    (lambda (envi)
      (cond
        [(null? newVar) envi]
        [(null? envi) (list newVar)]
        [else (cons newVar envi)]))))

;AssignVar-new
;Takes in a varAssign Expression list and adds it to the environment
(define AssignVar-new
  (lambda (newVars)
    (lambda (envi)
      (cond
        [(null? newVars) envi]
        [(and (null? envi) (list? newVars)) newVars]
        [(null? envi) (list newVars)]
        [else (cons (car newVars) ((AssignVar (cdr newVars))envi))]))))
        

;EvalOpExpr
;Takes in a snippet and evaluates it as an Operator Expression
(define EvalOpExpr
  (lambda (prog)
    (lambda(envi)
    (cond
      [(null? prog) #f]
      [(ArithExpr prog) ((EvalArithExpr prog)envi)]
      [(CondExpr prog) ((EvalCondExpr prog)envi)]
      [(LetExpr prog) ((EvalLetExpr prog) envi)]
      [else #f]))))

;EvalOpExpr
;Takes in a snippet and evaluates it as an operator expression, allowing for multi variable assignment
(define EvalOpExpr-new
  (lambda (prog)
    (lambda (envi)
      (cond
        [(null? prog) #f]
        [(ArithExpr prog) ((EvalArithExpr prog)envi)]
        [(CondExpr prog) ((EvalCondExpr prog)envi)]
        [(LetExpr-new prog) ((EvalLetExpr-new prog) envi)]
        [else #f]))))

;Number
;Takes in a snippet and returns true if it is number
(define Number
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(number? prog) #t]
      [else #f])))

;Symbol
;Takes in a snippet of code and sees if it is a symbol.  Does not check to see if it is free
(define Symbol
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(symbol? prog) #t]
      [else #f])))

;Expr
;Takes in values and determines if they are expressions
(define Expr
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(Number prog) #t]
      [(Symbol prog) #t]
      [(OpExpr prog) #t]
      [else #f])))

;Expr-new
;Tads in values and determines if they are expressions. Allows for multiple variable assignment
(define Expr-new
  (lambda (prog)
    (cond
      [(null? prog) #f]
      [(Number prog) #t]
      [(Symbol prog) #t]
      [(OpExpr-new prog) #t]
      [else #f])))

;EvalVar
;takes in a snippet and evaluates if the variable exists in the environment.  If the variable is free (does
;not exist in the environment) then it will return the message "Cannot Evaluate"
(define EvalVar
  (lambda (prog)
    (lambda (envi)
      (cond
        [(null? envi) '(Cannot Evaluate)]
        [(null? prog) #f]
        [else ((EvalExpr((FindVar prog) envi)) ((PopEnvi prog) envi))]))))

;PopEnvi
;Takes in a symbol and the environment, and if the symbol exists in the environment then it removes it from the environment
(define PopEnvi
  (lambda (symbol)
    (lambda (envi)
      (cond
        [(null? envi) '()]
        [(equal? symbol (car (car envi))) (cdr envi)]
        [(> (length envi) 1) (cons (car envi) ((PopEnvi symbol) (cdr envi)))]
        [else '()]))))

;FindVar
;Takes in a symbol and a list (environment) if the item exists in the environment, then it returns the other item
;associated with it, otherwise it returns "Cannot Evaluate
(define FindVar
  (lambda (symbol)
    (lambda (envi)
      (cond
        [(null? envi) '(Cannot Evaluate)]
        [(equal? symbol (car (car envi))) (cadr (car envi))]
        [(> (length envi) 1) ((FindVar symbol) (cdr envi))]
        [else '(Cannot Evaluate)]))))

;EvalExpr
;takes in a snippet and evaluates it
(define EvalExpr
  (lambda (prog)
    (lambda (envi)
    (cond
      [(null? prog) #f]
      [(equal? '(Cannot Evaluate) prog) '(Cannot Evaluate)]
      [(Number prog) prog]
      [(Symbol prog) ((EvalVar prog)envi)]
      [(OpExpr prog) ((EvalOpExpr prog)envi)]
      [else #f]))))

;EvalExpr-new
;Does the same as EvalExpr, but allows for multiple assignments in let expressions
(define EvalExpr-new
  (lambda (prog)
    (lambda (envi)
      (cond
        [(null? prog) #f]
        [(equal? '(Cannot Evaluate) prog) '(Cannot Evaluate)]
        [(Number prog) prog]
        [(Symbol prog) ((EvalVar prog) envi)]
        [(OpExpr-new prog) ((EvalOpExpr-new prog)envi)]
        [else #f]))))
  
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
    (lambda (envi)
    (cond
      [(null? prog) #f]
      [(not(synchk prog)) #f]
      [else ((EvalExpr prog)envi)]))))

;eval-new
;takes in a program allowing for multiple variable assignments in one line
(define eval-new
  (lambda (prog)
    (lambda (envi)
      (cond
        [(null? prog) #f]
        [else ((EvalExpr-new prog) envi)]))))

      
