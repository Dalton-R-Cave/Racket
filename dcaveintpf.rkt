#lang racket
(require "program.rkt")
(provide (all-defined-out))

;synchk function
;checks to make sure that functions adhere to both rules, then calls the rest of the syntax checker
(define (synchk expr)
  (if (properfunctions expr '())
      (synchk2 expr)
      false))

;checks to make sure that the expr follows the given grammar rules
(define (synchk2 expr)
  (cond
    [ (number? expr)  true ]
    [ (symbol? expr)  true ]
    [ (isarith (car expr))  (and (equal? (length expr) 3)
                                  (synchk2 (cadr expr))
                                  (synchk2 (cadr (cdr expr)))) ]
    [ (isletp (car expr)) (and (equal? (length expr) 3)
                               (symbol? (car (car (cadr expr))))
                               (synformalparams (cadr (car (cadr expr))))
                               (synchk2 (cadr (cadr expr))))]
    [ (islet (car expr)) (and (equal? (length expr) 3)
                              (symbol? (car (cadr expr)))
                              (synchk2 (cadr (cadr expr)))
                              (synchk2 (cadr (cdr expr)))) ]
    [ (and (list? (car expr))
           (iscond (car expr)))
                              (and 
                                  (syncondition (car expr))
                                  (synchk2 (cadr expr))
                                  (synchk2 (cadr (cdr expr)))) ]
    [ (isapply (car expr)) (and (equal? (length expr) 2)
                               (symbol? (car (cadr expr)))
                               (synargs (cadr (cadr expr))))]
    [ else false ]
    ))

;Checks to make sure that the functions follow the rules laid out by the homework
;returns true if the functions follow these rules and false otherwise
(define (properfunctions expr envi)
  (cond
    [ (null? expr) true]
    [ (number? expr) true]
    [ (symbol? expr) true]
    [ (isarith (car expr)) (and (properfunctions (cadr expr) envi)
                                (properfunctions (cadr (cdr expr)) envi))]
    [ (isletp (car expr)) (and (properfunctions (cadr (cadr expr)) (cons (pair (car (car (cadr expr))) (length (cadr (car (cadr expr))))) envi))
                               (properfunctions (cadr (cdr expr)) (cons (pair (car (car (cadr expr))) (length (cadr (car (cadr expr))))) envi)))]
    [ (isapply (car expr)) (and (mymember2 (car (cadr expr)) envi)
                                (argnummatches (car (cadr expr)) (cadr (cadr expr)) envi))]
    [ (islet (car expr)) (and (properfunctions (cadr (cdr expr)) envi)
                              (properfunctions (cadr (cadr expr)) envi))]
    [ (and (list? (car expr))
           (iscond (car expr)))
                         (and
                             (properfunctions (cadr expr) envi)
                             (properfunctions (cadr (cdr expr)) envi))]
))

;helper method to ensure the arguments from the function match the number of parameters given in the letp statement
(define (argnummatches name list envi)
  (cond
    [ (null? name) false]
    [ (null? envi) false]
    [ (equal? (length list) (getvalue name envi)) (checklist list envi)]
    [ else false]
    ))

;returns the number of arguments given a function name
(define (getvalue name envi)
  (cond
    [ (equal? name (car (car envi))) (cadr (car envi))]
    [ else (getvalue name (cdr envi))]
    ))

;checks a list of arguments to ensure that they do not contain any improper functions
(define (checklist list envi)
  (cond
    [ (null? list) true]
    [ (properfunctions (car list) envi) (checklist (cdr list) envi)]
    [ else false]
    ))

;creates a list of two items
(define (pair item1 item2)
  (cons item1 (list item2)))

;returns true if the expr is "apply"
(define (isapply expr)
  (equal? expr 'apply))

;returns true if the expr is "letp"
(define (isletp expr)
  (equal? expr 'letp))

;syntax checker for a list of arguments, checks the validity of each element in the list
(define (synargs args)
  (cond
    [ (null? args) true]
    [ (synchk2 args) true]
    [ (and (list? args)
           (synargs (car args))
           (synargs (cdr args)))]
    [else false]))

;syntax checker for the list of formal parameters, very similar to above, checks each element in the list
(define (synformalparams params)
  (cond
    [ (null? params) true]
    [ (symbol? params) true]
    [ (and (list? params)
           (synformalparams (car params))
           (synformalparams (cdr params)))]
    [else false]))

;syntax checker for conditional statements
(define (syncondition condition)
  (cond
    [ (equal? (car condition) 'gt)   (and (equal? (length condition) 3) (synchk2 (cadr condition)) (synchk2 (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'lt)   (and (equal? (length condition) 3) (synchk2 (cadr condition)) (synchk2 (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'eq)   (and (equal? (length condition) 3) (synchk2 (cadr condition)) (synchk2 (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'or)   (and (equal? (length condition) 3) (syncondition (cadr condition)) (syncondition (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'and)  (and (equal? (length condition) 3) (syncondition (cadr condition)) (syncondition (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'not)  (and (equal? (length condition) 2) (syncondition (cadr condition))) ]
    [ else false ]))
    

;; don't believe this needs to be included in this homework, but left it in anyway
(define (eval-new expr env)
  (eval (translate expr) env))

;; Translate expression given following the extended grammar of Q3
;;        to expression given following the grammar of Q1-2.
;; Will allow us to use the eval from Q2.
(define (translate expr)
  (cond
    [ (number? expr) expr ]  ;; no change
    [ (symbol? expr) expr ]  ;; no change
    ;; translate expressions 
    [ (islet (car expr)) (transvarassign (cadr expr) (translate (cadr (cdr expr)))) ] ;; new rule for sequence of varassigns
    [ (isarith (car expr)) (list (car expr) (translate (cadr expr)) (translate (cadr (cdr expr)))) ]
    [ (iscond (car expr)) (list (transcondition (car expr)) (translate (cadr expr)) (translate (cadr (cdr expr)))) ]
    ))


(define (transvarassign varassignseq expr)
  (if (null? varassignseq)
      expr
      (list 'let (car varassignseq) (transvarassign(cdr varassignseq) expr)))) 


(define (transcondition condition)
  (cond
    [ (equal? (car condition) 'gt)   (list 'gt (translate (cadr condition)) (translate (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'lt)   (list 'lt (translate (cadr condition)) (translate (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'eq)   (list 'eq (translate (cadr condition)) (translate (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'or)   (list 'or (transcondition (cadr condition)) (transcondition (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'and)  (list 'and (transcondition (cadr condition)) (transcondition (cadr (cdr condition)))) ]
    [ (equal? (car condition) 'not)  (list 'not (transcondition (cadr condition))) ]
    ))


;;;;;;;;;;;;;;;;;;;;;
;; nofree number: true
;;        var: if present in the boundvar list
;;        let (var expr1) expr2:  expr1 does not have free variables and expr2 does not have free variables
;;        added to this to allow for letp, and to check the arguments in the case of applys
(define (nofree expr boundvars)
  (cond
    [ (number? expr) true ]
    [ (symbol? expr) (if (mymember expr boundvars) true false) ]
    [ (islet (car expr)) (and (nofree (cadr (cadr expr)) boundvars) (nofree (cadr (cdr expr)) (cons (car (cadr expr)) boundvars))) ]
    [ (isletp (car expr)) (nofree (cadr (cdr expr)) boundvars)]
    [ (isapply (car expr)) (nofreeargs (cadr (cadr expr)) boundvars)]
    [ (isarith (car expr)) (and (nofree (cadr expr) boundvars) (nofree (cadr (cdr expr)) boundvars)) ]
    [ (iscond (car expr)) (and (nofreecondition (car expr) boundvars)
                               (nofree (cadr expr) boundvars)
                               (nofree (cadr (cdr expr)) boundvars)) ]
    ))

;checks through a list of arguments to make sure that there are no free variables present in them
(define (nofreeargs lst boundvars)
  (cond
    [(null? lst) true]
    [(nofree (car lst) boundvars) (nofreeargs (cdr lst) boundvars)]
    [else false]))

;Determines if x is an element of lst
(define (mymember x lst)
  (if (null? lst)
      false
      (if (equal? x (car lst))
          true
          (mymember x (cdr lst)))))

;determines if x is an element of lst
;this differs from mymember in that it is checking for function names
;which are represented differently than variables, so we look at different pieces
;in this helper function
(define (mymember2 x lst)
  (if (null? lst)
      false
      (if (mymember x (car lst))
          (if (equal? 1 (length lst)) true
              (mymember2 x (cdr lst)))
          (mymember2 x (cdr lst)))))

;checks for free variables in the conditional statements
(define (nofreecondition condition boundvars)
  (cond
    [ (or (equal? (car condition) 'gt)
          (equal? (car condition) 'lt)
          (equal? (car condition) 'eq))  (and (nofree (cadr condition) boundvars) (nofree (cadr (cdr condition)) boundvars)) ]
    [ (or (equal? (car condition) 'or)
          (equal? (car condition) 'and)) (and (nofreecondition (cadr condition) boundvars) (nofreecondition (cadr (cdr condition)) boundvars)) ]
    [ else (nofreecondition (cadr condition)) ]
    ))
      
;;;;;;;;;;;;;;;;;;;;

;performs the actual evaluation
(define (eval expr env)
  (cond
    [ (synchk expr) (cond
                      [ (not (nofree expr (getbound env))) '(Cannot Evaluate) ]  ;; if the expression contains free variable, do not evaluate
                      [ (myeval expr env) ])]   ;; updated the version from hw5:letp and apply
    [ else '(Invalid Syntax)]))

;transforms the original environment into a list of all bound vars for nofree function use
(define (getbound env)
  (cond
    [ (null? env) env]
    [else (cons (car (car env)) (getbound (cdr env)))]))

;helper function that does the heavy lifting for evaluation
(define (myeval expr env)
  (cond
    [ (number? expr)  expr ]
    [ (symbol? expr)  (findvalue expr env) ] 
    [ (islet (car expr)) (evallet expr env) ]  
    [ (isletp (car expr)) (evalletp expr env) ] ; new rule
    [ (isapply (car expr)) (executefunction (cadr expr) env) ] ; new rule
    [ (isarith (car expr)) (evalarith expr env) ] 
    [ (iscond (car expr))  (if (evalcondition (car expr) env)  
                               (myeval (cadr expr) env)       
                               (myeval (cadr (cdr expr)) env)) ]
    ))

;returns true if x is "let"
(define (islet x)
  (equal? x 'let))

;returns the value of a variable as found in the environment
(define (findvalue var env)
  (if (equal? var (car (car env))) ;; We already know expression does not contain free variables
      (cadr (car env))
      (findvalue var (cdr env))))

;execute a function being stored in the environment
(define (executefunction function env)
  (myeval (findfunction function env) (getenv function env)))

;find a function in the environment given its name
(define (findfunction function env)
  (cond
    [(and (list? (car (car env))) (equal? (car function) (car (car (car env)))))
     (cadr (car env))]
    [else (findfunction function (cdr env))]))

;get the environment in which a function should be executed
(define (getenv function env)
  (addtoenv (cadr function) (findparams function env) env))

;find the parameters of the function being stored in the environment, so we can apply them with the args to the front of the env
(define (findparams function env)
  (cond
    [(and (list? (car (car env))) (equal? (car function) (car (car (car env)))))
     (cadr (car (car env)))]
    [else (findparams function (cdr env))]))

;add the args and parameters for a function to the env
(define (addtoenv args params env)
  (cond
    [(null? args) env]
    [else (addtoenv (cdr args) (cdr params)
                    (cons (list (car params) (myeval (car args) env)) env))]))

;Evaluate a let expr
(define (evallet expr env)
  (myeval (cadr (cdr expr)) (cons (list (car (cadr expr)) (myeval (cadr (cadr expr)) env)) env)))

;Evaluate a letp expr: adding a function to the environment
(define (evalletp expr env)
  (myeval (cadr (cdr expr)) (cons (cadr expr) env)))

;return true if x is an operator
(define (isarith x)
  (or (equal? x '+) (equal? x '-) (equal? x '*) (equal? x '/)))

;Evaluate an arith expression
(define (evalarith expr env) 
  (cond
    [ (equal? (car expr) '+) (+ (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '-) (- (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '*) (* (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    [ (equal? (car expr) '/) (/ (myeval (cadr expr) env) (myeval (cadr (cdr expr)) env)) ]
    ))

;return true is x is boolean
(define (iscond condition)
  (cond
    [(list? condition)
     (or (equal? (car condition) 'gt)
         (equal? (car condition) 'lt)
         (equal? (car condition) 'eq)
         (equal? (car condition) 'or)
         (equal? (car condition) 'and)
         (equal? (car condition) 'not))]
     [else false]))

;Evaluate a boolean expression
(define (evalcondition condition env)
  (cond
    [ (equal? (car condition) 'gt)   (> (myeval (cadr condition) env) (myeval (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'lt)   (< (myeval (cadr condition) env) (myeval (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'eq)   (equal? (myeval (cadr condition) env) (myeval (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'or)   (or (evalcondition (cadr condition) env) (evalcondition (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'and)  (and (evalcondition (cadr condition) env) (evalcondition (cadr (cdr condition)) env)) ]
    [ (equal? (car condition) 'not)  (not (evalcondition (cadr condition) env)) ]
    ))
    
