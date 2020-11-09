;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname repeat) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define empty-env
  (lambda ()
    '()))
(define extend-env
  (lambda (name value env)
    (list name value env)))
(define extend-env*
  (lambda (loNames loValues env)
    (if (null? loNames)
        env
        (extend-env*
         (cdr loNames)
         (cdr loValues)
         (extend-env
          (car loNames)
          (car loValues)
          env)))))
(define empty-env?
  (lambda (env)
    (null? env)))
(define apply-env
  (lambda (name env)
    (cond
      ((empty-env? env) #f)
      ((eq? name (car env)) (cadr env))
      (else (apply-env name (caddr env))))))
(define lit-exp
  (lambda (number)
    number))
(define lit-exp?
  (lambda (lc-exp)
    (number? lc-exp)))
(define var-exp
  (lambda (symbol)
    symbol))
(define var-exp?
  (lambda (lc-exp)
    (symbol? lc-exp)))
(define lambda-exp
  (lambda (los lc-exp)
    (list 'lambda los lc-exp)))
(define lambda-get-params
  (lambda (lambda-exp)
    (caadr lambda-exp)))
(define lambda-get-body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
(define lambda-exp?
  (lambda (lc-exp)
    (if (or (not (list? lc-exp))
            (null? lc-exp))
        #f
        (eqv? (car lc-exp) 'lambda))))
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list lc-exp1 lc-exp2)))
(define new-app-exp
  (lambda (lc-exp1 loExpressions)
    (append (list lc-exp1) loExpressions)))
(define app-exp-get-operator
  (lambda (app-exp)
    (car app-exp)))
(define app-exp-get-operand
  (lambda (app-exp)
    (cadr app-exp)))
(define app-exp?
  (lambda (lc-exp)
    (and (list? lc-exp)
         (= (length lc-exp) 2))))
(define occurs?
  (lambda (exp s)
    (cond
      ((symbol? exp) (eq? exp s))
      ((eq? (car exp) 'lambda)
       (let ((body (caddr exp)))
         (occurs? body s)))
      (else
       (let ((operator (car exp))
             (operand (cadr exp)))
         (or (occurs? operator s)
             (occurs? operand s)))))))
(define contains?
  (lambda (los s)
    (cond
      ((null? los) #f)
      ((eq? (car los) s) #t)
      (else (contains? (cdr los) s)))))
    
(define occurs-bound?
  (lambda (exp s)
    (cond
      ((symbol? exp) #f)
      ((eq? (car exp) 'lambda)
       (let ((loBoundVarNames (cadr exp))
             (body (caddr exp)))
         (if (contains? loBoundVarNames s)
             (occurs? body s)
             (occurs-bound? body s))))
      (else
       (let ((operator (car exp))
             (operand (cadr exp)))
         (or (occurs-bound? operator s)
             (occurs-bound? operand s)))))))
(define occurs-free?
  (lambda (exp s)
    (cond
      ((var-exp? exp) (eq? s exp))
      ((lambda-exp? exp)
       (let ((loBoundVarNames (cadr exp))
             (body (caddr exp)))
         (if (contains? loBoundVarNames s)
             #f
             (occurs-free? body s))))
      (else
       (or (occurs-free? (car exp) s)
           (occurs-free? (cadr exp) s))))))

(define apply-exp-helper
  (lambda (loexp env)
  (cond
    ((null? env) loexp)
    ((null? loexp) '())
    ((lit-exp? loexp) loexp)
    ((var-exp? loexp) (apply-env loexp env))
    ((lambda-exp? loexp) (apply-exp-helper (lambda-get-body loexp) env))
    (else
      (cons (apply-exp-helper (car loexp) env) (apply-exp-helper (cdr loexp) env))))))
(define apply-expression
  (lambda (expr env)
    (cond
      ((lit-exp? expr) expr)
      ((var-exp? expr) (apply-env expr env))
      ((lambda-exp? expr) (apply-exp-helper (cddr expr) env))
      (else
        (let ((operator (app-exp-get-operator expr))
              (resolved-operand (apply-exp-helper (cdr expr) env)))
              (apply-expression
                operator
                (extend-env* (cadr operator) (car resolved-operand) env)))))))
 (define apply-program
  (lambda (expr)
    (car (apply-expression expr testENV))))

 (define repeat-helper
   (lambda (lit-expr lc-exp)
     (if (zero? lit-expr)
         '()
         (cons lc-exp (repeat-helper (- lit-expr 1) lc-exp)))))
 (define repeat
   (lambda (lit-expr lc-exp)
     (cons 'repeat (repeat-helper lit-expr lc-exp))))
 (define repeat?
   (lambda (repeat-lst)
     (eqv? (car repeat-lst) 'repeat)))
 
           
(define testENV (extend-env* '(a b x) '(1 2 3) (empty-env)))
    
;(define working (app-exp (lambda-exp '(a) (var-exp 'a)) (lit-exp 7)))
;;(define new-list (new-app-exp (lambda-exp '(a b) (var-exp 'b)) (list (lit-exp 7) (lit-exp 8))))
;;(define newTest (app-exp (lambda-exp '(a b d) 'd) (list (lit-exp 7) (lit-exp 8) (lit-exp 'c))))
;;(apply-program newTest)
(define body (lambda-exp '(x) (var-exp 'x)))
(define newTest (app-exp (lambda-exp '(a b d) (lambda-exp '(x) (var-exp 'x)))
                         (list (lit-exp 7) (lit-exp 8) (var-exp 'x))))
 (apply-program newTest)

 (define repeatTest (repeat (lit-exp 3) body))
 repeatTest
 (repeat? repeatTest)
 

