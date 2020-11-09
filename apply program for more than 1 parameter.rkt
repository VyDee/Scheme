;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |apply program for more than 1 parameter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define apply-expression
  (lambda (expr env)
    (cond
      ((lit-exp? expr) expr)
      ((var-exp? expr) (apply-env expr env))
      ((lambda-exp? expr) (apply-expression (lambda-get-body expr) env)) ;get body: (caddr expr)
      
      (else (let ((operator (app-exp-get-operator expr)) ;;get oprator (car expr)
                  (resolved-operand (apply-expression (append-lst (cdr expr)) env))) ;;get operand: (cadr expr)
              (apply-expression operator
                                (extend-env*
                                 (lambda-get-params operator) resolved-operand env))))))) ;;get para: (caadr expr)



           
(define testENV (extend-env* '(a b c) '(1 2 3) (empty-env)))

(define append-lst
  (lambda (lst)
    (if (null? lst)
        '()
        (append (cdar lst) (append-lst (cdr lst))))))

(define apply-program
  (lambda (lc-expr)
    (apply-expression lc-expr (empty-env))))
    
(define workingTestEXPR (app-exp (lambda-exp '(a) (var-exp 'a)) (lit-exp 7)))
(define new (new-app-exp (lambda-exp '(a b) (var-exp 'b)) '((lit-exp 7) (lit-exp 8))))
(apply-program workingTestEXPR)
;(apply-program newTestEXPR) should work for the hw
