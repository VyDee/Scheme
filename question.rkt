;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname question) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Implement a conditional ability in our language similar to that of C.
;That is to say a value is true if it boils down to 1 and false otherwise.
;Make sure you submit the self eval.

;question-exp -> (question test-exp true-exp false-exp)

;test-exp is any kind of lc-expr that ultimately boils down to a number

;true-exp is the expression that fires if test-exp is a 1

;false-exp is the expression that fires if test-exp is a 0

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
    (cadr lambda-exp)))
(define lambda-get-body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
(define lambda-exp?
  (lambda (lc-exp)
    (if (or (not (list? lc-exp))
            (null? lc-exp))
        #f
        (eqv? (car lc-exp) 'lambda))))
(define app-exp-old
  (lambda (lc-exp1 lc-exp2)
    (list lc-exp1 lc-exp2)))
(define app-exp
  (lambda (lc-exp1 loExpressions)
    (append (list lc-exp1) loExpressions)))
(define app-exp-get-operator
  (lambda (app-exp)
    (car app-exp)))
(define app-exp-get-operands
  (lambda (app-exp)
    (cdr app-exp)))
(define app-exp?
  (lambda (lc-exp)
    (and (list? lc-exp)
         (= (length lc-exp) 2))))
(define question-exp
  (lambda (test-exp true-exp false-exp)
    (list 'question test-exp true-exp false-exp)))
(define question-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'question)))
(define question-get-test-exp
  (lambda (lc-exp)
    (cadr lc-exp)))
(define question-get-true-exp
  (lambda (lc-exp)
    (caddr lc-exp)))
(define question-get-false-exp
  (lambda (lc-exp)
    (cadddr lc-exp)))

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

(define condition-check
  (lambda (condition-exp)
    (if (zero? (question-get-test-exp condition-exp))
        (question-get-false-exp condition-exp)
        (question-get-true-exp condition-exp))))
    
(define apply-expression
  (lambda (expr env)
    (cond
      ((lit-exp? expr) expr)
      ((var-exp? expr) (apply-env expr env))
      ((lambda-exp? expr) (apply-expression (lambda-get-body expr) env))
      ((question-exp? expr) (let* ((test-number (apply-expression (question-get-test-exp expr) env))
                                      (condition
                                       (lambda (expr)
                                         (if (zero? test-number)
                                                (apply-expression (question-get-false-exp expr) env)
                                                (apply-expression (question-get-true-exp expr) env)))))
                               (condition expr)))
                                                  
      (else (let* ((operator (app-exp-get-operator expr))
                  (looperands (app-exp-get-operands expr))
                  (loResolvedOperands (map (lambda (x) (apply-expression x env)) looperands))
                  (loParamNames (lambda-get-params operator)))
              (apply-expression operator
                                (extend-env* loParamNames loResolvedOperands env)))))))
           
(define testENV (extend-env* '(a c) '(0 2) (empty-env)))
(define apply-program
  (lambda (lc-expr)
    (apply-expression lc-expr testENV)))
    
;(define workingTestEXPR (app-exp (lambda-exp '(a) (var-exp 'a)) (lit-exp 7)))
(define body (lambda-exp '(x) (var-exp 'x)))
(define newTestEXPR (app-exp (lambda-exp '(a b d) body) (list (lit-exp 7) (lit-exp 8) (var-exp 'c))))
;(apply-program workingTestEXPR)
(apply-expression (question-exp (var-exp 'a) (lit-exp 5) (lit-exp 6)) testENV)
(apply-expression (question-exp (lit-exp 'c) (lit-exp 5) (lit-exp 6)) testENV)
;(apply-program newTestEXPR)