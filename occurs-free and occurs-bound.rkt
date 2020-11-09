;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |occurs-free and occurs-bound|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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




(define var-exp
  (lambda (symbol)
    symbol))
(define var-exp?
  (lambda (lc-exp)
    (symbol? lc-exp)))
(define lambda-exp
  (lambda (symbol lc-exp)
    (list 'lambda (list symbol) lc-exp)))
(define lambda-exp?
  (lambda (lc-exp)
    (if (or (not (list? lc-exp))
            (null? lc-exp))
        #f
        (eqv? (car lc-exp) 'lambda))))
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list lc-exp1 lc-exp2)))
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
(define occurs-bound?
  (lambda (exp s)
    (cond
      ((symbol? exp) #f)
      ((eq? (car exp) 'lambda)
       (let ((boundVarName (caadr exp))
             (body (caddr exp)))
         (if (eq? boundVarName s)
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
      ((symbol? exp) (eqv? exp s))
      ((eq? (car exp) 'lambda)
       (let ((boundVarName (caadr exp))
             (body (caddr exp)))
         (if (eq? boundVarName s)
             #f
             (occurs-free? body s))))
      (else
       (let ((operator (car exp))
             (operand (cadr exp)))
         (or (occurs-free? operator s)
             (occurs-free? operand s)))))))

(define occurs-free2?
  (lambda (exp s)
    (if (occurs-bound? exp s)
        #f
        #t)))


(define theEnv (extend-env* '(a b c) '(1 2 3) (empty-env)))
(define var-expression (var-exp 'x))
(define lambda-expression (lambda-exp 'x var-expression))
(define app-expression (app-exp lambda-expression var-expression))
(define app-expression2 (app-exp lambda-expression lambda-expression))


(lambda-exp 'z (list 'z (lambda-exp 'x '(x z))))





