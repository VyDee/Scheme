;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |define and block test|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define remember-exp
  (lambda (name value)
    (list 'remember name value)))
(define remember-exp-get-name
  (lambda (expr)
    (cadr expr)))
(define remember-exp-get-value
  (lambda (expr)
    (caddr expr)))
(define remember-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'remember)))
(define block-exp
  (lambda (lolc-exp)
  (cons 'block lolc-exp)))
(define block-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'block)))
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
(define repeat-exp
  (lambda (number-of-times-exp body-exp)
    (list 'repeat-exp number-of-times-exp body-exp)))
(define repeat-helper
  (lambda (countdown lc-exp)
    (if (= countdown 0)
        '()
        (cons lc-exp (repeat-helper (- countdown 1) lc-exp)))))
(define repeat-exp2
  (lambda (number-of-times-exp body-exp)
    (cons 'repeat2 (repeat-helper number-of-times-exp body-exp))))
(define repeat2-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'repeat2)))
(define repeat-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'repeat-exp)))
(define repeat-get-count
  (lambda (lc-exp)
    (cadr lc-exp)))
(define repeat-get-body
  (lambda (lc-exp)
    (caddr lc-exp)))
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
      ((lambda-exp? expr) (apply-expression (lambda-get-body expr) env))
      ((remember-exp? expr)
          (apply-expression (remember-exp-get-name expr)
                            (extend-env (remember-exp-get-name expr) (remember-exp-get-value expr) env)))
      ((block-exp? expr) 
        (letrec ((body (cdr expr))
                 (next-execution 
                  (lambda (loexpr env)
                    (if (null? loexpr)
                        '()
                        (let ((first (car loexpr))
                              (rest (cdr loexpr)))
                          (if (remember-exp? first)
                              (let ((add-env (extend-env (remember-exp-get-name first) (remember-exp-get-value first) env)))
                                (cons (apply-expression (remember-exp-get-name first) add-env)
                                      (next-execution rest add-env)))
                              (cons (apply-expression first env) (next-execution rest env))))))))
          (next-execution body env)))
                                    
      ((repeat-exp? expr) 
      (letrec ((count (apply-expression (repeat-get-count expr) env))
                      (body (repeat-get-body expr))
                      (repeat-func
                        (lambda (x countdown)
                          (if (= countdown 0)
                              '()
                              (cons (apply-expression x env)
                                    (repeat-func x (- countdown 1)))))))
                            (repeat-func body count)))
      ((repeat2-exp? expr) (map (lambda (x) (apply-expression x env)) (cdr expr)))
      (else (let* ((operator (app-exp-get-operator expr))
                  (looperands (app-exp-get-operands expr))
                  (loResolvedOperands (map (lambda (x) (apply-expression x env)) looperands))
                  (loParamNames (lambda-get-params operator)))
              (apply-expression operator
                                (extend-env* loParamNames loResolvedOperands env)))))))
           
(define testENV (extend-env* '(c x) '(3 2) (empty-env)))
(define apply-program
  (lambda (lc-expr)
    (apply-expression lc-expr '())))
    
;;(define test-exp (repeat-exp2 (lit-exp 7) (lit-exp 5)))
;;(apply-program test-exp)
;;(define newTestEXPR (app-exp-old (lambda-exp '(a) 'x) (lit-exp 7)))
;;(define blocktest (block-exp (list newTestEXPR)))
(define body (lambda-exp '(x) (var-exp 'a)))
(define newTestEXPR (app-exp (lambda-exp '(e b d) body) (list (lit-exp 7) (lit-exp 8) (var-exp 'c))))
(define blocktest2 (block-exp (list (remember-exp 'a (apply-expression (app-exp-old (lambda-exp '(a) 'a) (lit-exp 5)) testENV)) newTestEXPR)))
(apply-expression blocktest2 testENV)
;;(apply-expression newTestEXPR testENV)
(define lst '(1 2 (3 4 (5 (6)))))