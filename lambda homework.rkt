;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |lambda homework|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
  (lambda (symbol lc-exp)
    (list 'remember symbol lc-exp)))
(define remember-exp?
  (lambda (lc-exp)
    (if (not (list? lc-exp)) #f (eqv? 'remember (car lc-exp)))))

(define block-exp
  (lambda (lo-lcexpr)
    (cons 'block lo-lcexpr)))
(define block-exp?
  (lambda (lc-exp)
    (eqv? 'block (car lc-exp))))

(define question-exp
  (lambda (bool-lcexp true-lcexp false-lcexp)
    (list 'question bool-lcexp true-lcexp false-lcexp)))
(define question-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'question)))

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

(define math-exp
  (lambda (math-op lolc-exp)
    (append (list 'math math-op) lolc-exp)))
(define math-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'math)))
(define math-get-op
  (lambda (lc-exp)
    (cadr lc-exp)))
(define math-get-body
  (lambda (lc-exp)
    (cddr lc-exp)))

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

(define math-exp-helper
  (lambda( math-op exp)
    (cond
      ((null? exp)
       (if (eq? math-op '/) 1 0))
      ((eq? math-op '+) (+ (car exp) (math-exp-helper math-op (cdr exp))))
      ((eq? math-op '-) (- (car exp) (math-exp-helper math-op (cdr exp))))
      ((eq? math-op '*) (* (car exp) (math-exp-helper math-op (cdr exp))))
      ((eq? math-op '/) (/ (car exp) (math-exp-helper math-op (cdr exp)))) 
      (else
       (modulo (car exp) (math-exp-helper math-op (cdr exp)))))))
      
      
            

(define apply-expression
  (lambda (expr env)
    (cond
      ((lit-exp? expr) expr)
      ((var-exp? expr) (apply-env expr env))
      ((math-exp? expr)
       (let*
           ((mathOperator (math-get-op expr))
            (mathBody (math-get-body expr))
           (mapNum (map (lambda (x) (apply-expression x env)) mathBody)))
       (math-exp-helper mathOperator mapNum)))
      ((remember-exp? expr) (let ((symbol (cadr expr))
                                  (value (caddr expr)))
                              (if (lambda-exp? value)
                                  (extend-env symbol value env)
                                  (extend-env symbol (apply-expression value env) env))))           
      ((block-exp? expr) (letrec ((lo-lcexpr (cdr expr))
                                  (process (lambda (lo-exp curr-env)
                                             (cond
                                               ((null? lo-exp) '())
                                               ((not (remember-exp? (car lo-exp)))
                                                (cons (apply-expression (car lo-exp) curr-env)
                                                      (process (cdr lo-exp) curr-env)))
                                               (else
                                                (let* ((remember-exp (car lo-lcexpr))
                                                      (rest-of-exps (cdr lo-lcexpr))
                                                      (new-env (apply-expression remember-exp env)))
                                                  (append (list #T) (apply-expression
                                                                     (block-exp rest-of-exps)
                                                                     new-env))))))))
                           (process lo-lcexpr env)))
      ((question-exp? expr)
       (let ((bool-exp (cadr expr))
             (true-exp (caddr expr))
             (false-exp (cadddr expr)))
         (if (= 0 (apply-expression bool-exp env))
             (if (null? false-exp) false-exp (apply-expression false-exp env))
             (apply-expression true-exp env))))
      ((lambda-exp? expr) (apply-expression (lambda-get-body expr) env))
      ((repeat-exp? expr) (letrec ((count (apply-expression (repeat-get-count expr) env))
                                   (body (repeat-get-body expr))
                                   (repeat-func
                                    (lambda (x countdown)
                                      (if (= countdown 0)
                                          '()
                                          (cons (apply-expression x env)
                                                (repeat-func x (- countdown 1)))))))
                            (repeat-func body count)))
      ((repeat2-exp? expr) (map (lambda (x) (apply-expression x env)) (cdr expr)))
      (else (let* ((operator (app-exp-get-operator expr))) ;operator is assumed to be a lambda
              (if (lambda-exp? operator)
                  (let* ((looperands (app-exp-get-operands expr))
                  (loParamNames (lambda-get-params operator))
                  (loResolvedOperands (map (lambda (x) (apply-expression x env)) looperands)))
                  (apply-expression operator
                                (extend-env* loParamNames loResolvedOperands env)))
                  (let ((map-param (map (lambda (x) (apply-expression x env)) expr)))
                        (apply-expression map-param env))))))))
           
(define testENV (extend-env* '(a c x) '(9 3 2) (empty-env)))
(define apply-program
  (lambda (lc-expr)
    (apply-expression lc-expr (empty-env))))

(define math-test (math-exp '/ '(a c)))
(apply-expression math-test testENV)
                            
(apply-expression (block-exp
                   (list (remember-exp 'x 25)
                         (remember-exp 'y 36)
                         (remember-exp 'z 49)
                         (remember-exp 'l (lambda-exp '(a) (var-exp 'a)))
                         (app-exp-old (var-exp 'l) (lit-exp 16))
                         (app-exp (lambda-exp '(f) (var-exp 'f)) (list 'z)))) testENV) ;should be (#t #t #t #t 16)                         
;(apply-program test-exp)