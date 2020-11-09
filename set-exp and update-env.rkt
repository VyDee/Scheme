;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |set-exp and update-env|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
(define update-env
  (lambda (name value env)
    (cond
      ((empty-env? env)(list name value env))
      ((eqv? (car env) name) (list name value (caddr env)))
      (else
       ;(append (list (car env) (cadr env)) (list (update-env name value (caddr env))))))))
       (extend-env (car env) (cadr env) (update-env name value (caddr env)))))))    
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
(define set-exp
  (lambda (s lc-exp)
    (list 'set-exp s lc-exp)))
(define set-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'set-exp)))
(define set-exp-get-var-name
  (lambda (set-exp)
    (cadr set-exp)))
(define set-exp-get-var-value
  (lambda (set-exp)
    (caddr set-exp)))
(define math-exp
  (lambda (math-op lc-exp1 lc-exp2)
    (list 'math-exp math-op lc-exp1 lc-exp2)))
(define math-exp?
  (lambda (lc-exp)
    (eqv? (car lc-exp) 'math-exp)))
(define math-exp-get-op
  (lambda (math-exp)
    (cadr math-exp)))
(define math-exp-get-left-operand
  (lambda (math-exp)
    (caddr math-exp)))
(define math-exp-get-right-operand
  (lambda (math-exp)
    (cadddr math-exp)))
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
(define bool-exp
  (lambda (bool-op lc-exp1 lc-exp2)
    (list 'bool bool-op lc-exp1 lc-exp2)))
(define bool-exp?
  (lambda (lc-exp)
    (eq? (car lc-exp) 'bool)))
(define bool-get-op
  (lambda (lc-exp)
    (cadr lc-exp)))
(define bool-get-left-body
  (lambda (lc-exp)
    (caddr lc-exp)))
(define bool-get-right-body
  (lambda (lc-exp)
    (cadddr lc-exp)))
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
(define apply-math
  (lambda (op loperand roperand)
    (cond
      ((eq? op '+) (+ loperand roperand))
      ((eq? op '-) (- loperand roperand)) ; subtract roperand from loperand
      ((eq? op '*) (* loperand roperand))
      ((eq? op '/) (/ loperand roperand)) ; divide loperand by roperand
      ((eq? op '//) (quotient loperand roperand))
      ((eq? op '%) (modulo loperand roperand))
      (else #f))))
(define apply-bool
  (lambda (op left-exp right-exp)
    (cond
      ((eq? op '==) (if (= left-exp right-exp) 1 0))
      ((eq? op '<) (if (< left-exp right-exp) 1 0))
      ((eq? op '>) (if (> left-exp right-exp) 1 0))
      ((eq? op '>=) (if (>= left-exp right-exp) 1 0))
      ((eq? op '<=) (if (<= left-exp right-exp) 1 0))
      (else
       (if (not (= left-exp right-exp)) 1 0)))))
(define apply-expression
  (lambda (expr env)
    (cond
      ((lit-exp? expr) expr)
      ((var-exp? expr) (apply-env expr env))
      ((remember-exp? expr) (let ((symbol (cadr expr))
                                  (value (caddr expr)))
                              (if (lambda-exp? value)
                                  (extend-env symbol value env)
                                  (extend-env symbol (apply-expression value env) env))))
      ((set-exp? expr) (update-env (set-exp-get-var-name expr) (apply-expression (set-exp-get-var-value expr) env) env))
      ((math-exp? expr) (let ((op (math-exp-get-op expr))
                              (loperand (apply-expression
                                         (math-exp-get-left-operand expr)
                                         env))
                              (roperand (apply-expression
                                         (math-exp-get-right-operand expr)
                                         env)))
                          (apply-math op loperand roperand)))
      ((bool-exp? expr) (let ((op (bool-get-op expr))
                              (left-body (apply-expression (bool-get-left-body expr) env))
                              (right-body (apply-expression (bool-get-right-body expr) env)))
                          
                              (apply-bool op left-body right-body)))
      ((block-exp? expr) (letrec ((lo-lcexpr (cdr expr))
                                  (process (lambda (lo-exp curr-env)
                                             (cond
                                               ((null? lo-exp) '())
                                               ((set-exp? (car lo-exp))
                                                (let* ((set-exp (car lo-lcexpr))
                                                      (rest-of-exps (cdr lo-lcexpr))
                                                      (new-env (apply-expression set-exp env)))
                                                  (append (list #T) (apply-expression
                                                                     (block-exp rest-of-exps)
                                                                     new-env)))) 
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
      (else (let* ((operator (app-exp-get-operator expr)))
              (if (lambda-exp? operator)
                  (let* ((looperands (app-exp-get-operands expr))
                  (loResolvedOperands (map (lambda (x) (apply-expression x env)) looperands))
                  (loParamNames (lambda-get-params operator)))
                    (apply-expression operator
                                (extend-env* loParamNames loResolvedOperands env)))
                  (let* ((resolved-operator (apply-expression operator env))
                         (looperands (app-exp-get-operands expr))
                         (loResolvedOperands (map (lambda (x) (apply-expression x env)) looperands))
                         (loParamNames (lambda-get-params resolved-operator)))
                    (apply-expression resolved-operator
                                (extend-env* loParamNames loResolvedOperands env)))))))))
                  
           
;(define testENV (extend-env* '(a c x) '(9 3 2) (empty-env)))
;(define test-env (update-env 'a 2 testENV))
(define apply-program
  (lambda (lc-expr)
    (apply-expression lc-expr '())))

(define test-exp (block-exp (list
                             (remember-exp 'a (lit-exp 7))
                             (remember-exp 'b (lit-exp 5))
                             (remember-exp 'addStuff
                                           (lambda-exp '(x y) (math-exp '+ (var-exp 'x) (var-exp 'y))))
                             (set-exp 'a (math-exp '* (var-exp 'a) (lit-exp 75))) ;79
                             (bool-exp '== (var-exp 'a) (var-exp 'b))
                         
                             (app-exp (var-exp 'addStuff) (list (var-exp 'a) (var-exp 'b))) ;;84
                             
                             )))
(apply-program test-exp)
 