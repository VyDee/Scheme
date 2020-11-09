;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |updated version of occur-free and occur-bound|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define para-occur?
  (lambda (para s)
    (if (null? para)
      #f
      (if (equal? (car para) s)
        #t
        (para-occur? (cdr para) s)))))

(define multi-occurs-bound?
  (lambda (exp s)
    (cond
      ((symbol? exp) #f)
      ((eq? (car exp) 'lambda)
       (let ((boundVarName (cadr exp))
             (body (caddr exp)))
         (if (para-occur? boundVarName s)
             (occurs? body s)
             #f)))
      (else
       (let ((operator (car exp))
             (operand (cadr exp)))
         (or (multi-occurs-bound? operator s)
             (multi-occurs-bound? operand s)))))))







(define occurs-free?
  (lambda (exp s)
    (cond
      ((var-exp? exp) (eq? s exp))
      ((lambda-exp? exp)
       (let ((bound-var (caadr exp))
             (body (caddr exp)))
         (if (eq? bound-var s)
             #f
             (occurs-free? body s))))
      (else
       (or (occurs-free? (car exp) s)
           (occurs-free? (cadr exp) s))))))
(define para-not-occur?
  (lambda (para s)
    (if (null? para)
      #t
      (if (equal? (car para) s)
        #f
        (para-not-occur? (cdr para) s)))))
        
(define multi-occurs-free?
 (lambda (exp s)
   (cond
     ((var-exp? exp) (eq? s exp))
     ((lambda-exp? exp)
      (let ((bound-var (cadr exp))
            (body (caddr exp)))
        (if (para-not-occur? bound-var s)
            (multi-occurs-free? body s)
            #f)))
     (else
      (or (multi-occurs-free? (car exp) s)
          (multi-occurs-free? (cadr exp) s))))))
    
(define theEnv (extend-env* '(a b c) '(1 2 3) (empty-env)))
(define expr (lambda-exp 'z (app-exp (lambda-exp 'a 'z) 'z)))
;;(occurs-free? expr 'z)
expr
;;(multi-occurs-free? expr 'c)