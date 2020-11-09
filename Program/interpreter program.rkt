;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |interpreter program|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
(define solution
  (lambda (list_of_classes body)
    (list 'a-solution list_of_classes body)))
(define solution?
  (lambda (exp)
    (eq? 'a-solution (car exp))))
(define get-solution-classes
  (lambda (solution)
    (cadr solution)))
(define get-solution-body
  (lambda (solution)
    (caddr solution)))
(define class
  (lambda (name parent-name list-of-fields list-of-methods)
    (list 'a-class name parent-name list-of-fields list-of-methods)))
(define class?
  (lambda (exp)
    (eq? 'a-class (car exp))))
(define get-class-name
  (lambda (class)
    (cadr class)))
(define get-class-parent-name
  (lambda (class)
    (caddr class)))
(define get-class-fields
  (lambda (class)
    (cadddr class)))
(define get-class-methods
  (lambda (class)
    (cadddr (cdr class))))
(define method
  (lambda (name list-of-parameters body)
    (list 'a-method name list-of-parameters body)))
(define method?
  (lambda (exp)
    (eq? 'a-method (car exp))))
(define get-method-name
  (lambda (method)
    (cadr method)))
(define get-method-params
  (lambda (method)
    (caddr method)))
(define get-method-body
  (lambda (method)
    (cadddr method)))
(define instance-exp
  (lambda (classname list_of_parameter_expressions)
    (list 'instance-exp classname list_of_parameter_expressions)))
(define instance-exp?
  (lambda (exp)
    (eq? 'instance-exp (car exp))))
(define instance-exp-get-classname
  (lambda (instance-exp)
    (cadr instance-exp)))
(define instance-exp-get-param-expressions
  (lambda (instance-exp)
    (caddr instance-exp)))
(define send-message-exp
  (lambda (instance-exp message list-of-parameter-expressions)
    (list 'send-message instance-exp message list-of-parameter-expressions)))
(define send-message-exp?
  (lambda (exp)
    (eq? 'send-message (car exp))))
(define send-message-exp-get-object-instance
  (lambda (send-message-exp)
    (cadr send-message-exp)))
(define send-message-exp-get-message
  (lambda (send-message-exp)
    (caddr send-message-exp)))
(define send-message-exp-get-param-expressions
  (lambda (send-message-exp)
    (cadddr send-message-exp)))

(define extend-scope
  (lambda (symbol value env)
    (list symbol value env)))

(define find-parent
  (lambda (a-class env)
      (cond
        ((null? env) 'not-found)
        ((eq? (get-class-parent-name a-class) (cadr env)) (list (cadddr env) (cadddr (cdr env))))
        (else
         (find-parent a-class (cadddr (cddr env)))))))

(define apply-scope
  (lambda (symbol env)
    (if (null? env)
        'not-found
        (if (eq? (car env) symbol)
           (cadr env)
           (apply-scope symbol (caddr env))))))

(define apply-env
  (lambda (symbol env)
    (if (null? env)
        'not-found
        (if (not(eq? (apply-scope symbol (car env)) 'not-found))
            (apply-scope symbol (car env))
            (apply-env symbol (cdr env))))))

(define apply-env-jack
  (lambda (symbol env)
    (if (null? env)
        'not-found
        (let ((result (apply-scope symbol (car env))))
              (if (eq? result 'not-found)
                  (apply-env-jack symbol (cdr env))
                  result)))))

(define push-scope
  (lambda (scope env)
    (cons scope env)))

(define pop-scope
  (lambda (env)
    (cdr env)))



(define object
  (class 'Object 'Dave '(1) '(1)))

(define get-class
  (lambda (list-of-classes a-class-name)
    (if (eq? (get-class-name (car list-of-classes)) a-class-name)
        (car list-of-classes)
        (get-class (cdr list-of-classes) a-class-name))))

(define apply-solution
  (lambda (a-solution)
    (apply-expression (get-solution-body a-solution) (list (get-solution-classes a-solution) object))))

(define apply-expression
  (lambda (body env) '()))

    
    

(define person
  (class 'Person 'Object '() '()))

;(apply-solution a-solution)