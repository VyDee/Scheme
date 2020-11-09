;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |homework 1.27 and 1.28 and 1.29|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Homeworl 1.27
(define flatten
  (lambda (lst)
    (if (null? lst)
        '()
        (if (not(list? (car lst)))
            (cons (car lst) (flatten (cdr lst)))
            (append (flatten (car lst)) (flatten(cdr lst)))))))

;; (flatten '(((d)) e))

;;Home work 1.28
(define merge
  (lambda (loi1 loi2 pred)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((pred (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2 pred)))
      (else (cons (car loi2) (merge (cdr loi2) loi1 pred))))))

;;(merge '(1 4) '(1 2 8))
;;(merge '(35 62 81 90 91) '(3 83 85 90))

;;Homework 1.29 and 1.30
(define spilt-head
  (lambda (lst n1 n2)
    (if (= n2 n1)
        '()
        (cons (car lst) (spilt-head (cdr lst) n1 (- n2 1))))))

(define spilt-tail
  (lambda (lst n n1 n2)
    (if (< n n1)
        (spilt-tail (cdr lst) (+ n 1) n1 n2)
        (if (< n n2)
            (cons (car lst) (spilt-tail (cdr lst) (+ n 1) n1 n2))
            '()))))
(define msort
  (lambda (pred lst)
    (if (> (length lst) 1)
        (merge (msort pred (spilt-head lst 0 (quotient (length lst) 2)))
               (msort pred (spilt-tail lst 0 (quotient (length lst) 2) (length lst))) pred)
        lst)))



