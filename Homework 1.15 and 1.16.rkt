;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Homework 1.15 and 1.16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (duple (- n 1) x)))))

(define i '((a 1) (a 2) (1 b) (2 b)))

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (append (cdr (car lst)) (list(car(car lst))))
          (invert (cdr lst))))))
