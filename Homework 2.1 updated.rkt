;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Homework 2.1 updated|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define zero
  (lambda () '(0)))

(define is-zero?
  (lambda (lst)
    (null? '())))

(define conversion-lst '(0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(define index-of
  (lambda (symbol lst n)
    (if (eqv? symbol (car lst))
        n
        (index-of symbol (cdr lst) (+ n 1)))))

;;(index-of 0 conversion-lst 0)

(define char-at 
  (lambda (num lst)
    (if (= num 0)
        (car lst)
        (char-at (- num 1) (cdr lst)))))
(char-at 17 conversion-lst)

(define N 32)

;;index-of(symbol) : number
;;char-at(number): symbol

(define successor
  (lambda (lst)
    (cond
      ((null? lst) '(1))
      ((< (index-of (car lst) conversion-lst 0) (- N 1)) (cons (char-at (+ (index-of (car lst) conversion-lst 0) 1) conversion-lst) (cdr lst)))
      (else (cons 0 (successor (cdr lst)))))))

;;(successor '(0 0))

(define list-zero
  (lambda (lst)
    (if (null? lst)
        '()
        (if (zero? (index-of (car lst) conversion-lst 0))
            (list-zero (cdr lst))
            #f))))


(define precessor
  (lambda (lst)
    (if (eqv? (list-zero lst) '())
        lst
        (cond
          ((> (index-of (car lst) conversion-lst 0) 0) (cons (char-at (- (index-of (car lst) conversion-lst 0) 1) conversion-lst) (cdr lst)))
          (else (cons (char-at (- N 1) conversion-lst) (precessor (cdr lst))))))))
(precessor '(0 0 0 8))













      