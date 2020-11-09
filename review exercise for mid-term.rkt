;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |review exercise for mid-term|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 1.15
(define duple
  (lambda (count lst)
    (if (= count 0)
        '()
        (cons lst (duple (- count 1) lst)))))
;Excercis 1.16
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (reverse (car lst)) (invert (cdr lst))))))
;Exercise 1.17
(define down
  (lambda (lst)
    (if (null? lst)
    '()
    (cons (list (car lst)) (down (cdr lst))))))
;Exercise 1.18
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))
            (if (eqv? s1 (car slist))
                (cons s2 (swapper s1 s2 (cdr slist)))
                (if (eqv? s2 (car slist))
                    (cons s1 (swapper s1 s2 (cdr slist)))
                    (cons (car slist) (swapper s1 s2 (cdr slist)))))))))
(define swapper2
  (lambda (s1 s2 slist)
    (cond
      ((null? slist) '())
      ((list? (car slist)) (cons (swapper2 s1 s2 (car slist)) (swapper2 s1 s2 (cdr slist))))
      ((eqv? s1 (car slist)) (cons s2 (swapper2 s1 s2 (cdr slist))))
      ((eqv? s2 (car slist)) (cons s1 (swapper2 s1 s2 (cdr slist))))
      (else
       (cons (car slist) (swapper s1 s2 (cdr slist)))))))
;Exercise 1.19
(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))
;Exercise 1.20
(define count-helper
  (lambda (s slist)
    (if (symbol? slist)
        (if (eqv? slist s) 1 0)
        (count s slist))))
(define count
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-helper s (car slist)) (count s (cdr slist))))))
;Exercise 1.21
(define product-helper
  (lambda (s los2)
    (if (null? los2)
        '()
        (cons (list s (car los2)) (product-helper s (cdr los2))))))
(define product
  (lambda (los1 los2)
      (if (or (null? los1) (null? los2))
              '()
              (append (product-helper (car los1) los2) (product (cdr los1) los2)))))
;Exercise 1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
;Exercise 1.23
(define list-index-helper
  (lambda (pred lst count)
    (if (null? lst)
        #f
        (if (pred (car lst))
            count
            (list-index-helper pred (cdr lst) (+ count 1))))))
(define list-index
  (lambda (pred lst)
    (list-index-helper pred lst 0)))
;Exercise 1.24
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))
;Exercise 1.25        
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))
;Exercise 1.26
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (car lst) (up (cdr lst)))
            (cons (car lst) (up (cdr lst)))))))
;Exercise 1.27
(define flatten
  (lambda (lst)
    (if (null? lst)
        '()
        (if (symbol? (car lst))
            (cons (car lst) (flatten (cdr lst)))
            (if (> (length (car lst)) 2)
                (append (car lst) (flatten (cdr lst)))
                (append (flatten (car lst)) (flatten (cdr lst))))))))
;define 1.28
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((< (car loi1) (car loi2))
          (cons (car loi1) (merge (cdr loi1) loi2)))
      (else
          (cons (car loi2) (merge loi1 (cdr loi2)))))))
;Exercise 1.29 ******
(define splitList
  (lambda (lst0 lst1 splitPos)
    (if (= splitPos 0)
        (list (reverse lst0) lst1)
        (splitList
         (cons (car lst1) lst0)
         (cdr lst1)
         (- splitPos 1)))))
(define sliceAndDice
  (lambda (lst)
    (splitList
     '() lst (quotient (length lst) 2))))
(sliceAndDice '(1 3 2 7 4))
    
(define msort
  (lambda (lst)
    (cond
      ((> (length lst) 1)
       (merge
        (msort (car (sliceAndDice lst)))
        (msort (cadr (sliceAndDice lst)))))
      (else lst))))
;Excercise 1.31 (leaf, interior-node --> build a bin tree, leaf?, lson, rson,
;contents-of --> extract the components of a node)
(define leaf
  (lambda (num)
    num))
(define interior-node
  (lambda (symbol binTree1 binTree2)
    (list symbol binTree1 binTree2)))
(define leaf?
  (lambda (num)
    (number? num)))
(define lson
  (lambda (interior-node)
    (cadr interior-node)))
(define rson
  (lambda (interior-node)
    (caddr interior-node)))
(define contents-of
  (lambda (interior-node)
    (if (leaf? interior-node)
        interior-node
        (car interior-node))))
;Exercise 1.32
(define double-tree
  (lambda (binTree)
    (if (leaf? binTree)
        (leaf (* binTree 2))
        (interior-node
         (contents-of binTree)
         (double-tree (lson binTree))
         (double-tree (rson binTree))))))
;Exercise 1.32











































































  

