;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |homework 1.35|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;Constructor: make new objects
;;leaf constructor for simple binary tree
(define leaf
  (lambda (x) x))
(define interior-node2
  (lambda (symbol tree1 tree2)
    (append (cons symbol (list tree1)) (list tree2))))
;;interior node will a list with three things inside (a symbol and 2 binary tree)... => list can take a lot of parameters and turn them into a list
(define interior-node
  (lambda (symbol tree1 tree2)
   (list symbol tree1 tree2)))
;; check if the leaf is a number ==> boolean true or false
(define leaf?
  (lambda (x)
    (number? x)))
;;left-son
(define lson
  (lambda (binTree)
    (cadr binTree)))
;;right-son
(define rson 
  (lambda (binTree)
    (car(cdr(cdr binTree)))))
;;contents-of only gets the symbol
(define contents-of
  (lambda (binTree)
          (car binTree)))
(define testTree
  (interior-node 'red 
    (interior-node 'bar
      (leaf 26) 
      (leaf 12)) 
    (interior-node 'red 
      (leaf 11) 
      (interior-node 'quux (leaf 117) (leaf 14)))))

(define count
  (lambda (binTree)
    (if (leaf? binTree)
        1
        (+ (count (lson binTree)) (count (rson binTree))))))

(define num-leave
  (lambda (binTree)
    (num-leave-helper binTree 0)))

(define num-leave-helper
  (lambda (binTree num)
    (let ((lson (lson binTree)) (rson (rson binTree)))
      (if (leaf? binTree)
          num
          (cond
            ((leaf? lson)
             (interior-node (contents-of binTree) (num-leave-helper lson num) (num-leave-helper rson (+ num 1))))
            ((leaf? rson)
             (interior-node (contents-of binTree) (num-leave-helper lson (+ num 1)) (num-leave-helper rson num)))
            (else
              (interior-node
               (contents-of binTree)
               (num-leave-helper lson num)
               (num-leave-helper rson (+ (count lson) num)))))))))

 
(define tree
  (interior-node 'lol 19 (interior-node 'lol 56 47)))

(define tree1
  (interior-node 'lol 19 29))

(define tree2
  (interior-node 'lol (interior-node 'lol 56 47) 19))

(define tree3
  (interior-node 'lol (interior-node 'lol 19 29) (interior-node 'lol 56 47)))

(num-leave tree)
;;(num-leave tree1)
;;(num-leave tree2)
;;(num-leave tree3)

