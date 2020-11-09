;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |final exam|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;((1 2 3) (4 5) (6) (7 8 9 10)) -> (6 9 6 34)

(define sum-one-lst
  (lambda (lst)
    (if (null? lst)
        0
        (+ (car lst) (sum-one-lst (cdr lst))))))
(define sum-lsts
  (lambda (lsts)
    (if(null? lsts)
       '()
       (cons (sum-one-lst (car lsts)) (sum-lsts (cdr lsts))))))
;(sum-lsts '((1 2 3) (4 5) (6) (7 8 9 10)))


;;removeSymbols
; '(1 2 s t y) => '(1 2)

(define removeSymbols
  (lambda (lst)
    (if (null? lst)
        '()
        (if (symbol? (car lst))
            (removeSymbols (cdr lst))
            (cons (car lst) (removeSymbols (cdr lst)))))))

;(removeSymbols '(1 2 s t 3))


;findMax
;'(9 4 10) => 10
(define compare
  (lambda (num1 num2)
    (if (> num1 num2)
        num1
        num2)))

(define findMax-helper
  (lambda (lst num)
    (if (= (length lst) 0)
        num
        (findMax-helper (cdr lst) (compare (car lst) num)))))


(define findMax
  (lambda (lst)
    (findMax-helper lst (car lst))))

;(findMax '(-1 -4 -7 -2 -10 -20 -9 -3 -100000))


;Maxiums
;'((1 2 3) (6 4 5) (8 9)) -> '(3 6 9)

(define maximums
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (findMax (car lst)) (maximums (cdr lst))))))

;(maximums '((1 2 3) (6 4 5) (8 9)))


;;Decimal to Binary        
(define decimalToBinary-helper
  (lambda (num)
    (if (= (quotient num 2) 0)
        '(1)
        (cons (modulo num 2) (decimalToBinary-helper (quotient num 2))))))
(define decimalToBinary
  (lambda (num)
    (reverse (decimalToBinary-helper num))))
            

;(decimalToBinary 13)


;Binary to Decimal
;'(1 1 0 1)

(define exponent
  (lambda (num times)
    (if(= times 0)
       1
       (* num (exponent num (- times 1))))))

(define binaryToDecimal-helper
  (lambda (lonum base count) ;(1 1 0 1)
    (if (null? lonum)
        0
        (+ (* (car lonum) (exponent base count)) (binaryToDecimal-helper (cdr lonum) base (- count 1))))))

(define binaryToDecimal
  (lambda (lonum)
    (binaryToDecimal-helper lonum  2 (- (length lonum) 1))))

(binaryToDecimal '(1 1 0 1))
















    


                















     