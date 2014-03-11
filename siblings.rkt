;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname siblings) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;;num-siblings: Leaf-labelled tree  Num --> Nat

;;The purpose of this function is to take in a leaf-labelled tree and a Num 
;; and find how many numbers accompany the given number in a list. 

;; Examples

;;Function Defintion 
(define (num-siblings-h llt num)
  (cond
    [(empty? llt) false]
    [(number? (first llt))
     (cond 
       [(equal? num (first llt)) 0]
       [else (num-siblings-h (rest llt) num) ])]
    [(cons? (first llt)) 
       (cond 
         [(member? num (first llt)) (- (length (first llt)) 1)]
         [else (list
                     (num-siblings-h (first llt) num)
                     (num-siblings-h (rest llt) num))])]))

;
;(define (find-and input1
;    (cond 
;      [(number? input1) input1]
;      
;         
;         
;         
;(define (num-siblings llt num)
;  (find-ans (num-siblings-h llt num)))
