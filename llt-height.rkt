;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname llt-height) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;height: leaf-labelled-tree --> Nat
;; The purpose of this function is to find the maximum height
;; of the input leaf-labled-tree, llt

;;Examples 
(check-expect (height (list 2 3 4)) 1)
(check-expect (height (list (list 1 2 3) (list (list 4 5 6)))) 3)
(check-expect (height (list (list 1 2 3) (list 1 13 (list 4 5 6)))) 3)
(check-expect (height empty) 0)

;;Function Definition 
(define (height llt)
  (cond 
    [(empty? llt) 0]
    [(cons? llt)
          (max ( + 1 (height (first llt)))
                (+ 0 (height (rest llt))))]
    [(number? llt) 0]))
    

;;Tests 
(check-expect (height (list 2 3)) 1)
(check-expect (height '(())) 1)
(check-expect (height '(1 (2 (3 (4))) 5 (6 (7)) 8)) 4)
(check-expect (height (list 2 (list 1 3 ) 4)) 2)
(check-expect (height (list (list 2 (list 4 5 6 (list 10 11 12 )) (list 13 14)) 
                            (list (list 15 16 17) 18 19 (list 20 21)) (list 23 24))) 4)
(check-expect (height (list (list 2 3 (list 4 5 6)
                                      (list 7 8 9))
                            (list 10 11 (list 12 13 14)
                                        (list 15 16 17)))) 3)
