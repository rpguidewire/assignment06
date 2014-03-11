;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname beval) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

;; A boolean expression (Bexp) is either
;; * a Boolean value (true or false),
;; * a comparison expression (Cexp), or
;; * a compound boolean expression (Cbexp)

(define-struct comp-exp (fn arg1 arg2))
;; A comparison expression (Cexp) is a structure
;; (make-comp-exp f a1 a2), where
;; * f is a Symbol from the set '>, '<, and '=
;; * a1 is a Num
;; * a2 is a Num

(define-struct compoundb-exp (op args))
;; A compound boolean expression (Cbexp) is a structure
;; (make-compoundb-exp b alist), where
;; * b is a Symbol from the set 'and and 'or
;; * alist is a Cbexplist

;; A Cbexplist is either
;; * empty, or
;; * (cons e alist), where e is a Bexp and alist is a Cbexplist

;; Templates (part (a))
;(define (my-Bexp-fn Bexp)
;  (cond 
;    [(boolean? Bexp).....]
;    [(comp-exp? Bexp)(my-comp-exp-fn Bexp)]
;    [(compound-exp? Bexp)(my-compound-exp-fn Bexp)]))
;
;(define (my-comp-exp-fn Bexp)
;  (...(comp-exp-fn Bexp)...)
;  (...(comp-exp-arg1 Bexp)...)
;  (...(comp-exp-arg2 Bexp)...))
;
;(define (my-compound-exp-fn Bexp)
;  (...(compoundb-exp-op Bexp)...)
;  (...(my-Cbexplist-fn (compoundb-exp-args Bexp)))..)
;
;(define (my-Cbexplist-fn alistCbexplist)
;  (cond
;    [(empty? alistCbexplist).....]
;    [else (...(first alistCbexplist)...
;           ...(my-Cbexplist-fn (rest alistCbexplist))...)]))
    
;; bool-eval (part (b))

;;comp-expfn: Comp-exp --> Boolean 
;; The purpose of this function is to take in a Bexp of type Comp-exp and deal with it accordingly to return a boolean. 

;;Examples 
(check-expect (bool-eval (make-comp-exp '< 4 7)) true)
(check-expect (bool-eval (make-comp-exp '< 7 4)) false)

;;Function Definition 
(define (comp-expfn compexp)
  (cond 
  [(symbol=? (comp-exp-fn compexp) '<) (< (comp-exp-arg1 compexp) (comp-exp-arg2 compexp))]
  [(symbol=? (comp-exp-fn compexp) '>) (> (comp-exp-arg1 compexp) (comp-exp-arg2 compexp))]
  [(symbol=? (comp-exp-fn compexp) '=) (= (comp-exp-arg1 compexp) (comp-exp-arg2 compexp))]))

;;Tests 
(check-expect (bool-eval (make-comp-exp '= 7 4)) false)
(check-expect (bool-eval (make-comp-exp '= 7 7)) true)
(check-expect (bool-eval (make-comp-exp '> 7 4)) true)
(check-expect (bool-eval (make-comp-exp '> 4 7)) false)
(check-expect (bool-eval (make-comp-exp '< 4.3 7.2)) true)
(check-expect (bool-eval (make-comp-exp '< 7.9 4.12)) false)
(check-expect (bool-eval (make-comp-exp '= 7.3 4.2)) false)
(check-expect (bool-eval (make-comp-exp '= 7.0 7.0)) true)
(check-expect (bool-eval (make-comp-exp '> 7.3 4.1)) true)
(check-expect (bool-eval (make-comp-exp '> 0.4 7.01)) false)

;;compound-exp-fn: Compoundb-exp --> Boolean 
;; The purpose of this function is to take in a Bexp of type Compoundb-exp, compoundbexp and deal with the operator and list to 
;; return a boolean 

;; Examples
(check-expect (compound-exp-fn (make-compoundb-exp 'and empty)) true)

;;Function Definition 
(define (compound-exp-fn compoundexp)
  (cond 
    [(symbol=? (compoundb-exp-op compoundexp) 'and) 
     (cond 
       [(empty? (compoundb-exp-args compoundexp)) true]
       [else 
        (and 
         (bool-eval (first (compoundb-exp-args compoundexp)))
         (bool-eval (make-compoundb-exp
                     (compoundb-exp-op compoundexp)
                     (rest (compoundb-exp-args compoundexp)))))])]
    [(symbol=? (compoundb-exp-op compoundexp) 'or)
     (cond 
       [(empty? (compoundb-exp-args compoundexp)) false]
       [else 
        (or 
          (bool-eval (first (compoundb-exp-args compoundexp)))
          (bool-eval (make-compoundb-exp
                     (compoundb-exp-op compoundexp)
                     (rest (compoundb-exp-args compoundexp)))))])]))

;;Tests 
(check-expect (compound-exp-fn (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'and (list))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'and (list (make-comp-exp '< 5 2) true true)))false)

(check-expect (compound-exp-fn (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true false)))false)

(check-expect (compound-exp-fn (make-compoundb-exp 'and (list (make-comp-exp '< 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '= 5 2) true)))false)

(check-expect (compound-exp-fn (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true 
                                                        (make-compoundb-exp 'and 
                                                        (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list))) false)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) true true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) false false))) false)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true false)))true)
(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '= 5 2) true)))true)
(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) true 
                                                        (make-comp-exp '> 1 2) true 
                                                        (make-comp-exp '= 5 2) true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true 
                                                        (make-compoundb-exp 'and 
                                                        (list (make-comp-exp '< 5 2)false 
                                                        (make-comp-exp '< 5 2) false
                                                        (make-comp-exp '< 5 2) false))
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (compound-exp-fn (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true 
                                                        (make-compoundb-exp 'and 
                                                        (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)








;;bool-evel: Bexp --> Boolean 
;;The purpose of this function is to take in a Bexp, bexp and deal with it accordingly based on if it is a 
;; Boolean, Comp-exp or, Compountb-exp

;; Examples 
(check-expect (bool-eval true) true)
(check-expect (bool-eval (make-comp-exp '> 4 7)) false)
(check-expect (bool-eval (make-compoundb-exp 'and (list))) true)

;;Function Definition 
(define (bool-eval bexp)
  (cond 
    [(boolean? bexp) bexp]
    [(comp-exp? bexp)(comp-expfn bexp)]
    [(compoundb-exp? bexp) (compound-exp-fn bexp)]))
    

;;Tests 


(check-expect (bool-eval false) false)

(check-expect (bool-eval (make-comp-exp '< 4 7)) true)
(check-expect (bool-eval (make-comp-exp '< 7 4)) false)
(check-expect (bool-eval (make-comp-exp '= 7 4)) false)
(check-expect (bool-eval (make-comp-exp '= 7 7)) true)
(check-expect (bool-eval (make-comp-exp '> 7 4)) true)


(check-expect (bool-eval (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true true))) true)

(check-expect (bool-eval (make-compoundb-exp 'and (list))) true)

(check-expect (bool-eval (make-compoundb-exp 'and (list (make-comp-exp '< 5 2) true true)))false)

(check-expect (bool-eval (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true false)))false)

(check-expect (bool-eval (make-compoundb-exp 'and (list (make-comp-exp '< 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '= 5 2) true)))false)

(check-expect (bool-eval (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (bool-eval (make-compoundb-exp 'and (list (make-comp-exp '> 5 2) true 
                                                        (make-compoundb-exp 'and 
                                                        (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true true))) true)

(check-expect (bool-eval (make-compoundb-exp 'or (list))) false)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) true true))) true)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) false false))) false)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true false)))true)
(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '= 5 2) true)))true)
(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '< 5 2) true 
                                                        (make-comp-exp '> 1 2) true 
                                                        (make-comp-exp '= 5 2) true))) true)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true 
                                                        (make-compoundb-exp 'and 
                                                        (list (make-comp-exp '< 5 2)false 
                                                        (make-comp-exp '< 5 2) false
                                                        (make-comp-exp '< 5 2) false))
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)

(check-expect (bool-eval (make-compoundb-exp 'or (list (make-comp-exp '> 5 2) true 
                                                        (make-compoundb-exp 'and 
                                                        (list (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))
                                                        (make-comp-exp '> 5 2) true 
                                                        (make-comp-exp '> 5 2) true))) true)






