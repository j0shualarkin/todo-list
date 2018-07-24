#lang typed/racket #:with-refinements

(require typed/rackunit
         (only-in racket/unsafe/ops
                  unsafe-vector-ref
                  unsafe-vector-set!))

(define-syntax (TODO* stx)
  (syntax-case stx ()
    [(_ msg)
     ;; Attach a notice that it is a TODO to be filled out
     (syntax-property
      (syntax/loc stx
        (error msg))
      'todo
      (vector "--------------------\n"
              (syntax->datum #'msg)))]))

#;
(let ([x 5])
  (let ([y 6]) 
    (let ([z x])
      (+ x 6 (ann (TODO* "oops") Number)))))

#;#;

(: foo (-> Real Real))
(define (foo x)
  (if (<= x 12) (let ((y (+ x 2)))
                  (/ y 2))
      (ann (TODO* "help!") Real)))






(: safe-vector-ref
   (All (A) (-> ([v : (Vectorof A)]
                 [n : (v) (Refine [i : Integer]
                                  (<= 0 i (- (vector-length v) 1)))])
                A)))
(define safe-vector-ref
  unsafe-vector-ref)

(: safe-vector-set!
   (All (A)
        (-> ([v : (Vectorof A)]
             [n : Natural]
             [a : A])
             #:pre (v n)
             (< n (vector-length v))
             Void)))
(define safe-vector-set! unsafe-vector-set!)


;; ========


(: swap (All (E)
             (-> ([vec : (Vectorof E)]
                  [i : (vec) (Refine [i : Integer]
                                     (<= 0 i (- (vector-length vec) 1)))]
                  [j : (vec) (Refine [j : Integer]
                                     (<= 0 j (- (vector-length vec) 1)))])
                 Void)))

;; swaps data at index i with data at index j 
(define (swap v i j)
  (let ([to-be-moved (safe-vector-ref v i)])
    (safe-vector-set! v i (safe-vector-ref v j))
    (safe-vector-set! v j to-be-moved)))



(: shift-left
   (All (E)
        (-> (Vectorof E)
            (Vectorof E))))
;; shift-left 
;;;; moves each item in the vector left once
;;;; putting the first element at the end

(define (shift-left v)
  (let ([len (vector-length v)])
    (cond
      [(<= len 1) v]
      [else (let loop
              ([i : (Refine [i : Integer]
                            (<= 0 i (- len 1))) 0])
              (when (< i (- len 1))
                
                (TODO* "in the loop")
                (swap v i (+ i 1))
                (loop (+ i 1)))
              v)])))



;
;(check-equal? (shift-left (vector 1 2 3)) (vector 2 3 1))
;(check-equal? (shift-left (vector)) (vector))
;(check-equal? (shift-left (vector 42)) (vector 42))
;(check-equal? (shift-left (shift-left (vector 12 62 13 45)))
;             (vector 13 45 12 62))
;
;; ===================================================================

#;
(: shift-right : (All (E)
                  (-> (Vectorof E)
                      (Vectorof E))))
#;
(define (shift-right v)
  (let ([len (vector-length v)])
    (cond
      [(<= len 1) v]
      [else (let loop
              ([i : (Refine [i : Integer]
                            (<= 0 i (- len 1)))
                  (- len 1)])
              
              (when (>= (- i 1) 0)
                (swap v i (- i 1))
                (loop (- i 1)))
              
              v)])))

#;
(check-equal? (shift-right (vector 'a 'b 'c 'd 'e))
             '#(e a b c d))
#;
(check-equal? (shift-right (shift-right (vector 'a 'b 'c 'd 'e)))
              '#(d e a b c))



