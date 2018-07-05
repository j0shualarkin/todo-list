#lang racket



(begin-for-syntax (require syntax/parse racket/match)
                  (define (extend-Γ G id type)
                    (cons (list id type) G))
                  (define (lookup x Γ)
                    (match Γ
                      ['() (error 'lookup "no type associated to: ~a" x)]
                      [(cons (list a t) d) #:when (free-identifier=? x a) t]
                      [(cons pr d) (lookup x d)]))

                  
                  (define (check/app t1 t2)
                    #;(match t1 ;; can't match on syntax....
                      [`(-> ,A ,B) (if (equal? A t2) B (error 'check/app "illtyped operand"))]
                      [A (error "illtyped operator" t1)])
                    (syntax-case t1 ()
                      [(-> A B) (if (free-identifier=? #'A t2) #'B (error "oops" #'A t2 #'B))])))


(define-syntax (findtype stx)
  (syntax-case stx ()
    [(_  body)
     (let ()
       (define exp-body (local-expand #'body 'expression '()))
       (define Γ (list (list #'add1 #'(-> Number Number))))
       (let check ([stx exp-body]
                   [Γ Γ])
         (syntax-parse stx #:literals (let-values λ #%plain-app)
           [x:id (lookup #'x Γ)]
           [n:nat #'Number]
           [(quote n:nat) #'Number]
           [(let-values ([(x:id) val]) body) (check #'body (extend-Γ Γ #'x (check #'val Γ)))]
           [(λ ([x : X]) body) #'(-> X #,(check #'body (extend-Γ Γ #'x #'X)))]
           [(#%plain-app rator rand) (check/app (check #'rator Γ) (check #'rand Γ))]
           [_ (error "no pattern found for given stx:" stx)]))
       exp-body)]))

(require rackunit)

;; numbers
(check-equal? (findtype 5)
              #'Number)

(check-equal? (findtype (add1 5))
              #'Number)

;; let
(check-equal? (findtype (let ([x 5]) x))
              #'Number)

(check-equal? (findtype (let ([x 5]) (add1 x)))
              #'Number)

;; lambda
(check-equal? (findtype (λ ([x : Number]) x))
              #'(-> Number Number))

(check-equal? (findtype (λ ([x : Number])
                          (λ ([y : Number])
                            y)))
              #'(-> Number (-> Number Number)))

;; application
(check-equal? (findtype ((λ ([x : Number]) (add1 (add1 x))) 5))
              #'Number)

(check-equal? (findtype ((λ ([x : Number])
                          (λ ([y : Number])
                            y)) 2))
              #'(-> Number Number))




















