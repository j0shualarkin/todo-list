#lang racket




;; is Γ in check function still a list of pairs of symbols
;; or is it stx-objs?


;; ========== temporary =============
#;#;#;#;#;#;
(define (extend-Γ G id type)
  (cons (list id type) G))

(define (lookup x Γ)
  (match Γ
    [`((,x0 ,t0) . ,rest) (if (eqv? x0 x) t0 (lookup x rest))]
    [_ (error 'lookup "could not find a type for var ~a" x)]))



(define-syntax (lookup x Γ)
  (syntax-case Γ ()
    [empty (error 'lookup "could not find a type for var ~a" x)]
    [((e t) . d) #:when (eqv? e x) t]))

(define (check/app rator-type rand-type)
  (match rator-type
    [`(-> ,A ,B) (if (equal? A rand-type) B
                     (error 'check/app "expected rand to be a ~a, got ~a" A rand-type))]
    [_ (error 'check/app "expected rator to be a (-> A B), got:" rator-type)]))

(define (check/add1 t)
  (if (eqv? t 'Number)
      'Number
      (error 'add1 "expected a number, given: ~a" t)))

;; typecheck :: Expr TypeEnv -> Type 
(define (typecheck expr Γ)
  (match expr
    [(? symbol? x) (lookup x Γ)]
    [(? number? x) 'Number]
    [`(add1 ,n) (check/add1 (typecheck n Γ))]
    [`(let ([,id ,val]) ,body) (typecheck body (extend-Γ Γ id (typecheck val Γ)))]
    [`(λ ([,x : ,X]) ,body) `(-> ,X ,(typecheck body (extend-Γ Γ x X)))] 
    [`(,rator ,rand) (check/app (typecheck rator Γ) (typecheck rand Γ))]
    ))

(define (check/app rator-type rand-type)
  (match rator-type
    [`(-> ,A ,B) (if (equal? A rand-type) B
                     (error 'check/app "expected rand to be a ~a, got ~a" A rand-type))]
    [_ (error 'check/app "expected rator to be a (-> A B), got:" rator-type)]))




(begin-for-syntax (require syntax/parse racket/match)
                  (define (extend-Γ G id type)
                    (cons (list id type) G))
                  (define (lookup x Γ)
                    (match Γ
                      ['() (error 'lookup "no type associated to: ~a" x)]
                      [(cons (list a t) d) #:when (free-identifier=? x a) t]
                      [(cons pr d) (lookup x d)]))
                  (define (check/app t1 t2)
                    (match t1
                      [`(-> ,A ,B) (if (equal? A t2) B (error 'check/app "illtyped operand"))]
                      [A (error "illtyped operator")])))





(define-syntax (findtype stx)
  (syntax-case stx ()
    [(_  body)
     (let ()
       (define exp-body (local-expand #'body 'expression '()))
       (print exp-body)
       (define Γ '())
       (let check ([stx exp-body]
                   [Γ Γ])
         (syntax-parse stx #:literals (let-values add1 #%plain-app λ)
           [x:id (lookup #'x Γ)]
           [n:nat #'Number]
           [(quote n:nat) #'Number]
           [(add1 n:nat) #'Number]
           [(let-values ([(x:id) val]) body) (check #'body (extend-Γ Γ #'x (check #'val Γ)))]
           [(λ ([x : X]) body) #'(-> X #,(check body (extend-Γ Γ x X)))]
           [(#%plain-app rator rand) (check/app (check #'rator Γ) (check #'rand Γ))]
           [_ (error "no pattern found for given stx:" stx)]))
       exp-body)]))

(require rackunit)

(check-equal? (findtype (let ([x 5]) x)) #'Number)



















