#lang racket

(define (extend-Γ G id type)
  (cons (list id type) G))

;; lookup :: Symbol TypeEnv -> Type
(define (lookup x Γ)
  (match Γ
    [`((,x0 ,t0) . ,rest) (if (eqv? x0 x) t0 (lookup x rest))]
    [_ (error 'lookup "could not find a type for var ~a" x)]))


#|
  Γ |- e1 : (-> A B)     Γ |- e2 : A
--------------------------------------
          Γ |- (e1 e2) : A
|#

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
