#lang typed/racket



(define-type TypeEnv (Listof (Pair Symbol Symbol)))
(define-type Id Symbol)
(define-type Type Symbol)

(define init-G '())

(: extend (-> TypeEnv Id Type
              TypeEnv))
;; extend updates the given type environment to have a
;; mapping between the given identifier and the given type
(define (extend Γ id t)
  (cons (cons id t) Γ))

(extend init-G 'x 'Nat)
(extend '((x . Nat) (y . Nat)) 'z 'String)


(: lookup (-> Id TypeEnv Type))
;; lookup returns the type that the given identifier maps to
;; an id not seen before will produce an error
(define (lookup x Γ)
  (match Γ
    [(cons (cons x0 t0) rest) (if (eqv? x0 x) t0 (lookup x rest))]
     ;; (? add an (or _ (alias? x0 x)))
    [_ (error "oops")]))



(: typecheck (-> Expr TypeEnv
                 Type))

(define (typecheck e Γ t)
  (match e
    [(? symbol? x) (lookup x Γ)]
  ))

(typecheck 'x init-G 'Symbol)



#;#;#;
(require/typed rackunit)

(check-equal? (extend init-G 'x 'Nat) '((x . Nat)))
(check-equal? (extend '((x . Nat) (y . Nat)) 'z 'String)
              '((z . String) (x . Nat) (y . Nat)))








