#lang typed/racket



(define-type TypeEnv (Listof (Pairof Symbol Symbol)))
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
  (let ([maybe-type : (U (Pairof Symbol Symbol) False) (assoc x Γ)])
    (if maybe-type (cdr maybe-type)
        (error 'lookup "could not find a type for ~a in ~a" x Γ))))

(: find-+ (-> Type Type Type))
;; helper function to findtype
;; (+ n1 n2) is a Number when n1 is a Number and n2 is a Number
(define (find-+ t1 t2)
  (match* (t1 t2)
    [('Number 'Number) 'Number]
    [(_ _) (error '+ "expects two Numbers got: ~a ~a" t1 t2)]))

(: findtype (-> Any TypeEnv Type))
;; takes a program and returns its type
;; traverses program and builds a type environment of all terms 
(define (findtype e Γ)
  (match e
    [(? symbol? x) (lookup x Γ)]
    [(? number? x) 'Number]
    ;[`(: ,x ,t) (extend Γ x t)]
    ;[`(let ([,id : ,t ,v]) ,b) (findtype b (extend Γ id t))]
    [`(+ ,n1 ,n2) (find-+ (findtype n1 Γ) (findtype n2 Γ))]))





#;#;#;
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








