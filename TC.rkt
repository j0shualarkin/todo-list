#lang racket




;; ======================================

(require rackunit)

(define init-G '())


(define (extend-Γ G id val)
  (if (symbol? val) (cons (list id 'Alias) G)
      (cons (list id val) G)))

;; typecheck :: Expr -> TypeEnv -> Type 
(define (typecheck expr Γ)
  (match expr
    [(? symbol? x) 'Atom]
    [(? number? x) 'Number]
    [`(let ([,id ,val]) ,body) (typecheck body (extend-Γ Γ id val))]
    [`(+ ,m ,n) (check-+ m n Γ)]
    ))



(check-equal? (typecheck (let ([x 5]) x) init-G) 'Number)
(check-equal? (typecheck (let ([x 5])
                           (let ([y x])
                             (+ x y))) init-G) 'Number)


;; ------------

(define (check-+ n1 n2 G)
  (cond
    [(number? n1) (if (number? n2) 'Number (error "oops"))]
    [else (error "oops")]))




