#lang racket




;; ======================================

(require rackunit)


#;(define-syntax-rule (check-error stx)
  (syntax-rules ()
    [(_ expr) #'(check-exn exn:fail? (λ () expr))]))

;; A Type is a Symbol

;; extend-Γ :: TypeEnv Symbol Type -> TypeEnv
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


;; ------
;; ======================
;; Tests for Symbols
;; ======================
(check-exn exn:fail? (λ ()
                       (typecheck 'y '((x Number)))))
(check-exn exn:fail? (λ ()
                       (typecheck 'x empty)))

(check-equal? (typecheck 'x '((x Number))) 'Number)


;; ======================
;; Tests for Lambda
;; ======================
(check-equal? (typecheck '(λ ([x : Number]) x) empty)  '(-> Number Number))

(check-equal? (typecheck '(λ ([n : Number])
                            (λ ([m : Number])
                              m)) empty)
              '(-> Number (-> Number Number)))

(check-equal? (typecheck '(let ([g (λ ([n : Number]) n)])
                            (let ([f (λ ([a : Number])
                                       (g (g a)))])
                              f)) empty)
             '(-> Number Number))


;; ---
(check-equal? (typecheck '(λ ([x : Number])
                            (λ ([y : Number])
                              (λ ([z : (-> Number (-> Number Number))])
                                y))) empty)
              '(-> Number
                   (-> Number
                       (-> (-> Number (-> Number Number))
                               Number))))

;; ======================
;; Tests for Application
;; ======================

(define G+ '((+ (-> Number (-> Number Number)))))

(check-equal? (typecheck '(f x) '((f (-> Number Number)) (x Number))) 'Number)

(check-equal?
 (typecheck '(let ([f (λ ([a : Number]) (add1 a))])
               (let ([y 5])
                 (f y)))
            '((add1 (-> Number Number))))
 'Number)

(check-equal? (typecheck '((+ 4) 2) '((+ (-> Number (-> Number Number)))))
              'Number)

(check-equal? (typecheck '(let ([m 4])
                            (let ([n 2])
                              ((+ m) n)))
                         G+)
              'Number)

(check-exn exn:fail?
           (λ ()
             (typecheck '(let ([m 4])
                           (let ([n x])
                             ((+ m) n)))
                        (cons (list 'x 'String) G+))))


(check-equal? (typecheck '((λ ([x : Number])
                             (λ ([y : Number])
                               (add1 x))) 0) empty)
              '(-> Number Number))


(check-equal? (typecheck
               '((((λ ([x : Number])
                  (λ ([y : Number])
                    (λ ([z : (-> Number (-> Number Number))])
                      ((z x) y)))) 5) 7)
                 (λ ([x : Number])
                   (λ ([y : Number]) y))) empty)
              'Number)

;; ======================
;; Tests for Numbers
;; ======================
(check-equal? (typecheck '6 empty) 'Number)

(check-equal? (typecheck '(let ([x 5]) x) empty) 'Number)
(check-equal? (typecheck '(let ([x 5]) (add1 x)) empty) 'Number)
(check-equal? (typecheck '(add1 5) empty) 'Number)

(check-exn exn:fail? (λ ()
                       (typecheck '(add1 m) '((m (-> Number Number))))))


#;(check-equal? (typecheck '(let ([x 5])
                              (let ([y x])
                                (+ x y))) empty) 'Number)

(typecheck '(let ([x 5])
                (let ([y x])
                  ((+ x) y))) G+)



;; ------------






