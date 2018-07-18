#lang racket

(require (for-syntax syntax/parse
                     racket/match))


;; A FreeId is a FreeIdentifier (compile-time identifier)
;; A TypeEnv is a (Listof (Listof FreeId FreeId))

(begin-for-syntax (define typetable (make-hash)))

;; Simple types for this subset of psuedo-TypedRacket
(define Number 'Number)
(define String 'String)
(define ⊥ '⊥)

;; init-Γ is for testing programs so add1 is known to have a type
(define-for-syntax init-Γ 
  (list (list #'add1  #'(list (quote ->) Number Number))
        (list #'+     #'(list (quote ->) Number Number Number))
        (list #'error #'(list (quote ->) String ⊥))))

;; extend-Γ :: TypeEnv FreeId FreeId -> TypeEnv
;; adds a binding in the given environment between the given term and type 
(define-for-syntax (extend-Γ Γ a τa)
  (cons (list a τa) Γ))

;; lookup :: FreeId TypeEnv -> [Maybe FreeId]
;; returns the associated type for the given term,
;; throws an error if there is no binding in the environment
(define-for-syntax (lookup x Γ)
  (match Γ
    ['() (error 'lookup "unbound identifier: ~a" x)]
    [(cons (list a τa) _) #:when (free-identifier=? x a) τa]
    [(cons _ d) (lookup x d)]))



;; check/app :: FreeId FreeId -> [Maybe FreeId]
;; returns the range (type) of the given operator when
;;  the first term is an arrow type and 
;;   the second term is of the domain
;; otherwise an error is thrown
(define-for-syntax (check/app τ-rator τ-rand)
    (syntax-case τ-rator (quote) 
      [(list (quote ->) A B)
       (if (or (free-identifier=? #'A τ-rand)
               (free-identifier=? #'⊥ τ-rand))
           #'B
           (raise-syntax-error 'check/app (format "expected rand type: ~a, got ~a"   #'A τ-rand) τ-rand))]
      [_ (error 'check/app "expected rator type (-> A B), got: ~a" τ-rator)]))


(define-for-syntax (fixup T)
  (syntax->datum T
   #;(syntax-parse T #:literals (list quote ->)
     ((list (quote ->) t0 ... t) #'(t0 ... -> t)))
   ))

;; env->string :: TypeEnv -> String
;; returns a string of all the variables in scope with their types
(define-for-syntax (env->string Γ)
  (match Γ
    ((list) "")
    ((list (list x0 T) pr ...) (string-append (format "(≡ ~a ~a)\n"
                                                      (syntax->datum x0)
                                                      (fixup T))
                                               (env->string pr)))))


(define-syntax (TODO* stx)
  (syntax-parse stx
    [(_ msg:str)
     ;; Expand a TODO to a runtime error
     (define runtime
       (syntax/loc stx
         (error msg)))
     ;; Attach a notice that it is a TODO to be filled out
     (syntax-property runtime 'todo (vector ":-)"
                                            (syntax->datum #'msg)))]))


(define-syntax (findtype stx)
  (syntax-case stx ()
    [(_  body)
     (let ()
       (define exp-body (local-expand #'body 'expression '()))
       ;; if you get bottom type, youve got absurd so you can say anything
       (define type
         (let check ([stx exp-body]
                     [Γ init-Γ])
           (syntax-parse stx
             #:literals (let-values λ #%plain-app
                          #%expression add1 + TODO*)
             [e
              #:when (syntax-property stx 'todo)
              (define this-todo (syntax-property stx 'todo))
              (eprintf "pre: ~a\n" this-todo)
              ;(displayln (env->string Γ))
              (if (vector? this-todo)
                  (begin
                    ;; update the todo's information
                    (let ([new-info (string-append (env->string Γ)
                                                   (vector-ref this-todo 0))])
                      (eprintf "new info: ~a\n" new-info)
                      (vector-set! this-todo 1 "not oops")
                      (vector-set! this-todo 0 new-info))
                    (eprintf "post: ~a\n" this-todo)
                    
                    ;; make sure we don't see this one again
                    #;(check (syntax-property stx 'todo #f) Γ)
                    #'⊥)
                  
                  (raise-syntax-error 'findtype/stx-prop-todo
                   "expected TODO* to be a vector got: " stx))]
             [x:id (lookup #'x Γ)]
             [n:nat #'Number]
             [(add1 n:nat) #'Number]
             ;[(+ m:nat n:nat) #'Number]
             [(quote n:nat) #'Number]
             [(quote s:string) #'String]
             [(let-values ([(x:id) y:id]) body) (check #'body (extend-Γ Γ #'y (lookup #'x Γ)))] ;; alias case
             [(let-values ([(x:id) val]) body) (check #'body (extend-Γ Γ #'x (check #'val Γ)))]
             [(lambda (x:id) X body)
              (with-syntax ([Y (check #'body (extend-Γ Γ #'x #'X))])
                #'(list (quote ->) X Y))]
             [(#%expression (lambda (x:id) X body))
              (with-syntax ([Y (check #'body (extend-Γ Γ #'x #'X))])
                #'(list (quote ->) X Y))]
             [(#%plain-app rator rand) (check/app (check #'rator Γ) (check #'rand Γ))]
             [_ (raise-syntax-error 'findtype/check "no pattern found for given stx" stx)])))
       #'body)]))



(require rackunit)
(findtype (let ([x 5])
            (let ([y 7])
              (add1 (TODO* "oops")))))




 ;; this should say Number


;; should be printing (≡ x Number) (≡ y Number)

;; get the gui thing working again so if a TODO* is
;; seen it'll pop up and say what it's type is
;
;;; numbers
;(check-equal? (findtype 5)
;              'Number)
;
;(check-equal? (findtype (add1 5))
;              'Number)
;;; let
;(check-equal? (findtype (let ([x 5]) x))
;              'Number)
;
;(check-equal? (findtype (let ([x 5]) (add1 x)))
;              'Number)
;
;;; lambda
;(check-equal? (findtype (λ (x) Number x))
;              '(-> Number Number))
;
;
;(check-equal? (findtype (λ (x) Number
;                            (λ (y) Number
;                              y)))
;              '(-> Number (-> Number Number)))
;#;
;(findtype (λ (x y) (-> Number Number)))
;
;;; application
;
;(check-equal? (findtype ((λ (x) Number (add1 (add1 x))) 5))
;              'Number)
;
;(check-equal? (findtype
;               ((λ (x) Number
;                  (λ (y) Number
;                    y)) 2))
;              '(-> Number Number))
;
;; sorry im late but it was for a good cause,
;; i just had half of the most delicious cinnamon roll
;
;
;#|
;    notes on aliasing:
;
;   how to know that we've come across an alias later
;   if we just mark y with the type we found for x
;
;|#
