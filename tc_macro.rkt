#lang racket

(require (for-syntax syntax/parse
                     racket/match)
         rackunit)


;; A FreeId is a FreeIdentifier (compile-time identifier)
;; A TypeEnv is a (Listof (Listof FreeId FreeId))

;; typetable is a global mapping from expressions to types
;; each time we `findtype` of an expr, we store the result
;;  | type in this table
(define-for-syntax typetable
  (make-hash))

;; Simple types for this subset of psuedo-TypedRacket
(define Number 'Number)
(define String 'String)
(define ⊥ '⊥)

;; init-Γ is for testing programs so add1 ... have known types
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
     (syntax-property runtime 'todo (vector "--------------------\n"
                                            (syntax->datum #'msg)))]))

(define-for-syntax (TC-simple expected T)
  (match expected
    (#f T)
    (E (if (free-identifier=? T E) T
           (error 'TC-simple (format "failed to check that ~a was type ~a" T expected))))))

(define-syntax (findtype stx)
  (syntax-case stx ()
    [(_  body)
     (let ()
       (define exp-body (local-expand #'body 'expression '()))
       ;; if you get bottom type, youve got absurd so you can say anything
       (define type
         (let check ([stx exp-body]
                     [Γ init-Γ]
                     [expected #f])
           (syntax-parse stx
             #:literals (let-values λ #%plain-app
                          #%expression add1 + TODO*)
             [e
              #:when (syntax-property stx 'todo)
              (define this-todo (syntax-property stx 'todo))
              (if (vector? this-todo)
                  (begin
                    ;; update the todo's information
                    (let ([new-info (string-append (env->string Γ)
                                                   (vector-ref this-todo 0)
                                                   (if expected (format "Expected Type: ~a" (syntax->datum expected)) ""
                                                   ))])
                      (vector-set! this-todo 0 new-info))
                    ;; make sure we don't see this one again
                    (check (syntax-property stx 'todo #f) Γ expected))
                  ;; -- end of begin expr
                  (raise-syntax-error 'findtype/stx-prop-todo
                   "expected TODO* to be a vector got: " stx))]

             ;; updated to switch between check and synth mode
             [x:id (TC-simple expected (lookup #'x Γ))]
             ;; new nat case
             [n:nat (TC-simple expected #'Number)]
             [(add1 n:nat) (TC-simple expected #'Number)]
             ;[(+ m:nat n:nat) #'Number]
             [(quote n:nat) (TC-simple expected #'Number)]
             [(quote s:string) (TC-simple expected #'String)]

             ;[(let-values ([(x:id) y:id]) body) (check #'body (extend-Γ Γ #'x (lookup #'y Γ)))] ;; alias case
             ;; check-mode for let
             [(let-values ([(x:id) val]) T body) (check #'body (extend-Γ Γ #'x #'T) expected)]
             ;; synth mode for let:
             [(let-values ([(x:id) val]) body) (check #'body (extend-Γ Γ #'x (check #'val Γ #f)) expected)]

             
             [(lambda (x:id) X body)
              (syntax-parse expected #:literals (quote)
                [(list (quote ->) DOM RNG) (check #'body (extend-Γ Γ #'x #'DOM) #'RNG)]
                [#f (with-syntax ([Y (check #'body (extend-Γ Γ #'x #'X) #f)])
                      #'(list (quote ->) X Y))]
                [_ (raise-syntax-error 'check/lambda (format "expected an arrow type got: ~a" expected))])]
             
             
             [(#%plain-app rator rand)
              (let ([arrow (check #'rator Γ #f)])
                (syntax-case arrow (quote)
                  ((list (quote ->) DOM RNG)
                   (let ([Y (check #'rand Γ #'DOM)])
                     (if Y #'RNG (error "bad app case"))))
                  [_ (raise-syntax-error 'check/app
                                         (format "expected operator to be an arrow type, got: ~a" arrow))]))]
             [(#%expression e) (check #'e Γ expected)]
             
             [_ (raise-syntax-error 'findtype/check "no pattern found for given stx" stx)])))
       exp-body)]))

;; map exp-body to the result of findtype in the global type-table


 ;; uncomment and make sure find-type returns exp-body in order to see the TODO window
(findtype
 (let ([x 5])
   (let ([y 7])
     (let [[z y]]
       (add1 (TODO* "ex1"))))))

(findtype (TODO* "a"))



 ;; this should say Number


;; should be printing (≡ x Number) (≡ y Number)

;; get the gui thing working again so if a TODO* is
;; seen it'll pop up and say what it's type is

;; numbers
(check-equal? (findtype 5)
              'Number)

(check-equal? (findtype (add1 5))
              'Number)
;;; let
(check-equal? (findtype (let ([x 5]) x))
              'Number)

(check-equal? (findtype (let ([x 5]) (add1 x)))
              'Number)

(check-equal? (findtype (let ([x 5]) Number (add1 x)))
              'Number)
;
;; lambda
(check-equal? (findtype (λ (x) Number x))
              '(-> Number Number))


(check-equal? (findtype (λ (x) Number
                            (λ (y) Number
                              y)))
              '(-> Number (-> Number Number)))

;(findtype (λ (x y) (-> Number Number)))
;
;;; application
;
(check-equal? (findtype ((λ (x) Number (add1 (add1 x))) 5))
              'Number)

(check-equal? (findtype
               ((λ (x) Number
                  (λ (y) Number
                    y)) 2))
              '(-> Number Number))

(check-equal?
 (findtype (let ([f (λ (x) Number
                      (λ(y) String
                        y))])
             (f 2)))
 '(-> String String))

#;
(check-equal? (findtype
               (let ([f (λ(x) Number
                          (λ (y) String
                            (add1 x)))]) (list (quote ->) Number (list (quote ->) String Number))
                 ((f 2) "dog")))
              'Number)
