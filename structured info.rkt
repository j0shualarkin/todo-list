#lang racket

(require (for-syntax syntax/parse
                     racket/match
                     racket/format))


;; Custom Writing for the info struct, used
;; to hold lexical env information

(define (info-print info port mode)
  (let* ([TYPES (string-append "Types: " (info-type info))]
         [PROPS (string-append "Props: " (info-prop info))]
         [ALIAS (string-append "Alias: " (info-alias info))])
    (writeln TYPES port)
    (writeln PROPS port)
    (writeln ALIAS port)))

(struct info (type prop alias)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc info-print)])


(define sample-info (info "(: x Int), (: y Int)" "" "(≡ x y)"))
(displayln sample-info)

(set-info-prop! sample-info "(> x 0)")
(display sample-info)


;; =================================================================

(begin-for-syntax
  ;; structures for TODO and showing the environment
  (struct command (name module-path function arguments) #:prefab)
  (struct env (types props aliases) #:transparent)


  (define (info-print info port mode)
    (writeln "Types" port)
    (writeln (info-type info) port)
    (writeln "Props" port)
    (writeln (info-prop info) port)
    (writeln "Aliases" port)
    (writeln (info-alias info) port))

  (struct info (type prop alias)
    #:mutable
    #:methods gen:custom-write
    [(define write-proc info-print)])
  
  (define (init-info)
    (info "" "" ""))
  
  ;; starting 
  (define lexical-env (make-parameter (env '() '() '())))

  ;; helper functions
  (define (clean-up xs sym)
    (map (λ (p)
           (match p
             [`(,x . ,t) `(,sym ,x ,t)])) xs))
  
  (define (build-info nm ty pr al)
    (let ([smpl-str (λ (id x)
                      (string-append id ": " x "\n"))])
      (string-append (smpl-str "Message" nm)
                     (smpl-str "Types" (~a ty))
                     (smpl-str "Props" (~a pr))
                     (smpl-str "Aliases" (~a al))))))


(define-syntax (TODO* stx)
  (syntax-parse stx
    [(_ msg:str) 
     (define runtime
       (syntax/loc stx
         (error msg)))
     
     (syntax-property runtime 'todo
                       (let ([le (lexical-env)])
                         (build-info (syntax->datum #'msg)
                                     (clean-up (env-types le) ':)
                                     (env-props le)
                                     (clean-up (env-aliases le) '≡))))]))


(define-syntax (let^ stx)
  (syntax-case stx (:)
    ((_ ((x y)) bodies ...)
     (and (identifier? #'x) (identifier? #'y))
     ;; alias case
     #;#;
     (define new-types (cons (cons (syntax-e #'x)
                                   (cdr (assv (syntax-e #'y)
                                              (info-types) ;; <---
                                              )))))
     (define new-alias (cons (cons (syntax-e #'x)
                                   (syntax-e #'y))
                             (info-alias) ;; <---
                             ))
     (local-expand #'(let ((x y)) bodies ...) 'expression '())
     
     )))


(define-syntax (LET stx)
  (syntax-case stx (:)
    [(_ ([x y]) bodies ...)
     ;; guard for both vars, aka aliasing
     (and (identifier? #'x) (identifier? #'y))
     (parameterize ([lexical-env (struct-copy env (lexical-env)
                                              ;; annotate the new var with the type of the aliased
                                              [types (cons (cons (syntax-e #'x) (cdr (assv (syntax-e #'y)
                                                                                           (env-types (lexical-env)))))
                                                           (env-types (lexical-env)))]
                                              ;; recreate the env with an updated alias field
                                              [aliases (cons (cons (syntax-e #'x) (syntax-e #'y))
                                                             (env-aliases (lexical-env)))])])
       ;;  
       (local-expand #'(let ([x y]) bodies ...) 'expression '()))]
    
    [(_ ([x : t v]) bodies ...)
     (identifier? #'x)
     (parameterize ([lexical-env (struct-copy env (lexical-env)
                                    [types (cons (cons (syntax-e #'x) (syntax-e #'t))
                                                 (env-types (lexical-env)))])])
       (local-expand #'(let ([x v]) bodies ...) 'expression '()))]))



;; Example for types and alias
(LET ([x : Int 5])
  (LET ([y x])
    (LET ([q : Int 10])
      (begin (TODO* "Three Var Test")
             (+ x y q)))))
