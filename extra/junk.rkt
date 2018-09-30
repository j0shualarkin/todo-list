#lang racket



#|


#;
(define-syntax (inner-TODO stx)
  (define ctx (or (syntax-parameter-value #'definition-context) stx))
  (syntax-parse stx
    [(_ msg:string)
     (define item
       (located ctx
                (todo-item (syntax->datum #'msg)
                           (syntax-parse ctx
                             #:literals (define/todos)
                             ;[(define/todos x e) (syntax->datum #'x)]
                             [_ (syntax->datum #'msg)]))))
     (syntax-property (syntax/loc stx (error 'inner-todo msg)) 'todo item)]))


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


#;
(define-syntax (typecheck body)
  (define expanded-body (local-expand body 'expression '()))
  ;(define current-info (info "" "" ""))
  
  (let loop! ([env '()]
              [stx expanded-body])
    
    (syntax-parse stx #:literals (let-values)

      [_ #:when (todo-item? (syntax-property stx 'todo "3args?"))

         (set-todo-item-summary! stx (info-print some-info some-port #t))
         ;; I want the port to be the todo-item-summary that I'm mutating, right?
         
         ;; also I don't know that the string the todo-summary wants is
         ;; going to come from info-print, since its a void method

         ;; how should i be using the env to fill in the info struct properly
         (set-todo-item-full! stx (info-print some-info some-port #t))]
      
      [(let-values ([(x) x-body:id]) body)
       (loop! (cons (cons #'x #'x-body) env) #'body)]
      ;; should I be converting the env to the info struct once we hit a todo item
      
      [(let-values ([(x) x-body]) body)
       (loop! env #'x-body)
       (loop! (cons (cons #'x 'Type) env) #'body)]
      
      [(a . d) (loop! env #'a) (loop! env #'d)]
      
      [_ (void)])))

|#