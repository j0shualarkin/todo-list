#lang racket

;; Joshua Larkin

;; David Christiansen, Andrew Kent


(begin-for-syntax (require syntax/parse)
                  (struct located [location value] #:prefab)

                  (define (prepare-info i)
                    (let* ([TYPES (string-append "Types: " (info-type i))]
                            [PROPS (string-append "Props: " (info-prop i))]
                            [ALIAS (string-append "Alias: " (info-alias i))])
                       (string-append TYPES "\n" PROPS "\n" ALIAS "\n")))
                  
                  (struct info (type prop alias) #:mutable)
                  (define curr-info (info "" "" ""))
                  (struct command (name module-path function arguments) #:prefab))



(define-syntax (TODO* stx)
  (syntax-parse stx
    [(_ msg:str)
     ;; Expand a TODO to a runtime error
     (define runtime
       (syntax/loc stx
         (error msg)))
     ;; Attach a notice that it is a TODO to be filled out
     (syntax-property runtime 'todo (vector (prepare-info curr-info) "summary"))]))
;; I think summary is going to overwrite everytime, would prefer it be msg



(define-syntax (typecheck body)
  (syntax-case body ()
    ((_ . body)
     (let () 
       (define expanded-body (local-expand #'body 'expression '()))
       (define current-info (info "" "" ""))
  
       (let loop! ([nfo current-info]
                   [stx expanded-body])
         (syntax-parse stx #:literals (let-values)
 
           [_ #:when (vector? stx)
              (define current-todo
                (syntax-property stx 'todo))
              (vector-set! current-todo 1 "filler")
              (vector-set! current-todo 0 "some info")]
      
           [(let-values ([(x) x-body:id]) body)

            (set-info-alias! nfo (string-append (info-alias nfo)
                                                (format "(≡ ~a ~a)"
                                                        (syntax-e #'x)
                                                        (syntax-e #'x-body))))
            (loop! nfo #'body)]

           [(let-values ([(x) x-body]) body)

            (set-info-type! nfo (string-append (info-type nfo)
                                               (format "(: ~a ~a)"
                                                       (syntax-e #'x)
                                                       'Type)))
            (loop! nfo #'x-body)
            (loop! nfo #'body)]
      
           [(a . d) (loop! nfo #'a) (loop! nfo #'d)]

           [_ (void)]))
       expanded-body)))) 

(typecheck (let ([x 5])
             (let ([y x])
               (+ x y))))
;; --> Number




