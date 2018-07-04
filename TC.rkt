#lang racket


(begin-for-syntax (require syntax/parse))

;; typecheck :: Expr -> TypeEnv -> Type 



#;(typecheck (let ([x 5])
             (let ([y x])
               (+ x y))))
;; --> Number
