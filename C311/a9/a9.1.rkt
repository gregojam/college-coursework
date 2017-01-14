#lang racket
(require "parenthec.rkt")

(define-union expr
  (const cexp)
  (var exp)
  (if test conseq alt)
  (mult x1 x2)
  (sub1 x)
  (zero x)
  (letcc body)
  (throw kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-cps
  (lambda (exp env-cps k)
    (union-case exp expr
      [(const exp)
       (apply-k k exp)]
      [(var exp)
       (apply-env env-cps exp k)]
      [(mult x1 x2)
       (value-of-cps x1 env-cps (mult-outer-k x2 env-cps k))]
      [(sub1 x)
       (value-of-cps x env-cps (sub1-k k))]
      [(zero x)
       (value-of-cps x env-cps (zero-k k))]
      [(if test conseq alt)
       (value-of-cps test env-cps (if-k conseq alt env-cps k))]
      [(letcc body)
       (value-of-cps body (extend-env k env-cps) k)]
      [(throw kexp vexp)
       (value-of-cps kexp env-cps (throw-k vexp env-cps))]
      [(let exp body)
       (value-of-cps exp env-cps (let-k body env-cps k))]
      [(lambda body)
       (apply-k k (make-closure body env-cps))]
      [(app rator rand)
       (value-of-cps rator env-cps (app-outer-k rand env-cps k))])))

;;;;;
 
(define empty-k
  (lambda ()
    `(empty-k)))

(define mult-inner-k
  (λ (v^ k^)
    `(mult-inner-k ,v^ ,k^)))

(define mult-outer-k
  (λ (x2^ env-cps^ k^)
    `(mult-outer-k ,x2^ ,env-cps^ ,k^)))

(define sub1-k
  (λ (k^)
    `(sub1-k ,k^)))

(define zero-k
  (λ (k^)
    `(zero-k ,k^)))

(define if-k
  (λ (conseq^ alt^ env-cps^ k^)
    `(if-k ,conseq^ ,alt^ ,env-cps^ ,k^)))

(define throw-k
  (λ (v-exp^ env-cps^)
    `(throw-k ,v-exp^ ,env-cps^)))

(define let-k
  (λ (body^ env-cps^ k^)
    `(let-k ,body^ ,env-cps^ ,k^)))

(define app-inner-k
  (λ (c-cps^ k^)
    `(app-inner-k ,c-cps^ ,k^)))

(define app-outer-k
  (λ (rand^ env-cps^ k^)
    `(app-outer-k ,rand^ ,env-cps^ ,k^)))

(define apply-k
  (λ (k v)
    (match k
      [`(empty-k)
       v]
      [`(mult-inner-k ,v^ ,k^)
       (apply-k k^ (* v^ v))]
      [`(mult-outer-k ,x2^ , env-cps^ ,k^)
       (value-of-cps x2^ env-cps^ (mult-inner-k v k^))]
      [`(sub1-k ,k^)
       (apply-k k^ (sub1 v))]
      [`(zero-k ,k^)
       (apply-k k^ (zero? v))]
      [`(if-k ,conseq^ ,alt^ ,env-cps^ ,k^)
       (if v
           (value-of-cps conseq^ env-cps^ k^)
           (value-of-cps alt^ env-cps^ k^))]
      [`(throw-k ,v-exp^ ,env-cps^)
       (value-of-cps v-exp^ env-cps^ v)]
      [`(let-k ,body^ ,env-cps^ ,k^)
       (value-of-cps body^ (extend-env v env-cps^) k^)]
      [`(app-inner-k ,c-cps^ ,k^)
       (apply-closure c-cps^ v k^)]
      [`(app-outer-k ,rand^ ,env-cps^ ,k^)
       (value-of-cps rand^ env-cps^ (app-inner-k v k^))])))

;;;;;

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env
  (λ (a^ env-cps^)
    `(extend-env ,a^ ,env-cps^)))

(define apply-env
  (λ (env-cps y k)
    (match env-cps
      [`(empty-env) (error 'value-of-cps "unbound identifier")]
      [`(extend-env ,a^ ,env-cps^) (if (zero? y) 
                                       (apply-k k a^)
                                       (apply-env env-cps^ (sub1 y) k))])))

;;;;;

(define make-closure
  (λ (body env-cps)
    `(closure ,body ,env-cps)))
  
(define apply-closure
  (λ (c-cps a k)
    (match c-cps
      [`(closure ,body ,env-cps)
       (value-of-cps body (extend-env a env-cps) k)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (let ((f (lambda (f)
;;   	      (lambda (n)
;; 	        (if (zero? n) 
;; 		    1
;; 	            (* n ((f f) (sub1 n))))))))
;;   (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))