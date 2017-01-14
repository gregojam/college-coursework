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
       (let* ([v exp])
         (apply-k k v))]
      [(var exp)
       (let* ([y exp])
         (apply-env env-cps y k))]
      [(mult x1 x2)
       (let* ([exp x1]
              [k (kt_mult-outer-k x2 env-cps k)])
         (value-of-cps exp env-cps k))]
      [(sub1 x)
       (let* ([exp x]
              [k (kt_sub1-k k)])
         (value-of-cps exp env-cps k))]
      [(zero x)
       (let* ([exp x]
              [k (kt_zero-k k)])
         (value-of-cps exp env-cps k))]
      [(if test conseq alt)
       (let* ([exp test]
              [k (kt_if-k conseq alt env-cps k)])
         (value-of-cps exp env-cps k))]
      [(letcc body)
       (let* ([exp body]
              [env-cps (envr_extend k env-cps)])
         (value-of-cps exp env-cps k))]
      [(throw kexp vexp)
       (let* ([exp kexp]
              [k (kt_throw-k vexp env-cps)])
         (value-of-cps exp env-cps k))]
      [(let exp body)
       (let* ([k (kt_let-k body env-cps k)])
         (value-of-cps exp env-cps k))]
      [(lambda body)
       (let* ([v (clos_closure body env-cps)])
         (apply-k k v))]
      [(app rator rand)
       (let* ([exp rator]
              [k (kt_app-outer-k rand env-cps k)])
         (value-of-cps exp env-cps k))])))

;;;;;

(define-union kt
  (empty-k)
  (mult-inner-k v^ k^)
  (mult-outer-k x2^ env-cps^ k^)
  (sub1-k k^)
  (zero-k k^)
  (if-k conseq^ alt^ env-cps^ k^)
  (throw-k v-exp^ env-cps^)
  (let-k body^ env-cps^ k^)
  (app-inner-k c-cps^ k^)
  (app-outer-k rand^ env-cps^ k^))

(define apply-k
  (λ (k v)
    (union-case k kt
      [(empty-k)
       v]
      [(mult-inner-k v^ k^)
       (let* ([k k^]
              [v (* v^ v)])
         (apply-k k v))]
      [(mult-outer-k x2^ env-cps^ k^)
       (let* ([exp x2^]
              [env-cps env-cps^]
              [k (kt_mult-inner-k v k^)])
         (value-of-cps exp env-cps k))]
      [(sub1-k k^)
       (let* ([k k^]
              [v (sub1 v)])
         (apply-k k v))]
      [(zero-k k^)
       (let* ([k k^]
              [v (zero? v)])
         (apply-k k v))]
      [(if-k conseq^ alt^ env-cps^ k^)
       (if v
           (let* ([exp conseq^]
                  [env-cps env-cps^]
                  [k k^])
             (value-of-cps exp env-cps k))
           (let* ([exp alt^]
                  [env-cps env-cps^]
                  [k k^])
             (value-of-cps exp env-cps k)))]
      [(throw-k v-exp^ env-cps^)
       (let* ([exp v-exp^]
              [env-cps env-cps^]
              [k v])
         (value-of-cps exp env-cps k))]
      [(let-k body^ env-cps^ k^)
       (let* ([exp body^]
              [env-cps (envr_extend v env-cps^)]
              [k k^])
         (value-of-cps exp env-cps k))]
      [(app-inner-k c-cps^ k^)
       (let* ([c-cps c-cps^]
              [a v]
              [k k^])
         (apply-closure c-cps a k))]
      [(app-outer-k rand^ env-cps^ k^)
       (let* ([exp rand^]
              [env-cps env-cps^]
              [k (kt_app-inner-k v k^)])
         (value-of-cps exp env-cps k))])))

;;;;;

(define-union envr
  (extend a^ env-cps^)
  (empty-env))

(define apply-env
  (λ (env-cps y k)
    (union-case env-cps envr
                [(empty-env) (error 'value-of-cps "unbound identifier")]
                [(extend a^ env-cps^) (if (zero? y)
                                          (let* ([v a^])
                                            (apply-k k v))
                                          (let* ([env-cps env-cps^]
                                                 [y (sub1 y)])
                                            (apply-env env-cps y k)))])))

;;;;;

(define-union clos
  (closure body env-cps))
  
(define apply-closure
  (λ (c-cps a k)
    (union-case c-cps clos
                [(closure body env-cps)
                 (let* ([exp body]
                        [env-cps (envr_extend a env-cps)])
                   (value-of-cps exp env-cps k))])))

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
     (envr_empty-env)
     (kt_empty-k))))