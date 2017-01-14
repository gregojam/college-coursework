;#lang racket
;(require "parenthec.rkt")

(define-program-counter pc)

(define-registers
  exp
  env-cps
  k
  v
  y
  a
  c-cps)

(define-union expr
  (const cexp)
  (var ex)
  (if test conseq alt)
  (mult x1 x2)
  (sub1 x)
  (zero x)
  (letcc body)
  (throw kexp vexp)
  (let ex body)              
  (lambda body)
  (app rator rand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-label value-of-cps
    (union-case exp expr
      [(const ex)
       (begin (set! v ex)
              (set! pc apply-k))]
      [(var ex)
       (begin (set! y ex)
              (set! pc apply-env))]
      [(mult x1 x2)
       (begin (set! exp x1)
              (set! k (kt_mult-outer-k x2 env-cps k))
              (set! pc value-of-cps))]
      [(sub1 x)
       (begin (set! exp x)
              (set! k (kt_sub1-k k))
              (set! pc value-of-cps))]
      [(zero x)
       (begin (set! exp x)
              (set! k (kt_zero-k k))
              (set! pc value-of-cps))]
      [(if test conseq alt)
       (begin (set! exp test)
              (set! k (kt_if-k conseq alt env-cps k))
              (set! pc value-of-cps))]
      [(letcc body)
       (begin (set! exp body)
              (set! env-cps (envr_extend k env-cps))
              (set! pc value-of-cps))]
      [(throw kexp vexp)
       (begin (set! exp kexp)
              (set! k (kt_throw-k vexp env-cps))
              (set! pc value-of-cps))]
      [(let ex body)
       (begin (set! exp ex)
              (set! k (kt_let-k body env-cps k))
              (set! pc value-of-cps))]
      [(lambda body)
       (begin (set! v (clos_closure body env-cps))
              (set! pc apply-k))]
      [(app rator rand)
       (begin (set! exp rator)
              (set! k (kt_app-outer-k rand env-cps k))
              (set! pc value-of-cps))]))

;;;;;

(define-union kt
  (empty-k dismount)
  (mult-inner-k v^ k^)
  (mult-outer-k x2^ env-cps^ k^)
  (sub1-k k^)
  (zero-k k^)
  (if-k conseq^ alt^ env-cps^ k^)
  (throw-k v-exp^ env-cps^)
  (let-k body^ env-cps^ k^)
  (app-inner-k c-cps^ k^)
  (app-outer-k rand^ env-cps^ k^))

(define-label apply-k
    (union-case k kt
      [(empty-k dismount)
       (dismount-trampoline dismount)]
      [(mult-inner-k v^ k^)
       (begin (set! k k^)
              (set! v (* v^ v))
              (set! pc apply-k))]
      [(mult-outer-k x2^ env-cps^ k^)
       (begin (set! exp x2^)
              (set! env-cps env-cps^)
              (set! k (kt_mult-inner-k v k^))
              (set! pc value-of-cps))]
      [(sub1-k k^)
       (begin (set! k k^)
              (set! v (sub1 v))
              (set! pc apply-k))]
      [(zero-k k^)
       (begin (set! k k^)
              (set! v (zero? v))
              (set! pc apply-k))]
      [(if-k conseq^ alt^ env-cps^ k^)
       (if v
           (begin (set! exp conseq^)
                  (set! env-cps env-cps^)
                  (set! k k^)
                  (set! pc value-of-cps))
           (begin (set! exp alt^)
                  (set! env-cps env-cps^)
                  (set! k k^)
                  (set! pc value-of-cps)))]
      [(throw-k v-exp^ env-cps^)
       (begin (set! exp v-exp^)
              (set! env-cps env-cps^)
              (set! k v)
              (set! pc value-of-cps))]
      [(let-k body^ env-cps^ k^)
       (begin (set! exp body^)
              (set! env-cps (envr_extend v env-cps^))
              (set! k k^)
              (set! pc value-of-cps))]
      [(app-inner-k c-cps^ k^)
       (begin (set! c-cps c-cps^)
              (set! a v)
              (set! k k^)
              (set! pc apply-closure))]
      [(app-outer-k rand^ env-cps^ k^)
       (begin (set! exp rand^)
              (set! env-cps env-cps^)
              (set! k (kt_app-inner-k v k^))
              (set! pc value-of-cps))]))

;;;;;

(define-union envr
  (extend a^ env-cps^)
  (empty-env))

(define-label apply-env
    (union-case env-cps envr
                [(empty-env) (error 'value-of-cps "unbound identifier")]
                [(extend a^ env-cps^) (if (zero? y)
                                          (begin (set! v a^)
                                                 (set! pc apply-k))
                                          (begin (set! env-cps env-cps^)
                                                 (set! y (sub1 y))
                                                 (set! pc apply-env)))]))

;;;;;

(define-union clos
  (closure body^ env-cps^))
  
(define-label apply-closure
    (union-case c-cps clos
                [(closure body^ env-cps^)
                 (begin (set! exp body^)
                        (set! env-cps (envr_extend a env-cps^))
                        (set! pc value-of-cps))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (let ((f (lambda (f)
;;   	      (lambda (n)
;; 	        (if (zero? n) 
;; 		    1
;; 	            (* n ((f f) (sub1 n))))))))
;;   (* (letcc k ((f f) (throw k ((f f) 4)))) 5))

(define-label main 
    (begin (set! exp (expr_let 
                      (expr_lambda
                       (expr_lambda 
                        (expr_if
                         (expr_zero (expr_var 0))
                         (expr_const 1)
                         (expr_mult (expr_var 0)
                                    (expr_app
                                     (expr_app (expr_var 1) (expr_var 1))
                                     (expr_sub1 (expr_var 0)))))))
                      (expr_mult
                       (expr_letcc
                        (expr_app
                         (expr_app (expr_var 1) (expr_var 1))
                         (expr_throw (expr_var 0)
                                     (expr_app
                                      (expr_app (expr_var 1) (expr_var 1))
                                      (expr_const 4)))))
                       (expr_const 5))))
           (set! env-cps (envr_empty-env))
           (set! pc value-of-cps)
           (mount-trampoline kt_empty-k k pc)
           (printf "Fact 5: ~s\n" v)))