#lang racket
(require "mk.rkt")

(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
      (== `(,a . ,G^) G)
      (fresh (aa da)
        (== `(,aa . ,da) a)
        (conde
          ((== aa e) (== da t))
          ((=/= aa e) (apply-Go G^ e t)))))))

(define !-
  (lambda (G e t)
    (conde
      [(numbero e) (== 'Nat t)]
      [(== t 'Bool)
       (conde
         ((== #t e))
         ((== #f e)))]
      [(fresh (ne1 ne2)
         (== `(+ ,ne1 ,ne2) e)
         (== 'Nat t)
         (!- G ne1 'Nat)
         (!- G ne2 'Nat))]
      [(fresh (teste anse elsee)
        (== `(if ,teste ,anse ,elsee) e)
        (!- G teste 'Bool)
        (!- G anse t)
        (!- G elsee t))]
      [(symbolo e) (apply-Go G e t)]
      [(fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb)          
          (== `(,tx -> ,tb) t)
          (!- `((,x . ,tx) . ,G) b tb)))]
      [(fresh (e1 arg)
        (== `(,e1 ,arg) e)
        (fresh (targ)
          (!- G e1 `(,targ -> ,t))
          (!- G arg targ)))]
      [(fresh (arg1 arg2)
              (== `(* ,arg1 ,arg2) e)
              (== 'Nat t)
              (!- G arg1 'Nat)
              (!- G arg2 'Nat))]
      [(fresh (arg)
              (== `(not ,arg) e)
              (== 'Bool t)
              (!- G arg 'Bool))]
      [(fresh (arg)
              (== `(zero? ,arg) e)
              (== 'Bool t)
              (!- G arg 'Nat))]
      [(fresh (arg)
              (== `(sub1 ,arg) e)
              (== 'Nat t)
              (!- G arg 'Nat))]
      [(fresh (exp arg)
              (== `((fix ,exp) ,arg) e)
              (fresh (u)
                     (!- G `(,exp arg) u)
                     (fresh (t1 t2)
                            (== `(,t1 -> ,t2) u)
                            (== t2 t)
                            (!- G arg t1))))])))
                     
              
