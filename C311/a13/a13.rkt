#lang racket

(require "mk.rkt")
(require "numbers.rkt")

(define listo
  (λ (ls)
    (conde
     [(== '() ls) (== '() ls)]
     [(=/= '() ls)
      (fresh (a d)
             (== `(,a . ,d) ls)
             (listo d))])))

(define facto
  (λ (n1 n2)
    (conde
     [(zeroo n1) (== '(1) n2)]
     [(fresh (x)
             (minuso n1 '(1) x)
             (fresh (a)
                    (facto x a)
                    (*o n1 a n2)))])))

(define fibso
  (λ (n o1 o2)
    (conde
     [(zeroo n) (== '(1) o1) (== '(1) o2)]
     [(fresh (n-)
             (minuso n '(1) n-)
             (fresh (u v)
                    (fibso n- u v)
                    (== v o1)
                    (fresh (u+v)
                           (pluso u v u+v)
                           (== u+v o2))))])))

(define lookup
  (λ (vars vals y o)
    (fresh (x vars^ vals^ v)
           (== `(,x . ,vars^) vars)
           (== `(,v . ,vals^) vals)
           (conde
            [(== x y) (== v o)]
            [(=/= x y) (lookup vars^ vals^ y o)]))))

(define valof
  (λ (exp vars vals o)
    (conde
     [(numbero exp) (== exp o)]
     [(symbolo exp) (lookup vars vals exp o)]
     [(== exp `(list)) (== '() o)]
     [(fresh (a args)
             (== exp `(list ,a . ,args))
             (fresh (vs v)
                    (== `(,v . ,vs) 0)
                    (valof `(list . ,args) vars vals vs)
                    (valof a vars vals v)))]
     [(fresh (v)
             (== exp `(quote ,v))
             (== `(quote ,v) o))]
     [(fresh (x b)
             (== exp `(lambda (,x) ,b))
             (symbolo x)
             (== `(closure ,x ,b ,vars ,vals) o))]
     [(fresh (rator rand)
             (== exp `(,rator ,rand))
             (fresh (x b vars^ vals^ a)
                    (valof rator vars vals `(closure ,x ,b ,vars^ ,vals^))
                    (valof rand vars vals a)
                    (valof b `(,x . ,vars) `(,a . ,vals) o)))])))

(define fo-lavo
  (λ (exp vars vals o)
    (conde
     [(numbero exp) (== exp o)]
     [(symbolo exp) (lookup vars vals exp o)]
     [(== exp `(tsil)) (== '() o)]
     [(fresh (a args)
             (== exp `(,args ,a . tsil))
             (fresh (vs v)
                    (== `(,vs . ,v) 0)
                    (fo-lavo `(,args . tsil) vars vals vs)
                    (fo-lavo a vars vals v)))]
     [(fresh (v)
             (== exp `(,v etouq))
             (== `(,v etouq) o))]
     [(fresh (x b)
             (== exp `(,b (,x) adbmal))
             (symbolo x)
             (== `(,vals ,vars ,b ,x erusolc) o))]
     [(fresh (rator rand)
             (== exp `(,rand ,rator))
             (fresh (x b vars^ vals^ a)
                    (fo-lavo rator vars vals `(,vals^ ,vars^ ,b ,x erusolc))
                    (fo-lavo rand vars vals a)
                    (fo-lavo b `(,x . ,vars) `(,a . ,vals) o)))])))

;;;;;;;;;;

(define middle-earth
    '((lindon eriador forodwaith)
      (forodwaith lindon rhovanion eriador)
      (eriador lindon forodwaith rhovanion enedwaith)
      (rhovanion forodwaith eriador enedwaith rohan rhun)
      (enedwaith eriador rhovanion rohan gondor)
      (rohan enedwaith rhovanion rhun gondor mordor)
      (gondor enedwaith rohan mordor)
      (rhun rohan rhovanion khand mordor)))
;      (mordor gondor rohan rhun khand harad)
;      (khand mordor rhun harad)
;      (harad mordor khand)))

(define membero
  (λ (x ls)
    (fresh (a d)
            (== `(,a . ,d) ls)
            (conde
             [(== x a) (fresh (o) (== '() o))]
             [(=/= x a) (membero x d)]))))

(define color-check
  (λ (c border als)
    (conde
     [(== '() als) (fresh (o) (== '() o))]
     [(fresh (a d aa da)
             (== `(,a . ,d) als)
             (== `(,aa . ,da) a)
             (conde
              [(== c da)
               (absento aa border)               
               (color-check c border d)]
              [(=/= c da)
               (color-check c border d)]))])))

(define realm-color
  (λ (colors ls o)
    (conde
     [(== '() ls) (== '() o)]
     [(=/= '() ls)
      (fresh (a d aa da c v)
             (== `(,a . ,d) ls)
             (== `(,aa . ,da) a)
             (== `((,aa . ,c) . ,v) o)
             (membero c colors)
             (realm-color colors d v)
             (color-check c da v))])))
             

;;takes a while, but it works
(define color-middle-earth
  (λ (ls)
    (run 1 (q)
         (realm-color ls middle-earth q))))
           