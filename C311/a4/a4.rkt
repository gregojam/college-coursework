#lang racket

;; lex : LCE empty -> [Listof [Listof X]]
;; returns a list of each bound variable in an LCE paired
;; with its lexical address
(define lex
  (λ (lce ls)
    (match lce
      [`,y #:when (symbol? y)
           (if (memv y ls)
               `(var ,(length (takef ls (λ (e) (not (eqv? e y))))))
               '())]
      [`,y #:when (number? y) `(const ,y)]
      [`(zero? ,x) `(zero? ,(lex x ls))]
      [`(sub1 ,x) `(sub1 ,(lex x ls))]
      [`(* ,x ,y) `(* ,(lex x ls) ,(lex y ls))]
      [`(if ,test ,then ,alt) `(if ,(lex test ls)
                                   ,(lex then ls)
                                   ,(lex alt ls))]
      [`(let ((,x ,exp)) ,body) `(let ,(lex exp (cons x ls))
                                   ,(lex body (cons x ls)))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x ls)))]
      [`(,rator ,rand) `(,(lex rator ls) ,(lex rand ls))])))

;****************************************************************

(define empty-env
  (λ ()
    `(empty-env)))

(define extend-env
  (λ (x arg env)
    `(extend-env ,x ,arg ,env)))

(define apply-env
  (λ (env y)
    (match env
      [`(empty-env) (lambda (y) (error 'value-of "unbound variable ~s" y))]
      [`(extend-env ,x ,arg ,env) (if (eqv? y x) arg (apply-env env y))])))

(define value-of
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (apply-env env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(zero? ,test) (zero? (value-of test env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(* ,x ,y) (* (value-of x env) (value-of y env))]
      [`(if ,test ,then ,alt) (if (value-of test env)
                                  (value-of then env)
                                  (value-of alt env))]
      [`(let ([,id ,expr]) ,body)
       (value-of body (extend-env id (value-of expr env) env))]
      [`(lambda (,x) ,body) (λ (arg) (value-of body (extend-env x arg env)))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))])))

;****************************************************************

(define make-closure-fn
  (λ  (x body env)
    (λ (arg)
      (value-of-fn body (extend-env x arg env)))))

(define apply-closure-fn
  (λ (rator rand)
    (rator rand)))

(define value-of-fn
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (apply-env env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(zero? ,test) (zero? (value-of-fn test env))]
      [`(sub1 ,x) (sub1 (value-of-fn x env))]
      [`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env))]
      [`(if ,test ,then ,alt) (if (value-of-fn test env)
                                  (value-of-fn then env)
                                  (value-of-fn alt env))]
      [`(let ([,id ,expr]) ,body) ((make-closure-fn id body env)
                                   (value-of expr env))]
      [`(lambda (,x) ,body) (make-closure-fn x body env)]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env)
                                         (value-of-fn rand env))])))

;****************************************************************

(define make-closure-ds
  (λ  (x body env)
    `(closure ,x ,body, env)))

(define apply-closure-ds
  (λ (rator rand)
    (match rator
      [`(closure ,x ,body ,env)
       ((λ (arg)
          (value-of-ds body (extend-env x arg env))) rand)])))

(define value-of-ds
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (apply-env env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(zero? ,test) (zero? (value-of-ds test env))]
      [`(sub1 ,x) (sub1 (value-of-ds x env))]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(if ,test ,then ,alt) (if (value-of-ds test env)
                                  (value-of-ds then env)
                                  (value-of-ds alt env))]
      [`(let ([,id ,expr]) ,body) (apply-closure-ds (make-closure-ds id body env)
                                                    (value-of expr env))]
      [`(lambda (,x) ,body) (make-closure-ds x body env)]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env)
                                         (value-of-ds rand env))])))

;*****************************************************************

(define value-of-dynamic
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (apply-env env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(null? ,test) (null? (value-of-dynamic test env))]
      [`(zero? ,test) (zero? (value-of-dynamic test env))]
      [`(cons ,1st ,2nd) (cons (value-of-dynamic 1st env)
                               (value-of-dynamic 2nd env))]
      [`(car ,thing) (car (value-of-dynamic thing env))]
      [`(cdr ,thing) (cdr (value-of-dynamic thing env))]
      [`(quote ,thing) thing]
      [`(sub1 ,x) (sub1 (value-of-dynamic x env))]
      [`(* ,x ,y) (* (value-of-dynamic x env) (value-of-dynamic y env))]
      [`(if ,test ,then ,alt) (if (value-of-dynamic test env)
                                  (value-of-dynamic then env)
                                  (value-of-dynamic alt env))]
      [`(let ([,id ,expr]) ,body)
       (value-of-dynamic body (extend-env id (value-of-dynamic expr env) env))]
      [`(lambda (,x) ,body) `(lambda (,x) ,body)]
      [`(,rator ,rand) (match-let
                           ([`(lambda (,x) ,body)
                             (value-of-dynamic rator env)]
                            [var (value-of-dynamic rand env)])
                         (value-of-dynamic body (extend-env x var env)))])))