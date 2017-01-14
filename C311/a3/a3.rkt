#lang racket

(define empty-env
  (λ ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define value-of
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(zero? ,test) (zero? (value-of test env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(* ,x ,y) (* (value-of x env) (value-of y env))]
      [`(if ,test ,then ,alt) (if (value-of test env)
                                  (value-of then env)
                                  (value-of alt env))]
      [`(let ([,id ,expr]) ,body) (value-of body (λ (y) (if (eqv? y id)
                                                            (value-of expr env)
                                                            (env y))))]
      [`(lambda (,x) ,body) (λ (arg) (value-of body (λ (y) (if (eqv? y x)
                                                               arg
                                                               (env y)))))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))])))

;****************************************************************

(define empty-env-fn
  (λ ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define extend-env-fn
  (λ (x arg env)
    (λ (y)
      (if (eqv? y x)
          arg
          (env y)))))

(define apply-env-fn
  (λ (env y)
    (env y)))

(define value-of-fn
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (apply-env-fn env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(zero? ,test) (zero? (value-of-fn test env))]
      [`(sub1 ,x) (sub1 (value-of-fn x env))]
      [`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env))]
      [`(if ,test ,then ,alt) (if (value-of-fn test env)
                                  (value-of-fn then env)
                                  (value-of-fn alt env))]
      [`(let ([,id ,expr]) ,body) (value-of-fn body (extend-env-fn id (value-of-fn expr env) env))]
      [`(lambda (,x) ,body) (λ (arg) (value-of-fn  body (extend-env-fn x arg env)))]
      [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))])))

;****************************************************************

(define empty-env-ds
  (λ ()
    `(empty-env-ds)))

(define extend-env-ds
  (λ (x arg env)
    `(extend-env-ds ,x ,arg ,env)))

(define apply-env-ds
  (λ (env y)
    (match env
      [`(empty-env-ds) (lambda (y) (error 'value-of "unbound variable ~s" y))]
      [`(extend-env-ds ,x ,arg ,env) (if (eqv? y x) arg (apply-env-ds env y))])))

(define value-of-ds
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (apply-env-ds env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(zero? ,test) (zero? (value-of-ds test env))]
      [`(sub1 ,x) (sub1 (value-of-ds x env))]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(if ,test ,then ,alt) (if (value-of-ds test env)
                                  (value-of-ds then env)
                                  (value-of-ds alt env))]
      [`(let ([,id ,expr]) ,body) (value-of-ds body (extend-env-ds id (value-of-ds expr env) env))]
      [`(lambda (,x) ,body) (λ (arg) (value-of-ds body (extend-env-ds x arg env)))]
      [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))])))

;****************************************************************



(define fo-eulav
  (λ (exp env)
    (match exp
      [`,y #:when (symbol? y)  (env y)]
      [`,y #:when (number? y)  y]
      [`,y #:when (boolean? y) y]
      [`(,test ?orez) (zero? (fo-eulav test env))]
      [`(,x 1bus) (sub1 (fo-eulav x env))]
      [`(,y ,x *) (* (fo-eulav x env) (fo-eulav y env))]
      [`(,alt ,then ,test fi) (if (fo-eulav test env)
                                  (fo-eulav then env)
                                  (fo-eulav alt env))]
      [`(,body ([,expr ,id]) tel) (fo-eulav body (λ (y) (if (eqv? y id)
                                                            (fo-eulav expr env)
                                                            (env y))))]
      [`(,body (,x) adbmal) (λ (arg) (fo-eulav body (λ (y) (if (eqv? y x)
                                                               arg
                                                               (env y)))))]
      [`(,rand ,rator) ((fo-eulav rator env) (fo-eulav rand env))])))