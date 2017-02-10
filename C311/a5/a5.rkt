#lang racket

(define empty-env
  (λ ()
    (lambda (y) (error 'value-of "unbound variable ~s" y))))

(define extend-env
  (λ (x arg env)
    (λ (y)
      (if (eqv? y x)
          arg
          (env y)))))

(define apply-env
  (λ (env y)
    (env y)))

(define make-closure-cbv
  (λ  (x body env)
    (λ (arg)
      (val-of-cbv body (extend-env x arg env)))))

(define apply-closure
  (λ (rator rand)
    (rator rand)))

(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                    (val-of-cbv conseq env)
                                    (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(set! ,x ,val) (set-box! (apply-env env x) (val-of-cbv val env))]
      [`(let ([,id ,expr]) ,body) ((make-closure-cbv id body env)
                                   (box (val-of-cbv expr env)))]
      [`(lambda (,x) ,body) (make-closure-cbv x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbv rator env)
                                      (box (unbox (apply-env env rand))))]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env)
                                      (box (val-of-cbv rand env)))])))

;****************************************************************

(define make-closure-cbr
  (λ  (x body env)
    (λ (arg)
      (val-of-cbr body (extend-env x arg env)))))

(define val-of-cbr
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                    (val-of-cbr conseq env)
                                    (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`(set! ,x ,val) (set-box! (apply-env env x) (val-of-cbr val env))]
      [`(let ([,id ,expr]) ,body) ((make-closure-cbr id body env)
                                   (box (val-of-cbr expr env)))]
      [`(lambda (,x) ,body) (make-closure-cbr x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbr rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env)
                                      (box (val-of-cbr rand env)))])))

;****************************************************************

(define make-closure-cbname
  (λ  (x body env)
    (λ (arg)
      (val-of-cbname body (extend-env x arg env)))))

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))]
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                    (val-of-cbname conseq env)
                                    (val-of-cbname alt env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`(let ([,id ,expr]) ,body) ((make-closure-cbname id body env)
                                   (box (val-of-cbname expr env)))]
      [`(lambda (,x) ,body) (make-closure-cbname x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                       (apply-closure (val-of-cbname rator env)
                                      (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env)
                                      (box (lambda ()
                                             (val-of-cbname rand env))))])))

;****************************************************************

(define make-closure-cbneed
  (λ  (x body env)
    (λ (arg)
      (val-of-cbneed body (extend-env x arg env)))))

(define unbox/need
  (λ (b)
    (let ([v ((unbox b))])
      (set-box! b (lambda () v))
      v)))

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,y #:when (symbol? y) (unbox/need (apply-env env y))]
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n) n]
      [`(zero? ,n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                    (val-of-cbneed conseq env)
                                    (val-of-cbneed alt env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`(let ([,id ,expr]) ,body) ((make-closure-cbneed id body env)
                                   (box (val-of-cbneed expr env)))]
      [`(lambda (,x) ,body) (make-closure-cbneed x body env)]
      [`(,rator ,rand) #:when (symbol? rand)
                        (apply-closure (val-of-cbneed rator env)
                                       (apply-env env rand))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env)
                                      (box (lambda ()
                                             (val-of-cbneed rand env))))])))

(define random-sieve
    '((lambda (n)
        (if (zero? n)
            (if (zero? n)
                (if (zero? n)
                    (if (zero? n)
                        (if (zero? n)
                            (if (zero? n)
                                (if (zero? n)
                                    #t #f) #f) #f) #f) #f) #f)
            (if (zero? n)
                #f
                (if (zero? n)
                    #f
                    (if (zero? n)
                        #f
                        (if (zero? n)
                            #f
                            (if (zero? n)
                                #f
                                (if (zero? n)
                                    #f
                                    #t))))))))
      (random 2)))