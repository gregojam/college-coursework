#lang racket

(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) ls]
                [(eqv? (car ls) 0) (apply-k k (last-non-zero (cdr ls)))]
                [else (cons (car ls) (last-non-zero (cdr ls)))]
                ))))
        (last-non-zero ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lex
  (λ (lce ls)
    (match lce
      [`,y #:when (symbol? y)
           (if (memv y ls)
               `(var ,(length (takef ls (λ (e) (not (eqv? e y))))))
               '())]
      [`,y #:when (number? y) `(const ,y)]
      [`(zero? ,x) `(zero ,(lex x ls))]
      [`(sub1 ,x) `(sub1 ,(lex x ls))]
      [`(* ,x ,y) `(mult ,(lex x ls) ,(lex y ls))]
      [`(if ,test ,then ,alt) `(if ,(lex test ls)
                                   ,(lex then ls)
                                   ,(lex alt ls))]
      [`(let ((,x ,exp)) ,body) `(let ,(lex exp ls)
                                   ,(lex body (cons x ls)))]
      [`(let/cc ,x ,body) `(let/cc ,(lex body (cons x ls)))]
      [`(throw ,k-exp ,v-exp) `(throw ,(lex k-exp ls) ,(lex v-exp ls))]
      [`(lambda (,x) ,body) `(lambda ,(lex body (cons x ls)))]
      [`(,rator ,rand) `(app ,(lex rator ls) ,(lex rand ls))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of-cps
  (lambda (expr env-cps k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(var ,expr) (apply-env env-cps expr k)]
      [`(mult ,x1 ,x2) (value-of-cps
                        x1
                        env-cps
                        (λ (v)
                          (value-of-cps
                           x2
                           env-cps
                           (λ (w)
                             (apply-k k (* v w))))))]
      [`(sub1 ,x) (value-of-cps
                   x
                   env-cps
                   (λ (v)
                     (apply-k k (sub1 v))))]
      [`(zero ,x) (value-of-cps
                   x
                   env-cps
                   (λ (v)
                     (apply-k k (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps
                                 test
                                 env-cps
                                 (λ (v)
                                   (if v
                                       (value-of-cps conseq env-cps k)
                                       (value-of-cps alt env-cps k))))]
      [`(let/cc ,body) (value-of-cps
                        body
                        (extend-env k env-cps)
                        k)]
      [`(throw ,k-exp ,v-exp) (value-of-cps
                               k-exp
                               env-cps
                               (λ (k^)
                                 (value-of-cps v-exp
                                               env-cps
                                               k^)))]
      [`(let ,e ,body) (value-of-cps
                        e
                        env-cps
                        (λ (a)
                          (value-of-cps
                           body
                           (extend-env a env-cps)
                           k)))]
      [`(lambda ,body) (apply-k k (make-closure body env-cps))]
      [`(app ,rator ,rand) (value-of-cps
                            rator
                            env-cps
                            (λ (c-cps)
                              (value-of-cps
                               rand
                               env-cps
                               (λ (a)
                                 (apply-closure c-cps a k)))))])))
 
(define empty-k
  (lambda ()
    (lambda (v) v)))

(define apply-k
  (λ (k v)
    (k v)))

(define empty-env
  (lambda ()
    '(empty-env)))

(define extend-env
  (λ (a^ env-cps^)
    `(extend-env ,a^ ,env-cps^)))
    
(define apply-env
  (λ (env-cps y k)
    (match env-cps
      ['(empty-env) (lambda (y k)
                      (error 'value-of-cps "unbound identifier"))]
      [`(extend-env ,a^ ,env-cps^) (if (zero? y) 
                                       (apply-k k a^)
                                       (apply-env env-cps^ (sub1 y) k))])))

(define make-closure
  (λ (body env-cps)
    (λ (a k^) (value-of-cps
               body
               (extend-env a env-cps) 
               k^))))
  
(define apply-closure
  (λ (c-cps a k)
    (c-cps a k)))



