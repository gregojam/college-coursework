#lang racket

(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ((last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) ls]
                [(eqv? (car ls) 0) (k (last-non-zero (cdr ls)))]
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
      [`(const ,expr) (k expr)]
      [`(var ,expr) (env-cps expr k)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env-cps
                                     (λ (v)
                                       (value-of-cps x2 env-cps
                                                     (λ (w)
                                                       (k (* v w))))))]
      [`(sub1 ,x) (value-of-cps x env-cps
                                (λ (v)
                                  (k (sub1 v))))]
      [`(zero ,x) (value-of-cps x env-cps
                                (λ (v)
                                  (k (zero? v))))]
      [`(if ,test ,conseq ,alt) (value-of-cps
                                 test env-cps
                                 (λ (v)
                                   (if v
                                       (value-of-cps conseq env-cps k)
                                       (value-of-cps alt env-cps k))))]
      [`(let/cc ,body) (value-of-cps
                          body
                          (λ (y k^) (if (zero? y) (k^ k) (env-cps (sub1 y) k^)))
                          k)]
;       (let/cc k
;         (value-of-cps body (lambda (y) (if (zero? y) k (env-cps (sub1 y))))))]
      [`(throw ,k-exp ,v-exp) (value-of-cps k-exp env-cps
                                            (λ (k^)
                                              (value-of-cps
                                               v-exp env-cps
                                               k^)))]
;       ((value-of-cps k-exp env-cps) (value-of-cps v-exp env-cps))]
      [`(let ,e ,body) (value-of-cps
                        e env-cps
                        (λ (a)
                          (value-of-cps
                           body (λ (y k^) (if (zero? y) (k^ a) (env-cps (sub1 y) k^)))
                           k)))]
;       (let ((a (value-of-cps e env-cps)))
;         (value-of-cps body (lambda (y) (if (zero? y) a (env-cps (sub1 y))))))]
      [`(lambda ,body) (k (λ (a k^) (value-of-cps
                                     body
                                     (λ (y k^^) (if (zero? y) 
                                                   (k^^ a)
                                                   (env-cps (sub1 y) k^^)))
                                     k^)))]
;       (lambda (a) (value-of-cps body (lambda (y) (if (zero? y) a (env-cps (sub1 y))))))]
      [`(app ,rator ,rand) (value-of-cps rator env-cps
                                         (λ (c-cps)
                                           (value-of-cps rand
                                                         env-cps
                                                         (λ (a)
                                                           (c-cps a k)))))])))
;       ((value-of-cps rator env-cps) (value-of-cps rand env-cps))])))
 
(define empty-k
  (lambda ()
    (lambda (v) v)))

(define empty-env
  (lambda ()
    (lambda (y k)
      (error 'value-of-cps "unbound identifier"))))

