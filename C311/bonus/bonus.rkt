#lang racket

(define filter-sps
  (λ (pred ls s)
    (cond
      [(null? ls) (values '() s)]
      [(pred (car ls)) (let-values ([(ls2 s2) (filter-sps pred (cdr ls) s)])
                         (values (cons (car ls) ls2) s2))]
      [else (let-values ([(ls2 s2) (filter-sps pred (cdr ls) s)])
              (values ls2 (cons (car ls) s2)))])))

(define filter*-sps
  (lambda (pred ls s)
    (cond
      [(null? ls) (values '() s)]
      [(pair? (car ls))
       (let*-values ([(ls2 s2) (filter*-sps pred (car ls) s)]
                    [(ls3 s3) (filter*-sps pred (cdr ls) s)])
       (values (cons ls2 ls3) (cons s2 s3)))]
      [(null? (car ls)) (values '() s)]
      [(pred (car ls)) (let-values ([(ls2 s2) (filter*-sps pred (cdr ls) s)])
                         (values (cons (car ls) ls2) s2))]
      [else (let-values ([(ls2 s2) (filter*-sps pred (cdr ls) s)])
              (values ls2 (cons (car ls) s2)))])))

(define fib-sps
  (λ (n s)
    (cond
      [(assv n s)=> (λ (pr) (values (cdr pr) s))]
      ;;ensure 0th at end of store
      [(eqv? n 0) (values n (append (reverse s) `((,n . ,n))))]
      [(eqv? n 1) (values n (cons `(,n . ,n) s))]
      [else
       (let*-values ([(v s2) (fib-sps (- n 2) s)]
                     [(v2 s3) (fib-sps (- n 1) s2)])
           (values (+ v v2) (cons `(,n . ,(+ v v2)) s3)))])))


;;;;;;;;;;

(define-syntax and*
  (syntax-rules ()
    [(and*) #t]
    [(and* e) e]
    [(and* e e* ...) (if e (and* e* ...) #f)]))

(define-syntax list*
  (syntax-rules ()
    [(list*) (raise-syntax-error "Incorrect argument-count to list*")]
    [(list* e) e]
    [(list* e e* ...) (cons e (list* e* ...))]))

(define-syntax macro-list
  (syntax-rules ()
    [(_) '()]
    [(_ e) '(e)]
    [(_ e e* ...) (cons e (macro-list e* ...))]))

(define-syntax mcond
  (syntax-rules (else =>)
    [(_ (e)) (if e e (values))]
    [(_ (truthy => proc)) (if truthy (proc truthy) (values))]
    [(_ (else e)) e]
    [(_ (test e)) (if test e (values))]
    [(_ (test e e* ...)) (if test (begin e e* ...) (vaules))]
    [(_ (test e) (test* e*)...) (if test e (mcond (test* e*)...))]))

(define-syntax copy-code
  (syntax-rules ()
    [(_ x) `(,x x)]))

(define-syntax quote-quote
  (syntax-rules ()
    [(_ e) (quote (quote e))]))

(define-syntax macro-map
  (syntax-rules ()
    [(_ mac '()) '()]
    [(_ mac '(e)) `(,(mac e))]
    [(_ mac '(e e* ...)) (cons (mac e) (macro-map mac '(e* ...)))]))
    