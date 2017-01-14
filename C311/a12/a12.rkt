#lang racket

(require "monads.rkt")

(define assv-maybe
  (λ (v ls)
    (cond
      [(null? ls) (fail)]
      [(eqv? v (caar ls)) (return-maybe (cdar ls))]
      [else (assv-maybe v (cdr ls))])))

(define partition-writer
  (λ (p ls)
    (cond
      [(null? ls) (return-writer '())]
      [(p (car ls))
       (bind-writer
        (tell-writer (car ls))
        (λ (_)
          (partition-writer p (cdr ls))))]
      [else
       (bind-writer
        (partition-writer p (cdr ls))
        (λ (res)
          (return-writer (cons (car ls) res))))])))

(define powerXpartials
  (λ (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(= n 1) (return-writer x)]
      [(odd? n)
       (bind-writer
        (powerXpartials x (sub1 n))
        (λ (res)
          (bind-writer
           (tell-writer res)
           (λ (_)
             (return-writer (* x res))))))]
      [(even? n)
       (bind-writer
        (powerXpartials x (/ n 2))
        (λ (res)
          (bind-writer
           (tell-writer res)
           (λ (_)
             (return-writer (* res res))))))])))

(define replace-with-count
  (λ (x ls)
    (cond
      [(null? ls) (return-state '())]
      [(pair? (car ls))
       (do bind-state
         (a <- (replace-with-count x (car ls)))
         (d <- (replace-with-count x (cdr ls)))
         (return-state (cons a d)))]
      [(eqv? x (car ls))
       (do bind-state
         (s <- (get-state))
         (put-state (add1 s))
         (d <- (replace-with-count x (cdr ls)))
         (return-state (cons s d)))]
      [else
       (do bind-state
         (d <- (replace-with-count x (cdr ls)))
         (return-state (cons (car ls) d)))])))

(define traverse
  (lambda (return bind f)
    (letrec
        ((trav
          (lambda (tree)
            (cond
              [(pair? tree)
               (do bind
                 (a <- (trav (car tree)))
                 (d <- (trav (cdr tree)))
                 (return (cons a d)))]
              [else (f tree)]))))
      trav)))

(define reciprocal
  (λ (x)
    (cond
      [(zero? x) (fail)]
      [else (return-maybe (/ x))])))

(define traverse-reciprocal
    (traverse return-maybe bind-maybe reciprocal))

(define halve
  (λ (x)
    (cond
      [(even? x) (return-writer (/ x 2))]
      [else
       (bind-writer
        (tell-writer x)
        (λ (_)
          (return-writer x)))])))

(define traverse-halve
  (traverse return-writer bind-writer halve))

(define state/sum
  (λ (x)
    (do bind-state
      (s <- (get-state))
      (put-state (+ x s))
      (return-state s))))

(define traverse-state/sum
  (traverse return-state bind-state state/sum))