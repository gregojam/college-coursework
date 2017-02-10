#lang racket

(define m* 'stuff)
(define n* 'junk)
(define a* 'mess)
(define v* 'garbage)
(define k* 'crud)

(define empty-k
  (λ ()
    `(empty-k)))

;;;;;;;;;;;;;;;;;;;;

(define ack-else-k
  (λ ()
      `(else-k ,m* ,k*)))

(define ack-apply-k
  (λ ()
    (match k*
      [`(empty-k) v*]
      [`(else-k ,m ,k) (begin (set! m* (sub1 m))
                                  (set! k* k)
                                  (set! n* v*)
                                  (ack))])))

;;

(define ack
  (lambda ()
    (cond
      [(zero? m*) (begin (set! v* (add1 n*)) (ack-apply-k))]
      [(zero? n*) (begin (set! m* (sub1 m*)) (set! n* 1) (ack))]
      [else (begin (set! n* (sub1 n*))
                   (set! k* (ack-else-k))
                   (ack))])))

;;

(define ack-reg-driver
  (λ (m n)
    (begin
      (set! m* m)
      (set! n* n)
      (set! k* (empty-k))
      (ack))))

;;;;;;;;;;;;;;;;;;;;

(define depth-inner-k
  (λ ()
    `(inner-k ,(add1 n*) ,k*)))

(define depth-outer-k
  (λ ()
    `(outer-k ,m* ,k*)))

(define depth-apply-k
  (λ ()
    (match k*
      [`(empty-k) v*]
      [`(inner-k ,n ,k) (begin
                            (set! k* k)
                            (if (< n v*)
                                (depth-apply-k)
                            (begin
                              (set! v* n)
                              (depth-apply-k))))]
      [`(outer-k ,m ,k) (begin
                             (set! m* (cdr m))
                             (set! n* v*)
                             (set! k* k)
                             (set! k* (depth-inner-k))
                             (depth))])))

;;

(define depth
  (lambda ()
    (cond
      [(null? m*) (begin
                     (set! v* 1)
                     (depth-apply-k))]
      [(pair? (car m*)) (begin
                           (set! k* (depth-outer-k))
                           (set! m* (car m*))
                           (depth))]
      [else (begin
              (set! m* (cdr m*))
              (depth))])))

(define depth-reg-driver
  (λ (ls)
    (begin
      (set! m* ls)
      (set! k* (empty-k))
      (depth))))

;;;;;;;;;;;;;;;;;;;;

(define fact-else-k
  (λ ()
    `(else-k ,n* ,k*)))

(define fact-apply-k
  (λ ()
    (match k*
      [`(empty-k) v*]
      [`(else-k ,n ,k) (begin
                         (set! k* k)
                         (set! v* (* n v*))
                         (fact-apply-k))])))

(define fact
  (lambda ()
    (cond
      [(zero? n*) (begin (set! v* 1) (fact-apply-k))]
      [else (begin
              (set! k* (fact-else-k))
              (set! n* (sub1 n*))
              (fact))])))

(define fact-reg-driver
  (λ (n)
    (begin
      (set! n* n)
      (set! k* (empty-k))
      (fact))))

;;;;;;;;;;;;;;;;;;;;

(define f* 'balls)

(define pascal-else-inner-k
  (λ ()
    `(else-inner-k ,a* ,k*)))

(define pascal-else-outer-k
  (λ ()
    `(else-outer-k ,a* ,k*)))

(define pascal-let-k
  (λ ()
    `(let-k ,k*)))

(define pascal-apply-k
  (λ ()
    (match k*
      [`(empty-k) v*]
      [`(else-inner-k ,a ,k) (begin
                               (set! a* a)
                               (set! v* (cons a* v*))
                               (set! k* k)
                               (pascal-apply-k))]
      [`(else-outer-k ,a ,k) (begin
                               (set! a* a)
                               (set! k* k)
                               (set! k* (pascal-else-inner-k))
                               (pascal))])))

(define pascal
  (lambda ()
    (cond
      [(> m* n*) (pascal-apply-k)]
      [else (begin
              (set! a* (+ a* m*))
              (set! m* (add1 m*))
              (set! k* (pascal-else-outer-k))
              (pascal))])))

(define pascal-reg-driver
  (λ (n)
    (begin
      (set! n* n)
      (set! m* 1)
      (set! a* 0)
      (set! v* '())
      (set! k* (empty-k))
      (pascal))))