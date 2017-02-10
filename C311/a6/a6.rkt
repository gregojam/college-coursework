#lang racket

(require rackunit)

(define empty-k
  (λ ()
    (let ((once-only #f))
      (λ (v)
        (if once-only
            (error 'empty-k
                   "You can only invoke the empty continuation once!")
            (begin (set! once-only #t) v))))))

(define apply-k
  (λ (k v)
    (k v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

(define binary-to-decimal-cps
  (λ (n k)
    (cond
      [(null? n) (apply-k k 0)]
      [else (binary-to-decimal-cps
             (cdr n)
             (λ (v) (apply-k k (+ (car n) (* 2 v)))))])))

(test-eq? "bin1"
          (binary-to-decimal-cps '() (empty-k))
          (binary-to-decimal '()))
(test-eq? "bin2"
          (binary-to-decimal-cps '(1) (empty-k))
          (binary-to-decimal '(1)))
(test-eq? "bin3"
          (binary-to-decimal-cps '(0 1) (empty-k))
          (binary-to-decimal '(0 1)))
(test-eq? "bin4"
          (binary-to-decimal-cps '(1 1 0 1) (empty-k))
          (binary-to-decimal '(1 1 0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (apply-k k 1)]
      [(zero? (car ls)) (apply-k k 0)]
      [else (times-cps (cdr ls) (λ (v) (apply-k k (* (car ls) v))))])))

(test-eq? "times1"
          (times-cps '(1 2 3 4 5) (empty-k))
          (times '(1 2 3 4 5)))
(test-eq? "times2"
          (times-cps '(1 2 3 0 3) (empty-k))
          (times '(1 2 3 0 3)))

(define times-cps-shortcut
  (λ (ls k)
    (cond
      [(null? ls) (apply-k k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (λ (v) (apply-k k (* (car ls) v))))])))

(test-eq? "short1"
          (times-cps-shortcut '(1 2 3 4 5) (empty-k))
          (times '(1 2 3 4 5)))
(test-eq? "short2"
          (times-cps-shortcut '(1 2 3 0 3) (empty-k))
          (times '(1 2 3 0 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

(define plus-cps
  (λ (m k)
    (apply-k k (λ (n k^) (apply-k k^ (+ m n))))))

(test-eq? "plus1"
             (plus-cps 2 (λ (f) (f 3 (empty-k))))
             ((plus 2) 3))
(test-eq? "plus2"
             (plus-cps 2 (λ (f)
                           (f 3 (λ (f)
                                   (plus-cps f (λ (f)
                                                 (f 5 (empty-k))))))))
             ((plus ((plus 2) 3)) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

(define remv-first-9*-cps
  (λ (ls k)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(remv-first-9*-cps
           (car ls)
           (λ (ra) (equal? (car ls) ra)))
          (remv-first-9*-cps
           (cdr ls)
           (λ (rd) (apply-k k (cons (car ls) rd))))]
         [else
          (remv-first-9*-cps
           (car ls)
           (λ (ra) (apply-k k (cons ra (cdr ls)))))])]
      [(eqv? (car ls) '9) (apply-k k (cdr ls))]
      [else (remv-first-9*-cps
             (cdr ls)
             (λ (v)
               (apply-k k (cons (car ls) v))))])))

(test-equal? "remv1"
             (remv-first-9*-cps '((1 2 (3) 9)) (empty-k))
             (remv-first-9* '((1 2 (3) 9))))
(test-equal? "remv2"
             (remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
             (remv-first-9* '(9 (9 (9 (9))))))
(test-equal? "remv3"
             (remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))
             (remv-first-9* '(((((9) 9) 9) 9) 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

(define cons-cell-count-cps
  (λ (ls k)
    (cond
      [(pair? ls)
       (cons-cell-count-cps
        (car ls)
        (λ (ca) (cons-cell-count-cps
                 (cdr ls)
                 (λ (cd) (apply-k k (add1 (+ ca cd)))))))]
      [else (apply-k k 0)])))

(test-equal? "cCount1"
             (cons-cell-count-cps '((((((())))))) (empty-k))
             (cons-cell-count '((((((()))))))))
(test-equal? "cCount2"
             (cons-cell-count-cps '(1 2 3) (empty-k))
             (cons-cell-count '(1 2 3)))
(test-equal? "cCount3"
             (cons-cell-count-cps '(1 2 (3) 4 5) (empty-k))
             (cons-cell-count '(1 2 (3) 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

(define find-cps
  (λ (u s k)
    (let ((pr (assv u s)))
      (if pr
          (find-cps (cdr pr) s k)
          (apply-k k u)))))

(test-eq? "find1"
          (find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k))
          (find 7 '((5 . a) (6 . 5) (7 . 6))))
(test-eq? "find2"
          (find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k))
          (find 5 '((5 . a) (6 . b) (7 . c))))
(test-eq? "find3"
          (find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k))
          (find 5 '((5 . 6) (9 . 6) (2 . 9))))
(test-eq? "find4"
          (find-cps 4 '((1 . a) (2 . b) (3 . c)) (empty-k))
          (find 4 '((1 . a) (2 . b) (3 . c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

(define ack-cps
  (λ (m n k)
    (cond
      [(zero? m) (apply-k k (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (λ (v) (ack-cps (sub1 m) v k)))])))

(test-eq? "ack1"
          (ack-cps 0 0 (empty-k))
          (ack 0 0))
(test-eq? "ack2"
          (ack-cps 1 1 (empty-k))
          (ack 1 1))
(test-eq? "ack3"
          (ack-cps 3 8 (empty-k))
          (ack 3 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fib
  (lambda (n)
    ((lambda (fib)
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

(define fib-cps
  (λ (n k)
    ((λ (fib-cps k)
       (fib-cps fib-cps n k))
     (λ (fib-cps n k)
       (cond
         [(zero? n) (apply-k k 0)]
         [(= 1 n) (apply-k k 1)]
         [else (fib-cps fib-cps (sub1 n)
                  (λ (v)
                    (fib-cps fib-cps (sub1 (sub1 n))
                       (λ (w) (apply-k k (+ v w))))))]))
     k)))

(test-eq? "fib1"
          (fib-cps 0 (empty-k))
          (fib 0))
(test-eq? "fib2"
          (fib-cps 1 (empty-k))
          (fib 1))
(test-eq? "fib3"
          (fib-cps 20 (empty-k))
          (fib 20))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
	 (if (p seed)
	     ans
	     ((h h) (g seed) (cons (f seed) ans))))))))

(define null?-cps
    (lambda (ls k)
      (apply-k k (null? ls))))

(define car-cps
    (lambda (pr k)
      (apply-k k (car pr))))

(define cdr-cps
    (lambda (pr k)
      (apply-k k (cdr pr))))

(define unfold-cps
  (λ (p f g seed k)
    ((λ (h k)
       (h h (λ (a) (a seed '() k))))
     (λ (h k)
       (apply-k
        k
        (λ (seed ans k^)
          (p seed
             (λ (test)
               (if test
                   (apply-k k^ ans)
                   (h h
                      (λ (a)
                        (g seed
                           (λ (b)
                             (f seed
                                (λ (c)
                                  (a b (cons c ans)
                                     (λ (d)
                                       (apply-k k^ d)))))))))))))))
     k)))

(test-equal? "unfold1"
             (unfold-cps null?-cps car-cps cdr-cps
                         '(a b c d e) (empty-k))
             (unfold null? car cdr '(a b c d e)))
(test-equal? "unfold"
             (unfold-cps null?-cps car-cps cdr-cps '() (empty-k))
             (unfold null? car cdr '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-s
  (lambda ()
    '()))
 
(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) `((,u . ,v) . ,s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

(define unify-cps
  (λ (u v s k)
    (cond
      [(eqv? u v) (apply-k k s)]
      [(number? u) (apply-k k `((,u . ,v) . ,s))]
      [(number? v) (unify-cps v u s k)]
      [(pair? u)
       (if (pair? v)
           (find-cps
            (car u) s
            (λ (f1)
              (find-cps
               (car v) s
               (λ (f2)
                 (find-cps
                  (cdr u) s
                  (λ (f3)
                    (find-cps
                     (cdr v) s
                     (λ (f4)
                       (unify-cps f1 f2 s
                                  (λ (s)
                                    (if s
                                        (unify-cps f3 f4 s k)
                                        #f)))))))))))
       #f)]
      [else #f])))

(test-equal? "unify1"
             (unify-cps '(x y) '(5 6) (empty-s) (empty-k))
             (unify '(x y) '(5 6) (empty-s)))
(test-equal? "unify2"
             (unify-cps 'x 5 (empty-s) (empty-k))
             (unify 'x 5 (empty-s)))
(test-equal? "unify3"
             (unify-cps 5 'x (empty-s) (empty-k))
             (unify 5 'x (empty-s)))
(test-equal? "unify4"
             (unify-cps 'y 6 (empty-s)
                        (λ (u)
                          (unify-cps 'x 5 u (empty-k))))
             (unify 'x 5 (unify 'y 6 (empty-s))))
(test-equal? "unify5"
             (unify-cps '(1 2 3) '(x 1 2) (empty-s) (empty-k))
             (unify '(1 2 3) '(x 1 2) (empty-s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

(define M-cps
  (λ (f k)
    (apply-k
     k
     (λ (ls k^)
       (cond
         [(null? ls) (apply-k k^ '())]
         [else (f (car ls)
                  (λ (f^)
                    (M-cps f
                           (λ (m)
                             (m (cdr ls)
                                (λ (Mrest)
                                  (apply-k
                                   k^
                                   (cons f^ Mrest))))))))])))))

(test-equal? "M1"
             (M-cps null?-cps (λ (f) (f '(1 2 3) (empty-k))))
             ((M null?) '(1 2 3)))
(test-equal? "M2"
             (M-cps null?-cps (λ (f) (f '(1 2 ()) (empty-k))))
             ((M null?) '(1 2 ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

(define use-of-M-cps
  (M-cps (λ (n k) (apply-k k (add1 n)))
         (λ (f) (f '(1 2 3 4 5) (empty-k)))))

(test-equal? "use-of-M"
             use-of-M-cps
             use-of-M)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

(define strange-cps
  (λ (x k)
    ((λ (g k) (lambda (x k) (g g k)))
     (λ (g k) (λ (x k) (g g k)))
     k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

(define use-of-strange-cps
  (strange-cps 5
               (λ (x)
                 (x 6 (λ (y)
                        (y 7 (λ (z)
                               (z 8 (λ (w)
                                      (w 9 (λ (q)
                                             (q 10 (empty-k)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define why-cps
  (λ (f k)
    ((λ (g k)
       (f (λ (x k) (g g (λ (g)
                        (g x (λ (g)
                               (apply-k k g))))))
          k))
     (λ (g k)
       (f (λ (x k) (g g (λ (g)
                        (g x (λ (g)
                               (apply-k k g))))))
          k))
     k)))

(define almost-length
    (lambda (f)
      (lambda (ls)
        (if (null? ls)
            0
            (add1 (f (cdr ls)))))))

(define almost-length-cps
  (λ (f k)
    (apply-k
     k
     (λ (ls k^)
       (if (null? ls)
           (apply-k k^ 0)
           (f (cdr ls) (λ (f)
                         (apply-k k^ (add1 f)))))))))

(test-eq? "why"
          (why-cps almost-length-cps (λ (f) (f '(a b c d e) (empty-k))))
          ((why almost-length) '(a b c d e)))