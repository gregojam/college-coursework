#lang racket
(require rackunit)

;; countdown : Natural -> [Listof Natural]
;; returns a list of natural numbers equal to or less than a given
;; natural number
(define countdown
  (λ (n)
    (cond
      [(zero? n) (cons n '())]
      [else (cons n (countdown (sub1 n)))])))

(test-equal? "countdown1" (countdown 0) '(0))
(test-equal? "countdown2" (countdown 5) '(5 4 3 2 1 0))

;; insertR : Symbol Symbol [Listof Symbol] -> [Listof Symbol]
;; returns a list with given Symbol sym2 inserted after each occurrence
;; of a given Symbol sym1
(define insertR
  (λ (sym1 sym2 los)
    (cond
      [(null? los) '()]
      [(eqv? (car los) sym1) (cons sym1
                                   (cons sym2
                                         (insertR sym1 sym2 (cdr los))))]
      [else (cons (car los) (insertR sym1 sym2 (cdr los)))])))

(test-equal? "ins1" (insertR 'x 'y '(x z z x y x)) '(x y z z x y y x y))
(test-equal? "ins2" (insertR 'a 'b '(a a a a a)) '(a b a b a b a b a b))
(test-equal? "ins3" (insertR 'a 'b '()) '())
(test-equal? "ins4" (insertR 'a 'b '(b c d e f)) '(b c d e f))

;; remv-1st : Symbol [Listof Symbol] -> [Listof Symbol]
;; removes the first occurrence of Symbol sym from a list
(define remv-1st
  (λ (sym los)
    (cond
      [(null? los) '()]
      [(eqv? sym (car los)) (cdr los)]
      [else (cons (car los) (remv-1st sym (cdr los)))])))

(test-equal? "remv1" (remv-1st 'x '(x y z x)) '(y z x))
(test-equal? "remv2"(remv-1st 'x '(a b c d)) '(a b c d))

;; An Atom is one of:
;; - Number
;; - String
;; - Boolean
;; - Symbol

;; list-index-ofv? : Atom [Listof Atom] -> Natural
;; returns the index of a given Atom thing in a given List lot
;; Assumes -- thing is a member of lot
(define list-index-ofv?
  (λ (thing lot)
    (cond
      [(eqv? thing (car lot)) 0]
      [else (add1 (list-index-ofv? thing (cdr lot)))])))

(test-eq? "index1" (list-index-ofv? 'x '(x y z x x)) 0)
(test-eq? "index2" (list-index-ofv? 'x '(y z x x)) 2)

;; filter : Predicate [Listof X] -> [Listof X]
;; removes the elements of a list that do not satisfy a given predicate
;; Assumes -- pred? is applicable to each element of lox
(define filter
  (λ (pred? lox)
    (cond
      [(null? lox) '()]
      [(pred? (car lox)) (cons (car lox) (filter pred? (cdr lox)))]
      [else (filter pred? (cdr lox))])))

(test-equal? "filter1" (filter even? '(1 2 3 4 5 6)) '(2 4 6))
(test-equal? "filter2" (filter odd? '(2 4 6 8)) '())

;; zip : [Listof X] [Listof Y] -> [Listof (X . Y)]
;; returns a list of pairs of corresponding elements of two given lists
(define zip
  (λ (lox loy)
    (cond
      [(or (null? lox) (null? loy)) '()]
      [else (cons (cons (car lox) (car loy))
                  (zip (cdr lox) (cdr loy)))])))

(test-equal? "zip1" (zip '(1 2 3) '(a b c)) '((1 . a) (2 . b) (3 . c)))
(test-equal? "zip2"
             (zip '(1 2 3 4 5 6) '(a b c)) '((1 . a) (2 . b) (3 . c)))
(test-equal? "zip3"
             (zip '(1 2 3) '(a b c d e f)) '((1 . a) (2 . b) (3 . c)))

;; map : Procedure [Listof X] -> [Listof Y]
;; applies Procedure p to each element of a given List ls
;; Assumes -- p takes one argument and is applicable to each
;; element of ls
(define map
  (λ (p ls)
    (cond
      [(null? ls) '()]
      [else (cons (p (car ls)) (map p (cdr ls)))])))

(test-equal? "map1" (map add1 '(1 2 3 4)) '(2 3 4 5))
(test-equal? "map2" (map symbol->string '(a b c d)) '("a" "b" "c" "d"))

;; append : [Listof X] [Listof Y] -> [Listof Z]
;; appends List ls1 to List ls2
(define append
  (λ (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [else (cons (car ls1) (append (cdr ls1) ls2))])))

(test-equal? "append1" (append '(a b c) '(1 2 3)) '(a b c 1 2 3))
(test-equal? "append2" (append '() '(1 2 3)) '(1 2 3))
(test-equal? "append3" (append '(a b c) '()) '(a b c))

;; reverse : [Listof X] -> [Listof X]
;; returns the reverse of a given list
(define reverse
  (λ (lox)
    (cond
      [(null? lox) '()]
      [else (append (reverse (cdr lox)) `(,(car lox)))])))

(test-equal? "reverse1" (reverse '(a 3 x)) '(x 3 a))
(test-equal? "reverse2" (reverse '(d (2 4) x 8)) '(8 x (2 4) d))

;; fact : Natural -> Natural
;; computes the factorial of a given Natural n
(define fact
  (λ (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))

(test-eq? "fact1" (fact 0) 1)
(test-eq? "fact2" (fact 5) 120)

;; A ListOr is one of
;; - [Listof Atom}
;; - Boolean

;; memv : Atom [Listof Atom] -> ListOr
;; returns a list containing the first occurrence of a given Atom thing
;; and every element following in a given List ls or #f if thing is not
;; a member of ls
(define memv
  (λ (thing ls)
    (cond
      [(null? ls) #f]
      [(eqv? thing (car ls)) ls]
      [else (memv thing (cdr ls))])))

(test-equal? "memv1" (memv 'a '(a b c)) '(a b c))
(test-eq? "memv2" (memv 'b '(a ? c)) #f)
(test-equal? "memv3" (memv 'b '(a b c b)) '(b c b))

;; fib : Natural -> Natural
;; computes the nth number of the Fibonacci Sequence where n
;; is a given Natural
(define fib
  (λ (n)
    (cond
      [(zero? n) 0]
      [(zero? (sub1 n)) 1]
      [else (+ (fib (sub1 n)) (fib (- n 2)))])))

(test-eq? "fib1" (fib 0) 0)
(test-eq? "fib2" (fib 1) 1)
(test-eq? "fib3" (fib 7) 13)


;;***********************************************
;; '((w . (x . ())) . (y . ((z . ()))))
;;***********************************************


;; A BD is one of
;; - 0
;; - 1

;; binary->natural : [Listof BD] -> Natural
;; converts an unsigned, reverse bit binary number into a decimal number
(define binary->natural
  (λ (bin)
    (cond
      [(null? bin) 0]
      [(zero? (car bin)) (* 2 (binary->natural (cdr bin)))]
      [else (add1 (* 2 (binary->natural (cdr bin))))])))

(test-eq? "binary1" (binary->natural '()) 0)
(test-eq? "binary2" (binary->natural '(0 0 1)) 4)
(test-eq? "binary3" (binary->natural '(0 0 1 1)) 12)
(test-eq? "binary4" (binary->natural '(1 1 1 1)) 15)
(test-eq? "binary5" (binary->natural '(1 0 1 0 1)) 21)
(test-eq? "binary6" (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191)

;; minus : Int Int -> Int
;; subtracts Int n2 from Int n1
(define minus
  (λ (n1 n2)
    (cond
      [(zero? n2) n1]
      [else (if (> n2 0)
                (sub1 (minus n1 (sub1 n2)))
                (add1 (minus n1 (add1 n2))))])))

(test-eq? "minus1" (minus 5 3) 2)
(test-eq? "minus2" (minus 100 50) 50)
(test-eq? "minus3" (minus 50 100) -50)
(test-eq? "minus4" (minus 7 -10) 17)
(test-eq? "minus4" (minus -4 2) -6)

;; div : Int Int -> Int
;; performs Euclidean division on n1 by n2
;; Assumes -- n2 is non-zero
(define div
  (λ (n1 n2)
    (cond
      [(and (< n1 0)
            (< n2 0)) (cond
                        [(zero? (add1 n2)) (* -1 n1)]
                        [(>= n1  n2) 1]
                        [else (add1 (div (minus n1 n2) n2))])]
      [(< n1 0) (* -1 (div n1 (* -1 n2)))]
      [(< n2 0) (* -1 (div n1 (* -1 n2)))]
      [else (cond
              [(zero? (sub1 n2)) n1]
              [(< n1 n2) 0]
              [else (add1 (div (minus n1 n2) n2))])])))

(test-eq? "div1" (div 4 2) 2)
(test-eq? "div2" (div 17 5) 3)
(test-eq? "div4" (div 4 -2) -2)
(test-eq? "div5" (div 4 -3) -1)
(test-eq? "div6" (div -10 -5) 2)
(test-eq? "div7" (div -10 -6) 2)
(test-eq? "div8" (div -9 3) -3)
(test-eq? "div9" (div -9 4) -3)
(test-eq? "div3" (div 0 3) 0)

;; append-map : Procedure [Listof X] -> [Listof Y]
;; applies p to each element of ls such that each result
;; of p's application is a list and each list appended together
(define append-map
  (λ (p ls)
    (cond
      [(null? ls) '()]
      [else (append (p (car ls)) (append-map p (cdr ls)))])))

(test-equal? "append-map" (append-map countdown (countdown 5))
             '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0))

;; set-difference : [Listof X] [Listof Y] -> [Listof Z]
;; returns a list containing all the elements in s1 that are not in s2
;; Assumes -- both lists are flat sets
(define set-difference
  (λ (s1 s2)
    (cond
      [(null? s1) '()]
      [(not (member (car s1) s2))
       (cons (car s1)
             (set-difference (cdr s1) s2))]
      [else (set-difference (cdr s1) s2)])))

(test-equal? "setdif1"
             (set-difference '(1 2 3 4 5) '(2 4 6 8)) '(1 3 5))
(test-equal? "setdif2"
             (set-difference '(a b (1 2) c) '(b d (1 s))) '(a (1 2) c))
(test-equal? "setdif3"
             (set-difference '(a b (1 2) c) '(b d (1 2))) '(a c))