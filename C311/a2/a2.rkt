#lang racket

;; list-ref : [Listof X] Natural -> X
;; returns the nth element of a list
(define list-ref
  (λ (ls n)
    (letrec
        ((nth-cdr
          (λ (n)
            (cond
              [(zero? n) ls]
              [else (cdr (nth-cdr (sub1 n)))]))))
      (car (nth-cdr n)))))

;; union : [Listof X] [Listof Y] -> [Listof Z]
;; returns the union of two lists
(define union
  (λ (ls1 ls2)
    (letrec
        ((dupe-check
          (λ (ls2)
            (cond
              [(null? ls2) '()]
              [(member (car ls2) ls1) (dupe-check (cdr ls2))]
              [else (cons (car ls2) (dupe-check (cdr ls2)))]))))
      (append ls1 (dupe-check ls2)))))

;; extend : Any Pred -> Boolean
;; extends a predicate to also include a given value
(define extend
  (λ (x pred)
    (λ (y)
      (or (equal? x y) (pred y)))))

;; walk-symbol : Symbol [Listof (X . Y)] -> Y
;; returns the associated value of x in a given association list
(define walk-symbol
  (λ (x s)
    (letrec
        ((walker
          (λ (ls)
            (cond
              [(null? ls) x]
              [(and (equal? x (caar ls))) (symbol? (cdar ls))
               (walk-symbol (cdar ls) s)]
              [(equal? x (caar ls)) (cdar ls)]
              [else (walker (cdr ls))]))))
      (walker s))))

;; LCE : Symbol | (lambda (x) [body]) | (LCE LCE)

;; lambda->lumbda : LCE -> X
;; changes lambda keyword to lumbda
(define lambda->lumbda
  (λ (lce)
    (match lce
      [`,y #:when (symbol? y) y]
      [`(lambda ,rand ,rator) `(lumbda ,rand ,(lambda->lumbda rator))]
      [`(,first ,second) `( ,(lambda->lumbda first)
                            ,(lambda->lumbda second))])))

;; var-occurs? : Symbol LCE -> Boolean
;; determines whether a variable occurs in an LCE
(define var-occurs?
  (λ (s lce)
    (match lce
      [`,y #:when (symbol? y) (eqv? s y)]
      [`(lambda ,rand ,rator) (var-occurs? s rator)]
      [`(,first ,second) (or (var-occurs? s first)
                             (var-occurs? s second))])))

;; vars : LCE -> [Listof Symbol]
;; returns a list of all variables in an LCE
(define vars
  (λ (lce)
    (match lce
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda ,rand ,rator) (append (vars rator))]
      [`(,first ,second) (append (vars first)
                                 (vars second))])))

;; unique-vars : LCE -> [Listof Symbol]
;; returns a list of all variables in an LCE without duplicates
(define unique-vars
  (λ (lce)
    (match lce
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda ,rand ,rator) (append (unique-vars rator))]
      [`(,first ,second) (union (unique-vars first)
                                (unique-vars second))])))

;; var-occurs-free? : Symbol LCE -> Boolean
;; determines whether a variable occurs freely in an LCE
(define var-occurs-free?
  (λ (s lce)
    (match lce
      [`,y #:when (symbol? y) (eqv? s lce)]
      [`(lambda ,rand ,rator) (and (var-occurs-free? s rator)
                                   (not (eqv? s (car rand))))]
      [`(,first ,second) (or (var-occurs-free? s first)
                             (var-occurs-free? s second))])))

;; var-occurs-bound? : Symbol LCE -> Boolean
;; determines whether a variable occurs bound in an LCE
(define var-occurs-bound?
  (λ (s lce)
    (match lce
      [`,y #:when (symbol? y) #f]
      [`(lambda ,rand ,rator) (or (and (var-occurs? s rator)
                                       (eqv? s (car rand)))
                                  (var-occurs-bound? s rator))]
      [`(,first ,second) (or (var-occurs-bound? s first)
                             (var-occurs-bound? s second))])))

;; unique-free-vars : LCE -> [Listof Symbol]
;; returns a list of all of the variables that occur freely in an LCE
(define unique-free-vars
  (λ (lce)
    (letrec
        ((aux
          (λ (a)
            (match a
              [`,y #:when (symbol? y) (if (var-occurs-free? y lce)
                                          `(,y)
                                          '())]
              [`(lambda ,rand ,rator) (append (aux rator))]
              [`(,first ,second) (union (aux first)
                                        (aux second))]))))
      (aux lce))))

;; unique-bound-vars : LCE -> [Listof Symbol]
;; returns a list of all of the variables that occur bound in an LCE
(define unique-bound-vars
  (λ (lce)
    (letrec
        ((aux
          (λ (a)
            (match a
              [`,y #:when (symbol? y) (if (var-occurs-bound? y lce)
                                          `(,y)
                                          '())]
              [`(lambda ,rand ,rator) (append (aux rator))]
              [`(,first ,second) (union (aux first)
                                        (aux second))]))))
      (aux lce))))

;; lex : LCE empty -> [Listof [Listof X]]
;; returns a list of each bound variable in an LCE paired
;; with its lexical address
(define lex
  (λ (lce ls)
    (match lce
      [`,y #:when (symbol? y) (if (memv y ls)
                                  `(var ,(length (takef ls (λ (e) (not (eqv? e y))))))
                                  '())]
      [`(lambda ,rand ,rator) `(lambda ,(lex rator (append rand ls)))]
      [`(,first ,second) `(,(lex first ls) ,(lex second ls))])))