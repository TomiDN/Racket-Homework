#lang racket

(define (argmax func list)
  (cmp (cdr list) > (car list) func))

(define (argmin func list)
  (cmp (cdr list) < (car list) func))

(define (cmp list op key func)
  (cond ((null? list) key)
        ((op (func (car list)) (func key)) (cmp (cdr list) op (car list) func))
        (else (cmp (cdr list) op key func))))

(define (% a b) (remainder a b))

(define (** a n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (** a (- n))))
        (else (* a (** a (- n 1))))))

(define (reduce num)
  (if (< num 10) num
      (let* ((list (reverse (listify num '())))
             (max (maxEl (car list) (cdr list)))
             (mult (** 10 (- (length list) 2))))
            (reduce (* (numberify max 0 #f mult list) max)))))

(define (listify num list)
  (if (< num 10) (cons num list)
      (cons (% num 10) (listify (/ (- num (% num 10)) 10) list))))

(define (maxEl num list)
  (cond ((null? list) num)
        ((> (car list) num) (maxEl (car list) (cdr list)))
        (else (maxEl num (cdr list)))))

(define (numberify max num past mult list)
  (cond ((null? list) num)
        ((or (equal? past #t) (not (equal? (car list) max)))
         (numberify max (+ num (* (car list) mult)) past (/ mult 10) (cdr list)))
        (else (numberify max num #t mult (cdr list)))))

(define (-> x) (exact->inexact x))

(define (solver f a b approx cnt)
  (define mid (/ (+ a b) 2))
  (cond ((or (or (approx (f mid)) (equal? (f mid) 0))
            (approx (- b a))) (cons (-> mid) cnt))
        ((< (* (f a) (f mid)) 0) (solver f a mid approx (+ cnt 1)))
        (else (solver f mid b approx (+ cnt 1)))))

(define (find-root f a b eps)
  (define (approx-zero? x) (< (abs x) eps))
  (solver f a b approx-zero? 0))
