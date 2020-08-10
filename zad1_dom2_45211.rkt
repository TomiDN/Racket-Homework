#lang racket

(define (row-reduce m)
  (define gauss-row (Gauss-Elim m))
  (map m gauss-row calc-row coef-calc))

(define (calc-row gauss-row row num)
  (if (or (null? gauss-row) (equal? gauss-row row)) row
  (cons (+ (* num (car gauss-row)) (car row)) (calc-row (cdr gauss-row) (cdr row) num))))

(define (Gauss-Elim m)
  (if (not (equal? (car (car m)) 0)) (car m)
      (Gauss-Elim (cdr m))))

(define (coef-calc row1 row2) (- (/ (car row2) (car row1))))

(define (map m row func1 func2)
  (if (null? m) m
      (cons (func1 row (car m) (func2 row (car m))) (map (cdr m) row func1 func2))))
