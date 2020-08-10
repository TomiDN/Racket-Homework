#lang racket

(define (cross-out m)
  (define M (get-size m 0))
  (define N (get-size (car m) 0))
  (gen m M N M N))

(define (get-size l size)
  (if (null? l) size
      (get-size (cdr l) (+ size 1))))

(define (row-1 list i index)
  (cond ((null? list) list)
        ((eq? i index) (row-1 (cdr list) i (+ index 1)))
        (else (cons (car list) (row-1 (cdr list) i (+ index 1))))))

(define (col-1 list i)
  (if (null? list) list
      (cons (row-1 (car list) i 1) (col-1 (cdr list) i)))) 

(define (gen m M0 N0 M1 N1)
  (cond ((eq? M1 0) '())
        ((> N1 0) (cons (col-1 (row-1 m M1 1) N1) (gen m M0 N0 M1 (- N1 1))))
        (else (gen m M0 N0 (- M1 1) N0))))