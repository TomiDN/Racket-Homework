#lang racket

(define (expr->tree e)
  (if (null? e) '()
      (list (root e) (expr->tree (left e)) (expr->tree (right e)))))

(define (right e)
  (if (and (list? e) (eq? (length e) 3)) (caddr e) '()))

(define (left e)
  (if (and (list? e) (> (length e) 1)) (cadr e) '()))

(define (root e)
  (if (list? e) (car e) e))

(define (tree-eval t x)
  (define el (get-el (car t) x))
  (if (op? el)
      (el (tree-eval (cadr t) x) (tree-eval (caddr t) x)) el))

(define (op? el)
  (if (number? el) #f #t))
     
(define (get-el arg x)
   (cond ((eq? arg 'x) x)
         ((number? arg) arg)
         (else (eval arg))))

(define (derive-expt l)
  (list '* (caddr l) (list (car l) (cadr l) (list '- (caddr l) '(1 () ())))))

(define (derive-mul l)
  (list '+
        (list '* (tree-derive (cadr l)) (caddr l))
        (list '* (tree-derive (caddr l)) (cadr l))))

(define (derive-div l)
  (list '/
        (list '-
              (list '* (tree-derive (cadr l)) (caddr l))
              (list '* (tree-derive (caddr l)) (cadr l)))
        (list '* (caddr l) (caddr l))))

(define (tree-derive t)
  (if (eq? (car t) 'expt) (derive-expt t)
      (if (eq? (car t) '*) (derive-mul t)
          (if (eq? (car t) '/) (derive-div t)
              (if (eq? (car t) 'x) '(1 () ())
                  (if (number? (car t)) '(0 () ())
      (list (car t) (tree-derive (cadr t)) (tree-derive (caddr t)))))))))
