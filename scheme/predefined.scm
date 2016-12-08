#lang r5rs

;(define-syntax error
;  (syntax-rules ()
;    ((_ REASON ARG ...) (error REASON ARG ...))))

(define-syntax error
    (syntax-rules ()
      ((error) (begin (newline) "ERROR-Output END"))
      ((error a) (display a))
      ((error a b ...) (begin (display a) (display " ") (error b ...)))))

(define true #t)

(define false #f)

(define nil '())

(define (identity x) x)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

;;; @section Streams

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define the-empty-stream '())

(define (stream-null? x) (null? x))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr?stream)))
