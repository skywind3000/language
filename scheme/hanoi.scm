#!/bin/guile -s
!#

;汉诺塔游戏--scheme实现

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;堆栈模块
(define (front-ptr stack) (car stack))

(define (rear-ptr stack) (cdr stack))

(define (set-front-ptr! stack item) (set-car! stack item))

(define (set-rear-ptr! stack item) (set-cdr! stack item))

(define (empty-stack? stack) (null? (front-ptr stack)))

(define (make-stack) (cons '() '()))

(define (front-stack stack)
    (if (empty-stack? stack)
        '()                      ;当堆栈为空时，取顶端返回空表
        (car (front-ptr stack))))


(define (push-stack! stack item)
    (let ((new-pair (cons item '())))
        (cond ((empty-stack? stack)
               (set-front-ptr! stack new-pair)
               (set-rear-ptr! stack new-pair)
               stack)
              (else
               (set-cdr! new-pair (front-ptr stack))
               (set-front-ptr! stack new-pair)
               stack))))

(define (pop-stack! stack)
    (cond ((empty-stack? stack)
           (error "POP! called with an empty stack" stack))
          (else
           (set-front-ptr! stack (cdr (front-ptr stack)))
           stack)))

(define (print-stack stack)
    (front-ptr stack))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;汉诺塔模块内部实现
;使用堆栈模块函数做为接口，操作汉诺塔

;初始化一个塔
(define (init-hanoi level)
    (define (init-hanoi-iter level hanoi)
        (if (= level 0)
            hanoi
            (init-hanoi-iter (- level 1) (push-stack! hanoi level))))
    (init-hanoi-iter level (make-stack)))

;汉诺塔完成度检测，汉诺塔游戏是否完成？
(define (complete-hanoi? level hanoi)
    (if (= level (length (print-stack hanoi)))
        #t
        #f))

;打印出一个汉诺塔
(define (print-hanoi hanoi)
    (print-stack hanoi))

;汉诺塔移塔规则
(define (can-be-push? hanoi item)
    (if (null? (front-stack hanoi))
        #t
        (if (> (front-stack hanoi) item)
            #t
            #f)))

(define (can-be-pop? hanoi)
    (if (null? (front-stack hanoi))
        #f
        #t))

;移动一个塔上的顶圆盘到另一个塔,若无法移动则保持原状态不变。
(define (move-hanoi hanoi-x hanoi-y)
    (if (can-be-pop? hanoi-x)
        (let ((disk (front-stack hanoi-x)))
             (if (can-be-push? hanoi-y disk)
                 (begin
                    (push-stack! hanoi-y disk)
                    (pop-stack! hanoi-x)
                    #t)))
         #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;汉诺塔算法模块

;显示三个汉诺塔
(define (print-all-hanoi h1 h2 h3)
    (let ((a (print-hanoi h1))
          (b (print-hanoi h2))
          (c (print-hanoi h3)))
         (begin
            (display a)
            (display b)
            (display c)
            (newline))))

;递归版的hanoi塔算法
(define (dohanoi n to from using)
  (if (> n 0)
      (begin
        (dohanoi (- n 1) using from to)
        (move-hanoi from to)
        (dohanoi (- n 1) to using from) 
        #t)
      #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;测试，汉诺塔雏形
(define level 5)

(define h1 (init-hanoi level))

(define h2 (init-hanoi 0))

(define h3 (init-hanoi 0))

(print-all-hanoi h1 h2 h3)

(dohanoi level h3 h1 h2)
(newline)

(display (complete-hanoi? level h3))
(newline)
(newline)

(print-all-hanoi h1 h2 h3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
