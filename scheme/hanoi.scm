#!/bin/guile -s
!#

;��ŵ����Ϸ--schemeʵ��

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;��ջģ��
(define (front-ptr stack) (car stack))

(define (rear-ptr stack) (cdr stack))

(define (set-front-ptr! stack item) (set-car! stack item))

(define (set-rear-ptr! stack item) (set-cdr! stack item))

(define (empty-stack? stack) (null? (front-ptr stack)))

(define (make-stack) (cons '() '()))

(define (front-stack stack)
    (if (empty-stack? stack)
        '()                      ;����ջΪ��ʱ��ȡ���˷��ؿձ�
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
;��ŵ��ģ���ڲ�ʵ��
;ʹ�ö�ջģ�麯����Ϊ�ӿڣ�������ŵ��

;��ʼ��һ����
(define (init-hanoi level)
    (define (init-hanoi-iter level hanoi)
        (if (= level 0)
            hanoi
            (init-hanoi-iter (- level 1) (push-stack! hanoi level))))
    (init-hanoi-iter level (make-stack)))

;��ŵ����ɶȼ�⣬��ŵ����Ϸ�Ƿ���ɣ�
(define (complete-hanoi? level hanoi)
    (if (= level (length (print-stack hanoi)))
        #t
        #f))

;��ӡ��һ����ŵ��
(define (print-hanoi hanoi)
    (print-stack hanoi))

;��ŵ����������
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

;�ƶ�һ�����ϵĶ�Բ�̵���һ����,���޷��ƶ��򱣳�ԭ״̬���䡣
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
;��ŵ���㷨ģ��

;��ʾ������ŵ��
(define (print-all-hanoi h1 h2 h3)
    (let ((a (print-hanoi h1))
          (b (print-hanoi h2))
          (c (print-hanoi h3)))
         (begin
            (display a)
            (display b)
            (display c)
            (newline))))

;�ݹ���hanoi���㷨
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
;���ԣ���ŵ������
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
