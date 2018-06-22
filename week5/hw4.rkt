
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define ones (lambda () (cons 1 ones)))

;; put your code below

(define (sequence low high stride)
  (letrec ([f (lambda (low)
                (if (> low high)
                    '()
                    (cons low (f (+ low stride)))))])
    (f low)))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod xs n)
    (cond [(< n 0) (error "list-nth-mod: negative number")]
          [(null? xs) (error "list-nth-mod: empty list")]
          [#t (car(list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (stream i)
                (let ([pr (stream)])
                  (if(>= i n)
                     (cons (car pr) '())
                     (cons (car pr) (f (cdr pr) (+ 1 i))))))])
    (if(= 0 n)
       '()
       (f s 1))))

(define funny-number-stream
  (letrec ([g (lambda(x)
                (if (= (remainder x 5) 0)
                    (- 0 x)
                    x))]
           [f (lambda (x)
                   (cons (g x) (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (flag)
                (if flag
                (cons "dan.jpg"  (lambda() (f #f)))
                (cons "dog.jpg"  (lambda() (f #t)))))])
    (lambda() (f #t))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (s)
              (cons (cons 0 (car (s))) (lambda() (f (cdr (s))))))])
    (lambda() (f stream))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda(n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (f (+ 1 n)))))])
    (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda(i)
                (if (>= i l)
                    #f
                    (letrec ([ve (vector-ref vec i)])
                      (if(and (pair? ve) (equal? (car ve) v))
                                ve
                                (f (+ 1 i))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [i 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                  ans
                  (let ([new-ans (assoc v xs)])
                    (begin
                      (vector-set! memo i new-ans)
                      (if(= (- n 1) i)
                         (set! i 0)
                         (set! i (+ i 1)))
                      new-ans)))))])
   (lambda(v) (f v))))