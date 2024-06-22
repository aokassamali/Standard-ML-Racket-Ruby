
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([f (lambda (acc current)
                (if (>= current high)
                    (reverse acc)
                    (f (cons current acc) (+ current stride))))])
    (f null low)))


(define (string-append-map xs suffix) (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n) (cond
                              [(< n 0) (error "list-nth-mod: negative number")]
                              [(null? xs) (error "list-nth-mod: empty list")]
                              [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (let loop ([acc null] [count n] [thunk (s)])
    (if (= count 0)
        (reverse acc)
        (loop (cons (car thunk) acc) (- count 1) ((cdr thunk))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))


(define dan-then-dog
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([len-xs (length xs)] [len-ys (length ys)] [f (lambda (n)
                (cons (cons (list-nth-mod xs (remainder n len-xs)) (list-nth-mod ys (remainder n len-ys)))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))
                          