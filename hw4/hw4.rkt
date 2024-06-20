
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