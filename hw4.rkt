#lang racket

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))
  

(define (list-nth-mod xs n)
  (cond ([< n 0] (error "list-nth-mod: negative number")) 
        ([null? xs] (error "list-nth-mod: empty list"))
        (#t (begin (let ([r (remainder n (length xs))])
                     (car (list-tail xs r)))))))        

(define (stream-for-n-steps s n)
  (if (= n 0) 
      null
      (letrec ([ p (s)] [num (car p)] [next_s (cdr p)])
        (cons num stream-for-n-steps next_s (- n 1)))))

(define funny-number-stream
  (letrec ([funny-number-stream-helper 
            (lambda (i) 
              (if (= (remainder i 5) 0)
                  (cons (* -1 i) (lambda () (funny-number-stream-helper (+ i 1))))
                  (cons i (lambda () (funny-number-stream-helper (+ i 1))))))])
    (lambda() (funny-number-stream-helper 1))))


(define dan-then-dog
  (letrec ([dan-then-dog-helper 
            (lambda (pick) 
              (if (= pick 1) 
                  (cons "dan.jpg" (lambda() (dan-then-dog-helper 2))) 
                  (cons "dog.jpg" (lambda() (dan-then-dog-helper 1)))))])
    (lambda() (dan-then-dog-helper 1))))            


(define (stream-add-zero p0)
  (letrec ([pair (p0)]
           [v1 (cons 0 (car pair))]
           [p1 (cdr pair)])
    (lambda() (cons v1 (stream-add-zero p1)))))