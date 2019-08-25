#lang racket

(provide (all-defined-out));; so we can put tests in a second file

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


(define (cycle-lists xs ys)
  (letrec ([helper (lambda(xs ys n) 
                     (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda() (helper xs ys (+ n 1)))))])
    (lambda() (helper xs ys 0))))


(define (vector-assoc v vec)
  (letrec ([size (vector-length vec)]
        [vector-assoc-helper (lambda(idx) 
                               (if (< idx size)
                                   (begin (let ([elm (vector-ref vec idx)])
                                            (if (and (pair? elm) (equal? (car elm) v))
                                                elm
                                                (vector-assoc-helper (+ idx 1)))))
                                   #f))])
    (vector-assoc-helper 0)))


(define (cached-assoc xs n)
  (let ([memo (make-vector n)]
        [pos 0])
    (lambda(key)
      (let ([cache_found (assoc key memo)])
        (if cache_found
          cache_found
          (begin
            (let ([found (assoc key xs)])
              (if found
                  (begin
                    (vector-set! memo pos found)
                    (set! pos (remainder (+ pos 1) n))
                    found)
                  #f))))))))
      

(define (fib n)
  (if (<= n 0)
      (error "invalid argument")
      (letrec ([memo null]
               [fib_helper 
                (lambda(n)
                  (letrec ([v (assoc n memo)])
                    (if v
                        (cdr v)
                        (begin 
                          (letrec ([new_ans (if (>= n 3)
                                                (+ (fib_helper (- n 2)) 
                                                   (fib_helper (- n 1)))
                                                1)])
                            (set! memo (cons (cons n new_ans) memo))
                            new_ans)))))])
      (fib_helper n))))

(define (fib_slow n)
  (if (<= n 0)
      (error "invalid argument")
      (letrec ([fib_helper 
                (lambda(n memo)
                  (letrec ([v (assoc n memo)])
                    (if v
                        (cdr v)
                        (begin 
                          (letrec ([new_ans (if (>= n 3)
                                                (+ (fib_helper (- n 2) memo) 
                                                   (fib_helper (- n 1) memo))
                                                1)])
                            (set! memo (cons (cons n new_ans) memo))
                            new_ans)))))])
      (fib_helper n null))))