#lang racket
(define (quick-sort array)
  (cond
    [(empty? array) empty]                                                    
    [else (append 
           (quick-sort (filter (lambda (x) (< x (first array))) array))        
           (filter (lambda (x) (= x (first array))) array)
           (quick-sort (filter (lambda (x) (> x (first array))) array)))]))