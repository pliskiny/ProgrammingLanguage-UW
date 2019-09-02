;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct closure (env fun) #:transparent) ;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to

;; Problem 1
(define (racket-foldl f xs acc)
  (if (null? xs)
      acc
      (racket-foldl f (cdr xs) (f (car xs) acc))))

(define (racket-foldr f xs acc)
  (if (null? xs)
      acc
      (f (car xs) (racket-foldr f (cdr xs) acc))))

(define (mupl-foldr f xs acc)
  (if (aunit? (eval-exp xs))
      acc              
      (f (apair-e1 xs) (mupl-foldr f (apair-e2 xs) acc))))

(define (racketlist->mupllist xs)
  (racket-foldr (lambda(first second) (apair first second)) xs (aunit)))

(define (mupllist->racketlist2 xs)
  (mupl-foldr (lambda(first second) (list first second)) xs null))

(define (mupllist->racketlist xs) 
  (if (aunit? xs)
      (list)
      (begin
        (let ([fst (apair-e1 xs)]
              [snd (apair-e2 xs)])              
          (if (aunit? snd)
              (list fst)
              (begin
                (letrec ([helper-fn (lambda(xs) (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs))))])
                  (helper-fn xs))))))))


;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-exp e)
  (eval-under-env e null))

(define (eval-under-env e env)
  (cond [(aunit? e) e]

        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]         

        [(var? e) 
         (envlookup env (var-string e))]

        [(int? e) e]

        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)] [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (int (+ (int-num v1) (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)] [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]

        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)] [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(fst? e)
         (let ([pair (fst-e e)])
           (if (apair? pair)
             (eval-under-env (apair-e1 pair) env)
             (error "not apair, can not get first part")))]

        [(snd? e)
         (let ([pair (snd-e e)])
           (if (apair? pair)
             (eval-under-env (apair-e2 pair) env)
             (error "not apair, can not get second part")))]        

        [(mlet? e)
         (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) null))]

        [(fun? e)
         (closure (list) e)]

        [(call? e)
         (let ([exp-of-closure (call-funexp e)])
           (if (closure? exp-of-closure)
               (letrec ([exp-of-fun (closure-fun exp-of-closure)]                        
                        [body-of-fun (fun-body exp-of-fun)]
                        [arg-name (fun-formal exp-of-fun)] 
                        [actual-arg (eval-under-env (call-actual e) env)]                             
                        [env-of-closure (cons (cons arg-name actual-arg) (closure-env exp-of-closure))])
                 (eval-under-env body-of-fun env-of-closure))
               (error "call e1 e2, e1 is not closure")))]

        [(closure? e) e]
        
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (isaunit e1) (eval-exp e2) (eval-exp e3)))

(define (add-new-var-env elm env)
  (let ([name (car elm)]
        [val (eval-under-env (cdr elm) env)])
    (cons (cons name val) env)))

(define (mlet* lstlst e2)
  (let ([env (racket-foldl add-new-var-env lstlst null)])
    (eval-under-env e2 env)))

(define (ifeq e1 e2 e3 e4)
  (if (and (int? e1) (int? e2))
      (if (= (int-num e1) (int-num e2))
        (eval-under-env e3 null)
        (eval-under-env e4 null))
      (error "MUPL ifeq applied to non-number")))

;; Problem 4
(define (racket-map f)
  (letrec ([ret-fn (lambda(xs)
                     (if (null? xs)                         
                         null
                         (cons (f (car xs)) (ret-fn (cdr xs)))))])
    ret-fn))

(define (mupl-map f)
  (letrec ([ret-fn (fun #f "xs"
                        (if (isaunit (var "xs"))
                            (aunit)
                            (apair (call (eval-exp f) (fst (var "xs"))) (call (eval-exp ret-fn) (snd (var "xs"))))))])
    ret-fn))
  


(define mupl-mapAddN 
  (mlet "map" mupl-map
        (call (var "map") (fun #f "x" (add (var "x") (int 9))))))

(define (mupl-mapAddS i)
  (mupl-map (lambda(x) (eval-exp (add i x)))))
  

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
