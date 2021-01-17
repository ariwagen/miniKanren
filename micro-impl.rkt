#lang racket


;; Our variables are Racket's numbers
(define (var n) n)
(define (var? n) (number? n))

#|
An atomic term is one of:
 | symbol
 | variable
 | boolean
 | () 
A term is one of:
 | atomic term
 | cons term term
|#

#|
An association is a (cons var term)

A substitution is a Listof associations without any cycles

Note for me: A substitution is a series of key / value pairs, akin to a perl hash (key . value)
|#

(define my-subst '((3 . cat) (2 . 3) (4 . fish) (1 . (sea . 2))))


;; !!! WALK !!!
;; takes var and a subst and returns a term
;; looks for variable x in substitution s ... assv finds pair (pr)
;; cdr(pr) is the value corresponding to the key x
;; walk recurses this to make sure that it returns a "final" value

(define (walk x s)
  (let ((pr (assv x s)))
    (if pr (walk (cdr pr) s) x)))

(define my-new-subst '((1 . turtle) (2 . 1) (3 . 2) (4 . 3)))

(define a-terrible-subst '((2 . 1) (1 . 2) (3 . 3) (4 . (turtle . 4))))


;; !!! OCCURS? !!!
;; var and term (vague -- see above) and subst and returns a boolean
;; determines if variable x is present in term v wrt (with respect to) substitution s

(define (occurs? x v s)
  (cond
    ((var? v) (eqv? x v)) ;; if term v is a variable, is it equivalent to x?
    ((cons? v) ;; is v a cons?
     (or (occurs? x (walk (car v) s) s) ;; iterate car
         (occurs? x (walk (cdr v) s) s))) ;; iterate cdr
    (else #f)))


;; !!! EXT-S !!!
;; var and a term and a subst and returns (maybe) a subst
;; extends the subst with (x . v) unless x occurs in v; then it fails (returns #f)
;; in laymans terms; if x not present in v wrt  s, then return s with additional (x . v)

(define (ext-s x v s)
  (cond
    ((occurs? x v s) #f)
    (else `((,x . ,v) . ,s))))


;; !!! UNIFY !!!
;; takes two terms and a substitution, and retuns (maybe) a substitution showing
;; how to make u and v equal wrt all the equations we already have in the substiution
;; fails if the two terms cannot be equal wrt s.
;; only works if u or v is a variable / can be walked to a variable

(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      ((eqv? u v) s) ;; are they the same var already?
      ((var? u) (ext-s u v s)) ;; if they are not already the same, and the first is a var, try to
      ;; make them the same
      ((var? v) (ext-s v u s)) ;; same deal, but if v is a variable
      ((and (cons? u) (cons? v))
       (let ((new-s (unify (car u) (car v) s)))
         (and new-s (unify (cdr u) (cdr v) s))))
      (else #f))))

;; a goal is a function that takes a state ... returns a list of states
;; state is a substitution with a variable counter at the end

;; !!! == !!!
;; 

(define ((== u v) st)
  (let ((s (car st))
        (var-ct (cdr st)))
    (let ((new-s (unify u v s)))
      (if new-s (list `(,new-s . ,var-ct)) '()))))


(define (fake-run g)
  (g '(() . 0)))


;; !!! CALL/FRESH !!!
;; f is a function that "eats" a variable and returns a goal

(define ((call/fresh f) state)
  (let ((s (car state))
        (c (cdr state)))
    ((f (var c))
     `(,s . ,(add1 c)))))


;; !!! DISJ !!!
;; takes one goal & another goal & adds them together

(define (disj g1 g2)
  (lambda (state)
    ($app (g1 state) (g2 state))))


;; !!! CONJ !!!

(define (conj g1 g2)
  (lambda (state)
    ($app-map(g1 state) g2)))


;; $APP
(define ($app $1 $2)
  (cond
    ((empty? $1) $2)
    ((procedure? $1) (lambda () ($app $2 ($1)))) ;; interleaving
    (else (cons (car $1) ($app (cdr $1) $2)))))


;; $APP-MAP
(define ($app-map $ g)
  (cond
    ((empty? $) '())
    ((procedure? $) (lambda () ($app-map ($) g)))
    ((cons? $) ($app (g (car $)) ($app-map (cdr $) g)))))

;; PULL
(define (pull $)
  (if (procedure? $) (pull ($)) $))

;; TAKE
(define (take n $)
  (cond
    ((zero? n) '())
    ((empty? $) '())
    ((equal? n 1) (list (car (pull $))))
    (else
     (let ((p$ (pull $)))
     (cons (car p$) (take (sub1 n) (cdr p$)))))))

(define (run n g)
  (take n (fake-run g)))

;; my own TESTing ?? i'm not the best at this ... yet
;; this is basically just what we did in class :( i'm not sure how i would improve on it

;; this is my formula to solve the quadratic formula ... it's messy right now
;; but this is the sort of code i feel comfortable writing in racket

;; !!! QUADRATIC FORMULA !!!
;; takes a b c from ax^2 + bx + c = 0
;; returns a pair of solutions

(define (quadratic-formula a b c)
  (cond
    ((= a 0) (/ (- c) b))
    (else
  (let ((discriminant (- (* b b) (* 4 (* a c)))))
  (cons (/ (+ (- b) (sqrt discriminant)) (* 2 a)) (/ (- (- b) (sqrt discriminant)) (* 2 a)))))))


(define a-and-b
  (conj 
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh 
    (lambda (b) 
      (disj
       (== b 5)
       (== b 6))))))

;; I'm trying to run this and it's giving me errors :

;; (run 1 (a-and-b empty-state))