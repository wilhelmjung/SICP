;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1

;; Section 1.1.
;; The Elements of Programming

;;Exercise 1.1.
;; Below is a sequence of expressions.
;; What is the result printed by the interpreter
;; in response to each expression? Assume that the sequence
;; is to be evaluated in the order in which it is presented.
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; Exercise 1.2.
;; Translate the following expression into prefix form
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3.
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.
;; values -> tuple
(define (sum-square-of-larger-two x y z)
  (define (sum-square-of-two x y)
    (+ (* x x) (* y y)))
  ; find larger two of three:
  (define (loop x y z)
    (if (<= x y)
        (loop y x z) ; buble y: x <-> y
        (if (<= y z)
            (loop x z y) ; buble z: y <-> z
            (sum-square-of-two x y)))) ; x > y > z
  (loop x y z))

"sum square of larger two: "
(sum-square-of-larger-two 1 2 3)
(sum-square-of-larger-two 1 3 2)
(sum-square-of-larger-two 2 1 3)
(sum-square-of-larger-two 2 3 1)
(sum-square-of-larger-two 3 1 2)
(sum-square-of-larger-two 3 2 1)

;;
;; Exercise 1.4.
;; Observe that our model of evaluation allows for combinations
;; whose operators are compound expressions.
;; Use this observation to describe the behavior of the following
;; procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; (op a b) form will evaluate op first;
;; if b > 0, compute a + |b| as a + b;
;; if b <= 0, compute a + |b| as a - b.

;;
;; Exercise 1.5.
;;
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))

; The evaluation of the expression will never end.
; so we can see that '(p)' will be evaluated before that of 'test',
; that is definitely an app-order evaluation. 

;;
;; Exercise 1.6.
;;
;; the new-if defined in terms of cond:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
; demo:
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

; rewritten square-root prog:
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; other functions:
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x)) 

(define (sqrt* x)
  (sqrt-iter 1.0 x))

; demo:
; WARNING: the excution of this form will never end.
;(sqrt* 3)

; Because both the 'then-clause' and 'else-clause' form
; will be evaluated before that of 'predicate'.
; so 'if' must be implemented as a special form.

;; Exercise 1.7.
;; Implement a better 'good-enough?':

; original sqrt-iter:
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
"sqrt*-1: "
(sqrt* 2)

; rewritten good-enough?
; diff/x < 0.000001
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) (* 0.000001 guess)))

"sqrt*-2: "
(sqrt* 2)

"sqrt: "
(sqrt 2)

"sqrt*-2: 1000000"
(sqrt* 1000000)

"sqrt*-2: 0.000001"
(sqrt* 0.000001)

; yes! this version of good-enough? is work for both
; samll and large numbers.

;;
;; Exercise 1.8.
;; better-guess for cube root
;; y: approximation of x
;; return a better approximation of x.
(define (better-approximation x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cube-root x)
  (define (cube x) (* x x x))
  (define (good-enough-3? guess x)
    (< (abs (- (cube guess) x)) (* 0.000001 guess)))
  (define (cube-root-iter guess x)
    (if (good-enough-3? guess x)
        guess
        (cube-root-iter (better-approximation x guess) x)))       
  (cube-root-iter 1.0 x))

"cube-root of 0.000001:"
(cube-root 0.000001)

"cube-root of 1000000:"
(cube-root 1000000)
