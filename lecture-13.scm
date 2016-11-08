;; ZERO  = λ f x . x = λ f . (λ x . x)
(define zero (λ (f) (λ (x) x)))
;; ONE   = λ f x . f x
(define one (λ (f) (λ (x) (f x))))
;; TWO   = λ f x . f (f x)
(define two (λ (f) (λ (x) (f (f x)))))
;; THREE = λ f x . f (f (f x))
(define three (λ (f) (λ (x) (f (f (f x))))))

(define lc-nat->int (λ (n) ((n (λ (x) (+ x 1))) 0)))

;; SUCC = λ n . (λ f x . f (n f x))
(define succ (λ (n) (λ (f) (λ (x) (f ((n f) x))))))

;; PLUS = λ n m . n SUCC m
(define plus (λ (n) (λ (m) ((n succ) m))))

;; > (lc-nat->int ((plus two) three))
;; 5

;; TIMES n m = n (PLUS m) zero
(define times (λ (n) (λ (m) ((n (plus m)) zero))))
