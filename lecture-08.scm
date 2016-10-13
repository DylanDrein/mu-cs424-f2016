;;; Tail Recursion
;;;  - Tail Calls
;;;    - Tail Call Elimination
;;;      - Tail Recursive Loops

;; foo() {
;;   bar();
;;   return baz();       // arrange for baz to return directly to caller of foo
;;                       // is TCO = Tail Call Optimization
;; }

(define dub (λ (n) (if (zero? n) 0 (+ 2 (dub (- n 1))))))

(define dub_ (λ (n) (dub_aux n 0)))

(define dub_aux
  (λ (n a)
    (if (zero? n)
	a
	(dub_aux (- n 1) (+ 2 a)))))

;;; dub_aux in (pidgin) machine code
;;;
;;; dub_aux:     // r1 holds n, r2 hold a
;;;          cmp r1 0
;;;          jmp_t is_zero
;;;          sub #1 r1
;;;          add #2 r2
;;;          jmp dub_aux
;;; is_zero:
;;;          mv r2 r0          // r0 hold return values
;;;          jmp (*returnStack--)

;;; Fibbonacci Sequence
;;; f(0) = 1
;;; f(1) = 1
;;; f(n) = f(n-1) + f(n-2)     for n>1

(define fibb (λ (n) (if (<= n 1) 1 (+ (fibb (- n 1)) (fibb (- n 2))))))

;;; function fibb(n)
;;;  f_im1 := 1
;;;  f_im2 := 1
;;;  for i=2 .. n-1     // interesting program point!
;;;    f_i := fim1 + fim2
;;;    f_im2 := f_im1
;;;    f_im1 := f_i
;;;  end
;;;  return f_im1 + f_im2

(define fibb_ (λ (n) (fibb_pp n 2 1 1)))
(define fibb_pp
  (λ (n i f_i-1 f_i-2)
    (let ((f_i (+ f_i-1 f_i-2)))
      (if (= i n)
	  f_i
	  (fibb_pp n (+ i 1) f_i f_i-1)))))
