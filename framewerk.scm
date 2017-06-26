#!/usr/bin/guile \
-e main -s
!#

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-27))
(use-modules (ice-9 format))


;; init random with the time of day
(let ((time (gettimeofday)))
 (set! *random-state*
  (seed->random-state (+ (car time)
		       (cdr time)))))

;; support functions
(define (round-to-neareast-integer i)
  (inexact->exact 
    (round 
      (if (rational? i) 
	(exact->inexact i)
	i))))

;; die
(define (xd10)
  (let ((n (+ 1 (random-integer 10))))
    (cond
      ((< n 10) n)
      ((= n 10) (+ n (xd10))))))

(define (d10)
  (+ 1 (random-integer 10)))

(define (roll n)
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (vector-set! v i (d10)))
    (vector->list (stable-sort! v <))))


;; These evaluation methods assume a 'pool' is a sorted
;;  list containing a number of single die rolls

;;; Grab the highest value from a rolled pool
(define (highest-value pool)
  (apply max pool))

;;; Grab the highest-sum set of 2 identical die from a rolled pool
(define (highest-identical-values pool)
  (apply max 
	 (map (lambda (n) 
                (let ((c (count (lambda (x) (= x n)) pool)))
                  (if (> c 1)
		    (+ n n)
                     0)))
	      pool)))

;; grab the highest-sum of a consecutive run of 3 or more values
(define (highest-consecutive-values pool)
  (let ((max-run 0))
    (letrec ((grab (lambda (n lst)
		     (append (list n)
			     (if (not (null? lst))
			       (if (= (+ 1 n) (car lst))
				 (grab (car lst) (cdr lst))
				 '())
			       '()))))
	     (search (lambda (lst)
		       (if (not (null? lst))
			 (when (not (null? (cdr lst)))
			   (let ((g (grab (car lst) (cdr lst))))
			     (if (and
				   (>= (length g) 3) 
				   (> (apply + g) max-run))
			       (set! max-run (apply + g))))
			   (search (cdr lst)))))))
      (search pool))
    max-run))
            

;; find the highest value of the different
;;  evaluation methods
(define (evaluate pool)
  (let* ((high-val (highest-value pool))
	 (high-ident (highest-identical-values pool))
	 (straight (highest-consecutive-values pool)))
    (apply max (list high-val high-ident straight))))

;; for printing results
(define (output-stats pool-size results-vector)
  (let ((results-vector (sort results-vector <)))
    (let* ((n (vector-length results-vector))
	   (low (vector-ref results-vector (round-to-neareast-integer (* 0.25 n))))
	   (med (vector-ref results-vector (round-to-neareast-integer (* 0.5 n))))
	   (high (vector-ref results-vector (round-to-neareast-integer (* 0.75 n)))))
      (format #t "~&Pool: ~d,   |~2d|<-----~2d----->|~2d|~%" pool-size low med high))))
      ;(format #t "~d, ~2d, ~2d, ~2d~%" pool-size low med high))))

(define (main argv)
  (let* ((SAMPLE_SIZE 10000)
	 (SKILL '(1 2 3 4 5)))
    (let ((do-sim (lambda (skill)
                     (let ((v (make-vector SAMPLE_SIZE)))
		       (do ((i 0 (+ 1 i)))
			 ((>= i SAMPLE_SIZE))
			 (vector-set! v i (evaluate (roll skill))))
		       (output-stats skill v)))))
	(for-each do-sim SKILL))))

