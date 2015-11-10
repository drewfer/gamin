#!/usr/bin/guile \
-e main -s
!#

(use-modules (srfi srfi-1)
	     (srfi srfi-27)
	     (ice-9 format)
             (ice-9 getopt-long))

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

(define (roll r k)
  (let ((v (make-vector r)))
    (do ((i 0 (+ i 1)))
      ((>= i r))
      (vector-set! v i (xd10)))
    (apply + (take (vector->list (stable-sort! v >)) k))))

;; for printing results
(define (output-stats roll keep results-vector)
  (let ((results-vector (sort results-vector <)))
    (let* ((n (vector-length results-vector))
	   (low (vector-ref results-vector (round-to-neareast-integer (* 0.25 n))))
	   (med (vector-ref results-vector (round-to-neareast-integer (* 0.5 n))))
	   (high (vector-ref results-vector (round-to-neareast-integer (* 0.75 n)))))
      (format #t "~&~2dk~d,   |~2d|<-----~2d----->|~2d|~%" roll keep low med high))))
      ;(format #t "~2dk~d, ~2d, ~2d, ~2d, ~2d, ~2d~%" roll keep low med high roll keep))))

(define (sim r k)
  (let* ((SAMPLE_SIZE 10000)
	 (ROLL (if (null? r)
                 '(1 2 3 4 5 6 7 8 9 10)
                 r))
	 (KEEP (if (null? k)
                 '(1 2 3 4 5 6 7 8 9 10)
                 k)))
    (let ((do-sim (lambda (r k)
		    (let ((v (make-vector SAMPLE_SIZE)))
		      (do ((i 0 (+ 1 i)))
			((>= i SAMPLE_SIZE))
			(vector-set! v i (roll r k)))
		      (output-stats r k v)))))
      (for-each (lambda (k)
		  (for-each (lambda (r)
			      (if (<= k r)
				(do-sim r k)))
			    ROLL))
		  KEEP))))

(define (show-help)
  (format #t "~&\
  Util to simulate die roll mechanic of the L5R gaming system

  Will print the 25th percentile, 50th (median), and the 75th percentile 
  value of a large set of simulated die rolls

  USAGE: l5r.scm [<options>] [[<kept>] or [<rolled> <kept>]]

  Options:
     -h, --help  - print this help message.

  Arguments:
    kept   - number of kept die
    rolled - number of rolled die

  Notes:
    If only a single argument is specified it is assumed this is a
      kept die query and all stats for that kept die option are printed.

    When two values are given the data for that single query is displayed.

    Rolled must be >= Kept die
~%"))

(define (main argv)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                        (data (single-char #\d) (value #f))))
	 (options (getopt-long argv option-spec))
	 (help-wanted (option-ref options 'help #f))
	 (data-wanted (option-ref options 'data #f)))
  (cond
    (help-wanted
      (show-help))
    (data-wanted
      (sim '() '())) 
    ((= (length argv) 2)
     (sim '() (list (string->number (cadr argv)))))
    ((= (length argv) 3)
     (let ((r (string->number (cadr argv)))
	   (k (string->number (caddr argv))))
       (if (>= r k)
	 (sim (list r) (list k))
	 (show-help))))
    (else (show-help)))))

