#!/usr/bin/guile \
-e main -s 
!#
(use-modules (ice-9 format)
             (ice-9 getopt-long))

(define *data-population-size* 10000)
(define *query-population-size* 1000)

;; init random with the time of day
(let ((time (gettimeofday)))
 (set! *random-state*
  (seed->random-state (+ (car time)
		       (cdr time)))))

; generate a die that will re-roll if the max value is rolled
(define (make-exploding-die n)
  (lambda ()
    (letrec ((roll (lambda ()
      (let ((v (+ 1 (random n))))
        (if (not (= v n))
          v
          (+ v (roll)))))))
      (roll))))

; the standard die types
(define d4 (make-exploding-die 4))
(define d6 (make-exploding-die 6))
(define d8 (make-exploding-die 8))
(define d10 (make-exploding-die 10))
(define d12 (make-exploding-die 12))

; make a test using a die of type 'n' and the wild-card die
(define (wc-test n)
  (let ((wild (d6)))
    (case n
      ((4) (max wild (d4)))
      ((6) (max wild (d6)))
      ((8) (max wild (d8)))
      ((10) (max wild (d10)))
      ((12) (max wild (d12)))
      (else (throw 'nonstandard-polyhedral "I don't understand that die type")))))
       
(define (round-to-neareast-integer i)
  (cond
    ((integer? i) 
      i)
    ((rational? i) 
      (inexact->exact (round (exact->inexact i))))))
  
(define (extract-staticstics vec)
 ;; expects a vector input, returns a hash-table of stats
  (let ((len (vector-length vec))
        (my-vec (sort vec (lambda (a b) (< a b))))
        (ht (make-hash-table)))
    (let ((i25 (round-to-neareast-integer (/ len 4)))
          (i50 (round-to-neareast-integer (/ len 2)))
          (i75 (round-to-neareast-integer (* 3 (/ len 4)))))
      (hash-set! ht 'i25 (vector-ref my-vec i25))
      (hash-set! ht 'i50 (vector-ref my-vec i50))
      (hash-set! ht 'i75 (vector-ref my-vec i75)))
     ht))

(define (simulate-rolls die population-size)
  (let ((vec (make-vector population-size 0)))
    (begin
      (do ((i 0 (+ 1 i)))
          ((>= i population-size))
         (vector-set! vec i (wc-test die)))
      (extract-staticstics vec))))

(define (dump-data)
  (let ((dice '(4 6 8 10 12)))
    (let ((do-sim  (lambda (die)
      (let* ((ht  (simulate-rolls die 100000))
             (v25 (hash-ref ht 'i25))
             (v50 (hash-ref ht 'i50))
             (v75 (hash-ref ht 'i75)))
         (format #t "d~d, ~2d, ~2d, ~2d~%" die v25 v50 v75)))))
      (for-each do-sim dice))))

(define (query die)
  (let* ((ht (simulate-rolls die 10000))
         (v25 (hash-ref ht 'i25))
         (v50 (hash-ref ht 'i50))
         (v75 (hash-ref ht 'i75)))
    (format #t "d~d => 25th%: ~2d | 50th%: ~2d | 75%: ~2d ~%" die v25 v50 v75)))

(define (show-help)
  (format #t "~&\
  Util to simulate die roll mechanic of the Savage Worlds gaming system

  Will print the 25th percentile, 50th (median), and the 75th percentile 
  value of a large set of simulated die rolls

  All die rolls 'explode' on max value and include the extra d6 'wild card' die

  USAGE: savageworlds.scm [<options>] [<die>]

  Options: 
    -d, --data - dump out 25th, 50th (median), and 75th percentile  
                 values for all die types in a CSV format.  
                 Any further arguments are ignored

    -h, --help - print this usage information

  Arguments:
    die   - type of dice to roll (4, 6, 8, 10, 12)

  Notes:
     Currently uses a population of ~e rolls for simple querries and
     ~e rolls for generating data suitable for plotting.~%" *query-population-size* *data-population-size*))

(define (main argv)
  (let* ((option-spec '((data (single-char #\d) (value #f))
                        (help (single-char #\h) (value #f))))
         (options (getopt-long argv option-spec))
         (data-wanted (option-ref options 'data #f))
         (help-wanted (option-ref options 'help #f)))
    (cond 
      (help-wanted
        (show-help))
      (data-wanted
        (dump-data))
      (else
        (if (< (length argv) 2)
            (show-help)
            (let* ((die (string->number (cadr argv))))
              (query die)))))))
          

