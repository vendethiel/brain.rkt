#lang racket
(require rackunit rackunit/text-ui)
(require "helpers.rkt" "vm.rkt")

(define while-tests
  (test-suite
   "Tests for `while`"

   (test-case
    "it stops after the condition stops being true"
    (let ([i 0])
      (while (< i 2)
        (set! i (+ i 1)))
      (check = i 2)))

   (test-case
    "it never iterates if the condition is false"
    (let ([i 0])
      (while (< i 0)
        (set! i (+ i 1)))
      (check = i 0)))))

(define until-tests
  (test-suite
   "Tests for `until`"

   (test-case
    "it stops after the condition stops being false"
    (let ([i 0])
      (until (= i 2)
        (set! i (+ i 1)))
      (check = i 2)))

   (test-case
    "it never iterates if the condition is true"
    (let ([i 0])
      (until (= i 0)
        (set! i (+ i 1)))
      (check = i 0)))))

(define once-tests
  (test-suite
   "Tests for `once`"

   (test-case
    "it (always) gives the correct value"
    (define x (once 1))
    (check = (x) 1)
    (check = (x) 1))

   (test-case
    "it only calls the function once"
    (let [(i 0)]
      (define x (once (begin
                        (set! i (+ i 1))
                        i)))
      (check = (x) 1)
      (check = (x) 1)
      (check = i 1)))))

; TODO "let" this somehow
(define getter-testclass%
  (class object%
    (init)
    (super-new)

    (define i 0)

    (define-getter i)))

(define define-getter-tests
  (test-suite
   "Tests for `define-getter`"

   (test-case
    "it generates a getter"
    (let ([inst (new getter-testclass%)])
      (check = (send inst get-i) 0)))))

(define vector-mutate!-tests
  (test-suite
   "Tests for `vector-mutate!`"

   (test-case
    "it mutates the field correctly"
    (let ([vector (make-vector 100)] [idx 0])
      (check = 0 (vector-ref vector idx))
      (vector-mutate! vector idx (lambda (n) (+ n 5)))
      (check = 5 (vector-ref vector idx))
      (vector-mutate! vector idx (lambda (n) (* n 5)))
      (check = 25 (vector-ref vector idx))))))

(define (make-vm [instructions "."])
  (new vm% [size 2000] [instructions instructions]))
(define vm%-tests
  (test-suite
   "Tests for `vm%"

   (test-case
    "it starts off clean"
    (let ([vm (make-vm)])
      (check = 0 (send vm get-ip))
      (check = 0 (send vm get-dp))))

   (test-case
    "it gets/sets/mutates the value"
    (let ([vm (make-vm)])
      (check = 0 (send vm cur))
      (send vm set-cur! 2)
      (check = 2 (send vm cur))
      (send vm mutate-cur! (lambda (n) (* n 3)))
      (check = 6 (send vm cur))))

   (test-case
    "it moves the ip forward/backward or arbitrarily"
    (let ([vm (make-vm)])
      (send vm next)
      (check = 1 (send vm get-ip))
      (send vm prev)
      (check = 0 (send vm get-ip))
      (send vm move-ip +3)
      (check = 3 (send vm get-ip))
      (send vm move-ip -2)
      (check = 1 (send vm get-ip))))

   (test-case
    "it answers to done once there's no instructions left"
    (let ([vm (make-vm)])
      (check eq? #f (send vm done?))
      (send vm next)
      (check eq? #t (send vm done?))))

   (test-case
    "it lets you move the dp"
    (let ([vm (make-vm)])
      (send vm move-dp +3)
      (check = 3 (send vm get-dp))
      (send vm move-dp -2)
      (check = 1 (send vm get-dp))))

   (test-suite
    "tests for `go-to`"

    (test-case
     "it stops without erroring"
     (let ([vm (make-vm ".")])
       (send vm go-to +1 #\[) ; (any char not there)
       (check eq? #t (send vm done?))))

    (test-case
     "it finds the correct char to go to (without nested occurences"
     (let ([vm (make-vm ".[]")])
       (send vm go-to +1 #\])
       (check = 2 (send vm get-ip))
       (send vm go-to -1 #\[)
       (check = 1 (send vm get-ip))))

    (test-case
     "it finds the correct char to go to (WITH nested occurences)"
     (let ([vm (make-vm ".[.[].]")])
       (send vm go-to +1 #\[)
       (check = 1 (send vm get-ip))
       (display-and-flush "GO!\n")
       (send vm go-to +1 #\] #:opposite #\[)
       (check = 6 (send vm get-ip)))))))

(display "# while\n")
(run-tests while-tests)
(display "# until\n")
(run-tests until-tests)
(display "# once\n")
(run-tests once-tests)
(display "# define-getter\n")
(run-tests define-getter-tests)
(display "# vector-mutate!\n")
(run-tests vector-mutate!-tests)
(display "# vm%\n")
(run-tests vm%-tests)
; TODO test display and flush...somehow
