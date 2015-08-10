#!/usr/bin/env racket
#lang racket
(require "helpers.rkt" "vm.rkt")

;; config
(define +size+ 2000)

;; off we go!
(define (read-and-run filename)
  (if (file-exists? filename)
      (run (file->string filename))
      (raise (string-append filename ": no such file or directory"))))

(define (run instructions)
  (define vm (new vm% [size +size+] [instructions instructions]))
  (define instructions-count (string-length instructions))
  ; TODO I *really* need to find a way to "update" an immutable vector
  ;  (copy with set in the process...)
  (while (not (send vm done?))
    (match (send vm cur-instruction)
      [#\> (send vm move-dp +1)                  (send vm next)]
      [#\< (send vm move-dp -1)                  (send vm next)]
      [#\+ (send vm mutate-cur! (lambda (n) (+ n 1))) (send vm next)]
      [#\- (send vm mutate-cur! (lambda (n) (- n 1))) (send vm next)]
      [#\,
       ; TODO flush input port (to remove the #\newline)
       (send vm set-cur! (read-byte))
       (send vm next)]
      [#\. (begin
             (display-and-flush (string (integer->char  (send vm cur))))
             (send vm next))]
      [#\[ (when (= (send vm cur) 0)
             (send vm go-to +1 #\] #:opposite #\[))
           (send vm next)] ; skip anyway
      [#\] (if (not (= (send vm cur) 0))
               (send vm go-to -1 #\[ #:opposite #\])
               (send vm next))]
      [else (send vm next)])))

;; cli
(command-line
  #:program "Brainket"
  #:args (filename)

  (read-and-run filename))
