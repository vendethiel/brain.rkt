#!/usr/bin/env racket
#lang racket
(require "helpers.rkt")
(require macro-debugger/expand)

;; config
(define +size+ 2000)

;; off we go!
(define (read-and-run filename)
  (if (file-exists? filename)
      (run (file->string filename))
      (raise (string-append filename ": no such file or directory"))))

(define vm%
  (class object%
    (init-field size instructions)

    (super-new)

    (define memory (make-vector size))
    (define ip 0)
    (define dp 0)
    (define jumps '())

    (define/public (cur)
      (vector-ref memory dp))

    (define/public (cur-instruction)
      (string-ref instructions ip))

    (define/public (set-cur! value)
      (vector-set! memory dp value))

    (define/public (mutate-cur! fn)
      (vector-mutate! memory dp fn))

    (define/public (move-ip n)
      (set! ip (+ ip n)))

    (define/public (next)
      (move-ip 1))

    (define/public (move-dp n)
      (set! dp (+ dp n)))

    (define-getter ip dp)))

(define (run instructions)
  (define vm (new vm% [size +size+] [instructions instructions]))
  (define instructions-count (string-length instructions))
  ; TODO I *really* need to find a way to "update" an immutable vector
  ;  (copy with set in the process...)
  (while (not (= (send vm get-ip) instructions-count))
    (match (send vm cur-instruction)
      [#\> (send vm move-dp +1)              (send vm next)]
      [#\< (send vm move-dp -1)              (send vm next)]
      [#\+ (send vm mutate-cur! (curry + 1)) (send vm next)]
      [#\- (send vm mutate-cur! (curry - 1)) (send vm next)]
      [#\, (send vm set-cur! (read-byte))    (send vm next)]
      [#\. (begin
             (display-and-flush (number->string (send vm cur)))
             (send vm next))]
      ;[#\[ (if (= (cur) 0) forward-to-next-command )]
      ;[#\] ()]
      [else (send vm next)])))

;; cli
(command-line
  #:program "Brainket"
  #:args (filename)

  (read-and-run filename))
