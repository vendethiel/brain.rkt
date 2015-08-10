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

    (define/public (apply-cur fn)
      (vector-apply memory dp fn))

    (define/public (move-ip n)
      (set! ip (+ ip n)))

    (define/public (next)
      (move-ip 1))

    (define/public (move-dp n)
      (set! dp (+ dp n)))

    (define-getter ip dp)))

(define (run instructions)
  (define vm (new vm% [size +size+] [instructions instructions]))
  (display (send vm get-ip))
  (display (send vm get-dp)))
  ;(define instructions-count (string-length instructions))
  ; TODO I *really* need to find a way to "update" an immutable vector
  ;  (copy with set in the process...)
;(let run ([vm (new vm% [size +size+ instructions instructions])])
;    (when (not (= ip instructions-count))
;      (match (string-ref instructions ip)
;          [#\> (move-dp +1)]
;          [#\< (move-dp -1)]
;          [#\+ (apply-cur (curry + 1))]
;          [#\- (apply-cur (curry - 1))]
;          [#\. (begin
;                 (display-and-flush (number->string (cur)))
;                 (next))]
;          [#\, (apply-cur (lambda _ (read-byte)))]
;         ;[#\[ (if (= (cur) 0) forward-to-next-command )]
;         ;[#\] ()]
;          [else (next)]))))

;; cli
(command-line
  #:program "Brainket"
  #:args (filename)

  (read-and-run filename))
