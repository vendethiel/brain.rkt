#!/usr/bin/env racket
#lang racket
(require "helpers.rkt")

;; config
(define +size+ 2000)

(define (read-and-run filename)
  (if (file-exists? filename)
      (run (file->string filename))
      (raise (string-append "Cannot evaluate " filename ": no such file or directory"))))

(define (run instructions)
  (define instructions-count (string-length instructions))
  ; TODO I *really* need to find a way to "update" an immutable vector (copy with set in the process...)
  (let run ([memory (make-vector +size+)]
            [ip 0]
            [dp 0]
            [jumps '()])
    ; TODO I definitely need to move this named let arguments into
    ;      a struct... (or a class)
    ;      (the functions can be free ones, or for a class, ... methods)
    (let* ([cur (lambda () (vector-ref memory dp))]
           [apply-cur (lambda (fn) (run (vector-apply memory dp fn)
                                    (+ ip 1) dp jumps))]
           [move-ip (lambda (n) (run memory (+ ip n) dp jumps))]
           [next (lambda () (move-ip 1))]
           [move-dp (lambda (n) (run memory (+ ip 1) (+ dp n) jumps))])
      (when (not (= ip instructions-count))
        (match (string-ref instructions ip)
          [#\> (move-dp +1)]
          [#\< (move-dp -1)]
          [#\+ (apply-cur (curry + 1))]
          [#\- (apply-cur (curry - 1))]
          [#\. (begin
                 (display-and-flush (number->string (cur)))
                 (next))]
          [#\, (apply-cur (lambda _ (read-byte)))]
         ;[#\[ (if (= (cur) 0) forward-to-next-command )]
         ;[#\] ()]
          [else (next)])))))

;; cli
(command-line
  #:program "Brainket"
  #:args (filename)

  (read-and-run filename))
