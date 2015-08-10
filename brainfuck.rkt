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

    (define instructions-length (string-length instructions))
    (define memory (make-vector size))
    (define ip 0)
    (define dp 0)

    (define/public (cur)
      (vector-ref memory dp))

    (define/public (cur-instruction)
      (string-ref instructions ip))

    (define/public (done?)
      (>= ip instructions-length))

    (define/public (set-cur! value)
      (vector-set! memory dp value))

    (define/public (mutate-cur! fn)
      (vector-mutate! memory dp fn))

    (define/public (move-ip n) (set! ip (+ ip n)))

    (define/public (next) (move-ip 1))

    (define/public (prev) (move-ip -1))

    (define/public (move-dp n) (set! dp (+ dp n)))

    (define/public (go-to ip-mod char #:opposite [opposite #f])
      (define char-stack 0)
      (let loop ()
        (move-ip ip-mod)
        (unless (done?)
          (if (eq? char (cur-instruction))
              (when (> 0 char-stack)
                (set! char-stack (- 1 char-stack))
                (loop))
              (begin
                (when (eq? opposite (cur-instruction))
                  (set! char-stack (+ 1 char-stack)))
                (loop))))))

    (define-getter ip dp)))

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
