#!/usr/bin/env racket
#lang racket
;; config
(define +size+ 2000)

(define-syntax-rule (while cond body ...)
  (let loop ()
    (when cond
      body ...
      (loop))))

(define (compile-and-run filename)
  (unless (file-exists? filename)
    (raise (string-append "Cannot evaluate " filename ": no such file or directory")))
  (define instructions (file->string filename))
  (define instructions-count (string-length instructions))
  (let run ([memory (make-vector +size+)]
            [ip 0]
            ; TODO jump stack
            )
    (when (not (= ip instructions-count))
      (match (string-ref instructions ip)
        [#\< (display "hey")]
        [else (run memory (+ ip 1))]))))

;; cli
(command-line
  #:program "Brainket"
  #:args (filename)

  (compile-and-run filename))
