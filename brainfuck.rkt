#!/usr/bin/env racket
#lang racket
;; config
(define +size+ 2000)

(define-syntax-rule (while cond body ...)
  (let loop ()
    (when cond
      body ...
      (loop))))

(define-syntax-rule (lazy-const body ...)
  (let ([const-result '()]
        [inited #f])
    (lambda ()
      (unless inited
        (set! inited #t)
        (set! const-result (begin body ...)))
      const-result)))

(define (vector-apply vec idx fn)
  (vector-set! (fn (vector-ref vec idx))))

(define (read-and-run filename)
  (unless (file-exists? filename)
    (raise (string-append "Cannot evaluate " filename ": no such file or directory")))
  (define instructions (file->string filename)))

(define (run instructions)
  (define instructions-count (string-length instructions))
  (let run ([memory (make-vector +size+)]
            [ip 0]
            [jumps '()])
    (let ([cur (lambda () (vector-ref memory ip))]
          [change-cur (lambda (fn) (run memory (vector-apply memory ip fn) jumps))]
          [move-ip (lambda (n) (run memory (+ ip n) jumps))])
      (display "hi")
      (when (not (= ip instructions-count))
        (match (string-ref instructions ip)
          [#\< (move-ip +1)]
          [#\> (move-ip -1)]
          [#\+ (change-cur (curry + 1))]
          [#\- (change-cur (curry - 1))]
          [#\. (display (cur))]
          [#\, (change-cur (lambda (_) (read-byte)))]
         ;[#\[ (if (= (cur) 0) forward-to-next-command )]
         ;[#\] ()]
          [else (run memory (+ ip 1) jumps)])))))

;; cli
(command-line
  #:program "Brainket"
  #:args (filename)

  (compile-and-run filename))
