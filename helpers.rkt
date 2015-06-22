#lang racket

(provide (all-defined-out))

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define-syntax-rule (once body ...)
  (let ([const-result '()]
        [inited #f])
    (lambda ()
      (unless inited
        (set! inited #t)
        (set! const-result (begin body ...)))
      const-result)))

(define-syntax-rule (lazy-const expr)
  (lambda _ expr))

(define (vector-apply vec idx fn)
  (vector-set! vec idx (fn (vector-ref vec idx)))
  vec)

(define (display-and-flush . strs)
  (display (apply string-append strs))
  (flush-output))
