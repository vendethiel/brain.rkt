#lang racket

(provide while once lazy-const display-and-flush)

(provide/contract [vector-apply (vector? integer? any/c . -> . any/c)])

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
