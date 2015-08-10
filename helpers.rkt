#lang racket
(require (for-syntax racket/syntax))

(provide while until once define-getter)

(provide/contract [vector-mutate!
                   (vector? integer? (any/c . -> . any/c) . -> . any/c)]
                  [display-and-flush
                   (() #:rest (listof string?) . ->* . any/c)])

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define-syntax-rule (until condition body ...)
  (while (not condition)
    body ...))

(define-syntax-rule (once body ...)
  (let ([const-result '()]
        [inited #f])
    (lambda ()
      (unless inited
        (set! inited #t)
        (set! const-result (begin body ...)))
      const-result)))

(define-syntax (define-getter stx)
  (syntax-case stx ()
    [(define-getter) #'(begin)]
    [(define-getter id0 id ...)
     (with-syntax ([getter-name
                    (format-id #'id0 #:source #'id0
                               "get-~a" (syntax-e #'id0))])
       #'(begin
           (define/public (getter-name)
             id0)
           (define-getter id ...)))]))

(define-syntax (define-setter stx)
  (syntax-case stx ()
    [(define-setter) #'(begin)]
    [(define-setter id0 id ...)
     (with-syntax ([setter-name
                    (format-id #'id0 #:source #'id0
                               "set-~a!" (syntax-e #'id0))])
       #'(begin
           (define/public (setter-name)
             id0)
           (define-setter id ...)))]))

(define-syntax-rule (define-getter-setter id ...)
  (begin
    (define-getter id ...)
    (define-setter id ...)))

(define (vector-mutate! vec idx fn)
  (vector-set! vec idx (fn (vector-ref vec idx)))
  vec)

(define (display-and-flush . strs)
  (display (apply string-append strs))
  (flush-output))
