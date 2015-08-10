#lang racket
(require "helpers.rkt")

(provide vm%)

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

    (define/public (set-cur! value)
      (vector-set! memory dp value))

    (define/public (mutate-cur! fn)
      (vector-mutate! memory dp fn))

    (define/public (move-dp n) (set! dp (+ dp n)))

    (define/public (cur-instruction)
      (string-ref instructions ip))

    (define/public (done?)
      (>= ip instructions-length))

    (define/public (move-ip n) (set! ip (+ ip n)))

    (define/public (next) (move-ip 1))

    (define/public (prev) (move-ip -1))

    (define/public (go-to ip-mod char #:opposite [opposite #f])
      (define char-stack 0)
      (let loop ()
        (move-ip ip-mod)
        (unless (done?)
          (if (eq? char (cur-instruction))
              (when (> char-stack 0)
                (set! char-stack (- 1 char-stack))
                (loop))
              (begin
                (when (eq? opposite (cur-instruction))
                  (set! char-stack (+ 1 char-stack)))
                (loop))))))

    (define-getter ip dp)))
