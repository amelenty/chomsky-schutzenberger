#lang racket
(require racket/class)
(require "base.rkt")

(define stack%
  (class object%
    (super-new)
    (field [contents '()])
    (define/public (push x) (set! contents (cons x contents)))
    (define/public (peek)
      (if (is-empty)
          #f
          (car contents))
      )
    (define/public (pop)
      (if (is-empty)
          #f
          (let ((result (car contents)))
          (set! contents (cdr contents))
          result)
          )
      )
    (define/public (is-empty) (empty? contents))
    (define/public (get-contents) contents)
    )
  )
