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

(struct automaton-premise (state input)
  #:transparent)

; non-deterministic
(define automaton%
  (class object%
    (super-new)
    (init-field [states '()]
                [alphabet '()]
                [rules '()]
                [initial #f]
                [final #f])
    (field [current-states (list initial)])
    (define/public (reset) (set! current-states (list initial)))

    (define/public (get-current-states) current-states)

    (define/public (in-current-states q) (index-of current-states q))

    (define/public (is-accepting) (in-current-states final))
    
    (define/public (in-alphabet x)
      (index-of alphabet x))

    (define/public (transition r)
      (if (in-current-states (automaton-premise-state (rule-premise r)))
          (rule-conclusion r)
          '()
          )
      )
    
    (define/public (process x)
      (if (in-alphabet x)
          ; process all rules for which premise state is in current states and symbol is the current symbol
          ; BFS, essentially
          (set! current-states
                ; get all states the current state and input yield
                (flatten (map (lambda (r) (transition r))
                              ; all rules that have current symbol as input
                              (filter (lambda (r) (equal? x (automaton-premise-input (rule-premise r)))) rules)
                              )
                         )
                )
          #f
          )
      )
    )
  )

(define pushdown-automaton%
  (class automaton%
    (super-new)
    (init-field stack-alphabet)
    (field [stack (new stack%)])
    )
  )