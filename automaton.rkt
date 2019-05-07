#lang racket
(require racket/class)
(require "base.rkt")

; for our purposes, empty stack returns the "epsilon" empty symbol
(define stack%
  (class object%
    (super-new)
    (field [contents '()])
    ; only push non-epsilon
    (define/public (push x) (
                             if (equal? x epsilon)
                                #f
                                (set! contents (cons x contents))
                                )
      )
    (define/public (peek)
      (if (is-empty)
          epsilon
          (car contents))
      )
    (define/public (pop)
      (if (is-empty)
          epsilon
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

(struct pushdown-automaton-premise automaton-premise (top))

(struct pushdown-automaton-conclusion (state top) #:transparent)

; non-deterministic
(define automaton%
  (class object%
    (super-new)
    (init-field [states '()]
                [alphabet '()]
                [rules '()]
                [initial #f]
                [final #f]
                )
    (init-field [current-states (list initial)])
    (define/public (reset) (set! current-states (list initial)))

    (define/public (get-current-states) current-states)

    (define/public (in-current-states q) (index-of current-states q))

    (define/public (is-accepting) (in-current-states final))
    
    (define/public (in-alphabet x)
      (index-of alphabet x))

    ; returns a conclusion reachable by rule r, if currently applicable
    (define/public (transition r)
      (if (in-current-states (automaton-premise-state (rule-premise r)))
          (rule-conclusion r)
          '()
          )
      )
    
    (define/public (process-symbol x)
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

    ; string as a list of symbols
    (define/public (process-string s)
      ; process all symbols
      (map (lambda (x) (process-symbol x)) s)
      (is-accepting)
      )
    )
  )

(define pushdown-automaton%
  (class automaton%

    (super-new)
    (inherit-field initial)
    (inherit-field current-states)
    (define/override (reset) (set! current-states (list (pushdown-automaton-conclusion initial epsilon))))
    (reset)
    
    (init-field {stack-alphabet '()})
    (field [stack (new stack%)])

    (inherit-field rules)
    (inherit in-alphabet)
    (inherit transition)
    (inherit process-string)

    (define/public (get-stack) stack)
    
    ; PDA is accepting iff it's in an accepting state and its stack is empty
    (define/override (is-accepting)
      (and (super is-accepting) (send stack is-empty)))

    (define/override (in-current-states q)
      (index-of
       (map (lambda (st) (pushdown-automaton-conclusion-state st)) current-states)
       q)
      )

    (define/override (process-symbol x)
      (if (in-alphabet x)
          ; process all rules for which premise state is in current states and symbol is the current symbol
          ; BFS, essentially
          (set! current-states
                ; get all states the current state and input yield
                (flatten (map (lambda (r) (transition r))
                              ; all rules that have current stack top
                              (filter (lambda (r) (equal? (pushdown-automaton-premise-top (rule-premise r))(send stack peek)))
                                      ; all rules that have current symbol as input
                                      (filter (lambda (r) (equal? x (automaton-premise-input (rule-premise r)))) rules))
                              )
                         )
                )
          #f
          )
      )
    
    )
  )