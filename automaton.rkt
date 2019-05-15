#lang racket
(require racket/class)
(require "base.rkt")

; for our purposes, empty stack returns the "epsilon" empty symbol
(define stack%
  (class object%
    (super-new)
    (init-field [contents '()])
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

(struct pushdown-automaton-state (state stack) #:transparent)

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
                (flatten (map (λ (r) (transition r))
                              ; all rules that have current symbol as input
                              (filter (λ (r) (equal? x (automaton-premise-input (rule-premise r)))) rules)
                              )
                         )
                )
          #f
          )
      )

    ; string as a list of symbols
    (define/public (process-string s)
      ; process all symbols
      (map (λ (x) (process-symbol x)) s)
      (is-accepting)
      )
    )
  )

(define pushdown-automaton%
  (class automaton%

    (super-new)
    (inherit-field initial)
    (init-field initial-stack)
    (inherit-field current-states)
    (define/override (reset)
      (set! current-states (list (pushdown-automaton-state initial (new stack%))))
      (send (pushdown-automaton-state-stack (car current-states)) push initial-stack)
      )
    (reset)
    
    (init-field {stack-alphabet '()})

    (inherit-field rules)
    (inherit-field final)
    (inherit in-alphabet)
    (inherit process-string)
    
    ; PDA is accepting iff it's in an accepting state and its stack is empty
    (define/override (is-accepting)
      (not (empty?
       (filter (λ (st) (send (pushdown-automaton-state-stack st) is-empty))
               (filter (λ (st) (equal? final (pushdown-automaton-state-state st))) current-states))
       ))
      )

    (define/public (get-applicable-current-states premise)
      (filter (λ (st) (equal? (pushdown-automaton-premise-top premise) (send (pushdown-automaton-state-stack st) peek)))
               (filter (λ (st) (equal? (automaton-premise-state premise) (pushdown-automaton-state-state st))) current-states)
       )
      )

    ; returns a conclusion reachable by rule r, if currently applicable, while adding to the stack
    (define/override (transition r)
      (map (λ (st) (
                         pushdown-automaton-state
                          (pushdown-automaton-conclusion-state (rule-conclusion r))
                          (new stack% [contents (cons (pushdown-automaton-conclusion-top (rule-conclusion r)) (cdr (get-field contents (pushdown-automaton-state-stack st))))]
                         )))
           (get-applicable-current-states (rule-premise r)))
      )
    
    ;(define/override (process-symbol x)
    ;  (if (in-alphabet x)
    ;      ; process all rules for which premise state is in current states and symbol is the current symbol
    ;      ; BFS, essentially
    ;      (set! current-states
    ;            ; get all states the current state and input yield
    ;            (flatten (map (λ (r) (transition r))
    ;                                  ; all rules that have current symbol as input
    ;                                  (filter (λ (r) (equal? x (automaton-premise-input (rule-premise r)))) rules))
    ;                          )
    ;                     )
    ;            )
    ;      #f
    ;      )
    ;  )
    
    )
  )