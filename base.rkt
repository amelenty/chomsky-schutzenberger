#lang racket
(require racket/class)

(provide (struct-out rule))

; premise and conclusion are both lists
(struct rule (premise conclusion)
  #:transparent)

; empty symbol
(define epsilon "")

(define cf-grammar%
  (class object%
    (super-new)
    (init-field [terminal-alphabet '()]
                [non-terminal-alphabet '()]
                [rules '()]
                [start epsilon])

    (define/public (get-start) start)

    (define/public (get-rules) rules)                

    (define/public (is-terminal x)
      (index-of terminal-alphabet x))

    (define/public (is-non-terminal x)
      (index-of non-terminal-alphabet x))
    )
  )

; terminal alphabet is n types of pairs/brackets: a list of pairs
(define dyck-grammar%
  (class cf-grammar%
    (init terminal-pairs)
    (define start "Start")                     
    (define non-terminal-alphabet (list start))
    (define rules
            ; S -> epsilon
      (cons (rule start (list epsilon))
            ; S -> a_i S b_i S for every pair of brackets
            (map (lambda (bracket-pair)
                   (rule start (list (first bracket-pair) start (last bracket-pair) start)))
                 terminal-pairs)
            )
      )
    (super-new
     [start start]
     [terminal-alphabet (flatten terminal-pairs)]
     [non-terminal-alphabet non-terminal-alphabet]
     [rules rules])
    (inherit is-terminal is-non-terminal)
    )
  )