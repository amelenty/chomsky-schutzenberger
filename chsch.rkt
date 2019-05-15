#lang racket

(require "base.rkt")

(provide create-paired-alphabet)
(provide create-homomorphism)
(provide create-homomorphism-description)

(define (create-paired-alphabet alphabet)
  (append
   (map (λ (c) (list c (string-append c "'"))) alphabet)
   '(("[" "]") ("(" ")"))
   ))

(define (create-homomorphism alphabet)
  (λ (c)
    (if (index-of alphabet c)
    c
    epsilon)))

(define (create-homomorphism-description paired-alphabet homomorphism)
  (flatten
   (map
    (λ (c)
      (string-append c " → " (homomorphism c)))
    (flatten paired-alphabet))))