#lang racket/gui

(require "chsch.rkt")

; user data
(define alphabet '())
(define (get-alphabet-string) (string-join alphabet))
(define paired-alphabet '())
(define (homomorphism) '())

; main GUI
(define main-frame
  (new frame%
       [label "Interactive Chomsky-Schutzenberger Theorem Proof"]
       [height 480]
       [width 640]))

(define tab-panel
  (new tab-panel% [parent main-frame]
       [choices '("Alphabet" "Dyck language" "Context-free language" "Regular language" "Run")]
       [callback
        (λ (tp event)
          (case (send tp get-item-label (send tp get-selection))
            [("Alphabet")
             (send tp change-children
             (λ (children) (list alphabet-tab)))]
            [("Dyck language")
             (send tp change-children
             (λ (children) '()))]))]))
; alphabet tab
(define alphabet-tab
  (new vertical-panel% [parent tab-panel]))

(define alphabet-field
  (new text-field% [parent alphabet-tab]
       [label "Alphabet T"]
       [init-value (get-alphabet-string)]))

(new button% [parent alphabet-tab]
     [label "Update alphabet"]
     [callback (λ (button event)
                 (set! alphabet (string-split (send alphabet-field get-value)))
                 (println alphabet)
                 (update-paired-alphabet-message)
                 (update-homomorphism-message)
                 (send alphabet-next enable #t)
                 )])

(define paired-alphabet-message
  (new message% [parent alphabet-tab]
       [label "Alphabet T1:"]
       [auto-resize #t]))

(define (update-paired-alphabet-message)
  (set! paired-alphabet (create-paired-alphabet alphabet))
  (println paired-alphabet)
  (send paired-alphabet-message set-label
        (string-join (cons "Alphabet T1:" (flatten paired-alphabet)))))

(define homomorphism-message
  (new message% [parent alphabet-tab]
       [label "Homomorphism f:"]
       [auto-resize #t]))

(define (update-homomorphism-message)
  (set! homomorphism (create-homomorphism alphabet))
  (let ((description-list (create-homomorphism-description paired-alphabet homomorphism)))
    (println description-list)
    (send homomorphism-message set-label
          (string-join (cons "Homomorphism f:" description-list) "\n")))
  )

(define alphabet-next
  (new button% [parent alphabet-tab]
     [label "Next >"]
     [enabled #f]
     [callback (λ (button event)
                 (send tab-panel set-selection 1)
                 (send tab-panel change-children
             (λ (children) '())))])
)
;(new canvas% [parent alphabet-tab]
;             [paint-callback
;              (λ (canvas dc)
;                (send dc set-scale 3 3)
;                (send dc set-text-foreground "blue")
;                (send dc draw-text "Don't Panic!" 0 0))])
 
;start
(send main-frame show #t)