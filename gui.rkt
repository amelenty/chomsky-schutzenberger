#lang racket/gui

(define alphabet '())

(define main-frame
  (new frame%
       [label "Interactive Chomsky-Schutzenberger Theorem Proof"]
       [height 480]
       [width 640]))

(new button% [parent main-frame]
     [label "Update alphabet"]
     [callback (lambda (button event)
                 (set! alphabet '("Hello" "World"))
                 (println alphabet))])

(new canvas% [parent main-frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Don't Panic!" 0 0))])
 
;start
(send main-frame show #t)