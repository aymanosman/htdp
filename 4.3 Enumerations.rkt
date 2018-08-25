;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4.3 Enumerations|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; 4.3 Enumerations

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")

(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

(define (draw color)
  (circle 30 "solid" color))

(define (tock color)
  (traffic-light-next color))

; A Position is a Number.
; interpretation distance between the left margin and the ball 
 
; Position KeyEvent -> Position
; computes the next location of the ball 
 
(check-expect (keh 13 "left") 8)
(check-expect (keh 13 "right") 18)
(check-expect (keh 13 "a") 13)
(define (keh p k)
  (cond
    [(string=? "left" k)
     (- p 5)]
    [(string=? "right" k)
     (+ p 5)]
    [else p]))
(define BALL (circle 10 "solid" "blue"))

(define (draw-ball pos)
  (place-image BALL pos 50 (empty-scene 100 100)))

(big-bang 13
  [on-draw draw-ball]
  [on-key keh]
  )