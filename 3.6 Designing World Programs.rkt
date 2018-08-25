;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3.6 Designing World Programs|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; 3.6 Designing World Programs

;; Auxilliary

(define (neg n) (- 0 n))

; Steps

; physics
(define WIDTH-OF-WORLD 200)
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define Y-CAR (* 3 WHEEL-RADIUS))

; graphics
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define XXXTREE 15)
(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 XXXTREE
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND
  (overlay/xy tree
              (neg (/ WIDTH-OF-WORLD 2))
              (neg XXXTREE)
              (empty-scene WIDTH-OF-WORLD 50)))
BACKGROUND
(define SPACE
  (rectangle (* 2 WHEEL-RADIUS) 0 "solid" "white"))

(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR-LENGTH
  (* 8 WHEEL-RADIUS))
(define HALF (/ CAR-LENGTH 2))
(define QUART (/ CAR-LENGTH 4))

(define ROOF-HEIGHT WHEEL-RADIUS)
(define ROOF
  (rectangle (/ CAR-LENGTH 2) ROOF-HEIGHT  "solid" "red"))
(define CHASSIS-HEIGHT (* 2 ROOF-HEIGHT))
(define CHASSIS
  (rectangle  CAR-LENGTH (* 2 ROOF-HEIGHT) "solid" "red"))

(define CAR-BODY
  (overlay/xy ROOF (- 0 QUART) ROOF-HEIGHT CHASSIS))

(define CAR
  (overlay/xy BOTH-WHEELS
              (- 0 (/ CAR-LENGTH 8))
              (neg CHASSIS-HEIGHT)
              CAR-BODY))

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car
 
; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
(define (render ws)
  (place-image CAR ws Y-CAR BACKGROUND))

(define (end? ws)
  (> ws WIDTH-OF-WORLD))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [stop-when end?]
     [to-draw render]))


