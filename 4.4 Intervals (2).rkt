;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4.4 Intervals (2)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; 4.4 Intervals (2)

; A WorldState falls into one of three intervals: 
; – between 0 and CLOSE
; – between CLOSE and HEIGHT
; – below HEIGHT
; interpretation number of pixels between the top and the UFO

(define (f ws)
  (cond
    [(<= 0 ws CLOSE) ...]
    [(<= CLOSE ws HEIGHT) ...]
    [(> ws HEIGHT) ...]))

; constants
(define WIDTH 300) ; distances in terms of pixels
(define HWIDTH (/ WIDTH 2))
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay (circle 10 "solid" "green") (rectangle 40 10 "solid" "blue")))

; functions
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render/ufo]))
 
; WorldState -> WorldState
; computes next location of UFO 
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; places UFO at given height into the center of MTSCN
(check-expect (render/ufo 11) (place-image UFO HWIDTH 11 MTSCN))
(define (render/ufo y)
  (place-image UFO HWIDTH y MTSCN))

(check-expect (render/status 90 (render/ufo 90))
              (place-image (text "closing in" 11 "red") 50 10 (render/ufo 90)))
(define (render/status y scn) ; y :: WorldState
  (cond
    [(<= 0 y CLOSE)
     (place-image (text "descending" 11 "blue") 50 10 (render/ufo y))]
    [(<= CLOSE y HEIGHT)
     (place-image (text "closing in" 11 "red") 50 10 (render/ufo y))]
    [(> y HEIGHT)
     (place-image (text "closing in" 11 "red") 50 10 (render/ufo y))]))

(render/status 90 (render/ufo 90))