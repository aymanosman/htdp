;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4.4 Intervals|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; 4.4 Intervals

; A WorldState is a Number.
; interpretation number of pixels between the top and the UFO
 
(define WIDTH 300) ; distances in terms of pixels 
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay (circle 10 "solid" "green")
                     (rectangle 40 10 "solid" "blue")))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render]))
 
; WorldState -> WorldState
; computes next location of UFO 
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; places UFO at given height into the center of MTSCN
(check-expect (render/ufo 11)
              (place-image UFO (/ WIDTH 2) 11 MTSCN))
(define (render y)
  (render/status y (render/ufo y)))

(define (render/ufo y) (place-image UFO (/ WIDTH 2) y MTSCN))

(check-expect (render/status 90 (render/ufo 90))
              (place-image (text "closing in" 11 "red")
                           50 10
                           (render/ufo 90)))
(define (render/status y scn)
  (place-image (text (status-text y) 11 (status-color y))
               50 10 scn))

(define (status-text y)
  (cond
    [(< y (* HEIGHT 2/3)) "descending"]
    [else "closing in"]))

(define (status-color y)
  (cond
    [(< y (* HEIGHT 2/3)) "blue"]
    [else "red"]))

(render 0)
(render 90)
