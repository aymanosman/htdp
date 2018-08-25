;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2.5 Programs|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; 2.5 Programs
(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(define (number->square s)
  (square s "solid" "red"))

(define (reset s ke)
  100)

(define (const a b) a)

(define (me-h s x y kind)
  (const s (write-file 'stdout
                       (string-append
                        "x: "
                        (number->string x)
                        ", y: "
                        (number->string y)
                        ", kind: "
                        kind
                        "\n"
                        ))))

;(big-bang 100
;  [to-draw number->square]
;  [on-tick sub1]
;  [on-key reset]
;  [on-mouse me-h]
;  [stop-when zero?])

; Figure 14: A first interactive program

(define BACKGROUND (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))
 
(define (main y)
  (big-bang y
    [on-tick sub1]
    [stop-when zero?]
    [to-draw place-dot-at]
    [on-key stop]))
 
(define (place-dot-at y)
  (place-image DOT 50 y BACKGROUND))
 
(define (stop y ke)
  0)

