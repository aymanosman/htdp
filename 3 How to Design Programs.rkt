;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3 How to Design Programs|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; 3.2 Finger Exercises: Functions

; Ex 34

; String -> 1String
; computes the first character of string s
(check-expect (string-first "hello") "h")
(define (string-first s)
  (string-ith s 0))


; Ex 35 design string-last

; String -> 1String
; last char of string s
(check-expect (string-last "hello") "o")
(define (string-last s)
  (string-ith s (- (string-length s) 1)))


