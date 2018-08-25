;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2. Functions and Programs|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define (ff x) (* 10 x))

;; 2.1 Functions
;; =============

; ex.11

; /x^2 + y^2
(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

(distance 3 4) ; expect 5

; ex.12

; cvolume cube-volume
(define (cvolume side)
  (* side side side))

(cvolume 3) ; expect 27

; Ex.13

(define (string-first s)
  (string-ith s 0))

(string-first "world") ; expect "w"

; ex 14 -- skip

; Ex 15

(define (==> sunny friday)
;  (if (or (boolean=? sunny #false) (boolean=? friday #true))
;      #true
;      #false)
  (if sunny
      friday
      #true))
      
(==> #true #false) ; expect false
(==> #false #true) ; expect true
(==> #true #true)  ; expect true

; ex-16,17 -- skip

; ex18

(define (string-join s t)
  (string-append s "_" t))

(string-join "" "") ; expect _
(string-join "" "a") ; expect _a

; Ex-19
; Assume [i] is a number between 0 and (length str)
; so for "" (0,0] (half-open interval)
(define (string-insert str i)
  (string-join (substring str 0 i) (substring str i)))

(string-insert "" 0)   ; expect _
(string-insert "a" 0)  ; expect _a
(string-insert "a" 1)  ; expect a_
(string-insert "ab" 1) ; expect a_b

; ex20 -- skip

;; 2.2 Computing
;; =============
