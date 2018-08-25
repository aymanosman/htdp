;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5.5 Computing with Structures|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct entry [name phone email])

(make-entry "Al Abe" "666-7771" "lee@x.me")

(define-struct vel [deltax deltay])

(define (f v)
  (+ (vel-deltax v) (vel-deltay v)))

(define-struct centry [name home office cell])
(define-struct phone [area number])

(phone-area
 (centry-office
  (make-centry "Shriram Fisler"
    (make-phone 207 "363-2421")
    (make-phone 101 "776-1099")
    (make-phone 208 "112-9981"))))

; 
; (make-phone# Number Number Number)
; (make-phone# a s n)
; interpretation
; a xxx 000-999
; s xxx 000-999
; n xxxx 0000-9999
(define-struct phone# [area switch num])