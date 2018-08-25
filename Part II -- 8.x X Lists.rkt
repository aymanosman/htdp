;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Part II -- 8.x X Lists|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(check-expect
  (contains-flatt? (cons "X" (cons "Y"  (cons "Z" '()))))
  #false)
(check-expect
  (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

; Exercise 134.
; String -> ListOf String -> Boolean
; interpretation: Determine if the given string is in the given list of string.
(check-expect
  (contains? "Flatt" (cons "X" (cons "Y"  (cons "Z" '()))))
  #false)
(check-expect
  (contains? "Flatt" (cons "A" (cons "Flatt" (cons "C" '()))))
  #true)
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [(cons? l)
     (or (string=? s (first l))
         (contains? s (rest l)))]))
