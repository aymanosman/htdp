;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |11.3 Auxiliary Functions that Recur|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; 11.3 Auxiliary Functions that Recur

; List-of-numbers -> List-of-numbers
; rearranges alon in descending order

(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1)) (list 3 2 1))
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(check-expect (sort> (list 12 20 -5))
              (list 20 12 -5))

(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [else
     (insert (first alon) (sort> (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon

(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5))
              (list 20 12 -5))
(check-expect (insert 2 (list 3 2 1))
              (list 3 2 2 1))

(define (insert n alon)
  (cond
    [(empty? alon) (list n)]
    [(cons? alon)
     (if (>= n (first alon))
         (cons n alon)
         (cons (first alon) (insert n (rest alon))))]))

;;; Exercise 186. Use sorted>? from exercise 145 to reformulate the tests
;;; for sort> with check-satisfied.

;; Part 1
(check-satisfied (sort> (list 1 2 3)) sorted>?)

;; From exercise 145
; NEList-of-numbers -> Boolean
(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [else
     (and (>= (first l) (first (rest l)))
          (sorted>? (rest l)))]))

;; Part 2
; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))

(check-satisfied (cons (sort>/bad (list 1 2 3))
                       (list 1 2 3))
                 not-sorted>?)

; (cons List-of-numbers List-of-number) -> Boolean
; (cons sorted-list original-list)
(define (not-sorted>? stuff)
  (not
   (sorted-conditions (first stuff) (rest stuff))))

; List-of-numbers List-of-numbers -> Boolean
; applies a series of predicates to the two lists
(define (sorted-conditions sl l)
  (and
   (= (length l) (length sl)) ; same-length?
   #true; same-elements?
   ))


;;; Exercise 187. Design a program that sorts lists of game players by score:
;;; Hint Formulate a function that compares two elements of GamePlayer.

(define-struct gp [name score])
; A GamePlayer is a structure:
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points

(define gp3 (make-gp "gp3" 30))
(define gp2 (make-gp "gp2" 20))
(define gp1 (make-gp "gp1" 10))

; List-of-GamePlayer -> List-of-GamePlayer

(check-expect (sort-gp> (list gp1 gp3 gp2))
              (list gp3 gp2 gp1))
(define (sort-gp> l)
  (cond
    [(empty? l) '()]
    [else
     (insert-gp (first l) (sort-gp> (rest l)))]))

; GamePlayer List-of-GamePlayer -> List-of-GamePlayer
; insert p into sorted sl
(define (insert-gp p sl)
  (cond
    [(empty? sl) (list p)]
    [else
     (if (gp> p (first sl))
         (cons p sl)
         (cons (first sl) (insert-gp p (rest sl))))]))

; GamePlayer GamePlayer -> Boolean
; p's score is greater than q's
(define (gp> p q)
  (> (gp-score p) (gp-score q)))
