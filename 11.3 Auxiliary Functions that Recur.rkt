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
; for sort> with check-satisfied.

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
; Hint Formulate a function that compares two elements of GamePlayer.

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

;;; Exercise 188. Design a program that sorts lists of emails by date:
(define-struct email [from date message])
; An Email Message is a structure:
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time

;;; Also develop a program that sorts lists of email messages by name. To
; compare two strings alphabetically, use the string<? primitive.

(define e1 (make-email "Aaa" 300 "The last message"))
(define e2 (make-email "Ccc" 200 "The middle message"))
(define e3 (make-email "Bbb" 100 "The first message"))

(check-expect (sort-email> (list e3 e2 e1))
              (list e1 e2 e3))

(define (sort-email> l)
  (cond
    [(empty? l) '()]
    [else
     (insert-email (first l) (sort-email> (rest l)))]))

(define (insert-email e sl)
  (cond
    [(empty? sl) (list e)]
    [else
     (if (email> e (first sl))
         (cons e sl)
         (cons (first sl) (insert-email e (rest sl))))]))

(define (email> e f)
  (> (email-date e) (email-date f)))

;; Part 2: sort by name

(check-expect (sort-email-by-name< (list e3 e2 e1))
              (list e1 e3 e2))

(define (sort-email-by-name< l)
  (cond
    [(empty? l) '()]
    [else
     (insert-email-by-name< (first l) (sort-email-by-name< (rest l)))]))

(define (insert-email-by-name< e sl)
  (cond
    [(empty? sl) (list e)]
    [else
     (if (email-name< e (first sl))
         (cons e sl)
         (cons (first sl) (insert-email-by-name< e (rest sl))))]))

(define (email-name< e f)
  (string<? (email-from e) (email-from f)))

;;; Exercise 189. Here is the function search:
; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))
;;; It determines whether some number occurs in a list of numbers. The function
; may have to traverse the entire list to find out that the number of interest
; isn’t contained in the list.

;;; Develop the function search-sorted, which determines whether a number occurs
; in a sorted list of numbers. The function must take advantage of the fact that
; the list is sorted.

; Number List-of-numbers -> Boolean
; is n in the sorted list alon?
(check-expect (search-sorted 3 (list 7 2 1)) #false)
(check-expect (search-sorted 3 (list 7 3 1)) #true)
(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [else
     (cond
       [(= n (first alon)) #true]
       [(> n (first alon)) #false]
       [else (search-sorted n (rest alon))])]))

;;; Exercise 190. Design the prefixes function, which consumes a list of
; 1Strings and produces the list of all prefixes. A list p is a prefix of l if p
; and l are the same up through all items in p. For example, (list "a" "b" "c") is
; a prefix of itself and (list "a" "b" "c" "d").

; Lo1s -> List-of-Lo1s
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a" "b"))
              (list (list "a")
                    (list "a" "b")))
(check-expect (prefixes (list "a" "b" "c"))
              (list (list "a")
                    (list "a" "b")
                    (list "a" "b" "c")))

(define (prefixes l)
  (cond
    [(empty? l) '()]
    [else
     (cons (list (first l))
           (mapcons (first l) (prefixes (rest l))))]))

; 1String List-of-Lo1s -> List-of-Lo1s
; add s to the front of every list
(check-expect (mapcons "a" (list (list "b") (list "b" "c")))
              (list (list "a" "b")
                    (list "a" "b" "c")))
(define (mapcons s lls)
  (cond
    [(empty? lls) '()]
    [else
     (cons (cons s (first lls))
           (mapcons s (rest lls)))]))
  
;; Part 2
;;; Design the function suffixes, which consumes a list of 1Strings and produces
; all suffixes. A list s is a suffix of l if p and l are the same from the end, up
; through all items in s. For example, (list "b" "c" "d") is a suffix of itself
; and (list "a" "b" "c" "d").

; Lo1s -> List-of-Lo1s
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a" "b"))
              (list (list "b")
                    (list "a" "b")))
(check-expect (suffixes (list "a" "b" "c"))
              (list (list "c")
                    (list "b" "c")
                    (list "a" "b" "c")))
(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else
     ; (first l)
     ; (suffixes (rest l))
     ; - a
     ; - (suffixes (list b c)) -> (list (list c) (list b c))
     ; -> (list (list c) (list b c) (list a b c))
     '()]))
