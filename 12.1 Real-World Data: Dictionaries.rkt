;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.1 Real-World Data: Dictionaries|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; On OS X:
(define LOCATION "/usr/share/dict/words")
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; – "a"
; – ...
; – "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

;;; Exercise 195. Design the function starts-with#, which consumes a
; Letter and Dictionary and then counts how many words in the given
; Dictionary start with the given Letter. Once you know that your
; function works, determine how many words start with "e" in your
; computer’s dictionary and how many with "z".

; Letter Dictionary -> Number
; how many words start with l in d
(check-expect (starts-with# "a" (list "c" "cc")) 0)
(check-expect (starts-with# "a" (list "a" "ab" "c")) 2)
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else
     ; l                         : Letter
     ; (first d)                 : String
     ; first-letter              : String -> Letter
     ; (first-letter (first d))  : Letter
     ; (string=? l _)            : Boolean
     ; (starts-with# l (rest d)) : Number
     (+ (if (string=? l (first-letter (first d)))
            1
            0)
        (starts-with# l (rest d)))]))

; first-letter : String -> Letter
; first letter of w
(define (first-letter w)
  (substring w 0 1))

;; (starts-with# "e" AS-LIST) ; -> 7818
;; (starts-with# "z" AS-LIST) ; -> 719

;;; Exercise 196. Design count-by-letter. The function consumes a
; Dictionary and counts how often each letter is used as the first one
; of a word in the given dictionary. Its result is a list of
; Letter-Counts, a piece of data that combines letters and counts.

; Dictionary -> List-of-Letter-Counts
; how many words in d start with each letter in LETTERS
(define (count-by-letter d)
  (count-by-letter/with d LETTERS))

(check-expect (count-by-letter/with
               (list "a" "aa" "b" "c" "cc" "ccc") (list "a" "b" "c"))
              (list (list "a" 2) (list "b" 1) (list "c" 3)))

; Dictionary Letters -> List-of-Letter-Counts
(define (count-by-letter/with d ls)
  (cond
    [(empty? ls) '()]
    [else
     ; (first ls) : Letter
     ; (list (first ls) _) : Letter-Count
     ; (starts-with# (first ls) d) : Number
     ; (count-by-letter/with d (rest l)) : List-of-Letter-Counts
     (cons
      (list (first ls) (starts-with# (first ls) d))
      (count-by-letter/with d (rest ls)))]))

; (count-by-letter AS-LIST)

;;; Exercise 197. Design most-frequent. The function consumes a
; Dictionary. It produces the Letter-Count for the letter that occurs
; most often as the first one in the given Dictionary.
;
; What is the most frequently used letter in your computer’s
; dictionary and how often is it used?

;; Part 1: Design a function that selects the first from a sorted list
; of pairs.

; Dictionary -> Letter-Count
; most frequent Letter-Count in d
(check-expect (most-frequent/sort (list "a" "aa" "b" "c" "cc" "ccc"))
              (list "c" 3))

(define (most-frequent/sort d)
  (first (sort-by-frequency (count-by-letter d))))

; [NEList-of LC] -> [NEList-of LC] (LC = Letter-Count)
; sort by frequency
(check-expect (sort-by-frequency (list (list "a" 2) (list "b" 1) (list "c" 3)))
              (list (list "c" 3) (list "a" 2) (list "b" 1)))

(define (sort-by-frequency ls)
  (cond
    [(empty? (rest ls)) ls]
    [else
     (insert (first ls)
             (sort-by-frequency (rest ls)))]))

; LC List-of-LC -> List-of-LC
; insert lc into sorted list ls
(define (insert l ls)
  (cond
    [(empty? ls) (list l)]
    [else
     (if (lc> l (first ls))
         (cons l ls)
         (cons (first ls)
               (insert l (rest ls))))]))

; LC LC -> Boolean
(define (lc> a b)
  (>= (second a) (second b)))

;; Part 2: Design a function that picks the pair with the maximum
; count.

; Dictionary -> Letter-Count
; most frequent Letter-Count in d
(check-expect (most-frequent (list "a" "aa" "b" "c" "cc" "ccc"))
              (list "c" 3))

(define (most-frequent d)
  (max-pair (count-by-letter d)))

; max-pair : [NEList-of LC] -> LC
; select LC with highest frequency
(define (max-pair l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (if (lc> (first l) (max-pair (rest l)))
         (first l)
         (max-pair (rest l)))]))


;;; Exercise 198. Design words-by-first-letter. The function consumes
; a Dictionary and produces a list of Dictionary's, one per Letter.

; Dictionary -> [List-of Letter-and-Dictionary]
(define (words-by-first-letter d)
  (words-by-first-letter/with LETTERS d))

(check-expect (words-by-first-letter/with
               (list "a" "b" "c")
               (list "a" "aa" "b" "c" "cc" "ccc"))
              (list
               (list "a" (list "a" "aa"))
               (list "b" (list "b"))
               (list "c" (list "c" "cc" "ccc"))))

(define (words-by-first-letter/with ls d)
  (cond
    [(empty? ls) '()]
    [else
     (cons (list (first ls) (starts-with (first ls) d))
           (words-by-first-letter/with (rest ls) d))]))


; Letter Dictionary -> Dictionary
(check-expect (starts-with "c" (list "a" "aa" "b" "c" "cc" "ccc"))
              (list "c" "cc" "ccc"))
(define (starts-with l d)
  (cond
    [(empty? d) '()]
    [else
     (if (string=? l (first-letter (first d)))
         (cons (first d) (starts-with l (rest d)))
         (starts-with l (rest d)))]))

;; Part 2: Redesign most-frequent from exercise 197 using this new
; function. Call the new function most-frequent.v2. Once you have
; completed the design, ensure that the two functions compute the same
; result on your computer’s dictionary:

(define some-dict (list "a" "aa" "b" "c" "cc" "ccc"))

; Dictionary -> LC
(check-expect (most-frequent some-dict) (most-frequent.v2 some-dict))

(define (most-frequent.v2 d)
  (max-dict (words-by-first-letter d)))

; [NEList-of Letter-and-Dictionary] -> LC
; the item whose dictionary is the largest, returning the letter and count
(define (max-dict ld)
  (cond
    [(empty? (rest ld)) (to-lc (first ld))]
    [else
     (lc/max (to-lc (first ld)) (max-dict (rest ld)))]))

; LC LC -> LC
; larger Letter-Count
(define (lc/max a b) (if (lc> a b) a b))

; Letter-and-Dictionary -> Letter-Count
(define (to-lc ld)
  (list (first ld) (length (second ld))))

; NOTE: This is slow
;; (check-expect (most-frequent AS-LIST) (most-frequent.v2 AS-LIST))

