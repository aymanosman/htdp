;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |10.3 Lists in Lists, Files|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 10.3 Lists in Lists, Files
; Exercise 176: Matrices

; A Matrix is one of:
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
;  – '()
;  – (cons Number Row)

; Wish list: first* rest*

; Matrix -> List-of-numbers
; produces the first column of the matrix
(check-expect (first* (cons '() '())) '())
(check-expect (first* (list (list 1 2 3)
                            (list 4 5 6)
                            (list 7 8 9)))
              (list 1 4 7))

; Matrix -> List-of-numbers
(define (first* m)
  (cons-maybe (first-maybe (first m))
              (cond
                [(empty? (rest m)) '()]
                [(cons? (rest m)) (first* (rest m))])))

; Matrix -> Matrix
; matrix with first column removed
(check-expect (rest* (cons '() '())) (cons '() '()))
(check-expect (rest* (list (list 1 2 3)
                           (list 4 5 6)
                           (list 7 8 9)))
              (list (list 2 3)
                    (list 5 6)
                    (list 8 9)))

; Matrix -> Matrix
(define (rest* m)
  (cons (rest-or-empty (first m))
        (cond
          [(empty? (rest m)) '()]
          [(cons? (rest m)) (rest* (rest m))])))

; Row -> Row
(define (rest-or-empty r)
  (cond
    [(empty? r) '()]
    [(cons? r) (rest r)]))


; Transpose


; Matrix -> Matrix
; transposes the given matrix along the diagonal
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(check-expect (transpose mat1) tam1)

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))


;; TEMPLATES


;; ; Matrix -> Any
;; (define (for-Matrix m)
;;   (cond
;;     [(empty? (rest m))
;;      ... (for-Row (first m))
;;      ]
;;     [(cons? (rest m))
;;      ... (for-Row (first m))
;;      ... (for-Matrix (rest m))
;;      ]))

;; ; Row -> Any
;; (define (for-Row r)
;;   (cond
;;     [(empty? r) ...]
;;     [(cons? r)
;;      ... (first r) ; Number
;;      ... (for-Row r)
;;      ]))


;; Auxilliary


(define (cons-maybe x l)
  (if (false? x) l (cons x l)))

(define (first-maybe r)
  (cond
    [(empty? r) #false]
    [(cons? r) (first r)]))


;; Old


;; (define (first*.v1 m)
;;   (cond
;;     [(empty? (rest m))
;;      (cond
;;        [(empty? (first m)) '()]
;;        [(cons? (first m))
;;         (cons (first (first m)) '())])]
;;     [(cons? (rest m))
;;      (cond
;;        [(empty? (first m)) '()]
;;        [(cons? (first m))
;;         (cons (first (first m))
;;               (first* (rest m)))])]))

;; ; Matrix -> List-of-numbers
;; (define (first*.v2 m)
;;   (cond
;;     [(empty? (rest m))
;;      (cons-maybe (first-maybe (first m)) '())]
;;     [(cons? (rest m))
;;      (cons-maybe (first-maybe (first m)) (first* (rest m)))]))
