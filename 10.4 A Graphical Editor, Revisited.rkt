;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10.4 A Graphical Editor, Revisited|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; 10.4 A Graphical Editor, Revisited

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

; Lo1s -> Lo1s
; produces a reverse version of the given list
(check-expect (rev '()) '())
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(check-expect (add-at-end '() 2) (cons 2 '()))
(check-expect
  (add-at-end (cons "c" (cons "b" '())) "a")
  (cons "c" (cons "b" (cons "a" '()))))

(define (add-at-end l x)
  (cond
    [(empty? l) (cons x '())]
    [else (cons (first l) (add-at-end (rest l) x))]))


;;; Exercise 177. Design the function create-editor.
;;; The function consumes two strings and produces an Editor.
;;; The first string is the text to the left of the cursor
;;; and the second string is the text to the right of the cursor.
;;; The rest of the section relies on this function.

; String String -> Editor
;   (create-editor s t)
; Create an Editor where s is the text to the left of the cursor
; and t is the text to the right of the cursor.
(check-expect (create-editor "" "") (make-editor '() '()))
(check-expect (create-editor "ab" "cd")
              (make-editor (cons "b" (cons "a" '()))
                           (cons "c" (cons "d" '()))))
(define (create-editor s t)
  (make-editor (reverse (explode s)) (explode t)))

;;; END

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor

; If Editor is viewed as an interval, then we can distinguish three distinct ranges
; of values. An Editor with the cursor at the left extreme, right extreme and
; finally, and editor with the cursor surrounded by characters on both sides.
; This suggests three predicates:
; left? = (string=? "" pre)
; right? = (string=? "" post)
; surround? = ... this is just the else case.
; This is not quite right because 'right?' overlaps with 'left?' when editor
; is (create-editor "" "").

(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "" "fgh") "\b")
              (create-editor "" "fgh"))
(check-expect (editor-kh (create-editor "cd" "fgh") "\b")
              (create-editor "c" "fgh"))
(check-expect (editor-kh (create-editor "cd" "fgh") "left")
              (create-editor "c" "dfgh"))
(check-expect (editor-kh (create-editor "cd" "") "right")
              (create-editor "cd" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "right")
              (create-editor "cdf" "gh"))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; Editor 1String -> Editor
; insert the 1String k between pre and post
(check-expect
  (editor-ins (make-editor '() '()) "e")
  (make-editor (cons "e" '()) '()))

(check-expect
  (editor-ins
    (make-editor (cons "d" '())
                 (cons "f" (cons "g" '())))
    "e")
  (make-editor (cons "e" (cons "d" '()))
               (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

;;; Exercise 179. Design the functions editor-lft, editor-rgt and editor-del

; Editor -> Editor
; moves the cursor position one 1String left,
; if possible
(define (editor-lft ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
       (make-editor (rest (editor-pre ed))
                    (cons (first (editor-pre ed))
                          (editor-post ed)))]))

; Editor -> Editor
; moves the cursor position one 1String right,
; if possible
(define (editor-rgt ed)
  (cond
    [(empty? (editor-post ed)) ed]
    [else
       (make-editor (cons (first (editor-post ed))
                          (editor-pre ed))
                    (rest (editor-post ed)))]))

; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible
(define (editor-del ed)
  (cond
    [(empty? (editor-pre ed)) ed]
    [else
       (make-editor (rest (editor-pre ed))
                    (editor-post ed))]))

;;; END

(define rendered-pre-post
  (place-image/align
   (beside (text "pre" FONT-SIZE FONT-COLOR)
           CURSOR
           (text "post" FONT-SIZE FONT-COLOR))
   1 1
   "left" "top"
   MT))

(check-expect (editor-render (create-editor "pre" "post"))
              rendered-pre-post)

; Editor -> Image
(define (editor-render e)
  (place-image/align
    (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))

; Lo1s -> Image
; renders a list of 1Strings as a text image
(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

;;; Exercise 180. Design editor-text without using implode.
; Lo1s -> Image
(check-expect (editor-text.v2 (explode "hello")) (editor-text (explode "hello")))
(define (editor-text.v2 s)
  (cond
    [(empty? s) (empty-scene 0 0)]
    [(cons? s)
     (beside
      (text (first s) FONT-SIZE FONT-COLOR)
      (editor-text.v2 (rest s)))]))

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))
