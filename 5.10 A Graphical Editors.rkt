;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5.10 A Graphical Editors|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;; 5.10 A Graphical Editor

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

;; TEMPLATE
;; Editor -> Any
;(define (for-Editor ed)
;  ... (editor-pre ed)
;  ... (editor-post ed))

;;; Exercise 83. Design the function render,
;;; which consumes an Editor and produces an image.

; graphical constants
(define SCENE (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))

; Editor -> Image
; Render the text within an empty scene of 200 x 20 pixels.
; For the cursor, usa a 1 x 20 red rectangle and for the strings,
; black text of size 16.
(define (render e)
  (overlay/align "left" "center"
                 (beside (text/i (editor-pre e)) CURSOR (text/i (editor-post e)))
                 SCENE))

(define (text/i s) (text s 16 "black"))

(render (make-editor "hello" "world"))
(render (make-editor "hello " "world"))

;;; Exercise 84. Design edit. The function consumes two inputs,
;;; an editor ed and a KeyEvent ke, and it produces another editor.
;;; Its task is to add a single-character KeyEvent ke
;;; to the end of the pre field of ed, unless ke denotes the backspace ("\b") key.
;;; In that case, it deletes the character immediately to the left of the cursor
;;; (if there are any). The function ignores the tab key ("\t") and the return key ("\r").

;; TEMPLATE
;; WorldState KeyEvent -> ...
;(define (handle-key-events w ke)
;  (cond
;    [(= (string-length ke) 1) ...]
;    [(string=? "left" ke) ...]
;    [(string=? "right" ke) ...]
;    [(string=? "up" ke) ...]
;    [(string=? "down" ke) ...]
;    ...))

; Editor KeyEvent -> Editor
; Add a single-character ke to the end of pre.
; If ke is backspace \b, delete the last character of pre (if any).
; Ignore tab (\t) and return (\r).
; ke's "left" and "right" move the cursor left and right, respectively.
(check-expect (edit (make-editor "hello" "world") " ")
              (make-editor "hello " "world"))
(check-expect (edit (make-editor "hellos" " world") "\b")
              (make-editor "hello" " world"))
(define (edit ed ke)
  (cond
    [(or (string=? "\t" ke) (string=? "\r" ke)) ed]
    [(string=? "\b" ke) (editor-delete ed)]
    [(= 1 (string-length ke)) (editor-insert ke ed)]
    [(string=? "left" ke) (editor-cursor-left ed)] 
    [(string=? "right" ke) (editor-cursor-right ed)]
    [else ed]))

(define (string-remove-last s)
  (substring s 0 (- (string-length s) 1)))
(define (string-last s)
  (substring s (- (string-length s) 1) (string-length s)))
(define (string-first s)
  (cond
    [(string=? "" s) ""]
    [else (substring s 0 1)]))
(define (string-rest s)
  (substring s 1))

(define (editor-delete ed)
  (make-editor (string-remove-last (editor-pre ed)) (editor-post ed)))

(define (editor-insert ke ed)
  (make-editor (string-append (editor-pre ed) ke) (editor-post ed)))

; Editor -> Editor
(check-expect (editor-cursor-left (make-editor "" "world"))
              (make-editor "" "world"))
(check-expect (editor-cursor-left (make-editor "hello " "world"))
              (make-editor "hello" " world"))
(define (editor-cursor-left ed)
  (cond
    [(string=? "" (editor-pre ed)) ed]
    [else
     (make-editor (string-remove-last (editor-pre ed))
               (string-append (string-last (editor-pre ed)) (editor-post ed)))]))

; Editor -> Editor
(check-expect (editor-cursor-right (make-editor "hello" ""))
              (make-editor "hello" ""))
(check-expect (editor-cursor-right (make-editor "hello" " world"))
              (make-editor "hello " "world"))
(define (editor-cursor-right ed)
    (cond
    [(string=? "" (editor-post ed)) ed]
    [else
     (make-editor (string-append (editor-pre ed) (string-first (editor-post ed)))
                  (string-rest (editor-post ed)))]))



;;; Exercise 85. Define the function run.
;;; Given the pre field of an editor, it launches an interactive editor,
;;; using render and edit from the preceding two exercises for the to-draw
;;; and on-key clauses, respectively.
