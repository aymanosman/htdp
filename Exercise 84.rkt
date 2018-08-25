;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 84|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise 84 (5.10 A Graphical Editor)

; edit
; Editor KeyEvent -> Editor
;   (edit ed ke)
; 3 cases \b | \t,\r | rest
; interp. Add ke to ed.pre, if ke is letter,
;   remove if backspace, ignore tab and carriage return
(check-expect (edit (make-editor "hell" " world") "o") (make-editor "hello" " world"))
(check-expect (edit (make-editor "some" "thing") "\b") (make-editor "som" "thing"))
(check-expect (edit (make-editor "a" "b") "\t") (make-editor "a" "b"))
(define (edit ed ke)
  (cond
    [(string=? "\b" ke)
     (make-editor (string-remove-last (editor-pre ed)) (editor-post ed))]
    [(or (string=? "\t" ke) (string=? "\r" ke))
     ed]
    [else
     (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]))


;; from 83
(define-struct editor [pre post])
; from 38
(define (string-remove-last s)
  (if (string=? "" s)
      ""
      (substring s 0 (- (string-length s) 1))))
