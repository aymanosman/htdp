;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 83|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Exercise 83 (5.10 A Graphical Editor)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

(define SCENE (empty-scene 200 20))

(define (text/render s t)
  (overlay/align "left" "center"
               (text (string-append s t) 16 "black")
               SCENE))

(define (add-cursor offset scene)
  (overlay/align/offset "left" "center"
                        CURSOR
                        (* -8 offset) 0
                        scene))

(define CURSOR (rectangle 1 20 "solid" "red"))

(define (render e)
  (add-cursor (string-length (editor-pre e))
              (text/render (editor-pre e) (editor-post e))))

(render (make-editor "h" ""))
(render (make-editor "he" ""))
(render (make-editor "hel" ""))
(render (make-editor "hell" ""))
(render (make-editor "hello" ""))
