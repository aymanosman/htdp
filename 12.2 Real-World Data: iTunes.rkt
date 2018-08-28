;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12.2 Real-World Data: iTunes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; 12.2 Real-World Data: iTunes

(require 2htdp/batch-io)
(require 2htdp/itunes)

(define-struct asd [foo bar])

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define (take n l)
  (cond
    [(empty? l) '()]
    [(= 0 n) '()]
    [else
     (cons (first l) (take (- n 1) (rest l)))]))

(take 2 itunes-tracks)

;;; Exercise 199. While the important data definitions are already
; provided, the first step of the design recipe is still incomplete.
; Make up examples of Dates, Tracks, and LTracks. These examples come in
; handy for the following exercises as inputs.

(define eg/date-1 (create-date 2018 8 28 13 58 34))
(define eg/track-1 (create-track
                    "Track Name" ; name
                    "Paul M"     ; artist
                    "Cool Album" ; album
                    2000         ; time -- 2 second long song!
                    3            ; track#
                    eg/date-1    ; added
                    20           ; play#
                    eg/date-1    ; played
                    ))

;;; Exercise 200. Design the function total-time, which consumes an
; element of LTracks and produces the total amount of play time. Once
; the program is done, compute the total play time of your iTunes
; collection.

; Track -> Number
(check-expect (total-time eg/track-1) (* (track-play# eg/track-1) (track-time eg/track-1)))
(define (total-time t)
  (* (track-play# t) (track-time t)))
