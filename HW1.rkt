;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Date? anything --> Boolean
;; a Date is a (make-Date Number Number Number)
;; interp: a date where
;;    year is year
;;    month is month
;;    day is day
(define-struct Date (year month day))

(define date1 (make-Date 1998 2 1))
(define date2 (make-Date 1991 3 14))
(define date3 (make-Date 2010 12 21))

;; Film? anything --> Boolean
;; a Film is a (make-Film String String Number Date String)
;; interp: a film where
;;    genre is the type of the film
;;    rating is the score of the film
;;    runningTime is how long the film is in minutes
;;    openingDate is the exact date that the film open
;;    nominations is the association that nominated the film
(define-struct Film (genre rating runningTime openingDate nominations))

(define film1 (make-Film "genre1" "NR" 120 (make-Date 1998 2 1) "Academy Award"))
(define film2 (make-Film "genre2" "NC-17" 180 (make-Date 1991 3 14) "Academy Award"))
(define film3 (make-Film "genre3" "PG" 141 (make-Date 2010 12 21) "Academy Award"))


;; Film --> Boolean
;; consumes a film and returns true if it is a drama more than two and a half hours long
;;     or was both nominated for an Oscar and has a rating of NC-17 or NR.
(define (high-brow film)
  (if (or (and (equal? "drama" (Film-genre film))
               (< 120 (Film-runningTime film)))
          (and (equal? "Oscar" (Film-nominations film))
               (or (equal? "NC-17" (Film-rating film))
                   (equal? "NR" (Film-rating film))))) true false))
;---TEST---;
(define filmTest1 (make-Film "drama" "G" 140 (make-Date 1990 1 1) "None"))
(define filmTest2 (make-Film "drama" "G" 110 (make-Date 1990 1 1) "None"))
(define filmTest3 (make-Film "genre1" "NR" 100 (make-Date 1990 1 1) "Oscar"))
(define filmTest4 (make-Film "genre1" "NC-17" 110 (make-Date 1990 1 1) "None"))
(check-expect (high-brow filmTest1) #t)
(check-expect (high-brow filmTest2) #f)
(check-expect (high-brow filmTest3) #t)
(check-expect (high-brow filmTest4) #f)