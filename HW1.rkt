;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1
;; Date? anything --> Boolean
;; a Date is a (make-Date Number Number Number)
;; interp: a date where
;;    year is year
;;    month is month
;;    day is day
(define-struct Date (year month day))

;; examples
(define date1 (make-Date 1998 2 1))
(define date2 (make-Date 1991 3 14))
(define date3 (make-Date 2010 12 21))

;; Film? anything --> Boolean
;; a Film is a (make-Film String String Number Date String)
;; interp: a film where
;;    title is the title of the film 
;;    genre is the type of the film
;;    rating is the score of the film
;;    runningTime is how long the film is in minutes
;;    openingDate is the exact date that the film open
;;    nominations is the number of Oscar nominations the film received
(define-struct Film (title genre rating runningTime openingDate nominations))

;; examples
(define film1 (make-Film "a" "genre1" "NR" 120 (make-Date 1998 2 1) 12))
(define film2 (make-Film "b" "genre2" "NC-17" 180 (make-Date 1991 3 14) 14))
(define film3 (make-Film "c" "genre3" "PG" 141 (make-Date 2010 12 21) 11))

;; 3
;; high-brow?: Film --> Boolean
;; consumes a film and returns true if it is a drama more than two and a half hours long or was both nominated for an Oscar and has a rating of NC-17 or NR.
(define (high-brow? film)
  (or (and (equal? "drama" (Film-genre film))
           (< 120 (Film-runningTime film)))
      (and (>= (Film-nominations film) 1)
           (or (equal? "NC-17" (Film-rating film))
               (equal? "NR" (Film-rating film))))))
;; test
(define filmTest1 (make-Film "a" "drama" "G" 140 (make-Date 1990 1 1) 0))
(define filmTest2 (make-Film "b" "drama" "G" 110 (make-Date 1990 1 1) 0))
(define filmTest3 (make-Film "c" "genre1" "NR" 100 (make-Date 1990 1 1) 1))
(define filmTest4 (make-Film "e" "genre1" "NC-17" 110 (make-Date 1990 1 1) 0))
(check-expect (high-brow? filmTest1) #t)
(check-expect (high-brow? filmTest2) #f)
(check-expect (high-brow? filmTest3) #t)
(check-expect (high-brow? filmTest4) #f)

;; 4
;; total-nominations: Film Film -> Number
;; consumes two films and produces the sum of the Oscar nominations for the two films
(define (total-nominations filmA filmB)
  (+ (Film-nominations filmA) (Film-nominations filmB)))

;; test
(check-expect (total-nominations film1 film2) 26)
(check-expect (total-nominations film2 film3) 25)
(check-expect (total-nominations film1 film3) 23)

;; 5
;; update-nominations: Film Number -> Film
;; consumes a film and a Number (representing the number of Oscar nominations), and produces a film. The film that is produced is the same as the original except that its nominations has been replaced by the given nominations. 
(define (update-nominations originalFilm nominations)
  (make-Film (Film-title originalFilm)
             (Film-genre originalFilm)
             (Film-rating originalFilm)
             (Film-runningTime originalFilm)
             (Film-openingDate originalFilm)
             nominations))
;; test
(define test1 (update-nominations film1 10))
(define test2 (update-nominations film2 22))
(define test3 (update-nominations film3 0))
(check-expect (Film-nominations test1) 10)
(check-expect (Film-nominations test2) 22)
(check-expect (Film-nominations test3) 0)

;; 6
;; define constants
(define dateA (make-Date 2000 11 11))
(define filmA (make-Film "a" "genre1" "NR" 120 (make-Date 2001 11 11) 0))
(define filmB (make-Film "a" "genre1" "NR" 120 (make-Date 2000 12 11) 0))
(define filmC (make-Film "a" "genre1" "NR" 120 (make-Date 2000 11 12) 0))
(define filmD (make-Film "a" "genre1" "NR" 120 (make-Date 2000 11 11) 0))
(define filmE (make-Film "a" "genre1" "NR" 120 (make-Date 1999 10 10) 0))

;; helper 1
;; getYear: Film -> Number
;; consumes a film and produces the open year of the film
(define (getYear film)(Date-year (Film-openingDate film)))

;; test
(check-expect (getYear filmA) 2001)
(check-expect (getYear filmB) 2000)
(check-expect (getYear filmE) 1999)

;; helper 2
;; getMonth: Film -> Number
;; consumes a film and produces the open month of the film
(define (getMonth film)(Date-month (Film-openingDate film)))

;; test
(check-expect (getMonth filmA) 11)
(check-expect (getMonth filmB) 12)
(check-expect (getMonth filmE) 10)

;; helper 3
;; getDay: Film -> Number
;; consumes a film and produces the open day of the film
(define (getDay film)(Date-day (Film-openingDate film)))

;; test
(check-expect (getDay filmA) 11)
(check-expect (getDay filmC) 12)
(check-expect (getDay filmE) 10)

;; opened-after?: Film Date -> Boolean
;; consumes a film and a date, and produces a Boolean. The function produces true if the given film opened after the given date, and returns false otherwise.
(define (opened-after? film date) (or (or (> (getYear film)(Date-year date))
                                          (> (getMonth film)(Date-month date)))
                                      (> (getDay film)(Date-day date))))

;; test
(check-expect (opened-after? filmA dateA) #t)
(check-expect (opened-after? filmB dateA) #t)
(check-expect (opened-after? filmC dateA) #t)
(check-expect (opened-after? filmD dateA) #f)
(check-expect (opened-after? filmE dateA) #f)