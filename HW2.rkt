;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1
;; tornado?: any -> Boolean
;; a tornado is a (make-tornado String Number Number)
;; interp:
;;     scale is the Fujita scale rating
;;     distance is the the distance the tornado travels in miles
;;     max-winds is the maximum sustained wind in miles per hour
(define-struct tornado (scale distance max-winds))

;; example
(define TriState1 (make-tornado "F1" 1000 1000))
(define TriState2 (make-tornado "F4" 1000 1000))
(define TriState3 (make-tornado "F5" 1000 1000))

;; hurricane?: any -> Boolean
;; a hurricane is a (make-hurricane String Number Number Number String)
;; interp:
;;     name is hurricane's name
;;     category is hurricane's category
;;     max-winds is the maximum sustained wind in miles per hour
;;     velocity is the velocity of the storm in miles per hour
;;     heading is the storm's heading
(define-struct hurricane (name category max-winds velocity heading))

;; example
(define hurricane1 (make-hurricane "Reno" 3 1000 1000 "NNW"))
(define hurricane2 (make-hurricane "Reno" 4 1000 1000 "NNW"))
(define hurricane3 (make-hurricane "Reno" 5 1000 1000 "NNW"))

;; a thunderstorm is a (make-thunderstorm String Number Number Number)
;; interp:
;;     heading is the thunderstorm's heading
;;     velocity is the velocity of the storm in miles per hour
;;     rainfall is the number of inches of rainfall
;;     max-winds is the maximum sustained wind in miles per hour
(define-struct thunderstorm (heading velocity rainfall max-winds))

;; example
(define Panda1 (make-thunderstorm "NNW" 1000 3 1000))
(define Panda2 (make-thunderstorm "NNW" 1000 10 1000))

;; 2
;; tornado-func: Tornado ... -> ...
;; ...
;(define (tornado-func oneTornado ...)
;  (tornado-scale oneTornado)
;  (tornado-distance oneTornado)
;  (tornado-max-winds oneTornado))

;; hurricane-func: Hurricane ... -> ...
;; ...
;(define (hurricane-func oneHurricane ...)
;  (hurricane-name oneHurricane)
;  (hurricane-category oneHurricane)
;  (hurricane-max-winds oneHurricane)
;  (hurricane-velocity oneHurricane)
;  (hurricane-heading oneHurricane))

;; thunderstorm-func: Thunderstorm ... -> ...
;; ...
;(define (thunderstorm-func oneThunderstorm ...)
;  (thunderstorm-heading oneThunderstorm)
;  (thunderstorm-velocity oneThunderstorm)
;  (thunderstorm-rainfall oneThunderstorm)
;  (thunderstorm-max-winds oneThunderstorm))

;; 4
;; violent?: Windstorm -> Boolean
;; consumes a windstorm and produces a boolean
(define (violent? Windstorm) (cond [(tornado? Windstorm) (or (string=? (tornado-scale Windstorm) "F4")(string=? (tornado-scale Windstorm) "F5"))]
                                   [(thunderstorm? Windstorm) (and (> (thunderstorm-rainfall Windstorm) 5)(> (thunderstorm-max-winds Windstorm) 50))]
                                   [(hurricane? Windstorm) (or (= (hurricane-category Windstorm) 4)(= (hurricane-category Windstorm) 5))]))
(check-expect (violent? Panda1) #f)
(check-expect (violent? Panda2) #t)
(check-expect (violent? hurricane1) #f)
(check-expect (violent? hurricane2) #t)
(check-expect (violent? hurricane3) #t)
(check-expect (violent? TriState1) #f)
(check-expect (violent? TriState2) #t)
(check-expect (violent? TriState3) #t)

;; 5
;; change-max-wind: Windstorm Number -> Windstorm
;; consumes a windstorm and a number representing wind speeds and produces a windstorm. The windstorm that's produced is a windstorm the same as the original, except that its max-winds is changed to the new wind speeds.
(define (change-max-wind Windstorm new-wind) (cond [(tornado? Windstorm) (make-tornado (tornado-scale Windstorm) (tornado-distance Windstorm) new-wind)]
                                                   [(hurricane? Windstorm) (make-hurricane (hurricane-name Windstorm) (hurricane-category Windstorm) new-wind (hurricane-velocity Windstorm) (hurricane-heading Windstorm))]
                                                   [(thunderstorm? Windstorm) (make-thunderstorm (thunderstorm-heading Windstorm) (thunderstorm-velocity Windstorm) (thunderstorm-rainfall Windstorm) new-wind)]))
(define test1 (change-max-wind Panda1 3))
(define test2 (change-max-wind TriState1 4))
(define test3 (change-max-wind hurricane1 5))
(check-expect (= (thunderstorm-max-winds test1) 3) #t)
(check-expect (= (tornado-max-winds test2) 4) #t)
(check-expect (= (hurricane-max-winds test3) 5) #t)

;; a ListOfString is one of
;;  empty
;;  (cons String ListOfString)
;; interp:  ListOfString represents a list of strings

;; 6
;; acrostic: ListOfString -> String
;; consumes a ListOfString and produces a String. The function produces a string consisting of just the first character of each string in the ListOfString.
(define (acrostic list) (cond [(empty? list) ""]
                              [else (string-append (substring (first list) 0 1)(acrostic (rest list)))]))

(check-expect (acrostic (cons "123" (cons "234" (cons "345" empty)))) "123")
(check-expect (acrostic (cons "123" empty)) "1")

;; 7
;; ickle-strings: ListOfString -> ListOfString
;; consumes a ListOfString and produces a ListOfString. The list that's produced contains only those strings from the original list that have "ickle" as a substring somewhere in them.
(define (ickle-strings list) (cond [(empty? list) empty]
                                   [(string-contains? "ickle" (first list)) (cons (first list) (ickle-strings (rest list)))]
                                   [else (ickle-strings (rest list))]))

(check-expect (ickle-strings (cons "pickle" (cons "abc" (cons "tickle" empty)))) (cons "pickle" (cons "tickle" empty)))
(check-expect (ickle-strings (cons "pickle" (cons "tickle" empty))) (cons "pickle" (cons "tickle" empty)))
(check-expect (ickle-strings (cons "nickle" empty)) (cons "nickle" empty))

;; 8
;; lengths-of-strings: ListOfString -> ListOfNatural
;; consumes a ListOfString and produces a ListOfNatural. The function produces a list of the lengths of each of the strings in the given ListOfString.
(define (lengths-of-strings strList) (cond [(empty? strList) empty]
                                           [else (cons (string-length (first strList)) (lengths-of-strings (rest strList)))]))

(check-expect (lengths-of-strings (cons "ab" (cons "b" (cons "qwq" empty)))) (cons 2 (cons 1 (cons 3 '()))))
(check-expect (lengths-of-strings (cons "ab" empty)) (cons 2 empty))