;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname li-z-yang-b-hw5-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; user Name: Boyi(byang4@wpi.edu) & Zehai(zli28@wpi.edu)

;; Problem 1
;; river?: Any -> Boolean
;; a river is a (make-river String Number ListOfRiver)
;; river-name: River -> String
;; river-ph: River -> Number
;; river-bloom: River -> Boolean
;; river-tributaries: River -> ListOfRiver
;; interp:
;;     name is the name of river
;;     ph is the ph of a river
;;     bloom? is the presence of algal blooms
;;     tributaries is a list of rivers
(define-struct river (name ph bloom? tributaries))

;; a ListOfRiver is one of
;;     empty
;; (cons River ListOfRiver)

;; Problem 2
;; examples
(define river1  (make-river "a" 8   #f empty))
(define river2  (make-river "b" 14  #f empty))
(define river3  (make-river "c" 10  #f empty))
(define river4  (make-river "d" 10  #f empty))
(define river5  (make-river "e" 9.5 #f empty))
(define river6  (make-river "f" 9.5 #f empty))
(define river7  (make-river "g" 10  #f empty))
(define river8  (make-river "h" 13  #f empty))

(define river9  (make-river "i" 10  #f (list river1  river2)))
(define river10 (make-river "j" 1   #t (list river9  river3)))
(define river11 (make-river "k" 10  #f (list river10 river4  river5)))
(define river12 (make-river "l" 4   #t (list river6  river11)))
(define river13 (make-river "m" 10  #f (list river12 river7)))
(define river14 (make-river "n" 10  #f (list river13 river8)))

;; Problem 3
;; river-func: River ... -> ...
;; ...
;(define (river-func oneRiver ...)
;  (river-name oneRiver)
;  (river-ph oneRiver)
;  (river-bloom oneRiver)
;  (list-of-river-fcn (river-tributaries oneRiver)))

;; list-of-river-fcn: ListOfRiver -> ...
;;
;(define (list-of-river-fcn an-tributaries)
;  (cond [(empty? an-tributaries) (...)]
;        [(cons? an-tributaries) (river-func (first an-tributaries)
;                                                  (list-of-river-fcn (rest an-tributaries)))]))

;; Problem 4
;; list-alkaline-rivers: River -> ListOfString
;; consumes a river system and produces a list of string. The function returns a list of the names of rivers in the system that have a pH level of 9.5 or greater.
(define (list-alkaline-rivers river) (cond [(river? river) (cond [(and (>= (river-ph river) 9.5) (empty? (river-tributaries river))) (list (river-name river))]
                                                                 [(empty? (river-tributaries river)) empty]
                                                                 [(>= (river-ph river) 9.5) (append (list (river-name river)) (list-alkaline-rivers (first (river-tributaries river))) (list-alkaline-rivers (rest (river-tributaries river))))]
                                                                 [else (append (list-alkaline-rivers (first (river-tributaries river))) (list-alkaline-rivers (rest (river-tributaries river))))])]
                                           [(empty? river) empty]
                                           [else (append (list-alkaline-rivers (first river)) (list-alkaline-rivers (rest river)))]))
(check-expect (list-alkaline-rivers river14) (list "n" "m" "f" "k" "i" "b" "c" "d" "e" "g" "h"))

;; Problem 5
;; algae-free?: River -> Boolean
;; consumes a river system and produces a boolean. The function returns true if no river in the system has an algal bloom.
(define (algae-free? river) (cond [(river? river) (cond [(river-bloom? river) (river-bloom? river)]
                                                        [(not (empty? (river-tributaries river))) (or (algae-free? (first (river-tributaries river))) (algae-free? (rest (river-tributaries river))))]
                                                        [else #f])]
                                  [(empty? river) #f]
                                  [else (or (algae-free? (first river)) (algae-free? (rest river)))]))
(check-expect (algae-free? river14) #t)
(check-expect (algae-free? river11) #t)
(check-expect (algae-free? river9 ) #f)

;; Problem 6
;; raise-all-ph: River -> River      ;(make-river (river-name river) (river-ph (+ river 0.5)) (river-bloom? river) ())
;; consumes a river system and produces a river system. The river system that is produced is the same as the original, except that the pH values of all the rivers in the system have been raised by 0.5
(define (raise-all-ph river) (cond [(river? river) (cond [(empty? (river-tributaries river)) (make-river (river-name river) (+ (river-ph river) 0.5) (river-bloom? river) empty)]
                                                         [else (make-river (river-name river) (+ (river-ph river) 0.5) (river-bloom? river) (append (list (raise-all-ph (first (river-tributaries river)))) (raise-all-ph (rest (river-tributaries river)))))])]
                                   [(empty? river) empty]
                                   [else (append (list (raise-all-ph (first river))) (raise-all-ph (rest river)))]))

(define test1  (make-river "a" 8.5   #f empty))
(define test2  (make-river "b" 14.5  #f empty))
(define test3  (make-river "c" 10.5  #f empty))
(define test4  (make-river "d" 10.5  #f empty))
(define test5  (make-river "e" 10    #f empty))
(define test6  (make-river "f" 10    #f empty))
(define test7  (make-river "g" 10.5  #f empty))
(define test8  (make-river "h" 13.5  #f empty))

(define test9  (make-river "i" 10.5  #f (list test1  test2)))
(define test10 (make-river "j" 1.5   #t (list test9  test3)))
(define test11 (make-river "k" 10.5  #f (list test10 test4  test5)))
(define test12 (make-river "l" 4.5   #t (list test6  test11)))
(define test13 (make-river "m" 10.5  #f (list test12 test7)))
(define test14 (make-river "n" 10.5  #f (list test13 test8)))
;test
(check-expect (raise-all-ph river14) test14)
(check-expect (raise-all-ph river13) test13)
(check-expect (raise-all-ph river12) test12)
(check-expect (raise-all-ph river11) test11)
(check-expect (raise-all-ph river10) test10)
(check-expect (raise-all-ph river9 ) test9 )

;; Problem 7
;; find-subsystem: String River -> River or Boolean
;; consumes the name of a river and a river system and produces either a river system or false. The function returns the portion of the original river system that has the named river as its root. If there is no river in the system with the given name, the function returns false
(define (find-subsystem name river) (local [(define (helper1 name river) (cond [(river? river) (cond [(string=? (river-name river) name) (list river)]
                                                                                                     [(empty? (river-tributaries river)) empty]
                                                                                                     [else (append (helper1 name (first (river-tributaries river))) (helper1 name (rest (river-tributaries river))))])]
                                                                               [(empty? river) empty]
                                                                               [else (append (helper1 name (first river)) (helper1 name (rest river)))]))] (cond [(empty? (helper1 name river)) #f]
                                                                                                                                                                 [else (first (helper1 name river))])))
(check-expect (find-subsystem "l" river14) river12)
(check-expect (find-subsystem "a" river14) river1)
(check-expect (find-subsystem "b" river14) river2)
(check-expect (find-subsystem "i" river14) river9)
(check-expect (find-subsystem "e" river14) river5)
(check-expect (find-subsystem "z" river14) #f)

;; Problem 8
;; merchandise?: Any -> Boolean
;; a merchandise is a (make-merchandise String String Boolean Number Number)
;; merchandise-name: Merchandise -> String
;; merchandise-kind: Merchandise -> String
;; merchandise-autographed?: Merchandise -> Boolean
;; merchandise-quantity: Merchandise -> Number
;; merchandise-price: Merchandise -> Number
;; interp:
;;     name is the name of items
;;     kind is the type of the items
;;     autographed? is used to describe whether or not the item is autographed
;;     quantity is the number of the items sold
;;     price is the price of a single item
(define-struct merchandise (name kind autographed? quantity price))
;; examples :
(define merchandise1 (make-merchandise "a" "trading card" #t 1 1))
(define merchandise2 (make-merchandise "b" "costume"      #t 5 2))
(define merchandise3 (make-merchandise "c" "trading card" #t 1 12))
(define merchandise4 (make-merchandise "d" "trading card" #f 0 12))
(define merchandise5 (make-merchandise "e" "board game"   #t 1 4))
(define merchandise6 (make-merchandise "f" "board game"   #f 0 2))
(define merchandise7 (make-merchandise "g" "board game"   #t 1 2))

;; a ListOfMerchandise is one of
;;  empty
;;  (cons Merchandise ListOfMerchandise)
;; interp:  ListOfMerchandise represents a list of Merchandise
(define receipt1 (cons merchandise1 (cons merchandise2 empty)))
(define receipt2 (cons merchandise1 (cons merchandise3 (cons merchandise2 empty))))
(define receipt3 (cons merchandise3 (cons merchandise2 empty)))
(define receipt4 (cons merchandise1 (cons merchandise3 (cons merchandise2 (cons merchandise4 empty)))))
(define receipt5 (cons merchandise1 (cons merchandise3 (cons merchandise2 (cons merchandise4 (cons merchandise5 (cons merchandise6 (cons merchandise7 empty))))))))

;; bargain-items:  ListOfMerchandise -> ListOfString
;; consumes a list of merchandise items and produces a list of the names of all the items with prices under 10$
(define (bargain-items list1) (local [(define (find item) (cond [(< (merchandise-price item) 10) (merchandise-name item)]
                                                                [else empty]))
                                      (define (kick list1) (not (empty? list1)))]
                                (filter kick (map find list1))))
(check-expect (bargain-items receipt2) (list "a" "b"))
(check-expect (bargain-items receipt5) (list "a" "b" "e" "f" "g"))

;; Problem 9
;; any-of-kind?:  ListOfMerchandise String -> Boolean
;; consumes a ListOfMerchandise and a kind of merchandise item produces true if there is an item of that kind in the ListOfMerchandise
(define (any-of-kind? list1 kind) (local [(define (find item) (cond [(string=? (merchandise-kind item) kind) #t]
                                                                    [else #f]))
                                          (define (kick bool) bool)]
                                    (cond [(empty? (filter kick (map find list1))) #f]
                                          [else #t])))
(check-expect (any-of-kind? receipt5 "a") #f)
(check-expect (any-of-kind? receipt4 "costume") #t)
(check-expect (any-of-kind? receipt5 "board game") #t)

;; Problem 10
;; list-cheap-autograph:  ListOfMerchandise Number -> ListOfMerchandise
;; consumes a list of merchandise items and returns a list of those autographed items that cost at most the given amount
(define (list-cheap-autograph list1 num) (local [(define (find item) (cond [(and (= (merchandise-price item) num)(merchandise-autographed? item)) item]
                                                                           [else empty]))
                                                 (define (kick list1) (merchandise? list1))]
                                           (filter kick (map find list1))))
(check-expect (list-cheap-autograph receipt5 143) empty)
(check-expect (list-cheap-autograph receipt5 2) (list merchandise2 merchandise7))
(check-expect (list-cheap-autograph receipt5 1) (list merchandise1))