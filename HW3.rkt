;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1
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
(define merchandise2 (make-merchandise "b" "costume"      #f 5 2))
(define merchandise3 (make-merchandise "c" "trading card" #t 1 2))
(define merchandise4 (make-merchandise "c" "trading card" #f 0 2))
(define merchandise5 (make-merchandise "c" "board game"   #t 1 4))
(define merchandise6 (make-merchandise "c" "board game"   #f 0 2))
(define merchandise7 (make-merchandise "c" "board game"   #f 1 2))

;; Problem 2
;; merchandise-func: Merchandise ... -> ...
;; ...
;(define (merchandise-func oneMerchandise ...)
;  (merchandise-name oneMerchandise)
;  (merchandise-kind oneMerchandise)
;  (merchandise-autographed oneMerchandise)
;  (merchandise-quantity oneMerchandise)
;  (merchandise-price oneMerchandise))

;; Problem 3
;; a ListOfMerchandise is one of
;;  empty
;;  (cons Merchandise ListOfMerchandise)
;; interp:  ListOfMerchandise represents a list of Merchandise
(define receipt1 (cons merchandise1 (cons merchandise2 empty)))
(define receipt2 (cons merchandise1 (cons merchandise3 (cons merchandise2 empty))))
(define receipt3 (cons merchandise3 (cons merchandise2 empty)))
(define receipt4 (cons merchandise1 (cons merchandise3 (cons merchandise2 (cons merchandise4 empty)))))
(define receipt5 (cons merchandise1 (cons merchandise3 (cons merchandise2 (cons merchandise4 (cons merchandise5 (cons merchandise6 (cons merchandise7 empty))))))))

;; Problem 4
;; receipt-template: ListOfMerchandise -> ...
;;
;(define (receipt-template an-receipt)
;  (cond [(empty? an-receipt)      ]
;        [(cons? an-receipt) (Merchandise-template (first an-receipt)
;                                                  (receipt-template (rest an-receipt)))]))

;; Problem 5
;; helper1
;; getAutographed?: Merchandise -> Boolean
;; consumes a Merchandise and produce the autographed? of the Merchandise
(define (getAutographed? Merchandise) (merchandise-autographed? Merchandise))
(check-expect (getAutographed? merchandise1) #t)
(check-expect (getAutographed? merchandise2) #f)

;; helper2
;; getPrice: Merchandise -> Number
;; consumes a Merchandise and produce the price
(define (getPrice Merchandise) (merchandise-price Merchandise))
(check-expect (getPrice merchandise1) 1)
(check-expect (getPrice merchandise2) 2)

;; list-cheap-autograph: ListOfMerchandise Number -> ListOfMerchandise
;; consumes a receipt and a number (representing a threshold cost) and produces a receipt.
;; The receipt that is produced contains only those items from the original receipt that are autographed and that cost no more than the number given.
(define (list-cheap-autograph list cost) (cond[(empty? list) empty]
                                              [(and (getAutographed? (first list))(<= (getPrice (first list)) cost)) (cons (first list) (list-cheap-autograph (rest list) cost))]
                                              [else (list-cheap-autograph (rest list) cost)]))
(check-expect (list-cheap-autograph receipt1 2) (cons merchandise1 empty))
(check-expect (list-cheap-autograph receipt2 2) (cons merchandise1 (cons merchandise3 empty)))

;; Problem 6
;; helper1
;; getKind: Merchandise -> String
;; consumes a Merchandise and produce the kind of the Merchandise
(define (getKind Merchandise)(merchandise-kind Merchandise))
(check-expect (getKind merchandise1) "trading card")
(check-expect (getKind merchandise2) "costume")

;; helper2
;; getQuantity: Merchandise -> Number
;; consumes a Merchandise and produce the quantity of the Merchandise
(define (getQuantity Merchandise) (merchandise-quantity Merchandise))
(check-expect (getQuantity merchandise1) 1)
(check-expect (getQuantity merchandise2) 5)

;; count-trading-cards: ListOfMerchandise -> Number
;; consumes a receipt and returns the total number of items in the order that are trading cards.
(define (count-trading-cards list) (cond[(empty? list) 0]
                                        [(and (string=? (getKind (first list)) "trading card")(> (getQuantity (first list)) 0)) (+ 1 (count-trading-cards (rest list)))]
                                        [else (count-trading-cards (rest list))]))
(check-expect (count-trading-cards receipt4) 2)
(check-expect (count-trading-cards receipt3) 1)

;; Problem 7
;; helper1
;; getCost: Merchandise -> Number
;; consumes a Merchandise and produce the cost of the Merchandise
(define (getCost Merchandise)(merchandise-price Merchandise))
(check-expect (getCost merchandise1) 1)
(check-expect (getCost merchandise2) 2)

;; receipt-total: ListOfMerchandise -> Number
;; onsumes a receipt and produces the total cost of all the merchandise items (a number)
(define (receipt-total list) (cond[(empty? list) 0]
                                  [(> (getQuantity (first list)) 0) (+ (getCost (first list)) (receipt-total (rest list)))]
                                  [else (receipt-total (rest list))]))
(check-expect (receipt-total receipt5) 11)
(check-expect (receipt-total receipt4) 5)

;; Problem 8
;; board-games-cost: ListOfMerchandise -> Number
;; consumes a receipt and produces a number. The function calculates the total cost of all the board games contained in the receipt
(define (board-games-cost list) (cond[(empty? list) 0]
                                  [(and (string=? (getKind (first list)) "board game")(> (getQuantity (first list)) 0)) (+ (getCost (first list)) (board-games-cost (rest list)))]
                                  [else (board-games-cost (rest list))]))
(check-expect (board-games-cost receipt4) 0)
(check-expect (board-games-cost receipt5) 6)

;; Problem 9
;; helper1
;; calcFinalPrice: Number Number -> Number
;; consumes the cost of item and the discount, produces the final price of the item
(define (calcFinalPrice price discount) (* price discount))
(check-expect (calcFinalPrice 10 0.5) 5)
(check-expect (calcFinalPrice 100 0.5) 50)

;; halloween-sale: ListOfMerchandise Number -> Number
;; The function consumes a receipt and a number representing the discount on costume items (in decimal 
;; form). The function produces the total cost of the receipt, with the discount applied only to costume merchandise
(define (halloween-sale list discount) (cond[(empty? list) 0]
                                            [(> (getQuantity (first list)) 0) (+ (calcFinalPrice (getCost (first list)) discount) (halloween-sale (rest list) discount))]
                                            [else (halloween-sale (rest list) discount)]))
(check-expect (halloween-sale receipt5 0.1) 1.1)
(check-expect (halloween-sale receipt4 0.1) 0.5)