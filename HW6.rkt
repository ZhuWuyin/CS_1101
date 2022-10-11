;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #t () #t)))
;; Problem 1
;; a message is a (make-message String String Boolean)
;; message-username: Message -> String
;; message-text: Message -> String
;; message-read?: Message -> Boolean
;; interp:
;;     username is name of sender
;;     text is the message that the sender sent
;;     read? represent whether the message have been read or not
(define-struct message (username text read?))

;; a user is a (make-user String Message String)
;; user-username: User -> String
;; user-mailbox: User -> ListOfMessage
;; interp:
;;     username is the name of the user
;;     mailbox is the email address of the user
(define-struct user (username mailbox))

;; list-of-user-fcn: ListOfUser -> ...
;;
;; (define (list-of-user-fcn a-user)
;;   (cond [(empty? a-user) (...)]
;;         [(cons? a-user) (user-func (first a-user)
;;                                                   (list-of-user-fcn (rest a-user)))]))

;; Problem 2
(define mailsys empty)
(define newuser (make-user "Newuser" empty))

;; Problem 3
;; add-new-user: String -> Void
;; consumes a username and produces void. The new user should have an empty mailbox
;; The effect of the function is to add a new user with the given username to the mail system.
(define (add-new-user username) (set! mailsys (append mailsys (list (make-user username empty)))))

;; Problem 4
;; send-email-message: String String Message -> Void
;; consumes the name of the sender of an email, the name of the recipient of the email, and the text of an email message, and produces void.
;; The effect of the function is to store a new unread message in the recipient's mailbox
(define (send-email-message sender recipient text) (local [(define (helper subMailsys) (cond [(empty? subMailsys) (error "No such user in the system")]
                                                                                             [else (cond [(string=? (user-username (first subMailsys)) recipient) (set-user-mailbox! (first subMailsys) (append (user-mailbox (first subMailsys)) (list (make-message sender text #f))))]
                                                                                                         [else (helper (rest subMailsys))])]))] (helper mailsys)))

;; Problem 5
;; get-all-unread-messages: String -> ListOfMessage
;; consumes a username and produces a list of messages. The produced list contains the unread messages in the mailbox of the user with the given name. Assume the username is a valid user in the mail system
;; The effect of the function is that all unread messages in the named user's mailbox have been set to read
(define (get-all-unread-messages name) (local [(define (helper subMailsys) (cond [(empty? subMailsys) (error "No such user in the system")]
                                                                                 [else (cond [(string=? (user-username (first subMailsys)) name) (filter (lambda (m) (if (not (message-read? m)) (begin
                                                                                                                                                                                                   (set-message-read?! m #t)
                                                                                                                                                                                                   #t)
                                                                                                                                                                         #f)) (user-mailbox (first subMailsys)))]
                                                                                             [else (helper (rest subMailsys))])]))] (helper mailsys)))

;; Problem 6
;; most-total-messages: Nothing -> User
;; doesn't consume anything. The function produces the user in the mailsystem with the largest number of messages in his/her mailbox. If there are no users in the system, the function produces an appropriate error. If two or more users have the most messages, the function just needs to return one of them (it doesn't matter which one).
(define (most-total-messages) (local [(define (helper users) (cond [(empty? users) (error "No such user in the mail system")]
                                                                   [else (local [(define-struct acc (len user))
                                                                                 (define (helper users acc) (cond [(empty? users) (acc-user acc)]
                                                                                                                  [(> (length (user-mailbox (first users))) (acc-len acc)) (helper (rest users) (make-acc (length (user-mailbox (first users))) (first users)))]
                                                                                                                  [else (helper (rest users) acc)]))] (helper users (make-acc 0 #f)))]))] (helper mailsys)))

;; Problem 7
;; add-new-user
(add-new-user "a")
(add-new-user "b")
(add-new-user "c")
(check-expect mailsys (list (make-user "a" empty) (make-user "b" empty) (make-user "c" empty)))

;; send-email-message
(send-email-message "b" "a" "hello")
(send-email-message "b" "c" "hello")
(send-email-message "a" "c" "hello")
(check-expect mailsys (list (make-user "a" (list (make-message "b" "hello" #f))) (make-user "b" empty) (make-user "c" (list (make-message "b" "hello" #f) (make-message "a" "hello" #f)))))

;; get-all-unread-messages
(check-expect (get-all-unread-messages "a") (list (make-message "b" "hello" #true)))

;; most-total-messages
(check-expect (most-total-messages) (make-user "c" (list (make-message "b" "hello" #f) (make-message "a" "hello" #f))))

;; Problem 8
;; sum-of-string-lengths: ListOfString -> Number
;; consumes a ListOfString and produces the sum of the lengths of the strings in the list
(define (sum-of-string-lengths list1) (local [(define (helper strList acc) (cond [(empty? strList) acc]
                                                                                 [else (helper (rest strList) (+ acc (string-length (first strList))))]))] (helper list1 0)))
(check-expect (sum-of-string-lengths (list "a" "bb" "c")) 4)
(check-expect (sum-of-string-lengths empty) 0)

;; Problem 9
;; one-long-string: ListOfString -> String
;; consumes a ListOfString and produces the concatenation of strings in the list in the order they appear in the list
(define (one-long-string list1) (local [(define (helper strList acc) (cond [(empty? strList) acc]
                                                                           [else (helper (rest strList) (string-append acc (first strList)))]))] (helper list1 "")))
(check-expect (one-long-string (list "Alice" "Bob" "Eve")) "AliceBobEve")
(check-expect (one-long-string (list "Alice")) "Alice")