;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1
;; student?: Any -> Boolean
;; a student is a (make-student String String)
;; student-name: Student -> String
;; student-email: Student -> String
;; interp:
;;     name is the name of student
;;     email is the student's email
(define-struct student (name email))
(define student1 (make-student "a" "a@wpi.edu"))
(define student2 (make-student "b" "b@wpi.edu"))
(define student3 (make-student "c" "c@wpi.edu"))

;; a ListOfStudent is one of
;;  empty
;;  (cons Student ListOfStudent)
;; interp:  ListOfStudent represents a list of Student
(define list1 (list student1 student2 student3))
(define list2 (list student1 student3))

;; Problem 2
;; a BST is one of
;;   false
;;   Coursenode
;; a Coursenode is a (make-coursenode Number String String ListOfStudent BST BST)
(define-struct coursenode (project-id title advisor students left right))
(define node1 (make-coursenode 33.5546 "a" "a" list1 (make-coursenode 28.2222 "b" "b" list1 (make-coursenode 19.2222 "d" "d" list2 #false #false) (make-coursenode 31.2222 "e" "e" list1 #false #false)) (make-coursenode 40.2222 "c" "c" list2 #false #false)))

;; Problem 3
;; student-func: Student ... -> ...
;; ...
;(define (student-func oneStudent ...)
;  (student-name oneStudent)
;  (student-email oneStudent))

;; Problem 4
;; helper1
;; getID: Coursenode -> Number
;; consumes a coursenode and produces the project-id
(define (getID node) (coursenode-project-id node))
(check-expect (getID node1) 33.5546)
(check-expect (getID (coursenode-left node1)) 28.2222)

;; any-in-dept?: BST Number -> Boolean
;; consumes a binary search tree and the number of a department, and produces true if any of the projects in the project database are from that department.
(define (any-in-dept? BST num) (cond [(false? BST) #f]
                                     [(not (and (< (- (getID BST) num) 1) (> (- (getID BST) num) 0))) (or (any-in-dept? (coursenode-left BST) num)(any-in-dept? (coursenode-right BST) num))]
                                     [else #t]))
(check-expect (any-in-dept? node1 33) #t)
(check-expect (any-in-dept? node1 40) #t)
(check-expect (any-in-dept? node1 19) #t)
(check-expect (any-in-dept? node1 30) #f)

;; Problem 5
;; helper1
;; pop: ListOfStudent String -> ListOfStudent
;; consumes a ListOfString and a String, produces a new ListOfString which contains all element from the input-list except the input string
(define (pop list1 str) (cond [(empty? list1) empty]
                              [(not (string=? (student-email (first list1)) str)) (cons (first list1) (pop (rest list1) str))]
                              [else (pop (rest list1) str)]))
(check-expect (pop (list student1 student2 student3) "a@wpi.edu") (list student2 student3))
(check-expect (pop (list student1 student2 student3) "b@wpi.edu") (list student1 student3))
(check-expect (pop (list student1 student2 student3) "c@wpi.edu") (list student1 student2))

;; drop-student: BST Number String -> BST
;; consumes a binary search tree, a project number, and the email address of a student, and produces a binary search tree. The function drops (removes) the student with the given email address from the list of students working on the given project
(define (drop-student BST num str) (cond [(false? BST) #f]
                                         [(and (< (- (getID BST) num) 1) (> (- (getID BST) num) 0)) (make-coursenode (coursenode-project-id BST) (coursenode-title BST) (coursenode-advisor BST) (pop (coursenode-students BST) str) (drop-student (coursenode-left BST) num str) (drop-student (coursenode-right BST) num str))]
                                         [else (make-coursenode (coursenode-project-id BST) (coursenode-title BST) (coursenode-advisor BST) (coursenode-students BST) (drop-student (coursenode-left BST) num str) (drop-student (coursenode-right BST) num str))]))
(check-expect (drop-student node1 33 "a@wpi.edu") (make-coursenode 33.5546 "a" "a" (list student2 student3) (make-coursenode 28.2222 "b" "b" list1 (make-coursenode 19.2222 "d" "d" list2 #false #false) (make-coursenode 31.2222 "e" "e" list1 #false #false)) (make-coursenode 40.2222 "c" "c" list2 #false #false)))
(check-expect (drop-student node1 40 "a@wpi.edu") (make-coursenode 33.5546 "a" "a" list1 (make-coursenode 28.2222 "b" "b" list1 (make-coursenode 19.2222 "d" "d" list2 #false #false) (make-coursenode 31.2222 "e" "e" list1 #false #false)) (make-coursenode 40.2222 "c" "c" (list student3) #false #false)))
(check-expect (drop-student node1 19 "c@wpi.edu") (make-coursenode 33.5546 "a" "a" list1 (make-coursenode 28.2222 "b" "b" list1 (make-coursenode 19.2222 "d" "d" (list student1) #false #false) (make-coursenode 31.2222 "e" "e" list1 #false #false)) (make-coursenode 40.2222 "c" "c" list2 #false #false)))
(check-expect (drop-student node1 30 "a@wpi.edu") (make-coursenode 33.5546 "a" "a" list1 (make-coursenode 28.2222 "b" "b" list1 (make-coursenode 19.2222 "d" "d" list2 #false #false) (make-coursenode 31.2222 "e" "e" list1 #false #false)) (make-coursenode 40.2222 "c" "c" list2 #false #false)))

;; Problem 6
;; helper1
;; getLeft: BST -> BST
;; consumes a BST and produces it's left subnode
(define (getLeft BST) (coursenode-left BST))
(check-expect (getLeft node1) (coursenode-left node1))
(check-expect (getLeft node2) (coursenode-left node2))

;; helper2
;; getRight: BST -> BST
;; consumes a BST and produces it's right subnode
(define (getRight BST) (coursenode-right BST))
(check-expect (getRight node1) (coursenode-right node1))
(check-expect (getRight node2) (coursenode-right node2))

;; list-projects-in-order-by-id-num: BST -> ListOfString
;; consumes a binary search tree and produces a list of the titles of the projects, sorted in order by ascending project number
(define (list-projects-in-order-by-id-num BST) (cond [(false? BST) empty]
                                                     [else (append (list-projects-in-order-by-id-num (getLeft BST)) (list (coursenode-title BST)) (list-projects-in-order-by-id-num (getRight BST)))]))
(check-expect (list-projects-in-order-by-id-num node1) (list "d" "b" "e" "a" "c"))
(define node2 (make-coursenode 4.1111 "a" "a" list1 (make-coursenode 2.1111 "b" "b" list1 (make-coursenode 1.1111 "c" "c" list1 #f #f) (make-coursenode 3.1111 "d" "d" list1 #f #f))(make-coursenode 6.1111 "e" "e" list1 (make-coursenode 5.1111 "f" "f" list1 #f #f) (make-coursenode 7.1111 "g" "g" list1 #f #f))))
(check-expect (list-projects-in-order-by-id-num node2) (list "c" "b" "d" "a" "f" "e" "g"))
(define node3 (make-coursenode 6.1111 "e" "e" list1 (make-coursenode 4.1111 "d" "d" list1 (make-coursenode 3.1111 "b" "b" list1 (make-coursenode 1.1111 "a" "a" list1 #f #f) (make-coursenode 2.1111 "c" "c" list1 #f #f)) #f) (make-coursenode 7.1111 "f" "f" list1 #f #f)))
(check-expect (list-projects-in-order-by-id-num node3) (list "a" "b" "c" "d" "e" "f"))

;; Problem 7
;; add-project: BST Number String String -> BST
;; consumes a binary search tree, a project number, a project title, and the name of the advisor, and creates a binary search tree the same as the original except that a new project with the given information has been added to the tree.
(define (add-project BST projectID title advisor) (cond [(false? BST) (make-coursenode projectID title advisor empty #f #f)]
                                                        [(<= projectID (coursenode-project-id BST)) (make-coursenode (coursenode-project-id BST) (coursenode-title BST) (coursenode-advisor BST) (coursenode-students BST) (add-project (coursenode-left BST) projectID title advisor) (coursenode-right BST))]
                                                        [else (make-coursenode (coursenode-project-id BST) (coursenode-title BST) (coursenode-advisor BST) (coursenode-students BST) (coursenode-left BST) (add-project (coursenode-right BST) projectID title advisor))]))
(define test1 (make-coursenode 40.1111 "a" "a" empty #f #f))
(check-expect (add-project test1 33.1111 "b" "b") (make-coursenode 40.1111 "a" "a" '() (make-coursenode 33.1111 "b" "b" '() #false #false) #false))
(define test2 (make-coursenode 40.1111 "a" "a" '() (make-coursenode 33.1111 "b" "b" '() #false #false) #false))
(check-expect (add-project test2 55.1111 "c" "c") (make-coursenode 40.1111 "a" "a" '() (make-coursenode 33.1111 "b" "b" '() #false #false) (make-coursenode 55.1111 "c" "c" '() #false #false)))
(define test3 (make-coursenode 40.1111 "a" "a" '() (make-coursenode 33.1111 "b" "b" '() #false #false) (make-coursenode 55.1111 "c" "c" '() #false #false)))
(check-expect (add-project test3 54.1111 "g" "g") (make-coursenode 40.1111 "a" "a" '() (make-coursenode 33.1111 "b" "b" '() #false #false) (make-coursenode 55.1111 "c" "c" '() (make-coursenode 54.1111 "g" "g" '() #false #false) #false)))