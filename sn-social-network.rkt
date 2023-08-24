(module sn-social-network racket

  (provide 
   sn-ff-for
   sn-cmn-frnds-btwn
   sn-cmn-frnds
   sn-frnd-cnt
   sn-frndlst-user
   sn-unfrndlst-user  )

  (require rackunit)
  (require racket/math)


  (require "sn-graph.rkt")
  (require "sn-utils.rkt")
  (require srfi/1)

  (require racket/dict)
  ;(require data/dict)

  
  ;; social-network.
  ;; Easy
  ;; [(k,v)]| (u,vu) -> vu
  (define (sn-ff-for graph u1)
    ;; using built in function to retrieve value of a given key where key is user id and value is list
    ;;of friends if key does'nt exist return empty list, 
    (dict-ref graph u1 '())) 
    ;(/ 1 0))


  ;; Medium
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;; f2 & f3

  ;; this function is defined so that the function sn-cmn-frnds-btwn works 
  (define (my-intersect lst1 lst2) ;; takes in two lists and return new list 
  (filter (lambda (x) (member x lst2)) lst1)) ;; filters out elements from lst 1 that are also present
  ;;in lst 2 and creates new list contain common elements in both

(define (sn-cmn-frnds-btwn graph u1 u2)
  (let* ([ff1 (sn-ff-for graph u1)] ; get u1's common friends
         [ff2 (sn-ff-for graph u2)] ; get u2's common friends
         [ffs (my-intersect ff1 ff2)]) ; get intersection of u1's and u2's common friends
    (remove-duplicates ffs))) ; remove duplicates and return the list
    

  
  ;; Hard
  (define (sn-frnd-cnt graph)
  (define (count-friends user)  ;; defines a new function that takes a user as argument and returns a pair
    ;;containing user and length of their friend list
    (cons user (length (sn-ff-for graph user)))) ;; length of friend list is computed by calling the sn-ff-for function
  (map count-friends (sn-users graph))) ;; maps the count-friends function over the list of users in the graph

 
    ;( / 1 0))

  ;; pre: length > 0
  
  (define (sn-frndlst-user graph)
  (let* ([friend-counts (sn-frnd-cnt graph)] ;; let is used to define the three local variables, friend-counts
         ;;is defined by calling sn-frnd-cnt with graph argument
         [max-friends (apply max (map cdr friend-counts))] ;; defined by finding the max value of second element each pair
         ;;(user . count) in friends-count list 
         [max-friends-users (map car (filter (lambda (x) (= (cdr x) max-friends)) friend-counts))]) ;; defined by finding the car
         ;;(first element) of each (user . count) pair in friend-counts where count = max-friends
    (cons (car max-friends-users) max-friends)))


              
          
  ;; pre: length > 0

(define (sn-unfrndlst-user graph)
  (define min-friends (apply min (map cdr (sn-frnd-cnt graph)))) ;; defines min-friends and uses apply to apply
  ;;the min function to list of integers 
  (let ((unfriends (filter (lambda (x) (= (cdr x) min-friends)) (sn-frnd-cnt graph)))) ;; uses filter and lambda
    ;;functions to create a new list unfriends
    (cons (caar unfriends) min-friends))) ;; returns a pair consisting of user with min number of friends and
  ;;the number of friends they have 



    ;(/ 1 0))

  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds-ff2 graph u)
    (let*
        ([keys (sn-users graph)]
         [vals (map
                (lambda (key)
                  (sn-cmn-frnds-btwn graph u key))
                keys)]
       
         )
      (sn-dict-ks-vs keys vals)))


  ;; this is for free. Do not mdify (ROM)
  (define (sn-cmn-frnds graph )
    (let*
        ([keys (sn-users graph)]
         [vals (map
                (lambda (key)
                  (sn-cmn-frnds-ff2 graph key))
                keys)]
         )
      (sn-dict-ks-vs keys vals)))

  )

