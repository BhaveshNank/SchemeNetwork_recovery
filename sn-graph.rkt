(module sn-graph racket
  
  (require racket/dict)
 
  
  (provide sn-consistent
           sn-empty
           sn-add-user
           sn-users
           sn-add-frndshp)
           

  ;; required libraries. (imported above)
  ;(require racket/dict)
  ;;(require racket/set)

  
  ; Hard
  (define (sn-consistent p) #t)
  
  (require racket/dict)
  ;; graph
  ;; -> [a]
  ;; Easy (+0.5)
  
  ;; returns an empty dictionary 
  (define sn-empty
    empty)


  ;; Easy
  ;; [(k,v)] -> [u]
  
  ;; takes in graph as argument which is a dictionary
  (define (sn-users graph)
    ;; users are keys and list of friends values so dict-keys returns all the users in the dictionary
    (dict-keys  graph))
  
 
  ;(/ 1 0))

  
  ;; Hard
  ;; [(k,v)] u -> [(k,v)] | (u,{})
  (define (sn-add-user graph user)
    (if (dict-ref graph user #f)
      graph  ; user already exists, return network unchanged
      (dict-set graph user '()))) ; user doesn't exist, add new entry to the network
    

  ;; Hard
  ;; [(k,v)]|(u1,f1)|(u2,f2) ->
  ;;  [(k,v)] | (u1,f1+{f2}) | (u2,f2+{f1})
  (define (sn-add-frndshp graph u1 u2) 
    (map (lambda (lst) ;; mapping over each list in the graph
     (cond
       ;; checks if first element equal to u1, if yes then it checks if u2 is not in list if that is true it appends u2 to list 
      [(equal? (first lst) u1) (not (member u2 lst)) (append lst (list u2))]
      ;; checks if first element equal to u2, if yes then it checks if u1 is not in list if that is true it appends u1 to list
      [(equal? (first lst) u2) (not (member u1 lst)) (append lst (list u1))]
      [else lst] ;; neither the two found, it returns the original list
      ))graph))
  
 
    
    


  )
