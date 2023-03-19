#lang racket
 (provide insert)
  
    (provide traverse)
    (provide find)
    (provide nl-to-vl)


(struct node (value count left right) #:transparent #:mutable)

(struct bst (root) #:transparent #:mutable)


(define bst-create-empty   ; constructor---create an empty BST
  (lambda ()
    '()))

(define (find t v)
 (define (helper current-path current-node)  ;helper function route without 
    (cond
      
      [(null? current-node) current-path]
      [(= v (node-value current-node))
       (append (cons current-node current-path))]
      [(< v (node-value current-node))
      (helper (cons current-node current-path) (node-left current-node))]
      [else
       
       (helper (cons current-node current-path) (node-right current-node))]))
(helper '() (bst-root t)))


(define (traverse t)  ;traverse will use a helper function traverse-node to recursively go through the tree t
  (traverse-node (bst-root t))
  )
(define (traverse-node n)
  (cond
    
    [(null? n) null]
    [else (append (traverse-node (node-left n))  ;recursive call
                  (list n)
                  (traverse-node (node-right n)))])) ; more recursion

(define (nl-to-vl x)
  (cond
   
    [(empty? x) empty]
    
    [else
     (cons (node-value (first x)) ; use x to access the first 
           (nl-to-vl (rest x)))]))  ; use rest to recursively iterate through the list x

(define (random-value r)
  (+ (random (- r 1)) 1) )

(define (random-list n r)
  (build-list n (lambda (x) (random-value r))))



(define (insert t v)
  (define path (find t v)) ; define path to get the curr 'list' we are working with 
    (if (null? path) ; BST is empty
        (set-bst-root! t (node v 1 '() '())) ; use the root and the initial t tree to set the root to a new node v
        (let ((head (car path))) ; let will allow us to insert the value of the car of path using the head value
          (if (= v (node-value head)) ; node already exists
              (set-node-count! head (+ (node-count head) 1))  ;; like in java node.count? we increment this by one
              (let ((new-node (node v 1 '() '())))   ;;create the new node as we would in java.
                (if (< v (node-value head))
                    (set-node-left! head new-node)
                    (set-node-right! head new-node)))))))

(define (insert-from-list t y)
 (map (lambda (x) (insert t x) (displayln (nl-to-vl (traverse t)))) y) (void)
)


;(define (delete t v))



(define n10 (node 70 1 null null))
(define n09 (node 65 1 null n10))
(define n08 (node 30 1 null null))
(define n07 (node 87 1 null null))
(define n06 (node 60 1 null n09))
(define n05 (node 37 1 n08 null))
(define n04 (node 12 1 null null))
(define n03 (node 75 1 n06 n07))
(define n02 (node 25 1 n04 n05))
(define n01 (node 50 1 n02 n03))

(define b01 (bst n01))

 (node? n10) ; test for membership for node n10
(node-value n10) ; access of value field of node n10
(node-count n08) ; access of count field of node n08
(bst? b01) ; test for bst membership for b01
(empty? (bst-root b01)) ; check if root of b01 is empty


;;find
(find b01 70)
(find b01 50)


(nl-to-vl (find b01 70))
(nl-to-vl (find b01 50))


;traverse
(traverse b01)
(nl-to-vl (traverse b01))


;map
(nl-to-vl (traverse b01))

(map (lambda (x) (nl-to-vl (find b01 x))) (nl-to-vl (traverse b01)))

(define btree (bst null)) ; create new bst named btree initialized to empty
btree ; display its contents
(define rlist (random-list 15 1000)) ; create a random list rlist
rlist ; display its contents
(insert-from-list btree rlist) ; insert all values from rlist into btree

(define btree-2 (bst null))
btree-2
(define list-2 '(50 25 75 12 37 30 45 60 80 12 60))
list-2
(insert-from-list btree-2 list-2)

(nl-to-vl (traverse btree-2))

(define tree-3 (bst null))
tree-3
(define list-3 (random-list 10 1000))
list-3
(insert-from-list tree-3 list-3)