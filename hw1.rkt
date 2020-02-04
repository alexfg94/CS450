#lang racket

;;;Alex Green
;;;CS450
;;;HW1

;;;;;;PART 1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Question 1
;;;a.)
(define ex1
  (* (/ (- 10 2) 7) (- (/ 1 7) 8)))

;;;b.)
(define ex2
  (list
   (* (/ (- 10 2) 7) (- (/ 1 7) 8))
   (* (/ 8 7) (- 0.142857 8) )
   (* 1.142857 -7.857143 )
   8.979591))

;;;Question 2
(define ex3 (lambda (x y)
    (> (+ (- 6 y) (+ 11 y)) (+ x (+ y x)))))

;;;;;;PART 2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Question 3

(define null '() ) ;;;empty node 

(define Tree   ;;;parameterized constructor   (value . (right . left)) <=== Tree Node 
  (lambda (value left right)
    (cons value (cons left right))))

;;;getters 
(define get_value ;;node's value 
  (lambda (node)
    (car node))) 

(define get_left ;;node's left child 
  (lambda (node)
    (cadr node)))

(define get_right ;;node's right child 
  (lambda (node)
    (cddr node)))

;;;Setters
(define setLeft  ;;node's new left child 
  (lambda (node left)
    (cons (get_value node) (cons left (get_right node)))))
    
(define setRight ;;node's new right child 
  (lambda (node right)
    (cons (get_value node) (cons (get_left node) right))))

(define setValue ;;node's new value 
  (lambda (node value)
    (cons value (cons (get_left node) (get_right node)))))


(define insert  ;;insert node 
  (lambda ( node value )
    (cond
      ( (equal? node null) (Tree null value null) )
      ( (equal? value (get_value node)) (set_value node value))
      ( (< value (get_value node)) ( (insert (get_left node) value)))
      ( #t ((setRight node (insert (get_right node) value))))))) 

;;;Question 4
;;a.)
(define lambda?   ;;;(quote (lambda (x) x) )
  (lambda (expr)
    (if (list? expr)
        (if (equal? (car expr) 'lambda)
            (if (list? (cadr expr))
                (if (map symbol? (cadr expr))
                    (if (map symbol? (cddr expr))
                        #t
                        #f)
                    #f)
                #f)
            #f)
        #f)))

;;b.)
(define lambda-params
  (lambda (expr)
    (if (lambda? expr)
        (cadr expr)
        #f)))
       
;;c.)
(define lambda-body
  (lambda (expr)
    (if (lambda? expr)
        (cddr expr)
        #f)))

;;d.)
(define apply?
  (lambda (expr)
    (if (lambda? expr)
        #f
        #t)))

;;e.)
(define apply-func
  (lambda (expr)
    (if (list? expr)
        (cadr expr))
    #f))

;;f.)
(define apply-args
  (lambda (expr)
    (if (list? expr)
        (cdadr expr)
        #f)))
;;g.)
(define define?
  (lambda (expr)
    (if (list? expr)
         (if (equal? (car expr) 'define)
             #t
             #f)
         #f)))

;;h.)
(define define-basic?
  (lambda (expr)
    (if (list? expr)
        (if (define? expr)
            (if (not(list? (cadr expr)))
                #t
                #f)
            #f)
        #f)))

;;i.)
(define define-func?
  (lambda (expr)
    (if (list? expr)
        (if (define? expr)
            (if (list? (cadr expr))
                #t
                #f)
            #f)
        #f)))
                
     
               
            


