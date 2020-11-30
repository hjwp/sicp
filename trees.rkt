#lang sicp
(newline)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(define atree (list->tree '(1 2 3 4 5 6 7)))
'(4 
  (2 
  (1 () ()) 
  (3 () ()))
  (6 
   (5 () ()) 
   (7 () ())))


(define (tree->list-1 tree)
  (display ".")
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (display ".")
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
   (copy-to-list tree '()))


(newline)
(tree->list-1 atree)
(newline)
(tree->list-2 atree)
(newline)

(define badtree 
  '(1 
    () 
    (2 
     () 
     (3 
      () 
      (4 
       () 
       (5 
        () 
        (6 
         () 
         (7 
          () 
          ()))))))))

(tree->list-1 badtree)
(newline)
(tree->list-2 badtree)
(newline)
