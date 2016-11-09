#lang racket

(require rackunit)
(require "../src/tree.rkt")

(define-syntax (tree-bp stx)
  (syntax-case stx ()
    [(_ (LABEL SUBTREE ...))
     #'(node LABEL (list (tree-bp SUBTREE) ...))]))

(check-equal?
 (tree-bp
  (2
   (3)
   (4)))
 (tree-bp
  (2
   (3)
   (4))))

(check-not-equal?
 (tree-bp
  (1
   (3)
   (4)))
 (tree-bp
  (2
   (5)
   (6))))

(check-equal?
 (replace-first-subtree
  (tree-bp
   (1
    (2
     (3)
     (4))
    (2
     (3)
     (4))))
  (tree-bp
   (2
    (3)
    (4)))
  (tree-bp
   (5
    (6)
    (7))))
 (tree-bp
  (1
   (5
    (6)
    (7))
   (2
    (3)
    (4)))))

(check-equal?
 (replace-last-subtree
  (tree-bp
   (1
    (2
     (3)
     (4))
    (2
     (3)
     (4))))
  (tree-bp
   (2
    (3)
    (4)))
  (tree-bp
   (5
    (6)
    (7))))
 (tree-bp
  (1
   (2
    (3)
    (4))
   (5
    (6)
    (7)))))

(test-case
 "extracting a level from a tree"
 (check-equal?
  (horizontal-level (tree-bp (7)) 0)
  '(7))
 (check-equal?
  (horizontal-level (tree-bp (7)) 1)
  '())
 (check-equal?
  (horizontal-level
   (tree-bp
    (7
     (4)
     (5)))
   1)
  '(4 5))
 (check-equal?
  (horizontal-level
   (tree-bp
    (7
     (4)
     (5)))
   2)
  '())
 (check-equal?
  (horizontal-level
   (tree-bp
    (8
     (7)
     (5
      (3)
      (1))))
   2)
  '(3 1)))

(test-case
 "subtrees at a given level"
 (check-equal?
  (horizontal-level
   (tree-bp
    (8
     (7)
     (5)))
   0
   #t)
  (list
   (tree-bp
    (8
     (7)
     (5)))))
 (check-equal?
  (horizontal-level
   (tree-bp
    (8
     (7
      (3)
      (2))
     (5
      (9)
      (1))))
   1
   #t)
  (list
   (tree-bp
    (7
     (3)
     (2)))
   (tree-bp
    (5
     (9)
     (1))))))

(test-case
 "determining the depth of a tree"
 (check-equal?
  (node-depth
   (tree-bp
    (1)))
  0)
 (check-equal?
  (node-depth
   (tree-bp
    (1
     (2))))
  1)
 (check-equal?
  (node-depth
   (tree-bp
    (1
     (2
      (3)
      (4))
     (5))))
  2)
 (check-equal?
  (node-depth
   (tree-bp
    (1
     (2)
     (3
      (4)
      (5)))))
  2))