#lang racket

(require rackunit)
(require "../src/tree.rkt")

; tree implements custom equality check (so as to avoid transparency) so check that
(let* ([first-subtree (node 3 '())]
       [second-subtree (node 4 '())]
       [third-subtree (node 3 '())]
       [fourth-subtree (node 4 '())]
       [first-tree (node 2 (list first-subtree second-subtree))]
       [second-tree (node 2 (list third-subtree fourth-subtree))])
  (check-equal? first-tree second-tree))

(let* ([first-subtree (node 3 '())]
       [second-subtree (node 4 '())]
       [third-subtree (node 5 '())]
       [fourth-subtree (node 6 '())]
       [first-tree (node 1 (list first-subtree second-subtree))]
       [second-tree (node 2 (list third-subtree fourth-subtree))])
  (check-not-equal? first-tree second-tree))