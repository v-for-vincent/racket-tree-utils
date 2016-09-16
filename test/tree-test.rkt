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

(let* ([leaf1 (node 3 '())]
       [leaf2 (node 4 '())]
       [subtree1 (node 2 (list leaf1 leaf2))]
       [subtree2 subtree1]
       [top1 (node 1 (list subtree1 subtree2))]
       [new-leaf1 (node 6 '())]
       [new-leaf2 (node 7 '())]
       [new-subtree1 (node 5 (list new-leaf1 new-leaf2))]
       [new-top1 (node 1 (list new-subtree1 subtree2))])
  (check-equal? (replace-first-subtree top1 subtree1 new-subtree1) new-top1))

(let* ([leaf1 (node 3 '())]
       [leaf2 (node 4 '())]
       [subtree1 (node 2 (list leaf1 leaf2))]
       [subtree2 subtree1]
       [top1 (node 1 (list subtree1 subtree2))]
       [new-leaf3 (node 6 '())]
       [new-leaf4 (node 7 '())]
       [new-subtree2 (node 5 (list new-leaf3 new-leaf4))]
       [new-top1 (node 1 (list subtree1 new-subtree2))])
  (check-equal? (replace-last-subtree top1 subtree1 new-subtree2) new-top1))