#lang racket

(require rackunit)
(require "../src/tree.rkt" "../src/printer.rkt")

(define tree-drawing
(substring
"
a
|
+-b
| |
| `-e
|
+-c
| |
| +-f
| |
| `-g
|
`-d
" 1))

; two parameters are needed if we want to allow the freedom to use control characters,...
; note that using display makes it so that quotes are not shown inside the tree
(define (node-display tree out)
  (display (node-label tree) out))

; printing, in general, is a side-effect (allowing for decorated output) but printing to a string port also works
(let* ([g-tree (node "g" '())]
       [f-tree (node "f" '())]
       [e-tree (node "e" '())]
       [d-tree (node "d" '())]
       [c-tree (node "c" (list f-tree g-tree))]
       [b-tree (node "b" (list e-tree))]
       [a-tree (node "a" (list b-tree c-tree d-tree))]
       [string-port (open-output-string)]
       [actual (begin (tree-display a-tree node-display string-port) (get-output-string string-port))])
  (check-equal? actual tree-drawing))