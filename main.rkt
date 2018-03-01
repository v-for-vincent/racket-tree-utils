; MIT License
; 
; Copyright (c) 2016-2017 Vincent Nys
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

#lang at-exp racket

(require racket/contract/parametric
         racket/serialize
         racket/struct
         racket/generator
         scribble/srcdoc
         (for-doc scribble/manual)
         list-utils)

(module+ test
  ;; tree boilerplate: trees of literals only require parentheses
  (define-syntax (tree-bp stx)
    (syntax-case stx ()
      [(_ (LABEL SUBTREE ...))
       #'(node LABEL (list (tree-bp SUBTREE) ...))])))

(serializable-struct
 node (label children)
 #:methods
 gen:equal+hash
 [(define (equal-proc a b equal?-recur)
    (and (equal?-recur (node-label a) (node-label b))
         (equal?-recur (node-children a) (node-children b))))
  ; same hash function as in Racket docs, not too concerned about optimum here
  (define (hash-proc a hash-recur)
    (+ (hash-recur (node-label a))
       (* 3 (hash-recur (node-children a)))))
  (define (hash2-proc a hash2-recur)
    (+ (hash2-recur (node-label a))
       (hash2-recur (node-children a))))]
 #:methods
 gen:custom-write
 [(define
    write-proc
    (make-constructor-style-printer
     (λ (obj) 'node)
     (λ (obj) (list (node-label obj) (node-children obj)))))])
(module+ test
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
     (6)))))
(provide
 (struct*-doc
  node
  ([label any/c]
   [children (listof node?)])
  @{A simple positional tree, i.e. one in which a node can have any number of children, where the children are ordered with respect to one another.}))

(define (subtree-filter tree predicate)
  (define children (node-children tree))
  (if (predicate tree)
      (cons tree (append-map (λ (c) (subtree-filter c predicate)) children))
      (append-map (λ (c) (subtree-filter c predicate)) children)))
(module+ test
  (define (all-even? n)
    (and
     (even? (node-label n))
     (andmap all-even? (node-children n))))
  (check-equal?
   (subtree-filter
    (tree-bp
     (1
      (2
       (3)
       (4))
      (6)
      (8
       (10)
       (12))))
    all-even?)
   (list
    (node 4 empty)
    (node 6 empty)
    (tree-bp
     (8
      (10)
      (12)))
    (node 10 empty)
    (node 12 empty))))
(provide
 (proc-doc/names
  subtree-filter
  (-> node? procedure? (listof node?))
  (tree proc)
  @{Filters out all the subtrees of @racket[tree] to which @racket[proc] applies, in depth-first order.}))

;; auxiliary function obtained by extracting common code from replace-first-subtree and replace-last-subtree
;; no real reason to provide this as well
(define (replace-some-subtree-aux map-accumulate top replacee replacement)
  (if (equal? top replacee)
      (cons replacement #t)
      (match-let
          ([(cons mapping replaced?)
            (map-accumulate (λ (child acc)
                              (if acc
                                  (cons child acc)
                                  (replace-some-subtree-aux map-accumulate child replacee replacement)))
                            #f
                            (node-children top))])
        (cons (node (node-label top) mapping) replaced?))))

(define (replace-first-subtree top replacee replacement)
  (car ((λ (t r1 r2) (replace-some-subtree-aux map-accumulatel t r1 r2)) top replacee replacement)))
(module+ test
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
      (4))))))
(provide
 (proc-doc/names
  replace-first-subtree
  (-> node? node? node? node?)
  (top replacee replacement)
  @{Replaces the first subtree equal to @racket[replacee] with @racket[replacement] in @racket[top].}))

(define (replace-last-subtree top replacee replacement)
  (car ((λ (t r1 r2) (replace-some-subtree-aux map-accumulater t r1 r2)) top replacee replacement)))
(module+ test
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
      (7))))))
(provide
 (proc-doc/names
  replace-last-subtree
  (-> node? node? node? node?)
  (top replacee replacement)
  @{Replaces the last subtree equal to @racket[replacee] with @racket[replacement] in @racket[top].}))

(define (horizontal-level tree depth [with-subtrees #f])
  (define (level-aux tree depth acc)
    (match tree
      [(node l ch)
       (if
        (equal? depth acc)
        (list (if with-subtrees tree l))
        (if
         (null? ch)
         (list)
         (apply append (map (λ (c) (level-aux c depth (+ acc 1))) ch))))]))
  (level-aux tree depth 0))
(module+ test
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
(provide
 (proc-doc/names
  horizontal-level
  (->* (node? exact-nonnegative-integer?) (boolean?) list?)
  ((tree depth) ((with-subtrees #f)))
  @{Extracts all nodes at depth @racket[depth] from @racket[tree]
 and returns them in left-to-right order.
 If @racket[tree] does not have any nodes at depth @racket[depth],
 the result is an empty list.
 If @racket[with-subtrees] is @racket[#t], the result is a list of nodes
 with their children, rather than just labels.}))

(define (node-depth n)
  (match n
    [(node l (list)) 0]
    [(node l ch) (+ 1 (apply max (map node-depth ch)))]))
(module+ test
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
(provide
 (proc-doc/names
  node-depth
  (-> node? exact-nonnegative-integer?)
  (n)
  @{Computes the (maximum) depth of @racket[n], where 0 is the depth of a childless node.}))

(define (node-map/df proc n)
  (node (proc (node-label n))
        (map (curry node-map/df proc) (node-children n))))
(provide
 (proc-doc/names
  node-map/df
  (-> (-> any/c any/c) node? node?)
  (proc n)
  @{Recursively maps the procedure @racket[proc] over @racket[n] in a depth-first manner.}))

(define (node-max n)
  (max (node-label n) (apply max (map node-max (node-children n)))))
(provide
 (proc-doc/names
  node-max
  (-> node? real?)
  (n)
  @{Finds the maximum value in a tree of @racket[real?] values.}))

(define (for-each/tree proc n)
  (proc n)
  (for ([c (node-children n)])
    (for-each/tree proc c)))
(module+ test
  (require rackunit)
  (let* ([out (open-output-string)]
         [proc (match-lambda
                 [(node l ch)
                  (displayln (format "~a with ~a children" l (length ch)) out)])])
    (for-each/tree
     proc
     (tree-bp (3
               (2
                (1))
               (0))))
    (check-equal?
     (get-output-string out)
     "3 with 2 children\n2 with 1 children\n1 with 0 children\n0 with 0 children\n")))
(provide
 (proc-doc/names
  for-each/tree
  (-> (-> node? void?) node? void?)
  (proc n)
  @{Similar to @racket[node-map/df], but @racket[proc] is only called for its effect, and its result is ignored.}))

(define (size n)
  (match n
    [(node _ ch)
     #:when (null? ch) 1]
    [(node _ ch)
     (add1 (apply + (map size ch)))]))
(module+ test
  (check-equal?
   (size (node 'hello (list)))
   1)
  (check-equal?
   (size
    (tree-bp
     (1
      (2
       (3)
       (4))
      (6))))
   5))
(provide
 (proc-doc/names
  size
  (-> node? exact-positive-integer?)
  (n)
  @{Computes the total number of nodes in @racket[n].}))

(define (subtrees n)
  (define (reyield g)
    (for ([v (in-producer g 'done)])
      (yield v)))
  (generator
   ()
   (yield n)
   (for ([ch (node-children n)])
     (reyield (subtrees ch)))
   (yield 'done)))
(module+ test
  (check-equal?
   (sequence->list
    (in-producer
     (subtrees
      (tree-bp
       (1
        (2
         (3))
        (4))))
     'done))
   (list
    (tree-bp
     (1
      (2
       (3))
      (4)))
    (tree-bp
     (2
      (3)))
    (tree-bp (3))
    (tree-bp (4)))))
(provide
 (proc-doc/names
  subtrees
  (-> node? generator?)
  (n)
  @{Computes a generator for all the subtrees of @racket[n] in depth-first order, including @racket[n] itself.}))

(define (visit proc n)
  (proc n)
  (for ([c (node-children n)])
    (visit proc c)))
(module+ test
  (require rackunit)
  (let* ([out (open-output-string)]
         [proc (match-lambda
                 [(node l ch)
                  (displayln (format "~a with ~a children" l (length ch)) out)])])
    (visit proc (node 3 (list (node 2 (list (node 1 (list)))) (node 0 (list)))))
    (check-equal?
     (get-output-string out)
     "3 with 2 children\n2 with 1 children\n1 with 0 children\n0 with 0 children\n")))
(provide
 (proc-doc/names
  visit
  (-> (-> node? void?) node? void?)
  (proc n)
  @{Applies @racket[proc] to @racket[n] and to each of its descendants.}))

