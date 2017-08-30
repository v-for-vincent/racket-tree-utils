; MIT License
; 
; Copyright (c) 2016 Vincent Nys
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

(require racket-list-utils/utils)
(require racket/contract/parametric)
(require racket/serialize)
(require racket/struct) ; for constructor style printer
(provide (struct-out node))

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(serializable-struct
 node (label children)
 #:transparent
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

(define (subtree-filter tree predicate)
  (define children (node-children tree))
  (if (predicate tree)
      (cons tree (append* (map (λ (c) subtree-filter c predicate) children)))
      (append* (map (λ (c) subtree-filter c predicate) children))))

(define (replace-first-subtree top replacee replacement)
  (car ((λ (t r1 r2) (replace-some-subtree-aux map-accumulatel t r1 r2)) top replacee replacement)))
(provide (contract-out [replace-first-subtree (parametric->/c [T] (-> node? node? node? node?))]))

(define (replace-last-subtree top replacee replacement)
  (car ((λ (t r1 r2) (replace-some-subtree-aux map-accumulater t r1 r2)) top replacee replacement)))
(provide replace-last-subtree)

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

; TODO provide, with a contract

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
(provide
 (proc-doc/names
  node-depth
  (-> node? exact-nonnegative-integer?)
  (n)
  @{Computes the (maximum) depth of @racket[n], where 0 is the depth of a childless node.}))

(define (node-map proc n)
  (node (proc (node-label n))
        (map (curry node-map proc) (node-children n))))
(provide
 (proc-doc/names
  node-map
  (-> (-> any/c any/c) node? node?)
  (proc n)
  @{Recursively maps the procedure @racket[proc] over @racket[n].}))

(define (node-max n)
  (max (node-label n) (apply max (map node-max (node-children n)))))
(provide
 (proc-doc/names
  node-max
  (-> node? real?)
  (n)
  @{Finds the maximum value in a tree of @racket[real?] values.}))

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
    (node
     'hello
     (list
      (node 'world (list))
      (node
       'this
       (list
        (node 'is (list))))
      (node 'vincent (list)))))
   5))
(provide
 (proc-doc/names
  size
  (-> node? exact-positive-integer?)
  (n)
  @{Computes the total number of nodes in @racket[n].}))