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
(provide (struct-out node))

(require scribble/srcdoc)
(require (for-doc scribble/manual))

(serializable-struct node (label children)
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
                           (hash2-recur (node-children a))))])

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