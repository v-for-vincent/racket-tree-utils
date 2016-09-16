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

#lang racket

(require racket-list-utils/utils)
(provide (struct-out node) nodeof)

(struct node (label children)
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

(define (nodeof type-predicate)
  (λ (elem) (and (node? elem)
                 (type-predicate (node-label elem))
                 ((listof (nodeof type-predicate)) (node-children elem)))))

(define (subtree-filter tree predicate)
  (define children (node-children tree))
  (if (predicate tree)
      (cons tree (append* (map (λ (c) subtree-filter c predicate) children)))
      (append* (map (λ (c) subtree-filter c predicate) children))))
; TODO provide, with a contract


; TODO provide, with a contract

(define (replace-first-subtree top replacee replacement)
  (car ((λ (t r1 r2) (replace-some-subtree-aux map-accumulatel t r1 r2)) top replacee replacement)))
(provide replace-first-subtree)

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