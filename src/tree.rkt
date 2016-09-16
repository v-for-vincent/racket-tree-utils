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
  (match tree
    [(node label children)
     (if (predicate tree)
      (cons tree (append* (map (λ (c) subtree-filter c predicate) children)))
      (append* (map (λ (c) subtree-filter c predicate) children)))]))
; TODO provide, with a contract


; TODO provide, with a contract
(define (replace-first-subtree top replacee replacement)
  (second (replace-first-subtree-aux top replacee replacement)))

; result is a pair: whether replacement took place, plus the tree after replacement
(define (replace-first-subtree-aux top replacee replacement)
  (if (equal? top replacee)
      (cons #t replacement)
      (match top
       [(node label children)
        ; accumulator is whether-substitution-has-taken-place-yet
        (mapAccum (λ (c acc)
        ; dit gaat zo niet werken: alles behalve #f telt als #t, dus recursieve call zal altijd tellen als #t
        ;(mapAccum (λ (c acc) (if acc (cons c acc) (replace-first-subtree-aux c replacee replacement))) #f children)

; TODO provide, with a contract
;






;
;-- | Replace the last instance of a subtree with a different subtree.
;replaceLastSubtree :: (Eq a)
;                   => Tree a -- ^ the top-level tree
;                   -> Tree a -- ^ the original subtree
;                   -> Tree a -- ^ the new subtree
;                   -> Tree a -- ^ the resulting tree
;replaceLastSubtree top sub1 sub2 = snd $ replaceLastSubtree' top sub1 sub2
;

      
;-- | Replace the last instance of a subtree with a different subtree.
;replaceLastSubtree' :: (Eq a)
;                    => Tree a -- ^ the top-level tree
;                    -> Tree a -- ^ the original subtree
;                    -> Tree a -- ^ the new subtree
;                    -> (Bool, Tree a) -- ^ whether replacement took place, plus the resulting tree
;replaceLastSubtree' top sub1 sub2 
;  | top == sub1 = (True, sub2)
;replaceLastSubtree' (Node label children) sub1 sub2 = 
;  (update, (Node label updatedChildren)) where
;  (update, updatedChildren) = mapAccumR (\acc x -> if acc then (acc, x) else (replaceLastSubtree' x sub1 sub2)) False children