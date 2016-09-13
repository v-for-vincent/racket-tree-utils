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

; Note that the current code does not output a String (the expected functional behaviour).
; This is because a Void return type allows things like underlined terminal output.

#lang racket
(require "tree.rkt")

(define (list-init lst)
  (take lst (- (length lst) 1)))

(define (next-margin margin for-last-child?)
  (cond [(and for-last-child? (equal? margin "")) "`-"]
        [(equal? margin "") "+-"]
        [(and for-last-child? (equal? (substring margin (- (string-length margin) 2) (string-length margin)) "+-")) (string-append (substring margin 0 (- (string-length margin) 2)) "| `-")]
        [(equal? (substring margin (- (string-length margin) 2) (string-length margin)) "+-") (string-append (substring margin 0 (- (string-length margin) 2)) "| +-")]
        [(and for-last-child? (equal? (substring margin (- (string-length margin) 2) (string-length margin)) "`-")) (string-append (substring margin 0 (- (string-length margin) 2)) "  `-")]
        [else (string-append (substring margin 0 (- (string-length margin) 2)) "  +-")]))

(define (display-padding margin out)
  (cond [(equal? margin "") (displayln "|" out)]
        [(equal? (substring margin (- (string-length margin) 2) (string-length margin)) "+-") (displayln (string-append (substring margin 0 (- (string-length margin) 2)) "| |") out)]
        [else (displayln (string-append (substring margin 0 (- (string-length margin) 2)) "  |") out)]))

(define (tree-display the-tree node-display [out (current-output-port)])
  (tree-display-aux "" the-tree node-display out))

(define (tree-display-aux margin the-tree node-display out)
  (if (null? (node-children the-tree))
      (begin (display margin out) (node-display the-tree out) (newline out))
      (begin (display margin out)
             (node-display the-tree out)
             (newline out)
             (map (λ (c) (begin (display-padding margin out) (tree-display-aux (next-margin margin #f) c node-display out))) (list-init (node-children the-tree)))
             (display-padding margin out)
             (tree-display-aux (next-margin margin #t) (last (node-children the-tree)) node-display out))))

(provide tree-display)
