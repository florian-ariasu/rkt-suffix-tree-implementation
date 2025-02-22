#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implement a function that takes two words (lists of characters) w1 and w2
; and computes their longest common prefix, along with the remainder of both words
; after removing the common prefix.
; Example:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; Expected output:
; '((#\w #\h) (#\y) (#\e #\n))
; Use tail recursion.

(define (longest-common-prefix w1 w2)
  (longest-help w1 w2 null w1 w2))

(define (longest-help w1 w2 w3 restw1 restw2)
  (if (or (null? w1) (null? w2))
      (append (list w3) (list restw1) (list restw2))
      (if (equal? (car w1) (car w2))
          (longest-help (cdr w1) (cdr w2) (append w3 (list (car w1))) (cdr restw1) (cdr restw2))
          (longest-help null null w3 restw1 restw2))))

; TODO 3
; Implement a recursive function that takes a non-empty list of words starting 
; with the same character and calculates the longest common prefix of these words. 
; Stop searching once the current common prefix is guaranteed to be the final common prefix.

(define (longest-common-prefix-of-list words)
 
  (define (helper words prefix)
    (if (or (null? words)
            (null? (cdr prefix)))
        prefix
        (helper (cdr words)
                (first (longest-common-prefix prefix (car words))))))
 
  (helper (cdr words) (car words)))

; TODO 4
; Implement the match-pattern-with-label function that receives a suffix tree 
; and a non-empty pattern. It performs one step of the pattern matching process: 
; identifies the tree branch whose label starts with the first character of the pattern 
; and determines how well the pattern matches the label, returning:
; - true if the pattern is entirely contained in the label.
; - A list (label, new-pattern, subtree) if the pattern matches the label but is not fully contained in it.
; - A list (false, longest-common-prefix) if no match is found or no label starting with the desired letter is found.

(define (match-pattern-with-label st pattern)
  (define branch (get-ch-branch st (car pattern)))
 
  (if (not branch)
      (list #f '())
      (let* ((label (get-branch-label branch))
             (subtree (get-branch-subtree branch))
             (lcp (longest-common-prefix label pattern))
             (prefix (first lcp))
             (new-label (second lcp))
             (new-pattern (third lcp)))
        (cond ((null? new-pattern) #t)
              ((null? new-label) (list prefix new-pattern subtree))
              (else (list #f prefix))))))

; TODO 5
; Implement the st-has-pattern? function that takes a suffix tree and a pattern 
; and returns true if the pattern appears in the tree, or false otherwise.

(define (st-has-pattern? st pattern)
  (define match (match-pattern-with-label st pattern))
  (cond ((equal? match #t) #t)
        ((equal? (first match) #f) #f)
        (else (st-has-pattern? (third match) (second match)))))
