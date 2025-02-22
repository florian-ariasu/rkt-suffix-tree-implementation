#lang racket
(provide (all-defined-out))

;; In previous stages, we worked with the suffix tree representation
;; as a list of branches, where each branch was a pair of the first
;; edge label and the subtree under it. For the text "banana," the 
;; compact representation of the suffix tree (with strings for clarity) 
;; was:
;; '("$")
;;  ("a" ("$") ("na" ("$") ("na$")))
;;  ("banana$")
;;  ("na" ("$") ("na$"))
;;
;; For applications like pattern matching or finding the shortest
;; repeating substring, partial exploration of the suffix tree is sufficient, 
;; which benefits from a "lazy" construction — only calculating the necessary 
;; branches to the required depth. For example, to find a repeating 
;; substring of length 3 in "banana," only a few edges need to be explored:
;; '("$")   - this path fails
;;  ("a" ("$")) - this path fails
;;  ("na" <promise>) - the subtree under the "na" label is a non-empty stream
;;
;; When we find that the "na" node is internal and discover that the string 
;; "ana" (corresponding to the root path) has a length of 3, the function 
;; returns "ana" without building the rest of the tree (saving time on long texts).
;;
;; To achieve this, we change the representation so that the suffix tree 
;; becomes a stream (not a list) of branches. Each branch remains a pair 
;; (the first edge label and the subtree, which is also a stream, etc.). 
;; Texts and labels remain lists (not streams) of characters.

;; Now, we will redefine the suffix tree manipulation library 
;; to account for this change in representation.

;; TODO 1
;; Redefine the following constructors and operators of the ST data structure. 
;; Some definitions may remain the same as in stage 1.

; Empty suffix tree
(define empty-st empty-stream)

; Checks if an ST is empty
(define (st-empty? ST) (stream-empty? ST))

; Extracts the first branch of an ST
(define (first-branch ST)
  (if (stream-empty? ST)
      empty-stream
      (stream-first ST)))

; Extracts the rest of the branches of an ST (excluding the first one)
(define (other-branches ST)
  (if (stream-empty? ST)
      empty-stream
      (stream-rest ST)))

; Extracts the label from the top of a branch
(define (get-branch-label branch)
  (if (null? branch)
      '() 
      (car branch)))

; Extracts the subtree under the branch label
(define (get-branch-subtree branch)
  (if (null? branch)
      '() 
      (cdr branch)))

; Identifies the branch of an ST whose label starts with the character ch, 
;; if such a branch exists
(define (get-ch-branch st ch)
  (if (st-empty? st)
      #f
      (if (equal? ch (car (get-branch-label (first-branch st))))
          (first-branch st)
          (get-ch-branch (other-branches st) ch))))
