#lang racket

(provide (all-defined-out))

;; A suffix tree is a tree that contains all suffixes of a text T (usually a long text requiring multiple processing),
;; as shown in the figure below (the suffix tree for the text "BANANA", where nodes are represented by *, and edges are labeled with substrings of T).

;; Each path in the suffix tree corresponds to a substring of T, and each complete path corresponds to a suffix.
;; The suffixes are terminated with a special character '$' to avoid "losing" suffixes that are prefixes of others.

;; The suffix tree above is compact, meaning it uses the minimum number of edges, with each edge labeled by the longest common prefix of the suffixes below.
;; In contrast, an atomic suffix tree uses the maximum number of edges, with each edge labeled by a single character.

;; Regardless of the representation used, a suffix tree is represented as a list of branches, where each branch is a pair of the first edge label and the subtree beneath it.
;; For the text "banana", the compact suffix tree representation is:

;; '(("$")
;;   ("a" ("$")
;;        ("na" ("$")
;;              ("na$")))
;;   ("banana$")
;;   ("na" ("$")
;;         ("na$")))

;; For clarity, we use strings in the representation. In reality, we store each string as a list of characters, making the actual representation:

;; '(((#\$))
;;   ((#\a) ((#\$))
;;          ((#\n #\a) ((#\$))
;;                     ((#\n #\a #\$))))
;;   ((#\b #\a #\n #\a #\n #\a #\$))
;;   ((#\n #\a) ((#\$))
;;              ((#\n #\a #\$))))

;; You will now define a library for manipulating suffix trees.

;; TODO 1
;; Define the following constructors and operators for the suffix tree data structure (ST).

;; empty suffix tree
(define empty-st '())

;; operator to check if a ST is empty
(define (st-empty? ST)
  (if (null? ST)
      #t
      #f))

;; operator to extract the first branch of a ST
(define (first-branch ST)
  (if (null? ST)
      empty-st
      (car ST)))

;; operator to extract the remaining branches of a ST (excluding the first)
(define (other-branches ST)
  (if (null? ST)
      empty-st
      (cdr ST)))

;; operator to extract the label of the first branch
(define (get-branch-label branch)
  (if (null? branch)
      '()
      (car branch)))

;; operator to extract the subtree under a branch's label
(define (get-branch-subtree branch)
  (if (null? branch)
      '()
      (cdr branch)))

;; operator to find a branch in the ST whose label starts with character ch
(define (get-ch-branch st ch)
  (if (null? st)
      #f
      (if (equal? ch (car (get-branch-label (first-branch st))))
          (first-branch st)
          (get-ch-branch (other-branches st) ch))))
