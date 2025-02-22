#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")

(provide (all-defined-out))

;; In this stage, we define the algorithm for constructing
;; a suffix tree (both compact and atomic) based on a text
;; and a given alphabet. The text contains only symbols from the alphabet.
;; 
;; Approach:
;; 1. Obtain all suffixes of the text.
;; 2. For each character in the alphabet, find suffixes
;;    starting with that character. They will form branches of the tree.
;; 3. For each group of suffixes starting with the same character, 
;;    determine the branch label and the new suffixes:
;;    - For an AST, the label is the first character, and new suffixes
;;      are the old ones without the first character.
;;    - For a CST, the label is the longest common prefix, and 
;;      new suffixes are the old ones without this prefix.
;; 4. Transform each result into a branch:
;;    - The label is already computed.
;;    - Repeat steps 2-4 for new suffixes.

;; TODO 1: Implement a recursive function that takes a text 
;; (list of characters) and returns the list of all suffixes 
;; (from the longest to the shortest). The text ends with the 
;; special character "$", and all suffixes should end with "$".

(define (get-suffixes text)
  (if (null? text)
      '()
      (cons text (get-suffixes (cdr text)))))

;; TODO 2: Implement a function that receives a list of words
;; and a character `ch` and returns the words starting with `ch`.
;; Use functional programming (no explicit recursion).

(define (get-ch-words words ch)
  (filter (λ (word) (and (not (null? word)) (char=? (car word) ch)))
          words))

;; TODO 3: Implement a function that takes a non-empty list of suffixes 
;; starting with the same character and calculates the pair (label AST, 
;; list of new suffixes). The label for AST is the first character.

(define (ast-func suffixes)
  (cons (list (caar suffixes)) (map cdr suffixes)))

;; TODO 4: Implement a function that computes the pair (label CST, 
;; list of new suffixes) for suffixes starting with the same character.
;; The label for CST is the longest common prefix.

(define (cst-func suffixes)
  (let ([common-prefix (longest-common-prefix-of-list suffixes)])
    (cons common-prefix
          (map (λ (suffix) (drop suffix (length common-prefix))) suffixes))))

;; TODO 5: Implement the suffixes->st function that constructs a suffix tree
;; from a list of suffixes, an alphabet, and a labeling function.
;; It can generate both AST and CST based on the labeling function.

(define (suffixes->st labeling-func suffixes alphabet)
  (if (null? suffixes)
      empty-st
      (let* ([grouped-suffixes (group-suffixes-by-alphabet suffixes alphabet)]
             [non-empty-groups (filter (λ (group) (not (null? (cdr group))))
                                       grouped-suffixes)]
             [branches (map (λ (group) 
                              (let ([branch (labeling-func (cdr group))])
                                (cons (car branch)
                                      (suffixes->st labeling-func (cdr branch) alphabet))))
                            non-empty-groups)])
        branches)))

(define (group-suffixes-by-alphabet suffixes alphabet)
  (map (λ (ch) 
         (cons ch (get-ch-words suffixes ch)))
       alphabet))

;; TODO 6: Implement three functions: text->st, text->ast, text->cst.
;; These derive from text->st using partial application.
;; 
;; a) Implement text->st, which takes a text and a labeling function
;;    and returns the corresponding suffix tree.

(define (text->st labeling-func)
  (λ (text)
    (let* ([text-with-terminator (append text '(#\$))]
           [suffixes (generate-suffixes text-with-terminator)]
           [alphabet (create-sorted-alphabet text-with-terminator)])
      (build-suffix-tree labeling-func suffixes alphabet))))

(define (generate-suffixes text)
  (if (null? text)
      '()
      (cons text (generate-suffixes (cdr text)))))

(define (create-sorted-alphabet text)
  (sort (remove-duplicates text) char<?))

(define (build-suffix-tree labeling-func suffixes alphabet)
  (if (null? suffixes)
      empty-st
      (let* ([grouped-suffixes (group-by-first-char suffixes alphabet)]
             [non-empty-groups (filter (λ (group) (not (null? (cdr group))))
                                       grouped-suffixes)]
             [branches (map (λ (group)
                              (let ([branch (labeling-func (cdr group))])
                                (cons (car branch)
                                      (build-suffix-tree labeling-func (cdr branch) alphabet))))
                            non-empty-groups)])
        branches)))

(define (group-by-first-char suffixes alphabet)
  (map (λ (ch)
         (cons ch (filter (λ (suffix) 
                            (and (not (null? suffix)) 
                                 (char=? (car suffix) ch))) 
                          suffixes)))
       alphabet))

;; b) Derive text->ast, which returns the AST for a text.

(define text->ast
  (text->st ast-func))

;; c) Derive text->cst, which returns the CST for a text.

(define text->cst
  (text->st cst-func))
