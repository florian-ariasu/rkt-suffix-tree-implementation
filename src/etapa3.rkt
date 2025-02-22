#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; This stage is dedicated to applications of the suffix tree:
;; - Finding a pattern in a text
;; - Longest common substring of two texts
;; - Finding a substring of a given length that repeats in the text
;; A text is always represented as a list of characters.
;; The results of the functions below are also represented as lists of characters.

;; TODO 1: Implement the substring? function.
;; This function takes a text and a non-empty pattern and returns #t if the pattern is found in the text, #f otherwise.
;; It utilizes the function st-has-pattern? from etapa1.rkt, operating on the suffix tree representation of the text.

(define (substring? text pattern)
  (let* ([tree ((text->st ast-func) text)])  
    (st-has-pattern? tree pattern)))

;; TODO 2: Implement the longest-common-substring function.
;; This function takes two texts and finds their longest common substring.
;; 1. Construct the suffix tree for text1.
;; 2. Iterate over the suffixes of text2, finding the longest match in text1.
;; 3. Return the longest match found.

(define (longest-common-substring text1 text2)
  (let ([st1 (text->ast text1)])
    (define (find-longest-match pattern)
      (let process-pattern ([remaining pattern] [current-st st1] [acc '()])
        (if (null? remaining)
            acc  
            (let ([match-result (match-pattern-with-label current-st remaining)])
              (cond
                [(eq? match-result #t) (append acc remaining)]
                [(eq? (car match-result) #f) (append acc (cadr match-result))]
                [else  
                 (process-pattern (cadr match-result) (caddr match-result) (append acc (car match-result)))])
              ))))
    
    (let process-suffixes ([pos 0] [best-match '()])
      (if (= pos (length text2))
          best-match  
          (let* ([current-suffix (drop text2 pos)]
                 [current-match (find-longest-match current-suffix)])
            (if (> (length current-match) (length best-match))
                (process-suffixes (+ pos 1) current-match)
                (process-suffixes (+ pos 1) best-match)))))))

;; TODO 3: Implement the repeated-substring-of-given-length function.
;; This function takes a text and a length len, then finds a repeating substring of that length in the text.
;; If no such substring exists, the function returns #f.
;; The function ensures that the returned substring is the lexicographically first one.

(define (repeated-substring-of-given-length text len)
  (define (is-internal-node? tree)
    (and (not (st-empty? tree))
         (not (st-empty? (other-branches tree)))))
  
  (define (get-path-to-node tree acc)
    (if (st-empty? tree)
        acc
        (let ([branch (first-branch tree)])
          (cons (get-branch-label branch)
                (get-path-to-node (get-branch-subtree branch) acc)))))
  
  (define (find-repeated-substring tree path-length current-path)
    (if (st-empty? tree)
        #f
        (let* ([branch (first-branch tree)]
               [label (get-branch-label branch)]
               [subtree (get-branch-subtree branch)]
               [new-path (append current-path label)]
               [new-length (+ path-length (length label))])
          
          (cond 
            [(and (is-internal-node? subtree) (>= new-length len))
             (take new-path len)]
            
            [(< new-length len)
             (or (find-repeated-substring subtree new-length new-path)
                 (find-repeated-substring (other-branches tree) path-length current-path))]
            
            [else
             (find-repeated-substring (other-branches tree) path-length current-path)]))))
  
  (find-repeated-substring (text->cst text) 0 '()))
