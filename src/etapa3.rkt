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

; TODO 1
; Implement the substring? function, which takes a text and a non-empty pattern and returns true if the pattern is found in the text, or false otherwise.
; The core logic was already implemented in stage 1 in the function st-has-pattern?, which is an operator for the ST type.
; Now, we have all the necessary tools to implement the operator for the text type, as the suffix tree construction was implemented in stage 2.

(define (substring? text pattern)
  (let* ((tree ((text->st ast-func) text)))  
    (st-has-pattern? tree pattern)))  

; TODO 2
; Implement the longest-common-substring function that takes two texts and finds their longest common substring.
; 1. Build the suffix tree ST1 for the first text.
; 2. For each suffix of the second text (from longest to shortest), find the longest match with suffixes in the first text, following the relevant paths in ST1.
; 3. The final result is the longest match found (if there are ties, the first found string is kept).
; Use named let to process the suffixes.
; Note: Do not include the final $ character for the suffixes of the second text to avoid artificially increasing the length of the common substring.

(define (longest-common-substring text1 text2)
  (let ((st1 (text->ast text1)))
    (define (find-longest-match pattern)
      (let process-pattern ((remaining pattern)
                          (current-st st1)
                          (acc '()))
        (if (null? remaining)
            acc  
            (let ((match-result (match-pattern-with-label current-st remaining)))
              (cond
                ((eq? match-result #t)  
                 (append acc remaining))
                ((eq? (car match-result) #f)  
                 (append acc (cadr match-result)))
                (else  ; Continue matching in subtree
                 (process-pattern (cadr match-result)      
                                  (caddr match-result)       
                                  (append acc (car match-result)))))))))  
    
    (let process-suffixes ((pos 0)
                          (best-match '()))
      (if (= pos (length text2))
          best-match  ; Return the best match found
          (let* ((current-suffix (drop text2 pos))
                 (current-match (find-longest-match current-suffix)))
            (if (> (length current-match) (length best-match))
                (process-suffixes (+ pos 1) current-match)
                (process-suffixes (+ pos 1) best-match)))))))

; TODO 3
; Implement the repeated-substring-of-given-length function that takes a text and a natural number len and traverses the suffix tree of the text to find a substring of length len that repeats in the text.
; If such a substring does not exist, the function returns false.
; The result will be the first such substring alphabetically, as the suffix tree is built based on the sorted alphabet.
; Any path in the compact suffix tree ending with an internal node (a node with children, not a leaf) represents a repeated substring.

(define (repeated-substring-of-given-length text len)
  (define (is-internal-node? tree)
    (and (not (st-empty? tree))
         (not (st-empty? (other-branches tree)))))
  
  (define (get-path-to-node tree acc)
    (if (st-empty? tree)
        acc
        (let ((branch (first-branch tree)))
          (cons (get-branch-label branch)
                (get-path-to-node (get-branch-subtree branch) acc)))))
  
  (define (find-repeated-substring tree path-length current-path)
    (if (st-empty? tree)
        #f
        (let* ((branch (first-branch tree))
               (label (get-branch-label branch))
               (subtree (get-branch-subtree branch))
               (new-path (append current-path label))
               (new-length (+ path-length (length label))))
          
          (cond 
            ((and (is-internal-node? subtree)
                  (>= new-length len))
             (take new-path len))
            
            ((< new-length len)
             (or (find-repeated-substring subtree new-length new-path)
                 (find-repeated-substring (other-branches tree) path-length current-path)))
            
            (else
             (find-repeated-substring (other-branches tree) path-length current-path))))))

  (find-repeated-substring (text->cst text) 0 '()))
