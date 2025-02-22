#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; We will adapt all functions from steps 1-3 (except longest-common-substring, 
;; which doesn't work with streams as it traverses the entire tree) to work 
;; with suffix tree (ST) streams instead of lists.
;; Since an ST is built from a collection of suffixes, and we want to avoid 
;; computing all suffixes upfront, we'll modify all functions that worked 
;; with lists of suffixes to work with suffix streams.

;; Note: Without this change, we'd only need to alter the suffixes->st function, 
;; which is the constructor for the ST type. However, by converting lists to 
;; suffix streams, more functions need modification.

;; An alternative would have been to use a collection of suffixes from the start 
;; (instead of assuming they'll be processed as lists). Instead of using cons, 
;; car, cdr, map, filter, etc., we would have used collection-cons, collection-first, etc.

; TODO: 
; - Modify functions to work with Collection type instead of lists, using the 
;   constructor and operators defined in collection.rkt.
; - Ensure abstraction barriers are respected for both ST and Collection types.

(define (longest-common-prefix w1 w2)
  (longest-help w1 w2 null w1 w2))

(define (longest-help w1 w2 w3 restw1 restw2)
  (if (or (null? w1) (null? w2))
      (append (list w3) (list restw1) (list restw2))
      (if (equal? (car w1) (car w2))
          (longest-help (cdr w1) (cdr w2) (append w3 (list (car w1))) (cdr restw1) (cdr restw2))
          (longest-help null null w3 restw1 restw2))))

; Modified function: replaced "list" with "collection"

(define (longest-common-prefix-of-collection words)
  (define (helper words prefix)
    (if (or (collection-empty? words)
            (collection-empty? (collection-rest prefix)))
        prefix
        (helper (collection-rest words)
                (first (longest-common-prefix prefix (collection-first words))))))
  (helper (collection-rest words) (collection-first words)))

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

(define (st-has-pattern? st pattern)
  (define match (match-pattern-with-label st pattern))
  (cond ((equal? match #t) #t)
        ((equal? (first match) #f) #f)
        (else (st-has-pattern? (third match) (second match)))))

(define (get-suffixes text)
  (if (null? text)
      empty-collection
      (collection-cons text (get-suffixes (cdr text)))))

(define (get-ch-words words-stream ch)
  (if (collection-empty? words-stream)              
      empty-collection                      
      (collection-filter                     
       (λ (word) (and (not (collection-empty? word)) (same-character word ch)))
       words-stream)))

(define (same-character word ch)
  (if (eq? (collection-first word) ch)
      #t
      #f))

(define (ast-func suffixes)
  (if (collection-empty? suffixes)
      null
      (cons (list (car (collection-first suffixes))) (stream-map cdr suffixes))))

(define (cst-func suffixes)
  (if (collection-empty? suffixes)
      null
      (let ((common-prefix (longest-common-prefix-of-collection suffixes)))
        (append (list common-prefix)
                (stream-map (λ (suffix) (drop suffix (length common-prefix)))
                            suffixes)))))

; Consider that the alphabet parameter is also a stream
; (of course, suffixes are a stream as well, being a collection
; of suffixes)
(define (suffixes->st labeling-func suffixes alphabet)
  (if (stream-empty? suffixes)
      empty-stream
      (let* ((by-first-char (collection-map (λ (ch) (get-ch-words suffixes ch)) alphabet))
             (useful-ch-lists (collection-filter (compose not null?) by-first-char))
             (branches (collection-map labeling-func useful-ch-lists)))
        (collection-filter (compose not null?) (collection-map (λ (branch) (if (null? branch)
                                                                               null
                                                                               (cons (get-branch-label branch)
                                                                                     (suffixes->st labeling-func (cdr branch) alphabet))))
                                                               branches)))))

; Don't forget to convert the alphabet to a stream
(define (text->st labeling-func)
  (λ (text)
    (let* ((text$ (append text '(#\$)))
           (suffixes (get-suffixes text$))
           (alphabet (sort (remove-duplicates text$) char<?))
           (col-alphabet (list->collection alphabet)))
      (suffixes->st labeling-func suffixes col-alphabet))))

(define text->ast
  (text->st ast-func))

(define text->cst
  (text->st cst-func))

; If you respected the abstraction barrier,
; this function remains unchanged.
(define (substring? text pattern)
  (let* ((text-to-st ((text->st ast-func) text))
         (result (st-has-pattern? text-to-st pattern)))
    (if (equal? result #t)
        #t
        #f)))

; If you respected the abstraction barrier,
; this function remains unchanged.
(define (repeated-substring-of-given-length text len)
  (let loop ((st (text->cst text)) (need-len len) (result '()))
    (cond ((st-empty? st) #f)
          ((<= need-len 0) (take result len))
          (else
           (let* ((branch (first-branch st)) (label (get-branch-label branch)) (subtree (get-branch-subtree branch)))
             (or (loop subtree (- need-len (length label)) (append result label))
                 (loop (other-branches st) need-len result)))))))
