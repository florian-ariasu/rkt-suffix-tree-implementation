#lang racket

(provide (all-defined-out))

;; In this file, you define the constructors and operators for the Collection type.
;; In previous stages, collections were implemented as lists.
;; In the definitions below, consider a collection implemented as a stream.

;; Since stream-cons is not a regular function but a special syntax that
;; does not evaluate its arguments before the call (which is the behavior 
;; we want for collection-cons), we cannot use the definition:
;;    (define collection-cons stream-cons)
;; This would generate an error.
;; Similarly, defining it as:
;;    (define (collection-cons x xs) (stream-cons x xs))
;; is not a solution, as Racket functions are strict, and x and xs 
;; would be evaluated before entering the function body.
;; The correct way to define collection-cons to replicate the behavior of 
;; stream-cons is:

;; Note: You can change the function name if you prefer something else. 
;; This function is only used by you in the file etapa4.rkt, not by the checker.

;; TODO
;; Continue writing the rest of the definitions (those that do not require special syntax).

(define (collection-first l)
  (stream-first l))
(define (collection-rest l)
  (stream-rest l))
(define (collection-map func l)
  (stream-map func l))
(define (collection-filter func l)
  (stream-filter func l))
(define (collection-empty? f)
  (stream-empty? f))
(define empty-collection
  empty-stream)
(define (collection-cons elem l)
  (stream-cons elem l))

(define (list->collection lst)
  (if (null? lst)
      empty-stream
      (stream-cons (car lst) (list->collection (cdr lst)))))
