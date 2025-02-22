## Advanced Suffix Tree Implementation in Racket

A comprehensive, purely functional implementation of suffix trees in Racket, featuring both atomic (AST) and compact (CST) representations across four development stages. This implementation emphasises pattern matching, text analysis, streaming capabilities, and advanced functional programming concepts.

### 📑 Project Stages Overview

##### Stage 1: Core Implementation & Pattern Matching
- Basic suffix tree representation
- Core tree operations
- Pattern matching primitives
- Text analysis foundations

##### Stage 2: Tree Construction & Optimisation
- Atomic Suffix Tree (AST) implementation
- Compact Suffix Tree (CST) implementation
- Tree construction algorithms
- Performance optimisation

##### Stage 3: Advanced Pattern Matching
- Complex substring search operations
- Longest common substring detection
- Repeated substring identification
- Let-binding optimisations

##### Stage 4: Streaming & Collection Abstractions
- Stream-based tree construction
- Lazy evaluation implementation
- Collection type abstractions
- Memory optimisation techniques

### 🎯 Core Features

##### Base Tree Structure
The suffix tree is represented as a list of branches, where each branch contains:
- A label (list of characters)
- A subtree (another suffix tree)

Example Tree (for "BANANA$"):
```racket
'(((#\$))                          ; Branch 1: "$"
  ((#\A)                           ; Branch 2: "A"
       ((#\$))                     ; Branch 2.1: "A$"
       ((#\N #\A)                  ; Branch 2.2: "NA"
            ((#\$))                ; Suffix "ANA$"
            ((#\N #\A #\$))))      ; Suffix "ANANA$"
  ((#\B #\A #\N #\A #\N #\A #\$)) ; Branch 3: "BANANA$"
  ((#\N #\A)                      ; Branch 4: "NA"
       ((#\$))                     ; Suffix "NA$"
       ((#\N #\A #\$))))          ; Suffix "NANA$"
```

##### Core Operations (Stage 1)
```racket
; Base Tree Operations
(define (first-branch st)           ; Get first branch
(define (other-branches st)         ; Get remaining branches
(define (get-branch-label branch)   ; Extract branch label
(define (get-branch-subtree branch) ; Extract branch subtree
(define (get-ch-branch st ch)      ; Find branch starting with ch

; Pattern Matching
(define (longest-common-prefix w1 w2)         ; Get LCP of two words
(define (longest-common-prefix-of-list words) ; Get LCP of word list
(define (match-pattern-with-label st pattern) ; Match pattern in ST
(define (st-has-pattern? st pattern)         ; Check if pattern exists
```

##### Tree Construction (Stage 2)

###### Atomic Suffix Tree (AST)
- Single character edge labels
- Maximum number of edges
- Optimised for pattern matching
```racket
(define text->ast (text->st ast-func))
```

###### Compact Suffix Tree (CST)
- Longest common prefix edge labels
- Minimum number of edges
- Space-efficient representation
```racket
(define text->cst (text->st cst-func))
```

##### Advanced Pattern Matching (Stage 3)
```racket
; Substring search with let-binding optimisation
(define (substring? text pattern)
  (let* ([tree ((text->st ast-func) text)])
    (st-has-pattern? tree pattern)))

; Longest common substring detection
(define (longest-common-substring text1 text2)
  (let* ([st1 (text->ast text1)]
         [st2 (text->ast text2)])
    (letrec ([find-common (lambda (node1 node2)
                           ... ))])))

; Repeated substring finding
(define (repeated-substring-of-given-length text len)
  (let ([st (text->ast text)])
    ...))
```

##### Streaming Architecture (Stage 4)
- Lazy tree construction
- Stream-based node evaluation
- Memory-efficient processing
```racket
; Stream-based suffix collection
(define (make-suffix-stream text)
  (stream-cons (car text)
               (make-suffix-stream (cdr text))))

; Collection type abstraction
(define-type Collection
  (λ (suffixes)
    ...))
```

### 🔍 Implementation Details

##### Functional Abstractions
- Pure functional implementation
- Tail-recursive optimisations
- Higher-order functions
- Currying patterns
- Let-binding optimisations

##### Performance Considerations
- O(n) space complexity for construction
- O(m) time complexity for pattern matching
- Lazy evaluation prevents unnecessary computation
- Stream-based processing for memory efficiency
- Tail recursion optimisation for deep traversals

### 📊 Usage Examples

##### Basic Pattern Matching
```racket
(substring? "banana" "ana") ; Returns #t
```

##### Suffix Analysis
```racket
(get-suffixes '(#\w #\h #\y #\$))
; ⇒ '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
```

##### Advanced Operations
```racket
; Find longest common substring
(longest-common-substring 
  (string->list "banana") 
  (string->list "panama"))
; Returns '(#\a #\n #\a)

; Stream-based processing
(define text-stream (make-suffix-stream text))
(stream-ref text-stream 0) ; Get first suffix
```

### 🤝 Contributing
Contributions welcome in areas such as:
- Additional tree operations
- Performance optimisations
- New text analysis features
- Documentation improvements
- Stream processing enhancements

### 📜 Licence
This project is licensed under the MIT Licence. See the [LICENCE](./LICENSE) file for further details.
