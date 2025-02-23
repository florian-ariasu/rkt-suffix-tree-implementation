## Suffix Tree Implementation in Racket

### Overview

A comprehensive, purely functional implementation of suffix trees in Racket, providing both Atomic (AST) and Compact (CST) suffix tree representations. This implementation focuses on pattern matching, text analysis, streaming capabilities, and advanced functional programming paradigms.

### Implementation Stages

#### Stage 1: Core Implementation & Pattern Matching
- Suffix tree base representation
- Fundamental tree operations
- Pattern matching primitives
- Text analysis foundations

#### Stage 2: Tree Construction & Optimisation
- Atomic Suffix Tree (AST) implementation
- Compact Suffix Tree (CST) implementation
- Tree construction algorithms
- Performance optimisation strategies

#### Stage 3: Advanced Pattern Matching
- Complex substring search operations
- Longest common substring detection
- Repeated substring identification
- Let-binding optimisations

#### Stage 4: Streaming & Collection Abstractions
- Stream-based tree construction
- Lazy evaluation implementation
- Collection type abstractions
- Memory optimisation techniques

### Technical Specification

#### Base Tree Structure
The suffix tree is implemented as a list of branches, where each branch contains:
- A label (list of characters)
- A subtree (recursive suffix tree structure)

Example Tree Structure (for "BANANA$"):
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

#### Core Operations

##### Base Tree Operations
```racket
(define (first-branch st)           ; Returns the first branch
(define (other-branches st)         ; Returns remaining branches
(define (get-branch-label branch)   ; Extracts branch label
(define (get-branch-subtree branch) ; Extracts branch subtree
(define (get-ch-branch st ch)      ; Finds branch starting with ch
```

##### Pattern Matching Operations
```racket
(define (longest-common-prefix w1 w2)         ; Returns LCP of two words
(define (longest-common-prefix-of-list words) ; Returns LCP of word list
(define (match-pattern-with-label st pattern) ; Matches pattern in ST
(define (st-has-pattern? st pattern)         ; Verifies pattern existence
```

#### Tree Construction

##### Atomic Suffix Tree (AST)
Implementation optimised for pattern matching with single-character edge labels:
```racket
(define text->ast (text->st ast-func))
```

##### Compact Suffix Tree (CST)
Space-efficient implementation using longest common prefix edge labels:
```racket
(define text->cst (text->st cst-func))
```

#### Advanced Pattern Matching Implementation
```racket
(define (substring? text pattern)
  (let* ([tree ((text->st ast-func) text)])
    (st-has-pattern? tree pattern)))

(define (longest-common-substring text1 text2)
  (let* ([st1 (text->ast text1)]
         [st2 (text->ast text2)])
    (letrec ([find-common (lambda (node1 node2)
                           ... ))])))

(define (repeated-substring-of-given-length text len)
  (let ([st (text->ast text)])
    ...))
```

#### Streaming Architecture
```racket
(define (make-suffix-stream text)
  (stream-cons (car text)
               (make-suffix-stream (cdr text))))

(define-type Collection
  (λ (suffixes)
    ...))
```

### Technical Details

#### Implementation Characteristics
- Pure functional implementation
- Tail-recursive optimisation
- Higher-order function utilisation
- Currying pattern implementation
- Let-binding optimisation

#### Performance Analysis
- Construction: O(n) space complexity
- Pattern matching: O(m) time complexity
- Lazy evaluation for computation optimisation
- Stream-based processing for memory efficiency
- Tail recursion optimisation for deep traversals

### Usage Examples

#### Pattern Matching
```racket
(substring? "banana" "ana") ; Returns #t
```

#### Suffix Analysis
```racket
(get-suffixes '(#\w #\h #\y #\$))
; Returns '((#\w #\h #\y #\$) (#\h #\y #\$) (#\y #\$) (#\$))
```

#### Advanced Operations
```racket
(longest-common-substring 
  (string->list "banana") 
  (string->list "panama"))
; Returns '(#\a #\n #\a)

(define text-stream (make-suffix-stream text))
(stream-ref text-stream 0) ; Returns first suffix
```

### Contributing

#### Development Process
1. Fork the repository
2. Create a feature branch
3. Implement changes with tests
4. Submit a pull request

Areas for contribution:
- Tree operation optimisation
- Performance enhancement
- Text analysis feature expansion
- Stream processing improvement

### Licence

This project is licensed under the MIT Licence. See [LICENCE](./LICENSE) for complete details.
