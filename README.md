## 🌳 Advanced Suffix Tree Implementation in Racket

A high-performance, functional implementation of suffix trees in Racket, featuring both atomic and compact representations with streaming capabilities.

### 🚀 Features

- **Dual Tree Representations**
  - Atomic Suffix Tree (AST)
  - Compact Suffix Tree (CST)
  
- **Streaming Architecture**
  - Lazy evaluation for large-scale texts
  - Memory-efficient processing
  - Demand-driven branch construction

- **Pattern Matching**
  - Fast substring search
  - Longest common substring detection
  - Repeating pattern identification

- **Functional Abstractions**
  - Pure functional implementation
  - Tail-recursive optimizations
  - Collection-based operations

### 🛠️ Implementation Details

#### Core Components

1. **Base Suffix Tree Operations** (`suffix-tree.rkt`)
   - Tree construction primitives
   - Branch manipulation
   - Node traversal utilities

2. **Pattern Matching Engine** (`etapa1.rkt` & `etapa2.rkt`)
   - Common prefix computation
   - Pattern matching algorithms
   - Tree traversal strategies

3. **Streaming Architecture** (`suffix-tree-stream.rkt`)
   - Lazy tree construction
   - Stream-based node evaluation
   - Memory-efficient processing

4. **Collection Abstractions** (`collection.rkt`)
   - Generic collection operations
   - Stream-based implementations
   - Functional mapping and filtering

#### Key Algorithms

```racket
;; Pattern Matching
(define (substring? text pattern)
  (let* ([tree ((text->st ast-func) text)])  
    (st-has-pattern? tree pattern)))

;; Longest Common Substring
(define (longest-common-substring text1 text2)
  (let ([st1 (text->ast text1)])
    ...))

;; Repeating Substring Detection
(define (repeated-substring-of-given-length text len)
  ...)
```

### 📋 Usage Examples

```racket
;; Create a suffix tree from text
(define text (string->list "banana$"))
(define tree (text->ast text))

;; Check for pattern existence
(substring? text (string->list "ana")) ; Returns #t

;; Find longest common substring
(longest-common-substring 
  (string->list "banana") 
  (string->list "panama"))
; Returns '(#\a #\n #\a)
```

### 🔍 Architecture

The implementation follows a layered architecture:

```
Collection Abstractions
       ↓
Streaming Infrastructure
       ↓
Suffix Tree Core
       ↓
Pattern Matching
       ↓
Applications
```

### 🏃‍♂️ Performance Considerations

- Lazy evaluation prevents unnecessary tree construction
- Tail recursion optimization for deep tree traversals
- Stream-based processing for memory efficiency
- O(n) space complexity for compact representation
- O(m) time complexity for pattern matching (where m is pattern length)

### 📚 Further Reading

- [Suffix Trees: A Survey](https://www.cs.helsinki.fi/u/ukkonen/SuffixT1withFigs.pdf)
- [Ukkonen's Algorithm](https://www.cs.helsinki.fi/u/ukkonen/SuffixT1.pdf)
- [Applications in Bioinformatics](https://academic.oup.com/bioinformatics/article/25/11/1384/332905)

### 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### 📜 Licence
This project is licensed under the MIT Licence. See the [LICENCE](./LICENSE) file for further details.
