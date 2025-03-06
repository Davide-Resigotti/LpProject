;; -------------------------------------------------------------------------------
;; Utility Functions (Basic Building Blocks)
;; -------------------------------------------------------------------------------

;; Function: Returns the weight of a node (leaf or internal node).
(defun node-weight (node)
  (case (car node)
    (leaf (cadr node))
    (node (caddr node))
    (otherwise (error "Invalid node type: ~S" node))))

;; Function: Sorts nodes based on weight.
(defun sort-nodes (nodes)
  (sort nodes #'< :key #'node-weight))

;; Function: Inserts a node maintaining sort order by weight.
(defun insert-sorted (node nodes)
  (let ((weight (node-weight node)))
    (cond
      ((null nodes) (list node))
      ((<= weight (node-weight (car nodes))) (cons node nodes))
      (t (cons (car nodes) (insert-sorted node (cdr nodes)))))))

;; Function: Merges two nodes into a new internal node.
;; The node with the lower weight goes to the left.
(defun merge-nodes (node1 node2)
  (let ((w1 (node-weight node1))
        (w2 (node-weight node2)))
    (if (<= w1 w2)
        (list 'node node1 node2 (+ w1 w2))
        (list 'node node2 node1 (+ w1 w2)))))

;; Function: Writes bits to a stream (space-separated).
(defun write-bits-to-stream (stream bits)
  (loop for bit in bits do
        (write stream bit)
        (write stream #\Space)))

;; Function: Writes symbols to a stream.
(defun write-symbols-to-stream (stream symbols)
  (loop for symbol in symbols do
        (write stream symbol)))

;; Function: Splits a string by spaces (without using cl-ppcre).
(defun split-string-by-space (string)
  (loop with result = nil
        with start = 0
        do (let ((space-pos (position #\Space string :start start)))
             (if space-pos
                 (progn
                   (push (subseq string start space-pos) result)
                   (setf start (1+ space-pos)))
                 (progn
                   (push (subseq string start) result)
                   (return (reverse result)))))))


;; -------------------------------------------------------------------------------
;; Helper Functions (Using Basic Utilities)
;; -------------------------------------------------------------------------------

;; Function: Creates a list of leaf nodes from symbol-weight pairs.
(defun create-initial-nodes (symbols-n-weights)
  (mapcar (lambda (sw) (list 'leaf (car sw) (cadr sw))) symbols-n-weights))

;; Function: Recursively generates the symbol-bits table.
(defun generate-symbol-bits-table-recursive (tree bits)
  (case (car tree)
    (leaf (list (list 'sw (cadr tree) bits)))
    (node (append (generate-symbol-bits-table-recursive (cadr tree) (append bits '(0)))
                  (generate-symbol-bits-table-recursive (caddr tree) (append bits '(1)))))))

;; Function: Encodes symbols using the symbol-bits table.
(defun encode-symbols (symbols symbol-bits-table)
  (if (null symbols)
      nil
      (let ((symbol (car symbols))
            (rest-symbols (cdr symbols)))
        (let ((symbol-bits (find-symbol-bits symbol symbol-bits-table)))
          (if symbol-bits
              (append symbol-bits (encode-symbols rest-symbols symbol-bits-table))
              (error "Symbol not found in symbol-bits table: ~S" symbol))))))

;; Function: Finds the bit sequence for a symbol in the symbol-bits table.
(defun find-symbol-bits (symbol symbol-bits-table)
  (dolist (sw-pair symbol-bits-table nil)
    (when (eq (cadr sw-pair) symbol)
      (return (caddr sw-pair)))))

;; Function: Recursively decodes a symbol from the bit stream.
(defun decode-symbol (bits current-node root-tree)
  (if (null bits)
      (values nil bits)
      (case (car current-node)
        (leaf (values (cadr current-node) bits))
        (node (let ((bit (car bits))
                    (rest-bits (cdr bits)))
                (if (= bit 0)
                    (decode-symbol rest-bits (cadr current-node) root-tree)
                    (decode-symbol rest-bits (caddr current-node) root-tree))))
        (otherwise (error "Invalid node type in decode-symbol: ~S" current-node)))) )


;; Function: Recursively decodes bits to a message.
(defun decode-bits (bits huffman-tree)
  (if (null bits)
      nil
      (multiple-value-bind (symbol remaining-bits)
          (decode-symbol bits huffman-tree huffman-tree) ; pass root tree as current node initially
        (cons symbol (decode-bits remaining-bits huffman-tree)))))

;; Function: Reads symbols from a file.
(defun hucodec-read-symbols (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      (coerce 'list content)))) ; Read file content as list of characters

;; Function: Writes bits to a file.
(defun hucodec-write-bits (filename bits)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-bits-to-stream stream bits)))

;; Modified Function: Reads bits from a file (space-separated integers) without cl-ppcre.
(defun hucodec-read-bits (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((bit-string-line (read-line stream nil nil))) ; Read the entire line as a string, handling EOF
      (if bit-string-line ; Check if line was read successfully (not EOF)
          (mapcar #'parse-integer (split-string-by-space bit-string-line)) ; Split and parse if line exists
          nil)))) ; Return nil if file is empty or EOF is reached immediately


;; Function: Writes symbols to a file.
(defun hucodec-write-symbols (filename symbols)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-symbols-to-stream stream symbols)))


;; -------------------------------------------------------------------------------
;; Main Functions (High-Level Operations)
;; -------------------------------------------------------------------------------

;; Function: Generates the Huffman tree from a list of symbol-weight pairs.
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (if (null symbols-n-weights)
      (error "Errore: la lista di simboli e pesi è vuota.")
      (let* ((initial-nodes (create-initial-nodes symbols-n-weights))
             (sorted-nodes (sort-nodes initial-nodes)))
        (build-huffman-tree sorted-nodes))))

;; Function: Recursively builds the Huffman tree.
;; Base case: a single node remains -> it's the complete tree.
(defun build-huffman-tree (nodes)
  (if (null (cdr nodes)) ; Check if only one node left (cdr is nil if list of length 1)
      (car nodes)
      (let* ((n1 (car nodes))
             (n2 (cadr nodes))
             (rest-nodes (cddr nodes))
             (merged-node (merge-nodes n1 n2)))
        (build-huffman-tree (insert-sorted merged-node rest-nodes)))))


;; Function: Generates the symbol-bits table from a Huffman tree.
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (generate-symbol-bits-table-recursive huffman-tree nil))

;; Function: Encodes a message using a Huffman tree.
(defun hucodec-encode (symbols huffman-tree)
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (encode-symbols symbols symbol-bits-table)))

;; Function: Decodes bits to a message using a Huffman tree.
(defun hucodec-decode (bits huffman-tree)
  (decode-bits bits huffman-tree))

;; Function: Encodes a file using Huffman coding.
(defun hucodec-encode-file (input-filename huffman-tree output-filename)
  (let ((symbols (hucodec-read-symbols input-filename)))
    (if (null symbols)
        (error "Errore: il file di input è vuoto.")
        (let ((bits (hucodec-encode symbols huffman-tree)))
          (hucodec-write-bits output-filename bits)))))

;; Function: Decodes a file encoded with Huffman coding.
(defun hucodec-decode-file (huffman-tree input-encoded-filename output-decoded-filename)
  (let ((bits (hucodec-read-bits input-encoded-filename)))
    (let ((message (hucodec-decode bits huffman-tree)))
      (hucodec-write-symbols output-decoded-filename message))))

;; Function: Recursive helper for printing the Huffman tree.
(defun print-huffman-tree-recursive (tree indent-level)
  (case (car tree)
    (leaf (format t "~vT|Leaf: ~w, Weight: ~w~%" indent-level (cadr tree) (caddr tree)))
    (node (format t "~vT|Node: Weight: ~w~%" indent-level (caddr tree))
          (print-huffman-tree-recursive (cadr tree) (+ indent-level 4))
          (print-huffman-tree-recursive (caddr tree) (+ indent-level 4)))))
          
;; Function: Prints a Huffman tree to the terminal (for debugging).
(defun hucodec-print-huffman-tree (tree &optional (indent-level 0))
  (print-huffman-tree-recursive tree indent-level))




;; Example Usage (commented out to prevent automatic execution on load)
;; ;; Example symbol-weight list (like symbols_n_weights in Prolog example)
;; (defparameter *example-sws*
;;   '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))

;; ;; Generate Huffman Tree
;; (defparameter *ht* (hucodec-generate-huffman-tree *example-sws*))

;; ;; Generate Symbol Bits Table
;; (defparameter *sbt* (hucodec-generate-symbol-bits-table *ht*))

;; ;; Example Message
;; (defparameter *message* '(b a c))

;; ;; Encode Message
;; (defparameter *bits* (hucodec-encode *message* *ht*))

;; ;; Decode Bits
;; (defparameter *decoded-message* (hucodec-decode *bits* *ht*))

;; ;; Print Huffman Tree
;; (hucodec-print-huffman-tree *ht*)

;; ;; Example File Encoding/Decoding (you need to create input.txt with some text first)
;; ;; (hucodec-encode-file "input.txt" *ht* "encoded.txt")
;; ;; (hucodec-decode-file *ht* "encoded.txt" "decoded.txt")