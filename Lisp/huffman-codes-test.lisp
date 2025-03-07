(load "huffman-codes.lisp")

;; Funzione di stampa per visualizzare l'albero di Huffman
(defun print-huffman-tree (node &optional (indent 0))
  (if (eq (huffman-node-type node) 'leaf)
      (format t "~v@TLeaf: ~a, Weight: ~a~%" indent (huffman-node-symbol node) (huffman-node-weight node))
      (progn
        (format t "~v@TNode: Weight: ~a~%" indent (huffman-node-weight node))
        (print-huffman-tree (huffman-node-left node) (+ indent 4))
        (print-huffman-tree (huffman-node-right node) (+ indent 4)))))

;; Test data
(defparameter *test-symbols-n-weights* '(("A" . 8) ("B" . 3) ("C" . 1) ("D" . 1) ("E" . 1) ("F" . 1) ("G" . 1) ("H" . 1)))
(defparameter *test-huffman-tree* (hucodec-generate-huffman-tree *test-symbols-n-weights*))

;; Stampa l'albero di Huffman generato
(format t "Albero di Huffman generato:~%")
(print-huffman-tree *test-huffman-tree*)

;; Test: Generate Huffman Tree
(assert (equal (huffman-node-weight *test-huffman-tree*) 17))

(format t "Test for hucodec-generate-huffman-tree passed.~%")

;; Test per hucodec-generate-symbol-bits-table
(defparameter *test-symbol-bits-table* (hucodec-generate-symbol-bits-table *test-huffman-tree*))

;; Stampa la tabella dei simboli e dei bit generata
(format t "Tabella simboli-bit generata:~%")
(dolist (entry *test-symbol-bits-table*)
  (format t "~a -> ~a~%" (car entry) (cdr entry)))

;; Test: verifica che ogni simbolo abbia un codice binario
(assert (every #'listp (mapcar #'cdr *test-symbol-bits-table*)))

(format t "Test for hucodec-generate-symbol-bits-table passed.~%")

