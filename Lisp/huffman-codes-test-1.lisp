(load "huffman-codes-1.lisp")

(in-package :huffman-codes)

;; Funzione di stampa per visualizzare l'albero di Huffman
(defun print-huffman-tree (node &optional (indent 0))
  (if (eq (first node) 'leaf)
      (format t "~v@TLeaf: ~a, Weight: ~a~%" indent (second node) (third node))
      (progn
        (format t "~v@TNode: Weight: ~a~%" indent (fourth node))
        (print-huffman-tree (second node) (+ indent 4))
        (print-huffman-tree (third node) (+ indent 4)))))

;; Test data
(defparameter *test-symbols-n-weights* '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))
(defparameter *test-huffman-tree* (hucodec-generate-huffman-tree *test-symbols-n-weights*))

;; Stampa l'albero di Huffman generato
(format t "Albero di Huffman generato:~%")
(print-huffman-tree *test-huffman-tree*)

;; Test: Generate Huffman Tree
(assert (equal *test-huffman-tree*
               '(node (leaf a 8)
                      (node (node (node (leaf g 1) (leaf h 1) 2)
                                  (node (leaf e 1) (leaf f 1) 2) 4)
                            (node (node (leaf c 1) (leaf d 1) 2)
                                  (leaf b 3) 5) 9) 17)))

(format t "Test for hucodec-generate-huffman-tree passed.~%")

;; Rimuovi #| e |# intorno a questa parte per attivare il test:
;; Definisci la tabella dei bit dei simboli
(defparameter *test-symbol-bits-table*
  (mapcar (lambda (entry)
            (list 'sw (first entry) (second entry)))
          (hucodec-generate-symbol-bits-table *test-huffman-tree*)))

;; Test: Generate Symbol Bits Table
(assert (equal *test-symbol-bits-table*
               '((sw a (0))
                 (sw b (1 1))
                 (sw c (1 0 0))
                 (sw d (1 0 1))
                 (sw e (0 1 0))
                 (sw f (0 1 1))
                 (sw g (0 0 0))
                 (sw h (0 0 1)))))

(format t "Test for hucodec-generate-symbol-bits-table passed.~%")