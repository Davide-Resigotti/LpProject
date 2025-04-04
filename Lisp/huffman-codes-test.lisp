(compile-file "huffman-codes.lisp")
(load "huffman-codes.lisp")

; FILE DI TEST

; 1. Definizione frequenze di utilizzo
(defparameter symbols-frequencies
  '((#\c 1) (#\i 1) (#\a 1) (#\o 1)))

; 2. Genera l'albero di Huffman
(defparameter huffman-tree
  (hucodec-generate-huffman-tree symbols-frequencies))

; 3. Stampa l'albero
(hucodec-print-huffman-tree huffman-tree)

; 4. Genera la tabella simbolo-bit
(defparameter symbol-bits-table
  (hucodec-generate-symbol-bits-table huffman-tree))
(format t "Symbol-Bits Table: ~A~%" symbol-bits-table)

; 5. Messaggio da codificare (esempio: ciao)
(defparameter message-to-encode
  '(#\c #\i #\a #\o))

; 6. Codifica il messaggio
(defparameter encoded-bits
  (hucodec-encode message-to-encode huffman-tree))
(format t "Encoded Bits: ~A~%" encoded-bits)

; 7. Decodifica i bit
(defparameter decoded-message
  (hucodec-decode encoded-bits huffman-tree))
(format t "Decoded Message: ~A~%" decoded-message)
