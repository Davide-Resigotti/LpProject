(defpackage :huffman-codes
  (:use :cl))

(in-package :huffman-codes)

(defun hucodec-generate-huffman-tree (symbols-weights)
  (if (null symbols-weights)
      (progn
        (format t "Errore: la lista di simboli e pesi è vuota.~%")
        (return-from hucodec-generate-huffman-tree nil))
      (let* ((nodes (create-initial-nodes symbols-weights))
             (sorted-nodes (sort-nodes nodes)))
        (build-huffman-tree sorted-nodes))))

(defun create-initial-nodes (symbols-weights)
  (mapcar (lambda (sw) (make-leaf (first sw) (second sw))) symbols-weights))

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun build-huffman-tree (nodes)
  (if (= (length nodes) 1)
      (first nodes)
      (let* ((n1 (first nodes))
             (n2 (second nodes))
             (rest (nthcdr 2 nodes))
             (merged-node (merge-nodes n1 n2))
             (new-nodes (insert-sorted merged-node rest)))
        (build-huffman-tree new-nodes))))

(defun merge-nodes (node1 node2)
  (let* ((w1 (node-weight node1))
         (w2 (node-weight node2))
         (wsum (+ w1 w2)))
    (list 'node node1 node2 wsum)))

(defun node-weight (node)
  (if (eq (first node) 'leaf)
      (third node)
      (fourth node)))

(defun node-symbol (node)
  "Restituisce il simbolo se è un leaf, altrimenti NIL."
  (if (eq (first node) 'leaf)
      (second node)
      nil))

(defun sort-nodes (nodes)
  ;; Ordina prima per weight, poi per simbolo in ordine alfabetico (se i weight sono uguali).
  (sort nodes
        (lambda (n1 n2)
          (let ((w1 (node-weight n1))
                (w2 (node-weight n2))
                (s1 (node-symbol n1))
                (s2 (node-symbol n2)))
            (if (/= w1 w2)
                (< w1 w2)
                ;; Se i pesi sono uguali, fai un confronto sui simboli
                (string< (if s1 (string s1) "") (if s2 (string s2) "")))))))

(defun insert-sorted (node nodes)
  (if (null nodes)
      (list node)
      (let ((first-node (first nodes)))
        (if (<= (node-weight node) (node-weight first-node))
            (cons node nodes)
            (cons first-node (insert-sorted node (rest nodes)))))))

(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (generate-symbol-bits-table huffman-tree '()))

(defun generate-symbol-bits-table (node bits)
  (if (eq (first node) 'leaf)
      (list (list (second node) bits))
      (let* ((left (second node))
             (right (third node))
             (left-bits (append bits '(0)))
             (right-bits (append bits '(1)))
             (left-table (generate-symbol-bits-table left left-bits))
             (right-table (generate-symbol-bits-table right right-bits)))
        (append left-table right-table))))

(defun hucodec-encode (symbols huffman-tree)
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (encode-symbols symbols symbol-bits-table)))

(defun encode-symbols (symbols symbol-bits-table)
  (if (null symbols)
      '()
      (let* ((symbol (first symbols))
             (rest (rest symbols))
             (symbol-bits (second (assoc symbol symbol-bits-table))))
        (append symbol-bits (encode-symbols rest symbol-bits-table)))))

(defun hucodec-decode (bits huffman-tree)
  (decode-bits bits huffman-tree))

(defun decode-bits (bits huffman-tree)
  (if (null bits)
      '()
      (let* ((result (decode-symbol bits huffman-tree))
             (symbol (first result))
             (rest-bits (second result)))
        (cons symbol (decode-bits rest-bits huffman-tree)))))

(defun decode-symbol (bits node)
  (if (eq (first node) 'leaf)
      (list (second node) bits)
      (let ((bit (first bits))
            (rest-bits (rest bits)))
        (if (= bit 0)
            (decode-symbol rest-bits (second node))
            (decode-symbol rest-bits (third node))))))

(defun hucodec-encode-file (input-filename huffman-tree output-filename)
  (let ((symbols (hucodec-read-symbols input-filename)))
    (if (null symbols)
        (progn
          (format t "Errore: il file di input è vuoto.~%")
          nil)
        (let ((bits (hucodec-encode symbols huffman-tree)))
          (hucodec-write-bits output-filename bits)))))

(defun hucodec-read-symbols (filename)
  (with-open-file (stream filename :direction :input)
    (let ((string (read-line stream nil "")))
      (if (string= string "")
          nil
          (coerce string 'list)))))

(defun hucodec-write-bits (filename bits)
  (with-open-file (stream filename :direction :output)
    (write-bits stream bits)))

(defun write-bits (stream bits)
  (dolist (bit bits)
    (write-string (format nil "~a " bit) stream)))

(defun hucodec-read-bits (filename)
  (with-open-file (stream filename :direction :input)
    (let ((string (read-line stream nil "")))
      (mapcar #'parse-integer (split-string string ", ")))))

(defun split-string (string delimiter)
  (labels ((split-helper (start)
             (let ((end (search delimiter string :start2 start)))
               (if end
                   (cons (subseq string start end)
                         (split-helper (+ end (length delimiter))))
                   (list (subseq string start))))))
    (split-helper 0)))

(defun hucodec-write-symbols (filename symbols)
  (with-open-file (stream filename :direction :output)
    (dolist (symbol symbols)
      (write-string (string symbol) stream))))

(defun hucodec-add-commas-to-encoded-file ()
  (let ((bits (hucodec-read-bits "encoded.txt")))
    (with-open-file (stream "encoded_with_commas.txt" :direction :output)
      (write-bits-with-commas stream bits))))

(defun write-bits-with-commas (stream bits)
  (dolist (bit bits)
    (write-string (format nil "~a, " bit) stream)))

(defun hucodec-decode-file (huffman-tree)
  (let ((bits (hucodec-read-bits "encoded_with_commas.txt")))
    (hucodec-decode bits huffman-tree)))

(defun hucodec-print-huffman-tree (tree)
  (print-huffman-tree tree 0))

(defun print-huffman-tree (node indent)
  (if (eq (first node) 'leaf)
      (format t "~v@TLeaf: ~a, Weight: ~a~%" indent (second node) (third node))
      (progn
        (format t "~v@TNode: Weight: ~a~%" indent (fourth node))
        (let ((new-indent (+ indent 4)))
          (print-huffman-tree (second node) new-indent)
          (print-huffman-tree (third node) new-indent)))))