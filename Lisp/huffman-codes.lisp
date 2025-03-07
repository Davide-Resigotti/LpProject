(defun mag-p (L1 L2)

  ;; Confronta due coppie (simbolo . peso) e restituisce T se il peso di L1 è
  ;; minore di quello di L2
  (if (< (car (cdr L1)) (car (cdr L2)))
      T
      nil))

(defun flatten (x)
  ;; Appiattisce una lista annidata in una lista piatta
  (cond ((null x) x)
        ((atom x) (list x))
        (T (append (flatten (first x)) (flatten (rest x))))))

(defun hucodec-generate-huffman-tree (symbols-n-weights)
  ;; Genera l'albero di Huffman a partire da una lista di simboli e pesi
  (if (< (length symbols-n-weights) 2)
      (error "huffman tree can't be generated with only one symbol or less")
      (generate-tree symbols-n-weights)))

(defun generate-tree (symbols-n-weights)
  ;; Funzione ricorsiva che costruisce l'albero di Huffman
  (if (null (cdr symbols-n-weights))
      (car symbols-n-weights)
      (let ((sorted-nodes (stable-sort symbols-n-weights 'mag-p)))
        (generate-tree
         (append (list (cons
                        (flatten (list (car (car sorted-nodes))
                                       (car (car (cdr sorted-nodes)))))
                        (cons
                         (+ (car (cdr (car sorted-nodes)))
                            (car (cdr (car (cdr sorted-nodes)))))
                         (cons (car sorted-nodes)
                               (list (car (cdr sorted-nodes)))))))
                 (cdr (cdr sorted-nodes)))))))

(defun node-left (branch)
  ;; Restituisce il nodo sinistro di un ramo dell'albero
  (if (not (atom branch))
      (car (cdr (cdr branch)))))

(defun node-right (branch)
  ;; Restituisce il nodo destro di un ramo dell'albero
  (if (not (atom branch))
      (car (cdr (cdr (cdr branch))))))

(defun leaf-p (branch)
  ;; Verifica se un nodo è una foglia (ovvero non ha figli)
  (cond ((null (cdr (cdr branch))) T)
        (T nil)))

(defun leaf-symbol (leaf)
  ;; Restituisce il simbolo associato a una foglia
  (car leaf))

(defun choose-branch (bit branch)
  ;; Sceglie il ramo successivo dell'albero in base al bit (0 = sinistra, 1 =
  ;; destra)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (node-right branch))
        (t (error "Bad bit ~D. ERROR: while choosing the next branch, tree may
                   be incorrectly generated or be a simple leaf!" bit))))

(defun in-p (L elem)
  ;; Controlla se un elemento è presente in una lista
  (if (null L)
      nil
      (or (eql (if (atom L) L (car L)) elem)
          (if (not (atom L)) (in-p (rest L) elem)))))

(defun hucodec-decode (bits huffman-tree)
  ;; Decodifica una sequenza di bit usando l'albero di Huffman
  (labels ((decode-1 (bits current-node)
             (if (null bits)
                 nil
                 (let ((next-node (choose-branch (first bits) current-node)))
                   (if (leaf-p next-node)
                       (cons (leaf-symbol next-node)
                             (decode-1 (rest bits) huffman-tree))
                       (decode-1 (rest bits) next-node))))))
    (decode-1 bits huffman-tree)))

(defun choose-next-encoding-branch (branch current-symbol)
  ;; Trova il ramo corretto nell'albero di Huffman per codificare il simbolo
  ;; corrente
  (cond ((and (node-left branch)
              (in-p (car (node-left branch)) current-symbol))
         (node-left branch))
        ((and (node-right branch)
              (in-p (car (node-right branch)) current-symbol))
         (node-right branch))
        (T (error "ERROR: Symbol ~A not found in Huffman tree! Tree may be
                   incorrectly generated or symbol is not in the alphabet."
                  current-symbol))))

(defun hucodec-encode (message huffman-tree)
  ;; Codifica un messaggio utilizzando l'albero di Huffman
  (labels ((encode-1 (message current-node current-bits)
             (if (null message)
                 current-bits
                 (let* ((current-symbol (car message))
                        (next-branch (choose-next-encoding-branch
                                      current-node current-symbol)))
                   (if (leaf-p next-branch)
                       (if (equal (node-left current-node) next-branch)
                           (encode-1 (rest message) huffman-tree
                                     (append current-bits (list 0)))
                           (encode-1 (rest message) huffman-tree
                                     (append current-bits (list 1))))
                       (if (equal (node-left current-node) next-branch)
                           (encode-1 message next-branch
                                     (append current-bits (list 0)))
                           (encode-1 message next-branch
                                     (append current-bits (list 1)))))))))
    (encode-1 message huffman-tree '())))

(defun hucodec-generate-symbol-bits-table (huffman-tree)
  ;; Genera una tabella di simboli e bit associati per la codifica Huffman
  (labels ((generate-table-recursive (node current-code table)
             (if (leaf-p node)
                 (cons (list (leaf-symbol node) current-code) table)
                 (let ((left-child (node-left node))
                       (right-child (node-right node)))
                   (let ((updated-table
                          (generate-table-recursive left-child
                                                    (append current-code '(0))
                                                    table)))
                     (generate-table-recursive right-child
                                               (append current-code '(1))
                                               updated-table))))))
    (generate-table-recursive huffman-tree '() '())))

(defun hucodec-encode-file (filename huffman-tree)
  ;; Legge un file e lo codifica con l'albero di Huffman
  (with-open-file (stream filename :direction :input :element-type 'character)
    (let ((message (loop for char = (read-char stream nil nil)
                         while char
                         collect char)))
      (hucodec-encode message huffman-tree))))

(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  ;; Stampa l'albero di Huffman in modo leggibile
  (let ((indentation (make-string (* indent-level 2) :initial-element #\Space)))
    (if (leaf-p huffman-tree)
        (format t "~A- Leaf: Symbol=~A, Weight=~A~%"
                indentation (leaf-symbol huffman-tree) (car (cdr huffman-tree)))
        (progn
          (format t "~A- Node: Symbols=~A, Weight=~A~%"
                  indentation (car huffman-tree) (car (cdr huffman-tree)))
          (format t "~A  Left:~%" indentation)
          (hucodec-print-huffman-tree (node-left huffman-tree) (1+ indent-level))
          (format t "~A  Right:~%" indentation)
          (hucodec-print-huffman-tree (node-right huffman-tree) (1+ indent-level))))))
