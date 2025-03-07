(defvar huffman-tree nil "Variabile globale per memorizzare l'albero di Huffman.")

(defstruct huffman-node
  type     ; 'leaf or 'internal
  symbol   ; for leaf nodes: the symbol
  weight   ; weight of the node
  left     ; for internal nodes: left child (huffman-node)
  right    ; for internal nodes: right child (huffman-node)
  )

(defun make-leaf-node (symbol weight)
  "Crea un nodo foglia di Huffman."
  (make-huffman-node :type 'leaf :symbol symbol :weight weight))

(defun make-internal-node (left right)
  "Crea un nodo interno di Huffman a partire da due nodi figli."
  (make-huffman-node :type 'internal
                     :weight (+ (huffman-node-weight left) (huffman-node-weight right))
                     :left left
                     :right right))

(defun sort-huffman-nodes (nodes)
  "Ordina una lista di nodi di Huffman in base al peso."
  (sort (copy-list nodes) #'< :key #'huffman-node-weight))

(defun insert-huffman-node-sorted (node sorted-nodes)
  "Inserisce un nodo in una lista ordinata di nodi di Huffman mantenendo l'ordine per peso."
  (let ((weight (huffman-node-weight node)))
    (if (null sorted-nodes)
        (list node)
        (if (<= weight (huffman-node-weight (first sorted-nodes)))
            (cons node sorted-nodes)
            (cons (first sorted-nodes) (insert-huffman-node-sorted node (rest sorted-nodes)))))))

(defun hucodec-generate-huffman-tree (symbols-weights)
  "Genera un albero di Huffman dato un elenco di coppie (simbolo peso)."
  (if (null symbols-weights)
      (error "Errore: la lista di simboli e pesi è vuota.")
      (labels ((create-initial-leaf-nodes (sw-list)
                 (mapcar (lambda (sw) (make-leaf-node (car sw) (cdr sw))) sw-list))
               (build-huffman-tree (sorted-nodes)
                 (if (= (length sorted-nodes) 1)
                     (first sorted-nodes) ; Base case: return the root node
                     (let* ((node1 (first sorted-nodes))
                            (node2 (second sorted-nodes))
                            (rest-nodes (cddr sorted-nodes))
                            (merged-node (make-internal-node node1 node2)))
                       (build-huffman-tree (insert-huffman-node-sorted merged-node rest-nodes))))))

         (let* ((initial-nodes (create-initial-leaf-nodes symbols-weights))
                (sorted-initial-nodes (sort-huffman-nodes initial-nodes)))
           (build-huffman-tree sorted-initial-nodes)))))

;;; Esempio di utilizzo (da testare in REPL Lisp):
;; (hucodec-generate-huffman-tree '(("A" . 8) ("B" . 3) ("C" . 1) ("D" . 1) ("E" . 1) ("F" . 1) ("G" . 1) ("H" . 1)))
;; Dopo aver generato l'albero, usa hucodec-print-huffman-tree per visualizzarlo.

;------------------------------------------------------------------------------
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  "Stampa l'albero di Huffman in modo leggibile."
  (make-string (* 2 indent-level) :initial-element #\Space)
    (if (eq (huffman-node-type huffman-tree) 'leaf)
        (format t "~aLeaf: ~a (~a)~%" indent (huffman-node-symbol huffman-tree) (huffman-node-weight huffman-tree))
        (progn
          (format t "~aInternal: (~a)~%" indent (huffman-node-weight huffman-tree))
          (hucodec-print-huffman-tree (huffman-node-left huffman-tree) (+ indent-level 1))
          (hucodec-print-huffman-tree (huffman-node-right huffman-tree) (+ indent-level 1))))))

;;; Esempio di utilizzo (da testare in REPL Lisp):
;; (setq huffman-tree (hucodec-generate-huffman-tree '(("A" . 8) ("B" . 3) ("C" . 1) ("D" . 1) ("E" . 1) ("F" . 1) ("G" . 1) ("H" . 1))))
;; (hucodec-print-huffman-tree huffman-tree)

;------------------------------------------------------------------------------
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (declare (ignore huffman-tree))
  ;; Implementazione della funzione per generare la tabella dei bit dei simboli
  )
;------------------------------------------------------------------------------
(defun hucodec-encode (message huffman-tree)
  (declare (ignore message huffman-tree))
  ;; Implementazione della funzione di codifica
  )
;------------------------------------------------------------------------------
(defun hucodec-decode (bits huffman-tree)
  (declare (ignore bits huffman-tree))
  ;; Implementazione della funzione di decodifica
  )
;------------------------------------------------------------------------------
(defun hucodec-encode-file (filename huffman-tree)
  (declare (ignore filename huffman-tree))
  ;; Implementazione della funzione di codifica del file
  )

