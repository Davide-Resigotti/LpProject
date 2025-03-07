# README #

HUCODEC-GENERATE-HUFFMAN-TREE

La funzione `hucodec-generate-huffman-tree` permette di costruire un
albero di Huffman, usando una lista di coppie (liste a loro volta) e
due funzioni ausiliarie: `mag-p` e `flatten`. La lista `symbols-n-weights`
è costruita così:

((A 1) (B 3) … )

Il predicato `mag-p` ordina liste con `stable-sort`, definendo una
relazione d'ordine tra due elementi:

(A 1)  < (B 3)

La funzione `flatten` crea da una lista con sottoliste a
profondità variabile, una lista con gli stessi elementi, ma ad un
unico livello.

(flatten '((A) B))

; Risultato: (A B)

Esempio di chiamata di `hucodec-generate-huffman-tree`:

(hucodec-generate-huffman-tree '((a 5) (b 1) (c 2) (d 8)))

; Risultato: ((A C B D) 16 ((A C B) 8 ((A C) 3 (A 1) (C 2)) (B 5))
;            (D 8))
; (l'ordine dei nodi interni può variare)

Non si può chiamare con un solo simbolo e peso in `symbols-n-weights`,
perché non si può generare un albero di Huffman significativo con meno
di due elementi.

HUCODEC-ENCODE

`hucodec-encode` codifica un messaggio (lista di elementi):

(A B C …)

La codifica usa un albero di Huffman (`huffman-tree`), struttura per
codificare con il metodo omonimo. Esempio di chiamata:

(hucodec-encode '(A B) huffman-tree)

; Risultato: (0 1 1 1)
; (il risultato dipende dall'albero di Huffman generato)

HUCODEC-DECODE

`hucodec-decode` fa il lavoro inverso di `hucodec-encode`, con chiamata
simile. Prende in input una lista di bit (da `hucodec-encode`) e un
albero di Huffman.

(hucodec-decode '(0 1 1 1) huffman-tree)

; Risultato: (A B)
; (il risultato dipende dall'albero e dai bit forniti)

HUCODEC-GENERATE-SYMBOL-BITS-TABLE

Per avere una tabella di codifica dei simboli nelle foglie dell'albero
di Huffman (da `hucodec-generate-huffman-tree`), si usa
`hucodec-generate-symbol-bits-table`. Restituisce una lista di coppie
(liste a loro volta) simbolo-codifica. Esempio di chiamata:

(hucodec-generate-symbol-bits-table huffman-tree)

; Risultato: ((D (1)) (B (0 1)) (C (0 0 1)) (A (0 0 0)))

HUCODEC-ENCODE-FILE

`hucodec-encode-file` codifica il contenuto di un file di testo con
un albero di Huffman. Legge il file carattere per carattere e
restituisce i bit codificati. (a differenza di prolog abbiamo voluto
visualizzare la codifica in bit direttamente nel terminale come
output, invece di inserirla in un nuovo file, per dare 
un'idea più immediata del risultato)

(hucodec-encode-file "test.txt" huffman-tree)

; Risultato: (lista di bit codificati del contenuto di "test.txt")
; (il risultato dipende dal file e dall'albero di Huffman)

HUCODEC-PRINT-HUFFMAN-TREE

`hucodec-print-huffman-tree` stampa a terminale una rappresentazione
dell'albero di Huffman. Utile per debugging e visualizzazione.

(hucodec-print-huffman-tree huffman-tree)

; Output: (stampa a terminale la struttura dell'albero)

FUNZIONI AUSILIARIE

Altre funzioni ausiliarie importanti sono: `node-left`, `node-right`,
`leaf-p`, `leaf-symbol`, `choose-branch`, `choose-next-encoding-branch`
e `in-p`.  Occorre specificare la semantica di `in-p` e
`choose-next-encoding-branch`. `in-p` è un predicato che restituisce
`T` se l'elemento "elem" è nella lista `L`. `choose-next-encoding-branch`
è simile a `choose-branch`, ma differisce nella ricerca del ramo utile
per la codifica. Valuta come corretto il ramo che contiene,
nell'elencazione dei nodi del sottoalbero, il simbolo da codificare.