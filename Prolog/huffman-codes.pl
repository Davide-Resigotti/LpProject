% 1

% Genera l'albero di Huffman da una lista di sw(Simbolo, Peso)
hucodec_generate_huffman_tree([], _) :-
    writeln('Errore: la lista di simboli e pesi è vuota.'),
    fail.
hucodec_generate_huffman_tree(SymbolsWeights, HuffmanTree) :-
    % Crea i nodi iniziali (foglie) e ordina per peso
    create_initial_nodes(SymbolsWeights, Nodes),
    sort_nodes(Nodes, SortedNodes),
    % Costruisce l'albero
    build_huffman_tree(SortedNodes, HuffmanTree).

% Crea una lista di foglie a partire dai simboli e pesi
create_initial_nodes([], []).
create_initial_nodes([sw(S, W) | Rest], [leaf(S, W) | Nodes]) :-
    create_initial_nodes(Rest, Nodes).

% Costruisce ricorsivamente l'Huffman tree

% Caso base: un solo nodo rimasto è l'albero completo
build_huffman_tree([Tree], Tree).

% Caso ricorsivo: unisci i due nodi con peso minore e continua
build_huffman_tree([N1, N2 | Rest], Tree) :-
    % Unisci i due nodi più piccoli in un nuovo nodo
    merge_nodes(N1, N2, MergedNode),
    % Inserisci il nuovo nodo mantenendo l'ordine
    insert_sorted(MergedNode, Rest, NewNodes),
    % Ricorsione finché resta un solo nodo (caso base)
    build_huffman_tree(NewNodes, Tree).

% Unisce due nodi in un nuovo nodo interno
% Il nodo con peso minore va a sinistra
merge_nodes(Node1, Node2, node(Node1, Node2, WSum)) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    WSum is W1 + W2,
    W1 =< W2.

% Ritorna il peso di un nodo (foglia o nodo interno)
node_weight(leaf(_, W), W).
node_weight(node(_, _, W), W).

% Ordina i nodi in base al peso
sort_nodes(Nodes, Sorted) :-
    sort(2, @=<, Nodes, Sorted).

% Inserisce un nodo mantenendo l'ordinamento per peso
insert_sorted(Node, [], [Node]).
insert_sorted(Node, [H | T], [Node, H | T]) :-
    node_weight(Node, W1),
    node_weight(H, W2),
    W1 =< W2,
    !.
insert_sorted(Node, [H | T], [H | Rest]) :-
    insert_sorted(Node, T, Rest).

% ----------------------------------------------------------------------
% 2
% Prende come input un albero di Huffman e produce una lista di
% coppie (simbolo, codice binario)
hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
    generate_symbol_bits_table(HuffmanTree, [], SymbolBitsTable).

% Caso base: quando si incontra una foglia, si associa il simbolo S
% al codice binario accumulato Bits Il risultato viene memorizzato
% come una coppia sw(S, Bits) all'interno della tabella
generate_symbol_bits_table(leaf(S, _), Bits, [sw(S, Bits)]).

% Caso ricorsivo: quando si incontra un nodo interno, si aggiungono 0
% (sinistra) e 1 (destra), per esplorare i sottoalberi sinistro e
% destro La funzione viene chiamata ricorsivamente sui sottoalberi
% con i bit aggiornati e le tabelle risultanti vengono unite
generate_symbol_bits_table(node(Left, Right, _), Bits,
                            SymbolBitsTable) :-
    append(Bits, [0], LeftBits),
    append(Bits, [1], RightBits),
    generate_symbol_bits_table(Left, LeftBits, LeftTable),
    generate_symbol_bits_table(Right, RightBits, RightTable),
    append(LeftTable, RightTable, SymbolBitsTable).

% ----------------------------------------------------------------------
% 3
% hucodec_encode codifica una lista di simboli in una sequenza di bit
% usando l'albero di Huffman Prima genera la tabella di associazione
% simbolo-bit e poi usa encode_symbols per ottenere la sequenza di
% bit finale
hucodec_encode(Symbols, HuffmanTree, Bits) :-
    hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
    encode_symbols(Symbols, SymbolBitsTable, Bits).

% Caso base: se la lista dei simboli è vuota, anche la lista dei bit
% risultante è vuota
encode_symbols([], _, []).

% Caso ricorsivo: prende il primo simbolo, cerca il suo codice binario
% nella tabella SymbolBitsTable, poi codifica ricorsivamente il resto
% dei simboli e concatena i bit risultanti
encode_symbols([Symbol | Rest], SymbolBitsTable, Bits) :-
    member(sw(Symbol, SymbolBits), SymbolBitsTable),
    encode_symbols(Rest, SymbolBitsTable, RestBits),
    append(SymbolBits, RestBits, Bits).

% ----------------------------------------------------------------------

% 4
% La regola principale hucodec_decode decodifica una sequenza di bit
% in un messaggio usando l'albero di Huffman Passa la sequenza di
% bit e l'albero alla funzione ricorsiva decode_bits per ottenere il
% messaggio decodificato
hucodec_decode(Bits, HuffmanTree, Message) :-
    decode_bits(Bits, HuffmanTree, Message).

% Caso base: se non ci sono più bit da elaborare, il messaggio
% decodificato è vuoto
decode_bits([], _, []).

% Caso ricorsivo: decodifica il primo simbolo dalla sequenza di bit,
% restituendo il simbolo e i bit rimanenti Poi continua la decodifica
% con i bit restanti per ottenere il resto del messaggio
decode_bits(Bits, HuffmanTree, [Symbol | RestMessage]) :-
    decode_symbol(Bits, HuffmanTree, Symbol, RestBits),
    decode_bits(RestBits, HuffmanTree, RestMessage).

% Caso di errore: se i bit finiscono inaspettatamente prima della fine
% di un simbolo, la decodifica fallisce
decode_symbol([], _, _, _) :-
    fail.

% Caso in cui si raggiunge una foglia: il simbolo S viene estratto e i
% bit rimanenti non vengono modificati
decode_symbol(Bits, leaf(S, _), S, Bits).

% Caso ricorsivo: se il primo bit è 0, si segue il ramo sinistro
% dell'albero per decodificare il simbolo
decode_symbol([0 | RestBits], node(Left, _, _), Symbol, RemainingBits) :-
    decode_symbol(RestBits, Left, Symbol, RemainingBits).

% Caso ricorsivo: se il primo bit è 1, si segue il ramo destro
% dell'albero per decodificare il simbolo
decode_symbol([1 | RestBits], node(_, Right, _), Symbol, RemainingBits) :-
    decode_symbol(RestBits, Right, Symbol, RemainingBits).

% ----------------------------------------------------------------------
% 5
% hucodec_encode_file codifica il contenuto di un file utilizzando
% l'albero di Huffman Legge i simboli dal file di input, li codifica
% in una sequenza di bit e scrive il risultato in un file di output
hucodec_encode_file(InputFilename, HuffmanTree, OutputFilename) :-
    hucodec_read_symbols(InputFilename, Symbols),
    (   Symbols = []
    ->  writeln('Errore: il file di input è vuoto.'),
        % Gestisce il caso di file vuoto
        fail
    ;   hucodec_encode(Symbols, HuffmanTree, Bits),
        % Codifica i simboli in bit
        hucodec_write_bits(OutputFilename, Bits)
        % Scrive i bit nel file di output
    ).

% hucodec_read_symbols legge il contenuto di un file come una stringa
% e la converte in una lista di caratteri Se il file è vuoto,
% restituisce una lista vuota
hucodec_read_symbols(Filename, Symbols) :-
    open(Filename, read, Stream),
    read_string(Stream, _, String),
    close(Stream),
    (   String = ""
    ->  Symbols = []
        % Caso in cui il file è vuoto
    ;   string_chars(String, Symbols)
        % Converte la stringa in una lista di caratteri
    ).

% hucodec_write_bits scrive una lista di bit in un file, separandoli
% con spazi
hucodec_write_bits(Filename, Bits) :-
    open(Filename, write, Stream),
    write_bits(Stream, Bits),
    close(Stream).

% write_bits scrive ricorsivamente i bit nel file, separandoli con
% spazi
write_bits(_, []) :-
    !.
write_bits(Stream, [Bit | Rest]) :-
    write(Stream, Bit),
    write(Stream, ' '),
    write_bits(Stream, Rest).

% hucodec_read_bits legge una sequenza di bit da un file, convertendo
% la stringa in una lista di numeri
hucodec_read_bits(Filename, Bits) :-
    open(Filename, read, Stream),
    read_string(Stream, _, String),
    split_string(String, " ", "", BitStrings),
    % Divide la stringa in singoli bit
    maplist(atom_number, BitStrings, Bits),
    % Converte i bit in numeri interi
    close(Stream).

% hucodec_write_symbols scrive una lista di simboli in un file
hucodec_write_symbols(Filename, Symbols) :-
    open(Filename, write, Stream),
    write_symbols(Stream, Symbols),
    close(Stream).

% write_symbols scrive ricorsivamente i simboli nel file
write_symbols(_, []) :-
    !.
write_symbols(Stream, [Symbol | Rest]) :-
    write(Stream, Symbol),
    write_symbols(Stream, Rest).

% ----------------------------------------------------------------------
% 6
% hucodec_print_huffman_tree stampa l'albero di Huffman con
% un'indentazione per rappresentarne la struttura Chiama la funzione
% print_huffman_tree con un'indentazione iniziale di 0
hucodec_print_huffman_tree(Tree) :-
    print_huffman_tree(Tree, 0).

% Caso base: se il nodo è una foglia, stampa il simbolo e il suo peso
% con la giusta indentazione
print_huffman_tree(leaf(S, W), Indent) :-
    format('~*|Leaf: ~w, Weight: ~w~n', [Indent, S, W]).

% Caso ricorsivo: se il nodo è interno, stampa il suo peso e poi
% stampa ricorsivamente i due sottoalberi con un'indentazione
% maggiore
print_huffman_tree(node(Left, Right, W), Indent) :-
    format('~*|Node: Weight: ~w~n', [Indent, W]),
    NewIndent is Indent + 4,
    % Aumenta l'indentazione per i sottoalberi
    print_huffman_tree(Left, NewIndent),
    print_huffman_tree(Right, NewIndent).