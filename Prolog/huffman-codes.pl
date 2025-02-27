% huffman-codes.pl
% Implementazione della codifica di Huffman in Prolog

% Struttura dati dell'albero di Huffman
% - leaf(Simbolo, Peso)
% - node(Sinistro, Destro, Peso)

% Predicati principali
:- dynamic symbols_n_weights/1.
:- dynamic message/1.

% Generazione dell'albero di Huffman
hucodec_generate_huffman_tree(SymbolsWeights, HuffmanTree) :-
    create_initial_nodes(SymbolsWeights, Nodes),
    build_huffman_tree(Nodes, HuffmanTree).

% Crea la lista di foglie ordinata a partire dai simboli e pesi
create_initial_nodes([], []).
create_initial_nodes([sw(S,W)|T], [leaf(S,W)|Rest]) :-
    create_initial_nodes(T, Rest).

% Costruisce l'albero di Huffman
build_huffman_tree([Tree], Tree).
build_huffman_tree(Nodes, Tree) :-
    select_smallest_two(Nodes, N1, N2, RestNodes),
    merge_nodes(N1, N2, MergedNode),
    insert_sorted(MergedNode, RestNodes, NewNodes),
    build_huffman_tree(NewNodes, Tree).

% Seleziona i due nodi con il peso più piccolo
select_smallest_two(Nodes, N1, N2, Rest) :-
    sort_nodes(Nodes, [N1, N2 | Rest]).

% Unisce due nodi in un nodo interno, garantendo che il più piccolo sia a sinistra
merge_nodes(Node1, Node2, node(Node1, Node2, W_sum)) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    W_sum is W1 + W2,
    W1 =< W2, !.
merge_nodes(Node1, Node2, node(Node2, Node1, W_sum)) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    W_sum is W1 + W2.

% Ritorna il peso di un nodo
node_weight(leaf(_, W), W).
node_weight(node(_, _, W), W).

% Inserisce un nodo mantenendo l'ordinamento per peso
insert_sorted(Node, [], [Node]).
insert_sorted(Node, [H|T], [Node, H|T]) :-
    node_weight(Node, W1),
    node_weight(H, W2),
    W1 =< W2, !.
insert_sorted(Node, [H|T], [H|Rest]) :-
    insert_sorted(Node, T, Rest).

% Generazione della tabella simboli-bits
hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
    traverse_tree(HuffmanTree, [], SymbolBitsTable).

% Attraversa l'albero per creare la tabella dei bit
traverse_tree(leaf(S, _), Bits, [sb(S, Bits)]).
traverse_tree(node(Left, Right, _), CurrentBits, Table) :-
    append(CurrentBits, [0], LeftBits),
    append(CurrentBits, [1], RightBits),
    traverse_tree(Left, LeftBits, LeftTable),
    traverse_tree(Right, RightBits, RightTable),
    append(LeftTable, RightTable, Table).

% Codifica un messaggio in bit
hucodec_encode([], _, []).
hucodec_encode([Symbol|Rest], HuffmanTree, Bits) :-
    hucodec_generate_symbol_bits_table(HuffmanTree, Table),
    member(sb(Symbol, SymbolBits), Table),
    hucodec_encode(Rest, HuffmanTree, RestBits),
    append(SymbolBits, RestBits, Bits).

% Decodifica una sequenza di bit in un messaggio
hucodec_decode(Bits, HuffmanTree, Message) :-
    decode_bits(Bits, HuffmanTree, HuffmanTree, Message).

decode_bits([], _, _, []).
decode_bits(Bits, node(Left, Right, _), Root, [Symbol|Rest]) :-
    decode_next_symbol(Bits, node(Left, Right, _), Root, Symbol, RemainingBits),
    decode_bits(RemainingBits, Root, Root, Rest).

% Decodifica un simbolo dai bit
decode_next_symbol([], leaf(S, _), _, S, []).
decode_next_symbol([0|Bits], node(Left, _, _), Root, Symbol, Remaining) :-
    decode_next_symbol(Bits, Left, Root, Symbol, Remaining).
decode_next_symbol([1|Bits], node(_, Right, _), Root, Symbol, Remaining) :-
    decode_next_symbol(Bits, Right, Root, Symbol, Remaining).

% Codifica il contenuto di un file in bit
hucodec_encode_file(Filename, HuffmanTree, Bits) :-
    read_file_to_chars(Filename, Chars),
    hucodec_encode(Chars, HuffmanTree, Bits).

% Legge i caratteri da un file
read_file_to_chars(Filename, Chars) :-
    open(Filename, read, Stream),
    read_stream_chars(Stream, Chars),
    close(Stream).

read_stream_chars(Stream, []) :-
    at_end_of_stream(Stream), !.
read_stream_chars(Stream, [C|Chars]) :-
    get_char(Stream, C),
    read_stream_chars(Stream, Chars).

% Stampa l'albero di Huffman in formato grafico con archi
hucodec_print_huffman_tree(Tree) :-
    print_tree(Tree, '', true).

% Stampa un nodo foglia
print_tree(leaf(S, W), Prefix, IsLeft) :-
    (IsLeft -> format('~w|-- ~w (~d)~n', [Prefix, S, W])
    ; format('~w\\-- ~w (~d)~n', [Prefix, S, W])).

% Stampa un nodo interno con archi
print_tree(node(Left, Right, W), Prefix, IsLeft) :-
    (IsLeft -> format('~w|-- (~d)~n', [Prefix, W]), NewPrefix = '|   '
    ; format('~w\\-- (~d)~n', [Prefix, W]), NewPrefix = '    '),
    atom_concat(Prefix, NewPrefix, ChildPrefix),
    print_tree(Left, ChildPrefix, true),
    print_tree(Right, ChildPrefix, false).

% Ordinamento dei nodi in base al peso
sort_nodes(Nodes, Sorted) :-
    predsort(compare_nodes, Nodes, Sorted).

compare_nodes(Order, Node1, Node2) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    compare(Order, W1, W2).
