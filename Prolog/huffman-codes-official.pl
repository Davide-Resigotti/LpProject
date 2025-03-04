% 1    -------------------------------------------------------------------------------generate_huffman_tree-------------------------------
% Predicato principale: Genera l'albero di Huffman da una lista di sw(Simbolo, Peso).
hucodec_generate_huffman_tree(SymbolsWeights, HuffmanTree) :-
    % Step 1: Crea i nodi iniziali (foglie) e ordina per peso.
    create_initial_nodes(SymbolsWeights, Nodes),
    sort_nodes(Nodes, SortedNodes),
    % Step 2: Costruisci l'albero.
    build_huffman_tree(SortedNodes, HuffmanTree).

% Crea una lista di foglie (leaf/2) a partire dai simboli e pesi.
create_initial_nodes([], []).
create_initial_nodes([sw(S, W) | Rest], [leaf(S, W) | Nodes]) :-
    create_initial_nodes(Rest, Nodes).

% Costruisce ricorsivamente l'albero di Huffman.
% Caso base: un solo nodo rimasto → è l'albero completo.
build_huffman_tree([Tree], Tree).

% Caso ricorsivo: unisci i due nodi più leggeri e continua.
build_huffman_tree([N1, N2 | Rest], Tree) :-
    % Step 1: Unisci i due nodi più piccoli in un nuovo nodo.
    merge_nodes(N1, N2, MergedNode),
    % Step 2: Inserisci il nuovo nodo mantenendo l'ordine.
    insert_sorted(MergedNode, Rest, NewNodes),
    % Step 3: Ricorsione finché resta un solo nodo.
    build_huffman_tree(NewNodes, Tree).

% Unisce due nodi in un nuovo nodo interno (node/3).
% Il nodo con peso minore va a sinistra.
merge_nodes(Node1, Node2, node(Node1, Node2, WSum)) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    WSum is W1 + W2,
    W1 =< W2. %% prova <------------------------------------------------------------

% Ritorna il peso di un nodo (foglia o nodo interno).
node_weight(leaf(_, W), W).
node_weight(node(_, _, W), W).

% Ordina i nodi in base al peso.
sort_nodes(Nodes, Sorted) :-
    sort(2, @=<, Nodes, Sorted).

% Inserisce un nodo mantenendo l'ordinamento per peso.
insert_sorted(Node, [], [Node]).
insert_sorted(Node, [H | T], [Node, H | T]) :-
    node_weight(Node, W1),
    node_weight(H, W2),
    W1 =< W2, !.
insert_sorted(Node, [H | T], [H | Rest]) :-
    insert_sorted(Node, T, Rest).
% --------------------------------------------------------------------------------------------------------------------


% 2 
hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
    generate_symbol_bits_table(HuffmanTree, [], SymbolBitsTable).

generate_symbol_bits_table(leaf(S, _), Bits, [sw(S, Bits)]).
generate_symbol_bits_table(node(Left, Right, _), Bits, SymbolBitsTable) :-
    append(Bits, [0], LeftBits),
    append(Bits, [1], RightBits),
    generate_symbol_bits_table(Left, LeftBits, LeftTable),
    generate_symbol_bits_table(Right, RightBits, RightTable),
    append(LeftTable, RightTable, SymbolBitsTable).

% ----------------------------------------------------------------------------------------------------------------------------


% 3
hucodec_encode(Symbols, HuffmanTree, Bits) :-
    hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable),
    encode_symbols(Symbols, SymbolBitsTable, Bits).

encode_symbols([], _, []).
encode_symbols([Symbol | Rest], SymbolBitsTable, Bits) :-
    member(sw(Symbol, SymbolBits), SymbolBitsTable),
    encode_symbols(Rest, SymbolBitsTable, RestBits),
    append(SymbolBits, RestBits, Bits).

% -----------------------------------------------------------------------------


% 4
hucodec_decode(Bits, HuffmanTree, Message) :-
    decode_bits(Bits, HuffmanTree, Message).

decode_bits([], _, []).
decode_bits(Bits, HuffmanTree, [Symbol | RestMessage]) :-
    decode_symbol(Bits, HuffmanTree, Symbol, RestBits),
    decode_bits(RestBits, HuffmanTree, RestMessage).

decode_symbol(Bits, leaf(S, _), S, Bits).
decode_symbol([0 | RestBits], node(Left, _, _), Symbol, RemainingBits) :-
    decode_symbol(RestBits, Left, Symbol, RemainingBits).
decode_symbol([1 | RestBits], node(_, Right, _), Symbol, RemainingBits) :-
    decode_symbol(RestBits, Right, Symbol, RemainingBits).

% -----------------------------------------------------------------------------------------------------------------------

% 5
% hucodec_encode_file(Filename, HuffmanTree, Bits) :-
hucodec_encode_file(InputFilename, HuffmanTree, OutputFilename) :-
    % Leggi i simboli dal file di input
    hucodec_read_symbols(InputFilename, Symbols),
    % Codifica i simboli utilizzando l'albero di Huffman
    hucodec_encode(Symbols, HuffmanTree, Bits),
    % Scrivi i bit codificati nel file di output
    hucodec_write_bits(OutputFilename, Bits).

hucodec_read_symbols(Filename, Symbols) :-
    open(Filename, read, Stream),
    read_symbols(Stream, Symbols),
    close(Stream).

read_symbols(Stream, []) :-
    at_end_of_stream(Stream), !.
read_symbols(Stream, [Symbol | Rest]) :-
    read(Stream, Symbol),
    read_symbols(Stream, Rest).

hucodec_write_bits(Filename, Bits) :-
    open(Filename, write, Stream),
    write_bits(Stream, Bits),
    close(Stream).

write_bits(_, []) :- !.
write_bits(Stream, [Bit | Rest]) :-
    write(Stream, Bit),
    write(Stream, ' '),
    write_bits(Stream, Rest).

% ------------------------------------------------------------------------------------------------------------

% 6
hucodec_print_huffman_tree(Tree) :-
    print_huffman_tree(Tree, 0).

print_huffman_tree(leaf(S, W), Indent) :-
    format('~*|Leaf: ~w, Weight: ~w~n', [Indent, S, W]).
print_huffman_tree(node(Left, Right, W), Indent) :-
    format('~*|Node: Weight: ~w~n', [Indent, W]),
    NewIndent is Indent + 4,
    print_huffman_tree(Left, NewIndent),
    print_huffman_tree(Right, NewIndent).



% !!!!!!!!!!!!!!!!! SISTEMARE COME VIENE LETTO INPUT !!!!!!!!!!!!!!!!!!







