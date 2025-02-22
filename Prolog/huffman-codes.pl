% huffman-codes.pl
% Implementazione codifica di Huffman in Prolog

% Struttura dati albero
% - leaf(Symbolo, Peso)
% - node(Sinistro, Destro, Peso)

% Predicati principali
:- dynamic symbols_n_weights/1.
:- dynamic message/1.

% Generazione albero di Huffman
hucodec_generate_huffman_tree(SymbolsWeights, HuffmanTree) :-
    create_initial_nodes(SymbolsWeights, Nodes),
    build_huffman_tree(Nodes, HuffmanTree).

create_initial_nodes([], []).
create_initial_nodes([sw(S,W)|T], [leaf(S,W)|Rest]) :-
    create_initial_nodes(T, Rest).

build_huffman_tree([Tree], Tree).
build_huffman_tree(Nodes, Tree) :-
    select_smallest_two(Nodes, N1, N2, RestNodes),
    merge_nodes(N1, N2, MergedNode),
    insert_sorted(MergedNode, RestNodes, NewNodes),
    build_huffman_tree(NewNodes, Tree).

select_smallest_two(Nodes, N1, N2, Rest) :-
    sort_nodes(Nodes, Sorted),
    Sorted = [N1, N2|Rest].

merge_nodes(Node1, Node2, node(Node1, Node2, W_sum)) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    W_sum is W1 + W2.

node_weight(leaf(_, W), W).
node_weight(node(_, _, W), W).

insert_sorted(Node, List, [Node|List]) :-
    node_weight(Node, W),
    (   List = [] 
    ;   List = [H|_], node_weight(H, W1), W =< W1
    ), !.
insert_sorted(Node, [H|T], [H|Rest]) :-
    insert_sorted(Node, T, Rest).

% Generazione tabella simboli-bits
hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
    traverse_tree(HuffmanTree, [], SymbolBitsTable).

traverse_tree(leaf(S, _), Bits, [sb(S, Bits)]).
traverse_tree(node(Left, Right, _), CurrentBits, Table) :-
    append(CurrentBits, [0], LeftBits),
    append(CurrentBits, [1], RightBits),
    traverse_tree(Left, LeftBits, LeftTable),
    traverse_tree(Right, RightBits, RightTable),
    append(LeftTable, RightTable, Table).

% Codifica messaggio
hucodec_encode([], _, []).
hucodec_encode([Symbol|Rest], HuffmanTree, Bits) :-
    hucodec_generate_symbol_bits_table(HuffmanTree, Table),
    member(sb(Symbol, SymbolBits), Table),
    hucodec_encode(Rest, HuffmanTree, RestBits),
    append(SymbolBits, RestBits, Bits).

% Decodifica messaggio
hucodec_decode(Bits, HuffmanTree, Message) :-
    decode_bits(Bits, HuffmanTree, HuffmanTree, Message).

decode_bits([], _, _, []).
decode_bits(Bits, CurrentNode, Root, [Symbol|Rest]) :-
    decode_next_symbol(Bits, CurrentNode, Root, Symbol, RemainingBits),
    decode_bits(RemainingBits, Root, Root, Rest).

decode_next_symbol([Bit|RestBits], node(Left, Right, _), Root, Symbol, Remaining) :-
    (Bit = 0 -> NextNode = Left ; NextNode = Right),
    decode_next_symbol(RestBits, NextNode, Root, Symbol, Remaining).
decode_next_symbol([], leaf(S, _), _Root, S, []).

% Codifica file
hucodec_encode_file(Filename, HuffmanTree, Bits) :-
    read_file_to_chars(Filename, Chars),
    hucodec_encode(Chars, HuffmanTree, Bits).

read_file_to_chars(Filename, Chars) :-
    see(Filename),
    get_char(C),
    read_all_chars(C, Chars),
    seen.

read_all_chars(end_of_file, []) :- !.
read_all_chars(C, [C|Rest]) :-
    get_char(NextC),
    read_all_chars(NextC, Rest).

% Stampa albero
hucodec_print_huffman_tree(Tree) :-
    print_tree(Tree, 0).

print_tree(leaf(S, W), Indent) :-
    tab(Indent),
    format('Leaf: ~w (Weight: ~d)~n', [S, W]).
print_tree(node(Left, Right, W), Indent) :-
    tab(Indent),
    format('Node (Weight: ~d)~n', [W]),
    NewIndent is Indent + 4,
    print_tree(Left, NewIndent),
    print_tree(Right, NewIndent).

% Utilità
sort_nodes(Nodes, Sorted) :-
    predsort(compare_nodes, Nodes, Sorted).

compare_nodes(Order, Node1, Node2) :-
    node_weight(Node1, W1),
    node_weight(Node2, W2),
    compare(Order, W1, W2).