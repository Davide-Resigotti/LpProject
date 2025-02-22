% Caso base: Un solo simbolo (o nodo albero finale).
hucodec_generate_huffman_tree([sw(SymbolOrNode, _Weight)], SymbolOrNode).

% Caso ricorsivo:
hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
    % Ordina per peso (posizione 2).
    sort(2, @=<, SymbolsAndWeights, SortedSymbols),

    % Estrai i due elementi di peso minore.
    SortedSymbols = [sw(S1, W1), sw(S2, W2) | Rest],

    % Estrai il simbolo o il nodo.
    extract_symbol_or_node(sw(S1, W1), Node1),
    extract_symbol_or_node(sw(S2, W2), Node2),

    % Calcola il nuovo peso.
    NewWeight is W1 + W2,

    % Inverti l'ordine: il nodo con peso maggiore va a destra.
    (W1 =< W2 -> Left = Node1, Right = Node2 ; Left = Node2, Right = Node1),

    % Crea il nuovo nodo con la struttura corretta usando un functor.
    NewNode = node(NewWeight, Left, Right),

    % Inserisci il nuovo nodo nella lista e riordina.
    append([sw(NewNode, NewWeight)], Rest, TempListUnsorted),
    sort(2, @=<, TempListUnsorted, TempList),

    % Chiamata ricorsiva.
    hucodec_generate_huffman_tree(TempList, HuffmanTree).

% Estrae un simbolo o un nodo.
extract_symbol_or_node(sw(Symbol, _), Symbol) :- atom(Symbol).
extract_symbol_or_node(sw(Node, _), Node) :- compound(Node).

hucodec_print_huffman_tree(HuffmanTree) :-
    print_huffman_tree(HuffmanTree, 0).


print_huffman_tree((Weight, Left, Right), Depth) :-
    !,
    NewDepth is Depth + 1,
    print_huffman_tree(Right, NewDepth),
    print_indent(Depth),
    format('~w~n', [Weight]),
    print_huffman_tree(Left, NewDepth).

print_huffman_tree((Weight, Symbol), Depth) :-
    print_indent(Depth),
    format('~w (~w)~n', [Weight, Symbol]).

print_indent(0).
print_indent(N) :-
    N > 0,
    write('    '),
    N1 is N - 1,
    print_indent(N1).

% hucodec_decode(Bits, HuffmanTree, Message) :-
% 	% Add your implementation here
% 	true.

% hucodec_encode(Bits, HuffmanTree, Message) :-
% 	% Add your implementation here
% 	true.

% hucodec_encode_file(Filename, HuffmanTree, Bits) :-
% 	% Add your implementation here
% 	true.

% hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
% 	% Add your implementation here
% 	true.

% hucodec_print_huffman_tree(HuffmanTree) :-
% 	% Add your implementation here
% 	true.

% Query di esempio (senza debug)
% ?- hucodec_generate_huffman_tree([sw(a, 5), sw(b, 9), sw(c, 12)], Tree).
% Dovrebbe restituire: Tree = node(26, node(14, node(9, b), node(5, a)), node(12, c)).
                                                   
%hucodec_generate_huffman_tree([sw(a, 5), sw(b, 9), sw(c, 12)], Tree), hucodec_print_huffman_tree(Tree), !.                                               