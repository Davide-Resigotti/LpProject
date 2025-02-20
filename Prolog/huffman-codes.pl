% hucodec_decode(Bits, HuffmanTree, Message) :-
% 	% Add your implementation here
% 	true.

% hucodec_encode(Bits, HuffmanTree, Message) :-
% 	% Add your implementation here
% 	true.

% hucodec_encode_file(Filename, HuffmanTree, Bits) :-
% 	% Add your implementation here
% 	true.

% hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
% 	% Add your implementation here
% 	true.

% Caso base: Un solo simbolo (o nodo albero finale).
hucodec_generate_huffman_tree([sw(SymbolOrNode, Weight)], (Weight, SymbolOrNode)).

% Caso ricorsivo:
hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
    sort(2, @=<, SymbolsAndWeights, SortedSymbols), % Ordina per peso (posizione 2).
    SortedSymbols = [sw(S1, W1), sw(S2, W2) | Rest], % Estrae i due elementi minori.
    extract_symbol_or_node(sw(S1, W1), Node1),
    extract_symbol_or_node(sw(S2, W2), Node2),
    NewWeight is W1 + W2,                        % Calcola il nuovo peso.
    NewNode = (NewWeight, Node1, Node2),             % Crea il nuovo nodo albero.

    % Inserisce il nuovo nodo nella lista, usando il formato sw e riordina.
    append([sw(NewNode, NewWeight)], Rest, TempListUnsorted),
    sort(2, @=<, TempListUnsorted, TempList),
    hucodec_generate_huffman_tree(TempList, HuffmanTree). % Chiamata ricorsiva.

extract_symbol_or_node(sw(Symbol, _), Symbol) :- atom(Symbol).
extract_symbol_or_node(sw(Node, _), Node) :- compound(Node).

% hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable) :-
% 	% Add your implementation here
% 	true.

% hucodec_print_huffman_tree(HuffmanTree) :-
% 	% Add your implementation here
% 	true.