% % Caso base: Un solo simbolo (o nodo albero finale).
% hucodec_generate_huffman_tree([sw(SymbolOrNode, _Weight)], SymbolOrNode).

% % Caso ricorsivo:
% hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree) :-
%     % Ordina per peso (posizione 2).
%     sort(2, @=<, SymbolsAndWeights, SortedSymbols),

%     % Estrai i due elementi di peso minore.
%     SortedSymbols = [sw(S1, W1), sw(S2, W2) | Rest],

%     % Estrai il simbolo o il nodo.
%     extract_symbol_or_node(sw(S1, W1), Node1),
%     extract_symbol_or_node(sw(S2, W2), Node2),

%     % Calcola il nuovo peso.
%     NewWeight is W1 + W2,

%     % Inverti l'ordine: il nodo con peso maggiore va a destra.
%  (W1 >= W2 -> Left = Node1, Right = Node2 ; Left = Node2, Right = Node1),

%     % Crea il nuovo nodo con la struttura corretta usando un functor.
%     NewNode = node(NewWeight, Left, Right),

%     % Inserisci il nuovo nodo nella lista e riordina.
%     append([sw(NewNode, NewWeight)], Rest, TempListUnsorted),
%     sort(2, @=<, TempListUnsorted, TempList),

%     % Chiamata ricorsiva.
%     hucodec_generate_huffman_tree(TempList, HuffmanTree).

% % Estrae un simbolo o un nodo.
% extract_symbol_or_node(sw(Symbol, _), leaf(Symbol)) :- atom(Symbol).
% extract_symbol_or_node(sw(Node, _), Node) :- compound(Node).

% % Funzione per stampare l'albero di Huffman.
% hucodec_print_huffman_tree(Tree) :-
%     print_tree(Tree, '').

% print_tree(leaf(S), Prefix) :-
%     format('~wLeaf: ~w~n', [Prefix, S]).
% print_tree(node(W, Left, Right), Prefix) :-
%     format('~wNode (Weight: ~d)~n', [Prefix, W]),
%     atom_concat(Prefix, 'L--> ', LeftPrefix),
%     atom_concat(Prefix, 'R--> ', RightPrefix),
%     print_tree(Left, LeftPrefix),
%     print_tree(Right, RightPrefix).

% % Query di esempio (senza debug)
% % ?- hucodec_generate_huffman_tree([sw(a, 5), sw(b, 9), sw(c, 12)], Tree), hucodec_print_huffman_tree(Tree).
% % Output atteso:
% % Node (Weight: 26)
% % L--> Node (Weight: 14)
% % L--> L--> Leaf: b
% % L--> R--> Leaf: a
% % R--> Leaf: c
