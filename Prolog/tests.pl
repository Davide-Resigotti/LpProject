% once lo utilizziamo per evitare che Prolog cerchi altre soluzioni
% assertion ci permette di verificare che la condizione sia vera

:- consult('huffman-codes.pl'). 

:- begin_tests(hucodec).

test(generate_huffman_tree) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1),
    sw('o', 1)], Tree)),
    assertion(Tree \= []).

test(encode_file) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1),
    sw('o', 1)], Tree)),
    once(hucodec_encode_file('input.txt', Tree, 'encoded.txt')),
    assertion(exists_file('encoded.txt')).

test(decode) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1),
    sw('o', 1)], Tree)),
    once(hucodec_decode([1, 0, 1, 1, 0, 0, 0, 1], Tree, Message)),
    assertion(Message \= []).

test(generate_symbol_bits_table) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1),
    sw('o', 1)], Tree)),
    once(hucodec_generate_symbol_bits_table(Tree, SymbolBitsTable)),
    assertion(SymbolBitsTable \= []).

test(print_huffman_tree) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1),
    sw('o', 1)], Tree)),
    assertion(hucodec_print_huffman_tree(Tree)).

:- end_tests(hucodec).