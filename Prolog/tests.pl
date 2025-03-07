% Using once to avoid multiple solutions
% Using assertion to check if the test is passed


:- consult('huffman-codes.pl'). 

:- begin_tests(hucodec).

test(generate_huffman_tree) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1), sw('o', 1)], Tree)),
    assertion(Tree \= []).

test(encode_file) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1), sw('o', 1)], Tree)),
    once(hucodec_encode_file('input.txt', Tree, 'encoded.txt')),
    assertion(exists_file('encoded.txt')).


test(decode_message) :-
    once(hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1), sw('o', 1)], Tree)),
    once(hucodec_decode([1, 0, 1, 1, 0, 0, 0, 1], Tree, Message)),
    assertion(Message \= []).

:- end_tests(hucodec).



