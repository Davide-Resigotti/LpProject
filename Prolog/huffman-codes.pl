?- assert(symbols_n_weights(<symbols-n-weights>)).
true.
?- assert(message(<some-message>)).
true.
?- symbol_n_weights(SWs),
| message(M),
| hucodec_generate_huffman_tree(SWs, HT),
| hucodec_encode(M, HT, Bits),
| hucodec_decode(Bits, HT, M).
true ...