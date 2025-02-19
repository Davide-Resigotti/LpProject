<<<<<<< HEAD

hucodec_decode(Bits, HuffmanTree, Message)

hucodec_encode(Bits, HuffmanTree, Message)

hucodec_encode_file(Filename, HuffmanTree, Bits)

hucodec_generate_huffman_tree(SymbolsAndWeights, HuffmanTree)

hucodec_generate_symbol_bits_table(HuffmanTree, SymbolBitsTable)

hucodec_print_huffman_tree(HuffmanTree)

=======
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
>>>>>>> a169989e607be60d413d0e16e6d541edb8e712e6
