STRINGHE IN INPUT
    Noi abbiamo ipotizzato la possibilità di usare diversi tipi di input.
    Per questo abbiamo utilizzato il file input.txt contente la stringa o le 
    stringhe da codificare.

        esempi:
            1) ciao
            2) c i a o 
            3) ciao
               ciao
            4) ciao ciao



COME ESEGUIRE I TEST ?
- sfruttando il file tests.pl per verificare la correttezza:
    1) swipl
    2) consult('tests.pl').
    3) run_tests.

- eseguendo singolarmente ogni funzione per verificarne manualmente il 
  funzionamento:

    - hucodec_generate_huffman_tree:

        NB: come primo argomento passiamo i Symbols And Weights in base alla 
        stringa contenuta in "input.txt"

            hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1), 
            sw('o', 1)], Tree).

    - hucodec_encode_file:

        NB: verrà creato il file "encoded.txt" contenente la codifica di 
        huffman per il contenuto di "input.txt"

            hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1),
            sw('o', 1)], Tree), hucodec_encode_file('input.txt', Tree, 
            'encoded.txt').

    - hucodec_decode:

        NB: nell'argomento della decode passiamo la codifica in "encode.txt" 
        con ogni singolo elemento separato da una virgola, otteniamo una lista
        rappresentante il contenuto di "input.txt"

            hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1), 
            sw('o', 1)], Tree), hucodec_decode([1, 0, 1, 1, 0, 0, 0, 1 ], 
            Tree, Message).


    - hucodec_generate_huffman_tree:
    
            hucodec_generate_huffman_tree([sw('c', 1), sw('i', 1), sw('a', 1), 
            sw('o', 1)], Tree), hucodec_print_huffman_tree(Tree).










