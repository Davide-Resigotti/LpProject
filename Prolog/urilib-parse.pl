% Bonzagni Giorgio 914562
% Resigotti Davide 914986

% Converte la stringa URI in una struttura uri(...)
urilib_parse(URIString, uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    % Divide la stringa in Scheme e il resto
    sub_string(URIString, SchemeEnd, 1, AfterScheme, ":"),
    sub_string(URIString, 0, SchemeEnd, _, Scheme),
    % Analizza il resto (es. verifica la presenza di "//")
    (   sub_string(URIString, SchemeEnd + 1, 2, _, "//")
    ->  % Analisi authority (Userinfo, Host, Port) e Path
        AuthorityStart is SchemeEnd + 3,
        parse_authority_and_path(URIString, AuthorityStart, Userinfo, Host, Port, Path, Query, Fragment)
    ;   % Solo Path, Query, Fragment (es. mailto:)
        parse_path_query_fragment(URIString, SchemeEnd + 1, Path, Query, Fragment),
        Userinfo = [], Host = [], Port = 80
    ).

% Predicato per analizzare Authority e Path
parse_authority_and_path(URIString, Start, Userinfo, Host, Port, Path, Query, Fragment) :-
    % TODO: Implementare la logica per separare userinfo, host, port, path, query, e fragment
    true.

% Predicato per analizzare Path, Query e Fragment
parse_path_query_fragment(URIString, Start, Path, Query, Fragment) :-
    % TODO: Implementare la logica per separare path, query, e fragment
    true.
