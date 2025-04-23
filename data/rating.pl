:- encoding(utf8).
:- use_module(library(persistency)).

% Definição da tabela de ratings: cada tuplo liga (UserID, FilmID) a (Stars, Review)
:- persistent
    rating(user:atom, film:atom, stars:atom, review:atom).

% Ficheiro onde os dados ficam guardados
:- db_attach('ratingdb.pl', [create(true)]).

%% set_rating(+User, +Film, +StarsAtom, +ReviewAtom)
%% Insere ou atualiza a avaliação do User para o Film
set_rating(User, Film, StarsAtom, ReviewAtom) :-
    % Remove qualquer rating anterior
    retractall_rating(User, Film, _, _),
    % Insere o novo
    assert_rating(User, Film, StarsAtom, ReviewAtom).

%% get_rating(+User, +Film, -StarsAtom, -ReviewAtom)
%% Retorna a avaliação atual (se existir)
get_rating(User, Film, StarsAtom, ReviewAtom) :-
    rating(User, Film, StarsAtom, ReviewAtom).
