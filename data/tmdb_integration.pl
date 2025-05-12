% tmdb_integration.pl
:- module(tmdb_integration, [
    set_tmdb_api_key/1,
    load_all_user_films_metadata/1
]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(apply)).
:- use_module(library(uri)).
:- dynamic
    tmdb_api_key/1,
    film_tmdb_id/2,
    film_genre/2,
    film_actor/2,
    film_director/2.

%% set_tmdb_api_key(+Key)
%% Configura a chave de API da TMDb para uso em chamadas subsequentes.
set_tmdb_api_key(Key) :-
    retractall(tmdb_api_key(_)),
    assertz(tmdb_api_key(Key)).

%% load_all_user_films_metadata(+UserID)
%% Busca e armazena metadados TMDb para todos os filmes do usuário.
load_all_user_films_metadata(UserID) :-
    (   tmdb_api_key(_) -> true
    ;   throw(error(api_key_not_set, _))
    ),
    findall(Name-ID,
        ( db2(UserID, film, ID),
          db(ID, name, Name)
        ), Pairs),
    forall(member(Name-TMLocalID, Pairs),
           load_film_metadata(Name, TMLocalID)).

%% load_film_metadata(+FilmName, +LocalID)
%% Busca ID, detalhes e créditos do filme na TMDb e assert facts.
load_film_metadata(FilmName, LocalID) :-
    tmdb_search_movie(FilmName, TMDBID),
    assertz(film_tmdb_id(LocalID, TMDBID)),
    tmdb_fetch_details(TMDBID, Genres),
    forall(member(G, Genres), assertz(film_genre(LocalID, G))),
    tmdb_fetch_credits(TMDBID, Actors, Directors),
    forall(member(A, Actors), assertz(film_actor(LocalID, A))),
    forall(member(D, Directors), assertz(film_director(LocalID, D))).

%% tmdb_search_movie(+Query, -TMDBID)
%% Retorna o primeiro resultado de busca por nome de filme.
tmdb_search_movie(Query, ID) :-
    tmdb_api_key(Key),
    uri_encode(Query, Enc),
    format(atom(URL),
           'https://api.themoviedb.org/3/search/movie?api_key=~w&query=~w',
           [Key, Enc]),
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In),
    (   Dict.results = [First|_] -> ID = First.id
    ;   throw(error(movie_not_found(Query), _))
    ).

%% tmdb_fetch_details(+TMDBID, -Genres)
%% Extrai nomes de gêneros do filme.
tmdb_fetch_details(ID, Genres) :-
    tmdb_api_key(Key),
    format(atom(URL),
           'https://api.themoviedb.org/3/movie/~w?api_key=~w',
           [ID, Key]),
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In),
    findall(Name, (member(G, Dict.genres), Name = G.name), Genres).

%% tmdb_fetch_credits(+TMDBID, -Actors, -Directors)
%% Obtém elenco principal (até 5) e diretores.
tmdb_fetch_credits(ID, Actors, Directors) :-
    tmdb_api_key(Key),
    format(atom(URL),
           'https://api.themoviedb.org/3/movie/~w/credits?api_key=~w',
           [ID, Key]),
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In),
    findall(Name,
            ( nth1(N, Dict.cast, C), N =< 5, Name = C.name ),
            Actors),
    findall(Name,
            ( member(C, Dict.crew), C.job == "Director", Name = C.name ),
            Directors).
