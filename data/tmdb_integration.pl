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

% store your TMDb API key.
set_tmdb_api_key(Key) :-
    retractall(tmdb_api_key(_)),
    assertz(tmdb_api_key(Key)).

% fetch and cache TMDb metadata for each film the user has added.
load_all_user_films_metadata(UserID) :-
    (   tmdb_api_key(_) -> true
    ;   throw(error(api_key_not_set, _))
    ),
    findall(Name-ID,
        ( db2(UserID, film, ID),
          db(ID, name, Name)
        ),
        Pairs),
    forall(member(Name-LocalID, Pairs),
           load_film_metadata(Name, LocalID)).

% look up the TMDb ID and then fetch details & credits.
load_film_metadata(_FilmName, LocalID) :-
    tmdb_search_movie(LocalID, TMDBID),
    assertz(film_tmdb_id(LocalID, TMDBID)),
    tmdb_fetch_details(TMDBID, Genres),
    forall(member(G, Genres), assertz(film_genre(LocalID, G))),
    tmdb_fetch_credits(TMDBID, Actors, Directors),
    forall(member(A, Actors),   assertz(film_actor(LocalID, A))),
    forall(member(D, Directors), assertz(film_director(LocalID, D))).

% uses the local film title to find the TMDb movie ID.
tmdb_search_movie(LocalID, TMDBID) :-
    db(LocalID, name, Query),
    tmdb_api_key(Key),
    uri_encoded(query_value, Query, EncQuery),
    format(atom(URL),
           'https://api.themoviedb.org/3/search/movie?api_key=~w&query=~w',
           [Key, EncQuery]),
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In),
    (   Dict.results = [First|_] -> TMDBID = First.id
    ;   throw(error(movie_not_found(Query), _))
    ).

%Pulls the list of genre names.
tmdb_fetch_details(ID, Genres) :-
    tmdb_api_key(Key),
    format(atom(URL),
           'https://api.themoviedb.org/3/movie/~w?api_key=~w',
           [ID, Key]),
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In),
    findall(G.name, member(G, Dict.genres), Genres).



%Grabs up to 5 cast members and all directors.
tmdb_fetch_credits(ID, Actors, Directors) :-
    tmdb_api_key(Key),
    format(atom(URL),
           'https://api.themoviedb.org/3/movie/~w/credits?api_key=~w',
           [ID, Key]),
    http_open(URL, In, []),
    json_read_dict(In, Dict),
    close(In),
    findall(C.name,
            ( nth1(N, Dict.cast, C), N =< 5 ),
            Actors),
    findall(C.name,
            ( member(C, Dict.crew), C.job == "Director" ),
            Directors).
