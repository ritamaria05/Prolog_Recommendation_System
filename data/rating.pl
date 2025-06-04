% rating.pl
%used as a module
:- module(rating, [
    init_ratings_db/0,
    set_rating/5,
    all_user_ratings/2,
    rating/5
]).

:- use_module(library(date)).
:- dynamic
    rating/5.     % rating(User,Movie,Stars,Review,Timestamp)

% Inicializa: se existir um ficheiro com ratings anteriores, carrega-o
init_ratings_db :-
    ( exists_file('ratingsdb.pl')
    -> consult('ratingsdb.pl')
    ;  true ).

% Insere sempre um novo fact; o mais recente "ganha"
set_rating(User, Movie, Stars, Review, Timestamp) :-
    assertz(rating(User, Movie, Stars, Review, Timestamp)),
    save_ratings_db.

% Lista todos os ratings de um User
all_user_ratings(User, Ratings) :-
    findall(rating(User, M, S, R, T),
            rating(User, M, S, R, T),
            Ratings).

% Grava em disco todos os facts rating/5
save_ratings_db :-
    open('ratingsdb.pl', write, Out),
    forall(rating(U,M,S,Rev,TS),
           portray_clause(Out, rating(U,M,S,Rev,TS))),
    close(Out).
