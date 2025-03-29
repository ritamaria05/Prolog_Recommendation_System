:- use_module(library(readutil)).

% 🚀 Inicia a importação dos 7 ficheiros TSV
import_all :-
    import_titles,  % Importa title.basics.tsv (done)
    import_akas,    % Importa title.akas.tsv
    import_ratings, % Importa title.ratings.tsv
    import_crew,    % Importa title.crew.tsv
    import_principals, % Importa title.principals.tsv
    import_names,   % Importa name.basics.tsv
    import_episode. % Importa title.episode.tsv

% 🟢 IMPORTA title.basics.tsv (título, ano, género)
import_titles :-
    open('IMDb_datasets/title.basics.tsv', read, S),
    process_titles(S),
    close(S).

process_titles(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file -> ! 
    ;   parse_title(Line, MovieID, Type, PrimaryTitle, OriginalTitle, isAdult,  startYear, endYear, runtimeMinutes,  Genre),
        assertz(title(MovieID, Type, PrimaryTitle, OriginalTitle, isAdult,  startYear, endYear, runtimeMinutes,  Genre)), % 🔹 Armazena na base de dados
        process_titles(S)
    ).

parse_title(Line, MovieID, Type, PrimaryTitle, OriginalTitle, isAdult,  startYear, endYear, runtimeMinutes,  Genre) :-
    get_field1(Line, F0, Rem1), atom_codes(MovieID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(Type, F1),
    get_field1(Rem2, F2, Rem3), atom_codes(PrimaryTitle, F2),
    get_field1(Rem3, F3, Rem4), atom_codes(OriginalTitle, F3),
    get_field1(Rem4, F4, Rem5), atom_codes(isAdult, F4),
    get_field1(Rem5, F5, Rem6), atom_codes(startYear, F5),
    get_field1(Rem6, F6, Rem7), atom_codes(endYear, F6),
    get_field1(Rem7, F7, Rem8), atom_codes(runtimeMinutes, F7),
    get_field1(Rem8, F8, _), atom_codes(Genre, F8).

% 🟢 IMPORTA title.akas.tsv (aka nomes alternativos)
import_akas :-
    open('IMDb_datasets/title.akas.tsv', read, S),
    process_akas(S),
    close(S).

process_akas(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file -> ! 
    ;   parse_aka(Line, MovieID, Ordering, Title, Region, Language, Types, Attributes, isOriginalTitle),
        assertz(aka(MovieID, Ordering, Title, Region, Language, Types, Attributes, isOriginalTitle)), % 🔹 Armazena na base de dados
        process_akas(S)
    ).

parse_aka(Line, MovieID, Ordering, Title, Region, Language, Types, Attributes, isOriginalTitle) :-
    get_field1(Line, F0, Rem1), atom_codes(MovieID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(Ordering, F1),
    get_field1(Rem2, F2, Rem3), atom_codes(Title, F2),
    get_field1(Rem3, F3, Rem4), atom_codes(Region, F3),
    get_field1(Rem4, F4, Rem5), atom_codes(Language, F4),
    get_field1(Rem5, F5, Rem6), atom_codes(Types, F5),
    get_field1(Rem6, F6, Rem7), atom_codes(Attributes, F6),
    get_field1(Rem7, F7, _), atom_codes(isOriginalTitle, F7).

% 🟢 IMPORTA title.ratings.tsv (avaliação IMDb)
import_ratings :-
    open('IMDb_datasets/title.ratings.tsv', read, S),
    process_ratings(S),
    close(S).

process_ratings(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file -> ! 
    ;   parse_rating(Line, MovieID, averageRating, numVotes),
        assertz(rating(MovieID, averageRating, numVotes)), % 🔹 Armazena na base de dados
        process_ratings(S)
    ).

parse_rating(Line, MovieID, averageRating, numVotes) :-
    get_field1(Line, F0, Rem1), atom_codes(MovieID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(averageRating, F1),
    get_field1(Rem2, F2, _), atom_codes(numVotes, F2).

% 🟢 IMPORTA outros ficheiros (crew, principals, names, episode) (seguir mesma lógica)
import_crew :- 
    open('IMDb_datasets/title.crew.tsv', read, S),
    process_crew(S),
    close(S).

process_crew(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file ->  !
    ;   parse_crew(Line, MovieID, Directors, Writers),
        assertz(crew(MovieID, Directors, Writers)),
        process_crew(S)
    ).

parse_crew(Line, MovieID, Directors, Writers) :-
    get_field1(Line, F0, Rem1), atom_codes(MovieID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(Directors, F1),
    get_field1(Rem2, F2, _), atom_codes(Writers, F2).

import_principals :- 
    open('IMDb_datasets/title.principals.tsv', read, S),
    process_principals(S),
    close(S).

process_principals(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file ->  !
    ;   parse_principals(Line, MovieID, Ordering, NomineeID, Category, Job, Characters),
        assertz(principals(MovieID, Ordering, NomineeID, Category, Job, Characters)),
        process_principals(S)
    ).

parse_principals(Line, MovieID, Ordering, NomineeID, Category, Job, Characters) :-
    get_field1(Line, F0, Rem1), atom_codes(MovieID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(Ordering, F1),
    get_field1(Rem2, F2, Rem3), atom_codes(NomineeID, F2),
    get_field1(Rem3, F3, Rem4), atom_codes(Category, F3),
    get_field1(Rem4, F4, Rem5), atom_codes(Job, F4),
    get_field1(Rem5, F5, _), atom_codes(Characters, F5).

import_names :- 
    open('IMDb_datasets/name.basics.tsv', read, S),
    process_names(S),
    close(S).

process_names(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file ->  !
    ;   parse_names(Line, NomineeID, primaryName, birthYear, deathYear, primaryProfession, knownForTitles),
        assertz(names(NomineeID, primaryName, birthYear, deathYear, primaryProfession, knownForTitles)),
        process_names(S)
    ).
parse_names(Line, NomineeID, primaryName, birthYear, deathYear, primaryProfession, knownForTitles):-
    get_field1(Line, F0, Rem1), atom_codes(NomineeID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(primaryName, F1),
    get_field1(Rem2, F2, Rem3), atom_codes(birthYear, F2),
    get_field1(Rem3, F3, Rem4), atom_codes(deathYear, F3),
    get_field1(Rem4, F4, Rem5), atom_codes(primaryProfession, F4),
    get_field1(Rem5, F5, _), atom_codes(knownForTitles, F5).

import_episode :- 
    open('IMDb_datasets/title.episode.tsv', read, S),
    process_episode(S),
    close(S).

process_episode(S) :-
    read_line_to_codes(S, Line),
    (   Line == end_of_file ->  !
    ;   parse_episode(Line, MovieID, ParentID, SeasonNumber, EpisodeNumber),
        assertz(episode(MovieID, ParentID, SeasonNumber, EpisodeNumber)),
        process_episode(S)
    ).

parse_episode(Line, MovieID, ParentID, SeasonNumber, EpisodeNumber) :-
    get_field1(Line, F0, Rem1), atom_codes(MovieID, F0),
    get_field1(Rem1, F1, Rem2), atom_codes(ParentID, F1),
    get_field1(Rem2, F2, Rem3), atom_codes(SeasonNumber, F2),
    get_field1(Rem3, F3, _), atom_codes(EpisodeNumber, F3).

% 📌 UNIR DADOS PARA CONSULTAS
movie_details(MovieID, Title, startYear, Genre, PrimaryTitle, averageRating) :-
    title(MovieID, "movie", PrimaryTitle, _, _,  startYear, _, _, Genre),
    aka(MovieID, _,  Title, _ , _ , _, _, _),
    rating(MovieID, averageRating, _).

movies_by_genre(Genre, Movies) :-
    findall(PrimaryTitle, title(_, _, PrimaryTitle, _, _, _, _, _, Genre), Movies).


% 📌 FUNÇÃO PARA EXTRAIR CAMPOS DO TSV
get_field1([9|Rest], [], Rest) :- !.   % Tab ASCII = 9
get_field1([C|Line], [C|F], Rem1) :-
    get_field1(Line, F, Rem1).
