:- use_module(library(persistency)).
:- persistent db2(id:atom, property:atom, value:atom).
:- persistent db(id:atom, property:atom, value:atom).

:- db_attach('userdb.pl', []).  % Persistent database file
:- [movie].


:- nb_setval(current_user, none).

new_user(Name, Password) :-
    \+ db2(_, userName, Name) ->
        assert_db2(Name, userName, Name),
        assert_db2(Name, password, Password),
        writeln('New User added');
        writeln('Username already exists.').

delete_user(Name) :-
    db2(ID, userName, Name),
    retractall_db2(ID, _, _),
    writeln('User deleted.').

login(Name, Password) :- 
    \+ db2(Name, _, _) ->
        writeln('User not found');
    \+ db2(Name, password, Password) ->
        writeln('Wrong Password');
    nb_setval(current_user, Name),
    write('Login Sucessful').

logout :-
    nb_setval(current_user, none).

get_logged_in_user :-
    nb_getval(current_user, Name),
    format('Current logged in user ID: ~w~n', [Name]).

add_film(FilmID) :-
    nb_getval(current_user, UserID),  % Get the logged-in user ID
    UserID = none ->
        writeln('Login first');
    \+ db2(UserID, _, FilmID) ->
        assert_db2(UserID, film, FilmID), % Use UserID as argument
        writeln('Sucessfully added');
    writeln('Already added').

remove_film(FilmID) :-
    nb_getval(current_user, UserID),
    UserID = none -> 
        writeln('Login in first');
    db2(UserID, film , FilmID) ->
        retractall_db2(UserID, film, FilmID),
        writeln('Film removed.');
    writeln('Film not found').

show_films :- 
    nb_getval(current_user, UserID),                  % Get the logged-in user ID
    UserID = none -> 
        writeln('Login in first');
    forall(db2(UserID, film, FilmID),                  % For all films associated with the user
        ( db(FilmID, name, FilmName),                  % Get the FilmName from db/3 (movie.pl)
          writeln(FilmName)                            % Print the FilmName
        )
    ).







