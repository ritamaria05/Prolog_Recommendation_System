:- encoding(utf8).
:- use_module(library(persistency)).
:- persistent db2(id:atom, property:atom, value:atom).
:- persistent db(id:atom, property:atom, value:atom).
:- db_attach('userdb.pl', []).  % Persistent database file
:- [movie].


:- nb_setval(current_user, none).


:- use_module(library(crypto)).


hash_password(PlainPassword, HashedPassword) :-
    crypto_password_hash(PlainPassword, HashedPassword).

verify_password(PlainPassword, HashedPassword) :-
    crypto_password_hash(PlainPassword, HashedPassword).

new_user(User, Pass) :-
    new_user(User, Pass, Message),
    writeln(Message).

new_user(Name, Password, Message) :-
    (   \+ db2(_, userName, Name)
    ->  assert_db2(Name, userName, Name),
        hash_password(Password, HashedPassword),
        assert_db2(Name, password, HashedPassword),
        nb_setval(current_user, Name),
        Message = 'New User added and logged in'
    ;   Message = 'Username already exists.'
    ).


delete_user(Name) :-
    db2(ID, userName, Name),
    retractall_db2(ID, _, _),
    writeln('User deleted.').



% Updated login predicate that returns a result message.
login(User, Pass) :-
    login(User, Pass, Message),
    writeln(Message).
    
login(Name, Password, Message) :-
    (   \+ db2(Name, _, _) 
    ->  Message = 'User not found'
    ;   db2(Name, password, HashedPassword),
        \+ verify_password(Password, HashedPassword)
    ->  Message = 'Wrong Password'
    ;   nb_setval(current_user, Name),
        Message = 'Login Successful'
    ).


logout :-
    nb_setval(current_user, none).

get_logged_in_user :-
    nb_getval(current_user, Name),
    format('Current logged in user ID: ~w~n', [Name]).


add_film(FilmID) :-
    add_film_msg(FilmID, Message),
    writeln(Message).

%% add_film_msg(+FilmID, -Message)
%% Adds a film for the current user and returns a status message.
add_film_msg(FilmID, Message) :-
    get_current_user(UserID),
    (   UserID == none
    ->  Message = 'No user logged in'
    ;   ( \+ db2(UserID, film, FilmID)
        ->  assert_db2(UserID, film, FilmID),
            Message = 'Film added to account'
        ;   Message = 'Film already added'
        )
    ).


%% get_current_user(-UserID)
%% Tries to retrieve the current user from the HTTP session.  
%% If that predicate isn’t available or fails, it falls back to nb_getval/2.
get_current_user(UserID) :-
    (   current_predicate(http_session_data/1)
    ->  (   http_session_data(user(UserID)) 
        ->  true
        ;   UserID = none)
    ;   nb_getval(current_user, UserID)
    ).

%% remove_film_msg(+FilmID, -Message)
%% Attempts to remove FilmID for the current user and returns a message.
remove_film_msg(FilmID, Message) :-
    get_current_user(UserID),
    (   UserID == none
    ->  Message = 'No user logged in'
    ;   ( db2(UserID, film, FilmID)
        ->  retractall_db2(UserID, film, FilmID),
            Message = 'Film removed from account'
        ;   Message = 'Film not found'
        )
    ).

remove_film(FilmID) :-
    remove_film_msg(FilmID, Message),
    writeln(Message).

show_films :-
    nb_getval(current_user, UserID),
    (   UserID == none
    ->  writeln('Login first')
    ;   findall(FilmName,
                ( db2(UserID, film, FilmID),
                  db(FilmID, name, FilmName)
                ),
                FilmsRaw),
        sort(FilmsRaw, Films),  % Remove duplicates and sort the list
        (   Films == []
        ->  writeln('No films added.')
        ;   forall(member(Film, Films), writeln(Film))
        )
    ).




