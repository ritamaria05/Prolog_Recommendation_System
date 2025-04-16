:- use_module(library(persistency)).
:- persistent db2(id:atom, property:atom, value:atom).
:- persistent db(id:atom, property:atom, value:atom).

:- db_attach('userdb.pl', []).  % Persistent database file
:- [movie].


:- nb_setval(current_user, none).

new_user(User, Pass) :-
    new_user(User, Pass, Message),
    writeln(Message).

new_user(Name, Password, Message) :-
    (   \+ db2(_, userName, Name)
    ->  assert_db2(Name, userName, Name),
        assert_db2(Name, password, Password),
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
    ;   \+ db2(Name, password, Password)
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
    % Try to get the current user from the HTTP session; if not found, fallback to the global variable
    (   http_session_data(user(UserID))
    ->  true
    ;   nb_getval(current_user, UserID)
    ),
    ( UserID == none
    ->  writeln('Login first')
    ;   ( \+ db2(UserID, film, FilmID)
        ->  assert_db2(UserID, film, FilmID),
            writeln('Successfully added')
        ;   writeln('Already added')
        )
    ).

%% Attempts to add the film and returns a message.
add_film_msg(FilmID, Message) :-
    (   http_session_data(user(UserID))
    ->  true 
    ;   UserID = none
    ),
    (   UserID == none
    ->  Message = 'No user logged in'
    ;   ( \+ db2(UserID, film, FilmID)
        ->  assert_db2(UserID, film, FilmID),
            Message = 'Film added to account'
        ;   Message = 'Film already added'
        )
    ).

%% remove_film_msg(+FilmID, -Message)
%% Attempts to remove FilmID for the logged‑in user and returns a message.
remove_film_msg(FilmID, Message) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   nb_getval(current_user, UserID)
    ),
    (   UserID == none
    ->  Message = 'No user logged in'
    ;   ( db2(UserID, film, FilmID)
        ->  retractall_db2(UserID, film, FilmID),
            Message = 'Film removed from account'
        ;   Message = 'Film not found'
        )
    ).


remove_film(FilmID) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   nb_getval(current_user, UserID)
    ),
    ( UserID == none
    ->  writeln('Login first')
    ;   ( db2(UserID, film, FilmID)
        ->  retractall_db2(UserID, film, FilmID),
            writeln('Film removed.')
        ;   writeln('Film not found')
        )
    ).


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



