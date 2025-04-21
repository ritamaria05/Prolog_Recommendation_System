% server.pl
:- encoding(utf8).
:- use_module(library(http/http_session)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- ensure_loaded('users.pl').  % User management and movie DB
:- use_module(library(http/http_files)). % Serve static files
:- ensure_loaded('recommend.pl'). % Question-based recommendations

% Static file handlers
:- http_handler('/style.css', http_reply_file('style.css', []), []).
:- http_handler('/mascote.jpg', http_reply_file('mascote.jpg', []), []).

% HTTP Handlers
:- http_handler(root(.), home_page, []).
:- http_handler(root(register), register_page, []).
:- http_handler(root(register_submit), register_submit, []).
:- http_handler(root(login), login_page, []).
:- http_handler(root(login_submit), login_submit, []).
:- http_handler(root(logout), logout_handler, []).
:- http_handler(root(recommend), recommendation_page, []).
:- http_handler(root(recommend_myfilms), recommend_myfilms_page, []).
:- http_handler(root(recommend_questions), recommend_questions_form, []).
:- http_handler(root(recommend_questions_result), recommend_questions_result, []).
:- http_handler(root(addfilm), add_film_page, []).
:- http_handler(root(addfilm_submit), add_film_submit, []).
:- http_handler(root(showfilms), show_films_page, []).
:- http_handler(root(allfilms), all_films_page, []).
:- http_handler(root(removefilm), remove_film_page, []).
:- http_handler(root(removefilm_submit), remove_film_submit, []).

%%-----------------------------------------------------------------------
%% layout: injects the stylesheet, user‐info bar, and center_box wrapper
%%-----------------------------------------------------------------------
:- html_meta page_wrapper(+, html).

page_wrapper(Title, Body) :-
    reply_html_page(
        [ title(Title),
          meta([charset('UTF-8')], []),     % ← tell the browser it’s UTF‑8
          link([ rel(stylesheet),
                 type('text/css'),
                 href('/style.css')
               ], [])
        ],
        [ \current_user_info,
          div([class(center_box)], Body)
        ]).


%% Server launch predicate.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

%% Helper DCG that inserts current user info with profile image.
current_user_info -->
    {
        (   http_session_data(user(CurrUser))
        ->  format(string(Display), "Logged in as: ~w", [CurrUser])
        ;   Display = "Not logged in"
        )
    },
    html(div([class('user-info')], [
        a([href('/')], [img([src('/mascote.jpg'), class('mascote-pic')], [])]),
        p(Display)
    ])).




%% Home page with navigation links and current user info.
home_page(_Request) :-
    page_wrapper('Movie App Home', [
      h1('Welcome to Prolog, the Movie Recommender'),
      p('Tears were shed making this :)'),
      div([class(menu_container)], [
          p([class(menu_item)], a([href('/register')], 'Register')),
          p([class(menu_item)], a([href('/login')], 'Login')),
          p([class(menu_item)], a([href('/recommend')], 'Get Recommendations')),
          p([class(menu_item)], a([href('/addfilm')], 'Add Film')),
          p([class(menu_item)], a([href('/removefilm')], 'Remove Film')),
          p([class(menu_item)], a([href('/showfilms')], 'Show Your Films')),
          p([class(menu_item)], a([href('/allfilms')], 'Show All Films')),
          p([class(menu_item)], a([href('/logout')], 'Logout'))
      ])
    ]).


%% Registration Page: displays a form for new user registration.
register_page(_Request) :-
     page_wrapper('Register', [
       h1('Register New User'),
       form([ action('/register_submit'), method('post') ], [
         p([], [
           label([for(name)],     'Username:'),
           input([name(name), type(text)])
         ]),
         p([], [
           label([for(password)], 'Password:'),
           input([name(password), type(password)])
         ]),
         p([], input([type(submit), value('Register')]))
       ]),
       p(a([href('/')], 'Return Home'))
     ]).

%% Registration Form Handler: extracts parameters and calls new_user/2.
register_submit(Request) :-
    http_parameters(Request,
                    [ name(Name, []),
                      password(Pass, [])
                    ]),
    new_user(Name, Pass, Message),
    ( Message == 'New User added and logged in'
    -> http_session_retractall(user(_)),
       http_session_assert(user(Name))
    ; true
    ),
    page_wrapper('Registration Result', [
        h1('Registration'),
        p(Message),
        p(a([href('/')], 'Return Home'))
    ]).



%% Login Page: presents a login form.
login_page(_Request) :-
     page_wrapper('Login', [
       h1('User Login'),
       form([ action('/login_submit'), method('post') ], [
          p([], [
            label([for(name)],     'Username:'),
            input([name(name), type(text)])
          ]),
          p([], [
            label([for(password)], 'Password:'),
            input([name(password), type(password)])
          ]),
          p([], input([type(submit), value('Login')]))
        ]),
        p(a([href('/')], 'Return Home'))
     ]).

%% Login Handler: extracts parameters, calls login/3, and shows the result.
login_submit(Request) :-
    http_parameters(Request,
                    [ name(Name, []),
                      password(Pass, [])
                    ]),
    login(Name, Pass, Message),
    ( Message == 'Login Successful'
    -> http_session_retractall(user(_)),
       http_session_assert(user(Name))
    ; true
    ),
    page_wrapper('Login Result', [
        h1('Login'),
        p(Message),
        p(a([href('/')], 'Return Home'))
    ]).



%% Logout Handler: logs out the user.
logout_handler(_Request) :-
    http_session_retractall(user(_)),
    page_wrapper('Logout', [
        h1('Logout'),
        p('You have been logged out.'),
        p(a([href('/')], 'Return Home'))
    ]).

%% Recommendation Page: shows to options of Recommendation system
%% Get a Recommendation based on my Film List
%% Get a Recommendation based on specific questions
recommendation_page(_Request) :-
    page_wrapper('Recommendation', [
        h1('Get a Recommendation'),
        p('Choose one of the following options:'),
        div([class(menu_container)], [
            p([class(menu_item)], a([href('/recommend_myfilms')], 'Get a recommendation based on your film list')),
            p([class(menu_item)], a([href('/recommend_questions')], 'Get a recommendation based on specific questions'))
        ]),
        p(a([href('/')], 'Return Home'))
    ]).
%% Recommendation based on my film list
recommend_myfilms_page(_Request) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   UserID = none
    ),
    (   UserID == none
    ->  reply_html_page(
            title('Recommendation - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ]);   % Passo 1: Filmes que o usuário já viu
        findall(FilmID,
                db2(UserID, film, FilmID),
                UserFilmIDs),

        % Passo 2: Gêneros desses filmes
        findall(Genre,
                ( member(FID, UserFilmIDs),
                  db(FID, genre, Genre)
                ),
                GenresRaw),
        sort(GenresRaw, Genres),

        % Passo 3: Buscar filmes do mesmo género, que o usuário ainda não viu
        findall(RecommendedFilm,
                ( member(G, Genres),
                  db(RecFilmID, genre, G),
                  \+ member(RecFilmID, UserFilmIDs),
                  db(RecFilmID, name, RecommendedFilm)
                ),
                RecsRaw),
        sort(RecsRaw, Recs),

        % Passo 4: Gerar HTML e exibir recomendações
        films_html(Recs, RecHtml),
        page_wrapper('Recommended Films', [
            h1('Recommended Based on Your Film List'),
            (Recs == [] 
            ->
                (   p('No recommendations available.'),
                    p(a([href('/add_films')], 'Go to Add Films first'))
                )
            ;  % Caso haja recomendações
                RecHtml
            ),
            p(a([href('/')], 'Return Home'))
        ])
    ).

%% Recommendation based on specific questions (FORM)
recommend_questions_form(Request) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   UserID = none
    ),
    (   UserID == none
    ->  % Not logged in: pop up + redirect
        reply_html_page(
          title('Recommendation - Login Required'),
          [ \current_user_info,
            script([], 
              'alert("Please login first"); window.location.href = "/login";')
          ]
        )
    ;   % Logged in: render the questions form
        page_wrapper('Recommendation by Questions', [
          h1('Movie Recommendations'),
          form([action('/recommend_questions_result'),method(get)], [
            \render_question(1,'Do you prefer older movies (pre-2000)?',
                             ['No preference'=0,'Yes (pre-2000)'=1,'No, modern movies'=2],0),
            \render_question(2,'What types of movies are you in the mood for?',
                             ['No preference'=0,'Emotional'=1,'Historical'=2,
                              'Cerebral'=3,'Adventurous'=4,'Funny'=5],0),
            \render_question(3,'Do you have time for longer movies?',
                             ['No preference'=0,'Yes (>119min)'=1,'No (<120min)'=2],0),
            \render_question(4,'Which country\'s movie do you prefer?',
                             ['No preference'=0,'US'=1,'UK'=2,'Canada'=3,
                              'Japan'=4,'Korea'=5,'China'=6],0),
            \render_question(5,'Do you prefer high-scoring movies?',
                             ['No preference'=0,'Yes (>7)'=1],0),
            p(input([type(submit),value('Show Recommendations')] ))
          ]),
          p(a([href('/')],'Return Home'))
        ])
    ).
    
%% Recommendation based on specific questions (RESULT)
recommend_questions_result(Request) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   UserID = none
    ),
    (   UserID == none
    ->  % Not logged in: pop up + redirect
        reply_html_page(
          title('Recommendation - Login Required'),
          [ \current_user_info,
            script([], 
              'alert("Please login first"); window.location.href = "/login";')
          ]
        )
    ;   % Logged in: process answers and render results
        http_parameters(Request, [
          ans1(A1,[optional(true),default('0')]),
          ans2(A2,[optional(true),default('0')]),
          ans3(A3,[optional(true),default('0')]),
          ans4(A4,[optional(true),default('0')]),
          ans5(A5,[optional(true),default('0')])
        ]),
        maplist(atom_number_default(0), [A1,A2,A3,A4,A5], [N1,N2,N3,N4,N5]),
        findall(F, recommend(N1,N2,N3,N4,N5,F), Raw), sort(Raw, Films),
        ( Films == [] ->
            FilmsHtml = p('No matches found — try again with different answers.'),
            RetryLink = p(a([href('/recommend_questions')],'Try Again'))
        ;   films_html(Films, FilmsHtml),
            RetryLink = []
        ),
        page_wrapper('Recommendations by Questions', [
          h1('We recommend the following movies:'),
          FilmsHtml,
          RetryLink,
          p(a([href('/')],'Return Home'))
        ])
    ).


%% Gera um <p>label + <select> com as opções
render_question(Id, Label, Options, Selected) -->
    {
      format(atom(Name), 'ans~w', [Id])
    },
    html([
      p([ b(Label) ]),
      select([name(Name)], 
        \options_html(Options, Selected))
    ]).

options_html([], _) --> [].
options_html([Text=Val|T], Sel) -->
    {
      ( integer(Sel), Sel =:= Val -> Attrs=[value(Val),selected] ; Attrs=[value(Val)] )
    },
    html(option(Attrs, Text)),
    options_html(T, Sel).

recommend_list([]) --> [].
recommend_list([H|T]) -->
    html(li(H)),
    recommend_list(T).


%% Add Film Page: if a user is logged in, shows the add-film form.
%% Otherwise, sends a JavaScript pop-up alert and redirects to the login page.
add_film_page(_Request) :-
    (   http_session_data(user(_))
    ->  page_wrapper('Add Film', [
            h1('Add a Film to Your List'),
            form([action('/addfilm_submit'), method('post')],
                 [ p([], [label([for(film_id)], 'Film ID (e.g., tt0004972):'),
                          input([name(film_id), type(text)])]),
                   p([], input([type(submit), value('Add Film')]))
                 ])
        ])
    ;   reply_html_page(
            title('Add Film - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ])
    ).



%% Add Film Handler: calls add_film_msg/2 with the provided film ID.
add_film_submit(Request) :-
    http_parameters(Request, [ film_id(FilmID, []) ]),
    add_film_msg(FilmID, Message),
    page_wrapper('Add Film Result', [
        h1('Add Film'),
        p(Message),
        p(a([href('/')], 'Return Home')),
        p(a([href('/showfilms')], 'See My Films'))
    ]).


%% Remove Film Page: If a user is logged in, displays a form to remove a film.
%% Otherwise, sends a JavaScript alert and redirects to login.
remove_film_page(_Request) :-
    (   http_session_data(user(_))
    ->  page_wrapper('Remove Film', [
            h1('Remove Film from Your List'),
            form([action('/removefilm_submit'), method('post')],
                 [ p([], [label([for(film_id)], 'Film ID (e.g., tt0004972):'),
                          input([name(film_id), type(text)])]),
                   p([], input([type(submit), value('Remove Film')]))
                 ]),
            p(a([href('/')], 'Return Home'))
        ])
    ;   reply_html_page(
            title('Remove Film - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ])
    ).


%% Remove Film Handler: processes film removal requests.
remove_film_submit(Request) :-
    http_parameters(Request, [ film_id(FilmID, []) ]),
    remove_film_msg(FilmID, Message),
    page_wrapper('Remove Film Result', [
        h1('Remove Film'),
        p(Message),
        p(a([href('/')], 'Return Home')),
        p(a([href('/showfilms')], 'See My Films'))
    ]).



%% Show Films Page: shows the list of films for the currently logged in user.
%% If no user is logged in, it shows a pop-up and redirects to the login page.
show_films_page(_Request) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   UserID = none
    ),
    (   UserID == none
    ->  reply_html_page(
            title('Show Films - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ])
    ;   findall(FilmName,
                ( db2(UserID, film, FilmID),
                  db(FilmID, name, FilmName)
                ),
                FilmsRaw),
        sort(FilmsRaw, Films),  % Remove duplicates and sort alphabetically
        films_html(Films, FilmHtml),
        page_wrapper('Your Films', [
            h1('Your Film List'),
            FilmHtml,
            p(a([href('/')], 'Return Home'))
        ])
    ).



%% New helper that returns a chunk of HTML based on the list
films_html([], p('No films added.')).
films_html(Films, \list_film_elements(Films)).

%% Helper to render list items with year in brackets
list_film_elements([]) --> [].
list_film_elements([Name|T]) -->
    {
        db(FilmId, name, Name),
        ( db(FilmId, year, Year) -> format(string(YearStr), " (~w)", [Year]) ; YearStr = "" )
    },
    html(p([b(Name), span(YearStr)])),
    list_film_elements(T).


%% all_films_page(+Request)
%% Displays all films with a title‐search box and (–) filters removed for brevity.
all_films_page(Request) :-
    % 1. Parse GET parameters
    http_parameters(Request, [
        q(QAtom, [optional(true), default(''), atom]),
        year(Year, [optional(true), default(''), atom]),
        country(Country, [optional(true), default(''), atom]),
        genre(Genre, [optional(true), default(''), atom])
    ]),
    string_lower(QAtom, QLower),

    % 2. Collect matching film names based on filters
    findall(Name,
        ( db(FilmId, name, Name),
          ( QLower == '' -> true
          ; string_lower(Name, Lower), sub_string(Lower, _, _, _, QLower)
          ),
          ( Year == '' -> true ; atom_number(Year, YearNum), db(FilmId, year, YearNum) ),
          ( Country == '' -> true ; db(FilmId, country, Country) ),
          ( Genre == '' -> true ; db(FilmId, genre, Genre) )
        ),
        RawNames),
    sort(RawNames, FilmNames),

    % 3. Generate the dropdown for years in descending order
    findall(Y, db(_, year, Y), YearListRaw),
    sort(YearListRaw, YearList),  % Sort ascending
    reverse(YearList, YearListDesc),  % Reverse to descending order

    % 4. Collect the country and genre options
    findall(C, db(_, country, C), CountryListRaw),
    sort(CountryListRaw, CountryList),

    findall(G, db(_, genre, G), GenreListRaw),
    sort(GenreListRaw, GenreList),


    % 5. Decide on result rendering
    ( FilmNames = []
    -> Results = [ \html(p('No films match those criteria.')) ]
    ; Results = [ \html(ul([style('list-style:none; margin:10; padding:0; font-family: "Copperplate", sans-serif;')], \film_list_items(FilmNames))) ]
    ),

    % 6. Build full page
    page_wrapper('All Films in Database', [
        h1('Browse All Films'),
        \search_and_filter_form(QLower, Year, Country, Genre, YearListDesc, CountryList, GenreList),
        \html(Results),
        p(a([href('/')], 'Return Home'))
    ]).



%% film_list_items(+Names)// 
%% Renders each film as "<Name> – <Rating>" with extra spacing.
film_list_items([]) --> [].
film_list_items([Name|T]) -->
    {
        db(FilmId, name, Name),
        ( db(FilmId, year, Year) -> format(string(YearStr), " (~w)", [Year]) ; YearStr = "" ),
        ( db(FilmId, rating, Rating) -> true ; Rating = 'Unrated' )
    },
    html(li([ style('margin: 10px 0;') ],
        [ b(Name), span(YearStr), span(' – '), span(Rating) ])),
    film_list_items(T).


search_form(Query) -->
    html(form([method(get), action('/allfilms')], [
      input([type(text), name(q), value(Query), placeholder('Search…')]),
      input([type(submit), value('Search')])
    ])).


search_and_filter_form(Query, Year, Country, Genre, YearList, CountryList, GenreList) -->
    html(form([method(get), action('/allfilms')], [
        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Name:'),
        input([type(text), name(q), value(Query), placeholder('Search…'),
                style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none;')]),


        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Year:'),
        select([name(year),
                style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none;')],
               [option([value('')], 'Any') | \select_options(YearList, Year)]),

        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Country:'),
        select([name(country),
                style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none;')],
               [option([value('')], 'Any') | \select_options(CountryList, Country)]),

        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Genre:'),
        select([name(genre),
                style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none;')],
               [option([value('')], 'Any') | \select_options(GenreList, Genre)]),

        div([class('button-group')], [
            input([type(submit), value('Filter'),
                   style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none; cursor:pointer;')]),
            a([href('/allfilms'),
               style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; text-decoration:none;')],
              'Reset Filters')
        ])
    ])).



%% Render options with current selected
select_options([], _) --> [].
select_options([H|T], Selected) -->
    {
        % convert Selected (which is from form) to a number if possible
        ( catch(atom_number(Selected, SelectedNum), _, fail)
        ->  ( H == SelectedNum -> Opt = option([value(H), selected], H)
            ; Opt = option([value(H)], H)
            )
        ;   % fallback if Selected is not numeric
            ( H == Selected -> Opt = option([value(H), selected], H)
            ; Opt = option([value(H)], H)
            )
        )
    },
    html(Opt),
    select_options(T, Selected).



/*
To run the server, load this file into SWI-Prolog and run:

    ?- server(8080).

Then open your browser at http://localhost:8080/ to interact with your Movie App.
*/