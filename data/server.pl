% server.pl
:- encoding(utf8).
:- use_module(library(http/http_session)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(date)).
:- use_module(library(uri)).
:- ensure_loaded('users.pl').  % User management and movie DB
:- use_module(library(http/http_files)). % Serve static files
:- ensure_loaded('recommend.pl'). % Question-based recommendations
:- use_module(tmdb_integration).
:- ensure_loaded('tmdb_integration.pl'). % TMDb integration
:- initialization(set_tmdb_api_key('bccc509894efa9e817a1152273191223')).
:- use_module(rating).            % novo
:- initialization(init_ratings_db).
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
:- http_handler(root(ratefilm),      rate_film_page,      []).   % GET
:- http_handler(root(ratefilm_submit), rate_film_submit,   []).   % POST
:- http_handler(root(myratings),     show_ratings_page,      []).   % GET lista de ratings
:- http_handler(root(allfilms), all_films_page, []).
:- http_handler(root(removefilm), remove_film_page, []).
:- http_handler(root(removefilm_submit), remove_film_submit, []).
:- http_handler(root(film), film_page, []).

% —————————————————————————————
% 1. Configure sua chave TMDb aqui
% —————————————————————————————
tmdb_api_key('bccc509894efa9e817a1152273191223').
tmdb_base_url("https://api.themoviedb.org/3").

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
    % build your button CSS as a single‐line atom (no backslash continuations!)
    BtnStyle = 'font-family: \"Copperplate\", sans-serif; font-size: 17px;
                font-weight: 300; color: #222; margin:10px; padding:4px 8px;
                background:#ddd; border:none; border-radius:4px; text-decoration:none;',
    % hover‐in/out JS stays exactly the same
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Movie App Home', [
      h1('Welcome to Prolog, the Movie Recommender'),
      p('Tears were shed making this :)'),
      div([class(menu_container)], [
        p([class(menu_item)], a([ href('/register'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Register')),
        p([class(menu_item)], a([ href('/login'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Login')),
        p([class(menu_item)], a([ href('/recommend'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Get Recommendations')),
        p([class(menu_item)], a([ href('/showfilms'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Show Your Films')),
        p([class(menu_item)], a([ href('/myratings'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Show Your Ratings')),
        p([class(menu_item)], a([ href('/allfilms'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Show All Films')),
        p([class(menu_item)], a([ href('/logout'),
                                  style(BtnStyle),
                                  onmouseover(HoverIn),
                                  onmouseout(HoverOut)
                                ], 'Logout'))
      ])
    ]).



%% Registration Page: displays a form for new user registration.
register_page(_Request) :-
    % same button/link style as home_page
    BtnStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px;
               font-weight: 300; color: #222;  padding:4px 8px;
               background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    InputStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px;
               font-weight: 300; color: #222; margin:10px; padding:4px 8px;
               background:#ddd; border:none; border-radius:4px; text-decoration:none;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Register', [
      h1('Register New User'),
      form([ action('/register_submit'), method('post') ], [
        p([], [
          label([for(name)], 'Username:'),
          input([name(name), type(text),
                 style(InputStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ])
        ]),
        p([], [
          label([for(password)], 'Password:'),
          input([name(password), type(password),
                 style(InputStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ])
        ]),
        p([], input([type(submit), value('Register'),
                     style(BtnStyle),
                     onmouseover(HoverIn),
                     onmouseout(HoverOut)
                   ]))
      ]),
      p(a([ href('/'), style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
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

    % same styled button/link as the other pages
    BtnStyle = 'font-family: \"Copperplate\", sans-serif; font-size: 17px;
               font-weight: 300; color: #222; margin:10px; padding:4px 8px;
               background:#ddd; border:none; border-radius:4px; text-decoration:none;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Registration Result', [
        h1('Registration'),
        p(Message),
        p(a([ href('/'),
              style(BtnStyle),
              onmouseover(HoverIn),
              onmouseout(HoverOut)
            ], 'Return Home'))
    ]).

%% Login Page: presents a login form.
login_page(_Request) :-
    % shared button/link style
    BtnStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px;
               font-weight: 300; color: #222;  padding:4px 8px;
               background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    InputStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px;
               font-weight: 300; color: #222; margin:10px; padding:4px 8px;
               background:#ddd; border:none; border-radius:4px; text-decoration:none;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Login', [
      h1('User Login'),
      form([ action('/login_submit'), method('post') ], [
        p([], [
          label([for(name)], 'Username:'),
          input([ name(name), type(text),
                  style(InputStyle),
                  onmouseover(HoverIn),
                  onmouseout(HoverOut)
                ])
        ]),
        p([], [
          label([for(password)], 'Password:'),
          input([ name(password), type(password),
                  style(InputStyle),
                  onmouseover(HoverIn),
                  onmouseout(HoverOut)
                ])
        ]),
        p([], 
          button([ type(submit),
                   style(BtnStyle),
                   onmouseover(HoverIn),
                   onmouseout(HoverOut)
                 ], 'Login')
        )
      ]),
      p(a([ href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
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

    % reuse the same style
    BtnStyle = 'font-family: \"Copperplate\", sans-serif; font-size: 17px;
               font-weight: 300; color: #222; margin:10px; padding:4px 8px;
               background:#ddd; border:none; border-radius:4px; text-decoration:none;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Login Result', [
        h1('Login'),
        p(Message),
        p(a([ href('/'),
              style(BtnStyle),
              onmouseover(HoverIn),
              onmouseout(HoverOut)
            ], 'Return Home'))
    ]).


%% Logout Handler: logs out the user.
logout_handler(_Request) :-
    http_session_retractall(user(_)),
    page_wrapper('Logout', [
        h1('Logout'),
        p('You have been logged out.'),
        p(a([ href('/'),
               style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;'),
               onmouseover("this.style.background='#ccc';"),
               onmouseout("this.style.background='#ddd';")
             ],
             'Return Home'))
    ]).


%% Recommendation Page: shows to options of Recommendation system
%% Get a Recommendation based on my Film List
%% Get a Recommendation based on specific questions
recommendation_page(_Request) :-
    % reuse the same button style + hover you’ve used elsewhere
    BtnStyle = 'font-family: "Copperplate", sans-serif;
               font-size: 17px; font-weight: 300; color: #222;
               margin:25px; padding:4px 8px; background:#ddd;
               border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Recommendation', [
      h1('Get a Recommendation'),
      p('Choose one of the following options:'),
      div([class(menu_container)], [
        p(a([
             href('/recommend_myfilms'),
             style(BtnStyle),
             onmouseover(HoverIn),
             onmouseout(HoverOut)
           ], 'Based on Your Film List')),
        p(a([
             href('/recommend_questions'),
             style(BtnStyle),
             onmouseover(HoverIn),
             onmouseout(HoverOut)
           ], 'By Specific Questions'))
      ]),
      p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
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
recommend_questions_form(_Request) :-
    (   http_session_data(user(UserID))
    ->  true
    ;   UserID = none
    ),
    (   UserID == none
    ->  % Not logged in: pop-up + redirect
        reply_html_page(
          title('Recommendation – Login Required'),
          [ \current_user_info,
            script([], 'alert("Please login first"); window.location.href = "/login";')
          ]
        )
    ;   % Logged in: render the questions form
        page_wrapper('Recommendation by Questions', [
          h1('Movie Recommendations'),
          form([ action('/recommend_questions_result'),
                 method(get) ],
               [
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
                 p(input([
                     type(submit),
                     value('Show Recommendations'),
                     style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; cursor:pointer;'),
                     onmouseover("this.style.background='#ccc';"),
                     onmouseout("this.style.background='#ddd';")
                   ]))
               ]),
          p(a([ href('/'),
                 style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;'),
                 onmouseover("this.style.background='#ccc';"),
                 onmouseout("this.style.background='#ddd';")
               ],
               'Return Home'))
        ])
    ).

    
%% Recommendation based on specific questions (RESULT)
recommend_questions_result(Request) :-
    % 1. Get session user or none
    ( http_session_data(user(UserID)) -> true ; UserID = none ),

    % 2. If not logged in, pop up + redirect
    ( UserID == none ->
        reply_html_page(
          title('Recommendation – Login Required'),
          [ \current_user_info,
            script([], 'alert("Please login first"); window.location.href = "/login";')
          ])
    ; % 3. Logged in: read answers & compute
      http_parameters(Request, [
        ans1(A1,[optional(true),default('0')]),
        ans2(A2,[optional(true),default('0')]),
        ans3(A3,[optional(true),default('0')]),
        ans4(A4,[optional(true),default('0')]),
        ans5(A5,[optional(true),default('0')])
      ]),
      maplist(atom_number_default(0), [A1,A2,A3,A4,A5], [N1,N2,N3,N4,N5]),
      findall(F, recommend(N1,N2,N3,N4,N5,F), Raw),
      sort(Raw, Films),

      % 4. Build a single UL with a sub‐DCG call to list_films//1
      ( Films = [] ->
          FilmsBlock = [ p(style('font-family:"Copperplate",sans-serif;font-size:16px;color:#444;'),
                            'No matches found — try again with different answers.')
                       ]
      ; FilmsBlock = [ ul([ style('list-style:none;margin:20px 0;padding:0;') ],
                          \list_films(Films))
                     ]
      ),

      % 5. Button styling
      BtnStyle = 'font-family:"Copperplate",sans-serif;font-size:17px;font-weight:300;color:#222;
                  margin:10px;padding:4px 8px;background:#ddd;border:none;border-radius:4px;
                  text-decoration:none;cursor:pointer;',
      HoverIn  = "this.style.background='#ccc';",
      HoverOut = "this.style.background='#ddd';",

      % 6. Finally render
      page_wrapper('Your Movie Recommendations', [
          h1([style('font-family:"Copperplate",sans-serif;color:#222;')],
             'We recommend the following movies:'),
          div([ class(container) ], FilmsBlock),
          p(a([ href('/'),
                 style(BtnStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ], 'Return Home'))
      ])
    ).


%% Auxiliar: lista de <li> para cada filme
%% list_films(+Names)// 
%% Renders: <li><b>Name</b> (Year) [Add]</li>
list_films([]) --> [].
list_films([Title|T]) -->
  {
    db(Id, name, Title),
    db(Id, year, Year),
    LiStyle = 'margin-bottom:16px; list-style:none; font-family:"Copperplate",sans-serif;',
    format(atom(DetailHref), '/film?film_id=~w', [Id]),
    format(atom(YearStr),    "(~w)", [Year])
  },
  html(li([ style(LiStyle) ], [
    a([ href(DetailHref) ], b(Title)),  % now goes to your Film Page
    span([], ' '),
    span([], YearStr)
  ])),
  list_films(T).


%% Helper: convert an atom to an integer, defaulting to Default on failure
atom_number_default(Default, Atom, Num) :-
    catch(atom_number(Atom, N), _, N = Default),
    Num = N.

%% Gera um <p>label + <select> com as opções
render_question(Id, Label, Options, Selected) -->
    {
      format(atom(Name), 'ans~w', [Id])
    },
    html([
      p([ b(Label) ]),
      select([
          name(Name),
          style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300;
                 color: #222; margin:2px; padding:4px 8px; background:#ddd;
                 border:none; border-radius:4px; cursor:pointer;'),
          onmouseover("this.style.background='#ccc'; this.style.color='#000';"),
          onmouseout("this.style.background='#ddd'; this.style.color='#222';")
        ],
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
    BtnStyle = 'font-family: "Copperplate", sans-serif;
               font-size: 17px; font-weight: 300; color: #222;
               margin:10px; padding:4px 8px; background:#ddd;
               border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    (   http_session_data(user(_))
    ->  page_wrapper('Add Film', [
            h1('Add a Film to Your List'),
            form([action('/addfilm_submit'),method('post')], [
              p([], [
                label([for(film_id)], 'Film ID (e.g., tt0004972):'),
                input([name(film_id), type(text)])
              ]),
              p([], input([
                type(submit), value('Add Film'),
                style(BtnStyle),
                onmouseover(HoverIn),
                onmouseout(HoverOut)
              ]))
            ]),
            p(a([
                href('/'),
                style(BtnStyle),
                onmouseover(HoverIn),
                onmouseout(HoverOut)
              ], 'Return Home'))
        ])
    ;   reply_html_page(
            title('Add Film – Login Required'),
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
        p(a([href('/showfilms')], 'See My Films')),
        p(a([href('/allfilms')], 'See All Films'))
    ]).


%% Remove Film Page: If a user is logged in, displays a form to remove a film.
%% Otherwise, sends a JavaScript alert and redirects to login.
remove_film_page(_Request) :-
    BtnStyle = 'font-family: "Copperplate", sans-serif;
               font-size: 17px; font-weight: 300; color: #222;
               margin:10px; padding:4px 8px; background:#ddd;
               border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    (   http_session_data(user(_))
    ->  page_wrapper('Remove Film', [
            h1('Remove Film from Your List'),
            form([action('/removefilm_submit'),method('post')], [
              p([], [
                label([for(film_id)], 'Film ID (e.g., tt0004972):'),
                input([name(film_id), type(text)])
              ]),
              p([], input([
                type(submit), value('Remove Film'),
                style(BtnStyle),
                onmouseover(HoverIn),
                onmouseout(HoverOut)
              ]))
            ]),
            p(a([
                href('/'),
                style(BtnStyle),
                onmouseover(HoverIn),
                onmouseout(HoverOut)
              ], 'Return Home'))
        ])
    ;   reply_html_page(
            title('Remove Film – Login Required'),
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
    ;  
     
    findall(FilmName,
                ( db2(UserID, film, FilmID),
                  db(FilmID, name, FilmName)
                ),
                FilmsRaw),
        sort(FilmsRaw, Films),  % Remove duplicates and sort alphabetically
        films_html(Films, FilmHtml),
        
        page_wrapper('Your Films', [
            h1('Your Film List'),

            FilmHtml,
            % styled “Return Home” button
            p(a([ href('/'),
                   style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;'),
                   onmouseover("this.style.background='#ccc';"),
                   onmouseout("this.style.background='#ddd';")
                 ],
                 'Return Home'))
        ])
    ).


%% This DCG emits a “Remove” link iff there’s a logged‑in user.
remove_link(FilmId) -->
    {
        http_session_data(user(_)),  % only if logged in
        atom_concat('/removefilm_submit?film_id=', FilmId, Href)
    },
    html(a([ href(Href),
            style('font-family: "Copperplate", sans-serif; font-weight: bold; margin-left:20px; color:#007BFF; text-decoration:none; cursor:pointer;'),
            onmouseover("this.style.color='#0056FF'; this.style.textDecoration='underline';"),
            onmouseout("this.style.color='#007BFF'; this.style.textDecoration='none';")
          ], 'Remove')).
remove_link(_) --> [].  % otherwise, emit nothing

%% Rate link só se o user estiver em sessão.
rate_link(FilmId) -->
  {
      http_session_data(user(_)),                         % só se logged-in
      format(atom(Href), '/ratefilm?film_id=~w', [FilmId]) % monta href
  },
  html(a(
      [ href(Href),
        style('font-family: "Copperplate", sans-serif; font-weight: bold; margin-left:20px; color:#e67e22; text-decoration:none; cursor:pointer;'),
        onmouseover("this.style.color='#d35400'; this.style.textDecoration='underline';"),
        onmouseout("this.style.color='#e67e22'; this.style.textDecoration='none';")
      ],
      'Rate'
  )).
rate_link(_) --> [].


%% New helper that returns a chunk of HTML based on the list
films_html([], p('No films added.')).
films_html(Films, \list_film_elements(Films)).

%% Helper to render your films list with a Remove and rate link 
list_film_elements([]) --> [].
list_film_elements([Name|T]) -->
  {
    % Look up ID and optional year
    db(FilmId, name, Name),
    ( db(FilmId, year, Year)
    -> format(string(YearStr), " (~w)", [Year])
    ;  YearStr = ""
    ),
    % Build the link to our film_page/1
    format(string(DetailLink), "/film?film_id=~w", [FilmId])
  },
  html(p([
    % Film title now links to /film?film_id=…
    a([
        href(DetailLink),
        style('font-family:"Copperplate",sans-serif;color:#222;text-decoration:none;'),
        onmouseover("this.style.textDecoration='underline';"),
        onmouseout("this.style.textDecoration='none';")
      ],
      b(Name)
    ),
    span(YearStr),
    ' ',
    \remove_link(FilmId),
    \rate_link(FilmId)
  ])),
  list_film_elements(T).

%% GET: mostra o formulário de rating
rate_film_page(Request) :-
    http_session_data(user(_User)),
    http_parameters(Request, [ film_id(FilmId,[]) ]),
    ( db(FilmId,name,Title) -> true ; Title = FilmId ),

    % shared button style
    BtnStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    % shared select style
    SelStyle = 'font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:2px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; cursor:pointer;',

    page_wrapper(['Rate ',Title], [
        h1(['Rate ',Title]),
        form([action('/ratefilm_submit'), method(post)], [
            input([type(hidden), name(film_id), value(FilmId)]),

            p([
                label([for(stars)], 'Stars:'),
                select([
                    name(stars),
                    style(SelStyle),
                    onmouseover("this.style.background='#ccc';"),
                    onmouseout("this.style.background='#ddd';")
                ],
                [ option([value(0)], '0'),
                  option([value(1)], '1'),
                  option([value(2)], '2'),
                  option([value(3)], '3'),
                  option([value(4)], '4'),
                  option([value(5)], '5')
                ])
            ]),

            p([ label([for(review)], 'Review (optional):') ]),
            p([ textarea([ name(review),
                           rows(5),
                           cols(50),
                           style('font-family: "Copperplate", sans-serif; padding:4px;')
                         ], '') ]),

            p(input([
                type(submit),
                value('Submit'),
                style(BtnStyle),
                onmouseover(HoverIn),
                onmouseout(HoverOut)
            ]))
        ]),

        % “See My Films” button
        p(a([
            href('/showfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'See My Films')),

        % “Return Home” button
        p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
    ]).


%% POST: recebe a avaliação e guarda
rate_film_submit(Request) :-
    http_session_data(user(User)),
    http_parameters(Request, [
        film_id(FilmId, []),
        stars(StarsAtom, []),
        review(Review, [optional(true), default('')])
    ]),
    atom_number(StarsAtom, Stars),
    get_time(TS), stamp_date_time(TS, DT, local),
    set_rating(User, FilmId, Stars, Review, DT),

    % shared button style
    BtnStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Rating submitted!', [
        h1('Rating submitted!'),

        % “See My Films” button
        p(a([
            href('/showfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'See My Films')),

        % “See My Ratings” button
        p(a([
            href('/myratings'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'See My Ratings')),

        % “Return Home” button
        p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
    ]).


get_movie_title(MovieID, Title) :-
  db(MovieID, name, Title), !.
get_movie_title(_, 'Unknown Title').

%% Show Ratings Page: styled like show_films_page
show_ratings_page(_Request) :-
    (   http_session_data(user(User))
    ->  true
    ;   User = none
    ),
    (   User == none
    ->  reply_html_page(
            title('My Ratings - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ])
    ;   % fetch this user's ratings
        all_user_ratings(User, Ratings), % function in rating.pl
        filter_latest_ratings(Ratings, UnsortedRatings),
        sort_ratings_by_date_desc(UnsortedRatings, LatestRatings),


        % build the ratings block
        (   LatestRatings = []
        ->  Block = [ p('No ratings yet.') ]
        ;   Block = [ ul([ style('list-style:none; margin:20px 0; padding:0; font-family:"Copperplate",sans-serif;') ],
                          \rating_list_items(LatestRatings)) ]
        ),

        % common button style
        BtnStyle = 'font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
        HoverIn  = "this.style.background='#ccc';",
        HoverOut = "this.style.background='#ddd';",

        % render page
        page_wrapper('Your Ratings', [
          h1('Your Ratings'),
          div([class('ratings-list')], Block),

          % Return Home button
          p(a([ href('/'),
                 style(BtnStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ], 'Return Home')),

          % See My Films button
          p(a([ href('/showfilms'),
                 style(BtnStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ], 'See My Films'))
        ])
    ).


rating_list_items([]) --> [].
rating_list_items([rating(_,MovieID,Stars,Review,DT_utc)|Rest]) -->
  {
    % convert UTC datetime to local and format
    date_time_stamp(DT_utc, TS),
    stamp_date_time(TS, DT_local, local),
    format_time(atom(DateText), '%H:%M on %Y-%m-%d', DT_local),

    % look up title
    get_movie_title(MovieID, Title),

    % build optional review paragraph(s), now with quotes and margin-top
    ( Review \== '' ->
            ReviewBlock0 = [ div([ class(review),  style('margin-top:5px; font-size:15px; font-style:italic;') ],
                  ['"', Review, '"']) ]
    ;   ReviewBlock0 = []
    ),

    % build star‐span
    RatingStars = span([class(stars), style('font-weight:bold;')], [Stars, '★']),

    % append a horizontal rule after review block
    append( ReviewBlock0,
            [ hr([ style('border-top:1px solid #ccc; margin:10px 0;') ], []) ],
            ReviewBlock
          )
  },
  html(li([class(rating_item)], [
    h3([ style('margin-top:15px;') ], Title),
    div([class(details)], [
      RatingStars,
      span([class(date)], [' at ', DateText])
    ])
    % splice in the quoted, styled review + divider
    | ReviewBlock
  ])),
  rating_list_items(Rest).


% Filtra para manter apenas o rating mais recente de cada filme
filter_latest_ratings(Ratings, LatestRatings) :-
  group_by_movie(Ratings, Grouped),
  pick_latest_from_group(Grouped, LatestRatings).
% Ordena os ratings por data de forma decrescente
  sort_ratings_by_date_desc(Unsorted, Sorted) :-
    map_list_to_pairs(rating_timestamp, Unsorted, Pairs),
    keysort(Pairs, SortedAsc),        % keysort by ascending timestamp
    reverse(SortedAsc, Reversed),     % reverse to get descending
    pairs_values(Reversed, Sorted).   % extract the sorted ratings

rating_timestamp(rating(_, _, _, _, DateTime), Timestamp) :-
    date_time_stamp(DateTime, Timestamp).

% Agrupa os ratings pelo MovieID
group_by_movie(Ratings, Grouped) :-
  findall(MovieID, member(rating(_, MovieID, _, _, _), Ratings), MoviesDup),
  sort(MoviesDup, Movies),
  findall(Group, (
      member(MovieID, Movies),
      findall(rating(User, MovieID, Stars, Review, Timestamp),
              member(rating(User, MovieID, Stars, Review, Timestamp), Ratings),
              Group)
  ), Grouped).

% Para cada grupo escolhe o rating mais recente
pick_latest_from_group([], []).
pick_latest_from_group([RatingsList|Rest], [MostRecent|FilteredRest]) :-
  most_recent_rating(RatingsList, MostRecent),
  pick_latest_from_group(Rest, FilteredRest).


most_recent_rating([R], R).
most_recent_rating([R1, R2 | Rest], MostRecent) :-
    % só precisamos extrair o Timestamp de cada rating, daí o uso de '_' nos outros campos
    R1 = rating(_, _, _, _, DT1),
    R2 = rating(_, _, _, _, DT2),
    date_time_stamp(DT1, TS1),
    date_time_stamp(DT2, TS2),
    ( TS1 >= TS2 ->
        most_recent_rating([R1 | Rest], MostRecent)
    ;
        most_recent_rating([R2 | Rest], MostRecent)
    ).



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
    sort(YearListRaw, YearList),
    reverse(YearList, YearListDesc),

    % 4. Collect the country and genre options
    findall(C, db(_, country, C), CountryListRaw),
    sort(CountryListRaw, CountryList),
    findall(G, db(_, genre, G), GenreListRaw),
    sort(GenreListRaw, GenreList),

    % 5. Decide on result rendering
    ( FilmNames = []
    -> Results = [ \html(p('No films match those criteria.')) ]
    ;  Results = [ \html(
            ul([ style('list-style:none; margin:20px; padding:0; font-family: "Copperplate", sans-serif;') ],
               \film_list_items(FilmNames)))
       ]
    ),

    % 6. Build full page with styled “Return Home”
    page_wrapper('All Films in Database', [
        h1('Browse All Films'),
        \search_and_filter_form(QLower, Year, Country, Genre,
                                YearListDesc, CountryList, GenreList),
        \html(Results),
        p(a([ href('/'),
               style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;'),
               onmouseover("this.style.background='#ccc';"),
               onmouseout("this.style.background='#ddd';")
             ],
             'Return Home'))
    ]).

% —————————————————————————————
% 2. Predicado para buscar detalhes (sinopse + elenco)
% —————————————————————————————
get_tmdb_details(MovieId, Overview, ActorsList) :-
  tmdb_api_key(Key),
  % Monta a URL: movie/{id}?append_to_response=credits
  format(atom(URL),
         'https://api.themoviedb.org/3/movie/~w?api_key=~w&language=pt-BR&append_to_response=credits',
         [MovieId, Key]),
  % Faz a requisição HTTP
  setup_call_cleanup(
      http_open(URL, In, [timeout(20)]),
      json_read_dict(In, Dict),
      close(In)
  ),
  % Extrai overview (sinopse)
  (   get_dict(overview, Dict, Ov) 
  ->  Overview = Ov
  ;   Overview = 'Sinopse indisponível.'
  ),
  % Extrai elenco (cast): pega até 5 primeiros nomes
  (   get_dict(credits, Dict, Credits),
      get_dict(cast, Credits, CastList0)
  ->  findall(Name,
              ( nth1(I, CastList0, C), I =< 5,
                get_dict(name, C, Name)
              ),
              ActorsList)
  ;   ActorsList = []
  ).


%% ----------------------------------------------------------------------------
%% Film Page: shows details for the given film_id
%% URL: /film?film_id=tt1234567
%% ----------------------------------------------------------------------------
film_page(Request) :-
    % 1. Pull the film_id from the URL
    http_parameters(Request, [
      film_id(FilmId, [atom])
    ]),

    % 2. Lookup all the fields (default to 'Unknown'/'Unrated')
    ( db(FilmId, name,    Name)    -> true ; Name    = 'Unknown' ),
    ( db(FilmId, year,    Year)    -> true ; Year    = 'Unknown' ),
    ( db(FilmId, country, Country) -> true ; Country = 'Unknown' ),
    ( db(FilmId, producer, Producer) -> true ; Producer = 'Unknown' ),
    findall(G, db(FilmId, genre, G), Genres),
    ( db(FilmId, rating,  Rating) -> true ; Rating  = 'Unrated' ),

    % 3. Build the Genres paragraph
    ( Genres = [] ->
        GenresEl = p([b('Genres: '), 'None listed'])
    ; atomic_list_concat(Genres, ', ', GS),
      GenresEl = p([b('Genres: '), span(GS)])
    ),
    % — chama o TMDb para obter sinopse e elenco —
    get_tmdb_details(FilmId, Overview, ActorsList),
    PlotEl   = p([b('Sinopse: '), span(Overview)]),
    atomic_list_concat(ActorsList, ', ', ActorsStr),
    ActorsEl = p([b('Elenco: '), span(ActorsStr)]),
    % build imdb link
    format(atom(IMDbUrl), 'https://www.imdb.com/title/~w/', [FilmId]), IMDbLink = p([
    b('IMDb: '),
    a([ href(IMDbUrl),
        target('_blank'),
        style('font-family: "Copperplate", sans-serif; color: #1a0dab; text-decoration:underline;')
      ], IMDbUrl)
    ]),

    % 4. Common button styling
    BtnStyle = 'font-family: "Copperplate", sans-serif; font-size:17px;
                font-weight:300; color:#222; margin:10px; padding:4px 8px;
                background:#ddd; border:none; border-radius:4px;
                text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    % 5. Conditionally build “Add” and “Rate” buttons, side by side
    ( http_session_data(user(_)) ->
        format(atom(AddHref),  '/addfilm_submit?film_id=~w', [FilmId]),
        format(atom(RateHref), '/ratefilm?film_id=~w',        [FilmId]),
        Buttons = p([
          a([ href(AddHref),
              style('font-family: "Copperplate", sans-serif; font-size:17px;
                font-weight:300; background:#007BFF;color:#fff; margin:10px; padding:4px 8px;
                border:none; border-radius:4px;
                text-decoration:none; cursor:pointer;'),
              onmouseover("this.style.background='#0056FF';"),
              onmouseout("this.style.background='#007BFF';")
            ], 'Add'),
          a([ href(RateHref),
              style('font-family: "Copperplate", sans-serif; font-size:17px;
                font-weight:300; background:#e67e22;color:#fff; margin:10px; padding:4px 8px;
                border:none; border-radius:4px;
                text-decoration:none; cursor:pointer;'),
              onmouseover("this.style.background='#d35400';"),
              onmouseout("this.style.background='#e67e22';")
            ], 'Rate')
        ])
    ; Buttons = []
    ),

    % 6. Core film info
    Core = [
      h1(b(Name)),
      p([b('Year: '),      span(Year)]),
      p([b('Country: '),   span(Country)]),
      p([b('Director: '),span(Producer)]),
      p([b('Rating: '),    span(Rating)]),
      GenresEl,
      PlotEl,
      ActorsEl,
      IMDbLink,
      Buttons
    ],

    % 7. Footer navigation
    append(Core, [
      p(a([ href('/showfilms'),
             style(BtnStyle), onmouseover(HoverIn), onmouseout(HoverOut)
           ], 'Show Your Films')),
      p(a([ href('/allfilms'),
             style(BtnStyle), onmouseover(HoverIn), onmouseout(HoverOut)
           ], 'Show All Films')),
      p(a([ href('/'),
             style(BtnStyle), onmouseover(HoverIn), onmouseout(HoverOut)
           ], 'Return Home'))
    ], FullBody),

    % 8. Render
    page_wrapper([Name], FullBody).


%% This DCG emits an “Add” link iff there’s a logged‑in user.
add_link(FilmId) -->
    {
        % only show if someone’s logged in
        http_session_data(user(_)),
        % build the URL for your add‑film handler
        atom_concat('/addfilm_submit?film_id=', FilmId, Href)
    },
    html(a([ href(Href),
            style('font-family: "Copperplate", sans-serif;font-weight: bold;margin-left:20px;color:#007BFF; text-decoration:none; cursor:pointer;'),
            onmouseover("this.style.color='#0056FF'; this.style.textDecoration='underline';"),
            onmouseout("this.style.color='#007BFF'; this.style.textDecoration='none';")
           ],
           'Add')).
add_link(_) --> [].  % otherwise, emit nothing

%% film_list_items(+Names)// 
%% Renders each film as “Name (Year) – Rating [Add]” with spacing,
%% and injects the add_link//1 into the <li>.
film_list_items([]) --> [].
film_list_items([Name|T]) -->
      {
      db(Id, name, Name),
      db(Id, year, Year),
      LiStyle = 'margin-bottom:16px; list-style:none; font-family:"Copperplate",sans-serif;',
      format(atom(DetailHref), '/film?film_id=~w', [Id]),
      format(atom(YearStr),    "(~w)", [Year])
    },
    html(li([ style(LiStyle) ], [
      a([ href(DetailHref) ], b(Name)),  % now goes to your Film Page
      span([], ' '),
      span([], YearStr)
    ])),
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