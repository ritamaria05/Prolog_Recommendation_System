%server.pl
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
:- use_module(knn).
:- use_module(tmdb_integration).
:- ensure_loaded('tmdb_integration.pl'). % TMDb integration
:- initialization(set_tmdb_api_key('bccc509894efa9e817a1152273191223')). %API key
:- use_module(rating).
:- initialization(init_ratings_db).

% static file handlers for CSS and images
:- http_handler('/style.css', http_reply_file('style.css', []), []).
:- http_handler('/mascote.jpg', http_reply_file('mascote.jpg', []), []).

% site page handlers
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

%API base
tmdb_api_key('bccc509894efa9e817a1152273191223').
tmdb_base_url("https://api.themoviedb.org/3").

%-----------------------------------------------------------------------
% layout: injects the stylesheet, user‐info bar, and center_box wrapper
%-----------------------------------------------------------------------
:- html_meta page_wrapper(+, html).

page_wrapper(Title, Body) :-
    reply_html_page(
      % head
        [ title(Title),
          meta([charset('UTF-8')], []),     %tell the browser it’s UTF‑8
          link([ rel(stylesheet),
                 type('text/css'),
                 href('/style.css')
               ], [])
        ], %body
        [ \current_user_info,
          div([class(center_box)], Body)
        ]).


% server launch predicate.
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% helper DCG that inserts current user info with profile image.
current_user_info --> %arrow defines DCG
    {
      % check who current user is, if none display Not logged in
        (   http_session_data(user(CurrUser))
        ->  format(string(Display), "Logged in as: ~w", [CurrUser])
        ;   Display = "Not logged in"
        )
    },
    % dynamically show on html
    html(div([class('user-info')], [
        a([href('/')], [img([src('/mascote.jpg'), class('mascote-pic')], [])]),
        p(Display)
    ])).

%home page with navigation links and current user info.
home_page(_Request) :-
    % button style
    BtnStyle = 'font-family: \"Copperplate\", sans-serif; font-size: 17px;
                font-weight: 300; color: #222; margin:10px; padding:4px 8px;
                background:#ddd; border:none; border-radius:4px; text-decoration:none;',
    % hover‐in/out styles
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    page_wrapper('Movie App Home', [
      h1('Welcome to Prolog, the Movie Recommender'),
      p('All of your movie needs, in one place! :)'), %p([AttributeList], Content)
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



% registration page: displays a form for new user registration.
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
      h1('Register New User'), %submit data with post method
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

%registration form handler: extracts parameters and calls new_user
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

% login page: presents a login form.
login_page(_Request) :-
    % shared button style
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


% login handler: extracts parameters, calls login/3, and shows the result.
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


% logout Handler: logs out the user.
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


% Recommendation Page: shows two options of recommendation system
% get a recommendation based on your film list
% get a recommendation based on specific questions
recommendation_page(_Request) :-
    % button style
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
           ], 'Based on Your Films')),
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

% recommendation based on your film list, using the hybrid recommender
recommend_myfilms_page(_Request) :-
    % ensure user logged in
    (   http_session_data(user(UserID))
    ->  true
    ;   UserID = none
    ),
    % redirect if not logged in
    (   UserID == none
    ->  reply_html_page(
           title('Recommendation – Login Required'),
           [ \current_user_info,
             script([], 'alert("Please login first"); window.location.href="/login";')
           ]
        )
    ;   % compute hybrid recommendations
        K = 10, N = 10,
        ( hybrid_recommend(UserID, K, N, RecIDs, Counts)
        -> true
        ;  RecIDs = [], Counts = _{knn:0,genre:0,popular:0,random:0}
        ),
        % map to titles
        findall(Title,
          (
            member(X, RecIDs),
            (   X = _-FilmID        % if it was produced by KNN, X is of the form Score-FilmID
            ->  true
            ;   X = FilmID          % if it was produced by Genre (or Popular, or Random), X is just FilmID
            ),
            db(FilmID, name, Title)
          ),
          Titles),
        % button styling
        BtnStyle = 'font-family:"Copperplate",sans-serif; font-size:17px; font-weight:300; color:#222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
        HoverIn  = "this.style.background='#ccc';",
        HoverOut = "this.style.background='#ddd';",
        % render page: one DCG call for list + styled button
        page_wrapper('Recommended Films', [
            h1('Recommended for you'),
            p([
              'Contributions: ',
              'KNN=',Counts.knn,', ',
              'Genre=',Counts.genre,', ',
              'Popular=',Counts.popular,', ',
              'Random=',Counts.random
            ]),
            \render_recommendations(Titles),
            p(a([
                href('/'),
                style(BtnStyle),
                onmouseover(HoverIn),
                onmouseout(HoverOut)
              ], 'Return Home'))
        ])
    ).

% Helper DCG to either show links or fallback
render_recommendations([]) --> %"base case" no films (empty list)
    html([
      p('No recommendations available.'),
      p(a([
            href('/addfilm'),
            style('font-family:"Copperplate",sans-serif;color:#222;text-decoration:none;'),
            onmouseover("this.style.textDecoration='underline';"),
            onmouseout("this.style.textDecoration='none';")
          ],
          'Add more films to improve recommendations'
      ))
    ]).

render_recommendations(Titles) -->
    { Titles \= [] },  % works only when non-empty
    list_film_elements_rec(Titles).

% get films infos
list_film_elements_rec([]) --> []. % recursive base case
list_film_elements_rec([Name|T]) -->
  {
    % Look up ID and optional year ** (Condition -> Then ; Else) ***
    db(FilmId, name, Name),
    ( db(FilmId, year, Year)
    -> format(string(YearStr), " (~w)", [Year])
    ;  YearStr = "" %empty if no year
    ),
    % build the link
    format(string(DetailLink), "/film?film_id=~w", [FilmId])
  },
  html(p([
    a([
        href(DetailLink),
        style('font-family:"Copperplate",sans-serif;color:#222;text-decoration:none;'),
        onmouseover("this.style.textDecoration='underline';"),
        onmouseout("this.style.textDecoration='none';")
      ],
      b(Name)
    ),
    span(YearStr)
  ])),
  list_film_elements_rec(T).

% Recommendation based on specific questions (FORM - no login required)
recommend_questions_form(_Request) :-
    page_wrapper('Recommendation by Questions', [
      h1('Movie Recommendations'),
      form([ action('/recommend_questions_result'),
            method(get)
           ], [
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
                 style('font-family: \"Copperplate\", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; cursor:pointer;'),
                 onmouseover("this.style.background='#ccc';"),
                 onmouseout("this.style.background='#ddd';")
               ]))
           ]),
      p(a([ href('/'),
             style('font-family: \"Copperplate\", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;'),
             onmouseover("this.style.background='#ccc';"),
             onmouseout("this.style.background='#ddd';")
           ],
           'Return Home'))
    ]).

    
% recommendation based on specific questions (RESULT)
recommend_questions_result(Request) :-
    % Read answers & compute recommendations
    http_parameters(Request, [ % extract answer parameters, optional so if missing default is 0
        ans1(A1,[optional(true),default('0')]),
        ans2(A2,[optional(true),default('0')]),
        ans3(A3,[optional(true),default('0')]),
        ans4(A4,[optional(true),default('0')]),
        ans5(A5,[optional(true),default('0')])
    ]),
    maplist(atom_number_default(0), [A1,A2,A3,A4,A5], [N1,N2,N3,N4,N5]), %convert atoms to nums
    findall(F, recommend(N1,N2,N3,N4,N5,F), Raw),
    sort(Raw, Films), % find and sort recommended films

    % build recommendations block
    ( Films = [] ->
        Block = [ p(style('font-family:"Copperplate",sans-serif;font-size:16px;color:#444;'),
                    'No matches found — try again with different answers.') ]
    ;   Block = [ ul([ style('list-style:none; margin:20px 0; padding:0;') ],
                     \list_films(Films)) ]
    ),

    % button styling
    BtnStyle = 'font-family:"Copperplate",sans-serif; font-size:17px; font-weight:300; color:#222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

    % Render the page
    page_wrapper('Your Movie Recommendations', [
        h1([style('font-family:"Copperplate",sans-serif;color:#222;')],
           'We recommend the following movies:'),
        div([ class(container) ], Block),
        p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
    ]).


%Auxiliar: lista de <li> para cada filme
list_films([]) --> [].
list_films([Title|T]) -->
  {
    db(Id, name, Title),
    db(Id, year, Year),
    % same list‐item styling you already had:
    LiStyle   = 'margin-bottom:16px; list-style:none; font-family:"Copperplate",sans-serif;',
    % match link styling from list_film_elements//1:
    LinkStyle = 'font-family:"Copperplate",sans-serif;color:#222;text-decoration:none;',
    HoverIn   = "this.style.textDecoration='underline';",
    HoverOut  = "this.style.textDecoration='none';",
    format(atom(DetailHref), '/film?film_id=~w', [Id]),
    format(atom(YearStr),    "(~w)", [Year])
  },
  html(li([style(LiStyle)], [
    a([ href(DetailHref),
        style(LinkStyle),
        onmouseover(HoverIn),
        onmouseout(HoverOut)
      ],
      b(Title)
    ),
    span([], ' '),
    span([], YearStr)
  ])),
  list_films(T).


% Helper: convert an atom to an integer, defaulting to Default on failure
atom_number_default(Default, Atom, Num) :-
    catch(atom_number(Atom, N), _, N = Default), %catch to handle errors and not crash
    Num = N.

% gera um <p>label + <select> com as opções
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

options_html([], _) --> []. %base case
options_html([Text=Val|T], Sel) -->
    { %Checks if the current option value Val matches selected value Sel
      ( integer(Sel), Sel =:= Val -> Attrs=[value(Val),selected] ; Attrs=[value(Val)] )
    },
    html(option(Attrs, Text)), %option element created
    options_html(T, Sel).


% Add Film Page: if a user is logged in, shows the add-film form
% otherwise, sends a JavaScript pop-up alert and redirects to the login page.
add_film_page(_Request) :-
    BtnStyle = 'font-family: "Copperplate", sans-serif;
               font-size: 17px; font-weight: 300; color: #222;
               margin:10px; padding:4px 8px; background:#ddd;
               border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",

        %make sure user is logged in else create JS alert to login and send to login page
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
            title('Add Film - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ])
    ).


% Add Film handling submit
add_film_submit(Request) :-
    http_parameters(Request, [ film_id(FilmID, []) ]),
    add_film_msg(FilmID, Message),
    % button style
    BtnStyle = 'font-family:"Copperplate",sans-serif; font-size:17px; font-weight:300; color:#222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",
    % render page with styled buttons
    page_wrapper('Add Film Result', [
        h1('Add Film'),
        p(Message),
        p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
        ], 'Return Home')),
        p(a([
            href('/showfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
        ], 'See Your Films')),
        p(a([
            href('/allfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
        ], 'See All Films'))
    ]).


% Remove Film Page: If a user is logged in, displays a form to remove a film
% otherwise, sends a JavaScript alert and redirects to login.
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
            title('Remove Film - Login Required'),
            [ \current_user_info,
              script([], 'alert("Please login first"); window.location.href = "/login";')
            ])
    ).


% Remove Film Handler: processes film removal requests
remove_film_submit(Request) :-
    http_parameters(Request, [ film_id(FilmID, []) ]),
    remove_film_msg(FilmID, Message), % extract film ID and display message function in users.pl
    % button style
    BtnStyle = 'font-family:"Copperplate",sans-serif; font-size:17px; font-weight:300; color:#222; margin:10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;',
    HoverIn  = "this.style.background='#ccc';",
    HoverOut = "this.style.background='#ddd';",
    % render page with styled buttons
    page_wrapper('Remove Film Result', [
        h1('Remove Film'),
        p(Message),
        p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
        ], 'Return Home')),
        p(a([
            href('/showfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
        ], 'See Your Films'))
    ]).



% show films page: shows the list of films for the currently logged in user
% if no user is logged in, it shows a pop-up and redirects to the login page.
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
            sort(FilmsRaw, Films),  % remove duplicates and sort alphabetically
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


% This DCG emits a “Remove” link if there’s a logged in user.
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

% Rate link só se o user estiver em sessão.
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


% helper that returns a chunk of HTML based on the list
films_html([], p('No films added.')).
films_html(Films, \list_film_elements(Films)).

% helper to render your films list with a remove and rate link 
list_film_elements([]) --> [].
list_film_elements([Name|T]) -->
  {
    % look up ID and optional year
    db(FilmId, name, Name),
    ( db(FilmId, year, Year)
    -> format(string(YearStr), " (~w)", [Year])
    ;  YearStr = ""
    ),
    % build the link to our film_page/1
    format(string(DetailLink), "/film?film_id=~w", [FilmId])
  },
  html(p([
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

% GET: mostra o formulário de rating
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

        % “See Your Films” button
        p(a([
            href('/showfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'See Your Films')),

        % “Return Home” button
        p(a([
            href('/'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'Return Home'))
    ]).


% POST: recebe a avaliação e guarda
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

        % “See Your Films” button
        p(a([
            href('/showfilms'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'See Your Films')),

        % “See Your Ratings” button
        p(a([
            href('/myratings'),
            style(BtnStyle),
            onmouseover(HoverIn),
            onmouseout(HoverOut)
          ], 'See Your Ratings')),

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

show_ratings_page(_Request) :-
    (   http_session_data(user(User))
    ->  true
    ;   User = none
    ),
    (   User == none
    ->  reply_html_page(
            title('Your Ratings - Login Required'),
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

          % See Your Films button
          p(a([ href('/showfilms'),
                 style(BtnStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ], 'See Your Films')),

          % Return Home button
          p(a([ href('/'),
                 style(BtnStyle),
                 onmouseover(HoverIn),
                 onmouseout(HoverOut)
               ], 'Return Home'))
        ])
    ).


rating_list_items([]) --> []. %base case
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

% agrupa os ratings pelo MovieID
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



% Displays all films with a title‐search box and filters
all_films_page(Request) :-
    % Parse GET parameters
    http_parameters(Request, [
        q(QAtom, [optional(true), default(''), atom]),
        year(Year, [optional(true), default(''), atom]),
        country(Country, [optional(true), default(''), atom]),
        genre(Genre, [optional(true), default(''), atom])
    ]),
    string_lower(QAtom, QLower),

    % collect matching film names based on filters
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

    % enerate the dropdown for years in descending order
    findall(Y, db(_, year, Y), YearListRaw),
    sort(YearListRaw, YearList),
    reverse(YearList, YearListDesc),

    % Collect the country and genre options
    findall(C, db(_, country, C), CountryListRaw),
    sort(CountryListRaw, CountryList),
    findall(G, db(_, genre, G), GenreListRaw),
    sort(GenreListRaw, GenreList),

    % result rendering
    ( FilmNames = []
    -> Results = [ \html(p('No films match those criteria.')) ]
    ;  Results = [ \html(
            ul([ style('list-style:none; margin:20px; padding:0; font-family: "Copperplate", sans-serif;') ],
               \film_list_items(FilmNames)))
       ]
    ),

    % Build full page with styled “Return Home”
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

% predicado para buscar detalhes (sinopse + elenco)
get_tmdb_details(MovieId, Overview, ActorsList, PosterURL) :-
    tmdb_api_key(Key),
    format(atom(URL),
           'https://api.themoviedb.org/3/movie/~w?api_key=~w&language=en-US&append_to_response=credits',
           [MovieId, Key]),
    setup_call_cleanup(
        http_open(URL, In, [timeout(20)]),
        json_read_dict(In, Dict),
        close(In)
    ),

    %overview
    ( get_dict(overview, Dict, Ov) -> Overview = Ov ; Overview = 'No Synopsis Available.' ),

    % actors
    ( get_dict(credits, Dict, Credits),
      get_dict(cast, Credits, CastList0)
    ->  findall(Name,
                ( nth1(I, CastList0, C), I =< 5,
                  get_dict(name, C, Name)
                ),
                ActorsList)
    ;   ActorsList = []
    ),

    % poster path
    ( get_dict(poster_path, Dict, Path)
    -> atom_concat('https://image.tmdb.org/t/p/w500', Path, PosterURL)
    ;  PosterURL = ''  % fallback if no poster
    ).


% Film Page: shows details for the given film_id
film_page(Request) :-
    http_parameters(Request, [ film_id(FilmId, [atom]) ]),

    % base
    ( db(FilmId, name,    Name)    -> true ; Name    = 'Unknown' ),
    ( db(FilmId, year,    Year)    -> true ; Year    = 'Unknown' ),
    ( db(FilmId, country, Country) -> true ; Country = 'Unknown' ),
    ( db(FilmId, producer, Producer) -> true ; Producer = 'Unknown' ),
    findall(G, db(FilmId, genre, G), Genres),
    ( db(FilmId, rating,  Rating) -> true ; Rating  = 'Unrated' ),

    %genre formatting
    ( Genres = [] ->
        GenresHTML = 'None listed'
    ; atomic_list_concat(Genres, ', ', GenresHTML)
    ),

    % TMDb details
    get_tmdb_details(FilmId, Overview, ActorsList, PosterURL),
    atomic_list_concat(ActorsList, ', ', ActorsStr),
    format(atom(IMDbUrl), 'https://www.imdb.com/title/~w/', [FilmId]),

    % poster (if possible)
    ( PosterURL \== ''
    -> PosterEl = img([
           src(PosterURL),
           style('max-width:250px; border-radius:8px; box-shadow:0 2px 10px rgba(0,0,0,0.2);margin-left:40px;')
       ])
    ;  PosterEl = div([], [])
    ),

    % Add/Rate buttons (if logged in)
    ( http_session_data(user(_)) ->
        format(atom(AddHref), '/addfilm_submit?film_id=~w', [FilmId]),
        format(atom(RateHref), '/ratefilm?film_id=~w', [FilmId]),
        Buttons = [
          a([ href(AddHref),
              style('font-family:"Copperplate",sans-serif;font-weight:bold;
                     margin-right:10px;background:#007BFF;color:#fff;
                     border:none;border-radius:4px;cursor:pointer;
                     text-decoration:none;padding:4px 8px;'),
              onmouseover("this.style.background='#0056FF';"),
              onmouseout("this.style.background='#007BFF';")
            ], 'Add to Your Films'),
          a([ href(RateHref),
              style('font-family:"Copperplate",sans-serif;font-weight:bold;
                     background:#e67e22;color:#fff;border:none;
                     border-radius:4px;cursor:pointer;text-decoration:none;padding:4px 8px;'),
              onmouseover("this.style.background='#d35400';"),
              onmouseout("this.style.background='#e67e22';")
            ], 'Rate')
        ]
    ; Buttons = []
    ),

    % Ratings block
    findall(rating(User, FilmId, Stars, Review, TS),
            rating(User, FilmId, Stars, Review, TS),
            RatingsForFilm),

    ( RatingsForFilm == []
    -> ReviewsBlock = [h2([style('font-family:"Copperplate",sans-serif;margin-top:40px;padding-bottom:10px;border-bottom:1px solid #ccc;font-weight:bold;')], 'User Reviews'),
                       p(style('font-family:"Copperplate",sans-serif;font-size:16px;color:#444;padding-bottom:15px;border-bottom:1px solid #ccc;'),
                          'No reviews added yet.') ]
    ; ReviewsBlock = [
        h2([style('font-family:"Copperplate",sans-serif;margin-top:40px;padding-bottom:10px;border-bottom:1px solid #ccc;font-weight:bold;')], 'User Reviews'),
        ul([ style('list-style:none; margin:20px 0; padding:0; font-family:"Copperplate",sans-serif;') ],
           \film_rating_items(RatingsForFilm))
      ]),

    % final HTML body
    page_wrapper(['Film: ', Name], [
      h1([style('text-align:center; font-family:"Copperplate",sans-serif; ')], Name),
      div([style('display:flex; align-items:flex-start; gap:50px; margin-top:30px; flex-wrap:wrap; max-width:100%;')], [
        PosterEl,
        div([style('font-family:"Copperplate",sans-serif; text-align:left; max-width:600px; min-width:250px; flex:1;margin-left:20px;')], [
          p([b('Year: '), Year]),
          p([b('Country: '), Country]),
          p([b('Director: '), Producer]),
          p([b('Rating: '), Rating]),
          p([b('Genres: '), GenresHTML]),
          p([b('Synopsis: '), span(Overview)]),
          p([b('Cast: '), span(ActorsStr)]),
          div([], [
            a([ href(IMDbUrl),
                target('_blank'),
                style('font-family:"Copperplate",sans-serif;font-weight:bold;
                      background:#f1c40f;color:#222;border:none;
                      border-radius:4px;cursor:pointer;text-decoration:none;
                      padding:4px 8px; margin-bottom:15px; display:inline-block;'),
                onmouseover("this.style.background='#d4ac0d';"),
                onmouseout("this.style.background='#f1c40f';")
              ], 'IMDb')
          ]),
          div([], Buttons)
        ])
      ]),
      div([], ReviewsBlock),
      p(a([ href('/'), style('font-family: "Copperplate", sans-serif; font-size: 17px; font-weight: 300; color: #222; margin:30px 10px 10px; padding:4px 8px; background:#ddd; border:none; border-radius:4px; text-decoration:none; cursor:pointer;'),
             onmouseover("this.style.background='#ccc';"),
             onmouseout("this.style.background='#ddd';") ],
           'Return Home'))
    ]).


% DCG to render each rating
film_rating_items([]) --> []. %base case
film_rating_items([rating(User, _, Stars, Review, DT)|Rest]) -->
  {
    format_time(atom(DateText), '%H:%M on %Y-%m-%d', DT),
    RatingStars = span([style('font-weight:bold;')], [Stars, '★']),
    ( Review \== ''
    -> ReviewBlock0 = [ div([ style('margin-top:5px; font-size:15px; font-style:italic;' )],
                            ['"', Review, '"']) ]
    ;  ReviewBlock0 = [] ),
    append(ReviewBlock0,
           [ hr([ style('border-top:1px solid #ccc; margin:10px 0;') ], []) ],
           ReviewBlock)
  },
  html(li([style('margin-bottom:20px;')], [
    h3([style('margin-top:10px;')], User),
    div([], [RatingStars, span([style('margin-left:10px;')], DateText)])
    | ReviewBlock
  ])),
  film_rating_items(Rest).


% Renders each film
film_list_items([]) --> []. %base case
film_list_items([Name|T]) -->
  {
    db(Id, name, Name),
    ( db(Id, year, Year) -> format(string(YearStr), " (~w)", [Year]) ; YearStr = "" ),
    format(string(DetailHref), "/film?film_id=~w", [Id])
  },
  html(
    p([
      a([
          href(DetailHref),
          style('font-family:"Copperplate",sans-serif;color:#222;text-decoration:none;'),
          onmouseover("this.style.textDecoration='underline';"),
          onmouseout("this.style.textDecoration='none';")
        ],
        b(Name)
      ),
      span(YearStr)
    ])
  ),
  film_list_items(T).

search_and_filter_form(Query, Year, Country, Genre, YearList, CountryList, GenreList) -->
    html(form([method(get), action('/allfilms')], [ %action specifies to which URL the form data should be sent
        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Name:'),
        input([type(text), name(q), value(Query), placeholder('Search…'),
                style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none;')]),


        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Year:'),
        select([name(year),
                style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222; margin:10px; padding:4px 8px; background:#ddd; border-radius:4px; border:none;')],
               [option([value('')], 'Any') | \select_options(YearList, Year)]),

        span([style('font-family: "Copperplate", sans-serif; font-size: 15px; font-weight: 300; color: #222;')], 'Country:'),
        select([name(country), %select is for dropdowns
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

% render options with current selected
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
To run the server, load  file into SWI-Prolog and run:
  ?- server(8080).
Then open a browser tab at http://localhost:8080/ to use site.
*/
