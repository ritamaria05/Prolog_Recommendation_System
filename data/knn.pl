:- [ratingsdb].
:- [movie].



% item_vector(Item, Vector) — vetor com tuplos User-Score
item_vector(Item, Vector) :-
    findall(User-Score, rating(User, Item, Score, _, _), Vector).

% Cálculo da similaridade de cosseno
cosine_similarity(Item1, Item2, Sim) :-
    item_vector(Item1, V1),
    item_vector(Item2, V2),
    dot_product(V1, V2, Dot),
    norm(V1, N1),
    norm(V2, N2),
    (N1 =:= 0.0 ; N2 =:= 0.0 -> Sim = 0 ; Sim is Dot / (N1 * N2)).

% Produto escalar
dot_product([], _, 0).
dot_product([U1-S1|T1], V2, Result) :-
    (   select(U1-S2, V2, RestV2)
    ->  dot_product(T1, RestV2, R),
        Result is S1 * S2 + R
    ;   dot_product(T1, V2, Result)
    ).

% Norma de um vetor
norm([], 0).
norm([_-S|T], N) :-
    norm(T, R),
    N is sqrt(S*S + R*R).

% Todos os itens diferentes de Item
other_items(Item, Others) :-
    setof(I, U^S^C^D^rating(U, I, S, C, D), All),
    exclude(==(Item), All, Others).

% Lista de Similaridade-Item
item_similarities(Item, List) :-
    other_items(Item, Others),
    findall(Sim-Other, (
        member(Other, Others),
        cosine_similarity(Item, Other, Sim)
    ), List).

% Top K mais parecidos
top_k_similar_items(Item, K, TopK) :-
    item_similarities(Item, Sims),
    sort(0, @>=, Sims, Sorted),
    length(TopK, K),
    append(TopK, _, Sorted).

% Agrupa e tira média de similaridade de cada item
aggregate_similarities(Sims, Aggregated) :-
    findall(Item-List, (
        setof(Sim, member(Sim-Item, Sims), List)
    ), Grouped),
    findall(Avg-Item, (
        member(Item-SimList, Grouped),
        average(SimList, Avg)
    ), Aggregated).


% Função para obter itens vistos pelo utilizador
seen_items(User, Seen) :-
    setof(Item, S^C^D^rating(User, Item, S, C, D), Seen).

% Função para obter itens que o utilizador gostou (avaliados >= 4)
liked_items(User, Liked) :-
    setof(Item, Score^C^D^(rating(User, Item, Score, C, D), Score >= 4), Liked).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recomenda filmes usando KNN (similaridade do cosseno)
recommend_items(User, K, N, Recommendations) :-
    liked_items(User, Liked),
    findall(Sim-Cand, (
        member(Item, Liked),
        top_k_similar_items(Item, K, TopK),
        member(Sim-Cand, TopK)
    ), AllCandidates),
    seen_items(User, Seen),
    exclude([_-C]>>(member(C, Seen)), AllCandidates, Unseen),
    aggregate_similarities(Unseen, Aggregated),
    sort(0, @>=, Aggregated, Sorted),
    length(Recommendations, N),
    append(Recommendations, _, Sorted).

% Recomenda filmes populares (baseado em avaliações positivas)
popular_items(N, List) :-
    findall(Score-Item, (
        rating(_, Item, Score, _, _),
        Score >= 4
    ), AllScores),
    aggregate_scores(AllScores, Aggregated),
    sort(0, @>=, Aggregated, Sorted),
    length(List, N),
    append(List, _, Sorted).

aggregate_scores(Pairs, Aggregated) :-
    findall(Item-Scores, (
        setof(Score, member(Score-Item, Pairs), Scores)
    ), Grouped),
    findall(Avg-Item, (
        member(Item-Scores, Grouped),
        average(Scores, Avg)
    ), Aggregated).

% Média de uma lista
average(List, Avg) :-
    sum_list(List, Sum),
    length(List, Len),
    (Len > 0 -> Avg is Sum / Len ; Avg = 0).

% Recomenda filmes por género preferido
preferred_genre(User, Genre) :-
    findall(G, (
        rating(User, Item, Score, _, _),
        Score >= 4,
        db(Item, genre, G)
    ), Genres),
    most_frequent(Genres, Genre).

% Recomenda filmes por género preferido
recommend_by_genre(User, Genre, N, List) :-
    seen_items(User, Seen),
    findall(Item, (
        db(Item, genre, Genre),
        \+ member(Item, Seen)
    ), Candidates),
    sort(Candidates, Sorted),
    length(List, N),
    append(List, _, Sorted).

% Filme mais popular
most_frequent(List, Most) :-
    setof(Freq-Item, (
        member(Item, List),
        aggregate_all(count, member(Item, List), Freq)
    ), Freqs),
    sort(0, @>=, Freqs, [MostFreq-Most|_]).

% Recomenda filmes aleatórios
random_good_movies(User, N, List) :-
    findall(Item, (
        rating(_, Item, Score, _, _),
        Score >= 4
    ), AllGood),
    seen_items(User, Seen),
    exclude({Seen}/[I]>>member(I, Seen), AllGood, Unseen),
    sort(Unseen, Unique),
    random_permutation(Unique, Shuffled),
    length(List, N),
    append(List, _, Shuffled).

% Sistema híbrido de recomendações
smart_recommend(User, K, N, Recs, Method) :-  % Adicionamos o parâmetro Method
    (   recommend_items(User, K, N, R), R \= [] -> Recs = R, Method = 'KNN (similaridade de cosseno)'
    ;   preferred_genre(User, G) -> recommend_by_genre(User, G, N, Recs), Method = 'Recomendacao por genero preferido'
    ;   random_good_movies(User, N, Recs), Method = 'Recomendacao aleatoria de filmes bons'
    ;   popular_items(N, Recs), Method = 'Recomendacao de filmes populares'
    ).

% Exibe as recomendações, agora com o método utilizado
print_recommendations([], Method) :-
    format('Metodo: ~w~nSem recomendações disponíveis.~n', [Method]).
print_recommendations([Elem | T], Method) :-
    format('Metodo: ~w~n', [Method]),
    print_rec([Elem | T]).

print_rec([]).
print_rec([Score-ID | T]) :-
    number(Score), !,
    ( db(ID, name, Title) -> true ; Title = 'Titulo desconhecido' ),
    format('Recomendacao: ~w (~2f)~n', [Title, Score]),
    print_rec(T).
print_rec([ID | T]) :-
    ( db(ID, name, Title) -> true ; Title = 'Titulo desconhecido' ),
    format('Recomendacao: ~w~n', [Title]),
    print_rec(T).
