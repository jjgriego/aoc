:- module('02', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

solution1(X) :- phrase_from_file(solution1(X), 'inputs/02.txt').
solution1(X) --> solution1(X, 0).
solution1(X, Sum0) --> 
    { Bag = bag(12, 13, 14) },
    game(Id, Draws), `\n`, !, 
    {
        check_draws(Bag, Draws), !,
        Sum1 is Sum0 + Id
    ;
        Sum1 is Sum0
    },
    solution1(X, Sum1).
solution1(Sum, Sum) --> [].



solution2(X) :- phrase_from_file(solution2(X), 'inputs/02.txt').

solution2(X) --> solution2(X, 0).
solution2(X, Sum0) --> 
    game(_Id, Draws), `\n`, !, 
    {
        check_draws(bag(R, G, B), Draws),
        [R, G, B] ins 0..100000000,
        labeling([min], [R, G, B]), !,
        Sum1 is Sum0 + R * G * B
    },
    solution2(X, Sum1).
solution2(Sum, Sum) --> [].



check_draws(Bag, Draws) :- maplist(check_draw(Bag), Draws).
check_draw(Bag, Parts) :- maplist(check_part(Bag), Parts).
check_part(bag(Red, _, _), red - Count) :-
    Count #=< Red.
check_part(bag(_, Green, _), green - Count) :-
    Count #=< Green.
check_part(bag(_, _, Blue), blue - Count) :-
    Count #=< Blue.


game(Id, Draws) -->
  `Game `, number(Id), `: `, !,
  draws(Draws).

draws(Draws) --> 
    draw_parts(Draw), 
    ( 
        `; `, !, 
        { Draws = [Draw | More] },
        draws(More) 
    ; 
        { Draws = [Draw] },
        [] 
    ).
draw_parts(Parts) -->
    number(Count), ` `, color(Color),
    ( 
        `, `, !, { Parts = [Color - Count | More] }, draw_parts(More)
    ;
        { Parts = [Color - Count] }
    ).

color(red) --> `red`.
color(blue) --> `blue`.
color(green) --> `green`.
