:- module('02', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).

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

check_draws(Bag, Draws) :- forall(member(Draw, Draws), check_draw(Bag, Draw)).
check_draw(Bag, Parts) :- forall(member(Part, Parts), check_part(Bag, Part)).
check_part(bag(Red, _, _), red - Count) :-
    Count =< Red.
check_part(bag(_, Green, _), green - Count) :-
    Count =< Green.
check_part(bag(_, _, Blue), blue - Count) :-
    Count =< Blue.



solution2(X) :- phrase_from_file(solution2(X), 'inputs/02.txt').

solution2(X) --> solution2(X, 0).
solution2(X, Sum0) --> 
    game(_Id, Draws), `\n`, !, 
    {
        minimal_bag(Draws, Bag),
        power(Bag, P),
        Sum1 is Sum0 + P
    },
    solution2(X, Sum1).
solution2(Sum, Sum) --> [].

minimal_bag(Draws, Bag) :-
    minimal_bag(Draws, bag(0, 0, 0), Bag).
minimal_bag(Draws, Bag0, Bag) :-
    foldl(minimal_draw_bag, Draws, Bag0, Bag).
minimal_draw_bag(Parts, Bag0, Bag) :-
    foldl(minimal_part_bag, Parts, Bag0, Bag).
minimal_part_bag(red - Count, bag(R0, G, B), bag(R, G, B)) :-
    R is max(Count, R0).
minimal_part_bag(green - Count, bag(R, G0, B), bag(R, G, B)) :-
    G is max(Count, G0).
minimal_part_bag(blue - Count, bag(R, G, B0), bag(R, G, B)) :-
    B is max(Count, B0).

power(bag(R, G, B), P) :-
    P is R * G * B.



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
