:- module('03', [solution1/1, solution2/1]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

:- dynamic(symbol/3).
:- dynamic(number/4).

:- initialization(load_schematic).
load_schematic =>
    phrase_from_file(schematic(Loci), 'inputs/03.txt'), !,
    maplist(assertz, Loci).

schematic(Loci) --> `.`, !, schematic(Loci).
schematic(Loci) --> `\n`, !, schematic(Loci).
schematic([symbol(Line, Col, C) | More]) -->
    [ C ], { \+ code_type(C, digit), C \== 0'. }, !,
    lazy_list_location(file(_, Line, Col0, _)), !,
    { Col is Col0 - 1 },
    schematic(More).
schematic([number(Line, Col, Width, N) | More]) --> 
    lazy_list_location(file(_, Line, Col, _)),
    integer(N), !,
    lazy_list_location(file(_, _, EndCol, _)),
    { Width is EndCol - Col },
    schematic(More).
schematic([]) --> [].

solution1(X) :- findall(N, part(N), Parts), sum_list(Parts, X).
part(N) :-
    number(Line, Col, Width, N),
    once((symbol(SLine, SCol, _), adjacent(number(Line, Col, Width), symbol(SLine, SCol)))).

solution2(X) :- findall(R, gear(R), GearRatios), sum_list(GearRatios, X).
gear(R) :-
    symbol(SLine, SCol, 0'*),
    findall(N,
            (number(Line, Col, Width, N), adjacent(number(Line, Col, Width), symbol(SLine, SCol))),
            Numbers),
    Numbers = [N1, N2],
    R is N1 * N2.

adjacent(number(Line, Col, Width), symbol(SLine, SCol)) :-
    Line - 1 #=< SLine,
    SLine #=< Line + 1,
    Col - 1 #=< SCol,
    SCol  #=< Col + Width.
