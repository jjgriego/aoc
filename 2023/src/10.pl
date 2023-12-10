:- module('10', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

:- dynamic char/3.
:- initialization(init).

init :-
    phrase_from_file(chars(Cs, 1, 1), 'inputs/10.txt'),
    maplist(assertz, Cs).




chars(More, Line, _) -->
    { Line1 is Line + 1 },
    "\n", !, chars(More, Line1, 1).
chars([char(C, Line, Col) | More], Line, Col) -->
    [ C ], !, { Col1 is Col + 1 }, chars(More, Line, Col1).
chars([], _, _) --> [].
