:- module('10', [ solution1/1, solution2/1, dump/0 ]).

:- use_module(library(dcg/basics)).

solution1(X) :-
    aggregate_all(max(D),
        aggregate(min(N), path(N, _X, _Y), D),
        X).

solution2(X) :-
    aggregate_all(count, inside(_, _), X).
            
            

:- dynamic char/3.
:- dynamic extent/2.
:- initialization(init).

init :-
    phrase_from_file(chars(Cs, 1, 1), 'inputs/10.txt'),
    maplist(assertz, Cs),
    aggregate_all(max(X), char(_, X, _), Ex),
    aggregate_all(max(Y), char(_, Y, _), Ey),
    assertz(extent(Ex, Ey)),
    compile_predicates(
      [char/3, extent/2]
    ).

dir(north).
dir(south).
dir(east).
dir(west).

step(0'S, _D, stop).
step(C, In, Out) :- step_(C, Out, In).
step(C, Out, In) :- step_(C, Out, In).

step_(0'|, north, south).
step_(0'-, east, west).
step_(0'L, north, east).
step_(0'J, north, west).
step_(0'7, south, west).
step_(0'F, south, east).

flip(D0, D) :- step(0'-, D0, D).
flip(D0, D) :- step(0'|, D0, D).

grid(east,  X0, Y0, X, Y0) :- X is X0 + 1.
grid(west,  X0, Y0, X, Y0) :- X is X0 - 1.
grid(north, X0, Y0, X0, Y) :- Y is Y0 - 1.
grid(south, X0, Y0, X0, Y) :- Y is Y0 + 1.



:- table path_/4 as subsumptive.
path_(0, X0, Y0, D) :- char(0'S, X0, Y0), dir(D).
path_(N, X, Y, D) :- 
    path_(N0, X0, Y0, D0),
    N is N0 + 1,
    grid(D0, X0, Y0, X, Y),
    once(flip(D0, D1)),
    char(C, X, Y),
    step(C, D1, D).

path(N, X, Y) :- path_(N, X, Y, _).

:- table inside/2 as subsumptive.
:- table outside/3.

in_range(X, Y) :- extent(Ex, Ey), between(1, Ex, X), between(1, Ey, Y).

outside(X, Y) :- outside(_, X, Y).
outside(D, X, Y) :- var(D), !, between(1, inf, D), ( \+ outside(D, _, _), !, fail ; outside(D, X, Y), !).
outside(0, 0, Y)   :- extent(_, Ey), between(1, Ey, Y).
outside(0, Ex1, Y) :- extent(Ex, Ey), between(1, Ey, Y), Ex1 is Ex + 1.
outside(0, X, 0)   :- extent(Ex, _), between(1, Ex, X).
outside(0, X, Ey1) :- extent(Ex, Ey), between(1, Ex, X), Ey1 is Ey + 1.
outside(D, X, Y)   :- 
    D > 0,
    D0 is D - 1,
    outside(D0, X0, Y0),
    once((
      grid(_, X0, Y0, X, Y),
      in_range(X, Y),
      \+ (between(0, D0, D1), outside(D1, X, Y)),
      \+ path_(_, X, Y, _)
    )).



char_color(C, X, Y) :- outside(D, X, Y), !, once((0 is D mod 2, C = blue; C = cyan)).
char_color(red, X, Y) :- path(_, X, Y), !.
char_color(white, X, Y) :- !.

dump :-
  extent(Ex, Ey),
  between(1, Ey, Y),
  (
    between(1, Ex, X),
    char(C, X, Y),
    once(char_color(Color, X, Y)),
    ansi_format([fg(Color)], "~c", [C]),
    fail
  ;
    writeln(""),
    fail
  ).
dump :- true.


  

% inside(X, Y) :- path_(_, X, Y, _).
% inside(X, Y) :- inside(X0, Y0), grid(_, X, Y), tnot(path_(_, X, Y, _)), tnot(outside(X, Y)), 


chars(More, Line, _) -->
    { Line1 is Line + 1 },
    "\n", !, chars(More, Line1, 1).
chars([char(C, Col, Line) | More], Line, Col) -->
    [ C ], !, { Col1 is Col + 1 }, chars(More, Line, Col1).
chars([], _, _) --> [].
