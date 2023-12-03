:- module('03', [solution1/1]).

:- use_module(library(dcg/basics)).

:- dynamic(symbol/2).
:- dynamic(number/4).

solution1(X) :- findall(N, part(N), Parts), sum_list(Parts, X).

:- initialization(load_schematic).
load_schematic =>
  phrase_from_file(schematic(Loci), 'inputs/03.txt'), !,
  maplist(assertz, Loci).

schematic(Loci) --> `.`, !, schematic(Loci).
schematic(Loci) --> `\n`, !, schematic(Loci).
schematic([symbol(Line, Col) | More]) -->
  lazy_list_location(file(_, Line, Col, _)),
  [ C ], { \+ code_type(C, digit), C \== 0'. }, !,
  schematic(More).
schematic([number(Line, Col, Width, N) | More]) --> 
  lazy_list_location(file(_, Line, Col, _)),
  integer(N), !,
  lazy_list_location(file(_, _, EndCol, _)),
  { Width is EndCol - Col },
  schematic(More).
schematic([]) --> [].
  
part(N) :-
  number(Line, Col, Width, N),
  SLeft is Col - 1,
  SRight is Col + Width,
  once((
    symbol(Line, SLeft)
  ;
    symbol(Line, SRight)
  ;
    SLine is Line - 1,
    symbol(SLine, SCol),
    between(SLeft, SRight, SCol)
  ;
    SLine is Line + 1,
    symbol(SLine, SCol),
    between(SLeft, SRight, SCol)
  )).
  
