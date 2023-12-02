:- module('01', [ solution/1 ]).

solution(X) :- phrase_from_file(solution(X), 'inputs/01.txt').

solution(X) --> solution(X, 0).
solution(X, Sum0) --> calibration(N), `\n`, { Sum1 is Sum0 + N }, !, solution(X, Sum1).
solution(Sum, Sum) --> [].

calibration(N) -->
  calibration_digits(Ds),
  {
    [ First | _ ] = Ds,
    last(Ds, Last),
    N is First * 10 + Last
  }.

calibration_digits([D | More]) --> peek(digit(D)), !, [_], calibration_digits(More).
calibration_digits(Ds) -->
  [ C ], { C \== 0'\n }, !, calibration_digits(Ds).
calibration_digits([]) --> [].

peek(G, C0, C0) :- call_dcg(G, C0, _C).

digit(D) --> [ Code ], { code_type(Code, digit(D)) }.
digit(1) --> `one`.
digit(2) --> `two`.
digit(3) --> `three`.
digit(4) --> `four`.
digit(5) --> `five`.
digit(6) --> `six`.
digit(7) --> `seven`.
digit(8) --> `eight`.
digit(9) --> `nine`.
