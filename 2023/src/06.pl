:- module('06', [solution1/1, solution2/1]).

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

solution1(X) :-
    phrase_from_file(input(Times, Distances), 'inputs/06.txt'),
    solve(Times, Distances, X).
solution2(X) :-
    phrase_from_file(input2(Times, Distances), 'inputs/06.txt'),
    solve(Times, Distances, X).

solve(Times, Distances, X) :-
    findall(N, (
        nth1(I, Times, Duration),
        nth1(I, Distances, RecordDistance),
        ButtonTime * (Duration - ButtonTime) #= Distance,
        ButtonTime in 0..Duration,
        Distance #> RecordDistance,
        fd_size(ButtonTime, N)
    ), Ns),
    product_list(Ns, X).


product_list(L, N) :- product_list(L, 1, N).
product_list([], N, N).
product_list([X | Xs], N0, N) :- N1 is N0 * X, product_list(Xs, N1, N).

whites1 --> white, whites.

integers([X | Xs]) --> integer(X), whites1, integers(Xs).
integers([X]) --> integer(X).

spaced_digits([X | Xs]) --> digit(X), whites, spaced_digits(Xs).
spaced_digits([X]) --> digit(X).

input(Times, Distances) -->
    "Time:", whites, integers(Times), eol,
    "Distance:", whites, integers(Distances), eol.

input2([Time], [Distance]) -->
    "Time:", whites1, spaced_digits(TimeCodes), eol,
    "Distance:", whites1, spaced_digits(DistanceCodes), eol,
    { phrase(integer(Time), TimeCodes),
      phrase(integer(Distance), DistanceCodes) }.
