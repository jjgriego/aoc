:- module('09', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

solution1(X) :-
    phrase_from_file(input(Values), 'inputs/09.txt'),
    aggregate_all(sum(N), (
        member(Value, Values),
        describe(Value, Cs),
        length(Value, L0),
        L is L0 + 1,
        length(ValueExtrapolated, L),
        describe(ValueExtrapolated, Cs),
        last(ValueExtrapolated, N)
    ), X).

solution2(X) :-
    phrase_from_file(input(Values), 'inputs/09.txt'),
    aggregate_all(sum(N), (
        member(Value, Values),
        describe(Value, Cs),
        step_coefs([N | _], Cs)
    ), X).

input([Value | More]) -->
    numbers(Value), eol,
    input(More).
input([]) --> [].

numbers([N | More]) --> integer(N), " ", whites, numbers(More).
numbers([N]) --> integer(N).

adj([X1, X2 | MoreX], [Y1 | MoreY]) :-
    Y1 #= X2 - X1,
    adj([X2 | MoreX], MoreY).
adj([_], []).

describe(Xs, []) :- zeroes(Xs), !.
describe(Xs, [C | Coeffs]) :-
    Xs = [C | _],
    adj(Xs, Ys),
    describe(Ys, Coeffs).

zeroes([X | More]) :- X #= 0, zeroes(More).
zeroes([]).

step_coefs([C1, C2 | MoreC], [D1 | MoreD]) :-
    C2 #= D1 - C1,
    step_coefs([C2 | MoreC], MoreD).
step_coefs([C], [C]).
