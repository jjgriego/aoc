:- module('08', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).

:- dynamic node/3.
:- dynamic instructions/1.

:- initialization(load).
load :-
    phrase_from_file(input(Instructions, Network), 'inputs/08.txt'),
    maplist(assertz, Network),
    assertz(instructions(Instructions)).

solution1(X) :-
    instructions(Instructions),
    append(Instructions, InstructionsLoop, InstructionsLoop),
    run(InstructionsLoop, 'AAA', X).

solution2(X) :-
    instructions(Instructions),
    append(Instructions, InstructionsLoop, InstructionsLoop),
    findall(N, ghost_start(N), Xs),
    maplist(run(InstructionsLoop), Xs, Lens),
    lcm_list(Lens, X).

input(Instructions, Network) --> instructions(Instructions), eol, eol, network(Network).

instructions([left | More]) --> "L", !, instructions(More).
instructions([right | More]) --> "R", !, instructions(More).
instructions([]) --> [].

network([node(Name, Left, Right) | More]) --> node(Name, Left, Right), eol, !, network(More).
network([]) --> [].

node(Name, Left, Right) --> name(Name), " = (", name(Left), ", ", name(Right), ")".
name(Name) --> [C1, C2, C3], {atom_codes(Name, [C1, C2, C3])}.

ghost_start(X) :- node(X, _, _), atom_codes(X, [_, _, 0'A]).
ghost_end(X) :- node(X, _, _), atom_codes(X, [_, _, 0'Z]).

step(left, X0, X) :- node(X0, X, _).
step(right, X0, X) :- node(X0, _, X).

run(Instructions, X, N) :- run(Instructions, X, 0, N).
run(_, X, N, N) :- ghost_end(X), !.
run([Dir | More], X0, N0, N) :-
    N1 is N0 + 1,
    step(Dir, X0, X), !,
    run(More, X, N1, N).

lcm_list([X], X) :- !.
lcm_list([X | More], Lcm) :-
    lcm_list(More, Lcm0),
    Lcm is lcm(X, Lcm0).

prod_list([], 1).
prod_list([X | More], N) :-
    prod_list(More, N0),
    N is X * N0.
