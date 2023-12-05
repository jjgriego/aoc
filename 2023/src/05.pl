:- module('05', [solution1/1, solution2/1]).

:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- dynamic seed/1.
:- dynamic seed_range/2.
:- dynamic map_range/5.
:- initialization(load).
load :-
    phrase_from_file(input(Seeds, Maps), 'inputs/05.txt'),
    maplist(load_seed, Seeds),
    load_seed_ranges(Seeds),
    maplist(load_map, Maps).
load_seed(N) :- assertz(seed(N)).
load_seed_ranges([S0, Len | More]) :- assertz(seed_range(S0, Len)), !, load_seed_ranges(More).
load_seed_ranges([]).
load_map(map(Src, Dst, Ranges)) :- maplist(load_range(Src, Dst), Ranges).
load_range(Src, Dst, range(X0, Y0, Len)) :- assertz(map_range(Src, Dst, X0, Y0, Len)).

solution1(X) :- aggregate_all(min(L), (seed(S), seed_location(S, L)), X).
solution2(X) :- aggregate_all(min(L), (seed2(S), seed_location(S, L)), X).

seed_location(S, L) :-
    locationof(seed, S, L).

seed2(S) :- seed_range(S0, Len), S1 is S0 + Len - 1, between(S0, S1, S).

locationof(Src, X, L) :-
    correspond(Src, location, X, L), !.
locationof(Src, X, L) :-
    correspond(Src, Cat, X, Y), !,
    locationof(Cat, Y, L).

correspondence(seed, soil).
correspondence(soil, fertilizer).
correspondence(fertilizer, water).
correspondence(water, light).
correspondence(light, temperature).
correspondence(temperature, humidity).
correspondence(humidity, location).

correspond(Src, Dst, X, Y) :-
    map_range(Src, Dst, X0, Y0, Len),
    X #>= X0,
    Y #>= Y0,
    X #< X0 + Len,
    Y #< Y0 + Len,
    X #= X0 + (Y - Y0),
    Y #= Y0 + (X - X0),
    !.
correspond(Src, Dst, X, X) :- correspondence(Src, Dst).

spaces --> " ", !, spaces.
spaces --> [].

spaces1 --> " ", spaces.

input(Seeds, Maps) -->
    "seeds:", spaces1, numbers(Seeds), "\n\n",
    maps(Maps).

numbers([N | Ns]) --> integer(N), !, spaces, numbers(Ns).
numbers([]) --> [].

maps(Maps) --> map(Map), ("\n", !, {Maps = [Map | More]}, maps(More) ; {Maps = [Map]}, []).
maps([]) --> [].

map(map(Src, Dst, Ranges)) -->
    string(SrcCodes), "-to-", !, string(DstCodes), " map:\n", !,
    { atom_codes(Src, SrcCodes), atom_codes(Dst, DstCodes) },
    ranges(Ranges).

ranges([range(X0, Y0, Len) | More]) -->
    integer(Y0), spaces1, integer(X0), spaces1, integer(Len), "\n", !,
    ranges(More).
ranges([]) --> [].
