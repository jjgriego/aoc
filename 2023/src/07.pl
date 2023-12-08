:- module('07', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).

solution1(X) :-
    phrase_from_file(input(no_jokers, Hands), 'inputs/07.txt'),
    summarize(Hands, X).
solution2(X) :-
    phrase_from_file(input(jokers, Hands), 'inputs/07.txt'),
    summarize(Hands, X).

summarize(Hands, X) :-
    maplist(classify_hand, Hands, HandsClassified),
    maplist(demote_jokers, HandsClassified),
    sort(HandsClassified, HandsSorted),
    aggregate_all(sum(Score), (nth1(HandRank, HandsSorted, hand(_, _, Bid)), Score is Bid * HandRank), X).

classify_hand(hand(Ranks, Bid), hand(HandClass, Ranks, Bid)) :- 
    hand_structure(Structure, HandClass), \+ \+ permutation(Structure, Ranks), !.

demote_jokers(hand(_, Ranks, _)) :- maplist(demote_joker, Ranks).
demote_joker(Rank) :- ignore(Rank = 0).

hand_structure([A, A, A, A, A], 900).
hand_structure([A, A, A, A, _], 800).
hand_structure([A, A, A, B, B], 700).
hand_structure([A, A, A, _, _], 600).
hand_structure([A, A, B, B, _], 500).
hand_structure([A, A, _, _, _], 400).
hand_structure([_, _, _, _, _], 100).

input(Jokers, [hand(Ranks, Bid) | More]) --> hand(Jokers, Ranks, Bid), !, eol, input(Jokers, More).
input(_, []) --> eol.

hand(Jokers, Ranks, Bid) -->
    [C1, C2, C3, C4, C5],
    { maplist(card_rank_code(Jokers), Ranks, [C1, C2, C3, C4, C5]) },
    " ",
    integer(Bid).

card_rank_code(jokers, _,    0'J) :- !.
card_rank_code(_,      Rank, Code) :- nth1(Rank, `23456789TJQKA`, Code).
