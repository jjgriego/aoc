:- module('04', [ solution1/1, solution2/1 ]).

:- use_module(library(dcg/basics)).

solution1(X) :-
  phrase_from_file(cards(Cards), 'inputs/04.txt'),
  aggregate_all(sum(Score), (member(card(N, Winning, Have), Cards), card_score(N, Winning, Have, Score)), X).

card_score(_N, Winning, Have, Score) :-
  card_winning_count(Winning, Have, WinningCount),
  (WinningCount = 0 -> Score = 0 ; Score is 2 ^ (WinningCount - 1)).

solution2(X) :- 
  phrase_from_file(cards(Cards), 'inputs/04.txt'),
  Multiples0 = [ 1 | Multiples0 ],
  count_cards(Cards, Multiples0, _, 0, X).

count_cards([], _, _, Count, Count).
count_cards([card(_, Winning, Have) | More], [Multiple | Multiples0], Multiples, Count0, Count) :-
  card_winning_count(Winning, Have, WinCount),
  add_bonuses(Multiple, WinCount, Multiples0, Multiples1),
  Count1 is Count0 + Multiple,
  count_cards(More, Multiples1, Multiples, Count1, Count).

add_bonuses(_Multiple, 0, Multiples, Multiples) :- !.
add_bonuses(Multiple, N0, [M0 | More0], [M | More]) :-
    M is Multiple + M0,
    N1 is N0 - 1,
    add_bonuses(Multiple, N1, More0, More).

card_winning_count(Winning, Have, Count) :-
  aggregate_all(count, (member(N, Winning), member(N, Have)), Count).


cards([card(N, Winning, Have) | Cs]) --> card(N, Winning, Have), !, `\n`, cards(Cs).
cards([]) --> [].

card(N, Winning, Have) --> `Card`, whites, integer(N), `:`, whites, numbers(Winning), whites, `|`, whites, numbers(Have).

numbers([N | Ns]) --> integer(N), !, whites, numbers(Ns).
numbers([]) --> [].
