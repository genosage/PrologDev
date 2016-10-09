% Question 1.

replace(E1, [E1|T], E2, [E2|T]).
replace(E1, [H|T1], E2, [H|T2]):-
replace(E1, T1, E2, T2).

% Question 2.

zip([],[],[]).
zip([H1|T1], [H2|T2], [H1-H2|T]):-
zip(T1, T2, T).

% Question 3.

sublist([], _).
sublist([H|T1], [H|T2]):-
sublist(T1, T2).
sublist([H1|T1], [_|T2]):-
sublist([H1|T1], T2).