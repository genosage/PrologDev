%% :- ensure_loaded(borders).
%% :- ensure_loaded(cities).
%% :- ensure_loaded(countries).
%% :- ensure_loaded(rivers).

%% findBorders(X):-
%%     borders(australia, X).

%% findBorders1(X):-
%%     borders(france, X),
%%     borders(spain, X).

%% findCountries(X):-
%%     country(X,_,_,_,_,_,_,_),
%%     borders(france, X),
%%     borders(spain, X).

%% country(X):-
%%     country(X,_,_,_,_,_,_,_).

%% findCountries1(X):-
%%     country(X),
%%     borders(france, X),
%%     borders(spain, X).

%% fuckYou.

%% list_of(_, []).
%% list_of(Elt, [Elt|List]):-
%%     list_of(Elt, List).

%% all_same([]).
%% all_same([Elt|List]):-
%%     list_of(Elt, List).

%% adjacent(E1, E2, List):-
%%     append(_, [E1, E2|_], List).

%% adjacent1(E1, E2, [E1, E2|_]).
%% adjacent1(E1, E2, [_|List]):-
%%     adjacent1(E1, E2, List).

%% before(E1, E2, List):-
%%     append(_, [E1|_], L),
%%     append(L, [E2|_], List).

%% intset_member(N, tree(_, N, _)).
%% intset_member(N, tree(L, N1, _)):-
%%     N < N1,
%%     intset_member(N, L).
%% intset_member(N, tree(_, N1, R)):-
%%     N > N1,
%%     intset_member(N, R).

%% intset_insert(N, Set0, Set):-
%%     intset_member(N, Set0),
%%     Set0 = Set.
%% intset_insert(N, empty, tree(empty, N, empty)).
%% intset_insert(N, tree(empty, N1, R), tree(L, N1, R)):-
%%     N < N1,
%%     L = tree(empty, N, empty).
%% intset_insert(N, tree(L, N1, empty), tree(L, N1, R)):-
%%     N > N1,
%%     R = tree(empty, N, empty).
%% intset_insert(N, tree(L, N1, R), tree(L1, N1, R1)):-
%%     intset_insert(N, L, L1),
%%     R = R1;
%%     intset_insert(N, R, R1),
%%     L = L1.


sumlist(List, Sum):-
    sumlist(List, 0, Sum).

sumlist([], Sum, Sum).
sumlist([N|Ns], Sum, Result):-
    Sum1 = Sum + N,
    sumlist(Ns, Sum1, Result).


tree(empty).
tree(node(Left,_,Right)):-
    tree(Left),
    tree(Right).

tree_list(empty, []).
tree_list(node(L, N, R), List):-
    tree_list(L, List1),
    tree_list(R, List2),
    append(List1, [N|List2], List).

tree_list1(Tree, List):-
    tree_list1(Tree, List, []).
tree_list1(empty, List, List).
tree_list1(node(L, N, R), List, List0):-
    tree_list1(L, List, List1),
    List1 = [N|List2],
    tree_list1(R, List2, List0).

list_tree([], empty).
list_tree(List, node(L, N, R)):-
    append(List1, [N|List2], List),
    length(List1, Len),
    length(List2, Len),
    list_tree(List1, L),
    list_tree(List2, R).

same_elements(L1, L2):-
    same_helper(L1, L2),
    same_helper(L2, L1).

same_helper([], _).
same_helper([E|L1], L2):-
    member(E, L2),
    same_helper(L1, L2).

delete_elem([E|List], E, List).
delete_elem([E1|List], E, [E1|List1]):-
    delete_elem(List, E, List1).

permutation([], []).
permutation([E|List1], [E|List2]):-
    permutation(List1, List2).
permutation([E1|List1], [E2|List2]):-
    delete_elem(List1, E2, NewList1),
    delete_elem(List2, E1, NewList2),
    permutation(NewList1, NewList2).

newpermutation(L1, L2):-
    msort(L1, Sorted),
    msort(L2, Sorted).






