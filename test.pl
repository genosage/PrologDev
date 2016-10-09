command_loop:- 
repeat,
write('Enter command (end to exit): '),
read(X),
write(X), nl,
X = end.


factorial_1(1,1).
factorial_1(N,F):-
N > 1,
NN is N - 1,
factorial_1(NN,FF),
F is N * FF.


take(N, List, Front):-
length(Front,N),
append(Front,_,List).