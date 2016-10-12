multiply(X, Y, XY) :-
( X = 0 ->
    XY = 0
    ; X1 is X - 1,
      multiply(X1, Y, X1Y),
      XY is X1Y + Y
).