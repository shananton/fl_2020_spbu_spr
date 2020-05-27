eq(nil, nil).
eq(cons(X, Xs), cons(Y, Ys)) :- eq(X, Y), eq(Xs, Ys).

concat(nil, Ys, Zs) :- eq(Ys, Zs).
concat(cons(X, Xs), Ys, cons(Z, Zs)) :- eq(X, Z), concat(Xs, Ys, Zs).

length(nil, zero).
length(cons(X, Xs), succ(N)) :- length(Xs, N).

?- concat(cons(one, cons(two, nil)), const(three, cons(four, nil)), R).
