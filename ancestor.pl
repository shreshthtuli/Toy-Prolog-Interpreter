ancestor(bob, susan).
ancestor(A, X) :- parent(A, X).
ancestor(A, X) :- parent(A, C), ancestor(C, X).
parent(fred, sally).
parent(tina, sally).
parent(sally, john).
parent(sally, diane).
parent(sam, bill).

