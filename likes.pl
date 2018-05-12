likes(joe,books).
likes(joe,mary).
likes(mary,books).
likes(john,books).
likes(sue,joe).
likes(john,mary).
likes(mary,joe).
likes(mary,movies).

likes(bill,X) :- likes(X,books), likes(X,movies).
likes(alice,X) :- likes(X,mary).
