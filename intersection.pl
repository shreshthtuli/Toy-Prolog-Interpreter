intersection([],X,[]).
intersection([X|R],Y,[X|Z]) :- member(X,Y), !, intersection(R,Y,Z).
intersection([X|R],Y,Z) :- intersection(R,Y,Z).
