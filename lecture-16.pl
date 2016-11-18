%% -*- prolog -*-

%% FACTS:
parent(jacob,fishel).
parent(fishel,barak).
parent(anne,barak).
parent(albert,anne).
parent(esther,anne).
male(jacob).
male(fishel).
male(barak).
male(albert).
female(anne).
female(esther).

%% abstract definitions
father(X,Y) :- male(X), parent(X,Y).
father(X) :- father(X,_).	% warning with Y instead of _

grandparent(X,Z) :- parent(X,Y), parent(Y, Z).

%% Peano Arithmetic

%% terms representing Nat can be: z, s(T) where T is Nat.
%% z for zero, s for successor

paEqual(z,z).
paEqual(s(X),s(Y)) :- paEqual(X,Y).

%% plus(+,+,-)
%% plus(-,-,+)
%% plus(-,+,-)
%% plus(+,-,-)  (only if you're okay with explicit ...)
plus(X,z,X).
plus(X,s(Y),s(Z)) :- plus(X,Y,Z).

mul(z,_,z).
mul(s(X),Y,Z) :- mul(X,Y,W), plus(Y,W,Z).

%% | ?- mul(X,Y,s(s(s(s(s(s(z))))))).

%% X = s(z)
%% Y = s(s(s(s(s(s(z)))))) ? a

%% X = s(s(z))
%% Y = s(s(s(z)))

%% X = s(s(s(z)))
%% Y = s(s(z))

%% Fatal Error: global stack overflow (size: 32768 Kb, environment variable used: GLOBALSZ)
%% Process prolog exited abnormally with code 1

%% Lists:
%% [1,2,3]
%%   sugar for
%% dot(1,dot(2,dot(3,[])))
%%   exept . instead of dot, so
%% .(1,.(2,.(3,[])))

%% mem(X,.(X,_)).
%% mem(X,.(_,Ys)) :- mem(X,Ys).

mem(X,[X|_]).
mem(X,[_|Ys]) :- mem(X,Ys).

memboth(X,Ys,Zs) :- mem(X,Ys), mem(X,Zs).
