%% -*- prolog -*-

%% Type checking for simply typed lambda calculus.

%% Syntax of simply typed λ Calc:

%% Type ::= t1 | t2 | ... | Type -> Type
%% Expr ::= Var | Basis | Expr Expr | λ Var : Type . Expr
%% Basis ::= 1 | pi | sin
%% Global type env
%%    1 : Real, pi : Real, sin : Real->Real
%% t1 = Real

%% Type ::= t1 | t2 | ... | Type -> Type
%% in Prolog: t1, ..., ->(Type,Type)
%% Expr ::= Var | Basis | Expr Expr | λ Var : Type . Expr
%% in Prolog: ap(Expr,Expr) | lam(Var,Type,Expr)

%% wellTyped(+).
wellTyped(E) :- isTypeTop(E,_).
%% isTypeTop(+,-).
isTypeTop(E,T) :- topLevelType(Gamma), isType(E,T,Gamma).

%% topLevelType(-)
topLevelType([[pi,real],[sin,->(real,real)]]).

%% isType(+,-)
%%      [C.H. Iso: premise]
isType(V,T,G) :- member([V,T0],G), !, T=T0.
%%      [C.H. Iso: Modus Ponens]
%%               **                 *********                **
isType(ap(E1,E2),T3,G) :- isType(E1,->(T2,T3), G), isType(E2,T2,G). % really needs occurs check
%%      [C.H. Iso: assume T1, can conclude T2, therefore can infer T1->T2.
isType(lam(V,T1,E),->(T1,T2),G) :- isType(E,T2,[[V,T1]|G]).

%% Curry-Howard Isomorphism in Prolog...
entails(G,T) :- isType(_,T,G).
