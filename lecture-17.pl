%% -*- prolog -*-

%% app(Xs,Ys,Zs).			% true when Xs appended to Ys is Zs
app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

mem(X,Ys) :- app(_,[X|_],Ys).

not(X) :- X, !, fail.
not(_).

%% possible definition: X=X.
%% possible definition: =(X,X).

%% is(-,+).        %% for arithmetic

%% Prolog does *not* have an occurs check during unification, therefore allows "infinite" solutions.

%% DIFFERENCE LISTS

%%    [a,b,c,d,e,f] - [d,e,f]  represents [a,b,c]
%%   Xs - Ys represents first k elements of Xs, followed by tail of Ys.
%%   Invalid if Xs does not have tail equal to Ys.

diffListToList(Xs-Ys,Zs) :- append(Zs,Ys,Xs).

%% Natural language processing (toy).

%% 
%% ** la-la land in generative mode:
%% sentence(S) :- append(NP,VP,S), nounPhrase(NP), verbPhrase(VP).
%% ** la-la land in parsing mode:
%% sentence(S) :- nounPhrase(NP), verbPhrase(VP), append(NP,VP,S).

%% nounPhrase([N]) :- noun(N).
%% noun(sally).
%% noun(fido).
%% noun(tree).
%% verbPhrase([V]) :- verb(V).
%% verb(digs).
%% verb(walks).
%% verb(laughs).

dlAppend(-(X,Xt), -(Xt,Yt), -(X,Yt)).
%% sentence(S-St) :- nounPhrase(NP-NPt), verbPhrase(VP-VPt)
%%% CONTINUED
