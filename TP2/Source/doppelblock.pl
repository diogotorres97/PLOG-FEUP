% :- include('utils.pl').         %Utility predicates
% :- include('board.pl').       %Board display predicates
% :- include('solver.pl').            %CPU movement predicates
% :- include('display.pl').   %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).

doppelBlock([CSum, RSum, Rows]) :-
        length(Rows, 6), maplist(same_length(Rows), Rows), % Create a bidimensional matrix 6x6
        Rows = [A,B,C,D,E,F], %Each row of Matrix

        defineDomain(Rows),

        %first restrition
        defineCardinality(Rows),

        %second restrition
        eachRowBlacken(Rows),
        eachColumnBlacken(A,B,C,D,E,F),

        %third restrition
        eachRowSum2(RSum,A,B,C,D,E,F),
        %eachRowSum(Rsum,Rows),
      %  eachColSum(CSum,A,B,C,D,E,F),

       maplist(labeling([]), Rows),
       write(A),nl, write(B), nl, write(C),nl, write(D),nl, write(E),nl,write(F).
       %maplist(write, Rows).

defineDomain(Rows):-
        maplist(define_domain, Rows).

define_domain(X):- domain(X, 0, 4).

defineCardinality(Rows) :-
        maplist(define_Cardinality, Rows),
        transpose(Rows, Columns),
        maplist(define_Cardinality, Columns).

define_Cardinality(X) :-
  global_cardinality(X,[0-2,1-1,2-1,3-1,4-1]).

eachRowBlacken(Rows):-
  maplist(countEachRowBlacken, Rows).

countEachRowBlacken(X):-
  count(0,X,#=,2).

eachColumnBlacken([],[],[],[],[],[]).
eachColumnBlacken([A|T1],[B|T2],[C|T3],[D|T4],[E|T5],[F|T6]) :-
        eachColumnBlacken(T1, T2,T3,T4,T5,T6),
        count(0,[A,B,C,D,E,F],#=,2).

giveBlackenIndexes(L1, P1, P2):-
  element(P1, L1, 0),
  element(P2, L1, 0),
  P1 #\= P2, P1#<P2.

sumBetweenIndexes(_, P2, P2,Result, Result).
sumBetweenIndexes(L1, P1, P2,Temp, Result):-
  P1 #< P2,
  element(P1, L1, Value),
  Temp2 #= Temp + Value,
  Idx #= P1 + 1,
  sumBetweenIndexes(L1,Idx,P2,Temp2, Result).

calcSum(Value, L1) :-
  giveBlackenIndexes(L1, P1, P2),
  sumBetweenIndexes(L1, P1, P2,0 , Result),
  Value #= Result.

eachRowSum2([S1,S2,S3,S4,S5,S6],A,B,C,D,E,F):-
  calcSum(S1,A),  calcSum(S2,B),
  calcSum(S3,C),   calcSum(S4,D).
  %calcSum(S5,E),   calcSum(S6,F).

eachRowSum([],[]).
eachRowSum([S1|ST],[R1|SR]):-
  eachRowSum(ST,SR),
  calcSum(S1,R1).

eachColSum([],[],[],[],[],[],[]).
eachColSum([H|T], [A|T1],[B|T2],[C|T3],[D|T4],[E|T5],[F|T6]) :-
        calcSum(H,[A,B,C,D,E,F]),
        eachColSum(T,T1,T2,T3,T4,T5,T6).

test_board([
  [4, 8, 4, 5, 6, 5], % Column Sum
  [9, 7, 2, 10, 3, 1], % Row Sum
  _ % Map
  ]).
