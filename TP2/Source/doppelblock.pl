% :- include('utils.pl').         %Utility predicates
% :- include('board.pl').       %Board display predicates
% :- include('solver.pl').            %CPU movement predicates
% :- include('display.pl').   %Boards used for testing

:- use_module(library(clpfd)).

doppelBlock([CSum, RSum, Rows]) :-
        length(Rows, 6), maplist(same_length(Rows), Rows),
        Rows = [A,B,C,D,E,F],

        defineDomain(A,B,C,D,E,F), %usar maplist

        %first restrition
        defineCardinality(A,B,C,D,E,F), %usar maplist

        %second restrition
        eachRowBlacken(A,B,C,D,E,F),
        eachColumnBlacken(A,B,C,D,E,F),

        %third restrition
        eachRowSum(RSum,A,B,C,D,E,F),
        eachColSum(CSum,A,B,C,D,E,F),

        labeling([],Rows), write(Rows).

defineDomain(A,B,C,D,E,F) :-
        domain(A,0,5), domain(B,0,5),
        domain(C,0,5), domain(D,0,5),
        domain(E,0,5), domain(F,0,5).

defineCardinality(A,B,C,D,E,F) :-
        global_cardinality(A,[0-2,1-1,2-1,3-1,4-1,5-1]),
        global_cardinality(B,[0-2,1-1,2-1,3-1,4-1,5-1]),
        global_cardinality(C,[0-2,1-1,2-1,3-1,4-1,5-1]),
        global_cardinality(D,[0-2,1-1,2-1,3-1,4-1,5-1]),
        global_cardinality(E,[0-2,1-1,2-1,3-1,4-1,5-1]),
        global_cardinality(F,[0-2,1-1,2-1,3-1,4-1,5-1]).


eachRowBlacken(A,B,C,D,E,F):-
  exactly(0,A,2),  exactly(0,B,2),
  exactly(0,C,2), exactly(0,D,2),
  exactly(0,E,2),   exactly(0,F,2).

eachColumnBlacken([],[],[],[],[],[]).
eachColumnBlacken([A|T1],[B|T2],[C|T3],[D|T4],[E|T5],[F|T6]) :-
        exactly(0,[A,B,C,D,E,F],2),
        eachColumnBlacken(T1, T2,T3,T4,T5,T6).

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

eachRowSum([S1,S2,S3,S4,S5,S6],A,B,C,D,E,F):-
  calcSum(S1,A),   calcSum(S2,B),
  calcSum(S3,C),   calcSum(S4,D),
  calcSum(S5,E),   calcSum(S6,F).

eachColSum([],[],[],[],[],[],[]).
eachColSum([H|T], [A|T1],[B|T2],[C|T3],[D|T4],[E|T5],[F|T6]) :-
        calcSum(H,[A,B,C,D,E,F]),
        eachColumnBlacken(T,T1,T2,T3,T4,T5,T6).

test_board([
  [4, 8, 4, 5, 6, 5], % Column Sum
  [9, 7, 2, 10, 3, 1], % Row Sum
  _ % Map
  ]).
