% :- include('utils.pl').         %Utility predicates
% :- include('board.pl').       %Board display predicates
% :- include('solver.pl').            %CPU movement predicates
% :- include('display.pl').   %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).

doppelBlock([CSum, RSum, Rows]) :-
        reset_timer,
        length(CSum, N),
        getInitialBoard(N, Rows),

        defineDomain(N, Rows),

        %first restrition
        defineCardinality(N, Rows),

        %second restrition
        eachRowBlacken(Rows),
        transpose(Rows,Columns),
        eachRowBlacken(Columns),

        %third restrition
      Rows = [A,B,C,D,E,F], %Each row of Matrix
      eachRowSum(RSum,[A,B,C,D,E,F]),
      Columns = [A1,A2,A3,A4,A5,A6],
      eachRowSum(CSum,[A1,A2,A3,A4]),


      maplist(labeling([]), Columns), print_time,
       write(A),nl, write(B), nl, write(C),nl, write(D),nl, write(E),nl,write(F).

% Create a bidimensional matrix NxN
getInitialBoard(N, Board):-
   length(Board, N),
   maplist(same_length(Board), Board).

defineDomain(N, Rows):-
        MaxN #= N-2,
        maplist(define_domain(MaxN), Rows).

define_domain(MaxN,X):- domain(X, 0, MaxN).

defineCardinality(N, Rows) :-
        createCardinality(N,1,[0-2], Lista),
        assert(cardinalityList(Lista)),
        maplist(define_Cardinality(N), Rows),
        transpose(Rows, Columns),
        maplist(define_Cardinality(N), Columns).

define_Cardinality(N, X) :-
  cardinalityList(Lista),
  global_cardinality(X,Lista).

%[0-2,1-1,2-1,3-1,4-1]
 createCardinality(Length,Counter, Lista,Lista):- Counter is Length - 1.
 createCardinality(Length,Counter,Temp, Lista) :-
   append([Counter-1], Temp, Temp2),
   Counter =< Length - 2,
   Tmp is Counter+1,
   createCardinality(Length,Tmp, Temp2,Lista).

eachRowBlacken(Rows):-
  maplist(countEachRowBlacken, Rows).

countEachRowBlacken(X):-
  count(0,X,#=,2).

giveBlackenIndexes(L1, P1, P2):-
  Black #= 0, P1#<P2,
  element(P1, L1, Black),
  element(P2, L1, Black).

sumBetweenIndexes(_, P2, P2,Result, Result).
sumBetweenIndexes(L1, P1, P2,Temp, Result):-
  P1 #< P2,
  element(P1, L1, Value),
  Temp2 #= Temp + Value,
  Idx #= P1 + 1,
  sumBetweenIndexes(L1,Idx,P2,Temp2, Result).

calcSum(Value, L1) :-
  giveBlackenIndexes(L1, P1, P2),
  sumBetweenIndexes(L1, P1, P2, 0, Result),
  Value #= Result.

eachRowSum([S1|ST],[R1|SR]):-
  length(ST, N), N #>0,
  eachRowSum(ST,SR),
  calcSum(S1,R1).

eachRowSum(_,_).

%% Code for in test phase!!!

createCoeffs(Length,Length,_,_,Output, Output).
createCoeffs(Counter, Length ,Idx1, Idx2, Temp, Output) :-
  (Counter #> Idx1 #/\ Counter #< Idx2) #<=> Var,
  Temp2 #= Counter + 1 #/\
  createCoeffs(Temp2, Length, Idx1, Idx2, [Var|Temp], Output).

calcSum3(Value, L1) :-
  giveBlackenIndexes(L1, P1, P2),
  sumBetweenIndexes2(L1, P1, P2,Value).

sumBetweenIndexes2(L1, P1, P2,Value) :-
  length(L1, N1), write(N1),nl, write(P1), nl,  write(P2),nl,
  createCoeffs(0, N1 ,P1, P2, [], TempList),
  write('puta'),
  reverse(TempList, Output),
  write('cenas'),nl, write(Output), nl, write(L1),
  scalar_product(Output,L1,#= ,X), write(X).

eachRowSum3([S1,S2,S3,S4,S5,S6],A,B,C,D,E,F):-
  calcSum3(S1,A).%  calcSum3(S2,B),
%  calcSum3(S3,C), calcSum3(S4,D).
%  calcSum(S5,E),   calcSum(S6,F).

%createCoeffs([2,3,4,5,6,1,2,3,4],0,3,5,[],L).

sumBetweenIndexes3([], _, _, _,Result, Result):- write('lel').
sumBetweenIndexes3([H|T], Counter, P1, P2,Temp, Result):-
  (write('entrei') #/\ Counter #> P1 #/\ write('lel') #/\
   Counter #< P2 #/\
    element(Counter, [H|T], Value) #/\
    Temp2 #= Temp + Value #/\
    Idx #= Counter + 1 #/\
    sumBetweenIndexes3(T,Idx,P1,P2,Temp2, Result))
  #\
  (write('tou aqui') #/\ Idx #= Counter + 1 #/\
  sumBetweenIndexes3(T,Idx,P1,P2,Temp, Result)).

test_board([
  [4, 8, 4, 5, 6, 5], % Column Sum
  [9, 7, 2, 10, 3, 1], % Row Sum
  _ % Map
  ]).

reset_timer :- statistics(walltime,_).
print_time :-
        statistics(walltime,[_,T]),
        TS is ((T//10)*10)/1000,
nl, write('Time: '), write(TS), write('s'), nl, nl.
