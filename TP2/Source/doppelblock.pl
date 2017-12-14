% :- include('utils.pl').         %Utility predicates
% :- include('board.pl').       %Board display predicates
% :- include('solver.pl').            %CPU movement predicates
% :- include('display.pl').   %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).

doppelBlock([CSum, RSum, Rows]) :-

        length(CSum, N),
        getInitialBoard(N, Rows),

        defineDomain(N, Rows),

        %first restrition
        defineCardinality(N, Rows),

        %second restrition
        eachRowBlacken(Rows),
        transpose(Rows, Columns),
        eachRowBlacken(Columns),

        %third restrition
      eachRowSum(RSum, Rows),
      eachRowSum(CSum, Columns),

      reset_timer,
      maplist(labeling([]), Columns), print_time,
      maplist(writeBoard, Columns).

writeBoard(Line) :-
  write(Line), nl.

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
        maplist(define_Cardinality, Rows),
        transpose(Rows, Columns),
        maplist(define_Cardinality, Columns).

define_Cardinality(X) :-
  cardinalityList(Lista),
  global_cardinality(X,Lista).

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

eachRowSum([S1|ST], [R1|SR]):-
  length(ST, N), N #>0,
  eachRowSum(ST, SR),
  sumInLine(R1, S1).

eachRowSum(_,_).

getArc(0,Temp,Temp,_).

getArc(N,Temp, List, Counter) :-
  N > 0,
  append(Temp, [arc(q0,N,q0), arc(q1,N,q1,[Counter+N]), arc(q2,N,q2)], NewList),
  NewN is N-1,
  getArc(NewN, NewList, List, Counter).

sumInLine(Line, Value):-
  length(Line,N), NewN #= N-2,
  getArc(NewN, [arc(q0,0,q1), arc(q1,0,q2)], List, Counter),
  automaton(Line, _, Line, [source(q0), sink(q2)], List, [Counter], [0], [Value]).

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
