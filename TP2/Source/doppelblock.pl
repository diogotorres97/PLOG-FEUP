% :- include('board.pl'). %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic cardinalityList/1.

doppel(Size) :-
        
        repeat,
        once(generateBoard([], Board, Size)),
        once(append(Board, [_A], FinalBoard)),
        once(writeBoard(FinalBoard)),
        once(doppelBlock(FinalBoard)).

doppelBlock([CSum, RSum, Rows]) :-

        retractall(cardinalityList(_)),

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
	maplist(sumInLine, Rows, RSum),
	maplist(sumInLine, Columns, CSum),

        reset_timer, !,
        maplist(labeling([bisect, down]), Columns), print_time,
        maplist(writeBoard, Columns).

writeBoard(Line) :-
        write(Line), nl.

/**************************************************************************
                         Board random generation
***************************************************************************/

generateBoard(Board, FinalBoard, Count) :-
        getBoardMaxSum(Count, Max),
        genLine([], Line, Max, Count),
        append(Board, [Line], NewBoard),
        genLine([], Line2, Max, Count),
        append(NewBoard, [Line2], FinalBoard).
        %append(NewerBoard, [_], FinalBoard).

genLine(Board, OutBoard, _, 0) :- OutBoard = Board.

genLine(Board, OutBoard, Max, Count) :-
        random(1, Max, Number),
        append(Board, [Number], NewBoard),
        NewCount is Count - 1,
        genLine(NewBoard, OutBoard, Max, NewCount).

getBoardMaxSum(Size, Max) :-
        MaxNumber is Size - 2,
        Max is MaxNumber * (MaxNumber + 1) // 2.

/**************************************************************************
                              Restrictions
***************************************************************************/

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