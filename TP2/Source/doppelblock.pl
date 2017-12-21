:- include('board.pl'). %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic cardinalityList/1.

doppelBlock(Size) :-
        
        generateDoppel(Size, Doppel),
        solveDoppel(Doppel).

solveDoppel([CSum, RSum, Rows]) :-

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

        reset_time, !,
        maplist(labeling([bisect, down]), Columns),
        get_time(Time),
        
        write('Puzzle solved:'), nl,
        maplist(writeBoard, Columns), nl,
        print_time(Time).

writeBoard(Line) :-
        write(Line), nl.

/**************************************************************************
                            Board generation
***************************************************************************/

generateDoppel(Size, Doppel) :-

        retractall(cardinalityList(_)),

        getInitialBoard(Size, Rows),

        defineDomain(Size, Rows),

        %first restrition
        defineCardinality(Size, Rows),

        %second restrition
        eachRowBlacken(Rows),
        transpose(Rows, Columns),
        eachRowBlacken(Columns),

        maplist(noConsecutiveBlackCells, Rows),
        maplist(noConsecutiveBlackCells, Columns),

        maplist(labeling([value(myRandomSel)]), Columns),

        convertToDoppel(Columns, Doppel),
        write('Puzzle generated (Column Sum | Row Sum | Solution)'), nl,
        write(Doppel), nl, nl.

myRandomSel(Var, _Rest, BB, BB1) :-
        fd_set(Var, Set),
        findall(Value, fdset_member(Value, Set), List),
        length(List, Len),
        random(0, Len, Number),
        nth0(Number, List, NewValue),
        (   
           first_bound(BB, BB1), Var #= NewValue
        ;   
           later_bound(BB, BB1), Var #\= NewValue
        ).

genBoardArcs(0, ListBuild, ListBuild).

genBoardArcs(N, ListBuild, List) :-
        N > 0,
        append(ListBuild, [arc(q0,N,q0), arc(q1,N,q2), arc(q2,N,q2), arc(q3,N,q3)], NewList),
        NewN is N - 1,
        genBoardArcs(NewN, NewList, List).

noConsecutiveBlackCells(Line) :-
        length(Line, N), NewN #= N-2,
        genBoardArcs(NewN, [arc(q0,0,q1), arc(q2,0,q3)], List),
        automaton(Line, [source(q0), sink(q3)], List).

/**************************************************************************
                    Convert generated board to puzzle
***************************************************************************/

convertToDoppel(List, Doppel) :-
        buildSumList(List, [], RowSum),
        transpose(List, TList),
        buildSumList(TList, [], ColumnSum),
        append([ColumnSum], [RowSum], CRList),
        append(CRList, [_], Doppel).

buildSumList([], ListBuild, ListBuild).

buildSumList([H|T], ListBuild, SumList) :-
        nth0(ZeroIndex, H, 0), %Find first black cell
        Index is ZeroIndex + 1,
        getListBetweenBlackCells(H, Index, [], ListToSum),
        sumlist(ListToSum, Sum),
        append(ListBuild, [Sum], NewList),
        buildSumList(T, NewList, SumList).

getListBetweenBlackCells(Line, Index, ListBuild, FinalList) :-
        nth0(Index, Line, Value),
        Value \= 0, !,
        append(ListBuild, [Value], NewList),
        NewIndex is Index + 1,
        getListBetweenBlackCells(Line, NewIndex, NewList, FinalList).

getListBetweenBlackCells(_, _, ListBuild, ListBuild).

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

reset_time :- statistics(walltime,_).

get_time(TS) :-
        statistics(walltime, [_,T]),
        TS is ((T // 10) * 10) / 1000.

print_time(TS) :- write('Time: '), write(TS), write('s'), nl.