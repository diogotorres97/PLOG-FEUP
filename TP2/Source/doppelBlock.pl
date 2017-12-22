:- include('testBoards.pl'). %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic cardinalityList/1.

/**************************************************************************
                            Entry point / Menus
***************************************************************************/

doppelBlock(Size) :-

        generateDoppel(Size, Doppel),
        solveDoppel(Doppel).

/**************************************************************************
                            Doppelblock solver
***************************************************************************/

solveDoppel([CSum, RSum, Rows]) :-

        retractall(cardinalityList(_)),

        length(CSum, N),
        getInitialBoard(N, Rows),

        defineDomain(N, Rows),

        %first restrition - in  each  row  and  each  column restrict to 
		%exactly  two cells blackened  and  each  number(between 1 and N-2)  appears  exactly  once
        defineCardinality(N, Rows),

        %second restrition - sum of the numbers between the two black cells in the row and column corresponds to a specific value
		maplist(restrictSumInLine, Rows, RSum),
		transpose(Rows, Columns),
		maplist(restrictSumInLine, Columns, CSum),

        resetTime, !,
        maplist(labeling([bisect, down]), Columns),
        getTime(Time),
        
        write('Puzzle solved:'), nl,
        maplist(printLine, Columns), nl,
        printTime(Time).

/**************************************************************************
                            Board generation
***************************************************************************/

% Generates a Doppelblock puzzle of given Size (4x4 upwards)
generateDoppel(Size, Doppel) :-

        retractall(cardinalityList(_)),

        getInitialBoard(Size, Rows),

        defineDomain(Size, Rows),

        %first restrition - in each row and each column restrict to
		%exactly two cells blackened and each number(between 1 and N-2) appears exactly once
        defineCardinality(Size, Rows),

        % Restrict black cells so they can't be consecutive
        maplist(noConsecutiveBlackCells, Rows),
		transpose(Rows, Columns),
        maplist(noConsecutiveBlackCells, Columns),

        maplist(labeling([value(myRandomSel)]), Columns),

        convertToDoppel(Columns, Doppel),
        write('Puzzle generated (Column Sum | Row Sum | Solution)'), nl,
        write(Doppel), nl, nl.

% Select a random solution for generating a different board each try
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

/**************************************************************************
                    Convert generated board to puzzle
***************************************************************************/

% Builds a list of values between black cells, sums them into a list with the column sum and row sum needed for a Doppelblock puzzle
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

% Define domain of each variable to values between 0 and N-2
defineDomain(N, Rows):-
        MaxN #= N - 2,
        maplist(define_domain(MaxN), Rows).

define_domain(MaxN,X):- domain(X, 0, MaxN).

% For each row and each column put exactly two cells black and each number between 1 and N-2 appears exactly once
defineCardinality(N, Rows) :-
        createCardinality(N, 1, [0-2], List),
        assert(cardinalityList(List)),
        maplist(define_Cardinality, Rows),
        transpose(Rows, Columns),
        maplist(define_Cardinality, Columns).

define_Cardinality(X) :-
        cardinalityList(Lista),
        global_cardinality(X,Lista).

% Creates the Cardinality List in format [0-2,1-1,2-1,...,(N-2)-1]
createCardinality(Length, Counter, List, List) :- Counter is Length - 1.

createCardinality(Length, Counter, Temp, List) :-
        append([Counter - 1], Temp, Temp2),
        Counter =< Length - 2,
        Tmp is Counter + 1,
        createCardinality(Length, Tmp, Temp2, List).

% Finished building automaton
sumArcs(0, Temp, Temp, _).

% Automaton state transitions needed to count the value of sums between black cells
sumArcs(N, Temp, List, Counter) :-
        N > 0,
        append(Temp, [arc(q0,N,q0), arc(q1, N, q1, [Counter + N]), arc(q2, N, q2)], NewList),
        NewN is N - 1,
        sumArcs(NewN, NewList, List, Counter).

% Restricts sum between black cells to a specific value
restrictSumInLine(Line, Value):-
        length(Line,N), NewN #= N-2,
        sumArcs(NewN, [arc(q0,0,q1), arc(q1,0,q2)], List, Counter),
        automaton(Line, _, Line, [source(q0), sink(q2)], List, [Counter], [0], [Value]).

% Finished building automaton
genBoardArcs(0, ListBuild, ListBuild).

% Automaton state transitions needed to assure no 2 black cells are placed consecutively
genBoardArcs(N, ListBuild, List) :-
        N > 0,
        append(ListBuild, [arc(q0, N, q0), arc(q1, N, q2), arc(q2, N, q2), arc(q3, N, q3)], NewList),
        NewN is N - 1,
        genBoardArcs(NewN, NewList, List).

% Restricts black cells so they can't be consecutive
noConsecutiveBlackCells(Line) :-
        length(Line, N), NewN #= N - 2,
        genBoardArcs(NewN, [arc(q0, 0, q1), arc(q2, 0, q3)], List),
        automaton(Line, [source(q0), sink(q3)], List).

/**************************************************************************
                            Utility predicates
***************************************************************************/

% Create a NxN matrix
getInitialBoard(N, Board) :-
        length(Board, N),
        maplist(same_length(Board), Board).

% Writes line from list
printLine(Line) :-
        write(Line), nl.

% Clear console
clearConsole :- write('\33\[2J').

% Reset time
resetTime :- statistics(walltime, _).

% Get time elapsed in seconds
getTime(TS) :-
        statistics(walltime, [_,T]),
        TS is ((T // 10) * 10) / 1000.

% Print time elapsed since last "reset_time" call
printTime(TS) :- write('Time: '), write(TS), write('s'), nl.