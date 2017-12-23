:- include('testBoards.pl'). %Boards used for testing

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic cardinalityList/1.

/**************************************************************************
                            Entry point / Menus
***************************************************************************/

% Program entry point, queries user about board size and type and then presents solution
doppelBlock :-

        doppelTypeMenu(Type),
        doppelBoardMenu(Type, Doppel),

        clearConsole,
        printDoppelPuzzle(Doppel), nl,
        solveDoppel(Doppel, Solution, Time),
        printDoppelSolution(Doppel, Solution), nl,
        printTime(Time).

% Query user for type of doppel puzzle to use (premade or generated)
doppelTypeMenu(Type) :-
        
        repeat,
        clearConsole,
        
        write('Choose type of puzzle:'), nl,
        write('1 - premade'), nl,
        write('2 - generated'), nl,
        read_line(Input),
        registerType(Input, Type).
        
% Validate user doppel type selection and store it
registerType("1", premade).
registerType("2", generated).

% Queries user for premade board size to solve
doppelBoardMenu(premade, Doppel) :-
        
        repeat,
        clearConsole,
        
        write('Premade puzzle size?'), nl,
        write('1 - Size 4'), nl,
        write('2 - Size 5'), nl,
        write('3 - Size 6'), nl,
        write('4 - Size 7'), nl,
        write('5 - Size 8'), nl,
        read_line(Input),
        registerBoard(premade, Input, Doppel).

% Queries user for generated board size
doppelBoardMenu(generated, Doppel) :-
        
        repeat,
        clearConsole,
        
        write('Puzzle size to generate:'), nl,
        write('1 - Size 4'), nl,
        write('2 - Size 5'), nl,
        write('3 - Size 6'), nl,
        write('4 - Size 7'), nl,
        write('5 - Size 8'), nl,
        write('6 - Size 9'), nl,
        write('7 - Size 10'), nl,
        read_line(Input),
        registerBoard(generated, Input, Doppel).

% Validates user premade board size selection and unifies with premade boards
registerBoard(premade, "1", Doppel) :- boardS4(X), Doppel = X.
registerBoard(premade, "2", Doppel) :- boardS5(X), Doppel = X.
registerBoard(premade, "3", Doppel) :- boardS6(X), Doppel = X.
registerBoard(premade, "4", Doppel) :- boardS7(X), Doppel = X.
registerBoard(premade, "5", Doppel) :- boardS8(X), Doppel = X.

% Validates user generated board size selection and generates the board
registerBoard(generated, "1", Doppel) :- generateDoppel(4, Doppel).
registerBoard(generated, "2", Doppel) :- generateDoppel(5, Doppel).
registerBoard(generated, "3", Doppel) :- generateDoppel(6, Doppel).
registerBoard(generated, "4", Doppel) :- generateDoppel(7, Doppel).
registerBoard(generated, "5", Doppel) :- generateDoppel(8, Doppel).
registerBoard(generated, "6", Doppel) :- generateDoppel(9, Doppel).
registerBoard(generated, "7", Doppel) :- generateDoppel(10, Doppel).

/**************************************************************************
                           Doppelblock display
***************************************************************************/

% Prints the doppel board in a human friendly way
printDoppelPuzzle(Doppel) :-
        
        nth0(0, Doppel, CSum),
        nth0(1, Doppel, RSum),
        
        write('Doppelblock:'), nl,
        write('   '),
        
        % Prints column sum with even spacing
        maplist(printSpaced, CSum), nl,
        
        length(RSum, Len),
        
        % Create list of dots with the same size as the board
        length(Solution, Len),
        maplist(listToDot, Solution),
        
        printDoppelLine(RSum, Solution).

% Prints the doppel solution in a human friendly way
printDoppelSolution(Doppel, Solution) :-
        
        nth0(0, Doppel, CSum),
        nth0(1, Doppel, RSum),
        
        write('Doppelblock solution:'), nl,
        write('   '),
        
        % Prints column sum with even spacing
        maplist(printSpaced, CSum), nl,
        
        % Prints row sum and solution lines
        printDoppelSolutionLine(RSum, Solution).

% Converts list to ASCII '.'
listToDot(Element) :-
        Element = '.'.

% Prints a formatted doppelblock solution line
printDoppelSolutionLine([], []).

printDoppelSolutionLine([HSum|TSum], [HSol|TSol]) :-
        formatRowSum(HSum),
        maplist(printSpaced, HSol), nl,
        printDoppelSolutionLine(TSum, TSol).

% Prints a formatted doppelblock puzzle line
printDoppelLine([], _).

printDoppelLine([HSum|TSum], Solution) :-
        formatRowSum(HSum),
        maplist(printSpaced, Solution), nl,
        printDoppelLine(TSum, Solution).

% Pads row sum to align elements according to number of digits
formatRowSum(Number) :-
        Number > 9, !,
        write(Number), write(' ').

formatRowSum(Number) :-
        write(Number), write('  ').

% Converts and pads solution numbers
formatNumber(Number) :-
        Number > 9, !,
        write(Number), write(' ').

formatNumber(Number) :-
        Number == 0, !,
        write('.'), write('  ').

formatNumber(Number) :-
        write(Number), write('  ').

% Prints a list with one space per element, adds padding if number
printSpaced(Element) :-
        number(Element), !,
        formatNumber(Element).

printSpaced(Element) :-
        write(Element), write('  ').

/**************************************************************************
                            Doppelblock solver
***************************************************************************/

% Solves a Doppelblock puzzle
solveDoppel([CSum, RSum, Rows], Solution, Time) :-

        retractall(cardinalityList(_)),

        length(CSum, N),
        getInitialBoard(N, Rows),

        defineDomain(N, Rows),

        % First restrition - in each row and column restrict to
	% exactly two black cells and one of each number between 1 and N - 2
        defineCardinality(N, Rows),

        % Second restrition - in each row and column restrict the sum
        % of the numbers between the black cells to a specific value
        maplist(restrictSumInLine, Rows, RSum),
	transpose(Rows, Columns),
	maplist(restrictSumInLine, Columns, CSum),

        resetTime, !,
        maplist(labeling([bisect, down]), Rows),
        getTime(Time),
        
        Solution = Rows.

/**************************************************************************
                            Board generation
***************************************************************************/

% Generates a Doppelblock puzzle of given Size (4x4 upwards)
generateDoppel(Size, Doppel) :-

        retractall(cardinalityList(_)),

        getInitialBoard(Size, Rows),

        defineDomain(Size, Rows),

        % First restrition - in each row and column restrict to
        % exactly two black cells and one of each number between 1 and N - 2
        defineCardinality(Size, Rows),

        % Second restrition - in each row and column restrict
        % black cells so they can't be consecutive
        maplist(noConsecutiveBlackCells, Rows),
        transpose(Rows, Columns),
        maplist(noConsecutiveBlackCells, Columns),

        maplist(labeling([value(myRandomSel)]), Rows),

        convertToDoppel(Rows, Doppel).

% Select a random solution for generating a different board each attempt
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

% Define domain of each variable to values between 0 and N - 2
defineDomain(N, Rows):-
        MaxN #= N - 2,
        maplist(define_domain(MaxN), Rows).

define_domain(MaxN,X):- domain(X, 0, MaxN).

% For each row and column restrict to exactly two black cells and one of each number between 1 and N - 2
defineCardinality(N, Rows) :-
        createCardinality(N, 1, [0-2], List),
        assert(cardinalityList(List)),
        maplist(define_Cardinality, Rows),
        transpose(Rows, Columns),
        maplist(define_Cardinality, Columns).

define_Cardinality(X) :-
        cardinalityList(Lista),
        global_cardinality(X,Lista).

% Creates the Cardinality List in format [0-2, 1-1, 2-1, ..., (N-2)-1]
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