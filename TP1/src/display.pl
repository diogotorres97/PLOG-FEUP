init([
        ['G','Y','R','B','G','Y','Y','G','Y','G','G','B'],
        ['G','G','B','G','G','G','Y','G','Y','Y','G','B'],
        ['G','Y','Y','G','G','Y','Y','G','G','Y','G','R'],
        ['G','Y','Y','G','Y','Y','R','R','R','G','G','R'],
        ['R','G','Y','G','G','G','Y','Y','G','Y','Y','G'],
        ['G','Y','Y','R','Y','G','Y','Y','Y','G','Y','R'],
        ['Y','Y','G','G','B','G','G','G','G','G','G','G'],
        ['R','G','Y','R','G','G','G','R','Y','Y','Y','G'],
        ['Y','R','G','Y','Y','G','G','Y','G','Y','R','G'],
        ['G','Y','Y','G','G','R','G','Y','R','R','Y','G'],
        ['B','G','Y','Y','G','R','G','Y','Y','G','Y','G'],
        ['R','G','G','G','Y','R','G','Y','Y','R','G','G']]).

%Pads line with last hyfhen
printSeparatingLine(Count, Count) :-
        write('-').

%Prints a hyfhen separating line with 6 * Counter chars
printSeparatingLine(InitCount, Counter) :-
        write('------'),
        NewCounter is Counter + 1,
        printSeparatingLine(InitCount, NewCounter).

%Pads line with last |
printColumnSeparatorLine(Count, Count) :-
        write('|').

%Prints a | separated line with 6 * Counter chars
printColumnSeparatorLine(InitCount, Counter) :-
        write('|     '),
        NewCounter is Counter + 1,
        printColumnSeparatorLine(InitCount, NewCounter).

%Line finished printing
printLine([]).

%Prints a line with padding
printLine([First|Tail]) :-
        write('|  '),
        write(First),
        write('  '),
        printLine(Tail).

%Header finished printing
printHeader(Count, Count).

%Prints a header starting with A and ending on A + Counter
printHeader(InitCount, Counter) :-
        CharCode is Counter + 65,
        write('   '),
        put_code(CharCode),
        write('  '),
        NewCounter is Counter + 1,
        printHeader(InitCount, NewCounter).

%Board finished printing
printBoard([], _) :-
        printSeparatingLine(12, 0), nl.

%Prints a list of lists representing a board, displays in a grid-like array
printBoard([First|Tail], CurrLine) :-
        printSeparatingLine(12, 0), nl,
        printLine(First), write('| '), write(CurrLine), nl,
        NewCurrLine is CurrLine + 1,
        printBoard(Tail, NewCurrLine).

%Prints a list of lists representing a board, prints a header then the board
displayBoard(Board) :-
        printHeader(12, 0), nl,
        printColumnSeparatorLine(12, 0), nl,
        printBoard(Board, 1).