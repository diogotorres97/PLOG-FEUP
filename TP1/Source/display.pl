initial([
        ['Y','Y','Y','G','G','G','G','G','G','G','Y','Y'],
        ['G','R','R','R','R','Y','Y','G','G','Y','Y','Y'],
        ['G','G','Y','R','G','G','G','Y','G','G','Y','B'],
        ['R','G','Y','R','R','G','G','G','G','G','Y','Y'],
        ['G','G','Y','Y','R','G','G','G','G','G','Y','Y'],
        ['R','Y','Y','G','G','R','R','G','G','Y','Y','Y'],
        ['G','G','Y','Y','G','Y','G','G','Y','G','Y','Y'],
        ['G','Y','Y','B','G','Y','Y','G','G','Y','Y','Y'],
        ['R','Y','Y','G','G','G','R','B','Y','G','Y','Y'],
        ['R','B','Y','R','G','G','Y','R','B','Y','G','B'],
        ['G','G','G','R','G','R','G','R','Y','G','G','Y'],
        ['G','Y','G','G','G','R','G','Y','G','G','G','Y']]).

ongoing([
        ['Y','Y','Y','G','G','G','G','G','G','G','Y','Y'],
        ['G','R','R','R','R','Y','Y','G','G','Y','Y','Y'],
        ['G','G','Y',' ','G','G','G',' ',' ',' ',' ',' '],
        ['R','G','Y',' ',' ','G',' ',' ','G',' ','Y','Y'],
        ['G','G',' ',' ',' ',' ',' ',' ',' ',' ','Y','Y'],
        ['R','Y','Y',' ','G',' ',' ','G',' ',' ','Y','Y'],
        ['G','G','Y','Y','G',' ','G','G',' ','G','Y','Y'],
        ['G','Y','Y','B','G','Y','Y','G','G','Y','Y','Y'],
        ['R','Y','Y','G','G','G','R','B','Y','G','Y','Y'],
        ['R','B','Y','R','G','G','Y','R','B','Y','G','B'],
        ['G','G','G','R','G','R','G','R','Y','G','G','Y'],
        ['G','Y','G','G','G','R','G','Y','G','G','G','Y']]).

final([
        ['Y',' ',' ',' ',' ','G',' ',' ',' ',' ','G',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ','G',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ','R',' ',' ',' ',' ',' ',' ','Y',' ','Y'],
        ['G',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','Y'],
        ['G',' ',' ','G',' ',' ',' ',' ',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
        [' ','Y',' ',' ','G',' ','Y',' ',' ','B',' ','Y'],
        [' ',' ',' ',' ',' ',' ',' ','G',' ',' ',' ',' '],
        [' ',' ',' ',' ',' ',' ','B',' ','G',' ',' ',' ']]).

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