%*********************       Logic operators         *********************%

% Not operator
not(X):- X, !, fail.
not(_).

% Or operator -> Checks if 1st arg is equal to 2nd or 3rd
or(X,X,_).
or(X,_,X).
or(A,B) :- A;B.

% And operator
and(A,B) :- A,B.

% Nand operator
nand(A,B) :- not(and(A,B)).

% Xor operator
xor(A,B) :- or(A,B), nand(A,B).

% Abs operator
abs(X,Y):- X<0, Y is 0-X.
abs(X,Y):- X>=0, Y is X.

% If operator
ite(Condition,Then,_):-Condition,!,Then.
ite(_,_,Else):-Else.

%*********************       Input/Output functions         *********************%

getChar(X):-    get_char(X),
                                get_char(_).

getCode(X):-    get_code(X),
                                get_code(_).

waitForKey:-
                get_char(_).

clearConsole:- write('\33\[2J').

% Outputs to the screen the error message passed as argument and then waits for a key, failing afterwards.
outputMessage(Message):-
    write(Message), nl,
    write('Press enter to continue.'),
    waitForKey, !,
    fail.

%*********************       List manipulation         *********************%

% Gets the list length
listLength([], Length, Length).
listLength([_|Tail], Counter, Length):-
    NewLength is Counter + 1,
    listLength(Tail, NewLength, Length).

% Gives the list with the matrix elements that satisfies the belongsTo rule accordingly to it's owner
getMatrixElements([], [], _, _).
getMatrixElements([Head|Tail], Units, Owner, Row) :-
    NewRow is Row + 1,
    getRowElements(Head, RowUnits, Owner, Row, 0),
    getMatrixElements(Tail, BoardUnits, Owner, NewRow),
    append(RowUnits, BoardUnits, Units).

getRowElements([],[], _, _, _).
getRowElements([Head|Tail], [Row-Column|ListTail], Owner, Row, Column) :-
    NewCol is Column + 1,
    belongsTo(Head, Owner),
    getRowElements(Tail, ListTail, Owner, Row, NewCol).

getRowElements([_|Tail], List, Owner, Row, Column) :-
    NewCol is Column + 1,
    getRowElements(Tail, List, Owner, Row, NewCol).

% Parses Row-Column format present at the list to the arguments.
getElement([Row-Column|_], Row, Column).
getElement([_|Tail], Row, Column):-
    getElement(Tail, Row, Column).

% Receives Matrix and element position. iterates through the lines until it finds the correct line and calls getRowElement
getMatrixElement(_, Row, _, null):-
    or(
        Row < 0,
        Row >= 9
    ).
getMatrixElement(_, _, Column, null):-
    or(
        Column < 0,
        Column >= 9
    ).
getMatrixElement([Line|_], 0, Column, Piece):-
    getRowElement(Line, Column, Piece).
getMatrixElement([_|RestOfBoard], Row, Column, Piece):-
    Row > 0,
    NewRow is Row-1,
    getMatrixElement(RestOfBoard, NewRow, Column, Piece).

% Finds the respective column and stores the piece.
getRowElement([Piece|_], 0, Piece).
getRowElement([_|RestOfBoard], Column, Piece):-
    Column > 0,
    NewColumn is Column-1,
    getRowElement(RestOfBoard, NewColumn, Piece).

% Receives Board and element position. iterates through the lines until it finds the correct line and calls setRowElement
setMatrixElement([HeadRow|_], [NewHeadRow|_], 0, ColIndex, Piece):-
    setRowElement(HeadRow, NewHeadRow, ColIndex, Piece).
setMatrixElement([HeadRow|Rows], [HeadRow|NewRows], RowIndex, ColIndex, Piece):-
    RowIndex > 0,
    NewRowIndex is RowIndex-1,
    setMatrixElement(Rows, NewRows, NewRowIndex, ColIndex,Piece).

% Finds the respective column and replaces the piece.
setRowElement([_|Columns], [Piece|Columns], 0, Piece).
setRowElement([HeadCol|Columns], [HeadCol|ResultColumns], ColIndex, Piece):-
    ColIndex > 0,
    NewColIndex is ColIndex-1,
    setRowElement(Columns, ResultColumns, NewColIndex, Piece).

% Receives Board and element position. iterates through the lines until it finds the correct line and calls replaceInRow. NewBoard will contain the updated matrix
replace(Col, 0, Piece, [Line|RestOfBoard], NewBoard):-
    replaceInRow(Col, Piece, Line, NewLine),
    append([NewLine], RestOfBoard, NewBoard).

replace(Col, Row, Piece, [Line|RestOfBoard], NewBoard):-
    Row > 0,
    NewRow is Row-1,
    replace(Col, NewRow, Piece, RestOfBoard, IntermediateBoard),
    append([Line], IntermediateBoard, NewBoard).

%Replaces the element position and stores the new information at NewLine
replaceInRow(0, Piece, [_|RestOfLine], NewLine):-
    append([Piece], RestOfLine, NewLine).

replaceInRow(Col, Piece, [Element|RestOfLine], NewLine):-
    Col > 0, NewCol is Col-1,
    replaceInRow(NewCol, Piece, RestOfLine, IntermediateLine),
    append([Element], IntermediateLine, NewLine).
