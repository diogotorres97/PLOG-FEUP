convertToDisplay(0,' ').
convertToDisplay(1,'G').
convertToDisplay(2,'Y').
convertToDisplay(3,'R').
convertToDisplay(4,'B').

initial([
           [2,2,2,1,1,1,1,1,1,1,2,2],
           [1,3,3,3,3,2,2,1,1,2,2,2],
           [1,1,2,3,1,1,1,2,1,1,2,4],
           [3,1,2,3,3,1,1,1,1,1,2,2],
           [1,1,2,2,3,1,1,1,1,1,2,2],
           [3,2,2,1,1,3,3,1,1,2,2,2],
           [1,1,2,2,1,2,1,1,2,1,2,2],
           [1,2,2,4,1,2,2,1,1,2,2,2],
           [3,2,2,1,1,1,3,4,2,1,2,2],
           [3,4,2,3,1,1,2,3,4,2,1,4],
           [1,1,1,3,1,3,1,3,2,1,1,2],
           [1,2,1,1,1,3,1,2,1,1,1,2]]).

ongoing([
           [2,2,2,1,1,1,1,1,1,1,2,2],
           [1,3,3,3,3,2,2,1,1,2,2,2],
           [1,1,2,0,1,1,1,0,0,0,0,0],
           [3,1,2,0,0,1,0,0,1,0,2,2],
           [1,1,0,0,0,0,0,0,0,0,2,2],
           [3,2,2,0,1,0,0,1,0,0,2,2],
           [1,1,2,2,1,0,1,1,0,1,2,2],
           [1,2,2,4,1,2,2,1,1,2,2,2],
           [3,2,2,1,1,1,3,4,2,1,2,2],
           [3,4,2,3,1,1,2,3,4,2,1,4],
           [1,1,1,3,1,3,1,3,2,1,1,2],
           [1,2,1,1,1,3,1,2,1,1,1,2]]).

final([
         [2,0,0,0,0,1,0,0,0,0,1,0],
         [0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,1,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,3,0,0,0,0,0,0,2,0,2],
         [1,0,0,0,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,2],
         [1,0,0,1,0,0,0,0,0,0,0,0],
         [0,0,0,0,0,0,0,0,0,0,0,0],
         [0,2,0,0,1,0,2,0,0,4,0,2],
         [0,0,0,0,0,0,0,1,0,0,0,0],
         [0,0,0,0,0,0,4,0,1,0,0,0]]).

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
        convertToDisplay(First,X),
        write(X),
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
        printColumnSeparatorLine(12, 0), nl,!,
        printBoard(Board, 1).

isEmpty(0).

ite(Condition,Then,_):-Condition,!,Then.
ite(_,_,Else):-Else.

% Abs operator
abs(X,Y):- X<0, Y is 0-X.
abs(X,Y):- X>=0, Y is X.

or(A,B) :- A;B.

% Receives Matrix and element position. iterates through the lines until it finds the correct line and calls getRowElement
getMatrixElement(_, Row, _, null):-
        or(
             Row < 0,
             Row >= 12
          ).
getMatrixElement(_, _, Column, null):-
        or(
             Column < 0,
             Column >= 12
          ).
getMatrixElement([Line|_], 0, Column, Cell):-
        getRowElement(Line, Column, Cell).
getMatrixElement([_|RestOfBoard], Row, Column, Cell):-
        Row > 0,
        NewRow is Row-1,
        getMatrixElement(RestOfBoard, NewRow, Column, Cell).

% Finds the respective column and stores the piece.
getRowElement([Cell|_], 0, Cell).
getRowElement([_|RestOfBoard], Column, Cell):-
        Column > 0,
        NewColumn is Column-1,
        getRowElement(RestOfBoard, NewColumn, Cell).


%Asks the user to select a piece
getCoords(Row, Column):-
	write('Row coordinate: '),
	getCode(RowInput),
	write('Column coordinate: '),
	getCode(ColInput),
	Row is RowInput - 49,
	Column is ColInput - 97.

%checks if the coords are from outside the board's boundary
checkIfOutsideBoard(Row, Column):-
	Row >= 0, Row < 12, Column >= 0, Column < 12.

%check if moves only two square horizontally or diagonally
validateMovement(OriginRow, OriginCol, DestRow, DestCol):-
	abs(OriginRow-DestRow,RowDiff),
	abs(OriginCol-DestCol,ColDiff),
	TotalDiff is RowDiff+ColDiff,
	TotalDiff==2,
        (RowDiff == 0 ; ColDiff == 0).

not(X):- X, !, fail.
not(_).

% And operator
and(A,B) :- A,B.

% Nand operator
nand(A,B) :- not(and(A,B)).

% Xor operator
xor(A,B) :- or(A,B), nand(A,B).

% Outputs to the screen the error message passed as argument and then waits for a key, failing afterwards.
outputMessage(Message):-
        write(Message), nl,
        write('Press enter to continue.'),
        waitForKey, !,
        fail.


%Predicate that is responsible for the player movement
move(Board,Player, FinalBoard):-
        displayBoard(Board),
        repeat,

	%selects the frog to move and checks if it's possible to move it.
	selectSource(Row, Column,Board),

	%selects the coordinates of where to move it
	selectDestiny(DestRow, DestColumn, Board),

        % isJump(Row, Column, DestRow,DestColumn, Board),
        validMove(DestRow, DestColumn, Row, Column, Board),
	moveFrog(Column, Row, DestColumn, DestRow, Board, FinalBoard, Player),

        displayBoard(FinalBoard),
	write('Press enter to continue'), nl,
	waitForKey.

%Repeats until one source is valid
selectSource(Row, Column, Board):-
	repeat,
	clearConsole,
	%Gets the coordinates given by the user and validates the frog status
	nl, write('Select a frog to move it.'), nl,
	getCoords(Row, Column),
        validateSource(Row, Column, Board), !.

%Verifies if the source is valid
validateSource(Row, Column, Board):-
  	%Verifies if the coordinates are from a cell outside the board boundaries
  	checkIfOutsideBoard(Row, Column),
  	%Gets the piece that the user selected.
  	getMatrixElement(Board, Row, Column, Cell), !,
  	%evaluate if is a Cell is empty
  	ite(
  	      not(isEmpty(Cell)),
  	      true,
  	      outputMessage('Empty Cell! Choose another one.')
  	   ), !.


%Repeats until one destiny is valid
selectDestiny(DestRow, DestColumn, Board):-
	repeat,
	%Gets the destiny coordinates given by the user in order to check the condition of that cell
        nl, write('Write the coordinates of where to move it.'), nl,
        getCoords(DestRow, DestColumn),
        validateDestiny(DestRow, DestColumn, Board), !.

%Verifies if the source is valid
validateDestiny(Row, Column, Board):-
        %Verifies if the coordinates are from a cell outside the board boundaries
        checkIfOutsideBoard(Row, Column),
        %Gets the piece that the user selected.
        getMatrixElement(Board, Row, Column, Cell), !,
        write(Cell),
        %evaluate if is a Cell is empty
        ite(
              isEmpty(Cell),
              true,
              outputMessage('Not Empty Cell! Choose another one.')
           ), !.



%Verifies if the move is valid
validMove(DestRow, DestColumn, SrcRow, SrcCol, Board):-
        %check if movement itself is valid. First checks if it is a jump then a normal move.
        ite(
              isJump(SrcRow, SrcCol, DestRow, DestColumn, Board),
              ite(
                    validateMovement(SrcRow,SrcCol,DestRow,DestColumn),
                    true,
                    outputMessage('A frog can be only moved to adjacent cells+1')
                 ),
              fail
           ).

%checks if the respective movement is a jump - between a frog and empty space.
isJump(SrcRow, SrcCol, DestRow, DestCol, Board):-
        abs(SrcRow-DestRow,RowDiff),
        abs(SrcCol-DestCol,ColDiff),
        xor(RowDiff == 2, ColDiff == 2),
        TotalDiff is RowDiff+ColDiff,
        TotalDiff==2,
        getMatrixElement(Board, SrcRow, SrcCol, Cell),!,
        not(isEmpty(Cell)),

        delta(DestRow,SrcRow,X),
        IRow is SrcRow + X,
        delta(DestCol,SrcCol,Y),
        IColumn is SrcCol+Y,
        getMatrixElement(Board, IRow, IColumn, Frog),!,
        write(Frog),
        not(isEmpty(Frog)),

        getMatrixElement(Board, DestRow, DestCol, CellF),!,
        isEmpty(CellF).


delta(Y2, Y1, Y3) :- Y2-Y1>0, !, Y3=1.
delta(Y2, Y1, Y3) :- Y2==Y1, !, Y3=0.
delta(_,_,Y3) :- Y3 = -1.

%Receives destiny and source coordinates and updates that frog coords in the board
moveFrog(FromCol, FromRow, ToCol, ToRow, Board, NewBoard, Player):-
        getMatrixElement(Board, FromRow, FromCol, Frog), %Save in Frog what frog it will move
        replace(Board, FromRow, FromCol, 0, InterBoard),

        delta(ToRow,FromRow,X),
        IRow is FromRow + X,
        delta(ToCol,FromCol,Y),
        IColumn is FromCol+Y,
        getMatrixElement(Board, IRow, IColumn, Points), % Saves in Points what frog it was eatens
        replace( InterBoard,IRow, IColumn, 0, InterBoard2 ),

        replace( InterBoard2 , ToRow, ToCol , Frog , NewBoard ).


replace( [L|Ls] , 0 , Y , Z , [R|Ls] ) :- % once we find the desired row,
        replace_column(L,Y,Z,R).                 % - we replace specified column, and we're done.

replace( [L|Ls] , X , Y , Z , [L|Rs] ) :- % if we haven't found the desired row yet
        X > 0 ,                                 % - and the row offset is positive,
        X1 is X-1 ,                             % - we decrement the row offset
        replace( Ls , X1 , Y , Z , Rs ).         % - and recurse down


replace_column( [_|Cs] , 0 , Z , [Z|Cs] ) .  % once we find the specified offset, just make the substitution and finish up.
replace_column( [C|Cs] , Y , Z , [C|Rs] ) :- % otherwise,
        Y > 0 ,                                    % - assuming that the column offset is positive,
        Y1 is Y-1 ,                                % - we decrement it
        replace_column( Cs , Y1 , Z , Rs ).         % - and recurse down.


getChar(X):-    get_char(X),
        get_char(_).

getCode(X):-    get_code(X),
        get_code(_).

waitForKey:-
        get_char(_).

clearConsole:- write('\33\[2J').
