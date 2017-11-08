:- include('utils.pl').
:- include('display.pl').
:- include('menu.pl').
:- include('ai.pl').
:- use_module(library(random)).
:- use_module(library(lists)).

player(1).
player(2).

gameMode(pvp).

/*
*************************** GAME LOOPS ***************************
*/

playGame :-
        ongoing(Board),
        repeat,
		once(move(Board, 1, NewBoard)),
        once(pickMove(NewBoard, 2, NewerBoard)),
		once(displayBoard(Board)),
        endGame(NewerBoard).

/*
*************************** GAME OVER ***************************
*/

%Checks if the game has ended.
endGame(Board):-
        validMoves(Board,[],NewMoves),!,length(NewMoves,0).

%validMoves
validMoves(Board,Moves, NewMoves):-
	validMovesCol(Board,Moves, 0,0, NewMoves).

validMoves(_, Moves, NewMoves):- NewMoves= Moves.

validMovesCol(_,Moves,_,12,NewMoves):-
	NewMoves=Moves.

validMovesCol(Board,Moves, Row, Column,NewMoves):-
	Column < 12,
	validMovesRow(Board, Moves, Row, Column,NewMoves1),
	NewColumn is Column+1,
	validMovesCol(Board, NewMoves1, Row, NewColumn,NewMoves).

validMovesRow(_,Moves,12,_,NewMoves) :-
	NewMoves=Moves.

validMovesRow(Board,Moves, Row, Column, NewMoves):-
	Row < 12,
	checkValidMoves(Board, Moves, Row, Column,NewMoves1),
	NewRow is Row+1,
	validMovesRow(Board,NewMoves1, NewRow, Column,NewMoves).


checkValidMove(Board, Moves, Row, Column, DestRow, DestColumn, NewMoves):-
	checkIfOutsideBoard(DestRow, DestColumn), validMove(DestRow, DestColumn, Row, Column, Board,Points),
	append([[Points,Row - Column, DestRow - DestColumn]], Moves, NewMoves).

checkValidMove(_Board, Moves, _Row, _Column, _DestRow, _DestColumn, NewMoves):-
	NewMoves = Moves.

checkValidMoves(Board, Moves, Row, Column, FinalMoves):-
	NewLeft is Row-2, NewRight is Row+2, NewUp is Column+2, NewDown is Column-2,!,
	checkValidMove(Board,Moves, Row, Column, NewLeft, Column, Moves1),!,
	checkValidMove(Board,Moves1, Row, Column, NewRight, Column, Moves2),!,
	checkValidMove(Board,Moves2, Row, Column, Row, NewUp, Moves3),!,
	checkValidMove(Board,Moves3, Row, Column, Row, NewDown, FinalMoves).


/*
*************************** MOVEMENT ***************************
*/

%Predicate that is responsible for the player movement
move(Board,Player, FinalBoard):-
        displayBoard(Board),
        repeat,

	%selects the frog to move and checks if it's possible to move it.
	selectSource(Row, Column,Board),

	%selects the coordinates of where to move it
	selectDestiny(DestRow, DestColumn, Board),

  % isJump(Row, Column, DestRow,DestColumn, Board),
  validMove(DestRow, DestColumn, Row, Column, Board, Points),
	moveFrog(Column, Row, DestColumn, DestRow, Board, FinalBoard, Player).

%Repeats until one source is valid
selectSource(Row, Column, Board):-
	repeat,
	%clearConsole,
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
        %evaluate if is a Cell is empty
        ite(
              isEmpty(Cell),
              true,
              outputMessage('Not Empty Cell! Choose another one.')
           ), !.


%Verifies if the move is valid
validMove(DestRow, DestColumn, SrcRow, SrcCol, Board,Points):-
        %check if movement itself is valid. First checks if it is a jump then a normal move.
        ite(
              isJump(SrcRow, SrcCol, DestRow, DestColumn, Board,Points),
              ite(
                    validateMovement(SrcRow,SrcCol,DestRow,DestColumn),
                    true,
                    outputMessage('A frog can be only moved to adjacent cells+1')
                 ),
              fail
           ).

%checks if the respective movement is a jump - between a frog and empty space.
isJump(SrcRow, SrcCol, DestRow, DestCol, Board,Points):-
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
        getMatrixElement(Board, IRow, IColumn, Points),!,
        not(isEmpty(Points)),

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


%check if moves only two square horizontally or diagonally
validateMovement(OriginRow, OriginCol, DestRow, DestCol):-
	abs(OriginRow-DestRow,RowDiff),
	abs(OriginCol-DestCol,ColDiff),
	TotalDiff is RowDiff+ColDiff,
	TotalDiff==2,
        (RowDiff == 0 ; ColDiff == 0).
