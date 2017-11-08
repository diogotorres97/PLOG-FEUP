:- include('utils.pl').         %Utility predicates
:- include('display.pl').       %Board display predicates
:- include('ai.pl').            %CPU movement predicates

:- use_module(library(random)).
:- use_module(library(lists)).

:- dynamic currentPlayer/1.     %Current player turn
:- dynamic player1Type/1.
:- dynamic player2Type/1.

/**************************************************************************
                     Main game predicates and menus
***************************************************************************/

playGame :-
        setupGame,
        ongoing(Board),
        assert(currentPlayer(1)),
        gameLoop(Board),
        resetGame.

setupGame :-
        assert(player1Type(cpu)),
        assert(player2Type(cpu)).

resetGame :-
        retractall(currentPlayer(_)),
        retractall(player1Type(_)),
        retractall(player2Type(_)).

gameLoop(Board) :-
        displayBoard(Board),
        currentPlayer(Player),
        writePlayer(Player), nl,
        movement(Board, Player, NewBoard),
        displayBoard(NewBoard),
        ite(endGame(NewBoard), true, gameLoop(NewBoard)).

movement(Board, Player, NewBoard) :-
        cpuMove(Board, Player, NewBoard, hard),
        togglePlayer(Player, NewPlayer),
        retract(currentPlayer(Player)),
        assert(currentPlayer(NewPlayer)).

togglePlayer(Player, NewPlayer) :-
        ite(Player == 1, NewPlayer is 2, NewPlayer is 1).

writePlayer(Player) :-
        
        write('Current player: '), write(Player),
        ite(
           currentPlayer(1),
           ite(player1Type(cpu), write(' (CPU)'), write(' (Human)')),
           ite(player2Type(cpu), write(' (CPU)'), write(' (Human)'))
        ).

%Checks if the game has ended
endGame(Board) :-
        validMoves(Board, [], NewMoves), !, length(NewMoves, 0).

/**************************************************************************
                      Create list of valid moves
***************************************************************************/

%Creates list of valid moves in [Points, InitialPos, FinalPos] format
validMoves(Board,Moves, NewMoves) :-
	validMovesCol(Board, Moves, 0, 0, NewMoves).

validMoves(_, Moves, NewMoves) :- NewMoves = Moves.

validMovesCol(_, Moves, _, 12, NewMoves) :-
	NewMoves = Moves.

validMovesCol(Board, Moves, Row, Column, NewMoves) :-
	Column < 12,
	validMovesRow(Board, Moves, Row, Column, NewMoves1),
	NewColumn is Column + 1,
	validMovesCol(Board, NewMoves1, Row, NewColumn, NewMoves).

validMovesRow(_, Moves, 12, _, NewMoves) :-
	NewMoves = Moves.

validMovesRow(Board, Moves, Row, Column, NewMoves) :-
	Row < 12,
	checkValidMoves(Board, Moves, Row, Column, NewMoves1),
	NewRow is Row + 1,
	validMovesRow(Board, NewMoves1, NewRow, Column, NewMoves).

checkValidMove(Board, Moves, Row, Column, DestRow, DestColumn, NewMoves) :-
	checkIfOutsideBoard(DestRow, DestColumn), validMove(DestRow, DestColumn, Row, Column, Board, Points),
	append([[Points, Row - Column, DestRow - DestColumn]], Moves, NewMoves).

checkValidMove(_Board, Moves, _Row, _Column, _DestRow, _DestColumn, NewMoves) :-
	NewMoves = Moves.

checkValidMoves(Board, Moves, Row, Column, FinalMoves) :-
	NewLeft is Row - 2, NewRight is Row + 2, NewUp is Column + 2, NewDown is Column - 2, !,
	checkValidMove(Board, Moves, Row, Column, NewLeft, Column, Moves1), !,
	checkValidMove(Board, Moves1, Row, Column, NewRight, Column, Moves2), !,
	checkValidMove(Board, Moves2, Row, Column, Row, NewUp, Moves3), !,
	checkValidMove(Board, Moves3, Row, Column, Row, NewDown, FinalMoves).

/**************************************************************************
                      Player movement predicates
***************************************************************************/

%Predicate that is responsible for the player movement
move(Board, Player, FinalBoard):-
        
        repeat,
        
	%Selects the frog to move and checks if it's possible to move it
	selectSource(Row, Column, Board),

	%Selects the coordinates of where to move previously chosen frog
	selectDestination(DestRow, DestColumn, Board),

        validMove(DestRow, DestColumn, Row, Column, Board, Points),
	moveFrog(Column, Row, DestColumn, DestRow, Board, FinalBoard, Player).

%Gets source coordinates from user, repeats until source coordinates are valid
selectSource(Row, Column, Board) :-
        
	repeat,
	nl, write('Select a frog to move.'), nl,
	getCoords(Row, Column),
        validateSource(Row, Column, Board), !.

%Verifies if source coordinates are valid
validateSource(Row, Column, Board) :-
        
  	checkIfOutsideBoard(Row, Column),
        
  	%Gets the piece that the user selected.
  	getMatrixElement(Board, Row, Column, Cell), !,
        
  	%Evaluate if Cell is empty
  	ite(
  	      not(isEmpty(Cell)),
  	      true,
  	      outputMessage('Empty cell! Choose another one.')
  	   ), !.

%Gets destination coordinates from user, repeats until destination coordinates are valid
selectDestination(DestRow, DestColumn, Board) :-
        
	repeat,
        nl, write('Move frog where?'), nl,
        getCoords(DestRow, DestColumn),
        validateDestination(DestRow, DestColumn, Board), !.

%Verifies if destination coordinates are valid
validateDestination(Row, Column, Board) :-
        
        checkIfOutsideBoard(Row, Column),
        
        %Gets the piece that the user selected.
        getMatrixElement(Board, Row, Column, Cell), !,
        
        %Evaluate if Cell is empty
        ite(
              isEmpty(Cell),
              true,
              outputMessage('Not an empty cell! Choose another one.')
           ), !.

/**************************************************************************
                      Overall movement validation
***************************************************************************/

%Verifies if the move is valid by checking if it's a jump over an adjacent frog to an empty space immediately after
validMove(DestRow, DestColumn, SrcRow, SrcCol, Board, Points) :-

        ite(
              isJump(SrcRow, SrcCol, DestRow, DestColumn, Board, Points),
              ite(
                    validateMovement(SrcRow, SrcCol, DestRow, DestColumn),
                    true,
                    outputMessage('Not a valid jump! A frog has to jump over an adjacent frog.')
                 ),
              fail
           ).

%Checks if the respective movement is a jump over an adjacent frog to empty space.
isJump(SrcRow, SrcCol, DestRow, DestCol, Board, Points) :-
        
        abs(SrcRow - DestRow, RowDiff),
        abs(SrcCol - DestCol, ColDiff),
        xor(RowDiff == 2, ColDiff == 2),
        TotalDiff is RowDiff + ColDiff,
        TotalDiff == 2,
        getMatrixElement(Board, SrcRow, SrcCol, Cell), !,
        not(isEmpty(Cell)),

        delta(DestRow, SrcRow, X),
        IRow is SrcRow + X,
        delta(DestCol, SrcCol, Y),
        IColumn is SrcCol + Y,
        getMatrixElement(Board, IRow, IColumn, Points), !,
        not(isEmpty(Points)),

        getMatrixElement(Board, DestRow, DestCol, CellF), !,
        isEmpty(CellF).

delta(Y2, Y1, Y3) :- Y2-Y1>0, !, Y3=1.
delta(Y2, Y1, Y3) :- Y2==Y1, !, Y3=0.
delta(_,_,Y3) :- Y3 = -1.

%Checks if move is only two squares horizontally or vertically
validateMovement(OriginRow, OriginCol, DestRow, DestCol) :-
	abs(OriginRow - DestRow, RowDiff),
	abs(OriginCol - DestCol, ColDiff),
	TotalDiff is RowDiff + ColDiff,
	TotalDiff == 2,
        (RowDiff == 0 ; ColDiff == 0).

/**************************************************************************
                      Board manipulation predicates
***************************************************************************/

%Receives destination and source coordinates and updates frog coordinates on board
moveFrog(FromCol, FromRow, ToCol, ToRow, Board, NewBoard, Player) :-
        
        getMatrixElement(Board, FromRow, FromCol, Frog), %Save what Frog will move
        replace(Board, FromRow, FromCol, 0, InterBoard),

        delta(ToRow, FromRow, X),
        IRow is FromRow + X,
        delta(ToCol, FromCol, Y),
        IColumn is FromCol + Y,
        getMatrixElement(Board, IRow, IColumn, Points), %Saves Points of move
        
        replace(InterBoard, IRow, IColumn, 0, InterBoard2),
        replace(InterBoard2, ToRow, ToCol, Frog, NewBoard).