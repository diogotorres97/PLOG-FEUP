:- include('utils.pl').
:- include('display.pl').
:- include('menu.pl').
:- include('ai.pl').
:- use_module(library(random)).
:- use_module(library(lists)).

player(1).
player(2).

gameMode(pvp).

froglet:- mainMenu.

/*
*************************** GAME LOOPS ***************************
*/

% game(Mode,Board,Player). Player vs Player game loop.
game(pvp, Board, Player, _):-
	ite(
        endGame(Board, Player),
        (getOtherPlayer(Player, Winner), write(Winner), write(' player is the winner!!! Congratz.')),
        (move(Board, Player, Piece, FinalBoard), endTurn(Piece, FinalBoard, Player, pvp, null))
    ).

/*
*************************** GAME OVER ***************************
*/

%Checks if the game has ended.
endGame(Board, Player):-
	checkIfGameOver(Board, Player), !.

%will succeed if gameOver happens; fails if cannot gameOver
checkIfGameOver(Board,Player):-
	getNodeCoordinates(Board,Player, NodeRow-NodeCol),
	Row1 is NodeRow+1, Row2 is NodeRow-1, Col1 is NodeCol+1, Col2 is NodeCol-1, !,
	checkMobility(NodeCol,Row1,Player,Board,2),!,
	checkMobility(NodeCol,Row2,Player,Board,2),!,
	checkMobility(Col1,NodeRow,Player,Board,2),!,
	checkMobility(Col2,NodeRow,Player,Board,2).

%will succeed if does not have mobility. will fail if has mobility
checkMobility(Col,Row,_,_,_):- %check if outside board
	or(
                or(Col<0, Col>8),
                or(Row<0, Row>8)
        ).

checkMobility(Col,Row,Player,Board,Iterator):-
    getMatrixElement(Board, Row, Col, Piece), !,
    if1(isEmpty(Piece),fail,
        if1(Iterator =< 0,true,
            if1(belongsTo(Piece,Player),(
                Row1 is Row+1, Row2 is Row-1, Col1 is Col+1, Col2 is Col-1,NewIterator is Iterator-1,
                !,checkMobility(Col,Row1,Player,Board,NewIterator),!,
                checkMobility(Col,Row2,Player,Board,NewIterator),!,
                checkMobility(Col1,Row,Player,Board,NewIterator),!,
                checkMobility(Col2,Row,Player,Board,NewIterator))
                ,true)
    )).

/*
*************************** TURNS ***************************
*/

% Check if it's the end turn in cvc mode
endTurn(NodeRowI-NodeColI, NodeRowF-NodeColF, FinalBoard, Bot, Difficulty, Mode):-
	NodeRowI==NodeRowF,
	NodeColI==NodeColF,
	game(Mode, FinalBoard, Bot, Difficulty).
endTurn(_, _, FinalBoard, Bot, Difficulty, Mode):-
	getOtherPlayer(Bot, Enemy),
	game(Mode, FinalBoard, Enemy, Difficulty).

% Check if it's the end turn in pvp mode
endTurn(Piece, FinalBoard, Player, Mode, Difficulty):-
	getOtherPlayer(Player, Enemy),
	if1(isPlayerNode(Piece, Player), game(Mode, FinalBoard, Enemy, Difficulty), game(Mode, FinalBoard, Player, Difficulty)).

% Switches the current player/bot
getOtherPlayer(Player, red):-
	Player == blue.
getOtherPlayer(Player, blue):-
	Player == red.

/*
*************************** MOVEMENT ***************************
*/

%Predicate that is responsible for the player movement
move(Board, Player, Piece, FinalBoard):-
	%selects the unit to move and checks if it's possible to move it.
	selectSource(Row, Column, Piece, Board, Player),

	%selects the coordinates of where to move it
	selectDestiny(DestRow, DestColumn, Row, Column, Board),

	movePiece(Piece, Row, Column, DestRow, DestColumn, Board, FinalBoard),
	write('Press enter to continue'), nl,
	waitForKey.

%Repeats until one source is valid
selectSource(Row, Column, Piece, Board, Player):-
	repeat,
	clearConsole,
	write('***********************'), nl,
	write(Player), write(' turn!'), nl,
	write('***********************'), nl,
	displayBoard(Board),
	%Gets the coordinates given by the user and validates the piece status
	nl, write('Select a  unit to move it or a node to end turn.'), nl,
	getCoords(Row, Column),
	validateSource(Row, Column, Piece, Board, Player), !.

%Verifies if the source is valid
validateSource(Row, Column, Piece, Board, Player):-
	%Verifies if the coordinates are from a cell outside the board boundaries
	checkIfOutsideBoard(Row, Column),
	%Gets the piece that the user selected.
	getMatrixElement(Board, Row, Column, Piece), !,
	%evaluate if is a Piece and belongs to Player
	if1(
			not(isEmpty(Piece)),
			true,
			outputMessage('The unit to move must belong to your team!')
	), !,
	belongsTo(Piece, Player),
	%evaluate if has communication from a node
	getNodeCoordinates(Board, blue, BlueNodeRow-BlueNodeCol), %gets the node's coords
	getNodeCoordinates(Board, red, RedNodeRow-RedNodeCol), %gets the node's coords
	if1(
			checkCommunicationLines(Row, Column, BlueNodeRow, BlueNodeCol, RedNodeRow, RedNodeCol, Board),
			true,
			outputMessage('A unit can only move if its receiving signal from a node.')
	).

%Repeats until one destiny is valid
selectDestiny(DestRow, DestColumn, SrcRow, SrcCol, Board):-
	repeat,
	%Gets the destiny coordinates given by the user in order to check the condition of that cell
	validateDestiny(DestRow, DestColumn, SrcRow, SrcCol, Board), !.

%Verifies if the destiny is valid
validateDestiny(DestRow, DestColumn, SrcRow, SrcCol, Board):-
	nl, write('Write the coordinates of where to move it.'), nl,
	getCoords(DestRow, DestColumn),
	%Verifies if the coordinates are from a cell outside the board boundaries
	if1(
			checkIfOutsideBoard(DestRow, DestColumn),
			true,
			outputMessage('Invalid piece coordinates!')
	),
	%Gets the piece present at the given coordinates
	getMatrixElement(Board, DestRow, DestColumn, Piece), !,
	if1(
			isEmpty(Piece),
			true,
			outputMessage('A piece can be only moved to empty spaces!')
	), !,
	%check if movement itself is valid. First checks if it is a jump then a normal move.
	if1(
			isJump(SrcRow, SrcCol, DestRow, DestColumn, Board),
			true,
			if1(
					validateMovement(SrcRow,SrcCol,DestRow,DestColumn),
					true,
					outputMessage('A unit can be only moved to adjacent cells')
			)
	).

%Receives destiny and source coordinates and updates that piece coords in the board
movePiece(Piece, Row, Column, DestRow, DestCol, Board, NewBoard):-
	or(Piece, blueUnit, redUnit),
	moveUnit(Piece, Column, Row, DestCol, DestRow, Board, NewBoard).

movePiece(Piece, Row, Column, DestRow, DestCol, Board, NewBoard):-
	or(Piece, blueNode, redNode),
	moveUnit(Piece, Column, Row, DestCol, DestRow, Board, NewBoard).

moveUnit(Piece,FromCol, FromRow, ToCol, ToRow, Board, NewBoard):-
	replace(FromCol, FromRow, space, Board, IntermediateBoard),
	replace(ToCol, ToRow, Piece, IntermediateBoard, NewBoard).

%checks if the respective movement is a jump - between friendly or enemy unit.
isJump(SrcRow, SrcCol, DestRow, DestCol, Board):-
	abs(SrcRow-DestRow,RowDiff),
	abs(SrcCol-DestCol,ColDiff),
	xor(RowDiff == 2, ColDiff == 2),
	TotalDiff is RowDiff+ColDiff,
	TotalDiff==2,
	getMatrixElement(Board, SrcRow, SrcCol, SrcPiece),
	not(isNode(SrcPiece)),
	getIntermediatePiece(RowDiff, ColDiff, DestRow, DestCol, SrcCol, SrcRow, Board, Piece), !,
	not(isNode(Piece)),
	not(isEmpty(Piece)).

%isJump auxiliar predicate. Gets the piece between the cells that are in the same column
getIntermediatePiece(_, 0, DestRow, _, SrcCol, SrcRow, Board, Piece):-
	Diff is round((SrcRow-DestRow) / 2),
	PieceRow is DestRow + Diff,
	getMatrixElement(Board, PieceRow, SrcCol, Piece).

%Gets the piece between the cells that are in the same row
getIntermediatePiece(0, _, _, DestCol, SrcCol, SrcRow, Board, Piece):-
	Diff is round((SrcCol-DestCol) / 2),
	PieceCol is DestCol + Diff,
	getMatrixElement(Board, SrcRow, PieceCol, Piece).

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
	Row >= 0, Row < 9, Column >= 0, Column < 9.

%check if moves only one square horizontally or diagonally
validateMovement(OriginRow, OriginCol, DestRow, DestCol):-
	abs(OriginRow-DestRow,RowDiff),
	abs(OriginCol-DestCol,ColDiff),
	TotalDiff is RowDiff+ColDiff,
	TotalDiff==1.

/*
*************************** COMMUNICATION LINES ***************************
*/

%getNodeCoordinates(Board, PlayerColor, Coords). Receives a board and a color. Finds out the coordinates of the node with that color (starts at (0,0))
getNodeCoordinates(Board, PlayerColor, Coords):- %initializer
	getNodeCoordinates(Board, PlayerColor, 0, 0, Coords).

getNodeCoordinates([[Element|_]|_], Color, TempRow, TempCol, TempRow-TempCol):- %checks if found the wanted node.
	belongsTo(Element,Color),          	  %color is right
	or(Element, blueNode, redNode).       %is a node

getNodeCoordinates([[]|RestOfBoard], Color, TempRow, _, Coords):- %see next row, reseting column counter to 0
	NewRow is TempRow+1,                                   %increment row counter
	getNodeCoordinates(RestOfBoard,Color,NewRow,0,Coords). %check next row

getNodeCoordinates([[_|RestOfLine]|RestOfBoard], Color, TempRow, TempCol, Coords):- %see next column
	NewCol is TempCol+1,                                                      %increment column counter
	getNodeCoordinates([RestOfLine|RestOfBoard],Color,TempRow,NewCol,Coords). %check next column

%Verifies if the piece in the respective coordinates have communication with either nodes.
checkCommunicationLines(Row, Column, BlueNodeRow, BlueNodeCol, _, _, Board):-
	hasComLineBetween(Row,Column,BlueNodeRow,BlueNodeCol,blue,Board).

checkCommunicationLines(Row, Column, _, _, RedNodeRow, RedNodeCol, Board):-
	hasComLineBetween(Row,Column,RedNodeRow,RedNodeCol,red,Board).

checkCommunicationLines(Row, Column, NodeRow, NodeCol, Board, Player):-
        hasComLineBetween(Row, Column, NodeRow, NodeCol, Player, Board).

% hasComLineBetween(Row,Column,NodeRow,NodeColumn,Color,Board). Checks if there are lines of communication between the node and the piece.
hasComLineBetween(Row, Column, Row, Column, _, _). %the piece is the node itself!

hasComLineBetween(Row, Column, NodeRow, NodeCol, Color, Board):-
	isLined(Row, Column, NodeRow, NodeCol), %quickly checks if, in the best-case-scenario, there is a communication line
	setInc(Row, Column, NodeRow, NodeCol, IncRow, IncCol), %Inc is a vector with direction piece->node that will be added repeatedly to piece's coords
	Row2 is Row+IncRow, Col2 is Column+IncCol,!, %first increment
	comLineIsOK(Row2, Col2, NodeRow, NodeCol, IncRow, IncCol, Color, Board).%check if has the com line isn't intercepted by another player's piece

%comLineIsOK(Row,Col,NodeRow,NodeCol,IncRow,IncCol,Color,Board). recursively checks if the com line isn't intercepted by an enemy's piece.
%IncR and IncC are the vector with direction piece->node, which will be added until the piece "can reach" the node without interference.
comLineIsOK(Row,Col,Row,Col,_,_,_,_). %arrived at the node

comLineIsOK(Row,Col,NodeRow,NodeCol,IncR,IncC,Color,Board):- %piece is "moving" in the node direction
	getMatrixElement(Board,Row,Col,Piece),
	or( 							%check for intersections
		isEmpty(Piece),
		belongsTo(Piece,Color)
	),
	NewRow is Row+IncR,
	NewCol is Col+IncC,
	comLineIsOK(NewRow,NewCol,NodeRow,NodeCol,IncR,IncC,Color,Board). %recursive call

%checks if there could be a common road or conduit between these 2 coords
isLined(Row1,Col1,Row2,Col2):-
	or(
		or(Col1==Col2,Row1==Row2), %check if they have roads uniting them
		(abs(Row1-Row2,RowDiff),abs(Col1-Col2,ColDiff),RowDiff==ColDiff) %check if they have conduits uniting them
	).

%setInc(Row,Column,NodeRow,NodeCol,IncRow,IncCol). IncRow and IncCol will be the vector with direction piece->node.
setInc(Row,Column,NodeRow,Column,IncRow,IncCol):- %vert comLine
	IncCol is 0,
	MidRow is (NodeRow-Row), abs(MidRow,MidRowAbs), %intermediate calc
	MidRow \= 0,
	IncRow is integer(MidRow/MidRowAbs).
setInc(Row,Column,Row,NodeCol,IncRow,IncCol):- %horiz comLine
	IncRow is 0,
	MidCol is (NodeCol-Column), abs(MidCol,MidColAbs), %intermediate calc
	MidCol \= 0,
	IncCol is integer(MidCol/MidColAbs).
setInc(Row,Column,NodeRow,NodeCol,IncRow,IncCol):- %diagonal line
	MidCol is (NodeCol-Column), abs(MidCol,MidColAbs),
	MidRow is (NodeRow-Row), abs(MidRow,MidRowAbs),
	MidCol \= 0,
	MidRow \= 0,
	IncCol is integer(MidCol/MidColAbs),
	IncRow is integer(MidRow/MidRowAbs).
