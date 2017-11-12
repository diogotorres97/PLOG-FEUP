/**************************************************************************
                          CPU movement handling
***************************************************************************/

%Level 1 - cpu plays randomly from available moves, does not do multiple jumps
cpuMove(Board, PlayerNumber, FinalBoard, easy) :-
    findMove(Board, easy, Moves, Index),
    cpuDoMove(Board, Moves, Index, PreviousX, PreviousY, PlayerNumber, NewBoard),
    doCPUMultipleJump(NewBoard, easy-multiple, PreviousX, PreviousY, PlayerNumber, FinalBoard).

%Level 2 - greedy approach, cpu plays the best move available but does not predict future board states, does multiple jumps
cpuMove(Board, PlayerNumber, FinalBoard, hard) :-
    findMove(Board, hard, Moves, Index),
    cpuDoMove(Board, Moves, Index, PreviousX, PreviousY, PlayerNumber, NewBoard),
    doCPUMultipleJump(NewBoard, hard-multiple, PreviousX, PreviousY, PlayerNumber, FinalBoard).

cpuMove(Board, PlayerNumber, FinalBoard, harder) :-
    findMove(Board, harder, Moves, Index),
    write('Selection: '), write(Moves), write(' | '), write(Index), nl.

findBestMove(Board, Depth, Moves, Index) :-
    validMoves(Board, [], AvailMoves),
    length(AvailMoves, Length),
    Index is Length - 1,
    depthLoop(Board, 1, Index, 0, [], FinalPoints),
    write('points '), write(FinalPoints).

depthLoop(Board, 0, CurIndex, CurPoints, PointsBuild, FinalPointsList) :-
    
    validMoves(Board, [], AvailMoves),
    
    getMatrixElement(AvailMoves, CurIndex, 0, Points),
    
    getBoardElement(AvailMoves, CurIndex, 1, Xi - Yi), !,
    getBoardElement(AvailMoves, CurIndex, 2, Xf - Yf), !,
    
    simulateMove(Xi, Yi, Xf, Yf, Board, NewBoard),
    
    validMoves(NewBoard, [], AvailMoves2),
    length(AvailMoves2, Length),
    BreadthIndex is Length - 1,
    
    breadthLoop(NewBoard, 0, BreadthIndex, CurPoints, [], PointsList),
    NewIndex is CurIndex - 1,
    NewPoints is CurPoints - Points,
    append(PointsBuild, [PointsList], NewPointsBuild),
    depthLoop(Board, 1, NewIndex, NewPoints, NewPointsBuild, FinalPointsList).

depthLoop(_, _, -1, _, PointsBuild, FinalPointsList) :- FinalPointsList = PointsBuild.

depthLoop(Board, Depth, CurIndex, CurPoints, PointsBuild, FinalPointsList) :-
    
    write('Depth: '), write(Depth), write(' Index: '), write(CurIndex), nl,
    
    validMoves(Board, [], AvailMoves),
    
    getMatrixElement(AvailMoves, CurIndex, 0, Points),
    
    getBoardElement(AvailMoves, CurIndex, 1, Xi - Yi), !,
    getBoardElement(AvailMoves, CurIndex, 2, Xf - Yf), !,
    
    simulateMove(Xi, Yi, Xf, Yf, Board, NewBoard),
    
    validMoves(NewBoard, [], AvailMoves2),
    length(AvailMoves2, Length),
    
    NewPoints is CurPoints + Points,
    
    ite(Length == 0,
        (NewIndex is CurIndex - 1,
         append(PointsBuild, [NewPoints], NewPointsBuild),
         depthLoop(Board, Depth, NewIndex, CurPoints, NewPointsBuild, FinalPointsList)
        ),
        (NewDepth is Depth - 1,
         depthLoop(Board, NewDepth, CurIndex, NewPoints, PointsBuild, FinalPointsList)
        )
       ).

breadthLoop(_, _, -1, _, PointsBuild, PointsList) :- PointsList = PointsBuild.

breadthLoop(Board, Depth, CurIndex, CurPoints, PointsBuild, PointsList) :-

    validMoves(Board, [], AvailMoves),
    
    write('Depth: '), write(Depth), write(' Index: '), write(CurIndex), nl,
    
    getMatrixElement(AvailMoves, CurIndex, 0, Points),
    
    NewPoints is Points + CurPoints,
    
    append(PointsBuild, [NewPoints], NewPointsBuild),
    
    getBoardElement(AvailMoves, CurIndex, 1, Xi - Yi), !,
    getBoardElement(AvailMoves, CurIndex, 2, Xf - Yf), !,
    
    simulateMove(Xi, Yi, Xf, Yf, Board, _),
    
    NewIndex is CurIndex - 1,
    
    breadthLoop(Board, Depth, NewIndex, CurPoints, NewPointsBuild, PointsList).

simulateMove(FromRow, FromCol, ToRow, ToCol, Board, NewBoard) :-

        getBoardElement(Board, FromRow, FromCol, Frog),
        replace(Board, FromRow, FromCol, 0, InterBoard),

        delta(ToRow, FromRow, X),
        IRow is FromRow + X,
        delta(ToCol, FromCol, Y),
        IColumn is FromCol + Y,
        getBoardElement(Board, IRow, IColumn, _),

        RowI is FromRow + 1, RowF is ToRow + 1,
        convertColumn(FromCol, ColumnI), convertColumn(ToCol, ColumnF),

        write('Simulating ['), write(RowI), write(', '), write(ColumnI), write('] '),
        write('to ['), write(RowF), write(', '), write(ColumnF), write(']'), nl,
        
        replace(InterBoard, IRow, IColumn, 0, InterBoard2),
        replace(InterBoard2, ToRow, ToCol, Frog, NewBoard).
    
%Receives move and changes game state based on it, waits for user input to confirm move
cpuDoMove(Board, Moves, Index, Xf, Yf, PlayerNumber, FinalBoard) :-
    getBoardElement(Moves, Index, 1, Xi - Yi), !,
    getBoardElement(Moves, Index, 2, Xf - Yf), !,

    writeCPUMove(Xi, Yi, Xf, Yf),

    moveFrog(Xi, Yi, Xf, Yf, Board, FinalBoard, PlayerNumber),
    write('Press enter to continue'), nl,
    waitForKey, clearConsole.

%Handles multiple jump scenario for CPU player
doCPUMultipleJump(Board, Diff, InitRow, InitColumn, PlayerNumber, FinalBoard) :-
    checkValidMoves(Board, [], InitRow, InitColumn, AvailMoves),
    length(AvailMoves, NumMoves),
    NumMoves > 0,
    displayBoard(Board),

    findMove(Board, Diff, InitRow, InitColumn, Moves, Index),
    length(Moves, Length),
    Length > 0, !,
    write('CPU Multiple jump!'), nl,

    cpuDoMove(Board, Moves, Index, PreviousX, PreviousY, PlayerNumber, NewBoard),

    doCPUMultipleJump(NewBoard, Diff, PreviousX, PreviousY, PlayerNumber, FinalBoard).

doCPUMultipleJump(Board, _, _, _, _, FinalBoard) :- FinalBoard = Board.

%Writes CPU jump in user friendly way
writeCPUMove(Xi, Yi, Xf, Yf) :-
    RowI is Xi + 1, RowF is Xf + 1,
    convertColumn(Yi, ColumnI), convertColumn(Yf, ColumnF),

    write('CPU Moves from ['), write(RowI), write(', '), write(ColumnI), write('] '),
    write('to ['), write(RowF), write(', '), write(ColumnF), write(']'), nl.

%Writes CPU first move in user friendly way
writeCPUMove(Xi, Yi) :-
    RowI is Xi + 1,
    convertColumn(Yi, ColumnI),
    write('CPU removes frog at ['), write(RowI), write(', '), write(ColumnI), write('] '), nl.

%Chooses a move for the CPU according to difficulty / type of move
%Easy - chooses move randomly
findMove(Board, easy, Moves, Index) :-
    validMoves(Board, [], Moves),
    length(Moves, Length),
    random(0, Length, Index).

%Hard - chooses best move from current board, no prediction of future board states
findMove(Board, hard, Moves, Index) :-
    validMoves(Board, [], AvailMoves),
    length(AvailMoves, Length),
    sort(AvailMoves, Moves),
    Index is Length - 1.

findMove(Board, harder, Moves, Index) :-
    findBestMove(Board, Depth, Moves, Index).

%Easy multiple jump - chooses randomly whether to do multiple jump
findMove(Board, easy-multiple, Xi, Yi, Moves, Index) :-
    random(0, 2, Random),
    Random > 0,
    checkValidMoves(Board, [], Xi, Yi, Moves),
    length(Moves, Length),
    random(0, Length, Index).

findMove(_, easy-multiple, _, _, [], 0).

%Hard multiple jump - chooses best move from current initial position, which is the position from last move
findMove(Board, hard-multiple, Xi, Yi, Moves, Index) :-
    checkValidMoves(Board, [], Xi, Yi, AvailMoves),
    length(AvailMoves, Length),
    sort(AvailMoves, Moves),
    Index is Length - 1.

%CPU selects a random green frog to remove
cpuFirstMove(Board, FinalBoard) :-
    repeat,
    once(random(0, 12, Row)),
    once(random(0, 12, Column)),
    once(getBoardElement(Board, Row, Column, Cell)),
    Cell == 1,

    writeCPUMove(Row, Column),

    replace(Board, Row, Column, 0, FinalBoard),
    write('Press enter to continue'), nl,
    waitForKey, clearConsole.
