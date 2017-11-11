/**************************************************************************
                           CPU movement handling
***************************************************************************/

%Selects a move according to the difficulty
cpuMove(Board, PlayerNumber, FinalBoard, easy) :- pickRandomMove(Board, PlayerNumber, FinalBoard).

cpuMove(Board, PlayerNumber, FinalBoard, hard) :- pickBestMove(Board, PlayerNumber, FinalBoard).

%Level 1 - cpu plays randomly from available moves
pickRandomMove(Board, PlayerNumber, FinalBoard) :-

  validMoves(Board, [], NewMoves),

  length(NewMoves, Length),
  random(0, Length, Value),

  getBoardElement(NewMoves, Value, 1, Xi - Yi), !,
  getBoardElement(NewMoves, Value, 2, Xf - Yf), !,

  RowI is Xi + 1, RowF is Xf + 1,
  convertColumn(Yi, ColumnI), convertColumn(Yf, ColumnF),

  write('CPU Moves from ['), write(RowI), write(', '), write(ColumnI), write('] '),
  write('to ['), write(RowF), write(', '), write(ColumnF), write(']'), nl,

  moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, PlayerNumber),

  write('Press enter to continue'), nl,
  waitForKey, clearConsole.

%Level 2 - greedy approach, cpu plays the best move available but does not predict future board states
pickBestMove(Board, PlayerNumber, FinalBoard) :-

  validMoves(Board, [], NewMoves),

  length(NewMoves, Length),
  sort(NewMoves, OrderedMoves),
  Pos is Length - 1,

  getBoardElement(OrderedMoves, Pos, 1, Xi - Yi), !,
  getBoardElement(OrderedMoves, Pos, 2, Xf - Yf), !,

  RowI is Xi + 1, RowF is Xf + 1,
  convertColumn(Yi, ColumnI), convertColumn(Yf, ColumnF),

  write('CPU Moves from ['), write(RowI), write(', '), write(ColumnI), write('] '),
  write('to ['), write(RowF), write(', '), write(ColumnF), write(']'), nl,

  moveFrog(Yi, Xi, Yf, Xf, Board, NewBoard, PlayerNumber),

  write('Press enter to continue'), nl,
  waitForKey, clearConsole,
  doCPUMultipleJump(NewBoard, Xf, Yf, PlayerNumber, FinalBoard). 

%Handles multiple jump scenario for CPU player
doCPUMultipleJump(Board, InitRow, InitColumn, PlayerNumber, FinalBoard) :-
        checkValidMoves(Board, [], InitRow, InitColumn, AvailMoves),
        length(AvailMoves, NumMoves),
        NumMoves > 0, !,
        displayBoard(Board),
        
        write('CPU Multiple jump!'), nl,
        
        sort(AvailMoves, OrderedMoves),
        Pos is NumMoves - 1,
        
        getBoardElement(OrderedMoves, Pos, 1, Xi - Yi), !,
        getBoardElement(OrderedMoves, Pos, 2, Xf - Yf), !,
        
        RowI is Xi + 1, RowF is Xf + 1,
        convertColumn(Yi, ColumnI), convertColumn(Yf, ColumnF),
        
        write('CPU Moves from ['), write(RowI), write(', '), write(ColumnI), write('] '),
        write('to ['), write(RowF), write(', '), write(ColumnF), write(']'), nl,
        
        moveFrog(Yi, Xi, Yf, Xf, Board, NewBoard, PlayerNumber),
        
        write('Press enter to continue'), nl,
        waitForKey, clearConsole,
  
        doCPUMultipleJump(NewBoard, Xf, Yf, PlayerNumber, FinalBoard).
        
doCPUMultipleJump(Board, _, _, _, FinalBoard) :- FinalBoard = Board.

%CPU selects a random green frog to remove
cpuFirstMove(Board, FinalBoard) :-
  
  repeat,
  once(random(0, 12, Row)),
  once(random(0, 12, Column)),
  once(getBoardElement(Board, Row, Column, Cell)),
  Cell == 1,
  
  RowI is Row + 1,
  convertColumn(Column, ColumnI),
  write('CPU removes frog at ['), write(RowI), write(', '), write(ColumnI), write('] '), nl,
  
  replace(Board, Row, Column, 0, FinalBoard).