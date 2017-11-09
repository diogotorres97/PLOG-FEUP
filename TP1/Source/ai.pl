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

  getMatrixElement(NewMoves, Value, 1, Xi - Yi), !,
  getMatrixElement(NewMoves, Value, 2, Xf - Yf), !,

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

  getMatrixElement(OrderedMoves, Pos, 1, Xi - Yi), !,
  getMatrixElement(OrderedMoves, Pos, 2, Xf - Yf), !,

  RowI is Xi + 1, RowF is Xf + 1,
  convertColumn(Yi, ColumnI), convertColumn(Yf, ColumnF),

  write('CPU Moves from ['), write(RowI), write(', '), write(ColumnI), write('] '),
  write('to ['), write(RowF), write(', '), write(ColumnF), write(']'), nl,

  moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, PlayerNumber),

  write('Press enter to continue'), nl,
  waitForKey, clearConsole.

%CPU selects a random green frog to remove
cpuFirstMove(Board, FinalBoard) :-
  
  repeat,
  once(random(0, 12, Row)),
  once(random(0, 12, Column)),
  once(getMatrixElement(Board, Row, Column, Cell)),
  Cell == 1,
  
  RowI is Row + 1,
  convertColumn(Column, ColumnI),
  write('CPU removes frog at ['), write(RowI), write(', '), write(ColumnI), write('] '), nl,
  
  replace(Board, Row, Column, 0, FinalBoard).