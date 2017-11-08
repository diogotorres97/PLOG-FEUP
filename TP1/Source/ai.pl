/**************************************************************************
                       CPU movement handling
***************************************************************************/

%Selects a move according to the difficulty
cpuMove(Board, PlayerNumber, FinalBoard, Difficulty):-
  ite(Difficulty == easy, pickRandomMove(Board, PlayerNumber, FinalBoard), pickBestMove(Board, PlayerNumber, FinalBoard)).

%Level 1 - cpu plays randomly from available moves
pickRandomMove(Board, PlayerNumber, FinalBoard) :-
  
    validMoves(Board, [], NewMoves),

    length(NewMoves, Length),
    random(0, Length, Value),

    getMatrixElement(NewMoves, Value, 1, Xi-Yi), !,
    write(Xi), write(' '), write(Yi), nl,
    getMatrixElement(NewMoves, Value, 2, Xf-Yf), !,
    write(Xf), write(' '), write(Yf), nl,

    moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, PlayerNumber),

    write('Press enter to continue'), nl,
    waitForKey, clearConsole.

%Level 2 - greedy approach, cpu plays the best move available but does not predict future board states
pickBestMove(Board, PlayerNumber, FinalBoard) :-
  
  validMoves(Board, [], NewMoves),

  length(NewMoves, Length),
  sort(NewMoves, OrderedMoves),
  Pos is Length - 1,

  getMatrixElement(OrderedMoves, Pos, 1, Xi-Yi), !,
  write(Xi), write(' '), write(Yi), nl,
  getMatrixElement(OrderedMoves, Pos, 2, Xf-Yf), !,
  write(Xf), write(' '), write(Yf), nl,

  moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, PlayerNumber),

  write('Press enter to continue'), nl,
  waitForKey, clearConsole.