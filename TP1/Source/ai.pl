/*
*************************** CALLS FOR BOT MOVES ***************************
*/

%Selects a move accordingly to the difficulty
pickMove(Board, Bot, FinalBoard, Difficulty):-
  Difficulty == easy,!,
  displayBoard(Board),
  pickRandomMove(Board, Bot,FinalBoard).

pickMove(Board,Bot, FinalBoard,Difficulty):-
  displayBoard(Board),
  pickBestMove(Board,Bot,FinalBoard).

%bot will randomly make a move
pickRandomMove(Board, Bot,FinalBoard):-
    validMoves(Board,[], NewMoves),

    length(NewMoves, Length),
    random(0, Length, Value),

    getMatrixElement(NewMoves, Value, 1, Xi-Yi),!,
    write(Xi),write(' '),write(Yi),nl,
    getMatrixElement(NewMoves, Value, 2, Xf-Yf),!,
    write(Xf),write(' '),write(Yf),nl,

    moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, Bot),
    %displayBoard(FinalBoard),
    write('Press enter to continue'), nl,
    waitForKey, clearConsole.

pickBestMove(Board,Bot, FinalBoard):-
  validMoves(Board,[], NewMoves),

  length(NewMoves, Length),
  sort(NewMoves, OrderedMoves),
  Pos is Length-1,

  getMatrixElement(OrderedMoves, Pos, 1, Xi-Yi),!,
  write(Xi),write(' '),write(Yi),nl,
  getMatrixElement(OrderedMoves, Pos, 2, Xf-Yf),!,
  write(Xf),write(' '),write(Yf),nl,

  moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, Bot),
  %displayBoard(FinalBoard),
  write('Press enter to continue'), nl,
  waitForKey, clearConsole.
