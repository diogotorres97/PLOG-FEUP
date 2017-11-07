/*
*************************** CALLS FOR BOT MOVES ***************************
*/

%Selects a move accordingly to the difficulty
pickMove(Board, Bot, FinalBoard):-
  displayBoard(Board),
  pickRandomMove(Board, Bot,FinalBoard).

%bot will randomly make a move
pickRandomMove(Board, Bot,FinalBoard):-
    validMoves(Board,[], NewMoves),

    %write(NewMoves),nl,
    length(NewMoves, Length),
    %write(Length),nl,
    random(0, Length, Value),
    %write('My TURN'),nl,
    %write(Value),nl,

    getMatrixElement(NewMoves, Value, 0, Xi-Yi),!,
    write(Xi),write(' '),write(Yi),nl,
    getMatrixElement(NewMoves, Value, 1, Xf-Yf),!,
    write(Xf),write(' '),write(Yf),nl,

    moveFrog(Yi, Xi, Yf, Xf, Board, FinalBoard, Bot),
    %displayBoard(FinalBoard),
    write('Press enter to continue'), nl,
    waitForKey, clearConsole.
