%*********************       Logic operators         *********************%

% Not operator
not(X):- X, !, fail.
not(_).

% Or operator -> Checks if 1st arg is equal to 2nd or 3rd
or(X,X,_).
or(X,_,X).
or(A,B) :- A;B.

% And operator
and(A,B) :- A,B.

% Nand operator
nand(A,B) :- not(and(A,B)).

% Xor operator
xor(A,B) :- or(A,B), nand(A,B).

% Abs operator
abs(X,Y):- X<0, Y is 0-X.
abs(X,Y):- X>=0, Y is X.

% If operator
ite(Condition,Then,_):-Condition,!,Then.
ite(_,_,Else):-Else.

%*********************       Input/Output functions         *********************%

getChar(X):-    get_char(X),
                                get_char(_).

getCode(X):-    get_code(X),
                                get_code(_).

waitForKey:-
                                get_char(_).

clearConsole:- write('\33\[2J').

% Outputs to the screen the error message passed as argument and then waits for a key, failing afterwards.
outputMessage(Message):-
                                write(Message), nl,
                                write('Press enter to continue.'),
                                waitForKey, !,
                                fail.


%Asks the user to select a piece
getCoords(Row, Column):-
    	                        write('Row coordinate: '),
    	                        getCode(RowInput),
    	                        write('Column coordinate: '),
    	                        getCode(ColInput),
    	                        Row is RowInput - 49,
    	                        Column is ColInput - 97.

%*********************       List manipulation         *********************%

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
