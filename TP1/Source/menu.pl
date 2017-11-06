
mainMenu:-      clearConsole,
                showMainMenu,
                write('Choose the desired option'), nl,
                getChar(X),
                (X == '1' -> playMenu;
                X == '2' -> instructionsMenu;
                X == '3';

                nl, write('Invalid Input. Press a key to continue and try again!'), nl,
                waitForKey,
                mainMenu).

playMenu:-      clearConsole,
                showPlayMenu,
                write('Choose the desired option'), nl,
                getChar(X),
                (X == '1' ->    clearConsole,
                                nl, write('Mode: Player versus Player'), nl, nl,
                                write('Press enter to continue.'), nl,
                                waitForKey,
                                nl, initialBoard(Board),
                                game(pvp, Board, blue,_), !, waitForKey,
                                mainMenu;
                X == '2' ->     pvcDifficultyMenu;
                X == '3' ->     cvcDifficultyMenu;
                X == '4' -> mainMenu;

                nl, write('Invalid Input. Press a key to continue and try again!'), nl,
                waitForKey,
                playMenu).

pvcDifficultyMenu:-
                clearConsole,
                showDifficultyMenu,
                write('Choose the desired option'), nl,
                getChar(X),
                (X == '1' ->    clearConsole,
                                nl, write('Mode: Player versus Computer'), nl,
                                write('Easy mode'), nl, nl,
                                write('Press enter to continue.'), nl,
                                waitForKey,
                                nl, initialBoard(Board),
                                game(pvc, Board, blue, easy), !, waitForKey,
                                mainMenu;
                X == '2' ->     clearConsole,
                                nl, write('Mode: Player versus Computer'), nl,
                                write('Medium mode'), nl, nl,
                                write('Press enter to continue.'), nl,
                                waitForKey,
                                nl, initialBoard(Board), assert(lastMove([0-0, 0-0], 0)),
                                game(pvc, Board, blue, medium), !, waitForKey,
                                mainMenu;
                X == '3' -> playMenu;

                nl, write('Invalid Input. Press a key to continue and try again!'), nl,
                waitForKey,
                pvcDifficultyMenu).

cvcDifficultyMenu:-
                clearConsole,
                showDifficultyMenu,
                write('Choose the desired option'), nl,
                getChar(X),
                (X == '1' ->    clearConsole,
                                nl, write('Mode: Computer versus Computer'), nl,
                                write('Easy mode'), nl, nl,
                                write('Press enter to continue.'), nl,
                                waitForKey,
                                nl, initialBoard(Board),
                                game(cvc, Board, blue, easy), !, waitForKey,
                                mainMenu;
                X == '2' ->     clearConsole,
                                nl, write('Mode: Computer versus Computer'), nl,
                                write('Medium mode'), nl, nl,
                                write('Press enter to continue.'), nl,
                                waitForKey,
                                nl, initialBoard(Board), assert(lastMove([0-0, 0-0], 0)),
                                game(cvc, Board, blue, medium), !, waitForKey,
                                mainMenu;
                X == '3' -> playMenu;

                nl, write('Invalid Input. Press a key to continue and try again!'), nl,
                waitForKey,
                cvcDifficultyMenu).

instructionsMenu:-
        clearConsole,
        showInstructionsMenu1,
        nl, write('Press a key to continue'), nl,
        waitForKey,
        showInstructionsMenu2,
        nl, write('Press a key to continue'), nl,
        waitForKey,
        showInstructionsMenu3,
        nl, write('Press a key to continue'), nl,
        waitForKey,
        showInstructionsMenu4,
        nl, write('Press a key to continue'), nl,
        waitForKey,
        showInstructionsMenu5,
        nl, write('Press a key to return to the main menu'), nl,
        waitForKey,
        mainMenu.

showMainMenu:-  nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##              _  __        __                ##'), nl,
                write('##             / |/ /__  ___/ /__ ___          ##'), nl,
                write('##            /    / _ '\'/ _  / -_|_-<          ##'), nl,
                write('##           /_/|_/'\'___/'\'_,_/'\'__/___/          ##'), nl,
                write('##                                             ##'), nl,
                write('##              1 - Play                       ##'), nl,
                write('##              2 - Instructions               ##'), nl,
                write('##              3 - Quit                       ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showPlayMenu:-  nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##               Game modes                    ##'), nl,
                write('##                                             ##'), nl,
                write('##            1 - Player vs Player             ##'), nl,
                write('##            2 - Player vs PC                 ##'), nl,
                write('##            3 - PC vs PC                     ##'), nl,
                write('##            4 - Back                         ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showDifficultyMenu:-  nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##               Game difficulty               ##'), nl,
                write('##                                             ##'), nl,
                write('##                 1 - Easy                    ##'), nl,
                write('##                 2 - Medium                  ##'), nl,
                write('##                 3 - Back                    ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showInstructionsMenu1:-
                nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##             Nodes Instructions              ##'), nl,
                write('##                                             ##'), nl,
                write('##  1. Pecas                                   ##'), nl,
                write('##                                             ##'), nl,
                write('##  Existem 2 tipos de pecas:                  ##'), nl,
                write('##   -Unit: cada jogador tera\' 8 Units, cujo   ##'), nl,
                write('##      movimento e\' restringido apenas pela   ##'), nl,
                write('##      sua posicao relativamente ao seu Node. ##'), nl,
                write('##                                             ##'), nl,
                write('##   -Node: cada jogador tera\' apenas um Node, ##'), nl,
                write('##      o equivalente ao Rei no xadrez.        ##'), nl,
                write('##      Quando um jogador bloqueia o           ##'), nl,
                write('##      Node do adversario, ganha.             ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showInstructionsMenu2:-
                nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##             Nodes Instructions              ##'), nl,
                write('##                                             ##'), nl,
                write('##  2. Linhas de comunicacao                   ##'), nl,
                write('##                                             ##'), nl,
                write('##  As linhas de comunicacao sao a base do     ##'), nl,
                write('##  movimento de cada jogador. Cada Node       ##'), nl,
                write('##  transmite 8 linhas de comunicacao a partir ##'), nl,
                write('##  de si: 2 para as Roads horizontais         ##'), nl,
                write('##  adjacentes; 2 para as Roads verticais      ##'), nl,
                write('##  adjacentes; 4 para as Conduits diagonais   ##'), nl,
                write('##  adjacentes. Estas linhas de comunicacao    ##'), nl,
                write('##  estendem-se ate\' ao fim do tabuleiro,      ##'), nl,
                write('##  ou ate\' uma peca adversaria.               ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showInstructionsMenu3:-
                nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##             Nodes Instructions              ##'), nl,
                write('##                                             ##'), nl,
                write('##  3. O decorrer do jogo                      ##'), nl,
                write('##                                             ##'), nl,
                write('##  Neste jogo, os jogadores jogam por turnos. ##'), nl,
                write('##  O turno de cada jogador acontece da        ##'), nl,
                write('##  seguinte maneira:                          ##'), nl,
                write('##   -O jogador pode movimentar quantos Units  ##'), nl,
                write('##      pretender, dentro do que sao as        ##'), nl,
                write('##      regras aplicadas a esta peca.          ##'), nl,
                write('##                                             ##'), nl,
                write('##   -O jogador tem agora de movimentar        ##'), nl,
                write('##      exatamente uma vez o seu Node.         ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showInstructionsMenu4:-
                nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##             Nodes Instructions              ##'), nl,
                write('##                                             ##'), nl,
                write('##  4. Mover uma peca                          ##'), nl,
                write('##                                             ##'), nl,
                write('##  -Node: Move-se 1 espaco em qualquer uma    ##'), nl,
                write('##     das 4 direcoes (horizontal e vertical). ##'), nl,
                write('##                                             ##'), nl,
                write('##   -Unit: Move-se 1 espaco em qualquer uma   ##'), nl,
                write('##      das 4 direcoes, desde que se situe num ##'), nl,
                write('##      espaco onde incide uma linha de        ##'), nl,
                write('##      comunicacao. Pode tambem saltar por    ##'), nl,
                write('##      cima duma unit caso esteja na mesma    ##'), nl,
                write('##      road em conjunto com um node e a unit. ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.

showInstructionsMenu5:-
                nl,
                write('#################################################'), nl,
                write('#################################################'), nl,
                write('##                                             ##'), nl,
                write('##             Nodes Instructions              ##'), nl,
                write('##                                             ##'), nl,
                write('##  5. Fim do jogo                             ##'), nl,
                write('##                                             ##'), nl,
                write('##  O jogo acaba quando um jogador nao         ##'), nl,
                write('##  consegue movimentar o seu Node. Isto pode  ##'), nl,
                write('##  acontecer quando esta\' rodeado por units   ##'), nl,
                write('##  adversarias ou porque as units que o       ##'), nl,
                write('##  rodeiam estao tambem bloqueadas por units  ##'), nl,
                write('##  inimigos.                                  ##'), nl,
                write('##  Nesse sentido, cada jogador deve procurar  ##'), nl,
                write('##  proteger o seu Node desta condicao         ##'), nl,
                write('##  enquanto a provoca ao adversario.          ##'), nl,
                write('##                                             ##'), nl,
                write('#################################################'), nl,
                write('#################################################'), nl, nl.
