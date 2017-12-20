:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- dynamic cardinalityList/1.

doppel(N, Columns) :-

        retractall(cardinalityList(_)),

        getInitialBoard(N, Rows),

        defineDomain(N, Rows),

        %first restrition
        defineCardinality(N, Rows),

        %second restrition
        eachRowBlacken(Rows),
        transpose(Rows, Columns),
        eachRowBlacken(Columns),

	maplist(sumInLine, Rows),
	maplist(sumInLine, Columns),

        reset_timer, !,
        maplist(labeling([bisect, down]), Columns), print_time,
        maplist(writeBoard, Columns).


writeBoard(Line) :-
        write(Line), nl.

% Create a bidimensional matrix NxN
getInitialBoard(N, Board):-
        length(Board, N),
        maplist(same_length(Board), Board).

defineDomain(N, Rows):-
        MaxN #= N-2,
        maplist(define_domain(MaxN), Rows).

define_domain(MaxN,X):- domain(X, 0, MaxN).

defineCardinality(N, Rows) :-
        createCardinality(N,1,[0-2], Lista),
        assert(cardinalityList(Lista)),
        maplist(define_Cardinality, Rows),
        transpose(Rows, Columns),
        maplist(define_Cardinality, Columns).

define_Cardinality(X) :-
        cardinalityList(Lista),
        global_cardinality(X,Lista).

createCardinality(Length,Counter, Lista,Lista):- Counter is Length - 1.
createCardinality(Length,Counter,Temp, Lista) :-
        append([Counter-1], Temp, Temp2),
        Counter =< Length - 2,
        Tmp is Counter+1,
        createCardinality(Length,Tmp, Temp2,Lista).

eachRowBlacken(Rows):-
        maplist(countEachRowBlacken, Rows).

countEachRowBlacken(X):-
        count(0,X,#=,2).


getArc(0,Temp,Temp).

getArc(N,Temp, List) :-
        N > 0,
        append(Temp, [arc(q0,N,q0), arc(q1,N,q2),arc(q2,N,q2),arc(q3,N,q3)], NewList),
        NewN is N-1,
        getArc(NewN, NewList, List).

sumInLine(Line):-
        length(Line,N), NewN #= N-2,
        getArc(NewN, [arc(q0,0,q1), arc(q2,0,q3)], List),
        automaton(Line, [source(q0), sink(q3)], List).

reset_timer :- statistics(walltime,_).
print_time :-
        statistics(walltime,[_,T]),
        TS is ((T//10)*10)/1000,
        nl, write('Time: '), write(TS), write('s'), nl, nl.
