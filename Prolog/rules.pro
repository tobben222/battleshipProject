
/* Defining parts*/
    shipInput('A'). /*Top of ship*/
    shipInput('V'). /*Bottom of ship*/
    shipInput('<'). /*Left of ship*/
    shipInput('>'). /*Right of ship*/
    shipInput('+'). /*Middle of ship*/
    shipInput('*'). /*1x1 ship*/
    shipInput('S'). /*unknown ship part*/

    vaterInput('-'). /*vater*/
    unknownInput('?'). /*unknown*/

/*Defining leagal vertical combinations
leagalVertical(Left Most, Right Most)
*/
    leagalVertical('A','+'). /*horizontal ships*/
    leagalVertical('A','V').
    leagalVertical('-','A').
    leagalVertical('+','+').
    leagalVertical('+','V').
    leagalVertical('V','-').

    leagalVertical('*','-'). /*single ships*/
    leagalVertical('-','*').

    leagalVertical('<','-'). /*vertical ships*/
    leagalVertical('-','<').
    leagalVertical('>','-').
    leagalVertical('-','>').
    leagalVertical('+','-').
    leagalVertical('-','+').

/*Defining leagal vertical combinations
legalHorizontal(Higest, Lowest)
*/
    legalHorizontal('<','+'). /*vertical ships*/
    legalHorizontal('<','>').
    legalHorizontal('-','<').
    legalHorizontal('+','+').
    legalHorizontal('+','>').
    legalHorizontal('>','-').

    legalHorizontal('*','-'). /*single ships*/
    legalHorizontal('-','*').

    legalHorizontal('A','-'). /*horizontal ships*/
    legalHorizontal('-','A').
    legalHorizontal('+','-').
    legalHorizontal('-','+').
    legalHorizontal('V','-').
    legalHorizontal('-','V').

/*
Define leagal diagonal combinations
leagalDiagonal(Pos1,pos2)
(ship and vater) or (vater and ship) or (vater and vater)
*/
    leagalDiagonal(P1,P2):- 
        (shipInput(P1), vaterInput(P2));
        (shipInput(P2), vaterInput(P1));
        (vaterInput(P1),vaterInput(P2)).


/*place new row of water on all sides of puzzle so tests can check borders*/

    /*add rows*/

    addRow([[_] | _], ['-']).
    addRow([[_ | T] | _], ['-' | Temp]):- addRow([T],Temp).

    /*add coloms*/
    addColoms([[H|T]],[Return]) :- 
        append([H|T],['-'], Temp),
        append(['-'], Temp,Return).

    addColoms([[H|T]|T2],[Return| T3]):-
        append([H|T], ['-'], Temp),
        append(['-'], Temp,Return),
        addColoms(T2,T3).

/*check if the nuber in vertical matches Row*/

/*checking single row*/
checkRow([], 0).
checkRow([H|T], RowNumber):- not(H == '-'), RowNumber1 is RowNumber -1, checkRow(T, RowNumber1).
checkRow(['-'| T], RowNumber):- checkRow(T,RowNumber).

/*Checking all rows*/
checkRows([[H|T]],[RowNumber]):- checkRow([H,T], RowNumber).
checkRows([[H|T]| T2], [RowNumber| RestRowNumbers]):- 
    checkRow([H|T], RowNumber),
    checkRows(T2, RestRowNumbers).



/*check if the nuber in horizontal matches colums*/

/*checking single column*/
checkColumn([[H]], [], 1):- not(H == '-').
checkColumn([[H] | T], [] , ColumnNumber):-
    ColumnNumber > 0,
    not(H == '-'),
    ColumnNumber1 is ColumnNumber -1,
    checkColumn(T,[],ColumnNumber1).

checkColumn([[H|T]], [T], 1) :- not(H == '-').
checkColumn([[H|T] | T2], [T | New],ColumnNumber):- 
    ColumnNumber > 0,
    not(H == '-'),
    ColumnNumber1 is ColumnNumber -1,
    checkColumn(T2,New,ColumnNumber1).

checkColumn([['-']], [], 0).
checkColumn([['-'], T], [], ColumnNumber):- checkColumn(T , [], ColumnNumber).
checkColumn([['-'|T]], [T], 0).
checkColumn([['-'|T] | T2], [T, New], ColumnNumber) :- checkColumn(T2,New,ColumnNumber).

/*Checking all Colomns*/
checkColumns([[H | T], [ColumnNumber]]):- checkColumn([[H],T], [], ColumnNumber).

checkColumns([[H|T]|T2], [ColumnNumber | RestColumnNumber]):-
    checkColumn([[H| T], T2], NewPuzzle, ColumnNumber),
    checkColumns(NewPuzzle, RestColumnNumber).
