
/* Defining parts*/
    shipPart('A'). /*Top of ship*/
    shipPart('V'). /*Bottom of ship*/
    shipPart('<'). /*Left of ship*/
    shipPart('>'). /*Right of ship*/
    shipPart('+'). /*Middle of ship*/
    shipPart('*'). /*1x1 ship*/
    shipPart('S'). /*unknown ship part*/

    vaterPart('-'). /*vater*/
    unknownpart('?'). /*unknown*/

/*Defining leagal vertical combinations
leagalVertical(Left or Top Most, Right or Bottom Most)
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
        (shipPart(P1), vaterPart(P2));
        (shipPart(P2), vaterPart(P1));
        (vaterPart(P1),vaterPart(P2)).


/*place new row of water on all sides of puzzle so tests can check borders*/

    /*add rows*/

    addRow([[_] | _], ['-']).
    addRow([[_ | T] | _], ['-' | Retrun]):- addRow([T],Retrun).

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

checkColumns([[H|T]|T2], [ColumnNumber | RestColumnNumbers]):-
    checkColumn([[H| T], T2], NewPuzzle, ColumnNumber),
    checkColumns(NewPuzzle, RestColumnNumbers).


/*check leagal box*/

/*Single ship*/
checkBox([['-','-','-'], ['-','*','-'], ['-','-','-']]).

/*Horisontal*/
checkBox([['-','-','-'], [Left,'+',Right], ['-','-','-']]):-
    shipPart(Left),shipPart(Right),
    legalHorizontal(Left, '+'),
    legalHorizontal('+',Right).

checkBox([['-','-','-'], [Left,Middle,Right], ['-','-','-']]):-
    not(Middle == '+'), shipPart(Middle), (shipPart(Left); shipPart(Right)),
    legalHorizontal(Left,Middle),
    legalHorizontal(Middle,Right).

/*Verftical*/

checkBox([['-',Top,'-'], ['-','+','-'], ['-',Bottom,'-']]):-
    shipPart(Top), shipPart(Bottom),
    leagalVertical(Top, '+'),
    leagalVertical('+', Bottom).

checkBox([['-',Top,'-'], ['-',Middle,'-'], ['-',Bottom,'-']]):-
    not(Middle == '+'), shipPart(Middle), (shipPart(Top); shipPart(Bottom)),
    leagalVertical(Top,Middle), 
    leagalVertical(Middle,Bottom).

/*Diagonal*/
checkBox([[TopLeft,_, TopRight], [_, Middle, _], [BottomLeft,_,BottomRight]]):-
    leagalDiagonal(Middle,TopLeft),
    leagalDiagonal(Middle,TopRight),
    leagalDiagonal(Middle,BottomLeft),
    leagalDiagonal(Middle,BottomRight).


/*Check all boxes for leagal placment*/
/*Note: Directions are for visualisation purposes and might not mean what they say*/

/*checking corners or others that might hapend to apply*/
checkBoxes(_, [['-',Top,'-'], [Left, Middle,'-'], ['-','-','-']]):-
    checkBox([['-',Top,'-'], [Left, Middle,'-'], ['-','-','-']]).

checkBoxes(_, [[_,_,'-'], [_, '-','-'], ['-','-','-']]).

checkBoxes(Puzzle,[[_, Top,TopRight | T1], [_,'-', Right | T2], ['-','-','-' | T3]]):- 
    checkBoxes(Puzzle,[[Top,TopRight| T1], ['-',Right | T2],['-','-' | T3]]).

checkBoxes(Puzzle, [['-', Top,'-'] | T1], [Left,Middle,Right | T2], ['-','-','-' | T3]):-
    checkBox([['-',Top,'-'],[Left,Middle,Right],['-','-','-']]),
    checkBoxes(Puzzle,[[Top, '-' | T1], [Middle,Right | T2], ['-','-'|T3]]).

checkBoxes([_,PuzzleLine| PuzzleRest], [['-', Top, '-'],[Left,Middle,'-'],['-',Bottom,'-'] | _]):-
    checkBox([['-',Top, '-'],[Left, Middle, '-'],['-', Bottom,'-']]),
    checkBoxes([PuzzleLine | PuzzleRest], [PuzzleLine | PuzzleRest]).

checkBoxes([_,PuzzleLine| PuzzleRest],[[_,_,'-'], [_,'-','-'], [_,_,'-' ] | _]):-
    checkBoxes([PuzzleLine | PuzzleRest],[PuzzleLine | PuzzleRest]).

checkBoxes(Puzzle, [['-',Top,'-' | T1], [Left,Middle,Right | T2], ['-',Bottom,'-' | T3] | Tail]):-
    checkBox([['-',Top,'-'],[Left,Middle,Right],['-',Bottom,'-']]),
    checkBoxes(Puzzle,[[Top, '-' | T1],[Middle, Right | T2],[Bottom, '-' | T3] | Tail]).

checkBoxes(Puzzle,[[_, Top, TopRight | T1], [_,'-', Right | T2], [_,Bottom, BottomRight | T3] | Tail]):-
    checkBoxes(Puzzle,[[Top, TopRight | T1], ['-' , Right | T2], [Bottom, BottomRight | T3] | Tail]).

/* Get spesific puzzle box*/
getBox([H], 1, H) :- !.
getBox([H | _], 1, H):- !.
getBox([_ | T ], BoxToGet, Return):- BoxToGet1 is BoxToGet -1, getBox(T,BoxToGet1,Return).


/*Set Value of Box*/
setBox([_],   1, Box, [Box]):- !.
setBox([_|T], 1, Box, [Box| T]) :- !.
setBox([H|T], BoxToChange, Box, Return):- 
    BoxToChange > 1,
    BoxToChange1 is BoxToChange -1,
    setBox(T, BoxToChange1, Box, OldValue),
    append([H], OldValue, Return).

/*Get Column*/
getColumn([[FirstBox|RestBoxes]], ColumnToGet, [ThisRow]):-
    !, getBox([FirstBox | RestBoxes], ColumnToGet, ThisRow).
getColumn([[FirstBox | RestBoxes] | RestOfRows], ColumnToGet, [ThisRow | BoxInLaterRows]):-
    getColumn(RestOfRows, ColumnToGet, BoxInLaterRows),
    getBox([FirstBox| RestBoxes], ColumnToGet, ThisRow).


/*Get Size of ship*/

getShipSize(['V'], [], 1).
getShipSize(['A' | T], Remaining, Size):-
    !, getShipSize(T, Remaining, Result),
    Size is Result + 1.

getShipSize(['>'], [], 1).
getShipSize(['<' | T], Remaining, Size):-
    !, getShipSize(T, Remaining, Result),
    Size is Result + 1.

getShipSize(['+' | T], Remaining, Size):- 
    !, getShipSize(T, Remaining, Result),
    Size is Result + 1.

/*Get lenght of list*/
lenghtOfList([], 0):- !.
lenghtOfList([_], 1):- !.
lenghtOfList([_ | T], Lenght):- lenghtOfList(T, OldLenght), Lenght is OldLenght+1, !.

/*Find removed Boxes from first row*/
findNumber([[_], [_]], 1).
findNumber([[_], [_] | _], 1).
findNumber([[_], [_ | T]], Return):- findNumber(T, Lenght), Return is Lenght + 1.
findNumber([[_ | T] | _], Return):- findNumber(T,Lenght), Return is Lenght + 1.
findNumber([[_|T], [_|T2]], Return):- findNumber([T, T2],Return).
findNumber([[_ | T], [_ | T2] |  T3], Return):-findNumber([T, T2 | T3], Return), !.

/*Count Number of Ships of all types*/
countShips([['-' | T]], Ships):- !, countShips([T],Ships).
