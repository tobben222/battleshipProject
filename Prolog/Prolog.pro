outputFile('./battleships_solved.txt').
inputFile('./battleships_unsolved.txt').

/********************* dummy solution algorithms -> fill your correct algorithm here */
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
leagalVertical('S','-').
leagalVertical('-','S').
leagalVertical('S','+').
leagalVertical('+','S').
leagalVertical('S','V').
leagalVertical('A','S').

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
legalHorizontal('S','-').
legalHorizontal('-','S').
legalHorizontal('S','+').
legalHorizontal('+','S').
legalHorizontal('S','>').
legalHorizontal('<','S').

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


/*place new row and column of water on all sides of puzzle so tests can check borders*/

/*add rows*/
addRow([[_] | _], ['-']).
addRow([[_ | T] | _], ['-' | Retrun]):- 
  addRow([T],Retrun).

/*add coloms*/
addColoms(grid([[H|T]]),[Return]) :- 
  append([H|T],['-'], Temp),
  append(['-'], Temp,Return).

addColoms(grid([[Head | Tail] | Tail2]), [NewRow | Tail3]) :-
  append([Head | Tail], ['-'], TempRow),
  append(['-'], TempRow, NewRow),
  addColoms(grid(Tail2), Tail3).

addColoms(Puzzle,vertical).

/*check if the nuber in vertical matches Row*/

/*checking single row*/
checkRow([], 0).
checkRow([H|T], RowNumber):- not(H == '-'), RowNumber1 is RowNumber -1, checkRow(T, RowNumber1).
checkRow(['-'| T], RowNumber):- checkRow(T,RowNumber).

/*Checking all rows*/
checkRows([[H|T]],[RowNumber]):-
   checkRow([H,T], RowNumber).

checkRows(grid([[H|T]| T2]), horizontal([RowNumber| RestRowNumbers])):- 
  checkRow([H|T], RowNumber),
  checkRows(T2, RestRowNumbers).

checkRows([[H|T]| T2], [RowNumber| RestRowNumbers]):- 
  checkRow([H|T], RowNumber),
  checkRows(T2, RestRowNumbers).

checkRows(Puzzle,Horisontal).
  

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
checkColumns([[H] | T],[ColumnNumber]):- 
  checkColumn([[H] | T], [], ColumnNumber).

checkColumns(grid([[H] | T]),vertical([ColumnNumber])):- 
  checkColumn([[H] | T], [], ColumnNumber).

checkColumns(grid([[H|T] | T2]), vertical([ColumnNumber | RestColumnNumbers])):-
  checkColumn([[H|T], T2], NewPuzzle, ColumnNumber),
  checkColumns(NewPuzzle, RestColumnNumbers).

checkColumns([[H|T] | T2], [ColumnNumber | RestColumnNumbers]):-
  checkColumn([[H|T], T2], NewPuzzle, ColumnNumber),
  checkColumns(NewPuzzle, RestColumnNumbers).

checkColumns(Puzzle,Column).

/*check leagal box*/

/*Single ship*/
checkBox([],0).
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
checkBox([[TopLeft,_ , TopRight], [_, Middle, _], [BottomLeft,_,BottomRight]]):-
  leagalDiagonal(Middle,TopLeft),
  leagalDiagonal(Middle,TopRight),
  leagalDiagonal(Middle,BottomLeft),
  leagalDiagonal(Middle,BottomRight).

checkBox(PuzzleLine,PuzzleLine).

/*Check all boxes for leagal placment*/
/*Note: Directions are for visualisation purposes and might not mean what they say*/

/*checking corners or others that might hapend to apply*/

checkBoxes([],0).

checkBoxes(_, [[_,_,'-'], [_, '-','-'], ['-','-','-']]).

checkBoxes(_, [['-',Top,'-'], [Left, Middle,'-'], ['-','-','-']]):-
  checkBox([['-',Top,'-'], [Left, Middle,'-'], ['-','-','-']]).


checkBoxes(Puzzle,[[_, Top,TopRight | T1], [_,'-', Right | T2], ['-','-','-' | T3]]):- 
  checkBoxes(Puzzle,[[Top,TopRight| T1], ['-',Right | T2],['-','-' | T3]]).
checkBoxes(grid(Puzzle), [[_, Top,TopRight | T1], [_,'-', Right | T2], ['-','-','-' | T3]]):- 
  checkBoxes(Puzzle,[[Top,TopRight| T1], ['-',Right | T2],['-','-' | T3]]).

checkBoxes(Puzzle, [['-', Top,'-'] | T1], [Left,Middle,Right | T2], ['-','-','-' | T3]):-
  checkBox([['-',Top,'-'],[Left,Middle,Right],['-','-','-']]),
  checkBoxes(Puzzle,[[Top, '-' | T1], [Middle,Right | T2], ['-','-'|T3]]).
checkBoxes(grid(Puzzle), [['-', Top,'-'] | T1], horizontal([Left,Middle,Right | T2]), ['-','-','-' | T3]):-
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
checkBoxes(grid(Puzzle), horizontal([['-',Top,'-' | T1], [Left,Middle,Right | T2], ['-',Bottom,'-' | T3] | Tail])):-
  checkBox([['-',Top,'-'],[Left,Middle,Right],['-',Bottom,'-']]),
  checkBoxes(Puzzle,[[Top, '-' | T1],[Middle, Right | T2],[Bottom, '-' | T3] | Tail]).

checkBoxes(Puzzle,[[_, Top, TopRight | T1], [_,'-', Right | T2], [_,Bottom, BottomRight | T3] | Tail]):-
  checkBoxes(Puzzle,[[Top, TopRight | T1], ['-' , Right | T2], [Bottom, BottomRight | T3] | Tail]).
checkBoxes(grid(Puzzle), horizontal([[_, Top, TopRight | T1], [_,'-', Right | T2], [_,Bottom, BottomRight | T3] | Tail])):-
  checkBoxes(Puzzle,[[Top, TopRight | T1], ['-' , Right | T2], [Bottom, BottomRight | T3] | Tail]).

checkBoxes(Puzzle, Puzzle).

/* Get spesific puzzle box*/
getBox([H], 1, H) :- !.
getBox([H | _], 1, H):- !.
getBox([_ | T ], BoxToGet, Return):- BoxToGet1 is BoxToGet -1, getBox(T,BoxToGet1,Return).


/*Set Value in list*/
setValue([_],   1, Box, [Box]):- !.
setValue([_|T], 1, Box, [Box| T]) :- !.
setValue([H|T], BoxToChange, Box, Return):- 
  BoxToChange > 1,
  BoxToChange1 is BoxToChange -1,
  setValue(T, BoxToChange1, Box, OldValue),
  append([H], OldValue, Return).

setValue(grid([H|T]), BoxToChange, Box, Return):- 
  BoxToChange > 1,
  BoxToChange1 is BoxToChange -1,
  setValue(T, BoxToChange1, Box, OldValue),
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
/*only count the upper most and Left most peases of a boat to only count once and also single ships*/

countShips([],0):- !.
countShips([['-' | T]], Ships):- !, countShips([T],Ships).
countShips(grid([['-' | T]]), Ships):- !, countShips([T],Ships).

countShips([['-']| T], Ships):- !, countShips(T,Ships).
countShips(grid([['-']| T]), Ships):- !, countShips(T,Ships).

countShips([['-' | T] | T2], Ships):- !, countShips([T|T2], Ships).
countShips(grid([['-' | T] | T2]), Ships):- !, countShips([T|T2], Ships).

countShips([['+' | T] | T2], Ships):- !, countShips([T|T2], Ships).
countShips(grid([['+' | T] | T2]), Ships):- !, countShips([T|T2], Ships).

countShips([['V' | T] | T2], Ships):- !, countShips([T|T2], Ships).
countShips(grid([['V' | T] | T2]), Ships):- !, countShips([T|T2], Ships).

countShips([['>' | T] | T2], Ships):- !, countShips([T|T2], Ships).
countShips(grid([['>' | T] | T2]), Ships):- !, countShips([T|T2], Ships).

countShips([['A' | T] | T2], Ships):-
  !, findNumber([['A' | T]| T2], Number),
  getColumn(T2, Number, Column),
  getShipSize(['A' | Column], _ ,Size),
  getBox(Ships, Size, NumberOfSizeShip),
  NumberOfSizeShip > 0,
  NumberOfSizeShip1 is NumberOfSizeShip - 1,
  setValue(Ships,Size,NumberOfSizeShip1,NewShipList),
  countShips([T | T2], NewShipList).

countShips(grid([['A' | T] | T2]), Ships):-
  !, findNumber([['A' | T]| T2], Number),
  getColumn(T2, Number, Column),
  getShipSize(['A' | Column], _ ,Size),
  getBox(Ships, Size, NumberOfSizeShip),
  NumberOfSizeShip > 0,
  NumberOfSizeShip1 is NumberOfSizeShip - 1,
  setValue(Ships,Size,NumberOfSizeShip1,NewShipList),
  countShips([T | T2], NewShipList).

countShips([['<' | T] | T2], Ships):-
  !, findNumber([['<' | T]| T2], Number),
  getColumn(T2, Number, Column),
  getShipSize(['<' | Column], _ ,Size),
  getBox(Ships, Size, NumberOfSizeShip),
  NumberOfSizeShip > 0,
  NumberOfSizeShip1 is NumberOfSizeShip - 1,
  setValue(Ships,Size,NumberOfSizeShip1,NewShipList),
  countShips([T | T2], NewShipList).

countShips(grid([['<' | T] | T2]), Ships):-
  !, findNumber([['<' | T]| T2], Number),
  getColumn(T2, Number, Column),
  getShipSize(['<' | Column], _ ,Size),
  getBox(Ships, Size, NumberOfSizeShip),
  NumberOfSizeShip > 0,
  NumberOfSizeShip1 is NumberOfSizeShip - 1,
  setValue(Ships,Size,NumberOfSizeShip1,NewShipList),
  countShips([T | T2], NewShipList).

countShips([['*' | T], T2], Ships):- 
  !, getBox(Ships,1, NumberOfShips),
  NumberOfShips > 0,
  NewNumberOfShips is NumberOfShips -1,
  setValue(Ships, 1 ,NewNumberOfShips, NewShipList),
  countShips([T| T2], NewShipList).

countShips(grid([['*' | T], T2]), Ships):- 
  !, getBox(Ships,1, NumberOfShips),
  NumberOfShips > 0,
  NewNumberOfShips is NumberOfShips -1,
  setValue(Ships, 1 ,NewNumberOfShips, NewShipList),
  countShips([T| T2], NewShipList).

countShips(Puzzle, Ships).

doSolve(battleships(size(X),Ships, Row, Column, Puzzle), battleships(size(X), Ships, Row, Column, Puzzle)):-
  addColoms(Puzzle, TempPuzzle),
  addRow(TempPuzzle, VaterRow),
  append(TempPuzzle, [VaterRow], TempPuzzle2),
  append([VaterRow], TempPuzzle2, NewPuzzle),
  checkColumns(Puzzle, Column),
  checkRows(Puzzle,Row),
  writeGrid(NewPuzzle), nl,
  write('Riktig 7 '), nl,
  checkBoxes(NewPuzzle, NewPuzzle),
  write('Riktig 8'), nl,
  countShips(NewPuzzle, Ships),!.

doSolve(Solution,Solution):-
  write('ErrorErrorError'), nl.

/********************* writing the result */
writeFullOutput(battleships(size(N),_,_,_,grid(Puzzle))):- 
  write('size '), write(N), write('x'), write(N), nl, writeGrid(Puzzle).

writeGrid([]).
writeGrid([E|R]):- writeGridLine(E), writeGrid(R).

writeGridLine([]):- nl.
writeGridLine([E|R]):- E='?', !, write(E), write(' '), writeGridLine(R).
writeGridLine([E|R]):- write(E), write(' '), writeGridLine(R).


/********************** reading the input */
readProblem(battleships(size(N),boats(B),horizontal(H),vertical(V),grid(Grid))):- 
  findKW(size), readInt(N), readInt(M), M=N, length(H, N), length(V,N), length(Grid,N), 
  readShips(B), readHorizontal(H), readVertical(V), findKW(hints), readGridLines(N,Grid).

findKW(KW):- string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):- peek_code(-1), !, fail.
findKW(KW):- get_code(_), findKW(KW).

readKW([]):- get_code(_).
readKW([H|T]):- get_code(H), readKW(T).

readShips(L):- findKW(ships), readShipCount(L).

readShipCount(L):- 
  peek_code(C), is_number_code(C,_), readInt(Count), readInt(Size), 
  expandShips(Count,Size,L1), readShipCount(L2), append(L1,L2,L), !.
readShipCount([]).

expandShips(0,_,[]).
expandShips(N,S,[S|T]):- N>0, N1 is N-1, expandShips(N1,S,T).

readHorizontal(L):- findKW(horizontal), readNumberLine(L).
readVertical(L):- findKW(vertical), readNumberLine(L).

readGridLines(_,[]).
readGridLines(N,[H|T]):- length(H,N), readGridLine(H), readGridLines(N,T).

readGridLine([]).
readGridLine([E|T]):- get_code(M), translate(M,E), !, readGridLine(T).

translate(-1,'ERROR: EOF').
translate(63,_).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- string_codes(E,[X]).

whitespace(10). whitespace(12). whitespace(32).

readNumberLine([]).
readNumberLine([E|T]):- readInt(E), readNumberLine(T).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.
/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), 
  write(N), nl, solveProblems(N), told, seen, !.

run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(P), doSolve(P,S), writeFullOutput(S), !, N1 is N-1, solveProblems(N1).

:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

:- run.
:- halt.