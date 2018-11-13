outputFile('./battleships_solved.txt').
inputFile('./battleships_unsolved.txt').

/********************* dummy solution algorithms -> fill your correct algorithm here */


doSolve(Solution,Solution).

/********************* writing the result */
writeFullOutput(battleships(size(N),_,_,_,grid(Grid))):- 
  write('size '), write(N), write('x'), write(N), nl, writeGrid(Grid).

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
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(P), doSolve(P, S), writeFullOutput(S), !, N1 is N-1, solveProblems(N1).

:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

:- run.
:- halt.