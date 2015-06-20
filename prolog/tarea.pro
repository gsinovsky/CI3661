
%% Definición de estructura en la que retornará la solución
solucion(salto, x, y, movimientos(norte, sur, este, oeste)).


%% Predicado para verificar que las listas en una lista tiene el mismo tamaño
mismaLongitud([]) :- !.
mismaLongitud([[]]) :- !.
mismaLongitud([[_|_]]) :- !.
mismaLongitud([X,Y|Rest]) :- 
  length(X, Len), 
  length(Y, Len), 
  mismaLongitud([Y|Rest]).

%% Predicado para verificar que los elementos de  una lista sean numeros entre 0 y 9
verificarNumero([]).
verificarNumero([H|T]) :- 
    integer(H),
    H < 10,
    H > 0,
    verificarNumero(T).

%% Predicado tableroValido del enunciado
tableroValido(Tablero) :-
 mismaLongitud(Tablero),
 maplist(verificarNumero, Tablero).



