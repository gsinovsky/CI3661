
%% Definición de estructura en la que se retornará la solución
solucion(salto, x, y, movimientos(norte, sur, este, oeste)).


%% Predicado para verificar que las listas en una lista tiene el mismo tamaño
misma_longitud([]) :- !.
misma_longitud([[]]) :- !.
misma_longitud([[_|_]]) :- !.
misma_longitud([X,Y|Rest]) :- 
  length(X, Len), 
  length(Y, Len), 
  misma_longitud([Y|Rest]).


%% tableroValido(+Tablero)
%%  tableroValido.
%% saltoEnPosicion(+Tablero, +N, +X, +Y, -solucion).
%% salto(+Tablero, +N, -solucion).


