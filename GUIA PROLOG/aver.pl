gaga.
%Ejercicio 1
natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

%suc(0),N 


%Como Prolog resuelve las reglas de arriba hacia abajo, de izquierda a derecha, es
%importante el orden en que definimos las reglas. Los "casos base" tienen que estar
%primeros, y también es importante cortar el "caso recursivo" si ya no hay soluciones
%válidas. Si no cuando pedimos más resultados a Prolog podemos nunca llegar al false y
%entrar en una ejecución infinita.

%Ejercicio 4 
juntar([],YS,YS).
juntar([X|XS],YS,[X|ZS]):- juntar(XS,YS,ZS).    

%Ejercicio 5
%a.
%last(?L, ?U), donde U es el último elemento de la lista L.


%last([X|XS],R):- last(XS,R).
last(XS,R):- append(_,[R],XS).

%b.
%reverse(+L, ?R)

%Fonra:
reverse([],[]).
reverse([L|LS],R):- reverse(LS,R1),append(R1,[L],R).

%Agus:
reverse([],[]).
reverse(XS,RES):- append(L1,[R],XS),append([R],R2,RES),reverse(L1,R2).

%c.prefijo(?P, +L), donde P es prefijo de la lista L.
prefijo(X,Z):-append(X,_,Z).
%d
sufijo(L1,L2):- append(_,L1,L2).

%sublista(?S, +L), donde S es sublista de L.

sublista(S,L):-sufijo(T,L),prefijo(S,T).

%[1,2,3,4,5] = prefijos,sufijos,[2,3][2,3,4][3,4].

%Ejercicio 6
%aplanar(+XS,-YS).
aplanar([],[]).
aplanar([X|XS],ZS):-aplanar(X,Z1),aplanar(XS,Z2),append(Z1,Z2,ZS).
aplanar([X|XS],ZS):-not(aplanar(X,_)),aplanar(XS,Z1),append([X],Z1,ZS).

%Ejercicio 7
%interseccion(+L1,+L2,-L3)

%partir(N,L,L1,L2)
partir(0,L,[],L).
partir(N,[L|LS],[L|XS],Y):- partir(N1,LS,XS,Y),N is N1+1.

%borrar(+ListaOriginal,+X,-ListaSinXS)
borrar([],_,[]).
borrar([X|XS],T,[X|ZS]):-X\=T,borrar(XS,T,ZS).
borrar([X|XS],X,ZS):-borrar(XS,X,ZS).

%sacarDuplicados(+L1,-L2).
sacarDuplicados([],[]).
sacarDuplicados([X|XS],[X|ZS]):-not(member(X,XS)),sacarDuplicados(XS,ZS).
sacarDuplicados([X|XS],ZS):- member(X,XS),sacarDuplicados(XS,ZS).


%Ejercicio 8
%parteQueSuma(+L,+S,-P).

partesQueSuma(L,S,R):- partes(R,L), sumlist(R,S).

%partes(-R,+L).
partes([],[]).
partes([L|S1],[L|LS]):-sublista(S1,LS).
partes(S,[_|LS]):-sublista(S,LS).

