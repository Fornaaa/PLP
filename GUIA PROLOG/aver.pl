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
interseccion([],_,[]).
interseccion([X|XS],YS,ZS):-member(X,YS),interseccion(XS,YS,Z1),append([X],Z1,ZS).
interseccion([X|XS],YS,ZS):-not(member(X,YS)),interseccion(XS,YS,ZS).


%partir(N,L,L1,L2)
partir(0,L,[],L).
partir(N,[L|LS],[L|XS],Y):- partir(N1,LS,XS,Y), N is N1+1.

%borrar(+ListaOriginal,+X,-ListaSinXS)
borrar([],_,[]).
borrar([X|XS],T,[X|ZS]):-X\=T,borrar(XS,T,ZS).
borrar([X|XS],X,ZS):- borrar(XS,X,ZS).

%sacarDuplicados(+L1,-L2).
sacarDuplicados([],[]).
sacarDuplicados([X|XS],[X|ZS]):-not(member(X,XS)),sacarDuplicados(XS,ZS).
sacarDuplicados([X|XS],ZS):- member(X,XS),sacarDuplicados(XS,ZS).

%permutacion(+L1,+L2)
permutacion([],[]).
permutacion([X|XS],YS):-length([X|XS],L1),length(YS,L2),L1=:=L2,member(X,YS),borrar(YS,X,LS),permutacion(XS,LS).



%reparto(+XS,+N,-LLs).
reparto(_,0,[]).
reparto(XS,N,[L|LL]):-miembro(L,XS),reparto(XS,N1,LT),N1>=0, N is N1+1, append(L,LT,LL), aplanar(LL,XS).

miembro([],_).
miembro([L|LS],XS):-member(L,XS),miembro(LS,XS).

%Ejercicio 8
%parteQueSuma(+L,+S,-P).

partesQueSuma(L,S,R):- partes(R,L), sumlist(R,S).

%partes(-R,+L).
partes([],[]).
partes([L|S1],[L|LS]):-sublista(S1,LS).
partes(S,[_|LS]):-sublista(S,LS).

%Ejercicio 9 
%desde(+X,?Y).
desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).


%Ejercicio 11

vacio(nil).
raiz(bin(_, V, _), V).


altura(nil,0).
altura(bin(I,_,D),H):-altura(I,H1),altura(D,H2), maxi(H1,H2,H3), H is H3+1.


maxi(H1,H2,H1):- H1 >= H2.
maxi(H1,H2,H2):- H1 < H2.

cantNodos(nil,0).
cantNodos(bin(I,_,D),H):-cantNodos(I,H1),cantNodos(D,H2),H is 1+H1+H2.

%inorder izq r der
inorder(nil,[]).
inorder(bin(I,C,D),Z):-inorder(I,V1),inorder(D,V2),append(V1,[C|V2],Z).

%Arbol con inorder
aBinorder([],nil).
aBinorder(L,bin(I,C,D)):-append(RI,[C|RD],L),aBinorder(RI,I),aBinorder(RD,D).

%ABB de forna:

aBB(X):- inorder(X,T),ordenada(T).

ordenada([X]).
ordenada([X|XS]):- head(XS,Y) ,X=<Y,ordenada(XS).

head([X|XS],X).

%esta menos gaga q el de agus
aBB(nil).
aBB(bin(nil, _, nil)).
aBB(bin(Izq, V, nil)) :- raiz(Izq, RI), RI < V, aBB(Izq). 
aBB(bin(nil, V, Der)) :- raiz(Der, RD), V < RD, aBB(Der).
aBB(bin(Izq, V, Der)) :- raiz(Izq, RI), raiz(Der, RD), RI < V, V < RD, aBB(Izq), aBB(Der).

%13
