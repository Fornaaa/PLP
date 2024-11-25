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

ordenada([_]).
ordenada([X|XS]):- head(XS,Y) ,X=<Y,ordenada(XS).

head([X|_],X).

%esta menos gaga q el de agus
%aBB(nil).
%aBB(bin(nil, _, nil)).
%aBB(bin(Izq, V, nil)) :- raiz(Izq, RI), RI < V, aBB(Izq). 
%aBB(bin(nil, V, Der)) :- raiz(Der, RD), V < RD, aBB(Der).
%aBB(bin(Izq, V, Der)) :- raiz(Izq, RI), raiz(Der, RD), RI < V, V < RD, aBB(Izq), aBB(Der).

%aBBinsertar(+X,+T1,-T2)
aBBinsertar(X,nil,bin(nil,X,nil)).
aBBinsertar(X,bin(I,C,D),bin(I,C,TN)):-X>C,aBBinsertar(X,D,TN).
aBBinsertar(X,bin(I,C,D),bin(TN,C,D)):-X<C,aBBinsertar(X,I,TN).

%13.


coprimos(X,Y):- desde(1,X),between(1,X,Y), sonCoprimos(X,Y).

sonCoprimos(X,Y):- 1 is gcd(X,Y).

%14
%generarCuadrado(_,_,0,[]).
%generarCuadrado(N,M,T,[X|XS]):-length(X,N),generarLista(N,X),sumlist(X,M),generarCuadrado(N,M,T1,XS),T is T1+1.


cuadradoSemiMagico(N,XS):- desde(0,K),length(XS,N),generarCuadrados(N,K,XS).

generarCuadrados(N,K,[X]):-generarLista(N,K,X).
generarCuadrados(N,K,[X|XS]):-generarLista(N,K,X),generarCuadrados(N,K,XS).

%generarCuadradoN(0,_,[]).
%generarCuadradoN(N,K,[X|XS]):- generarLista(N,K,X),generarCuadradoN(N,K,XS). 

generarLista(0,0,[]).
generarLista(N,M,[X|XS]):-N>0,between(0,M,X),M1 is M-X,N1 is N-1,generarLista(N1,M1,XS).

%16
frutal(frutilla).
frutal(banana).
frutal(manzana).

cremoso(frutilla).
cremoso(banana).

aver(X):-not(cremoso(X)),frutal(X).


%17
corteMasParejo(L,L1,L2):- corte(L,L1,L2,R1), not((corte(L,_,_,R2),R2<R1)).

corte(L,L1,L2,X):-append(L1,L2,L), sumlist(L1,S1), sumlist(L2,S2), X is abs(S1-S2).


% próximoNumPoderoso(+X,-Y)


%primo(X):- X>1,divisores(X,[1]).


proxNumPod(X,K):- X1 is X+1,desde(X1,K),numPoderoso(K),!.  

numPoderoso(1).
numPoderoso(X):- between(1,X,K),primo(K),0 is X mod K,K2 is K * K,0 is X mod K2,borrarDivisor(X,K,N2),numPoderoso(N2),!.

borrarDivisor(X,K,X):- not(0 is X mod K).
borrarDivisor(X,K,RES):- 0 is X mod K, R is X / K,borrarDivisor(R,K,RES).

primo(X):-X\=1,X1 is X-1,not((between(2,X1,K),0 is X mod K)).


%Ejercicio parcial 2024
%tieneMateriaAprobada(+E,M).
estudiante(juan).
notas([(forna,plp,6)]).
tieneMateriaAprobada(E,M):-estudiante(E),notas(XS),member((E,M,N),XS),N>=4.

%eliminarAplazos(+NS,-L).
eliminarAplazos([],[]).
eliminarAplazos([(E,M,N)|XS],[(E,M,N)|L]):-N>=4,eliminarAplazos(XS,L).
eliminarAplazos([(E,M,N)|XS],[(E,M,N)|L]):- N<4,not(tieneMateriaAprobada(E,M)),eliminarAplazos(XS,L).
eliminarAplazos([(E,M,N)|XS],L):-tieneMateriaAprobada(E,M),N<4,eliminarAplazos(XS,L).

%promedio(+A,-P).
promedio(A,P):-estudiante(A),notas(XS),eliminarAplazos(XS,Z),calcularPromedio(A,Z,0,0,P).

calcularPromedio(_,[],P,C,Z):-Z is P/C.
calcularPromedio(E,[(E,_,N)|XS],P,C,Z):- P1 is P+N, C1 is C+1 ,calcularPromedio(E,XS,P1,C1,Z).
calcularPromedio(E,[(A,_,_)|XS],P,C,Z):- E\=A,calcularPromedio(E,XS,P,C,Z).

%mejorEstudiante(A).
mejorEstudiante(A):-promedio(A,ZS),not((promedio(B,ZS),B>=A)).


%Ejercicio Recuperatorio 2024
%generarCapicuas(-L)

generarCapicuas(L):-desde(1,N),generarListasGenerico(L,N),esCapicua(L).

generarListasGenerico([],0).
generarListasGenerico(XS,N):- N>0,between(1,N,N1),N2 is N-N1,generarListasGenerico(X1,N2),append([N1],X1,XS). 

esCapicua([]).
esCapicua([_]).
esCapicua(L):-append([X],XS,L),append(YS,[X],XS),esCapicua(YS).

%tokenizar(+D,L,-T)
tokenizar(XS,[L|LS],T):- 