% Valores de los nodos.
valor(a,15).
valor(b,10).
valor(c,20).
valor(d,25).
valor(e,27).
valor(f,29).
valor(g,31).
valor(h,33).
valor(i,19).
valor(j,21).
valor(k,17).
valor(l,17).
valor(m,25).
valor(n,28).
valor(o,30).
valor(p,32).
valor(q,36).
valor(r,50).
valor(s,51).
valor(t,100).

% Nodos con sus coordenadas.
coordenadas(a,0,0).
coordenadas(b,0,-1).
coordenadas(c,0,-2).
coordenadas(d,0,-3).
coordenadas(e,1,-3).
coordenadas(f,1,-4).
coordenadas(g,1,-5).
coordenadas(h,2,-5).
coordenadas(i,2,-3).
coordenadas(j,3,-4).
coordenadas(k,1,-2).
coordenadas(l,2,-2).
coordenadas(m,4,-4).
coordenadas(n,4,-5).
coordenadas(o,5,-4).
coordenadas(p,6,-4).
coordenadas(q,6,-5).
coordenadas(r,5,-3).
coordenadas(s,5,-2).
coordenadas(t,5,-1).

% Conexiones entre nodos.
conexión(a,b). conexión(b,a).
conexión(a,k). conexión(k,a).
conexión(b,c). conexión(c,b).
conexión(c,d). conexión(d,c).
conexión(c,k). conexión(k,c).
conexión(d,e). conexión(e,d).
conexión(e,f). conexión(f,e).
conexión(f,g). conexión(g,f).
conexión(g,h). conexión(h,g).
conexión(h,i). conexión(i,h).
conexión(h,j). conexión(j,h).
conexión(i,j). conexión(j,i).
conexión(i,k). conexión(k,i).
conexión(j,m). conexión(m,j).
conexión(k,l). conexión(l,k).
conexión(l,m). conexión(m,l).
conexión(m,n). conexión(n,m).
conexión(m,o). conexión(o,m).
conexión(o,p). conexión(p,o).
conexión(o,r). conexión(r,o).
conexión(p,q). conexión(q,p).
conexión(r,s). conexión(s,r).
conexión(s,t). conexión(t,s).

% Costos de las conexiones entre los nodos.
costo(a,b,71).  costo(b,a,71).
costo(a,k,151). costo(k,a,151).
costo(b,c,75).  costo(c,b,75).
costo(c,d,118). costo(d,c,118).
costo(c,k,140). costo(k,c,140).
costo(d,e,111). costo(e,d,111).
costo(e,f,70).  costo(f,e,70).
costo(f,g,75).  costo(g,f,75).
costo(g,h,120). costo(h,g,120).
costo(h,i,146). costo(i,h,146).
costo(h,j,138). costo(j,h,138).
costo(i,j,97).  costo(j,i,97).
costo(i,k,80).  costo(k,i,80).
costo(j,m,101). costo(m,j,101).
costo(k,l,99).  costo(l,k,99).
costo(l,m,211). costo(m,l,211).
costo(m,n,90).  costo(n,m,90).
costo(m,o,85).  costo(o,m,85).
costo(o,p,75).  costo(p,o,75).
costo(o,r,142). costo(r,o,142).
costo(p,q,75).  costo(q,p,75).
costo(r,s,92).  costo(s,r,92).
costo(s,t,37).  costo(t,s,37).

% -----------------------------------------------------------------------%

% Búsqueda en profundidad.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Manda llamar al método profundidadR.
% Al encontrar la solución con amplitudR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
profundidad(Inicio,Fin,Solución):-
    profundidadR([Inicio],Fin,Res),
    reverse(Res,Solución),!.

% Método que encuentra la solución utilizando la búsqueda en
% profundidad.
% Visita el primer sucesor encontrado, verifica si no pertenece ya
% al camino, y en casi de que no, lo agrega al recorrido.
% En caso de que sí, se regresa y busca el siguiente sucesor.
profundidadR([],_,[]):-!.
profundidadR([Fin|Camino],Fin,[Fin|Camino]):-!.
profundidadR([Actual|Camino],Fin,Solución):-
    (conexión(Actual,Sig),\+member(Sig,Camino)),
    append([Sig],[Actual|Camino],Recorrido),
    profundidadR(Recorrido,Fin,Solución).

% -----------------------------------------------------------------------%

% Búsqueda en amplitud.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Manda llamar al método amplitudR.
% Al encontrar la solución con amplitudR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
amplitud(Inicio,Fin,Solución):-
    amplitudR([Inicio],Fin,[],Res),
    reverse(Res,Solución),!.

% Método que encuentra la solución utilizando la búsqueda en amplitud.
% Utiliza el método predefinido findall/3.
% findall/3 se compone de tres argumentos:
% (1) Cómo va a agregar los elementos a la lista.
% (2) Qué condiciones se tienen que cumplir para agregarlos
% (3) La lista resultante.
% Es decir, agrega al recorrido los sucesores que no hayan sido
% visitados ya.
amplitudR([],_,[],[]):-!.
amplitudR([Fin|_],Fin,Camino,[Fin|Camino]):-!.
amplitudR([Actual|Otros],Fin,Solución,Res):-
    findall(Sig,(conexión(Actual,Sig),\+member(Sig,Solución),\+member(Sig,Otros)),Nuevos),
    append(Otros,Nuevos,Camino),
    amplitudR(Camino,Fin,[Actual|Solución],Res).

% -----------------------------------------------------------------------%

% Hill climbing.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Manda llamar al método hillClimbingR.
% Al encontrar la solución con hillClimbingR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
hillClimbing(Inicio,Fin,Solución):-
    hillClimbingR([Inicio],Fin,Camino),
    reverse(Camino,Solución),!.

% Método utilizado en la búsqueda Hill-Climbing.
% Agrega a la lista el sucesor que cumpla con 2 condiciones:
% 1. No pertenecer ya al recorrido.
% 2. Que el valor del nodo encontrado sea mayor al nodo actual, así se
% garantiza que el estado encontrado presente mejoría en comparación con
% el estado actual.
sucesor([],[]):-!.
sucesor([Actual|Recorrido],Camino):-
    (conexión(Actual,Sig),\+member(Sig,Recorrido)),
    valor(Actual,Valor1),valor(Sig,Valor2),
    Valor2>Valor1,
    append([Sig],[Actual|Recorrido],Camino).

% Método que encuentra la solución utilizando la búsqueda Hill Climbing.
hillClimbingR([],_,[]):-!.
hillClimbingR([Fin|Camino],Fin,[Fin|Camino]):-!.
hillClimbingR(Recorrido,Meta,Final):-
    sucesor(Recorrido,Res),
    hillClimbingR(Res,Meta,Final).

% -----------------------------------------------------------------------%


% Steepest Ascent Hill Climbing.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Manda llamar al método sa_HillClimbing.
% Al encontrar la solución con sa_HillClimbing, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
sa_HillClimbing(Inicio,Fin,Solución):-
    saHillClimbingR([Inicio],Fin,Camino),
    reverse(Camino,Solución),!.

% Método utilizado en la búsqueda Steepest Ascent Hill-Climbing.
% Agrega a la lista los sucesores, con sus valores,  que cumpla con 2
% condiciones:
% 1. No pertenecer ya al recorrido.
% 2. Que el valor del nodo encontrado sea mayor al nodo actual, así
% se garantiza que el estado encontrado presente mejoría en comparación
% con el estado actual.
% Utiliza el método predefinido findall/3.
% findall/3 se compone de tres argumentos:
% (1) Cómo va a agregar los elementos a la lista.
% (2) Qué condiciones se tienen que cumplir para agregarlos
% (3) La lista resultante.
siguientePos([],[]):-!.
siguientePos([Actual|Recorrido],Res):-
    findall([NodoSig|Valor],(conexión(Actual,NodoSig),\+member(NodoSig,Recorrido),valor(NodoSig,Valor)),Estados),
    sort(2,@>=,Estados,[[NodoSig|Valor2]|_]),
    valor(Actual,Valor1),
    Valor2>=Valor1,
    append([NodoSig],[Actual|Recorrido],Res),!.

% Método que encuentra la solución utilizando la búsqueda Steepest
% Ascent Hill Climbing.
saHillClimbingR([],_,[]):-!.
saHillClimbingR([Fin|Camino],Fin,[Fin|Camino]):-!.
saHillClimbingR(Recorrido,Meta,Final):-
    siguientePos(Recorrido,Res),
    saHillClimbingR(Res,Meta,Final).

% -----------------------------------------------------------------------%

% Best-First search.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Manda llamar al método bestFirstR.
% Al encontrar la solución con bestFirstR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
bestFirst(Inicio,Fin,Solución):-
    bestFirstR([[Inicio]|[]],Fin,Camino),
    reverse(Camino,Solución),!.

% Método que encuentra la solución utilizando best-first search.
% Utiliza el método predefinido findall/3.
% findall/3 se compone de tres argumentos:
% (1) Cómo va a agregar los elementos a la lista.
% (2) Qué condiciones se tienen que cumplir para agregarlos
% (3) La lista resultante.
bestFirstR([],_,[]):-!.
bestFirstR([[Meta|Recorrido]|_],Meta,[Meta|Recorrido]):-!.
bestFirstR([[Actual|Recorrido]|OtrosCaminos],Meta,RecorridoFinal) :-
    findall([Siguiente,Actual|Recorrido],(costo(Actual,Siguiente,_),\+member(Siguiente,Recorrido)),Nuevos),
    append(OtrosCaminos,Nuevos,Todos),
    sortListas(Todos,Óptimo),
    bestFirstR(Óptimo,Meta,RecorridoFinal).

% -----------------------------------------------------------------------%

% Beam search.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y el tamaño
% deseado de opciones abiertas y regresa el camino obtenido.
% Manda llamar al método beamR.
% Al encontrar la solución con beamR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
beam(Inicio,Fin,TamañoB,Solución):-
    beamR([[Inicio]|[]],Fin,TamañoB,Camino),
    reverse(Camino,Solución),!.

% Método utilizado en la beam search.
% Expande todos los recorridos guardados en la lista.
% Utiliza el método predefinido findall/3.
% findall/3 se compone de tres argumentos:
% (1) Cómo va a agregar los elementos a la lista.
% (2) Qué condiciones se tienen que cumplir para agregarlos
% (3) La lista resultante.
siguientes([],[]):-!.
siguientes([[Actual|Recorrido]|OtrosCaminos],TodosNuevos):-
    siguientes(OtrosCaminos,OtrosNuevos),
    findall([Siguiente,Actual|Recorrido],(conexión(Actual,Siguiente),\+member(Siguiente,Recorrido)),UnosNuevos),
    append(UnosNuevos,OtrosNuevos,TodosNuevos).

% Obtiene, para cada recorrido, el costo de pasar al último nodo
% agregado.
fCosto([A,B|Resto],[Costo,A,B|Resto]) :-
    costo(B,A,Costo),!.
fCosto([[A,B|Resto]|C],[[Costo,A,B|Resto]|Cola]) :-
    fCosto(C,Cola),
    costo(B,A,Costo),!.
fCosto([],[]):-!.

% Conserva únicamente los recorridos que ha hecho la búsqueda.
sóloCamino([[_|Camino]|Cola],[Camino|ColaRes]):-
    sóloCamino(Cola,ColaRes).
sóloCamino([],[]):-!.

% Acomoda los recorridos en orden ascendente, de acuerdo al costo de
% cada uno.
% Utiliza el método predefinido sort/4.
% sort/4 acomoda las listas de una lista en el orden deseado.
% 1) Lugar de cada lista en el que se debe guiar para realizar el sort.
% 2) Orden en que se desea acomodar: @<= significa "en orden ascendente
% y mantener repetidos".
% 3) Regresa la lista de listas ordenada.
sortListas(Lista,Sorted):-
    fCosto(Lista,Costos), % Agrega el costo al inicio de cada recorrido.
    sort(1,@=<,Costos,Ordenada), % Acomoda las listas de menor a mayor.
    sóloCamino(Ordenada,Sorted). % Elimina el costo de cada recorrido.

% Método utilizado en la beam search.
% Poda los recorrido menos eficientes, y se queda con los B más
% eficientes.
poda(_,_,[]):-!.
poda(Beam,[Camino|Otros],[Camino|NoPodados]) :-
    Beam>0,!,
    B is Beam-1,
    poda(B,Otros,NoPodados).

% Método utilizado en la beam search.
% Verifica si alguno de los recorridos resulntantes llega al nodo final.
% En caso de que sí, se asegura de que dicho recorrido esté al final de
% la lista, para después, en el método beam, voltear la lista y que
% quede al principio.
verifica([],_,[]):-!.
verifica([[X|Y]|_],X,[[X|Y]]):-!.
verifica([A|B],X,[A|C]):-
    verifica(B,X,C),!.

% Método que encuentra la solución utilizando la búsqueda Beam.
% Encuentra los sucesores de cada recorrido.
% Acomoda los recorrido dependiendo del costo.
% Poda.
% Verifica si alguno de los caminos podados es la solución.
% Por si sí lo es, voltea la lista para que el camino solución quede al
% principio.
beamR([],_,_,[]):-!.
beamR([[Meta|Recorrido]|_],Meta,_,[Meta|Recorrido]):-!.
beamR(Caminos,Meta,Beam,RecorridoFinal) :-
    siguientes(Caminos,Nuevos),
    sortListas(Nuevos,Sorted),
    poda(Beam,Sorted,Restantes),
    verifica(Restantes,Meta,Verificados),
    reverse(Verificados,Recorrido),
    beamR(Recorrido,Meta,Beam,RecorridoFinal).

% -----------------------------------------------------------------------%

% A* search.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Obtiene las coordenadas del nodo inicial y manda llamar al método
% aEstrellaR.
% Al encontrar la solución con aEstrellaR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
aEstrella(Inicio,Fin,Solución):-
    coordenadas(Inicio,Ax,Ay),
    aEstrellaR(Ax,Ay,[[Inicio]|[]],Fin,Camino),
    reverse(Camino,Solución),!.

% Función H().
% Utilizada en el A*.
% Obtiene la distancia en línea recta de pasar del nodo inicial al
% último nodo agregado al recorrido.
h(CXa,CYa,[B|_],Km):-
    coordenadas(B,CXb,CYb), % Obtiene las coordenadas del último nodo visitado.
    Km is (90*(sqrt(((CXb-CXa)**2)+((CYb-CYa)**2)))),!. % Obtiene la distancia.
h(_,_,[],[]):-!.

% Función G().
% Utilizada en el A*.
% Obtiene el costo total del recorrido recorrido sumando los costos de
% pasar a cada nodo visitando.
g([A,B|Resto],Costo):-
    g([B|Resto],Costo1),
    costo(B,A,Costo2),!,
    Costo is Costo1+Costo2.
g([A,B],Costo):-
    costo(B,A,Costo),!.
g([],[]):-!.

% Función F().
% Utilizada en el A*.
% Obtiene, para cada recorrido, su función F()=G()+H().
f(CXa,CYa,[Camino],[[F|Camino]]):-
    g(Camino,G),
    h(CXa,CYa,Camino,H),
    F is G+H,!.
f(CXa,CYa,[Camino|Resto],[[F|Camino]|Cola]):-
    f(CXa,CYa,Resto,Cola),
    g(Camino,G),
    h(CXa,CYa,Camino,H),
    F is G+H,!.
f(_,_,[],[]):-!.

% Método utilizado en A*.
% Acomoda los recorridos en orden ascendente, de acuerdo la función F()
% de cada uno.
% Utiliza el método predefinido sort/4.
% sort/4 acomoda las listas de una lista en el orden deseado.
% 1) Lugar de cada lista en el que se debe guiar para realizar el sort.
% 2) Orden en que se desea acomodar: @<= significa "en orden ascendente
% y mantener repetidos".
% 3) Regresa la lista de listas ordenada.
caminoÓptimo(CXa,CYa,Lista,Óptimo):-
    f(CXa,CYa,Lista,F), % Agrega el valor de F() al inicio de cada recorrido.
    sort(1,@=<,F,Ordenada), % Acomoda las listas de menor a mayor.
    sóloCamino(Ordenada,Óptimo). % Elimina el costo de cada recorrido.

% Método que encuentra la solución utilizando A*.
% Utiliza el método predefinido findall/3.
% findall/3 se compone de tres argumentos:
% (1) Cómo va a agregar los elementos a la lista.
% (2) Qué condiciones se tienen que cumplir para agregarlos
% (3) La lista resultante.
aEstrellaR(_,_,[],_,[]):-!.
aEstrellaR(_,_,[[Meta|Recorrido]|_],Meta,[Meta|Recorrido]):-!.
aEstrellaR(CXa,CYa,[[Actual|Recorrido]|OtrosCaminos],Meta,RecorridoFinal) :-
    findall([Siguiente,Actual|Recorrido],(costo(Actual,Siguiente,_),\+member(Siguiente,Recorrido)),Nuevos),
    append(OtrosCaminos,Nuevos,Todos),
    caminoÓptimo(CXa,CYa,Todos,Óptimo),
    aEstrellaR(CXa,CYa,Óptimo,Meta,RecorridoFinal).

% -----------------------------------------------------------------------%


tiempo :-
   X is cputime,
   Tiempo is X/60,
   nl, write('Este programa lleva funcionando '),
   write(Tiempo), write(' minutos '), write('('),
   write(X), write(' segundos).').
