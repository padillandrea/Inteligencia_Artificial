/* Elaborado por:
 * 1) Andrea Carolina Padilla Rodríguez, 166605
 *  padillandrea.ap@gmail.com
 * 2) Ivana Lucho Beltrán, 167028
 *  ivanalucho@gmail.com
 */

% Base de conocimiento:
leeDatos:-
    csv_read_file('Vuelos.csv',Vuelos,[functor(vuelo),arity(9)]),maplist(assert,Vuelos).

% Imprime la ruta resultante:
%imprimir.

% mejor_ruta(Inicio,Fin):- encuentra_ruta(rutas(Inicio,Fin,Ruta)),write(Ruta).
% mejor_ruta(Inicio,Fin):- jerarquia(Inicio,Fin,J),


% -----------------------------------------------------------------------%
% A* search.
% Recibe el nodo de inicio, el nodo al que se quiere llegar y
% regresa el camino obtenido.
% Obtiene las coordenadas del nodo inicial y manda llamar al método
% aEstrellaR.
% Al encontrar la solución con aEstrellaR, regresa el dicho camino al
% revés, para así comenzar con el nodo inicial y terminar con el nodo
% meta.
aEstrella(Inicio,Fin):-
    %leeDatos,
    aEstrellaR([[Inicio]|[]],Fin,Camino),
    reverse(Camino,Solución),
    write(Solución),!.

% Función H().
% Utilizada en el A*.
% Obtiene el tiempo total de la ruta recorrida sumando las horas y
% minutos de los vuelos.
h([A,B|Resto],Tiempo):-
    h([B|Resto],Tiempo1),
    vuelo(_,B,_,_,_,_,_,A,_,_,_,_,_,_,Tiempo2,_,_),!,
    Tiempo is Tiempo1+Tiempo2.
h([A,B],Tiempo):-
    vuelo(_,B,_,_,_,_,_,A,_,_,_,_,_,_,Tiempo,_,_),!.
h([],[]):-!.

% Función G().
% Utilizada en el A*.
% Obtiene el costo total de la ruta recorrida sumando los costos de los
% vuelos.
g([A,B,X|Resto],Costo):-
    g([B|Resto],Costo1),
    vuelo(X,B,_,A,_,_,_,_,Costo2),!,
    Costo is Costo1+Costo2.
g([A,B,X],Costo):-
    vuelo(X,B,_,A,_,_,_,_,Costo),!.
g([],[]):-!.

% Función F().
% Utilizada en el A*.
% Obtiene, para cada recorrido, su función F()=G()+H().
f([Camino],[[F|Camino]]):-
    g(Camino,G),
    h(Camino,H),
    F is G*H,!.
f([Camino|Resto],[[F|Camino]|Cola]):-
    f(Resto,Cola),
    g(Camino,G),
    h(Camino,H),
    F is G*H,!.
f([],[]):-!.

% Conserva únicamente los recorridos que ha hecho la búsqueda.
sóloCamino([[_|Camino]|Cola],[Camino|ColaRes]):-
    sóloCamino(Cola,ColaRes).
sóloCamino([],[]):-!.

% Método utilizado en A*.
% Acomoda los recorridos en orden ascendente, de acuerdo la función F()
% de cada uno.
% Utiliza el método predefinido sort/4.
% sort/4 acomoda las listas de una lista en el orden deseado.
% 1) Lugar de cada lista en el que se debe guiar para realizar el sort.
% 2) Orden en que se desea acomodar: @<= significa "en orden ascendente
% y mantener repetidos".
% 3) Regresa la lista de listas ordenada.
caminoÓptimo(Lista,Óptimo):-
    f(Lista,F), % Agrega el valor de F() al inicio de cada recorrido.
    sort(1,@=<,F,Ordenada), % Acomoda las listas de menor a mayor.
    sóloCamino(Ordenada,Óptimo). % Elimina el costo de cada recorrido.

% Método que encuentra la solución utilizando A*.
% Utiliza el método predefinido findall/3.
% findall/3 se compone de tres argumentos:
% (1) Cómo va a agregar los elementos a la lista.
% (2) Qué condiciones se tienen que cumplir para agregarlos
% (3) La lista resultante.
aEstrellaR([],_,[]):-!.
aEstrellaR([[Meta|Recorrido]|_],Meta,[Meta|Recorrido]):-!.
aEstrellaR([[Origen|Recorrido]|OtrosCaminos],Meta,RecorridoFinal) :-
    findall([Destino,Origen,AL|Recorrido],(vuelo(AL,Origen,_,Destino,_,_,_,_,_),\+member([Destino,_,AL],Recorrido)),Nuevos),
    append(OtrosCaminos,Nuevos,Todos),
    caminoÓptimo(Todos,Óptimo),
    aEstrellaR(Óptimo,Meta,RecorridoFinal).
% -----------------------------------------------------------------------%
