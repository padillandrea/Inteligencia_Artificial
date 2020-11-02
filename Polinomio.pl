/* Elaborado por:
 * 1) Andrea Carolina Padilla Rodríguez, 166605
 *  padillandrea.ap@gmail.com
 * 2) Ivana Lucho Beltrán, 167028
 *  ivanalucho@gmail.com
 * 3) Emiliano Aguilar Belmont, 166636
 *  emiliano.aguilar678@gmail.com
 */

/* Nuestro programa utiliza polinomios instanciados como listas.
 * Los elementos de la lista son los coeficientes del polinomio, de manera que el primer coeficiente
 * equivale a x^0, el segundo a x^1, y así sucesivamente.
 * Todas las operaciones se realizan con este formato.
 * Una restricción es que no permite exponentes negativos, ya que, como se mencionó,
 * el exponente mínimo para cada ax^b es 0.
 */

% Hecho utilizado para "instanciar" los polinomios.
polinomio(P,P).

% Suma: Método recursivo.
% Recibe dos listas, Pol1 y Pol2.
% Escribe el polinomio resultante en la lista [ARes|BRes].
% Suma los coeficientes cuyos exponentes sean iguales.
suma([],[],[]):-!. % Se detiene si las tres listas son vacías.
suma([],Pol2,Pol2):-!. % Se detiene si la primera lista es vacía.
suma(Pol1,[],Pol1):-!. % Se detiene si la segunda lista es vacía.
suma([A1|B1],[A2|B2],[ARes|BRes]):-
    suma(B1,B2,BRes), % Llamadas recursivas para extraer los coeficientes.
    ARes is A1+A2. % La cabeza de la lista resultante es igual a la suma de los coeficientes de grados iguales.

% Resta: Método recursivo.
% Recibe dos listas, Pol1 y Pol2.
% Escribe el polinomio resultante en la lista [ARes|BRes].
% Resta los coeficientes cuyos exponentes sean iguales.
resta([],[],[]):-!. % Se detiene si las tres listas son vacías.
resta(Pol1,[],Pol1):-!. % Se detiene si la segunda lista es vacía.
resta([],[A2|B2],[ARes|BRes]):- % Si la primera lista ya se vació, pero la segunda sigue teniendo elementos,
    resta([],B2,BRes), % hace las llamadas recursivas para extraer los coeficientes, y
    ARes is 0-A2. % el negativo de los coeficientes restantes de Pol2 los agrega a la lista resultante.
resta([A1|B1],[A2|B2],[ARes|BRes]):-
    resta(B1,B2,BRes), % Llamadas recursivas para extraer los coeficientes.
    ARes is A1-A2. % La cabeza de la lista resultante es igual a la resta de los coeficientes de grados iguales.

% Multiplicación de polinomios: Método recursivo.
% Recibe dos listas, Pol1 y [A|B].
% Escribe el polinomio resultante en ResP.
% Ejecuta la multiplicación de polinomios, realizando la multiplicación
% del primer polinomio por los coeficientes del segundo, y sumando el
% resultado de dichos productos.
prod([],[],[]):-!. % Se detiene si las tres listas son vacías.
prod([],_,[]):-!. % Se detiene si la primera lista es vacía, sin importar lo que exista en la segunda.
prod(_,[],[]):-!. % Se detiene si la segunda lista es vacía, sin importar lo que exista en la primera.
prod(Pol1,[A|B],ResP):-
    prod(Pol1,B,Res), % Llamadas recursivas para extraer los coeficientes del segundo polinomio.
    multS(Pol1,A,PolS), % Manda llamar al método que multiplica el primer polinomio por los coeficientes del segundo.
    suma(PolS,[0|Res],ResP). % Suma los coeficientes de los polinomios resultantes cuyos grados son iguales.

% Multiplicación por escalares: Método recursivo.
% Recibe dos variables: [A|B] (la lista que representa al polinomio 1) y
% Num (el coeficiente del segundo polinomio para realizar el producto).
% Escribe el polinomio resultante en [ARes|BRes].
multS([],_,[]):-!. % Se detiene si el primer polinomio es vacío, sin importar el coef. del 2° polinomio.
multS(_,[],[]):-!. % Se detiene si no existe coeficiente del segundo polinomio, sin importar el primer polinomio.
multS([],[],[]):-!. % Se detiene si no hay argumentos en el método.
multS([A|B],Num,[ARes|BRes]):-
    ARes is A*Num, % Multiplica cada coeficiente del 1er pol. por el del segundo, y agrega el resultado a la nueva lista.
    multS(B,Num,BRes). % Realiza la llamada recursiva con el resto del 1er polinomio.

% Composición: Método recursivo.
% Compuesto por tres parámetros: la primera lista representa a P(x), la
% segunda representa a Q(X) y la tercera es el resultado de P(Q(x)).
comp([],_,[0]):-!. % Se detiene si la primera lista es vacía y el resultado es el polinomio cero.
comp([A|B],Y,Res):-
    comp(B,Y,X), % Llamadas recursivas para extraer los coeficientes de P(x).
    prod(X,Y,Mult), % Según el método de Horner, evalúa Q(x) en P(x). Para esto, primero realiza la composición de Q(x) en
    suma([A],Mult,Res). % P(x) y después suma los coeficientes cuyos grados sean iguales.

% Evaluación de polinomio: Método recursivo.
% Recibe el polinomio y el número a evaluar en P(x).
% El resultado, el cual es un sólo número, se escribirá en Res.
evalua([],_,0). % Se detiene si la lista es vacía, sin importar el número a evaluar.
evalua([A|B],X,Res):-
    evalua(B,X,Res1), % Realiza las llamadas recursivas para extraer los coeficientes del polinomio.
    Res is Res1*X+A. % Según el método de Horner, se multiplica el valor por el acumulador y se le suma el coeficiente.

% Derivar: Método recursivo.
% De este método existen dos versiones:
% La primera sólo cuenta con dos parámetros: el polinomio a derivar y el
% polinomio resultante. La sobrecarga se compone de tres: primero el
% grado del polinomio, después la lista a derivar y al final el
% resultado de la diferenciación.
deriva([],[]):-!. % Se detiene cuando ambas listas son vacías.
deriva([],_):-!. % Se detiene si la primera lista es vacía, sin importar el contenido de la segunda.
deriva(Pol,Res):-
    deriva(0,Pol,Res),!. % Llama a la sobrecarga, iniciando con el exponente 0.
deriva(_,[],[]):-!. % Se detiene si la segunda y tercera lista son vacías, sin importar el primer parámetro.
deriva(0,[_|B],Res):-
    deriva(1,B,Res).  % Llamada recursiva para separar la cola.
deriva(Grado,[A|B],[ARes|BRes]):-
    GradoAux is Grado+1, % Aumenta el grado en 1.
    deriva(GradoAux,B,BRes), % Llamada recursiva con el nuevo valor del grado, y la cola del polinomio.
    ARes is A*Grado. % Aquí se efectúa la derivación; se multiplica el coeficiente por su exponente y se agrega a la lista.

% Imprimir: Método recursivo.
% Existen dos versiones de este método:
% 1) imprime/1: Sólo recibe la lista. Escribe el primer coeficiente del
% polinomio y manda a llamar al método que escribe el resto del mismo,
% en el mismo formato que el código en Java.
% 2) imprime/2: Recibe la lista y el grado del coeficiente a imprimir.
% Distingue si el coeficiente es mayor, igual o menor a cero, y hace lo
% mismo con el exponente.
imprime([]). % Se detiene si la lista es vacía.
imprime([A|B]):-
    length([A|B],Tamaño), % Regresa en "Tamaño" el número de elementos de la lista.
    Tamaño=:=1-> % "if" que representa: "Si la lista sólo tiene un elemento".
    write(A);  % Si tamaño=1, escribe el único coeficiente de la lista.
    (reverse([A|B],MayorAMenor),escribe1C(MayorAMenor)). % Si tamaño no es 1, ordena la lista de mayor a menor grado y manda llamar al método que escribe el primero coeficiente.
imprime([],_). % Se detiene si la lista es vacía.
imprime([A|B],T):-
    X is T-1,Neg is A*(-1), % "X" es el exponente correspondiente al coeficiente. "Neg" es el negativo del coeficiente.
    A=\=0 -> % "if" que representa: "Si el coeficiente no es 0".
    (X=:=0 -> % Si coef no es cero, "if" que representa: "Si el exponente es 0".
    (A>0 -> (format(' + ~w',A), % Si exp=0, "if" que representa: "Si coef>0" escribe '+ coef'...
             imprime(B,X)); % ...y realiza la llamada recursiva, ahora con la cola.
    (format(' - ~w',Neg),imprime(B,X))); % Si coef<0, escribe '- coef' y realiza la llamada recursiva, ahora con la cola.
    (A<0 -> % Si exp no es 0, "if" que representa: "Si coef<0".
    (X>1 -> (format(' - ~wx^',Neg),write(X), % Si coef<0, "if" que representa: "Si el exp>1" escribe '- ax^b'...
             imprime(B,X)); % ...y realiza la llamada recursiva, ahora con la cola.
    (format(' - ~wx',Neg),imprime(B,X))); % Si coef<0 & exp=1, escribe '- ax' y realiza, con la cola, la llamada recursiva.
    (X>1 -> (format(' + ~wx^',A),write(X), % Si coef>0, "if" que representa: "Si el exp>1" escribe '+ ax^b'...
             imprime(B,X)); % ...y realiza la llamada recursiva, ahora con la cola.
    (format(' + ~wx',A),imprime(B,X))))); % Si coef>0 & exp=1, escribe '+ ax' y realiza, con la cola, la llamada recursiva.
    (X is T-1,imprime(B,X)). % Si coef es 0, realiza la llamada recursiva, ahora con la cola del polinomio.

% Método auxiliar para escribir el primer ax^b del polinomio. Además,
% elimina los ceros que puedan existir al principio de la lista.
% Recibe la lista en orden de mayor a menor exponente.
escribe1C([A|B]):-
    length(B,T), % Calcula el grado del polinomio.
    A=\=0 -> % "if" que representa: "Si el primer coeficiente no es 0".
    (T>1 -> % "if" que representa: "Si el grado es mayor a 1".
    (format('~wx^',A),write(T), % Si grado>1, escribe ax^b y...
     imprime(B,T)); % ...manda a llamar al método que imprime el resto del polinomio.
    (format('~wx',A), % Si grado<1, escribe ax y...
     imprime(B,T))); % ...manda a llamar al método que imprime el resto del polinomio.
    escribe1C(B). % Si el primer coeficiente es 0, se ejecuta el mismo método con el resto de la lista.

%Main para correr el programa.
main:-
    % Zero = 0.
    % Salida: zero(x) = 0.
    polinomio([0],Zero),
    write('zero(x)     = '),
    imprime(Zero),nl,

    % El polinomio P es el resultado de P1+P2+P3+P4.
    % Salida: p(x) = 4x^3 + 3x^2 + 2x + 1.
    polinomio([0,0,0,4],P1), % P1 = 4x^3.
    polinomio([0,0,3],P2), % P2 = 3x^2.
    polinomio([1],P3), % P3 = 1.
    polinomio([0,2],P4), % P4 = 2x.
    suma(P1,P2,SumaP1P2), % SumaP1P2 = P1 + P2.
    suma(SumaP1P2,P3,SumaP1P2P3), % SumaP1P2P3 = SumaP1P2 + P3.
    suma(SumaP1P2P3,P4,P), % P = SumaP1P2P3 + P4.
    write('p(x)        = '),
    imprime(P),nl,

    % El polinomio Q es el resultado de Q1+Q2.
    % Salida: q(x) = 3x^2 + 5.
    polinomio([0,0,3],Q1), % Q1 = 3x^2.
    polinomio([5],Q2), % Q2 = 5.
    suma(Q1,Q2,Q), % Q = Q1 + Q2.
    write('q(x)        = '),
    imprime(Q),nl,

    % R = P + Q.
    % Salida: p(x) + q(x) = 4x^3 + 6x^2 + 2x + 6.
    suma(P,Q,R),
    write('p(x) + q(x) = '),
    imprime(R),nl,

    % S = P * Q.
    % Salida: p(x) * q(x) = 12x^5 + 9x^4 + 26x^3 + 18x^2 + 10x + 5.
    prod(P,Q,S),
    write('p(x) * q(x) = '),
    imprime(S),nl,

    % T = P(Q).
    % Salida: p(q(x)) = 108x^6 + 567x^4 + 996x^2 + 586.
    comp(P,Q,T),
    write('p(q(x))     = '),
    imprime(T),nl,

    % A = Zero - P.
    % Salida: 0 - p(x) = -4x^3 - 3x^2 - 2x - 1.
    resta(Zero,P,A),
    write('0 - p(x)    = '),
    imprime(A),nl,

    % B = P(3).
    % Salida: p(3) = 142.
    evalua(P,3,B),
    write('p(3)        = '),
    write(B),nl,

    % C = P'.
    % Salida: p'(x) = 12x^2 + 6x + 2.
    deriva(P,C),
    write("p'(x)       = "),
    imprime(C),nl,

    % D = C' = P''.
    % Salida: p''(x) = 24x + 6.
    deriva(C,D),
    write("p''(x)      = "),
    imprime(D).



