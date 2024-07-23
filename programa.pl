% Casa Hogwarts
% PARTE 1 - Sombrero Seleccionador

% Contexto: Para determinar en qué casa queda una persona cuando 
% ingresa a Hogwarts, el Sombrero Seleccionador tiene en cuenta: 
% - el carácter de la persona, 
% - lo que prefiere 
% - y en algunos casos su status de sangre.

% caracteristicas mago, status sangre, casa que odiaria

%esMago(harry, mestiza, [corajudo, amistoso, orgulloso, inteligente], slytherin).
%esMago(draco, pura, [inteligente, orgulloso], hufflepuff).
%esMago(hermione, impura, [inteligente, orgulloso, responsable],_).

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).
sangre(neville, pura).    % agregue a neville para ejemplos
sangre(luna, pura).       % agregue a luna para ejemplos  

mago(Mago) :- sangre(Mago,_). % alguien que tiene tipo de sangre, es mago

%mago(harry).
%mago(draco).          % esto seria por extension (MAS LARGO) 
%mago(hermione).

%tieneCaracteristica(Mago, Caracterisitca).
tieneCaracteristica(harry, coraje).
tieneCaracteristica(harry, amistad).
tieneCaracteristica(harry, orgullo).
tieneCaracteristica(harry, inteligencia).

tieneCaracteristica(draco, inteligencia).
tieneCaracteristica(draco, orgullo).

tieneCaracteristica(hermione, inteligencia).
tieneCaracteristica(hermione, orgullo).
tieneCaracteristica(hermione, responsabilidad).

tieneCaracteristica(neville, responsabilidad). % agregue a neville para poder probar ejemplos
tieneCaracteristica(neville, coraje).
tieneCaracteristica(neville, amistad).

tieneCaracteristica(luna, amistad).            % agregue a luna para poder probar ejemplos 
tieneCaracteristica(luna, inteligencia).
tieneCaracteristica(luna, responsabilidad).

%odiariaEntrar(Mago, CasaQueOdiariaEntrar).
odiariaEntrar(harry, slytherin).
odiariaEntrar(draco, hufflepuff).
% como hermione NO odia ninguna casa, entonces no lo agrego!!

% 1) Saber si una casa permite entrar a un mago, lo cual se cumple para cualquier mago 
% y cualquier casa excepto en el caso de Slytherin, que no permite entrar a magos 
% de sangre impura

casa(slytherin).
casa(hufflepuff).
casa(gryffindor).
casa(ravenclaw).

% permiteEntrar(gryffindor, _).
% permiteEntrar(hufflepuff ,_).   REPETICION DE LOGICA, y si agrego una nueva casa tengo que modificar
% permiteEntrar(ravenclaw, _).

%permiteEntrar(Casa,Mago) 
permiteEntrar(Casa, Mago) :-  
    casa(Casa),         % porque tiene que ser una casa de hogwarts (restringo el dominio de la Casa, a solo las casas de hogwarts)
    mago(Mago),         % porque tiene que ser un mago de hogwarts (restringo el dominio de magos)
    Casa \= slytherin.

permiteEntrar(slytherin, Mago) :-
    %mago(Mago) ... --> es al pedo porque ya en sangre se contempla que es un mago
    sangre(Mago,TipoDeSangre),
    TipoDeSangre \= impura.

% 2) Saber si un mago tiene el carácter apropiado para una casa, 
% lo cual se cumple para cualquier mago si sus características 
% incluyen todo lo que se busca para los integrantes de esa casa, 
% independientemente de si la casa le permite la entrada

%caracteristicaBuscada(Casa, Caracteristica).
caracteristicaBuscada(gryffindor,coraje).

caracteristicaBuscada(slytherin, orgullo).
caracteristicaBuscada(slytherin, inteligencia).

caracteristicaBuscada(ravenclaw, inteligencia).
caracteristicaBuscada(ravenclaw, responsabilidad).

caracteristicaBuscada(hufflepuff, amistad).

tieneCaracterApropiado(Mago, Casa) :-
    % todas las caracteristicas que busca la casa, las tiene ese mago
    mago(Mago), % entra ligado al forall (para ese mago en partcular..)
    casa(Casa), % entra ligado al forall (para esta casa en particular..) PERO NO TENGO QUE LIGAR LA CARACTERISTICA (PORQUE ES EL VALOR QUE VA A IR BUSCANDO)
    forall(caracteristicaBuscada(Casa, Caracteristica),
        tieneCaracteristica(Mago,Caracteristica)).
% PARA TODAS las caracteristicas buscadas por ESA CASA, el MAGO tiene esas 

% 3) Determinar en qué casa podría quedar seleccionado un mago 
% sabiendo que tiene que tener el carácter adecuado para la casa, 
% la casa permite su entrada y además el mago no odiaría que lo manden 
% a esa casa. 
% Además Hermione puede quedar seleccionada en Gryffindor, 
% porque al parecer encontró una forma de hackear al sombrero.

puedeQuedarSeleccionadoPara(Mago, Casa) :-      % es una regla (general)
    tieneCaracterApropiado(Mago, Casa),
    permiteEntrar(Casa, Mago),
    not(odiariaEntrar(Mago,Casa)).

puedeQuedarSeleccionadoPara(hermione,gryffindor). % es un hecho (un FACT)

% 4) Definir un predicado cadenaDeAmistades/1 que se cumple para 
% una lista de magos si todos ellos se caracterizan por ser amistosos 
% y cada uno podría estar en la misma casa que el siguiente. 
% No hace falta que sea inversible, se consultará de forma individual.

amistoso(Mago) :- tieneCaracteristica(Mago, amistad). %alguien que tiene la caracteristica de amistad -> es amistoso

cadenaDeAmistades(Magos) :-
    todosAmistosos(Magos),
    cadenaDeCasas(Magos).

todosAmistosos(Magos) :-
    forall(member(Mago, Magos), amistoso(Mago)). % para todo mago que pertenece a lista de magos, ese mago es amistoso

% SI LO QUIERO HACER CON RECURSIVIDAD

%cadenaDeCasas(Magos) :-
cadenaDeCasas([]).  % el caso base (si la lista es vacia)
cadenaDeCasas([_]). % el caso base (si hay un solo elemento en la lista)
cadenaDeCasas([Mago1, Mago2 | MagosSiguientes]) :-
    puedeQuedarSeleccionadoPara(Mago1, Casa),
    puedeQuedarSeleccionadoPara(Mago2, Casa),
    cadenaDeCasas([Mago2 | MagosSiguientes]).

% ----------------------------------------------

% SI LO QUIERO HACER SIN RECURSIVIDAD

cadenaDeCasasV2(Magos) :-
    forall(consecutivos(Mago1, Mago2, Magos), 
    (puedeQuedarEnLaMismaCasa(Mago1, Mago2, _))).

consecutivos(Anterior, Siguiente, Lista) :-
    nth1(IndiceAnterior, Lista, Anterior),
    IndiceSiguiente is IndiceAnterior + 1,
    nth1(IndiceSiguiente, Lista, Siguiente).

puedeQuedarEnLaMismaCasa(Mago1, Mago2, Casa) :-
    puedeQuedarSeleccionadoPara(Mago1, Casa), 
    puedeQuedarSeleccionadoPara(Mago2, Casa),
    Mago1 \= Mago2.
    
% ----------------------------------------------

% PARTE 2 - La copa de las casas

%accionMala(fueraDeCama,50).

%accionMala(LugarProhibido,Puntos).
%accionMala(bosque,50).
%accionMala(seccionBiblioteca,10).
%accionMala(tercerPiso,75).

% hizo(Mago, AccionQueHizo)
hizo(harry, fueraDeCama).

%hizo(hermione, irATercerPiso).                         % sin functor
%hizo(hermione, irASeccionRestringidaBiblioteca).   

hizo(hermione, irA(tercerPiso)).                        % con functores que representan lugares
hizo(hermione, irA(seccionRestringidaBiblioteca)).

hizo(harry, irA(bosque)).
hizo(harry, irA(tercerPiso)).

hizo(draco, irA(mazmorras)).

hizo(ron, buenaAccion(50, ganarAlAjedrezMagioc)).

hizo(hermione, buenaAccion(50, salvarASusAmigos)).

hizo(harry, buenaAccion(60, ganarleAVoldemort)).

%esDe(Alumno, CasaQueQuedoSeleccionado)
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

esAlumno(Alumno) :- esDe(Alumno,_). % que sea de una casa, quiere decir que es alumno

% 1)
% a) Saber si un mago es buen alumno, que se cumple si hizo 
% alguna acción y ninguna de las cosas que hizo se considera 
% una mala acción (que son aquellas que provocan un puntaje negativo).

hizoAlgunaAccion(Mago) :- hizo(Mago,_). % si hizo algo, hizo alguna accion

hizoAlgoMalo(Mago) :-
    hizo(Mago, Accion),
    puntajeQueGenera(Accion, Puntaje),
    Puntaje < 0.

%puntaQueGenera(Accion, Puntaje)
puntajeQueGenera(fueraDeCama, -50). % es un hecho (un fact)

puntajeQueGenera(irA(Lugar), PuntajeQueResta) :-
    lugarProhibido(Lugar, Puntos),
    PuntajeQueResta is Puntos * -1. % porque los Puntos vienen en positivo

%puntajeQueGenera(irA(Lugar), 0) :-  % me genera 0 puntos un lugar que no esta prohibido
%    not(lugarProhibido(Lugar,_)).

puntajeQueGenera(buenaAccion(Puntaje, _), Puntaje). % es un hecho (porque ya le puse el puntaje antes)

%lugarProhibido(Lugar, PuntosQueResta)
lugarProhibido(bosque, 50).
lugarProhibido(seccionRestringidaBiblioteca, 10).
lugarProhibido(tercerPiso, 75).

esBuenAlumno(Mago) :-
    esAlumno(Mago),
    hizoAlgunaAccion(Mago),
    not(hizoAlgoMalo(Mago)).
    %forall(accion(Mago,Accion), accionBuena(Accion)).