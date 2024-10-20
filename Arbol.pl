/* padres(H,P,M,A) <- H tiene como padre a P y como madre a M, y nació el año A */
/* padres(H,P,M,A) <- H tiene como padre a P y como madre a M, y nació el año A */

padres('Don Modesto',p1,m1,1950).
padres('Doña Aurora',p2,m2,1955).
padres('Alberto',p3,m3,1986).
padres('Veronica', 'Don Modesto', 'Doña Aurora', 1985).
padres('Roberto', 'Don Modesto', 'Doña Aurora', 1980).
padres('Elideth', 'Don Modesto', 'Doña Aurora', 1988).
padres('Edsson', 'Alberto', 'Veronica', 2003).
padres('Denisse', 'Alberto', 'Veronica', 2009).
padres('Victor Jr', 'Victor', 'Elideth', 2000).
padres('Mayra', 'Victor', 'Elideth', 2004).
padres('Emmanuel', 'Victor', 'Elideth', 2010).
padres('Roberta', 'Roberto', 'Perla', 2020).

/* casados(H,M) <- El hombre H está casado con la mujer M */
casados('Don Modesto', 'Doña Aurora').
casados('Alberto', 'Veronica').
casados('Victor', 'Elideth').
casados('Roberto', 'Perla').

/* hombre(P) <- la persona P es del género masculino */
hombre('Don Modesto').
hombre('Roberto').
hombre('Alberto').
hombre('Victor').
hombre('Edsson').
hombre('Victor Jr').
hombre('Emmanuel').

/* mujer(P) <- la persona P es del género femenino */
mujer('Doña Aurora').
mujer('Veronica').
mujer('Elideth').
mujer('Perla').
mujer('Denisse').
mujer('Mayra').
mujer('Roberta').


/*------ Reglas ------*/
/* edad(P,E) <- la persona P tiene E años en el año de referencia */
edad(P,E) :- padres(P,_,_,A), 
    AnioRef is 2024, 
    E is AnioRef - A.

/* mayor(P1,P2) <- la persona P1 es mayor que P2 */
mayor(P1,P2) :- padres(P1,_,_,A1), 
                padres(P2,_,_,A2), 
                A1 < A2.

/* niño(P) <- P es un niño (menos de 14 años) */
ninyo(P) :- edad(P,E), 
            E =< 14.

/* joven(P) <- P es una persona joven (entre 14 y 25 años) */
joven(P) :- edad(P,E), 
            E > 14, E =< 25.

/* adulto(P) <- P es un adulto (entre 25 y 50 años) */
adulto(P) :- edad(P,E), 
             E > 25, E =< 50.

/* viejo(P) <- P es una persona vieja (más de 50 años) */
viejo(P) :- edad(P,E), 
            E > 50.

/* hermanos(H1,H2) <- H1 es hermano/a de H2 */
hermanos(H1, H2) :- padres(H1, P, M, _), padres(H2, P, M, _), H1 \= H2.

/* tio(T, S) <- T es el tio de S */
tio(T, S) :- hombre(T), padres(S, P, _, _), hermanos(T, P).
tio(T, S) :- hombre(T), padres(S, _, M, _), hermanos(T, M).
tio(T, S) :- hombre(T), padres(S, P, _, _), hermanos(T1, P), casados(T, T1).
tio(T, S) :- hombre(T), padres(S, _, M, _), hermanos(T1, M), casados(T, T1).

/* tia(T, S) <- T es la tia de S */
tia(T, S) :- mujer(T), padres(S, P, _, _), hermanos(T, P).
tia(T, S) :- mujer(T), padres(S, _, M, _), hermanos(T, M).
tia(T, S) :- mujer(T), padres(S, P, _, _), hermanos(T1, P), casados(T1, T).
tia(T, S) :- mujer(T), padres(S, _, M, _), hermanos(T1, M), casados(T1, T).

/* primos(P1, P2) <- P1 es primo/a de P2 */
primos(P1, P2) :- padres(P1, PA1, MA1, _), padres(P2, PA2, MA2, _), (hermanos(PA1, PA2); hermanos(PA1, MA2); hermanos(MA1, PA2); hermanos(MA1, MA2)).

/* abuelo(A, N) <- A es el abuelo de N */
abuelo(A, N) :- padres(N, P, M, _), (padres(P, A, _, _); padres(M, A, _, _)).

/* abuela(A, N) <- A es la abuela de N */
abuela(A, N) :- padres(N, P, M, _), (padres(P, _, A, _); padres(M, _, A, _)).


