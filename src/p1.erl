-module(p1).
-export([geneticosNReinas/1, generaPoblacion/2]). 

geneticosNReinas(N)when N<4 -> ["no existen respuestas para N = "| N];
geneticosNReinas(N) -> main(N,4,400,generaPoblacion(generaLista(N),N*4)).

main(N,Muestra,Cruce,P) -> seleccion_respuesta(generacion(P, N, N*Muestra),P,N,N*Muestra,Cruce).

generaLista(N) when 0 >= N -> [];
generaLista(N) -> [N|generaLista(N-1)].

generaPoblacion(_L,CantPoblacion) when 0 >= CantPoblacion -> [];
generaPoblacion(L,CantPoblacion)->[shuffleList(L)|generaPoblacion(L,CantPoblacion-1)].

shuffleList(L) -> [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

generacion(_Poblabcion, _N, 0)-> [];
generacion([H|T], N, CantPoblacion)-> [fitnes(H)|generacion(T,N,CantPoblacion-1)].

fitnes([],_I,Col)-> Col;
fitnes([H|T],I,Col) -> fitnes(T,H,T,I+1,0,true,Col).
fitnes([H|T])-> fitnes(T,H,T,0,0,true,0).

fitnes(TCromo,_Val,[], I, _J, _Cond, Col) -> fitnes(TCromo,I,Col);
fitnes(TCromo,Val,[H|T], I, J, _Cond, Col)when I=:=J -> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col);
fitnes(TCromo,Val,[H|T], I, J, Cond, Col) when Cond =:= true-> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col+1);
fitnes(TCromo,Val,[H|T], I, J, _Cond, Col) -> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col).



condi_fitness(A,B) when A =:= B-> true;
condi_fitness(_A,_B) -> false.




seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],N,CantPoblacion,Cruce) -> seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],[_Col|TCol],[_Cromo|Tcromo],N,CantPoblacion,Cruce).

seleccion_respuesta([],[],Colisiones,Poblacion,N,CantPoblacion,Cruce)-> ciclo_cruce(Poblacion, Colisiones,N,CantPoblacion,Cruce);
seleccion_respuesta([Col|_TCol],[Cromo|_Tcromo],_Colisiones,_Poblacion,_N,_CantPoblacion,_Cruce)when Col =:= 0 -> Cromo;
seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],Colisiones,Poblacion,N,CantPoblacion,Cruce) -> seleccion_respuesta(TCol,Tcromo,Colisiones,Poblacion,N,CantPoblacion,Cruce).


ciclo_cruce(Poblacion,_Colisiones,_N,_CantPoblacion,_Cruce) -> Poblacion.

encuentraElite([H|_T],Val) when Val=:=H -> H;
encuentraElite([_H|T],Val) -> encuentraElite(T,Val).

encuentraPadre(_Poblacion,N)->N.

cruce(Poblacion,_Elite,_Padre,_Madre) ->Poblacion.