-module(p1).
-export([geneticosNReinas/1, generaPoblacion/2, cruce/7, generaLista/1, cortaPadre/3, encuentraPadre/2, encuentraElite/2, busquedaByValue/3, generacion/3, cicloMutacion/4, mutacion/3, ciclo_cruce/5, mutaCromosoma/3,mutaCromosoma2/4] ). 

%funcion principal dispara el proceso
geneticosNReinas(N)when N<4 -> ["no existen respuestas para N = "| N]; %si el N entrada es menor que 4 se retorna no hay respuesta
geneticosNReinas(N) -> main(N,4,700,generaPoblacion(generaLista(N),N*4),N,0.05).

%funcion main para setear variables de funcionamiento del algoritmo (Muestras,Cruces,PorMuta)
main(N,Muestra,Cruce,P,N,PorMuta) -> seleccion_respuesta(generacion(P, N, N*Muestra),P,N,N*Muestra,Cruce,PorMuta).

%funcion que genera una lista ordenada del 1 al N ingresado 
generaLista(N) when 0 >= N -> [];
generaLista(N) -> [N|generaLista(N-1)].

%funcion que genera una lista del uno al N desordenada
generaPoblacion(_L,CantPoblacion) when 0 >= CantPoblacion -> [];
generaPoblacion(L,CantPoblacion)->[shuffleList(L)|generaPoblacion(L,CantPoblacion-1)].

%funcion que desordena aleatoriamente una lista
shuffleList(L) -> [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

%Genera lista de valores de fitnes, entre menor sea el valor de colision mejor sera el individuo
generacion(_Poblabcion, _N, 0)-> [];
generacion([H|T], N, CantPoblacion)-> [fitnes(H)|generacion(T,N,CantPoblacion-1)].


%funcion fitnes califica a un cromosoma por numero de coliciones diagonales y verticales (- coliones + apto)
fitnes([],_I,Col)-> Col;
fitnes([H|T],I,Col) -> fitnes(T,H,T,I+1,0,true,Col).
fitnes([H|T])-> fitnes(T,H,T,0,0,true,0).

fitnes(TCromo,_Val,[], I, _J, _Cond, Col) -> fitnes(TCromo,I,Col);
fitnes(TCromo,Val,[H|T], I, J, _Cond, Col)when I=:=J -> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col);
fitnes(TCromo,Val,[H|T], I, J, Cond, Col) when Cond =:= true-> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col+1);
fitnes(TCromo,Val,[H|T], I, J, _Cond, Col) -> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col).


%funcion que retorna T si los ds valores entrantes son iguales si no F
condi_fitness(A,B) when A =:= B-> true;
condi_fitness(_A,_B) -> false.


%recorre la poblacion si el valor de colision es 0 retorna ek resultado como solucion, si no ejecuta otra generacion hasta teminar con el numero de cruces si no se encuentra se retorna el valor elite de la ultima poblacion  
seleccion_respuesta(_Colisiones,Poblacion,_N,_CantPoblacion,0,_PorMuta)-> ["NO"|Poblacion];
seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],N,CantPoblacion,Cruce,PorMuta) -> seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],[_Col|TCol],[_Cromo|Tcromo],N,CantPoblacion,Cruce,PorMuta).

seleccion_respuesta([],[],Colisiones,Poblacion,N,CantPoblacion,Cruce,PorMuta)-> ciclo_cruce(cicloMutacion(cruce(Poblacion,busquedaByValue(Poblacion,Colisiones,lists:min(Colisiones)),cortaPadre(encuentraPadre(Poblacion,rand:uniform(N)),N div 2,[]),encuentraPadre(Poblacion,rand:uniform(N)),[],CantPoblacion,N),round(CantPoblacion*PorMuta-1),CantPoblacion,N),N,CantPoblacion,Cruce-1,PorMuta);
seleccion_respuesta([Col|_TCol],[Cromo|_Tcromo],_Colisiones,_Poblacion,_N,_CantPoblacion,_Cruce,_PorMuta)when Col =:= 0 -> ["SOLUCION"|Cromo];
seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],Colisiones,Poblacion,N,CantPoblacion,Cruce,PorMuta) -> seleccion_respuesta(TCol,Tcromo,Colisiones,Poblacion,N,CantPoblacion,Cruce,PorMuta).

busquedaByValue([PH|_PT],[CH|_CT],Val) when CH =:= Val->PH;
busquedaByValue([_PH|PT],[_CH|CT],Val)-> busquedaByValue(PT,CT,Val).


ciclo_cruce(Poblacion,N,CantPoblacion,Cruce,PorMuta) -> seleccion_respuesta(generacion(Poblacion,N,CantPoblacion),Poblacion,N,CantPoblacion,Cruce,PorMuta).

encuentraElite([H|_T],Val) when Val=:=H -> H;
encuentraElite([_H|T],Val) -> encuentraElite(T,Val).

encuentraPadre([PH|_PT],0) -> PH;
encuentraPadre([_PH|PT],N)-> encuentraPadre(PT,N-1).

cruce(_Poblacion,_Elite,_Padre,_Madre,NuevaPoblacion,0,_N) -> NuevaPoblacion;%,round(CantPoblacion*0.05),length(CantPoblacion),N) ;
cruce(Poblacion,Elite,Padre,Madre,[],CantPoblacion,N) -> cruce(Poblacion,Elite,Padre,Madre,[Elite],CantPoblacion-1,N);
cruce(Poblacion,Elite,Padre,Madre,NuevaPoblacion,CantPoblacion,N) -> cruce(Poblacion,Elite,cortaPadre(encuentraPadre(Poblacion,rand:uniform(N))	,N div 2,[]),encuentraPadre(Poblacion,rand:uniform(N)),[Padre++Madre--Padre|NuevaPoblacion],CantPoblacion-1,N).

cicloMutacion(Poblacion,0,_CantPoblacion,_N) -> Poblacion;
cicloMutacion(Poblacion,CantMutaciones,CantPoblacion,N) -> cicloMutacion(mutacion(Poblacion,rand:uniform(CantPoblacion),N),CantMutaciones-1,CantPoblacion,N).


mutacion([_PH|PT], Indice, N) -> mutacion(PT,Indice,[],N). 
mutacion([PH|PT], 0,NuevaPoblacion,N) -> NuevaPoblacion++[mutaCromosoma(PH,rand:uniform(N),rand:uniform(N))]++[PH|PT];
mutacion([PH|PT], Indice,NuevaPoblacion,N) -> mutacion(PT,Indice-1,[PH|NuevaPoblacion],N).


mutaCromosoma(Cromosoma, 1, J) -> mutaCromosoma2(Cromosoma,[lists:nth(J,Cromosoma)]++lists:nthtail(1,Cromosoma)--lists:nthtail(J-1,Cromosoma),1,J);
mutaCromosoma(Cromosoma, I, 1) -> mutaCromosoma2(Cromosoma,[lists:nth(I,Cromosoma)]++lists:nthtail(1,Cromosoma)--lists:nthtail(I-1,Cromosoma),1,I);
mutaCromosoma(Cromosoma, I, J) when I<J -> mutaCromosoma2(Cromosoma,lists:sublist(Cromosoma,I-1)++[lists:nth(J,Cromosoma)]++lists:nthtail(I,Cromosoma)--lists:nthtail(J-1,Cromosoma),I,J);
mutaCromosoma(Cromosoma, I, J) when J<I -> mutaCromosoma2(Cromosoma,lists:sublist(Cromosoma,J-1)++[lists:nth(I,Cromosoma)]++lists:nthtail(J,Cromosoma)--lists:nthtail(I-1,Cromosoma),J,I);
mutaCromosoma(Cromosoma, I, I) -> mutaCromosoma(Cromosoma,I,I+1).
mutaCromosoma2(Cromosoma, NCromosoma, I, J) -> NCromosoma++[lists:nth(I,Cromosoma)]++lists:nthtail(J,Cromosoma).

cortaPadre(_Padre,0,DivPa) -> DivPa;
cortaPadre([H|T],I,DivPa) -> cortaPadre(T,I-1,[H|DivPa]).

