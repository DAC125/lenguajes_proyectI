-module(p1).
-export([geneticosNReinas/1, generaPoblacion/2,  generaLista/1, cortaPadre/3, encuentraPadre/2,  busquedaByValue/3, generacion/3, cicloMutacion/4, mutacion/3, ciclo_cruce/5, mutaCromosoma/3,mutaCromosoma2/4, fitnes/1, cruza/4] ). 

%funcion principal dispara el proceso
geneticosNReinas(N)when N<4 -> ["no existen respuestas para N = "| N]; %si el N entrada es menor que 4 se retorna no hay respuesta
geneticosNReinas(N) -> main(N,7,1000,generaPoblacion(generaLista(N),N*7),N,0.1).


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
fitnes([H|T],I,Col) -> fitnes(T,H,T,I+1,I+2,false,Col).
fitnes([H|T])-> fitnes(T,H,T,0,1,false,0).

fitnes(TCromo,_Val,[], I, _J, _Cond, Col) -> fitnes(TCromo,I,Col);
fitnes(TCromo,Val,[H|T], I, J, _Cond, Col)when I=:=J -> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col);
fitnes(TCromo,Val,[H|T], I, J, Cond, Col) when Cond =:= true-> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col+1);
fitnes(TCromo,Val,[H|T], I, J, _Cond, Col) -> fitnes(TCromo,Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Col).


%funcion que retorna T si los ds valores entrantes son iguales si no F
condi_fitness(A,B) when A =:= B-> true;
condi_fitness(_A,_B) -> false.


%recorre la poblacion si el valor de colision es 0 retorna ek resultado como solucion, si no ejecuta otra generacion hasta teminar con el numero de cruces si no se encuentra se retorna el valor elite de la ultima poblacion  
seleccion_respuesta(Colisiones,Poblacion,_N,_CantPoblacion,0,_PorMuta)-> ["Cromosoma con menos colisiones"|busquedaByValue(Poblacion,Colisiones,lists:min(Colisiones))];
seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],N,CantPoblacion,Cruce,PorMuta) -> seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],[_Col|TCol],[_Cromo|Tcromo],N,CantPoblacion,Cruce,PorMuta).


seleccion_respuesta([],[],Colisiones,Poblacion,N,CantPoblacion,Cruce,PorMuta)-> ciclo_cruce(cruza(Poblacion,busquedaByValue(Poblacion,Colisiones,lists:min(Colisiones)),CantPoblacion,N),N,CantPoblacion,Cruce-1,PorMuta);
seleccion_respuesta([Col|_TCol],[Cromo|_Tcromo],_Colisiones,_Poblacion,_N,_CantPoblacion,_Cruce,_PorMuta)when Col =:= 0 -> ["SOLUCION"|Cromo];
seleccion_respuesta([_Col|TCol],[_Cromo|Tcromo],Colisiones,Poblacion,N,CantPoblacion,Cruce,PorMuta) -> seleccion_respuesta(TCol,Tcromo,Colisiones,Poblacion,N,CantPoblacion,Cruce,PorMuta).


busquedaByValue([PH|_PT],[CH|_CT],Val) when CH =:= Val->PH;
busquedaByValue([_PH|PT],[_CH|CT],Val)-> busquedaByValue(PT,CT,Val).


encuentraPadre([PH|_PT],0) -> PH;
encuentraPadre([_PH|PT],N)-> encuentraPadre(PT,N-1).


cortaPadre(_Padre,0,DivPa) -> DivPa;
cortaPadre([H|T],I,DivPa) -> cortaPadre(T,I-1,[H|DivPa]).


ciclo_cruce(Poblacion,N,CantPoblacion,Cruce,PorMuta) -> seleccion_respuesta(generacion(Poblacion,N,CantPoblacion),Poblacion,N,CantPoblacion,Cruce,PorMuta).


cruza(Poblacion,Elite,CantPoblacion,N) -> cruza(Poblacion,Elite,CantPoblacion-1,[Elite],cortaPadre(encuentraPadre(Poblacion,rand:uniform(N-1)),N div 2,[]), encuentraPadre(Poblacion,rand:uniform(N-1)),N).

cruza([PH|PT], Elite, CantPoblacion, [],Padre,Madre,N) -> cruza([PH|PT],Elite,CantPoblacion-1,[Elite],cortaPadre(encuentraPadre([PH|PT],rand:uniform(N-1)),N div 2,[]), encuentraPadre([PH|PT],rand:uniform(N-1)),N);
cruza(Poblacion,Elite,0,NuevaPoblacion,Padre,Madre,N) ->  cicloMutacion(NuevaPoblacion,round(length(NuevaPoblacion)*0.1),length(NuevaPoblacion),N);
cruza([PH|PT], Elite, CantPoblacion, NuevaPoblacion,Padre,Madre,N) -> cruza([PH|PT],Elite,CantPoblacion-1,NuevaPoblacion++[Padre++Madre--Padre],cortaPadre(encuentraPadre([PH|PT],rand:uniform(N-1)),N div 2,[]), encuentraPadre([PH|PT],rand:uniform(N-1)),N).


cicloMutacion(Poblacion,0,CantPoblacion,N) -> Poblacion;
cicloMutacion(Poblacion,CantMutaciones,CantPoblacion,N) -> cicloMutacion(mutacion(Poblacion,rand:uniform(CantPoblacion-1),N),CantMutaciones-1,CantPoblacion,N).



 	
mutacion(Poblacion, Indice,N) when Indice =:= length(Poblacion)-> lists:sublist(Poblacion, Indice-1)++[mutaCromosoma(lists:nth(Indice, Poblacion),rand:uniform(N),rand:uniform(N))];
mutacion(Poblacion, Indice,N) -> lists:sublist(Poblacion, Indice-1)++[mutaCromosoma(lists:nth(Indice, Poblacion),rand:uniform(N),rand:uniform(N))]++lists:nthtail(Indice,Poblacion).



mutaCromosoma(Cromosoma, 1, J) -> mutaCromosoma2(Cromosoma,[lists:nth(J,Cromosoma)]++lists:nthtail(1,Cromosoma)--lists:nthtail(J-1,Cromosoma),1,J);
mutaCromosoma(Cromosoma, I, 1) -> mutaCromosoma2(Cromosoma,[lists:nth(I,Cromosoma)]++lists:nthtail(1,Cromosoma)--lists:nthtail(I-1,Cromosoma),1,I);
mutaCromosoma(Cromosoma, I, J) when I<J -> mutaCromosoma2(Cromosoma,lists:sublist(Cromosoma,I-1)++[lists:nth(J,Cromosoma)]++lists:nthtail(I,Cromosoma)--lists:nthtail(J-1,Cromosoma),I,J);
mutaCromosoma(Cromosoma, I, J) when J<I -> mutaCromosoma2(Cromosoma,lists:sublist(Cromosoma,J-1)++[lists:nth(I,Cromosoma)]++lists:nthtail(J,Cromosoma)--lists:nthtail(I-1,Cromosoma),J,I);
mutaCromosoma(Cromosoma, J, J) when J =:= length(Cromosoma) -> mutaCromosoma(Cromosoma,J,J-1);
mutaCromosoma(Cromosoma, I, I) -> mutaCromosoma(Cromosoma,I,I+1).
mutaCromosoma2(Cromosoma, NCromosoma, I, J) -> NCromosoma++[lists:nth(I,Cromosoma)]++lists:nthtail(J,Cromosoma).





