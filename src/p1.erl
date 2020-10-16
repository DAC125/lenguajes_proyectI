-module(p1).
-export([geneticosNReinas/1, generaPoblacion/2]). 

geneticosNReinas(1) -> [1];
geneticosNReinas(N) -> main(N,4,400,generaPoblacion(generaLista(N),N*4)).

main(N,Muestra,_Cruce,P) -> seleccion_respuesta(generacion(P, N, N*Muestra),P).

generaLista(N) when 0 >= N -> [];
generaLista(N) -> [N|generaLista(N-1)].

generaPoblacion(_L,CantPoblacion) when 0 >= CantPoblacion -> [];
generaPoblacion(L,CantPoblacion)->[shuffleList(L)|generaPoblacion(L,CantPoblacion-1)].

shuffleList(L) -> [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

generacion(_Poblabcion, _N, 0)-> [];
generacion([H|T], N, CantPoblacion)-> [fitnes1(H)|generacion(T,N,CantPoblacion-1)].

fitnes1([H|T]) -> fitnes1(H,T,0,0,false,0).
fitnes1(_Val,[], _I, _J, _Cond, Apt) -> Apt;
fitnes1(Val,[H|T], I, J, _Cond, Apt)when I=:=J -> fitnes1(Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Apt);
fitnes1(Val,[H|T], I, J, Cond, Apt) when Cond =:= true-> fitnes1(Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Apt+1);
fitnes1(Val,[H|T], I, J, _Cond, Apt) -> fitnes1(Val, T, I, J+1, condi_fitness(abs((I+1)-(J+1)) , abs(Val-H)), Apt).

condi_fitness(A,B) when A =:= B-> true;
condi_fitness(_A,_B) -> false.

seleccion_respuesta(L,P)->[[L]|[P]].

