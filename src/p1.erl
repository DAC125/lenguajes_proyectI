-module(p1).
-export([geneticosNReinas/1]). %Mala practica pero permitido por la tarea

geneticosNReinas(1) -> [1];
geneticosNReinas(N) -> main(N,4,400).

main(N,Muestra,_Mutacion) -> generaPoblacion(generaLista(N),N*Muestra).


generaPoblacion(_L,N)when 0>=N -> [];
generaPoblacion(L,N)->[shuffleList(L)|generaPoblacion(L,N-1)].



generaLista(N)when 0>=N -> [];
generaLista(N)->[N|generaLista(N-1)].


shuffleList(L)->[X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].