;;Primer proyecto Lenguajes N-Reinas Genético, y Backtracking
;;Ricardo Murillo, Ronald Esquivel, Diego Acuña

;;Dominio: A->Número de Reinas a colocar
;;Codominio: B Número de soluciones a buscar
(define backtrackingNReinas
  (lambda (A B)
    (backtracking-aux A B 0 '() '(1))));;0 Respuestas contadas, '(): respuestas, '(1): lo que voy modificando

;;Dominio: N->Número de Reinas, B->Soluciones a buscar, K->Soluciones encontradas, Resp->Respuestas encontradas, temp->Lista que se modifica
;;Codominio:Resp: temp->Lista de listas con las respuestas || K->numero de respuestas encontradas
(define backtracking-aux
 (lambda (N sol K resp temp)
   (cond ((= sol 0) 0)
         ((<= sol K) resp)
         ((null? temp) K)
         ((repetidos temp) (backtracking-aux N sol K resp (siguiente temp N)))
         ((< (largo temp) N) (backtracking-aux N sol K resp (siguiente temp N)))
         ((solucion temp) (backtracking-aux N sol (+ K 1) (cons temp resp) (siguiente temp N)))
         (else (backtracking-aux N sol K resp (siguiente temp N))))))

;;Dominio: Una lista con elementos entre 1 y N(Cantidad de Reinas)
;;Codominio: True->Ninguna reina choca con otra || False->Hay choque
(define solucion
 (lambda (Lista)
   (cond ((= (largo Lista) 1) #t);;Lista tamaño 1 es solución parcial
         ((solucion-aux Lista 1 2 (largo Lista)) #t);;(and (repetido Lista) 
         (else #f))))

;;Obtenido de :http://cic.puj.edu.co/wiki/lib/exe/fetch.php?id=materias%3Alenguajes3%3Alenguajes_iii&cache=cache&media=materias:lenguajes3:backtracking.pdf
;;Dominio:Lista->Lista a probar ,N=1->Primera Reina; M=2-> No probar las mismas pos, Largo->Largo de la lista con x reinas
;;Codominio: True-> No choca en las diagonales || False-> chocan en alguna diagonal
;;Nota: Solo prueba Diagonales
(define solucion-aux
 (lambda (Lista N M Largo)
   (cond ((= N Largo) #t)
         ((= Largo 1) #t)
         ((= M (+ 1 Largo)) (solucion-aux Lista (+ N 1) (+ 1 (+ N 1)) Largo))
         ((= (abs (- (nsimo Lista M) (nsimo Lista N))) (abs (- N M))) #f)
         (else (solucion-aux Lista N (+ M 1) Largo)))))

;;Dominio: Una Lista y un número(N). N->Cantidad de reinas
;;Codominio: El siguiente elemento en el árbol de posibilidaddes->Tomando en cuenta restricciones
;;Ejemplos: (1)->(1,1)||(1,4,2)->(1,4,2,1)||EL largo no pasa a N
(define siguiente
  (lambda (Lista N)
    (cond ((null? Lista) '())
          ;;Hay repetidos y el último es menor que la las filas->Solo le sumo uno al ultimo elemento;;++
          ((and (repetidos Lista) (< (car Lista) N)) (cons (+ (car Lista) 1) (cdr Lista)));;(append (remove-last Lista) (list (+ (last Lista) 1))))
          ;;Cuando toda la lista contiene elementos == N;llega un N+1
          ((> (car (reverse Lista)) N) '())
          ;;Lista mismo tamaño N y último elemento mayor igual a N -> Se llama recursivamente sumando 1 al penúltimo elemento
          ((and (= (largo Lista) N) (>= (car Lista) N)) (siguiente (cons (+ (cadr Lista) 1)  (cddr Lista)) N))
          ;;Lista mismo tamaño N, Se suma 1 al ulimo elemento
          ((= (largo Lista) N) (cons (+ (car Lista) 1)  (cdr Lista)))
          ;;primer elemento menor o igual a N -> Se le agrega un 1 a la Lista
          ((<= (car Lista) N) (cons 1 Lista));;
          ;;Último de la Lista mayor a N y Lista tamaño 2, Los anteiores no sirven por el uso de cddr, ->último "car" + 1 
          ((and (> (car Lista) N) (= (largo Lista) 2)) (siguiente (cons 1 (list (+ (cadr Lista) 1))) N))
          ;;Último de la Lista mayor a N -> Llamo recursivamente sumando uno al penúltimo
          ((>= (car Lista) N) (siguiente (cons (+ (cadr Lista) 1)  (cddr Lista)) N)))))

;;Dominio: Una Lista y un número(pos)
;;Codominio: El elemento en pos
(define nsimo;;(nth '(3 4) 0)
  (lambda (Lista pos)
    (cond ((null? Lista) '())
      ((= pos 1) (car Lista))
          (else (nsimo (cdr Lista) (- pos 1))))))

;;Dominio:Una Lista
;;Codominio: True-> Si hay repetidos en toda la lista || False->Cuando NO hay repetidos
(define repetidos
  (lambda (Lista)
    (cond ((null? Lista) #f)
          ((miembro? (car Lista) (cdr Lista)) #t)
          (else (repetidos (cdr Lista))))))

;;Obtenido de: https://stackoverflow.com/questions/48548524/scheme-member-function
;;Dominio:un elemento, y una lista
;;Codominio: True-> si el elemento se encuentra en la lista || False si no
(define miembro?
  (lambda (x Lista)
    (cond ((null? Lista) #f)
      ((= x (car Lista)) #t)
      (else (miembro? x (cdr Lista))))))

;;Dominio: Lista
;;Codominio: Largo de la Lista
(define largo
  (lambda (Lista)
    (cond ((null? Lista) 0)
          (else (+ 1 (largo (cdr Lista)))))))

;;Dominio: Una lista
;;Codominio: La lista sin el ultimo elemento
(define remove-last
  (lambda (Lista)
    (cond ((null? (cdr Lista)) '())
          (else  (cons (car Lista) (remove-last (cdr Lista)))))))

(define remove-2-last
  (lambda (Lista)
    (cond ((null? (cddr Lista)) '())
          (
           else  (cons (car Lista) (remove-2-last (cdr Lista)))))))

(define my-but-last
  (lambda (lista)
    (cond ((null? lista) '())
          ((null? (cddr lista)) (car lista))
          (else (my-but-last (cdr lista))))))



(define prueba
  (lambda (A)
    (cond ((null? A) '())
          ((cond ((< (largo A) 5) #f)
                ((= A '(1 2)) #f)) #t)
          (else '(2)))))
 

(define repetidos2
 (lambda (Lista)
   (cond ((= (length Lista) 1) #f)
   (else (repetidos2-aux Lista 1 2 (length Lista))))))

(define repetidos2-aux
  (lambda (Lista N M Largo)
    (cond 
         ((= M (+ 1 Largo)) (repetidos2-aux (+ N 1) (+ 1 (+ N 1)) Largo))
         ((= (abs (- (nsimo Lista M) (nsimo Lista N))) (abs (- N M))) #f)
         (else (solucion-aux Lista N (+ M 1) Largo)))))









;;https://pdf.sciencedirectassets.com/277811/1-s2.0-S1877042813X00163/1-s2.0-S1877042813011853/main.pdf?X-Amz-Security-
;;Token=IQoJb3JpZ2luX2VjENz%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLWVhc3QtMSJHMEUCIG8rFiTFMVtJqx3wbjHPIMI8vxuYqEX6FqcPfWfftz
;WpAiEAr0LyO7HAnARfFLk2aogfatW%2Fb63Nk9s8CnVqwnvT2gQqtAMIJRADGgwwNTkwMDM1NDY4NjUiDFQSuDUsGHDCOEV6ISqRA1a%2Fvt757NTXt5I2uuCq
;;ueqeV%2F5pFcu3LCNcNYbh86K%2Fht5l5UboXNKVeCBTkyJk%2BeIgPMZ8O3LMT8m58PGCVRNCC%2BqibuDON%2FPlh8rALBPNGD%2Fv0tS5mOTlN4liQ7C0C
;;jdNRkAwgQ9LKQbXDgZivg01vY5WrAZSGv%2FiCm2oml8CnllUcoge0jhkxO28D6KbscU2oG9krrIBvtxOCZokDFaYc3NI1e8We4vZguRSuDX6zEDSkzc%
;;Ef%2BDom9hUH%2FclTwUYbcpu8j6lN8%2BLNX2E49nHg1jsEjsjbD33Nw%3D%3D&X-Amz-Algorithm=AWS4-HMAC
;;-SHA256&X-Amz-Date=20201018T043013Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTY5T2HAVUU%
;;2F20201018%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=2c99b56f61bfe1d62e00107489e97f15c0beb4724b0b7ab54681c5a2085
;;93560&hash=54148dcc01d36b036aa2ce7e72d9b183253f62d951c9df2145fd92aea8007a23&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S1877042813011853&tid=spdf-286210de-c736-455c-942b-e4499f360840&sid=d3c5ed0c307a66438879e8125014dcec96b2gxrqa&type=client


;;http://cic.puj.edu.co/wiki/lib/exe/fetch.php?id=materias%3Alenguajes3%3Alenguajes_iii&cache=cache&media=materias:lenguajes3:backtracking.pdf
;;https://www.etsisi.upm.es/sites/default/files/asigs/arquitecturas_avanzadas/practicas/MPI/nreinas.pdf
;;https://www.cs.buap.mx/~zacarias/FZF/nreinas3.pdf
