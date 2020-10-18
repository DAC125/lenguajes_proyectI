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
         ((<= sol K) K);;Cambiar K to resp para ver las respuestas
         ((null? temp) K)
         ((and (= (largo temp) N) (solucion temp)) (backtracking-aux N sol (+ K 1) resp (siguiente temp N)));;resp->;;(cons temp resp)arriba para ver respuestas
         ((= (largo temp) N) (backtracking-aux N sol K resp (siguiente temp N)))
         (else (backtracking-aux N sol K resp (siguiente temp N))))))

;;Dominio: Una lista con elementos entre 1 y N(Cantidad de Reinas)
;;Codominio: True->Ninguna reina choca con otra || False->Hay choque
(define solucion
 (lambda (Lista)
   (cond ((= (largo Lista) 1) #t);;Lista tamaño 1 es solución parcial
     ((and (repetido Lista) (solucion-aux Lista 1 2 (largo Lista))) #t)
         (else #f))))

;;Obtenido de :http://cic.puj.edu.co/wiki/lib/exe/fetch.php?id=materias%3Alenguajes3%3Alenguajes_iii&cache=cache&media=materias:lenguajes3:backtracking.pdf
;;Dominio:Lista->Lista a probar ,N=1->Primera Reina; M=2-> No probar las mismas pos, Largo->Largo de la lista con x reinas
;;Codominio: True-> No choca en las diagonales || False-> chocan en alguna diagonal
;;Nota: Solo prueba Diagonales
(define solucion-aux
 (lambda (Lista N M Largo)
   (cond ((equal? N Largo) #t)
         ((= Largo 1) #t)
         ((= M (+ 1 Largo)) (solucion-aux Lista (+ N 1) (+ 1 (+ N 1)) Largo))
         ((equal? (abs (- (nsimo Lista M) (nsimo Lista N))) (abs (- N M))) #f)
         (else (solucion-aux Lista N (+ M 1) Largo)))))

;;Dominio: Una Lista y un número(N). N->Cantidad de reinas
;;Codominio: El siguiente elemento en el árbol de posibilidaddes->Tomando en cuenta restricciones
;;Ejemplos: (1)->(1,1)||(1,4,2)->(1,4,2,1)||EL largo no pasa a N
(define siguiente
  (lambda (Lista N)
    (cond ((null? Lista) '())
          ;;Hay repetidos y el último es menor que la las filas->Solo le sumo uno al ultimo elemento
          ((and (not (repetido Lista)) (< (car (reverse Lista)) N)) (append (reverse (cdr (reverse Lista))) (list (+ (car (reverse Lista)) 1))))
          ;;Cuando toda la lista contiene elementos == N;llega un N+1
          ((> (car Lista) N) '())
          ;;Lista mismo tamaño N y último elemento mayor igual a N -> Se llama recursivamente sumando 1 al penúltimo elemento
          ((and (= (largo Lista) N) (>= (car (reverse Lista)) N)) (siguiente (append (reverse (cddr (reverse Lista))) (list (+ (cadr (reverse Lista)) 1))) N))
          ;;Lista mismo tamaño N, Se suma 1 al ulimo elemento
          ((= (largo Lista) N) (append (reverse (cdr (reverse Lista))) (list (+ (car (reverse Lista)) 1))))
          ;;último elemento menor o igual a N -> Se le agrega un 1 a la Lista
          ((<= (car (reverse Lista)) N) (append Lista '(1)))
          ;;Último de la Lista mayor a N y Lista tamaño 2, Los anteiores no sirven por el uso de cddr, ->último "car" + 1 
          ((and (> (car (reverse Lista)) N) (= (largo Lista) 2)) (siguiente (list (+ (car Lista) 1)) N))
          ;;Último de la Lista mayor a N -> Llamo recursivamente sumando uno al penúltimo
          ((>= (car (reverse Lista)) N) (siguiente (append (reverse (cddr (reverse Lista))) (list (+ (car (cdr (reverse Lista))) 1))) N)))))
;;agregar en el ultmo el return de una vez (3,2,1,6,_)5

;;Dominio: Una Lista y un número(pos)
;;Codominio: El elemento en pos
(define nsimo
  (lambda (Lista pos)
    (cond ((null? Lista) '())
      ((= pos 1) (car Lista))
          (else (nsimo (cdr Lista) (- pos 1))))))

;;Dominio:Una Lista
;;Codominio: True-> Si no hay repetidos en toda la lista || False->Cuando hay repetidos
(define repetido
  (lambda (Lista)
    (cond ((null? Lista) #t)
          ((member? (car Lista) (cdr Lista)) #f)
          (else (repetido (cdr Lista))))))

;;Obtenido de: https://stackoverflow.com/questions/48548524/scheme-member-function
;;Dominio:un elemento, y una lista
;;Codominio: True-> si el elemento se encuentra en la lista || False si no
(define member?
  (lambda (x Lista)
    (cond ((null? Lista) #f)
      ((eq? x (car Lista)) #t)
      (else (member? x (cdr Lista))))))

;;Dominio: Lista
;;Codominio: Largo de la Lista
(define largo
  (lambda (Lista)
    (cond ((null? Lista) 0)
          (else (+ 1 (largo (cdr Lista)))))))