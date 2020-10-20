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
          ((and (repetidos Lista) (< (car Lista) N)) (cons (+ (car Lista) 1) (cdr Lista)))
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

;;Dominio: Una lista
;;Codominio: True si hay repetidos en la lista
(define repetidos
 (lambda (Lista)
   (cond ((= (length Lista) 1) #f)
         ((member? (car Lista) (cdr Lista)) #t)
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



;;Autor Eddy Ramirez
;;Obtenido de: https://github.com/eddituss/99Problems/blob/master/Scheme/99SchemeProblems.scm
(define member?
  (lambda (elem l)
    (ormap (lambda (x) (equal? x elem)) l)))

