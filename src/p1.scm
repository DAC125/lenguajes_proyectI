;;voy agregando nnumeros ynverificando qie sirvan, o sea que las reinas no se choquen
;; si alguna cocha,que devuvo a la anterior(backtracking), si no, sumo a a lista nueros hasta que
;; lleguen a N, cuando llego a N, le sumo hasta que llegue a 5, si no

;;A[i] /= A[j]    i/=j    i,j∈[1,k]
;;|A[i]-A[j]|/=|i- j|    i/=j   i,j∈[1,k]

;;(define A '(6 3 7 1 3 5 8 4));;(2 4 1 3)valida::::2 5 3 1 4->5=2 invalida

(define largo
  (lambda (Lista)
    (cond ((null? Lista) 0)
          (else (+ 1 (largo (cdr Lista)))))))

;;(define L (largo A))

;;llamada principal
(define backtrackingNReinas
  (lambda (A B);;a = #N de reinas ^ b = #sol soluciones ^ k= contador soluciones
    (backtracking-aux A B 0 '() '(1))));; primera lista: respuestas;; segunda lista: lo que voy modificando

;;aca se hace el  back
(define backtracking-aux
 (lambda (N sol K resp temp);;temp= respuesta temporal
   (cond ;;((null? temp) K)
     ((= sol 0) 0)
         ((<= sol K) resp);; o devuelvo soluciones? o K
         ((and (= (largo temp) N) (solucion temp)) (backtracking-aux N sol (+ K 1) (cons temp resp) (siguiente temp N)));;sumar 1 a temp
         ((= (largo temp) N)  (backtracking-aux N sol K resp (siguiente temp N)))
         ((solucion temp) (backtracking-aux N sol K resp (siguiente temp N)))
         (else (backtracking-aux N sol K resp (siguiente temp N))))));;n antes era (largo temp)

;;devule el siguiente cuando la lista esta llena o cuando ultimo numero es = N, el backtracking
(define siguiente;; puede ser un anterior a la solucion no solo cuando le faltan elementos;
  (lambda (Lista N);;este siguiente es del back tracking cuando la lista tiene long N
    (cond ((null? Lista) '());;linea de abajo:::(reverse (cdr (reverse Lista))) en vez del append
          ((> (car Lista) N) '())
          ((and (= (largo Lista) N) (>= (car (reverse Lista)) N)) (siguiente (append (reverse (cddr (reverse Lista))) (list (+ (car (cdr (reverse Lista))) 1))) N));;((and (= (largo Lista) N) (<= (car (reverse Lista)) N)) (cons (+ (car Lista) 1) (cdr Lista)))
          ((= (largo Lista) N) (append (reverse (cdr (reverse Lista))) (list (+ (car (reverse Lista)) 1))));;((= (largo Lista) N) (siguiente (cdr Lista) N))
          ((<= (car (reverse Lista)) N) (append Lista '(1)));;((< (car Lista) N) (cons (+ (car Lista) 1) (cdr Lista)))
          ((and (>= (car (reverse Lista)) N) (= (largo Lista) 2)) (siguiente (list (+ (car Lista) 1)) N))
          ((>= (car (reverse Lista)) N) (siguiente (append (reverse (cddr (reverse Lista))) (list (+ (car (cdr (reverse Lista))) 1))) N)))));; o devolver lista sin car +1;;((< (car Lista) N) (cons (+ (car Lista) 1) (cdr Lista)))
;;agregar en el ultmo el return de una vez
;;penultima linea (reverse (cdr (reverse Lista))) (list (+ (car (reverse Lista)) 1))
;;ultima (siguiente (reverse (cdr (reverse Lista))) N)

;;devuelve el nsimo elemento
(define nsimo
  (lambda (Lista pos)
    (cond ((null? Lista) '())
      ((= pos 1) (car Lista))
          (else (nsimo (cdr Lista) (- pos 1))))))


;;+++++++++++++++++++++
(define repetido;;true cuando no hay repetidos
  (lambda (Lista)
    (cond ((null? Lista) #t)
          ((member? (car Lista) (cdr Lista)) #f)
          (else (repetido (cdr Lista))))))
(define member?;;https://stackoverflow.com/questions/48548524/scheme-member-function
  (lambda (x Lista)
    (cond ((null? Lista) #f)
      ((eq? x (car Lista)) #t)
      (else (member? x (cdr Lista))))))


(define solucion
 (lambda (Lista)
   (cond ((= (largo Lista) 1) #t)
     ((and (repetido Lista) (solucion-aux Lista 1 2 (largo Lista))) #t);;sol aux verifica diagonales repetido horizontal
         (else #f))))

;;http://cic.puj.edu.co/wiki/lib/exe/fetch.php?id=materias%3Alenguajes3%3Alenguajes_iii&cache=cache&media=materias:lenguajes3:backtracking.pdf
;;solución
(define solucion-aux
 (lambda (Lista N M Largo);;n-> inicio; M recorre depende de N, es para verificar todas; L largo de la lista
   (cond ((equal? N Largo) #t);;(- L 1);;((= Lista ))
         ((= Largo 1) #t)
         ((= M (+ 1 Largo)) (solucion-aux Lista (+ N 1) (+ 1 (+ N 1)) Largo))
         ((equal? (abs (- (nsimo Lista M) (nsimo Lista N))) (abs (- N M))) #f)
         (else (solucion-aux Lista N (+ M 1) Largo)))))
;;(equal? (nsimo Lista N) (nsimo Lista L));;; chequea horizontal
;;deltaRow = abs(Q1 row - Q2 row)
;;deltaCol = abs(Q1 col - Q2 col)
;;If deltaRow == deltaCol, the queens are on the same diagonal.
;;Just do that for all N queens.
