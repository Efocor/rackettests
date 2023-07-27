; Se crea  un lenguaje de programación que permita seguir esta estructura/sintaxis:
; x = 1;
; y = 2;
; z = x + y;
; grafar ('El valor de z es: ', z)

; Además aceptará otras operaciones y funciones. Tendrá su parser y su intérprete.

#lang racket

;; Definición del ambiente de variables como un diccionario mutable
(define ambiente (make-hash))

;; Función para evaluar una expresión en el ambiente dado
(define (evaluar expresion)
  (cond
    ((number? expresion) expresion)
    ((symbol? expresion) (buscar expresion))
    ((pair? expresion) (aplicar (car expresion) (cdr expresion)))
    (else (error "Expresión desconocida" expresion))))

;; Función para buscar una variable en el ambiente
(define (buscar variable)
  (let ((valor (hash-ref ambiente variable #f)))
    (if (eq? valor #f)
        (error "Variable no definida" variable)
        valor)))

;; Función aplicar que procesa las operaciones y funciones
(define (aplicar procedimiento argumentos)
  (cond
    ((eq? procedimiento '+) (aplicar-suma argumentos))
    ((eq? procedimiento '-) (aplicar-resta argumentos))
    ((eq? procedimiento '*) (aplicar-multiplicacion argumentos))
    ((eq? procedimiento '/) (aplicar-division argumentos))
    ((eq? procedimiento 'grafar) (aplicar-grafar argumentos))
    ((eq? procedimiento 'asignar) (aplicar-asignar argumentos))
    ((eq? procedimiento 'if) (aplicar-if argumentos))
    ((eq? procedimiento 'lambda) (aplicar-lambda argumentos))
    ((eq? procedimiento 'quote) (aplicar-quote argumentos))
    (else (error "Procedimiento desconocido" procedimiento))))

;; Operaciones matemáticas
(define (aplicar-suma argumentos)
  (apply + (map evaluar argumentos)))

(define (aplicar-resta argumentos)
  (if (null? argumentos)
      (error "La función resta requiere al menos un argumento")
      (apply - (map evaluar argumentos))))

(define (aplicar-multiplicacion argumentos)
  (apply * (map evaluar argumentos)))

(define (aplicar-division argumentos)
  (if (null? argumentos)
      (error "La función división requiere al menos un argumento")
      (apply / (map evaluar argumentos))))

;; Otras funciones predefinidas
(define (aplicar-grafar argumentos)
  (grafar (apply format #f (map evaluar argumentos))))

;; Asignación de variables al ambiente
(define (aplicar-asignar argumentos)
    (hash-set! ambiente (car argumentos) (evaluar (cadr argumentos))))

;; Condicional if
(define (aplicar-if argumentos)
  (if (evaluar (car argumentos))
      (evaluar (cadr argumentos))
      (evaluar (caddr argumentos))))

;; Expresiones lambda
(define (aplicar-lambda argumentos)
  (let ((parametros (car argumentos))
        (cuerpo (cdr argumentos)))
    (lambda parametros cuerpo)))

;; Función para evaluar una expresión sin evaluar su contenido (quote)
(define (aplicar-quote argumentos)
  (car argumentos))

;; Lista de funciones y operaciones predefinidas
(define funciones-predefinidas
  `((+ ,aplicar-suma)
    (- ,aplicar-resta)
    (* ,aplicar-multiplicacion)
    (/ ,aplicar-division)
    (grafar ,aplicar-grafar)
    (asignar ,aplicar-asignar)
    (if ,aplicar-if)
    (lambda ,aplicar-lambda)
    (quote ,aplicar-quote)))

;; Agregar funciones predefinidas al ambiente
(for-each (lambda (def) (hash-set! ambiente (car def) (cdr def))) funciones-predefinidas)

;; Función para imprimir una lista de valores
(define (grafar . args)
  (for-each display args)
  (newline))

;; Función para asignar un valor a una variable x por ejemplo: (asignar x 1) = (define x 1)
(define (asignar variable valor)
  (hash-set! ambiente variable valor))

;; Ejemplo de uso
(asignar 'x 1)
(define y 2)
(define z (+ x y))
(grafar "El valor de z es: " z)

;; Ejemplo de asignación y uso de una función lambda
(define (cuadrado x) (* x x))
(define (doble x) (+ x x))
(define resultado (cuadrado (doble 3)))
(grafar "Resultado: " resultado)