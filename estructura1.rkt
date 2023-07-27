; Codigo por Felipe Alexander Correa Rodríguez
; Programaremos en racket distintos tipos de estructuras de datos:

; Listas enlazadas

#lang racket
; Se define una lista enlazada:
(define-struct ubi (dato lasiga))

; Agregar un elemento a la lista
(define (agregar-ubi lista dato)
  (cond
    [(empty? lista) (ubi dato empty)]
    [else (ubi (ubi-dato lista) (agregar-ubi (ubi-lasiga lista) dato))]))

; Eliminar un elemento de la lista
(define (eliminar-ubi lista dato)
  (cond
    [(empty? lista) empty]
    [(equal? (ubi-dato lista) dato) (ubi-lasiga lista)]
    [else (ubi (ubi-dato lista) (eliminar-ubi (ubi-lasiga lista) dato))]))

; Buscar un elemento en la lista
(define (buscar-ubi lista dato)
  (cond
    [(empty? lista) false]
    [(equal? (ubi-dato lista) dato) true]
    [else (buscar-ubi (ubi-lasiga lista) dato)]))

; Imprimir la lista
(define (imprimir-ubi lista)
  (cond
    [(empty? lista) (displayln "")]
    [else (begin
            (display (ubi-dato lista))
            (display " ")
            (imprimir-ubi (ubi-lasiga lista)))]))


; --------------------------------------------------------------------------------
; --------------------------------------------------------------------------------
; Ejemplos de testeo:

; Crear una lista vacia
(define lista empty)

; Agregar un elemento a la lista
(agregar-ubi lista 5)

; Eliminar un elemento de la lista
(eliminar-ubi lista 3)

; Buscar un elemento en la lista
(buscar-ubi lista 3)

; Imprimir la lista
(imprimir-ubi lista)

; --------------------------------------------------------------------------------
; --------------------------------------------------------------------------------