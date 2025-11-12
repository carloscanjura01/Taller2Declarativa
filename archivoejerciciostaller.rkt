#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TALLER 2 - PROGRAMACIÓN DECLARATIVA
;; Ejercicio 1 – Contar elementos positivos en una lista
;; Usar filter y length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (contar-positivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))

;; Prueba
(displayln (string-append "Ejercicio 1: "
  (number->string (contar-positivos '(3 -2 7 0 -5 9)))
  " elementos positivos."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 2 – Generar lista de cuadrados pares
;; Usar map y filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cuadrados-pares lista)
  (map (lambda (x) (* x x))
       (filter even? lista)))

;; Prueba
(displayln (string-append "Ejercicio 2: "
  (format "~a" (cuadrados-pares '(1 2 3 4 5 6 7 8)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 3 – Calcular el factorial de un número
;; Recursión simple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

;; Prueba
(displayln (string-append "Ejercicio 3: "
  (number->string (factorial 5))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 4 – Elevar cada número al cubo
;; Usar lambda con map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cubos lista)
  (map (lambda (x) (* x x x)) lista))

;; Prueba
(displayln (string-append "Ejercicio 4: "
  (format "~a" (cubos '(2 3 4)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 5 – Sumar todos los elementos impares
;; Usar filter + foldl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (suma-impares lista)
  (foldl + 0 (filter odd? lista)))

;; Prueba
(displayln (string-append "Ejercicio 5: "
  (number->string (suma-impares '(1 2 3 4 5 6 7)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 6 – Determinar si una lista contiene negativos
;; Usar ormap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (contiene-negativo? lista)
  (ormap (lambda (x) (< x 0)) lista))

;; Prueba
(displayln (string-append "Ejercicio 6: "
  (format "~a" (contiene-negativo? '(5 9 -3 2)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 7 – Calcular la suma acumulada de una lista
;; Usar foldl para crear acumuladores personalizados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (suma-acumulada lista)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lista)))

;; Prueba
(displayln (string-append "Ejercicio 7: "
  (format "~a" (suma-acumulada '(1 2 3 4)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 8 – Concatenar cadenas de texto en una lista
;; Usar foldl con cadenas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (concatenar-cadenas lista)
  (foldl string-append "" lista))

;; Prueba
(displayln (string-append "Ejercicio 8: "
  (concatenar-cadenas '("Hola" " " "Mundo"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 9 – Doble de números mayores que 5
;; Usar filter y map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (doble-mayores-que-5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

;; Prueba
(displayln (string-append "Ejercicio 9: "
  (format "~a" (doble-mayores-que-5 '(3 6 8 2 10)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 10 – Invertir el orden de una lista
;; Usar foldl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

;; Prueba
(displayln (string-append "Ejercicio 10: "
  (format "~a" (invertir-lista '(1 2 3 4)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 11 – Función que recibe otra función
;; Funciones de orden superior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (aplicar-funcion f lista)
  (map f lista))

;; Prueba con función cuadrado
(displayln (string-append "Ejercicio 11: "
  (format "~a" (aplicar-funcion (lambda (x) (* x x)) '(1 2 3 4)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejercicio 12 – Reto integrador
;; Calcular promedio de los números mayores a 5
;; Usar map, filter y foldl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (promedio-mayores-que-5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (suma (foldl + 0 mayores))
         (n (length mayores)))
    (if (zero? n)
        0
        (/ suma n))))

;; Prueba
(displayln (string-append "Ejercicio 12: "
  (number->string (promedio-mayores-que-5 '(3 8 10 4 9 2 7)))))
