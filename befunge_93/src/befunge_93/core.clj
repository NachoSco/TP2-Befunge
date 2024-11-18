(ns befunge-93.core
    (:require [clojure.string :as str]
      [clojure.java.io :as io]))

;--------------- Leer el archivo, Buscarlo en el equipo, crear la matriz y llenarla------------------------

(defn leer-archivo [ruta]
      (try
        (let [contenido (slurp ruta)
              contenido-limpio (str/replace contenido #"\r" "")]
             (println contenido-limpio)
             contenido-limpio)
        (catch Exception e
          (println "Error al leer el archivo:" (.getMessage e)))))

(defn crear-matriz-vacia []
      (vec (repeat 25 (vec (repeat 80 \space))))) ; Crea una matriz 25x80 llena de espacios

(defn llenar-matriz [matriz contenido]
      (loop [matriz matriz
             chars (seq contenido)
             fila 0
             columna 0]
            (if (empty? chars)
              matriz ; Aquí retornamos la última versión de la matriz actualizada
              (let [char (first chars)]
                   (if (= char \newline)
                     ;; Salto de línea: avanza a la siguiente fila y reinicia columna
                     (recur matriz (rest chars) (inc fila) 0)
                     ;; Inserta el carácter en la posición actual
                     (let [nueva-fila (assoc (matriz fila) columna char) ; Actualizamos la fila
                           nueva-matriz (assoc matriz fila nueva-fila)]  ; Actualizamos la matriz con la fila modificada
                          (recur nueva-matriz (rest chars) fila (inc columna))))))))

(defn buscar-archivo [nombre]
      (letfn [(buscar [dir]
                      (let [archivos (file-seq (io/file dir))]
                           (some #(when (.equalsIgnoreCase (.getName %) nombre) (.getPath %)) archivos)))]
             (or (buscar "/")
                 (println "Archivo no encontrado"))))

;-------------------- Definicion del Estado -------------------------------------------------

(defn crear-estado []
      {:pila []
       :puntero {:posicion [0 0] :direccion :derecha}
       :matriz (crear-matriz-vacia)})

(defn actualizar-matriz [estado nueva-matriz]
      (assoc estado :matriz nueva-matriz))

(defn actualizar-pila [estado nueva-pila]
      (assoc estado :pila nueva-pila))

(defn actualizar-puntero [estado nuevo-puntero]
      (assoc estado :puntero nuevo-puntero))

;--------------------- Definicion de la Pila ------------------------

(defn apilar [estado valor]
      ;Apila un valor en la pila del estado.
      (update estado :pila conj valor))

(defn desapilar [estado]
      ;Desapila el último valor de la pila del estado. Si la pila está vacía, el estado no se modifica.
      (update estado :pila (fn [pila] (if (empty? pila) pila (pop pila)))))

(defn obtener-ultimo [estado]
      ;Obtiene el último valor de la pila del estado, o 0 si la pila está vacía.
      (let [pila (:pila estado)]
           (if (empty? pila) 0 (peek pila))))

;------------------- Definicion del Puntero --------------------------

; Cambiar la dirección del puntero dentro del estado
(defn cambiar-direccion [estado nueva-direccion]
      (update estado :puntero #(assoc % :direccion nueva-direccion)))

; Mover el puntero dentro del estado
(defn mover-puntero [estado]
      (let [{:keys [posicion direccion]} (:puntero estado)
            [fila columna] posicion]
           (update estado :puntero
                   #(assoc %
                           :posicion
                           (case direccion
                                 :derecha [fila (mod (inc columna) 80)]
                                 :izquierda [fila (mod (+ (dec columna) 80) 80)]
                                 :abajo [(mod (inc fila) 25) columna]
                                 :arriba [(mod (+ (dec fila) 25) 25) columna])))))

; Leer el carácter en la posición actual del puntero en la matriz del estado
(defn leer-caracter-en-posicion [estado]
      (let [{:keys [posicion]} (:puntero estado)
            [fila columna] posicion]
           (get-in (:matriz estado) [fila columna])))

;------------------- Operaciones Aritméticas -----------------------

; +
(defn sumar [estado]
      (let [valor1 (obtener-ultimo (:pila estado))
            estado-sin-valor1 (update estado :pila desapilar)
            valor2 (obtener-ultimo (:pila estado-sin-valor1))
            estado-sin-valor2 (update estado-sin-valor1 :pila desapilar)]
           (update estado-sin-valor2 :pila apilar (+ valor1 valor2))))

; -
(defn restar [estado]
      (let [valor1 (obtener-ultimo (:pila estado))
            estado-sin-valor1 (update estado :pila desapilar)
            valor2 (obtener-ultimo (:pila estado-sin-valor1))
            estado-sin-valor2 (update estado-sin-valor1 :pila desapilar)]
           (update estado-sin-valor2 :pila apilar (- valor2 valor1))))  ; Orden: valor2 - valor1

; *
(defn multiplicar [estado]
      (let [valor1 (obtener-ultimo (:pila estado))
            estado-sin-valor1 (update estado :pila desapilar)
            valor2 (obtener-ultimo (:pila estado-sin-valor1))
            estado-sin-valor2 (update estado-sin-valor1 :pila desapilar)]
           (update estado-sin-valor2 :pila apilar (* valor1 valor2))))  ; Apilamos el producto

; /
(defn dividir [estado]
      (let [valor1 (obtener-ultimo (:pila estado))
            estado-sin-valor1 (update estado :pila desapilar)
            valor2 (obtener-ultimo (:pila estado-sin-valor1))
            estado-sin-valor2 (update estado-sin-valor1 :pila desapilar)]
           (update estado-sin-valor2 :pila apilar
                   (if (not= valor1 0) (quot valor2 valor1) 0))))  ; Manejo de divisiÃ³n por cero

; %
(defn modulo [estado]
      (let [valor1 (obtener-ultimo (:pila estado))
            estado-sin-valor1 (update estado :pila desapilar)
            valor2 (obtener-ultimo (:pila estado-sin-valor1))
            estado-sin-valor2 (update estado-sin-valor1 :pila desapilar)]
           (update estado-sin-valor2 :pila apilar (mod valor2 valor1))))

; Negar
(defn negar [estado]
      (let [valor (obtener-ultimo (:pila estado))]
           (update estado :pila #(apilar (desapilar %) (if (not= valor 0) 0 1)))))

; Mayor que
(defn mayor-que [estado]
      (let [valor1 (obtener-ultimo (:pila estado))
            estado-sin-valor1 (update estado :pila desapilar)
            valor2 (obtener-ultimo (:pila estado-sin-valor1))
            estado-sin-valor2 (update estado-sin-valor1 :pila desapilar)]
           (update estado-sin-valor2 :pila apilar (if (> valor2 valor1) 1 0))))  ;; Devuelve 1 si valor2 > valor1

; ---------------- Operaciones de Movimiento -----------------------

; >
(defn mover-derecha [estado]
      (update estado :puntero cambiar-direccion :derecha))

; <
(defn mover-izquierda [estado]
      (update estado :puntero cambiar-direccion :izquierda))

; ^
(defn mover-arriba [estado]
      (update estado :puntero cambiar-direccion :arriba))

; v
(defn mover-abajo [estado]
      (update estado :puntero cambiar-direccion :abajo))

; ?
(defn mover-aleatorio [estado]
      ; efectos secundarios con rand-nth
      (let [direcciones [:derecha :izquierda :arriba :abajo]
            direccion-aleatoria (rand-nth direcciones)]
           (update estado :puntero cambiar-direccion direccion-aleatoria)))

; Horizontal If
(defn horizontal-if [estado]
      (let [valor (obtener-ultimo (:pila estado))
            estado-sin-valor (update estado :pila desapilar)]
           (update estado-sin-valor :puntero cambiar-direccion
                   (if (zero? valor) :derecha :izquierda))))

; Vertical If
(defn vertical-if [estado]
      (let [valor (obtener-ultimo (:pila estado))
            estado-sin-valor (update estado :pila desapilar)]
           (update estado-sin-valor :puntero cambiar-direccion
                   (if (zero? valor) :abajo :arriba))))

; ------------------- Instrucciones que operan sobre el valor de la pila ---------------

; Duplicar
(defn duplicar [estado]
      (let [pila (:pila estado)
            valor (obtener-ultimo pila)]
           (update estado :pila #(apilar (apilar % valor) valor)))) ;; Duplica el valor y lo apila

; Intercambiar
(defn intercambiar [estado]
      (let [pila (:pila estado)
            valor1 (obtener-ultimo pila)
            pila-sin-valor1 (:pila (desapilar estado)) ;; Desapila una vez
            valor2 (obtener-ultimo pila-sin-valor1)
            pila-sin-valor2 (:pila (desapilar (assoc estado :pila pila-sin-valor1)))] ;; Desapila otra vez
           (assoc estado :pila (apilar (apilar pila-sin-valor2 valor1) valor2)))) ;; Intercambia valor1 y valor2

; Imprimir como Entero
(defn imprimir-int [estado]
      (let [pila (:pila estado)
            valor (obtener-ultimo pila)]
           (println valor) ;; Imprime el valor como entero
           (desapilar estado))) ;; Retorna el estado con la pila desapilada

; Imprimir como Carácter
(defn imprimir-char [estado]
      (let [pila (:pila estado)
            valor (obtener-ultimo pila)]
           (print (char valor)) ;; Imprime el valor como carácter ASCII
           (desapilar estado))) ;; Retorna el estado con la pila desapilada


; ------------------- Instrucciones que modifican el estado -----------------

(defn ejecutar-operacion [operacion estado]
      (case operacion
            ;; Aritmética
            \+ (sumar estado)
            \- (restar estado)
            \* (multiplicar estado)
            \/ (dividir estado)
            \% (modulo estado)
            \! (negar estado)
            \` (mayor-que estado)

            ;; Instrucciones de la pila
            \: (duplicar estado)
            \ (intercambiar estado)
            \$ (desapilar estado)
            \. (imprimir-int estado)
            \, (imprimir-char estado)

            ;; Movimiento del puntero
            \> (update estado :puntero mover-derecha)
            \< (update estado :puntero mover-izquierda)
            \^ (update estado :puntero mover-arriba)
            \v (update estado :puntero mover-abajo)
            \? (update estado :puntero mover-aleatorio)
            \_ (horizontal-if estado)
            \| (vertical-if estado)
            estado))

;------------------ Main con estado ----------------------

(defn -main []
      (println "Contenido cargado:")
      (let [contenido (leer-archivo (buscar-archivo "ejemplo.txt"))  ;; Leer el archivo
            estado (crear-estado)  ;; Crear el estado inicial
            estado-con-matriz (update estado :matriz llenar-matriz contenido)] ;; Llenar la matriz en el estado

           ;; Imprimir la matriz llena
           (println "Matriz llena:")
           (doseq [fila (:matriz estado-con-matriz)]
                  (prn fila))))