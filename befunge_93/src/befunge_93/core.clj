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

;-------------------- Definicion de la Pila-------------------------------------------------
(defn crear-pila []
      [])

(defn apilar [list x]
      (conj list x))

(defn desapilar [list]
      (if (empty? list)
        0  ; Si la pila está vacía, devuelve 0
        (pop list)))  ; Si no está vacía, desapila un elemento

(defn obtener-ultimo [list]
      (first list))

;------------- Puntero-----------------------------------------

(def puntero {:posicion [0 0] :direccion :derecha})

(defn cambiar-direccion [puntero nueva-direccion]
      (assoc puntero :direccion nueva-direccion))

(defn mover-puntero [puntero]
      (let [{:keys [posicion direccion]} puntero
            [fila columna] posicion]
           (case direccion
                 :derecha (assoc puntero :posicion [fila (mod (inc columna) 80)])
                 :izquierda (assoc puntero :posicion [fila (mod (+ (dec columna) 80) 80)])
                 :abajo (assoc puntero :posicion [(mod (inc fila) 25) columna])
                 :arriba (assoc puntero :posicion [(mod (+ (dec fila) 25) 25) columna]))))

(defn leer-caracter-en-posicion [matriz puntero]
      (let [{:keys [posicion]} puntero
            [fila columna] posicion] ; extraemos fila y columna del puntero
           (get-in matriz [fila columna])))

;-----------------------Operaciones Aritmetícas---------------------------------------------
; +
(defn sumar [pila]
      (let [valor1 (desapilar pila)
            valor2 (desapilar pila)]
           (apilar pila (+ valor1 valor2))))
; -
(defn restar [pila]
      (let [valor1 (desapilar pila)
            valor2 (desapilar pila)]
           (apilar pila (- valor2 valor1))))  ; El orden cambia para restar: valor2 - valor1

; *
(defn multiplicar [pila]
      (let [valor1 (desapilar pila)
            valor2 (desapilar pila)]
           (apilar pila (* valor1 valor2))))  ; Apilamos el producto de ambos valores

; /
(defn dividir [pila]
      (let [valor1 (desapilar pila)
            valor2 (desapilar pila)]
           (if (not= valor1 0)  ; Verificamos que no haya división por cero
             (apilar pila (quot valor2 valor1))  ; Usamos quot para división entera
             (apilar pila 0))))  ; Si hay división por cero, devolvemos 0
; %
(defn modulo [pila]
      (let [valor1 (desapilar pila)
            valor2 (desapilar pila)]
           (apilar pila (mod valor2 valor1))))

(defn not [pila]
      (let [valor (obtener-ultimo pila)
            nueva-pila (desapilar pila)]
           (apilar nueva-pila (if (not= valor 0) 0 1))))


;----------------Instrucciones de movimiento-------------------------

; >
(defn mover-derecha [puntero]
      (cambiar-direccion puntero :derecha))
; <
(defn mover-izquierda [puntero]
      (cambiar-direccion puntero :izquierda))
; ^
(defn mover-arriba [puntero]
      (cambiar-direccion puntero :arriba))
; v
(defn mover-abajo [puntero]
      (cambiar-direccion puntero :abajo))
; ?
(defn mover-aleatorio [puntero]
      (let [direcciones [:derecha :izquierda :arriba :abajo]
            direccion-aleatoria (rand-nth direcciones)]  ;; Selecciona una dirección aleatoria
           (cambiar-direccion puntero direccion-aleatoria)))

;-------------Instrucciones que operan sobre los valores de la pila--------------------

(defn duplicar [pila]
      (let [valor (obtener-ultimo pila)]
           (apilar (apilar pila valor) valor)))  ;; Duplica el valor y lo apila


(defn intercambiar [pila]
      (let [valor1 (obtener-ultimo pila)
            pila-sin-valor1 (desapilar pila)
            valor2 (obtener-ultimo pila-sin-valor1)
            nueva-pila (desapilar pila-sin-valor1)]
           (apilar (apilar nueva-pila valor1) valor2)))  ;; Intercambia valor1 y valor2


(defn imprimir-int [pila]
      (let [valor (obtener-ultimo pila)]
           (println valor)  ;; Imprime el valor como un número entero
           pila))  ;; Retorna la pila sin cambios

(defn imprimir-char [pila]
      (let [valor (obtener-ultimo pila)]
           (print (char valor))  ;; Imprime el valor como un carácter ASCII
           pila))  ;; Retorna la pila sin cambios


(defn ejecutar-operacion [operacion pila puntero]
      (case operacion
            ;Atirmetica
            + (sumar pila)
            - (restar pila)
            * (multiplicar pila)
            / (dividir pila)
            % (modulo pila)
            ! (not pila)
            ` (mayor-que pila)
            ;Ins de la Pila
            \: (duplicar pila)
            \ (intercambiar pila)
            $ (desapilar pila)
            . (imprimir-int pila)
            , (imprimir-char pila)
            ;Mover Puntero
            > (mover-derecha puntero)
            < (mover-izquierda puntero)
            ^ (mover-arriba puntero)
            v (mover-abajo puntero)
            ? (mover-aleatorio puntero)
            _   pila))


(defn imprimir-estado-puntero [puntero]
      (println "Posición actual:" (:posicion puntero))
      (println "Dirección actual:" (:direccion puntero) "\n"))


(defn -main []
      (println "Contenido cargado:")
      (let [contenido (leer-archivo (buscar-archivo "ejemplo.txt"))  ;; Leer el archivo
            matriz (crear-matriz-vacia)  ;; Crear la matriz vacía
            matriz-llena (llenar-matriz matriz contenido)  ;; Llenar la matriz con el contenido del archivo
            puntero {:posicion [0 0] :direccion :derecha}
            puntero-2 (cambiar-direccion puntero :abajo)
            puntero-3 (mover-puntero puntero-2)]  ;; Inicializar el puntero]

           ;; Imprimir la matriz llena
           (println "Matriz llena:")
           (doseq [fila matriz-llena]
                  (prn fila))

           ;; Leer el carácter en la posición [0 0] del puntero
           (println "Primer carácter en la posición [1 0]:"
                    (leer-caracter-en-posicion matriz-llena puntero-3))))