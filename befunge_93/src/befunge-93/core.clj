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
  (vec (repeat 25 (vec (repeat 80 \space)))))               ; Crea una matriz 25x80 llena de espacios

(defn llenar-matriz [matriz contenido]
  (loop [matriz matriz
         chars (seq contenido)
         fila 0
         columna 0]
    (if (empty? chars)
      matriz                                                ; Aquí retornamos la última versión de la matriz actualizada
      (let [char (first chars)]
        (if (= char \newline)
          ;; Salto de línea: avanza a la siguiente fila y reinicia columna
          (recur matriz (rest chars) (inc fila) 0)
          ;; Inserta el carácter en la posición actual
          (let [nueva-fila (assoc (matriz fila) columna char) ; Actualizamos la fila
                nueva-matriz (assoc matriz fila nueva-fila)] ; Actualizamos la matriz con la fila modificada
            (recur nueva-matriz (rest chars) fila (inc columna))))))))

(defn buscar-archivo [nombre]
  (letfn [(buscar [dir]
            (let [archivos (file-seq (io/file dir))]
              (some #(when (= (.getName %) nombre) (.getPath %)) archivos)))]
    (let [archivo-encontrado (buscar "/home")]
      (if archivo-encontrado
        (do
          (println "Archivo encontrado en:" archivo-encontrado)
          archivo-encontrado)
        (do
          (println "Archivo no encontrado")
          nil)))))

;-------------------- Definicion del Estado -------------------------------------------------

(defn crear-estado []
  {:pila    []
   :puntero {:posicion [0 0] :direccion :derecha}
   :matriz  (crear-matriz-vacia)
   :stringmode false
   :activo true})

(defn toggle-stringmode [estado]
  (update estado :stringmode not))

;--------------------- Definicion de la Pila ------------------------

(defn apilar [estado valor]
  ;Apila un valor en la pila del estado.
  (update estado :pila conj valor))

(defn desapilar [estado]
  ;Desapila el último valor de la pila del estado. Si la pila está vacía, el estado no se modifica.
  (update estado :pila (fn [pila] (if (empty? pila) pila (pop pila)))))

(defn obtener-ultimo [estado]
  ;; Obtiene el último valor de la pila del estado, o 0 si la pila está vacía.
  (let [pila (:pila estado)]
    (if (empty? pila)
      0
      (let [ultimo (peek pila)]
        ultimo))))

;------------------- Definicion del Puntero --------------------------

; Mover el puntero dentro del estado
(defn mover-puntero [estado]
  (let [{:keys [posicion direccion]} (:puntero estado)
        [fila columna] posicion]
    ;(println "Dirección actual: " direccion)
    (update estado :puntero
            #(assoc %
               :posicion
               (case direccion
                 :derecha [fila (mod (inc columna) 80)]
                 :izquierda [fila (mod (+ (dec columna) 80) 80)]
                 :abajo [(mod (inc fila) 25) columna]
                 :arriba [(mod (+ (dec fila) 25) 25) columna])))))

;------------------- Operaciones Aritméticas -----------------------

(defn sumar [estado]
  (let [valor1 (obtener-ultimo estado)  ; Obtener el último valor
        estado-sin-valor1 (desapilar estado)  ; Desapilar el último valor
        valor2 (obtener-ultimo estado-sin-valor1)  ; Obtener el siguiente valor
        estado-sin-valor2 (desapilar estado-sin-valor1)]  ; Desapilar el siguiente valor
    (apilar estado-sin-valor2 (+ valor1 valor2))))  ; Apilar el resultado de la suma

(defn restar [estado]
  (let [valor1 (int (obtener-ultimo estado)) ; Convertir a número
        estado-sin-valor1 (desapilar estado)
        valor2 (int (obtener-ultimo estado-sin-valor1)) ; Convertir a número
        estado-sin-valor2 (desapilar estado-sin-valor1)]
    (apilar estado-sin-valor2 (- valor2 valor1)))) ; Restar: valor2 - valor1

(defn multiplicar [estado]
  (let [valor1 (obtener-ultimo estado)
        estado-sin-valor1 (desapilar estado)
        valor2 (obtener-ultimo estado-sin-valor1)
        estado-sin-valor2 (desapilar estado-sin-valor1)]
    (apilar estado-sin-valor2 (* valor1 valor2))))  ; Multiplicación: value1 * value2

(defn dividir [estado]
  (let [valor1 (obtener-ultimo estado)
        estado-sin-valor1 (desapilar estado)
        valor2 (obtener-ultimo estado-sin-valor1)
        estado-sin-valor2 (desapilar estado-sin-valor1)]
    (apilar estado-sin-valor2
            (if (not= valor1 0) (quot valor2 valor1) 0))))  ; División: value1 / value2

(defn modulo [estado]
  (let [valor2 (obtener-ultimo estado)
        estado-sin-valor2 (desapilar estado)
        valor1 (obtener-ultimo estado-sin-valor2)
        estado-sin-valor1 (desapilar estado-sin-valor2)]
    (apilar estado-sin-valor1 (mod valor1 valor2))))  ; Módulo: value1 mod value2

(defn negar [estado]
  (let [valor (obtener-ultimo estado)]
    (apilar (desapilar estado) (if (not= valor 0) 0 1))))  ; Negación: 0 si no es 0, 1 si es 0

(defn mayor-que [estado]
  (let [valor2 (obtener-ultimo estado)
        estado-sin-valor2 (desapilar estado)
        valor1 (obtener-ultimo estado-sin-valor2)
        estado-sin-valor1 (desapilar estado-sin-valor2)]
    (apilar estado-sin-valor1 (if (> valor1 valor2) 1 0))))  ;; Mayor que: 1 si value1 > value2, 0 en caso contrario

; ---------------- Operaciones de Movimiento -----------------------

; >
(defn mover-derecha [estado]
  (update estado :puntero assoc :direccion :derecha))

; <
(defn mover-izquierda [estado]
  (update estado :puntero assoc :direccion :izquierda))

; ^
(defn mover-arriba [estado]
  (update estado :puntero assoc :direccion :arriba))

; v
(defn mover-abajo [estado]
  (update estado :puntero assoc :direccion :abajo))

; ?
(defn mover-aleatorio [estado]
  ; efectos secundarios con rand-nth
  (let [direcciones [:derecha :izquierda :arriba :abajo]
        direccion-aleatoria (rand-nth direcciones)]
    (update estado :puntero assoc :direccion direccion-aleatoria)))

; Horizontal If
(defn horizontal-if [estado]
  (let [valor (obtener-ultimo estado)   ;; Obtener el valor en la cima de la pila
        estado-sin-valor (desapilar estado)]     ;; Desapilar el valor (actualizar la pila)
    ;(println "Valor en la cima de la pila:" valor)
    ;(println "Estado antes de actualizar puntero:" estado-sin-valor)
    (let [nuevo-estado (update estado-sin-valor :puntero assoc :direccion (if (zero? valor) :derecha :izquierda))]
      ; (println "Estado después de actualizar puntero:" nuevo-estado)
      nuevo-estado)))  ;; Retornar el estado con la dirección actualizada

; Vertical If
(defn vertical-if [estado]
  (let [valor (obtener-ultimo estado)
        estado-sin-valor (desapilar estado)]  ; Apilamos el valor de la pila
    (update estado-sin-valor :puntero assoc :direccion (if (zero? valor) :abajo :arriba))))

; ------------------- Instrucciones que operan sobre el valor de la pila ---------------

; Duplicar
(defn duplicar [estado]
  (if (empty? (:pila estado))
    estado ;; Si la pila está vacía, no hace nada.
    (let [valor (obtener-ultimo estado)]
      (apilar estado valor))))

; Intercambiar
(defn intercambiar [estado]
  (let [pila (:pila estado)]
    (cond
      ;; Si la pila está vacía, apilamos dos ceros.
      (empty? pila)
      (update estado :pila #(conj % 0 0))

      ;; Si la pila tiene un solo elemento, apilamos un cero.
      (= (count pila) 1)
      (update estado :pila conj 0)

      ;; Si hay al menos dos elementos, intercambiamos los dos superiores.
      :else
      (let [valor2 (obtener-ultimo estado)
            estado-sin-valor2 (desapilar estado)   ;; Desapila una vez
            valor1 (obtener-ultimo estado-sin-valor2)
            estado-sin-valor1 (desapilar estado-sin-valor2)] ;; Desapila otra vez
        (apilar (apilar estado-sin-valor1 valor2) valor1)))))

; Imprimir como Entero
(defn imprimir-int [estado]
  (let [pila (:pila estado)]
    (if (empty? pila)
      (do
        estado) ;; Devolvemos el estado sin cambios
      (let [valor (obtener-ultimo estado)]
        (println valor) ;; Imprime el valor como entero
        (desapilar estado))))) ;; Retorna el estado con la pila desapilada

; Imprimir como Carácter
(defn imprimir-char [estado]
  (let [pila (:pila estado)]
    (if (empty? pila)
      (do
        estado) ;; Devolvemos el estado sin cambios
      (let [valor (obtener-ultimo estado)]
        (print (char valor)) ;; Imprime el valor como carácter ASCII
        (desapilar estado))))) ;; Retorna el estado con la pila desapilada

; ------------------- Operaciones de matriz/toroide ------------------------

(defn obtener [estado]
  (let [pila (:pila estado)]
    (if (< (count pila) 2)
      estado ; Si hay menos de 2 elementos en la pila, no hacemos nada
      (let [y (obtener-ultimo estado)
            estado-sin-y (desapilar estado)
            x (obtener-ultimo estado-sin-y)
            estado-sin-x (desapilar estado-sin-y)
            valor-en-pos (if (or (< y 0) (> y 24) (< x 0) (> x 79))
                           0 ; Fuera de rango: apilar 0
                           (get-in (:matriz estado) [y x]))] ; Valor en la posición
        (apilar estado-sin-x (int valor-en-pos)))))) ; Apilar el valor ASCII

(defn colocar [estado]
  (let [pila (:pila estado)]
    (if (< (count pila) 3)
      (do estado)
      (let [y (obtener-ultimo estado)
            estado-sin-y (desapilar estado)
            x (obtener-ultimo estado-sin-y)
            estado-sin-x (desapilar estado-sin-y)
            value (obtener-ultimo estado-sin-x)
            estado-sin-value (desapilar estado-sin-x)
            matriz (:matriz estado)
            matriz-actualizada (assoc-in matriz [y x] value)]
        (assoc estado-sin-value :matriz matriz-actualizada)))))

; ------------------ Funciones que interactuan con el usuario ------------------

(defn input-int [estado]
  (println "Ingrese un número entero:")
  (try
    (let [valor (Integer/parseInt (read-line))] ;; Convierte la entrada en entero
      (apilar estado valor)) ;; Apila el valor en el estado
    (catch NumberFormatException e
      (do
        (println "Error: Debe ingresar un número entero.")
        estado)))) ;; Devuelve el estado sin cambios en caso de error

(defn input-char [estado]
  (println "Ingrese un carácter:")
  (let [entrada (read-line)]
    (if (and entrada (not (empty? entrada)))
      (let [char (first entrada)] ;; Obtiene el primer carácter de la entrada
        (apilar estado (int char))) ;; Convierte el carácter a código ASCII y lo apila
      (do
        (println "Error: No se ingresó ningún carácter.")
        estado)))) ;; Devuelve el estado sin cambios en caso de error

; ------------------- Instrucciones que modifican el estado -----------------

(defn ejecutar-instruccion [estado instruccion]
  (cond
    ;; Si la instrucción es un espacio, simplemente retorna el estado sin modificarlo
    (= instruccion \space) estado
    ;; Aritmética
    (= instruccion \+) (sumar estado)
    (= instruccion \-) (restar estado)
    (= instruccion \*) (multiplicar estado)
    (= instruccion \/) (dividir estado)
    (= instruccion \%) (modulo estado)
    (= instruccion \!) (negar estado)
    (= instruccion \`) (mayor-que estado)
    ;; Instrucciones de la pila
    (= instruccion \:) (duplicar estado)
    (= instruccion \\) (intercambiar estado)
    (= instruccion \$) (desapilar estado)
    (= instruccion \.) (imprimir-int estado)
    (= instruccion \,) (imprimir-char estado)
    ;; Movimiento del puntero
    (= instruccion \>) (mover-derecha estado)
    (= instruccion \<) (mover-izquierda estado)
    (= instruccion \^) (mover-arriba estado)
    (= instruccion \v) (mover-abajo estado)
    (= instruccion \?) (mover-aleatorio estado)
    (= instruccion \_) (horizontal-if estado)
    (= instruccion \|) (vertical-if estado)
    ;; Operaciones Matriz
    (= instruccion \g) (obtener estado)
    (= instruccion \p) (colocar estado)
    ;; Inputs y Outputs
    (= instruccion \&) (input-int estado)
    (= instruccion \~) (input-char estado)
    ;; Caso por defecto
    :else estado)) ;; Si no se encuentra la instrucción, devuelve el estado tal cual

(defn apilar-valor-desde-char [estado char]
  (apilar estado (int char)))

(defn procesar-instruccion [estado instruccion]
  (cond
    (= instruccion \@) (assoc estado :activo false) ;; Finaliza el programa
    (= instruccion \") (toggle-stringmode estado) ;; Alterna modo cadena
    (= instruccion \#) (mover-puntero estado)
    (:stringmode estado) (apilar-valor-desde-char estado instruccion) ;; En modo cadena
    (Character/isDigit instruccion)                ;; Si es un número
    (let [numero (Character/digit instruccion 10)] ;; Convierte el caracter en número
      (update estado :pila conj numero))           ;; Apila el número
    :else (ejecutar-instruccion estado instruccion))) ;; Ejecuta instrucción normal

(defn ejecutar-programa [estado]
  (loop [estado-actual estado]
    (when (:activo estado-actual)
      (let [{:keys [posicion direccion]} (:puntero estado-actual) ;; Extrae posición y dirección
            [x y] posicion
            instruccion (get-in (:matriz estado-actual) [x y])] ;; Obtener la instrucción actual
        ;        (println "Posición:" [x y]
        ;                 "Instrucción:" instruccion
        ;                 "Dirección:" direccion
        ;                 "Estado stringmode:" (:stringmode estado-actual)
        ;                 "Pila actual:" (:pila estado-actual))    ;; Imprimir la pila
        ;(Thread/sleep 500)                                  ;; Esperar 1 segundo
        ;; Procesar la instrucción y mover el puntero
        (recur (mover-puntero (procesar-instruccion estado-actual instruccion)))))))

;------------------ Pruebas de operaciones ------------------

(defn prueba-operaciones-pila []
  (let [estado-inicial (crear-estado)
        ; Apilamos algunos valores en la pila
        estado-apilado (-> estado-inicial
                           (apilar 10)
                           (apilar 20)
                           (apilar 30))
        ; Verificamos el último valor de la pila
        ultimo-valor (obtener-ultimo estado-apilado)
        ; Desapilamos un valor
        estado-despilado (desapilar estado-apilado)
        ; Verificamos el nuevo último valor después de desapilar
        ultimo-valor-despilado (obtener-ultimo estado-despilado)]
    (println "Pila inicial: " (:pila estado-inicial))           ; Pila vacía []
    (println "Pila después de apilar 10, 20, 30: " (:pila estado-apilado)) ; Pila con [10, 20, 30]
    (println "Último valor de la pila (después de apilar): " ultimo-valor) ; Debería imprimir 30
    (println "Pila después de desapilar: " (:pila estado-despilado)) ; Pila con [10, 20]
    (println "Último valor de la pila (después de desapilar): " ultimo-valor-despilado) ; Debería imprimir 20
    ))

(defn prueba-operaciones-aritmeticas []
  (let [estado-inicial (crear-estado)
        ; Apilamos algunos valores para probar las operaciones
        estado-apilado (-> estado-inicial
                           (apilar 10)                      ; Apilamos 10
                           (apilar 5))                      ; Apilamos 5
        ; Probamos la suma
        estado-suma (sumar estado-apilado)
        resultado-suma (:pila estado-suma)                  ; Debería ser [15]
        ; Probamos la resta
         estado-resta (restar estado-apilado)
        resultado-resta (:pila estado-resta) ; Debería ser [5]
        ; Probamos la multiplicación
        estado-multiplicacion (multiplicar estado-apilado)
        resultado-multiplicacion (:pila estado-multiplicacion) ; Debería ser [50]
        ; Probamos la división
        estado-division (dividir estado-apilado)
        resultado-division (:pila estado-division) ; Debería ser [2] (10 / 5)
        ; Probamos el módulo
        estado-modulo (modulo estado-apilado)
        resultado-modulo (:pila estado-modulo) ; Debería ser [0] (10 % 5)
        ; Probamos la negación
        estado-negado (negar estado-apilado)
        resultado-negado (:pila estado-negado) ; Debería ser [1] (negación de 0)
        ; Probamos el mayor que
        estado-mayor-que (mayor-que estado-apilado)
        resultado-mayor-que (:pila estado-mayor-que)
        ]                                                   ; Debería ser [1] (10 > 5)
    (println "Suma: " resultado-suma)
    (println "Resta: " resultado-resta)
    (println "Multiplicación: " resultado-multiplicacion)
    (println "División: " resultado-division)
    (println "Módulo: " resultado-modulo)
    (println "Negación: " resultado-negado)
    (println "Mayor que: " resultado-mayor-que)
    ))

(defn test-operaciones []
  (let [estado-inicial (crear-estado)
        estado-1 (apilar estado-inicial 10)
        estado-2 (apilar estado-1 5)
        estado-3 (apilar estado-2 43)
        estado-duplicado (duplicar estado-3)
        estado-intercambiado (intercambiar estado-3)
        estado-imprimir-int (imprimir-int estado-3)
        estado-imprimir-char (imprimir-char estado-3)]
    ;; Test Duplicar
    (println "Estado inicial: " (:pila estado-3))
    (println "Después de duplicar (debe agregar 43 otra vez): " (:pila estado-duplicado))
    ;; Test Intercambiar
    (println "Después de intercambiar (debe ser [10 43 5]): " (:pila estado-intercambiado))
    ;; Test Imprimir Entero
    (println "Después de imprimir entero (debe eliminar 43 de la pila): " (:pila estado-imprimir-int))
    ;; Test Imprimir Carácter
    (println "Después de imprimir carácter (debe eliminar el valor 43 de la pila): " (:pila estado-imprimir-char))))

;------------------ Main con estado ----------------------

(defn -main [& args]
  (println "Contenido cargado:")
  (let [contenido (leer-archivo (buscar-archivo (first args))) ;; Usamos el primer argumento
        estado (crear-estado)                               ;; Crear el estado inicial
        estado-con-matriz (update estado :matriz llenar-matriz contenido)] ;; Llenar la matriz en el estado
    (ejecutar-programa estado-con-matriz))) ;; Ejecutar el programa con el estado actualizado
