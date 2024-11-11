(ns befunge-93.core
    (:require [clojure.string :as str]
      [clojure.java.io :as io]))

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

(defn -main []
      (println "Contenido cargado:")
      (let [contenido (leer-archivo (buscar-archivo "ejemplo.txt"))
            matriz (crear-matriz-vacia)
            matriz-llena (llenar-matriz matriz contenido)]
           (println "Matriz llena:")
           (doseq [fila matriz-llena]
                  (prn fila))))