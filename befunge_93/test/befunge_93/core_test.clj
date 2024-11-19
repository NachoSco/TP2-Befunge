(ns befunge-93.core-test
    (:require [clojure.test :refer :all]
      [befunge-93.core :refer :all])) ;; Asegúrate de que todo esté referenciado correctamente

(deftest pila-operations-test
         (testing "Operaciones básicas de pila"
                  (let [estado-inicial (crear-estado)
                        estado-con-pila (update estado-inicial :pila #(conj % 3 4)) ; Pila inicial [3, 4]
                        estado-suma (ejecutar-operacion \+ estado-con-pila)
                        estado-duplicar (ejecutar-operacion \: estado-suma)]
                       (is (= (:pila estado-con-pila) [3 4]) "Apilar 3 y 4 debería crear la pila [3 4]")
                       (is (= (:pila estado-suma) [7]) "La suma debería dejar la pila con [7]")
                       (is (= (:pila estado-duplicar) [7 7]) "Duplicar debería dejar la pila con [7 7]"))))

(deftest puntero-operations-test
         (testing "Operaciones básicas del puntero"
                  (let [estado-inicial (crear-estado)
                        estado-mover-derecha (ejecutar-operacion \> estado-inicial)
                        estado-mover-abajo (ejecutar-operacion \v estado-mover-derecha)]
                       (is (= (:direccion (:puntero estado-inicial)) :derecha) "Dirección inicial debería ser :derecha")
                       (is (= (:direccion (:puntero estado-mover-derecha)) :derecha) "Mover derecha no debería cambiar la dirección (ya está en :derecha)")
                       (is (= (:direccion (:puntero estado-mover-abajo)) :abajo) "Mover abajo debería cambiar la dirección a :abajo"))))

(deftest imprimir-operations-test
         (testing "Operaciones de impresión"
                  (let [estado-inicial (crear-estado)
                        estado-con-pila (update estado-inicial :pila #(conj % 65)) ; Apilamos un valor ASCII (A)
                        _ (with-out-str (ejecutar-operacion \, estado-con-pila)) ; Imprime como char
                        output-int (with-out-str (ejecutar-operacion \. estado-con-pila))] ; Imprime como int
                       (is (= output-int "65\n") "Imprimir como entero debería mostrar 65 seguido de un salto de línea"))))

(deftest operaciones-no-validas-test
         (testing "Operación no válida debería dejar el estado sin cambios"
                  (let [estado-inicial (crear-estado)
                        estado-sin-cambios (ejecutar-operacion \# estado-inicial)]
                       (is (= estado-inicial estado-sin-cambios) "Una operación desconocida no debería modificar el estado"))))