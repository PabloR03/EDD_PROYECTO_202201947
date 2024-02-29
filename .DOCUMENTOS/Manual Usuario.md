# ESTRUCTURA DE DATOS B
## Proyecto - FASE 1
### PRIMER SEMESTRE 2024
```js
Universidad San Carlos de Guatemala
Programador: Pablo Andres Rodriguez Lima
Carne: 202201947
Correo: pabloa10rodriguez@gmail.com
```
---
## Descripción del Proyecto
Estamos encantados de presentarte nuestro programa diseñado para el manejo del flujo de una imprenta de una manera sencilla y visual. podrás realizar diversas acciones como ingresar clientes para ver su flujo y sobre todo ver el estado en el que se encuentra la estructura.

Para aprovechar al máximo nuestro programa, asegúrate de cumplir con los siguientes requisitos mínimos:

-Tener instalado Fortran, preferiblemente en su versión 13.2.0 .
-Te recomendamos utilizar un editor de texto con la extensión adecuada, como Visual Studio Code, para una experiencia de usuario más cómoda.
-Asegúrate de tener Graphviz instalado, ya que es una herramienta que nuestro programa utiliza para visualizar los datos de manera efectiva.

## Objetivos
* Objetivo General:
    * Desarrollar un programa que permita a los usuarios gestionar proyectos de manera efectiva.

* Objetivos Específicos:
    * Permitir a los usuarios ingresar clientes y ver su flujo.
    * Permitir a los usuarios ver el estado en el que se encuentra la estructura.

---
## Como usar el programa
Para utilizar nuestro programa, primero debes descargarlo desde nuestro repositorio de GitHub. Una vez descargado, puedes abrirlo en tu IDE o editor de texto preferido. Asegúrate de tener instalado Fortran en tu computadora, ya que es el lenguaje de programación que utilizamos para desarrollar nuestro programa. También es importante que tengas instalado Graphviz, ya que es una herramienta que utilizamos para visualizar los datos de manera efectiva.

### Menu Principal
Se muestra una serie de opciones que el usuario puede elegir para realizar una accion en el programa.

### Carga de Parametros iniciales
Se carga un archivo de del tipoJson con los parametros iniciales del programa (Clientes).
ademas se requerir un tamaño para determinar cuantas ventanillas estaran atendiendo a los clientes.

### Ejecutar paso
Se ejecuta un paso del programa, en el cual se atiende a un cliente y se muestran los cambios en la estructura.
esta tiene la habilidad de mover un cliente o sacarlo dependiendo el caso, mandar las imagenes a imprimir, y dejar la ventanilla disponible, tambien usa el tiempo para simular el proceso de impresion.

El cliente pasará a una lista de espera en la cual podra esperar a que le entreguen sus imagenes.

### Estado de las Estructuras
Se muestra el estado actual de las estructuras, con los clientes que estan siendo atendidos y los que estan en espera.

### Reportes
Se generan reportes de los clientes que han sido atendidos y los que estan en espera, segun el reporte se puede clasificar, dependiendo la cantidad de pasos, sus imagenes a imprimir y por nombre

### About
Se muestra informacion sobre el programa y los desarrolladores.

### Salir
Se cierra el programa.