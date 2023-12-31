# Cobol_TP01

## Trabajo Práctico 1

Un instituto educativo necesita calcular los promedios de las notas de 
los alumnos que cursan las materias que se dictan en la institución, con 
el fin de poder tener un registro ordenado con los promedios de los 
estudiantes que hayan rendido los 3 exámenes por materia.
El sistema también deberá identificar a los alumnos con los mejores 
promedios en cada materia, para poder ponerlos en un cuadro de 
honor.

### Instrucciones:
* Contamos con un archivo con la información de notas por alumno 
"ALUMNOS.TXT”. 
* Vamos a calcular el promedio de las notas de cada alumno por 
materia si y sólo si ha rendido los 3 exámenes para cada materia. 
* Generar un listado “PROMEDIO.TXT”. El mismo debe contar con la 
información de cantidad de alumnos leídos, cantidad de alumnos 
que cumplieron con la condición para calcular el promedio, la 
cantidad de alumnos que se descartó por no cumplir con los 3 
exámenes y la cantidad de registros con error.
* Generar un listado con los alumnos que no hayan rendido los 3 
exámenes o tengan errores de datos para una materia 
“DESCARTADOS.TXT”.
* Generar el listado "HONOR.TXT" que contendrá una lista de los 
alumnos con los mejores promedios en cada materia. En caso de 
tener más de un alumno con el mismo promedio, nos quedamos 
con el último y más reciente promedio obtenido.
* Informar los registros que no pudieron ser procesados por 
presentar algún error de dato en un archivo de errores 
"ERROR.TXT".

* Para los archivos de promedio, honor y descartados deberemos 
respetar el diseño definido más adelante, y para el de errores los 
registros deben ser igual al formato del archivo de entrada.

### Consideraciones y Buenas prácticas:
* Nombre del programa TP01EJ01 y el nombre del archivo físico 
TP01EJ01.cbl 
* En el inicio del código agregar comentado los miembros del 
grupo:
* APELLIDO, NOMBRE – AULA X
* Los nombres de los archivos físico de los copy deben llamarse de 
la con el mismo nombre de los archivos de entrada y salida 
(ALUMNOS.CPY, PROMEDIOS.CPY, HONOR.CPY, ERROR.CPY, 
DESCARTADOS.CPY)
* Las variables declaradas en la WORKING STORAGE deben 
comenzar con WS-, al menos que sean para indicar el File Status 
de los archivos que deben comenzar con FS-
* Respetar el formato de los nombres de los párrafo NNNN-VERBO-DESCRIPCION, y escribirlos en orden ascendente
* Inicializar todas las variables y lo necesario necesarias en el 
párrafo 1000, el proceso principal debe comenzar en el párrafo 
2000 y el finalizar a partir del 3000.
* No utilizar COMPUTE para los cálculos
* Realizar las validaciones correspondiente al uso de archivos, por 
ejemplo contemplar que exista
* El programa no debe contener DISPLAY
Entregables
* Diagrama de flujo/Jackson
* Archivo .cbl, el mismo debe compilar sin errores y debe poder 
ejecutarse
* Archivos .cpy con las estructuras de datos de todos los archivos 
(entrada y salida)

### Detalles:
Formato del archivo de entrada "ALUMNOS.TXT":
* Fecha AAAA-MM-DD : alfanumérico de 10 caracteres
* Apellido y Nombre: alfanumérico de 40 caracteres
* Materia : alfanumérico de 30 caracteres
* Nota : numérico de 5 dígitos enteros con 3 dígitos decimales
Respetar el formato de cada listado como se indica a 
continuación.

### Formato del listado de salida "PROMEDIOS.TXT".
```
* ==================================================
* Apellido y Nombre      | Materia    | Promedio
* ==================================================
* AAAAAAAAAA, AAAAAAAAAA | DDDDDDDDDD | 0,00
* AAAAAAAAAA, AAAAAAAAAA | DDDDDDDDDD | 0,00
* AAAAAAAAAA, AAAAAAAAAA | DDDDDDDDDD | 0,00
* AAAAAAAAAA, AAAAAAAAAA | DDDDDDDDDD | 0,00
* AAAAAAAAAA, AAAAAAAAAA | DDDDDDDDDD | 0,00
* AAAAAAAAAA, AAAAAAAAAA | DDDDDDDDDD | 0,00
* ---------------------------------------------------
* BBBBBBBBBB, BBBBBBBBBB | DDDDDDDDDD | 0,00
* BBBBBBBBBB, BBBBBBBBBB | DDDDDDDDDD | 0,00
* BBBBBBBBBB, BBBBBBBBBB | DDDDDDDDDD | 0,00
* BBBBBBBBBB, BBBBBBBBBB | DDDDDDDDDD | 0,00
* BBBBBBBBBB, BBBBBBBBBB | DDDDDDDDDD | 0,00
* BBBBBBBBBB, BBBBBBBBBB | DDDDDDDDDD | 0,00
* ---------------------------------------------------
* CCCCCCCCCC, CCCCCCCCCC | DDDDDDDDDD | 0,00
* CCCCCCCCCC, CCCCCCCCCC | DDDDDDDDDD | 0,00
* CCCCCCCCCC, CCCCCCCCCC | DDDDDDDDDD | 0,00
* CCCCCCCCCC, CCCCCCCCCC | DDDDDDDDDD | 0,00
* CCCCCCCCCC, CCCCCCCCCC | DDDDDDDDDD | 0,00
* CCCCCCCCCC, CCCCCCCCCC | DDDDDDDDDD | 0,00
* ====================================================
* Cantidad de registros leídos : 00000
* Cantidad de registros procesados : 00000
* Cantidad de registros descartados : 00000
* Cantidad de registros con error : 00000
* ====================================================
```


### Formato del listado de salida "HONOR.TXT"
```
====================================================
Materia    | Apellido y Nombre      | Promedio
====================================================
XXXXXXXXXX | AAAAAAAAAA, AAAAAAAAAA | 0,00
XXXXXXXXXX | BBBBBBBBBB, BBBBBBBBBB | 0,00
XXXXXXXXXX | CCCCCCCCCC, CCCCCCCCCC | 0,00
XXXXXXXXXX | DDDDDDDDDD, DDDDDDDDDD | 0,00
XXXXXXXXXX | EEEEEEEEEE, EEEEEEEEEE | 0,00
XXXXXXXXXX | FFFFFFFFFF, FFFFFFFFFF | 0,00
====================================================
```

### Formato del listado de salida "DESCARTADOS.TXT"
```
====================================================
Apellido y Nombre      | Materia    | Cantidad
====================================================
AAAAAAAAAA, AAAAAAAAAA | CCCCCCCCCC | 0
BBBBBBBBBB, BBBBBBBBBB | CCCCCCCCCC | 0
CCCCCCCCCC, CCCCCCCCCC | CCCCCCCCCC | 0 
====================================================
```

### Fecha de entrega (No hay clases estos días)
* Aula 1 y 2: Jueves 12 de Octubre de 2023
* Aula 3 y 4: Viernes 13 de Octubre de 2023


## Integrantes del Grupo:
* PIOVANO, PAOLA – AULA X
* CARDELLINO, MARIELA - AULA 2
* SUAREZ, MARIANO - AULA X
* WEICMAN, DIEGO - AULA X
* GUZMAN, WINSTON - AULA 2