       FD SALIDA-PROMEDIO.
       01 WS-SALIDA-PROMEDIO.

          05 WS-SAL-PROMEDIO-SEPARADOR  PIC X(133)    VALUES ALL   "-".

          05 WS-SAL-PROMEDIO-TITULOS.
             10 FILLER                  PIC X(40)
                                                      VALUE 
                                                    'Apellido y Nombre'.
             10 FILLER                  PIC X(03)     VALUE ' | '.
             10 FILLER                  PIC X(30)     VALUE 'Materia'.
             10 FILLER                  PIC X(03)     VALUE ' | '.
             10 FILLER                  PIC X(10)     VALUE 'Promedio'.
             10 FILLER                  PIC X(03)     VALUE ' | '.
             

          05 WS-SAL-PROMEDIO-DETALLE.
             10 WS-SAL-PROMEDIO-NOMBRE  PIC X(40).
             10 FILLER                  PIC X(06)     VALUE '    | '.
             10 WS-SAL-PROMEDIO-MATERIA PIC X(30).
             10 FILLER                  PIC X(03)     VALUE ' | '.
             
             10 WS-SAL-PROMEDIO-PROMEDIO
                                        PIC 9(5)V9(3).
             10 FILLER                  PIC X(03)     VALUE ' | '.
       
       01 WS-ESTUDIANTE-PROMEDIO.
          05 WS-EST-PROM-NOMBRE       PIC X(40)    VALUE " ".
          05 WS-EST-PROM-MATERIA      PIC X(30)     VALUE " ".
          05 WS-EST-PROM-SUM          PIC 9(5)V9(2) VALUE 0.
          05 WS-EST-PROM-CANT-MAT     PIC 9(1) VALUE 0.
          05 WS-EST-PROM-PROM         PIC 9(5)V9(2) VALUE 0.