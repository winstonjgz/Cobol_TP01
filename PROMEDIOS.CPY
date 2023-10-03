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
             