       01 WS-SALIDA-HONOR.

          05 WS-SAL-HONOR-SEPARADOR          PIC X(133) VALUES ALL "-".

          05 WS-SAL-HONOR-TITULOS.
             
             10 FILLER                      PIC X(30) VALUE 'Materia'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(40)
                                              VALUE 'Apellido y Nombre'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(10) VALUE 'Promedio'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             

          05 WS-SAL-HONOR-DETALLE.
             10 WS-SAL-HONOR-MATERIA   PIC X(30).
             10 FILLER                 PIC X(03) VALUE ' | '.
             10 WS-SAL-HONOR-NOMBRE    PIC X(40).
             10 FILLER                 PIC X(06) VALUE '    | '.
             10 WS-SAL-HONOR-PROMEDIO  PIC 9(5)V9(3).
             10 FILLER                 PIC X(03) VALUE ' | '.
             