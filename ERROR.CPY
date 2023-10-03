       01 WS-SALIDA-ERROR.

          05 WS-SAL-ERROR-SEPARADOR          PIC X(133) VALUES ALL "-".

          05 WS-SAL-ERROR-TITULOS.
             10 FILLER                      PIC X(10)
                                              VALUE 'Fecha'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(40)
                                              VALUE 'Apellido y Nombre'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(30) VALUE 'Materia'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(10) VALUE 'Cantidad'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             

          05 WS-SAL-ERROR-DETALLE.
             10 WS-SAL-ERROR-FECHA      PIC X(10).
             10 FILLER                      PIC X(06) VALUE '    | '.
             10 WS-SAL-ERROR-NOMBRE      PIC X(40).
             10 FILLER                      PIC X(06) VALUE '    | '.
             10 WS-SAL-ERROR-MATERIA           PIC X(30).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-SAL-ERROR-CANTIDAD        PIC X(10).
             10 FILLER                      PIC X(03) VALUE ' | '.
             