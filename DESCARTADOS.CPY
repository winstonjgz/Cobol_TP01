       
       01 SALIDA-DESCARTADO.
          05 WS-SAL-DESC-SEPARADOR    PIC X(133) VALUES ALL "-".
                 
          05 WS-SAL-DESC-TITULOS.
             10 FILLER                      PIC X(40)
                                              VALUE 'Apellido y Nombre'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(30) VALUE 'Materia'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(10) VALUE 'Cantidad'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             

          05 WS-SAL-DESC-DETALLE.
             10 WS-SAL-DESC-NOMBRE      PIC X(40).
             10 FILLER                      PIC X(06) VALUE '    | '.
             10 WS-SAL-DESC-MATERIA         PIC X(30).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-SAL-DESC-CANTIDAD        PIC X(10).
             10 FILLER                      PIC X(03) VALUE ' | '.
             
             