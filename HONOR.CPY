       FD SALIDA-HONOR.
       
       01 WS-SAL-HONOR-DETALLE.
          05 WS-SAL-HONOR-MATERIA   PIC X(30).
          05 FILLER                 PIC X(03) VALUE ' | '.
          05 WS-SAL-HONOR-NOMBRE    PIC X(40).
          05 FILLER                 PIC X(06) VALUE '    | '.
          05 WS-SAL-HONOR-PROMEDIO  PIC 9(5)V9(3).
          05 FILLER                 PIC X(03) VALUE ' | '.
             
       01 WS-ESTUDIANTE-HONOR.
          05 WS-ESTUDIANTE-HONOR        PIC X(40)     VALUE " ".
          05 WS-ESTUDIANTE-MAT-HONOR    PIC X(30)     VALUE " ".
          05 WS-ESTUDIANTE-PROM-HONOR   PIC 9(5)V9(2) VALUE 0.