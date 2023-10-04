      ******************************************************************
      * Equipo:     PIOVANO, PAOLA – AULA 2
      *             CARDELLINO, MARIELA - AULA 2
      *             SUAREZ, MARIANO - AULA 1
      *             WAICMAN, DIEGO - AULA 1
      *             GUZMAN, WINSTON - AULA 2
      * Date:       29/09/2023
      * Purpose:    TRABAJO PRACTICO DE COBOL 01 (SILVERTECH)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP01EJ01.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
           SELECT ENT-ALUMNOS
           ASSIGN TO '../ALUMNOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENT-ALUMNOS.

           SELECT SALIDA-PROMEDIO
           ASSIGN TO '../PROMEDIO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA-PROMEDIO.

           SELECT SALIDA-DESCARTADOS
           ASSIGN TO '../DESCARTADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA-DESCARTADOS.

           SELECT SALIDA-HONOR
           ASSIGN TO '../HONOR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA-HONOR.

           SELECT SALIDA-ERROR
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SALIDA-ERROR.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       COPY ALUMNOS.

       COPY DESCARTADOS.

       COPY ERROR.

       COPY HONOR.

       COPY PROMEDIOS.

       01 WS-SAL-ARCH-DESCARTADOS PIC X(84).

       01 WS-SAL-ARCH-ERROR PIC X(84).

       01 WS-SAL-ARCH-HONOR   PIC X(84).

       01 WS-SAL-ARCH-PROMEDIO   PIC X(84).
       WORKING-STORAGE SECTION.

       

       01 FS-STATUS.
          05 FS-ENT-ALUMNOS                       PIC X(2).
             88 FS-ENT-ALUMNOS-OK                         VALUE '00'.
             88 FS-ENT-ALUMNOS-EOF                        VALUE '10'.
             88 FS-ENT-ALUMNOS-NFD                        VALUE '35'.

          05 FS-SALIDA-PROMEDIO                   PIC X(02).
             88 FS-SALIDA-PROMEDIO-OK                 VALUE '00'.
             88 FS-SALIDA-PROMEDIO-EOF                VALUE '10'.
             88 FS-SALIDA-PROMEDIO-NFD                VALUE '35'.


          05 FS-SALIDA-DESCARTADOS                PIC X(02).
             88 FS-SALIDA-DESCARTADOS-OK              VALUE '00'.
             88 FS-SALIDA-DESCARTADOS-EOF             VALUE '10'.
             88 FS-SALIDA-DESCARTADOS-NFD             VALUE '35'.

          05 FS-SALIDA-ERROR                      PIC X(02).
             88 FS-SALIDA-ERROR-OK                    VALUE '00'.
             88 FS-SALIDA-ERROR-EOF                   VALUE '10'.
             88 FS-SALIDA-ERROR-NFD                   VALUE '35'.

          05 FS-SALIDA-HONOR                      PIC X(02).
             88 FS-SALIDA-HONOR-OK                    VALUE '00'.
             88 FS-SALIDA-HONOR-EOF                   VALUE '10'.
             88 FS-SALIDA-HONOR-NFD                   VALUE '35'.

          05 WS-MATERIA-VALIDA                    PIC X(1).
             88 WS-MATERIA-VALIDA-YES                 VALUE 'Y'.
             88 WS-MATERIA-VALIDA-NO                  VALUE 'N'.

       01 WS-ENT-ALUMNOS-EOF                   PIC X(1).
          88 WS-ENT-ALUMNOS-EOF-YES               VALUE 'Y'.
          88 WS-ENT-ALUMNOS-EOF-NO                VALUE 'N'.
       01 WS-SAL-PROMEDIO-EOF                  PIC X(1).
          88 WS-SAL-PROMEDIO-EOF-YES              VALUE 'Y'.
          88 WS-SAL-PROMEDIO-EOF-NO               VALUE 'N'.
       01 WS-SAL-DESCARTADOS-EOF               PIC X(1).
          88 WS-SAL-DESCARTADOS-EOF-YES           VALUE 'Y'.
          88 WS-SAL-DESCARTADOS-EOF-NO            VALUE 'N'.
       01 WS-SAL-HONOR-EOF                     PIC X(1).
          88 WS-SAL-HONOR-EOF-YES                 VALUE 'Y'.
          88 WS-SAL-HONOR-EOF-NO                  VALUE 'N'.
       01 WS-SAL-ERROR-EOF                     PIC X(1).
          88 WS-SAL-ERROR-EOF-YES                 VALUE 'Y'.
          88 WS-SAL-ERROR-EOF-NO                  VALUE 'N'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-ENTRADA         PIC 9(5)      VALUE 0.
          05 WS-CONT-REGISTROS-CORRECTOS
                                         PIC 9(5)      VALUE 0.
          05 WS-CONT-REGISTRO-DESCARTADO
                                         PIC 9(5)      VALUE 0.
          05 WS-CONT-REGISTROS-ERROR     PIC 9(5)      VALUE 0.
          05 WS-CONT-MATERIA-PRESENTADA  PIC 9(1)      VALUE 0.


       01 WS-NOMBRE-MATERIA              PIC X(30) OCCURS 20 TIMES.
          77 WS-INDICE                   PIC 9(1)  VALUE 1.
       01 WS-PROMEDIO                    PIC 9(5)V9(2) VALUE 0.


      *--------------------------------------------------------------
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
             

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.


           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

              IF FS-ENT-ALUMNOS-OK


                 PERFORM 2000-PROCESAR-PROGRAMA
                    THRU 2000-PROCESAR-PROGRAMA-FIN
                    UNTIL FS-ENT-ALUMNOS-EOF

              END-IF.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-FIN.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-ARCHIVO
              THRU 1100-ABRIR-ARCHIVO-FIN.

           PERFORM 1200-ABRIR-ARCHIVO-SALIDA
              THRU 1200-ABRIR-ARCHIVO-SALIDA-FIN.



       1000-INICIAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-ARCHIVO.

           OPEN INPUT ENT-ALUMNOS.

           EVALUATE FS-ENT-ALUMNOS
           WHEN '00'
                PERFORM 1500-LEER-ARCHIVO
                   THRU 1500-LEER-ARCHIVO-EXIT
           WHEN '35'
                DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                DISPLAY 'FILE STATUS: ' FS-ENT-ALUMNOS
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                DISPLAY 'FILE STATUS: ' FS-ENT-ALUMNOS
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-SALIDA.
           OPEN OUTPUT SALIDA-PROMEDIO.

           EVALUATE TRUE
           WHEN FS-SALIDA-PROMEDIO-OK
                CONTINUE
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA-PROMEDIOS'
                DISPLAY 'FILE STATUS: ' FS-SALIDA-PROMEDIO
           END-EVALUATE.
      *---------------------------------------------------------
           OPEN OUTPUT SALIDA-DESCARTADOS.

           EVALUATE TRUE
           WHEN FS-SALIDA-DESCARTADOS-OK
                CONTINUE
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR ARCHIVO DE SALIDA-DESCARTADOS'
                DISPLAY 'FILE STATUS: ' FS-SALIDA-DESCARTADOS
           END-EVALUATE.
      *---------------------------------------------------------
           OPEN OUTPUT SALIDA-ERROR.

           EVALUATE TRUE
           WHEN FS-SALIDA-ERROR-OK
                CONTINUE
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA-ERROR'
                DISPLAY 'FILE STATUS: ' FS-SALIDA-ERROR
           END-EVALUATE.
      *---------------------------------------------------------
           OPEN OUTPUT SALIDA-HONOR.

           EVALUATE TRUE
           WHEN FS-SALIDA-HONOR-OK
                CONTINUE
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA-HONOR'
                DISPLAY 'FILE STATUS: ' FS-SALIDA-HONOR
           END-EVALUATE.
       1200-ABRIR-ARCHIVO-SALIDA-FIN.
           EXIT.


      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.

           READ ENT-ALUMNOS.

           EVALUATE TRUE
              WHEN FS-ENT-ALUMNOS-OK
                 IF (WS-ENT-ALUMNOS-FECHA-EXAMEN = " ") 
      *           OR
      *       (WS-ENT-ALUMNOS-NOMBRE = " " OR
      *        WS-ENT-ALUMNOS-NOMBRE IS NOT ALPHABETIC)
      *       OR (WS-ENT-ALUMNOS-MATERIA  = " "
      *       OR WS-ENT-ALUMNOS-MATERIA IS NOT ALPHABETIC)
      *       OR (WS-ENT-ALUMNOS-NOTA  = " ") OR
      *       WS-ENT-ALUMNOS-NOTA IS NOT NUMERIC
                    MOVE WS-ENT-ALUMNOS TO WS-SAL-ERROR-DETALLE
                    PERFORM 2200-PROCESAR-ERROR
                       THRU 2200-PROCESAR-ERROR-FIN
                    PERFORM 1500-LEER-ARCHIVO
                      THRU 1500-LEER-ARCHIVO-EXIT
                 END-IF
                 ADD 1 TO WS-CONT-REG-ENTRADA

              WHEN FS-ENT-ALUMNOS-EOF
                   CONTINUE
              WHEN OTHER
                   DISPLAY 'ERROR AL LEER EL ARCHIVO DE ENTRADA'
                   DISPLAY 'FILE STATUS: ' FS-ENT-ALUMNOS
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

           INITIALIZE WS-CONTADORES.
      *     IF (WS-ENT-ALUMNOS-FECHA-EXAMEN NOT = " ") AND
      *       (WS-ENT-ALUMNOS-NOMBRE NOT = " " AND
      *        WS-ENT-ALUMNOS-NOMBRE IS ALPHABETIC)
      *       AND (WS-ENT-ALUMNOS-MATERIA IS NOT = " "
      *       AND WS-ENT-ALUMNOS-MATERIA IS ALPHABETIC)
      *       AND (WS-ENT-ALUMNOS-NOTA NOT = " ")

      *         PERFORM 2400-PROCESAR-PROMEDIO
      *              THRU 2400-PROCESAR-PROMEDIO-FIN
      *           UNTIL FS-ENT-ALUMNOS-EOF  OR
      *           WS-ENT-ALUMNOS-NOMBRE NOT = WS-EST-PROM-NOMBRE


      *      END-IF.


           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-DESCARTADOS.






           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.




       2100-PROCESAR-DESCARTADOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-ERROR.
      
           WRITE WS-SAL-ARCH-ERROR.

           EVALUATE FS-SALIDA-ERROR
               WHEN '00'
                    ADD 1 TO WS-CONT-REGISTROS-ERROR
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO ERRORES'
                    DISPLAY 'FILE STATUS: ' FS-SALIDA-ERROR
           END-EVALUATE.



       2200-PROCESAR-ERROR-FIN.
           EXIT.

      *----------------------------------------------------------------*
       2300-PROCESAR-HONOR.




           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.



       2300-PROCESAR-HONOR-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2400-PROCESAR-PROMEDIO.

           MOVE WS-ENT-ALUMNOS-NOMBRE TO
              WS-EST-PROM-NOMBRE
           .
           MOVE WS-ENT-ALUMNOS-MATERIA TO
             WS-EST-PROM-MATERIA
           .
           PERFORM 2450-PROCESAR-PROMEDIO
              THRU 2450-PROCESAR-PROMEDIO-FIN
              UNTIL WS-ENT-ALUMNOS-NOMBRE NOT =
               WS-EST-PROM-NOMBRE OR
               WS-ENT-ALUMNOS-MATERIA NOT =
               WS-EST-PROM-MATERIA
           .

           IF WS-CONT-MATERIA-PRESENTADA NOT = 3

              PERFORM 2100-PROCESAR-DESCARTADOS
                 THRU 2100-PROCESAR-DESCARTADOS-FIN
           ELSE
              DIVIDE WS-EST-PROM-SUM BY WS-CONT-MATERIA-PRESENTADA
              GIVING WS-EST-PROM-PROM

              PERFORM 2500-GRABAR-ARCHIVO-SAL
                 THRU 2500-GRABAR-ARCHIVO-SAL-FIN

           END-IF.


       2400-PROCESAR-PROMEDIO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2450-PROCESAR-PROMEDIO.

           ADD 1 TO WS-CONT-MATERIA-PRESENTADA
           .
           ADD WS-ENT-ALUMNOS-NOTA   TO WS-EST-PROM-SUM
           .



       2450-PROCESAR-PROMEDIO-FIN.
           EXIT.
      *----------------------------------------------------------------*

       2500-GRABAR-ARCHIVO-SAL.

      *     WRITE WS-SAL-ARCH-PROMEDIO.


           EVALUATE FS-SALIDA-PROMEDIO
           WHEN '00'
                ADD 1 TO WS-CONT-REGISTROS-CORRECTOS
           WHEN OTHER
                DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                DISPLAY 'FILE STATUS: ' FS-SALIDA-PROMEDIO
           END-EVALUATE.
       2500-GRABAR-ARCHIVO-SAL-FIN.
           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

      *     MOVE WS-LIS-SEPARADOR-2 TO SAL-REPORTE.
      *     PERFORM 2500-GRABAR-ARCHIVO-SAL
      *        THRU 2500-GRABAR-ARCHIVO-SAL-FIN.



           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.

           CLOSE ENT-ALUMNOS.

           IF NOT FS-ENT-ALUMNOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: ' FS-ENT-ALUMNOS
           END-IF.

           CLOSE SALIDA-DESCARTADOS.
           IF NOT FS-SALIDA-DESCARTADOS-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: '
              FS-SALIDA-DESCARTADOS
           END-IF.

           CLOSE SALIDA-ERROR.
           IF NOT FS-SALIDA-ERROR-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: '
              FS-SALIDA-ERROR
           END-IF.

           CLOSE SALIDA-HONOR.
           IF NOT FS-SALIDA-HONOR-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: '
              FS-SALIDA-HONOR
           END-IF.

           CLOSE SALIDA-PROMEDIO.
           IF NOT FS-SALIDA-PROMEDIO-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: '
              FS-SALIDA-PROMEDIO
           END-IF.


       3200-CERRAR-ARCHIVO-FIN.
           EXIT.

           DISPLAY WS-EST-PROM-NOMBRE
           DISPLAY WS-EST-PROM-MATERIA
           DISPLAY WS-EST-PROM-PROM
           .

      *----------------------------------------------------------------*

       END PROGRAM TP01EJ01.
