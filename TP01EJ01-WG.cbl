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
       
       FD SALIDA-DESCARTADOS.
       01 WS-SAL-ARCH-APAREO                   PIC X(84).

       FD SALIDA-ERROR.
       01 WS-SAL-ARCH-ERROR                    PIC X(84).

       FD SALIDA-HONOR.
       01 WS-SAL-ARCH-HONOR                    PIC X(84).

       FD SALIDA-PROMEDIO.
       01 WS-SAL-ARCH-PROMEDIO                 PIC X(88).


      
       WORKING-STORAGE SECTION.
       

       COPY DESCARTADOS.

       COPY ERROR.

       COPY HONOR.

       COPY PROMEDIOS.

       01 FS-STATUS.
          05 FS-ENT-ALUMNOS                 PIC X(2).
             88 FS-ENT-ALUMNOS-OK                         VALUE '00'.
             88 FS-ENT-ALUMNOS-EOF                        VALUE '10'.
             88 FS-ENT-ALUMNOS-NFD                        VALUE '35'.

          05 FS-SALIDA-PROMEDIO         PIC X(02).
             88 FS-SALIDA-PROMEDIO-OK                 VALUE '00'.
             88 FS-SALIDA-PROMEDIO-EOF                VALUE '10'.
             88 FS-SALIDA-PROMEDIO-NFD                VALUE '35'. 


          05 FS-SALIDA-DESCARTADOS      PIC X(02).
             88 FS-SALIDA-DESCARTADOS-OK VALUE      '00'.
             88 FS-SALIDA-DESCARTADOS-EOF       VALUE '10'.
             88 FS-SALIDA-DESCARTADOS-NFD             VALUE '35'.

          05 FS-SALIDA-ERROR   PIC X(02).
             88 FS-SALIDA-ERROR-OK                    VALUE '00'.
             88 FS-SALIDA-ERROR-EOF                   VALUE '10'.
             88 FS-SALIDA-ERROR-NFD                   VALUE '35'.

          05 FS-SALIDA-HONOR            PIC X(02).
             88 FS-SALIDA-HONOR-OK                    VALUE '00'.
             88 FS-SALIDA-HONOR-EOF                   VALUE '10'.
             88 FS-SALIDA-HONOR-NFD                   VALUE '35'.

          05 WS-MATERIA-VALIDA          PIC X(1).
             88 WS-MATERIA-VALIDA-YES                 VALUE 'Y'.
             88 WS-MATERIA-VALIDA-NO                  VALUE 'N'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-ENTRADA        PIC 9(5)      VALUE 0.
          05 WS-CONT-REGISTROS-CORRECTOS
                                        PIC 9(5)      VALUE 0.
          05 WS-CONT-REGISTRO-DESCARTADO
                                        PIC 9(5)      VALUE 0.
          05 WS-CONT-REGISTROS-ERROR    PIC 9(5)      VALUE 0.
          05 WS-CONT-MATERIA-PRESENTADA PIC 9(1)      VALUE 0.

       01 WS-ESTUDIANTE-HONOR.
          05 WS-ESTUDIANTE-HONOR        PIC X(40)     VALUE " ".
          05 WS-ESTUDIANTE-MAT-HONOR    PIC X(30)     VALUE " ".
          05 WS-ESTUDIANTE-PROM-HONOR   PIC 9(5)V9(2) VALUE 0.
          
       01 WS-NOMBRE-MATERIA             PIC X(30) OCCURS 20 TIMES.
          77 WS-INDICE                     PIC 9         VALUE 1.
       01 WS-PROMEDIO                   PIC 9(5)V9(2) VALUE 0.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *     MOVE 'Economía' TO WS-NOMBRE-MATERIA(1).
      *     MOVE 'Física' TO WS-NOMBRE-MATERIA(2).
      *     MOVE 'Informática' TO WS-NOMBRE-MATERIA(3).
      *     MOVE 'Inglés' TO WS-NOMBRE-MATERIA(4).
      *     MOVE 'Matemáticas' TO WS-NOMBRE-MATERIA(5).
      *     MOVE 'Química' TO WS-NOMBRE-MATERIA(6).
           
      *     IF NOMBRE IS ALPHA
      *        THEN DISPLAY "El campo es alfabético"
      *        ELSE DISPLAY "El campo no es alfabético".

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
                ADD 1 TO WS-CONT-REG-ENTRADA
      *          ADD ENT-IMPORTE TO WS-CC-IMPORTE-ACUM-TOTAL 
      *          MOVE WS-CC-IMPORTE-ACUM-TOTAL TO SAL-REPORTE 
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

      *     INITIALIZE WS-ACUMULADORES.
                      

      *     MOVE ENT-FECHA TO WS-CC-FECHA-ANT.
           
      *     MOVE WS-LIS-SEPARADOR-2 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      *     MOVE " " TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

      *     STRING WS-LIS-HEAD-FECHA DELIMITED BY SIZE
      *            WS-CC-FECHA-ANT DELIMITED BY SIZE INTO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

      *     MOVE WS-LIS-SEPARADOR-1 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      *     MOVE WS-LIS-HEADER TO SAL-REPORTE. 
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      *     MOVE WS-LIS-SEPARADOR-1 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      
           PERFORM 2100-PROCESAR-CORTES-FECHA
              THRU 2100-PROCESAR-CORTES-FECHA-FIN
              UNTIL FS-ENT-ALUMNOS-EOF 
      *        OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT
           .
           
      *     MOVE WS-CC-CANT-VENTAS-ACUM-T TO WS-LIS-D-CANTIDAD-T.
      *     MOVE WS-LIS-D-CANTIDAD-T TO SAL-REPORTE.
           
      *     MOVE WS-CC-IMPORTE-ACUM-T TO WS-LIS-D-TOTAL-T.
      *     MOVE WS-LIS-D-TOTAL-T TO SAL-REPORTE.
           


      *     MOVE WS-LIS-SEPARADOR-1 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      
      *     MOVE WS-LIS-FOOTER TO SAL-REPORTE.   
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-CORTES-FECHA.

           

           PERFORM 2200-PROCESAR-CORTE-CATEG
              THRU 2200-PROCESAR-CORTE-CATEG-FIN
              UNTIL FS-ENT-ALUMNOS-EOF 
      *       OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT
      *       OR ENT-CATEGORIA NOT EQUAL WS-CC-CATEGORIA-ANT.
           

           

           
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

           
           
           
       2100-PROCESAR-CORTES-FECHA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-CORTE-CATEG.

           
           

           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

           
      
       2200-PROCESAR-CORTE-CATEG-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.
           
           WRITE WS-SAL-ARCH-PROMEDIO.
           

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
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      *     MOVE WS-CONT-REG-ENTRADA TO WS-LIS-D-CANTIDAD-T.
      *     MOVE WS-LIS-D-CANTIDAD-T TO SAL-REPORTE.
      
      *     MOVE WS-CC-IMPORTE-ACUM-TOTAL TO WS-LIS-D-TOTAL-T.
      *     MOVE WS-LIS-D-TOTAL-T TO SAL-REPORTE.
           
      *     MOVE WS-LIS-FOOTER TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
           
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


      *----------------------------------------------------------------*

       END PROGRAM TP01EJ01.
