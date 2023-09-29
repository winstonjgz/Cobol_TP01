      ******************************************************************
      * Equipo:     PIOVANO, PAOLA – AULA X
      *             CARDELLINO, MARIELA - AULA 2
      *             SUAREZ, MARIANO - AULA X
      *             WEICMAN, DIEGO - AULA X
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
           SELECT ENTRADA
           ASSIGN TO '../ALUMNOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.

           SELECT SAL-PROMEDIO
           ASSIGN TO '../PROMEDIO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-PROMEDIO.

           SELECT SAL-DESCARTADOS
           ASSIGN TO '../DESCARTADOS.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-DESCARTADOS.

           SELECT SAL-HONOR
           ASSIGN TO '../HONOR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-HONOR.

           SELECT SAL-ERROR
           ASSIGN TO '../ERROR.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-SAL-ERROR.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENTRADA.
       01 ENT-ARCHIVO.
          05 ENT-FECHA                  PIC X(10).
          05 ENT-ID-EMPLEADO            PIC 9(05).
          05 ENT-NOMBRE-APELLIDO        PIC X(40).
          05 ENT-CATEGORIA              PIC X(20).
          05 ENT-IMPORTE                PIC 9(8)V9(2).

       FD SALIDA.
       01 SAL-REPORTE PIC X(42)    .
                   
       
       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-ENTRADA                 PIC X(2).
             88 FS-ENTRADA-OK                          VALUE '00'.
             88 FS-ENTRADA-EOF                         VALUE '10'.
             88 FS-ENTRADA-NFD                         VALUE '35'.

          05 FS-SALIDA                  PIC X(02).
             88 FS-SALIDA-OK                           VALUE '00'.
             88 FS-SALIDA-EOF                          VALUE '10'.
             88 FS-SALIDA-NFD                          VALUE '35'. 

       01 WS-CONTADORES.
          05 WS-CONT-REG-ENTRADA        PIC 9(5)       VALUE 0.

       01 WS-CORTE-CONTROL.
          05 WS-CC-FECHA-ANT            PIC X(10).
          05 WS-CC-CATEGORIA-ANT        PIC X(20).

       01 WS-ACUMULADORES.
          05 WS-CC-IMPORTE-ACUM         PIC 9(8)V9(2).
          05 WS-CC-CANT-VENTAS-ACUM     PIC 9(04).
          05 WS-CC-IMPORTE-ACUM-T       PIC 9(8)V9(2).
          05 WS-CC-CANT-VENTAS-ACUM-T   PIC 9(04).
          05 WS-CC-IMPORTE-ACUM-TOTAL   PIC 9(9)V9(2).
          05 WS-SALIDA-CANT-REG         PIC 9(04).

       01 WS-LISTADO.
          05 WS-LIS-SEPARADOR-1         PIC X(42)      VALUE ALL '-'.
          05 WS-LIS-SEPARADOR-2         PIC X(42)      VALUE ALL '#'.
          05 WS-LIS-HEADER.
             10 FILLER                  PIC X(12)      VALUE 'CATEGORIA'
           .
             10 FILLER                  PIC X(03)      VALUE ' | '.
             10 FILLER                  PIC X(09)      VALUE 'CANTIDAD'.
             10 FILLER                  PIC X(03)      VALUE ' | '.
             10 FILLER                  PIC X(13)      VALUE 'IMPORTE'.
          05 WS-LIS-HEAD-FECHA.              
             10 WS-LIS-HEADER-FECHA-CAB PIC X(12)      VALUE 'FECHA DE '
           .
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-CATEGORIA      PIC X(12).
             10 FILLER                  PIC X(07)      VALUE ' |     '.
             10 WS-LIS-D-CANTIDAD       PIC ZZZZ9.
             10 FILLER                  PIC X(03)      VALUE ' | '.
             10 WS-LIS-D-IMPORTE        PIC ZZZ.ZZZ.ZZ9,99.
          05 WS-LIS-FOOTER.
             10 WS-LIS-D-CANT-FECHA     PIC X(12)      VALUE 'TOTAL: '.
             10 FILLER                  PIC X(06)      VALUE ' |    '.
             10 WS-LIS-D-CANTIDAD-T     PIC ZZ.ZZ9.
             10 FILLER                  PIC X(03)      VALUE ' | '.
             10 WS-LIS-D-TOTAL-T        PIC ZZZ.ZZZ.ZZ9,99.


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-ENTRADA-OK

              
              DISPLAY WS-LIS-SEPARADOR-1

              PERFORM 2000-PROCESAR-PROGRAMA
                 THRU 2000-PROCESAR-PROGRAMA-FIN
                 UNTIL FS-ENTRADA-EOF

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

           OPEN INPUT ENTRADA.

           EVALUATE FS-ENTRADA
           WHEN '00'
                PERFORM 1500-LEER-ARCHIVO
                   THRU 1500-LEER-ARCHIVO-EXIT
           WHEN '35'
                DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE ENTRADA'
                DISPLAY 'FILE STATUS: ' FS-ENTRADA
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE ENTRADA'
                DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1100-ABRIR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*
       1200-ABRIR-ARCHIVO-SALIDA.
           OPEN OUTPUT SALIDA.

           EVALUATE TRUE
           WHEN FS-SALIDA-OK
                CONTINUE
           WHEN OTHER
                DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE SALIDA'
                DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.
       1200-ABRIR-ARCHIVO-SALIDA-FIN.
           EXIT.


      *----------------------------------------------------------------*
       1500-LEER-ARCHIVO.

           READ ENTRADA.

           EVALUATE TRUE
           WHEN FS-ENTRADA-OK
                ADD 1 TO WS-CONT-REG-ENTRADA
                ADD ENT-IMPORTE TO WS-CC-IMPORTE-ACUM-TOTAL 
      *          MOVE WS-CC-IMPORTE-ACUM-TOTAL TO SAL-REPORTE 
           WHEN FS-ENTRADA-EOF
                CONTINUE
           WHEN OTHER
                DISPLAY 'ERROR AL LEER EL ARCHIVO DE ENTRADA'
                DISPLAY 'FILE STATUS: ' FS-ENTRADA
           END-EVALUATE.

       1500-LEER-ARCHIVO-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.

      *     INITIALIZE WS-ACUMULADORES.
           MOVE ZEROES TO WS-CC-IMPORTE-ACUM.
           MOVE ZEROES TO WS-CC-CANT-VENTAS-ACUM.
           MOVE ZEROES TO WS-CC-CANT-VENTAS-ACUM-T.
           MOVE ZEROES TO WS-CC-IMPORTE-ACUM-T.
           

           MOVE ENT-FECHA TO WS-CC-FECHA-ANT.
           
           MOVE WS-LIS-SEPARADOR-2 TO SAL-REPORTE .
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
           MOVE " " TO SAL-REPORTE .
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

           STRING WS-LIS-HEAD-FECHA DELIMITED BY SIZE 
                    WS-CC-FECHA-ANT DELIMITED BY SIZE INTO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

           MOVE WS-LIS-SEPARADOR-1 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
           MOVE  WS-LIS-HEADER TO SAL-REPORTE. 
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
           MOVE  WS-LIS-SEPARADOR-1 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      
           PERFORM 2100-PROCESAR-CORTES-FECHA
              THRU 2100-PROCESAR-CORTES-FECHA-FIN
              UNTIL FS-ENTRADA-EOF
              OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT
           .
           
           MOVE WS-CC-CANT-VENTAS-ACUM-T TO WS-LIS-D-CANTIDAD-T.
           MOVE WS-LIS-D-CANTIDAD-T TO SAL-REPORTE.
           
           MOVE WS-CC-IMPORTE-ACUM-T TO WS-LIS-D-TOTAL-T.
           MOVE WS-LIS-D-TOTAL-T TO SAL-REPORTE.
           


           MOVE WS-LIS-SEPARADOR-1 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
      
           MOVE WS-LIS-FOOTER TO SAL-REPORTE.   
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-PROCESAR-CORTES-FECHA.

           MOVE ZEROES TO WS-CC-IMPORTE-ACUM.
           MOVE ZEROES TO WS-CC-CANT-VENTAS-ACUM.
           
           
           MOVE ENT-CATEGORIA TO WS-CC-CATEGORIA-ANT.
           

           PERFORM 2200-PROCESAR-CORTE-CATEG
              THRU 2200-PROCESAR-CORTE-CATEG-FIN
              UNTIL FS-ENTRADA-EOF
              OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT
              OR ENT-CATEGORIA NOT EQUAL WS-CC-CATEGORIA-ANT.
           

           MOVE WS-CC-CATEGORIA-ANT TO WS-LIS-D-CATEGORIA.
           MOVE WS-LIS-D-CATEGORIA TO SAL-REPORTE.
           MOVE WS-CC-CANT-VENTAS-ACUM TO WS-LIS-D-CANTIDAD.
           MOVE WS-LIS-D-CANTIDAD TO SAL-REPORTE.
           MOVE WS-CC-IMPORTE-ACUM TO WS-LIS-D-IMPORTE.
           MOVE WS-LIS-D-IMPORTE TO SAL-REPORTE.
          

           MOVE WS-LIS-DETALLE TO SAL-REPORTE.
            PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.

           ADD WS-CC-CANT-VENTAS-ACUM TO WS-CC-CANT-VENTAS-ACUM-T. 
           ADD WS-CC-IMPORTE-ACUM TO WS-CC-IMPORTE-ACUM-T.
           
           
       2100-PROCESAR-CORTES-FECHA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR-CORTE-CATEG.

           ADD ENT-IMPORTE TO WS-CC-IMPORTE-ACUM.
           ADD 1 TO WS-CC-CANT-VENTAS-ACUM.

           MOVE ENT-FECHA TO WS-CC-FECHA-ANT.
           MOVE WS-CC-FECHA-ANT TO SAL-REPORTE.
           MOVE ENT-CATEGORIA TO WS-CC-CATEGORIA-ANT.
           MOVE WS-CC-CATEGORIA-ANT TO SAL-REPORTE.

           

           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

           
      
       2200-PROCESAR-CORTE-CATEG-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2500-GRABAR-ARCHIVO-SAL.
           
           WRITE SAL-REPORTE .
           

           EVALUATE FS-SALIDA
           WHEN '00'
                ADD 1 TO WS-SALIDA-CANT-REG
           WHEN OTHER
                DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE SALIDA'
                DISPLAY 'FILE STATUS: ' FS-SALIDA
           END-EVALUATE.
       2500-GRABAR-ARCHIVO-SAL-FIN.
           EXIT.
      
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           MOVE WS-LIS-SEPARADOR-2 TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
           MOVE WS-CONT-REG-ENTRADA TO WS-LIS-D-CANTIDAD-T.
           MOVE WS-LIS-D-CANTIDAD-T TO  SAL-REPORTE.
      
           MOVE WS-CC-IMPORTE-ACUM-TOTAL TO WS-LIS-D-TOTAL-T.
           MOVE WS-LIS-D-TOTAL-T TO  SAL-REPORTE.
           
           MOVE  WS-LIS-FOOTER TO SAL-REPORTE.
           PERFORM 2500-GRABAR-ARCHIVO-SAL
              THRU 2500-GRABAR-ARCHIVO-SAL-FIN.
           
           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.

           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: ' FS-ENTRADA
           END-IF.

            CLOSE SALIDA.
           IF NOT FS-SALIDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO ENTRADA: ' FS-SALIDA
           END-IF.
           

       3200-CERRAR-ARCHIVO-FIN.
           EXIT.


      *----------------------------------------------------------------*

       END PROGRAM TP01EJ01.