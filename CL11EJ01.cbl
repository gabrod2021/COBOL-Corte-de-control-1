      ******************************************************************
      * Author: GABRIELA RODRIGUEZ
      * Date: 07/09/2023
      * Purpose: CORTE DE CONTROL SIMPLE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL11EJ01.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT ENTRADA
           ASSIGN TO '../AGOSTO.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-ENTRADA.
      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD ENTRADA.
       01 ENT-ARCHIVO.
          05 ENT-FECHA                      PIC X(10).
          05 ENT-ID-EMPLEADO                PIC 9(05).
          05 ENT-NOMBRE-APELLIDO            PIC X(40).
          05 ENT-CATEGORIA                  PIC X(20).
          05 ENT-IMPORTE                    PIC 9(8)V9(2).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-ENTRADA                      PIC X(2).
             88 FS-ENTRADA-OK                    VALUE '00'.
             88 FS-ENTRADA-EOF                   VALUE '10'.
             88 FS-ENTRADA-NFD                   VALUE '35'.

       01 WS-CONTADORES.
          05 WS-CONT-REG-ENTRADA             PIC 9(5) VALUE 0.

       01 WS-CORTE-CONTROL.
          05 WS-CC-FECHA-ANT                 PIC X(10).

       01 WS-ACUMULADORES.
          05 WS-CC-IMPORTE-ACUM              PIC 9(8)V9(2).
          05 WS-CC-CANT-VENTAS-ACUM          PIC 9(04).

       01 WS-LISTADO.
          05 WS-LIS-SEPARADOR               PIC X(37) VALUE ALL '-'.
          05 WS-LIS-HEADER.
             10 FILLER                      PIC X(10) VALUE 'FECHA'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(08) VALUE 'CANTIDAD'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(13) VALUE 'IMPORTE'.
          05 WS-LIS-DETALLE.
             10 WS-LIS-D-FECHA              PIC X(10).
             10 FILLER                      PIC X(07) VALUE ' |     '.
             10 WS-LIS-D-CANTIDAD           PIC ZZZ9.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-LIS-D-IMPORTE            PIC ZZ.ZZZ.ZZ9,99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR-PROGRAMA
              THRU 1000-INICIAR-PROGRAMA-FIN.

           IF FS-ENTRADA-OK

              DISPLAY WS-LIS-HEADER
              DISPLAY WS-LIS-SEPARADOR

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
       1500-LEER-ARCHIVO.

           READ ENTRADA.

           EVALUATE TRUE
               WHEN FS-ENTRADA-OK
                    ADD 1                   TO WS-CONT-REG-ENTRADA
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

           INITIALIZE WS-ACUMULADORES.

           MOVE ENT-FECHA                   TO WS-CC-FECHA-ANT.

           PERFORM 2100-ACUMULAR-DATOS
              THRU 2100-ACUMULAR-DATOS-FIN
             UNTIL FS-ENTRADA-EOF
                OR ENT-FECHA NOT EQUAL WS-CC-FECHA-ANT.

           PERFORM 2200-MOSTAR-DATO-PARCIAL
              THRU 2200-MOSTAR-DATO-PARCIAL-FIN.

       2000-PROCESAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2100-ACUMULAR-DATOS.

           ADD ENT-IMPORTE                  TO WS-CC-IMPORTE-ACUM.
           ADD 1                            TO WS-CC-CANT-VENTAS-ACUM.

           MOVE ENT-FECHA                   TO WS-CC-FECHA-ANT.

           PERFORM 1500-LEER-ARCHIVO
              THRU 1500-LEER-ARCHIVO-EXIT.

       2100-ACUMULAR-DATOS-FIN.
           EXIT.
      *----------------------------------------------------------------*
       2200-MOSTAR-DATO-PARCIAL.

           MOVE WS-CC-FECHA-ANT             TO WS-LIS-D-FECHA.
           MOVE WS-CC-CANT-VENTAS-ACUM      TO WS-LIS-D-CANTIDAD.
           MOVE WS-CC-IMPORTE-ACUM          TO WS-LIS-D-IMPORTE.

           DISPLAY WS-LIS-DETALLE.

       2200-MOSTAR-DATO-PARCIAL-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.

           DISPLAY WS-LIS-SEPARADOR.

           DISPLAY 'CANTIDAD DE REGISTROS LEIDOS: ' WS-CONT-REG-ENTRADA.

           PERFORM 3200-CERRAR-ARCHIVO
              THRU 3200-CERRAR-ARCHIVO-FIN.

       3000-FINALIZAR-PROGRAMA-FIN.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVO.

           CLOSE ENTRADA.

           IF NOT FS-ENTRADA-OK
              DISPLAY 'ERROR AL CERRAR ARCHUIVO ENTRADA: ' FS-ENTRADA
           END-IF.

       3200-CERRAR-ARCHIVO-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL11EJ01.
