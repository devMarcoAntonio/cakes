      *****************************
      *Author: DEV Marco Antonio  *
      *Propose: Time Travel       * 
      *****************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. BOLINHO.
           AUTHOR. Dev Marco.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO ASSIGN TO DISK
           ORGANIZATION        IS INDEXED
           ACCESS MODE         IS DYNAMIC
           RECORD KEY          IS SKU
           ALTERNATE RECORD KEY    IS NOME WITH DUPLICATES
           FILE STATUS             IS WS-STATUS-ARQUIVO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "BOLINHO.DAT".

           01 PRODUTO.
               05 SKU              PIC 9(04).
               05 NOME             PIC X(30).
               05 VALIDADE.
                   10 DIA          PIC 99.
                   10 MES          PIC 99.
                   10 ANO          PIC 9(04).
               05 VALOR-CUSTO      PIC 9(04)V99.
               05 VALOR-VENDA      PIC 9(04)V99.
               05 QTD-ESTOQUE      PIC 9(04).

       WORKING-STORAGE SECTION.
           77 WS-STATUS-ARQUIVO    PIC X(02).
           77 WS-MENSAGEM          PIC X(50) VALUE SPACES.
           77 WS-TEMPORIZADOR      PIC 9(05) VALUE ZEROS.

       SCREEN SECTION.

       PROCEDURE DIVISION.
      ***********************
      * PARAGRAFO PRINCIPAL  *
      *********************** 
       P-ABERTURA.
           OPEN I-O ARQUIVO
           IF WS-STATUS-ARQUIVO NOT = "00"
               IF WS-STATUS-ARQUIVO = "30"
                   OPEN OUTPUT ARQUIVO
                   MOVE "O arquivo esta sendo criado..." TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   CLOSE ARQUIVO
                   GO TO P-ABERTURA
                ELSE
                   GO TO P-FIM-EXIT.
      ***********************
      * PARAGRAFO MENSAGEM  *
      *********************** 
       P-MSG-ZERA.
           MOVE ZEROS TO WS-TEMPORIZADOR.

       P-MSG-DISPLAY.
           DISPLAY WS-MENSAGEM.
       P-MSG-TEMPO.
           ADD 1 TO WS-TEMPORIZADOR
           IF WS-TEMPORIZADOR < 2500
               GO TO P-MSG-TEMPO.
       P-MSG-FIM.
           MOVE SPACE TO WS-MENSAGEM
           EXIT.
      ***********************
      * PARAGRAFO MENSAGEM  *
      *********************** 
       P-FIM-EXIT.
           EXIT PROGRAM.

       P-FIM-STOP-RUN.
           STOP RUN.
           
               