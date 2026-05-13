       IDENTIFICATION DIVISION.
       PROGRAM-ID. listTransactions.
       AUTHOR. Pedro Rossi.
       DATE-WRITTEN. 29/04/2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT TRANSACTIONS ASSIGN TO "transactions.IDX"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS TR-KEY
           FILE STATUS IS TRANSACTIONS-CHECK-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD TRANSACTIONS.
       01 FS-TRANS-IDX.
           05 TR-KEY.
               10 TR-ID        PIC 9(5).               
               10 TR-ACC-ID    PIC 9(4).
           05 TR-TYPE          PIC X(6).
           05 TR-AMOUNT        PIC 9(9)V99.
           05 TR-DESC          PIC X(20).
           05 TR-DATE          PIC X(10).
           05 TR-STATUS        PIC X(8).

       WORKING-STORAGE SECTION.

       01 WS-END-OF-FILE PIC X VALUE 'N'.
           88 EOF     VALUE 'Y'.
           88 NOT-EOF VALUE 'N'.

       01 TRANSACTIONS-CHECK-STATUS PIC XX.

       01 WS-HEADER-01.
           02 FILLER PIC X(2) VALUE 'ID'.
           02 FILLER PIC X(6).
           02 FILLER PIC X(10) VALUE 'ACCOUNT ID'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(6) VALUE 'TYPE'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(6) VALUE 'AMOUNT'.
           02 FILLER PIC X(8).
           02 FILLER PIC X(11) VALUE 'DESCRIPTION'.
           02 FILLER PIC X(12).
           02 FILLER PIC X(10) VALUE 'CREATED-AT'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(6) VALUE 'STATUS'.

       01 WS-HEADER-02.
           02 FILLER PIC X(5) VALUE '-----'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(10) VALUE '----------'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(6) VALUE '------'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(12) VALUE '-----------'.
           02 FILLER PIC X(2).
           02 FILLER PIC X(20) VALUE '--------------------'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(10) VALUE '----------'.
           02 FILLER PIC X(3).
           02 FILLER PIC X(10) VALUE '----------'.

       01 WS-DETAIL-LINE.
           02 WS-ID          PIC 9(5).
           02 FILLER         PIC X(3).
           02 WS-ACCOUNT-ID  PIC 9(4).
           02 FILLER         PIC X(9).
           02 WS-TYPE        PIC X(6).
           02 FILLER         PIC X(3).
           02 WS-AMOUNT      PIC 9(9)V99.
           02 FILLER         PIC X(3).
           02 WS-DESCRIPTION PIC X(20).
           02 FILLER         PIC X(3).
           02 WS-CREATED-AT  PIC X(10).
           02 FILLER         PIC X(3).
           02 WS-STATUS      PIC X(8).

       PROCEDURE DIVISION.

       0100-MAIN.
           SET NOT-EOF TO TRUE
           OPEN INPUT TRANSACTIONS

           IF TRANSACTIONS-CHECK-STATUS NOT = "00"
               DISPLAY "ERROR OPENING FILE: "
                       TRANSACTIONS-CHECK-STATUS
               GO TO 0900-END-PROGRAM
           END-IF

           DISPLAY WS-HEADER-01
           DISPLAY WS-HEADER-02

           PERFORM UNTIL EOF
               READ TRANSACTIONS NEXT RECORD
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM 0200-PROCESS
               END-READ
           END-PERFORM

           PERFORM 0900-END-PROGRAM.
       0100-END.

       0200-PROCESS.
           MOVE TR-ID     OF FS-TRANS-IDX TO WS-ID
           MOVE TR-ACC-ID OF FS-TRANS-IDX TO WS-ACCOUNT-ID
           MOVE TR-TYPE   OF FS-TRANS-IDX TO WS-TYPE
           MOVE TR-AMOUNT OF FS-TRANS-IDX TO WS-AMOUNT
           MOVE TR-DESC   OF FS-TRANS-IDX TO WS-DESCRIPTION
           MOVE TR-DATE   OF FS-TRANS-IDX TO WS-CREATED-AT
           MOVE TR-STATUS OF FS-TRANS-IDX TO WS-STATUS
           DISPLAY WS-DETAIL-LINE.
       0200-END.

       0900-END-PROGRAM.
           CLOSE TRANSACTIONS
           EXIT PROGRAM.
       0900-END.
       