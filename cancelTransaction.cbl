       IDENTIFICATION DIVISION.
       PROGRAM-ID. cancelTransaction.
       AUTHOR. Pedro Rossi.
       DATE-WRITTEN. 12/05/2026.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT TRANSACTIONS ASSIGN TO "transactions.IDX"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TR-KEY
           FILE STATUS IS TRANSACTIONS-CHECK-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTIONS.
       01  FD-TRANSACTION-DETAILS.
           05 TR-KEY.
               10 TR-ID              PIC 9(5).               
               10 TR-ACC-ID          PIC 9(4).
           05 TR-TYPE                PIC X(6).
           05 TR-AMOUNT              PIC 9(9)V99.
           05 TR-DESC                PIC X(20).
           05 TR-DATE                PIC X(10).
           05 TR-STATUS              PIC X(8).

       WORKING-STORAGE SECTION.
       01  WS-FOUND-FLAG              PIC X VALUE "N".
           88  FOUND                  VALUE "Y".
           88  NOT-FOUND              VALUE "N".
       01  WS-END-OF-FILE             PIC X VALUE "N".
           88  EOF                    VALUE "Y".
           88  NOT-EOF                VALUE "N".
       01  TRANSACTIONS-CHECK-STATUS  PIC XX.
       01  WS-CANCELED-ID              PIC 9(5).

       PROCEDURE DIVISION.

       0100-MAIN.
           OPEN I-O TRANSACTIONS

           IF TRANSACTIONS-CHECK-STATUS NOT = "00"
               DISPLAY "ERROR OPENING FILE: "
                       TRANSACTIONS-CHECK-STATUS
               GO TO 0900-END-PROGRAM
           END-IF

           PERFORM 0150-PROMPT-USER

           SET NOT-EOF TO TRUE
           SET NOT-FOUND TO TRUE

           PERFORM UNTIL EOF OR FOUND
               READ TRANSACTIONS NEXT RECORD
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM 0200-CANCEL-TRANSACTION
               END-READ
           END-PERFORM

           IF NOT-FOUND
              DISPLAY "TRANSACTION DOES NOT EXIST."
           END-IF

           PERFORM 0900-END-PROGRAM.

       0100-END.

       0150-PROMPT-USER.
           DISPLAY "ENTER THE ID OF THE TRANSACTION YOU WISH TO CANCEL:"
           ACCEPT WS-CANCELED-ID.
       0150-END.

       0200-CANCEL-TRANSACTION.
           IF TR-ID = WS-CANCELED-ID AND NOT-FOUND
              SET FOUND TO TRUE

              IF TR-STATUS = "CANCELED"
                 DISPLAY "THIS TRANSACTION HAD ALREADY BEEN CANCELED."
                 PERFORM 0900-END-PROGRAM
              END-IF

              MOVE "CANCELED" TO TR-STATUS
              REWRITE FD-TRANSACTION-DETAILS
              DISPLAY "TRANSACTION CANCELED SUCCESSFULLY!"
           END-IF.
       0200-END.

       0900-END-PROGRAM.
           SET NOT-FOUND TO TRUE
           SET NOT-EOF TO TRUE
           CLOSE TRANSACTIONS
           EXIT PROGRAM.
       0900-END.
