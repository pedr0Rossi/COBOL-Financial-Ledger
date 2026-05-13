       IDENTIFICATION DIVISION.
       PROGRAM-ID. closeAccount.
       AUTHOR. Pedro Rossi.
       DATE-WRITTEN. 28/04/2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT ACCOUNTS ASSIGN TO "accounts.IDX"
           ORGANIZATION IS INDEXED       
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS FS-ID           
           FILE STATUS IS ACCOUNTS-CHECK-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD  ACCOUNTS.
       01  FS-ACC.
           02 FS-ID           PIC 9999.
           02 FS-FULLNAME     PIC X(13).
           02 FS-TYPE         PIC X(10).
           02 FS-STATUS       PIC X(7).
           02 FS-CREATED-AT   PIC X(10).

       WORKING-STORAGE SECTION.

       01  ACCOUNTS-CHECK-STATUS     PIC XX.

       01  WS-SEARCH-ID              PIC 9999.

       01  WS-END-OF-FILE      PIC X VALUE 'N'.
           88  EOF             VALUE 'Y'.
           88  NOT-EOF         VALUE 'N'.

       01  WS-FOUND-FLAG      PIC X VALUE 'N'.
           88  FOUND          VALUE 'Y'.
           88  NOT-FOUND      VALUE 'N'.

       PROCEDURE DIVISION.

       0100-MAIN.
           SET NOT-FOUND TO TRUE
           SET NOT-EOF TO TRUE
           OPEN I-O ACCOUNTS

           IF ACCOUNTS-CHECK-STATUS  NOT = "00"
               DISPLAY "ERROR OPENING FILE: "
               GO TO 0900-END-PROGRAM
           END-IF

           DISPLAY "ENTER THE ID OF THE ACCOUNT YOU WISH TO CLOSE: "
           ACCEPT WS-SEARCH-ID

           SET NOT-EOF TO TRUE
           SET NOT-FOUND TO TRUE

           PERFORM UNTIL EOF
               READ ACCOUNTS
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM 0200-PROCESS-RECORD
               END-READ
           END-PERFORM

           CLOSE ACCOUNTS           

           IF NOT FOUND
               DISPLAY "ACCOUNT NOT FOUND."
               GO TO 0900-END-PROGRAM
           END-IF.
       
       0100-END.

       
       0200-PROCESS-RECORD.
           IF FS-ID = WS-SEARCH-ID AND NOT-FOUND
               IF FS-STATUS = "CLOSED" 
                 DISPLAY "THIS ACCOUNT IS ALREADY CLOSED."
                 PERFORM 0900-END-PROGRAM
               END-IF

               SET FOUND TO TRUE
               MOVE "CLOSED" TO FS-STATUS
               REWRITE FS-ACC
               DISPLAY "ACCOUNT CLOSED SUCCESSFULLY."
           END-IF.
       0200-END.

       0900-END-PROGRAM.
           EXIT PROGRAM.
       0900-END.
