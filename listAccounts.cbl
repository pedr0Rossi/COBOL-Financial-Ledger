       IDENTIFICATION DIVISION.
       PROGRAM-ID. listAccounts.
       AUTHOR. Pedro Rossi. 
       DATE-WRITTEN. 2026-04-17.
      
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
       FD ACCOUNTS.
       01  FS-ACC-DETAILS.
           02 FS-ID              PIC 9999. 
           02 FS-FULLNAME        PIC X(13).
           02 FS-TYPE            PIC X(10).
           02 FS-STATUS          PIC X(7).
           02 FS-CREATED-AT      PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-END-OF-FILE      PIC X VALUE 'N'.
           88  EOF             VALUE 'Y'.
           88  NOT-EOF         VALUE 'N'.

       01  ACCOUNTS-CHECK-STATUS PIC XX.

       01  WS-HEADER-01.
           02 FILLER                PIC X(2) VALUE 'ID'.
           02 FILLER                PIC X(5).
           02 FILLER                PIC X(8) VALUE 'FULLNAME'.
           02 FILLER                PIC X(8).
           02 FILLER                PIC X(4) VALUE 'TYPE'.
           02 FILLER                PIC X(9).
           02 FILLER                PIC X(6) VALUE 'STATUS'.
           02 FILLER                PIC X(4).
           02 FILLER                PIC X(10) VALUE 'CREATED-AT'.

       01  WS-HEADER-02.
           02 FILLER                PIC X(4) VALUE '----'.
           02 FILLER                PIC X(3).
           02 FILLER                PIC X(13) VALUE '-------------'.
           02 FILLER                PIC X(3).
           02 FILLER                PIC X(10) VALUE '----------'.
           02 FILLER                PIC X(3).
           02 FILLER                PIC X(7) VALUE '-------'.
           02 FILLER                PIC X(3).
           02 FILLER                PIC X(10) VALUE '----------'.

       01  WS-DETAIL-LINE.     
           02 WS-ID                 PIC 9999.
           02 FILLER                PIC X(3).
           02 WS-FULLNAME           PIC X(13).
           02 FILLER                PIC X(3).
           02 WS-TYPE               PIC X(10).
           02 FILLER                PIC X(3).
           02 WS-STATUS             PIC X(7).
           02 FILLER                PIC X(3).
           02 WS-CREATED-AT         PIC X(10).

       PROCEDURE DIVISION.
       
       0100-MAIN.
           SET NOT-EOF TO TRUE

           OPEN INPUT ACCOUNTS

           IF ACCOUNTS-CHECK-STATUS NOT = "00"
               DISPLAY "ERROR OPENING FILE: "
                       ACCOUNTS-CHECK-STATUS
               GO TO 0900-FINISH-PROGRAM
           END-IF

           READ ACCOUNTS 
            AT END MOVE 'Y' TO WS-END-OF-FILE
           END-READ

           DISPLAY WS-HEADER-01
           DISPLAY WS-HEADER-02

           PERFORM 0200-LIST-ACCOUNTS UNTIL EOF

           PERFORM 0900-FINISH-PROGRAM.
       0100-END.

       0200-LIST-ACCOUNTS.
           MOVE FS-ID TO WS-ID
           MOVE FS-FULLNAME TO WS-FULLNAME
           MOVE FS-TYPE TO WS-TYPE
           MOVE FS-STATUS TO WS-STATUS
           MOVE FS-CREATED-AT TO WS-CREATED-AT
           DISPLAY WS-DETAIL-LINE
           READ ACCOUNTS 
            AT END MOVE 'Y' TO WS-END-OF-FILE
           END-READ.
       0200-END.

       0900-FINISH-PROGRAM.
           CLOSE ACCOUNTS
           EXIT PROGRAM.
       0900-END.
