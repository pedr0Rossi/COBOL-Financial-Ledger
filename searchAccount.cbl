       IDENTIFICATION DIVISION.
       PROGRAM-ID. searchAccount.
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
       FD ACCOUNTS.
       01  FS-ACC-DETAILS.
           02 FS-ID              PIC 9999. 
           02 FS-FULLNAME        PIC X(13).
           02 FS-TYPE            PIC X(10).
           02 FS-STATUS          PIC X(7).
           02 FS-CREATED-AT      PIC X(10).

       WORKING-STORAGE SECTION.

       01  WS-SEARCH-ID               PIC 9(4).
       01  ACCOUNTS-CHECK-STATUS      PIC XX.
       
       01  WS-END-OF-FILE      PIC X VALUE 'N'.
           88  EOF             VALUE 'Y'.
           88  NOT-EOF         VALUE 'N'.

       01  WS-FOUND-FLAG      PIC X VALUE 'N'.
           88  FOUND          VALUE 'Y'.
           88  NOT-FOUND      VALUE 'N'.

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
           INITIALIZE WS-ID
           INITIALIZE WS-DETAIL-LINE
           SET NOT-EOF TO TRUE
           SET NOT-FOUND TO TRUE
           
           OPEN INPUT ACCOUNTS
           IF ACCOUNTS-CHECK-STATUS NOT = "00"
               DISPLAY "ERROR OPENING FILE: " ACCOUNTS-CHECK-STATUS
               GO TO 0900-END-PROGRAM
           END-IF
           
           DISPLAY "SEARCH AN ID: "
           ACCEPT WS-SEARCH-ID
           
           PERFORM UNTIL EOF OR FOUND
               READ ACCOUNTS
                   AT END
                       SET EOF TO TRUE
                   NOT AT END
                       PERFORM 0200-CHECK-RECORD
               END-READ
           END-PERFORM
           
           IF NOT FOUND
               DISPLAY "ACCOUNT NOT FOUND. RETURNING TO MENU..."
               GO TO 0900-END-PROGRAM
           END-IF
           
           DISPLAY WS-HEADER-01
           DISPLAY WS-HEADER-02
           DISPLAY WS-DETAIL-LINE
           
           PERFORM 0900-END-PROGRAM.

       0100-END.

       0200-CHECK-RECORD.      
           IF FS-ID = WS-SEARCH-ID
               SET FOUND TO TRUE
               MOVE FS-ID         TO WS-ID
               MOVE FS-FULLNAME   TO WS-FULLNAME
               MOVE FS-TYPE       TO WS-TYPE
               MOVE FS-STATUS     TO WS-STATUS
               MOVE FS-CREATED-AT TO WS-CREATED-AT
           END-IF.
       0200-END.

       0900-END-PROGRAM.

           CLOSE ACCOUNTS
           EXIT PROGRAM.
           
       0900-END.
       