       IDENTIFICATION DIVISION.
       PROGRAM-ID. createAccount.
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
       
       01 FS-ACC-DETAILS.
          02 FS-ID           PIC 9999.
          02 FS-FULLNAME     PIC X(13).
          02 FS-TYPE         PIC X(10).
          02 FS-STATUS       PIC X(7).
          02 FS-CREATED-AT   PIC X(10).

       WORKING-STORAGE SECTION.
      
       01  ACCOUNTS-CHECK-STATUS PIC XX.
       01  WS-END-OF-FILE        PIC X VALUE "N".
           88 EOF                VALUE "Y".
           88 NOT-EOF            VALUE "N".
       01  WS-FOUND-FLAG         PIC X VALUE "N".
           88 FOUND              VALUE "Y".
           88 NOT-FOUND          VALUE "N".

       01  WS-NEW-ACCOUNT-IN.
           02 WS-ID-IN           PIC 9999. 
           02 WS-FULLNAME-IN     PIC X(13).
           02 WS-TYPE-IN         PIC X(10).
           02 WS-STATUS-IN       PIC X(7).

       01  WS-SYSTEM-DATE.
           02 WS-DATE-YYYYMMDD   PIC 9(8).

       01  WS-NEW-ACCOUNT-OUT.
           02 WS-ID-OUT          PIC 9999. 
           02 WS-FULLNAME-OUT    PIC X(13).
           02 WS-TYPE-OUT        PIC X(10).
           02 WS-STATUS-OUT      PIC X(7).
           02 WS-CREATED-AT-OUT.     
              05 WS-CREATED-YEAR  PIC 9(4).
              05 FILLER           VALUE "-".
              05 WS-CREATED-MONTH PIC 9(2).
              05 FILLER           VALUE "-".
              05 WS-CREATED-DAY   PIC 9(2).

       PROCEDURE DIVISION.
           
       0100-MAIN.    
           INITIALIZE WS-NEW-ACCOUNT-IN
           INITIALIZE WS-NEW-ACCOUNT-OUT
           INITIALIZE WS-SYSTEM-DATE
      
           PERFORM 0150-PROMPT-USER 

           OPEN I-O ACCOUNTS

           IF ACCOUNTS-CHECK-STATUS NOT = "00"
              DISPLAY "ERROR OPENING FILE: " 
                       ACCOUNTS-CHECK-STATUS
              GO TO 0900-END-PROGRAM
           END-IF

           PERFORM 0200-PROCESS-DATA
      
           WRITE FS-ACC-DETAILS FROM WS-NEW-ACCOUNT-OUT 

           DISPLAY "ACCOUNT CREATED SUCCESSFULLY!"

           GO TO 0900-END-PROGRAM.
       0100-END.

       0150-PROMPT-USER.
           DISPLAY "INSERT THE NEW ACCOUNT'S ID: "
           ACCEPT WS-ID-IN

           PERFORM 0170-CHECK-ID-EXISTS
           IF FOUND
              DISPLAY "THE ACCOUNT WAS NOT CREATED - ID ALREADY EXISTS!"
              GO TO 0900-END-PROGRAM
           END-IF
      
           DISPLAY "INSERT THE NEW ACCOUNT'S FULLNAME: "
           ACCEPT WS-FULLNAME-IN
      
           DISPLAY "INSERT THE NEW ACCOUNT'S TYPE: "
           ACCEPT WS-TYPE-IN
      
           DISPLAY "INSERT THE NEW ACCOUNT'S STATUS: "
           ACCEPT WS-STATUS-IN
       
           MOVE FUNCTION CURRENT-DATE(1:8)
                TO WS-DATE-YYYYMMDD.
       0150-END.

       0170-CHECK-ID-EXISTS.
           OPEN I-O ACCOUNTS
           IF ACCOUNTS-CHECK-STATUS NOT = "00"
              DISPLAY "ERROR OPENING FILE: "
                       ACCOUNTS-CHECK-STATUS
              SET FOUND TO TRUE
              GO TO 0170-END
           END-IF

           SET NOT-EOF TO TRUE
           SET NOT-FOUND TO TRUE

           PERFORM UNTIL EOF
              READ ACCOUNTS
                 AT END
                    SET EOF TO TRUE
                 NOT AT END
                    IF FS-ID = WS-ID-IN
                       SET FOUND TO TRUE
                       SET EOF TO TRUE
                    END-IF
              END-READ
           END-PERFORM

           CLOSE ACCOUNTS.
       0170-END.

       0200-PROCESS-DATA.
           MOVE WS-ID-IN 
                TO WS-ID-OUT
      
           MOVE WS-FULLNAME-IN
                TO WS-FULLNAME-OUT
      
           MOVE FUNCTION UPPER-CASE (WS-TYPE-IN)
                TO WS-TYPE-OUT
      
           MOVE FUNCTION UPPER-CASE (WS-STATUS-IN)
                TO WS-STATUS-OUT
      
           MOVE WS-DATE-YYYYMMDD(1:4)
                TO WS-CREATED-YEAR
           MOVE WS-DATE-YYYYMMDD(5:2)
                TO WS-CREATED-MONTH
           MOVE WS-DATE-YYYYMMDD(7:2)
                TO WS-CREATED-DAY.
       0200-END.

       0900-END-PROGRAM.
           CLOSE ACCOUNTS
           EXIT PROGRAM.
       0900-END.
