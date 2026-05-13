       IDENTIFICATION DIVISION.
       PROGRAM-ID. addTransaction.
       AUTHOR. Pedro Rossi.
       DATE-WRITTEN. 29/04/2026.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT TRANSACTIONS ASSIGN TO "transactions.IDX"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TR-KEY
           FILE STATUS IS TRANSACTIONS-CHECK-STATUS.

       SELECT ACCOUNTS ASSIGN TO "accounts.IDX"
           ORGANIZATION IS INDEXED       
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS FD-ID           
           FILE STATUS IS ACCOUNTS-CHECK-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTIONS.
       01 FD-TRANSACTION-DETAILS.
           05 TR-KEY.
               10 TR-ID              PIC 9(5).               
               10 TR-ACC-ID          PIC 9(4).
           05 TR-TYPE                PIC X(6).
           05 TR-AMOUNT              PIC 9(9)V99.
           05 TR-DESC                PIC X(20).
           05 TR-DATE                PIC X(10).
           05 TR-STATUS              PIC X(8).

       FD ACCOUNTS.
       01 FD-ACC-DETAILS.
           02 FD-ID                   PIC 9999.
           02 FD-FULLNAME             PIC X(13).
           02 FD-TYPE                 PIC X(10).
           02 FD-STATUS               PIC X(7).
           02 FD-CREATED-AT           PIC X(10).          

       WORKING-STORAGE SECTION.
       01  WS-END-OF-TRANSACTIONS      PIC X VALUE "N".
           88  EOF-TRANSACTIONS        VALUE "Y".
           88  NOT-EOF-TRANSACTIONS    VALUE "N".

       01  WS-END-OF-ACCOUNTS          PIC X VALUE "N".
           88  EOF-ACCOUNTS            VALUE "Y".
           88  NOT-EOF-ACCOUNTS        VALUE "N".

       01  TRANSACTIONS-CHECK-STATUS   PIC XX.
       01  ACCOUNTS-CHECK-STATUS       PIC XX.

       01  WS-FOUND-FLAG               PIC X VALUE "N".
           88  FOUND                   VALUE "Y".
           88  NOT-FOUND               VALUE "N".

       01  WS-NEW-TRANSACTION.
           05 WS-KEY.
               10 WS-ID              PIC 9(5).               
               10 WS-ACC-ID          PIC 9(4).
           05 WS-TYPE                PIC X(6).
           05 WS-AMOUNT              PIC 9(9)V99.
           05 WS-DESC                PIC X(20).
           05 WS-DATE.              
              10 WS-CREATED-YEAR     PIC X(4).
              10 FILLER              VALUE "-".
              10 WS-CREATED-MONTH    PIC X(2).
              10 FILLER              VALUE "-".
              10 WS-CREATED-DAY      PIC X(2).
           05 WS-STATUS      PIC X(8).

       01  WS-SYSTEM-DATE.
           02 WS-DATE-YYYYMMDD       PIC 9(8).

       PROCEDURE DIVISION.

       0100-MAIN.    

           INITIALIZE WS-NEW-TRANSACTION
           INITIALIZE WS-SYSTEM-DATE
           MOVE ZEROES TO WS-ID
           SET NOT-EOF-TRANSACTIONS TO TRUE
           SET NOT-EOF-ACCOUNTS TO TRUE
           OPEN INPUT ACCOUNTS
           OPEN I-O TRANSACTIONS

           IF ACCOUNTS-CHECK-STATUS NOT = "00"
               DISPLAY "ERROR OPENING ACCOUNTS FILE: "
                       ACCOUNTS-CHECK-STATUS
               GO TO 0900-END-PROGRAM
           END-IF

           IF TRANSACTIONS-CHECK-STATUS NOT = "00"
              DISPLAY "ERROR OPENING TRANSACTIONS FILE: " 
                       TRANSACTIONS-CHECK-STATUS
              GO TO 0900-END-PROGRAM
           END-IF

           PERFORM 0150-PROMPT-USER THRU 0160-GET-NEW-ID           

           PERFORM 0200-PROCESS-DATA
      
           WRITE FD-TRANSACTION-DETAILS 

           DISPLAY "TRANSACTION ADDED SUCCESSFULLY."

           GO TO 0900-END-PROGRAM.

       0100-END.

       0150-PROMPT-USER.                     

           DISPLAY "INSERT THE NEW TRANSACTIONS ACCOUNT ID."
           ACCEPT WS-ACC-ID
      
           SET NOT-EOF-ACCOUNTS TO TRUE
           SET NOT-FOUND TO TRUE

           PERFORM 0170-SEARCH-ACCOUNT UNTIL EOF-ACCOUNTS OR FOUND

           IF NOT FOUND
              DISPLAY "THE ACCOUNT DOES NOT EXIST!"
              DISPLAY "THE TRANSACTION WAS NOT CREATED."
              GO TO 0900-END-PROGRAM
           END-IF.           

           DISPLAY "INSERT THE TYPE OF THE NEW TRANSACTION: "
           ACCEPT WS-TYPE
      
           DISPLAY "INSERT THE AMOUNT: "
           ACCEPT WS-AMOUNT

           DISPLAY "INSERT THE DESCRIPTION: "
           ACCEPT WS-DESC
       
           MOVE FUNCTION CURRENT-DATE(1:8)
                TO WS-DATE-YYYYMMDD

           MOVE "ACTIVE" TO WS-STATUS.

       0150-END.

       0160-GET-NEW-ID.

           SET NOT-EOF-TRANSACTIONS TO TRUE

           PERFORM UNTIL EOF-TRANSACTIONS
              READ TRANSACTIONS
                 AT END
                    MOVE TR-ID OF FD-TRANSACTION-DETAILS TO WS-ID
                    SET EOF-TRANSACTIONS TO TRUE
              END-READ
           END-PERFORM.

       0160-END.

       0170-SEARCH-ACCOUNT.

           IF WS-ACC-ID = FD-ID 
              IF FD-STATUS = "CLOSED"
                 SET EOF-ACCOUNTS TO TRUE
                 DISPLAY "CAN'T ADD TRANSACTION TO A CLOSED ACCOUNT!"
                 GO TO 0900-END-PROGRAM
              END-IF

              SET FOUND TO TRUE
           END-IF

           READ ACCOUNTS 
              AT END SET EOF-ACCOUNTS TO TRUE
           END-READ.

       0170-END.

       0200-PROCESS-DATA.
                           
           COMPUTE TR-ID = WS-ID + 1

           MOVE WS-ACC-ID
                TO TR-ACC-ID

           MOVE FUNCTION UPPER-CASE (WS-TYPE)
                TO TR-TYPE             

           MOVE WS-AMOUNT
                TO TR-AMOUNT
                      
           MOVE FUNCTION UPPER-CASE (WS-DESC)
                TO TR-DESC
      
           MOVE WS-DATE-YYYYMMDD(1:4)
                TO WS-CREATED-YEAR
           MOVE WS-DATE-YYYYMMDD(5:2)
                TO WS-CREATED-MONTH
           MOVE WS-DATE-YYYYMMDD(7:2)
                TO WS-CREATED-DAY
           MOVE WS-DATE
                TO TR-DATE

           MOVE WS-STATUS TO TR-STATUS.

       0200-END.

       0900-END-PROGRAM.

           CLOSE TRANSACTIONS
           CLOSE ACCOUNTS
           EXIT PROGRAM.

       0900-END.
       