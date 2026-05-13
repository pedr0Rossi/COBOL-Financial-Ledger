       IDENTIFICATION DIVISION.
       PROGRAM-ID. generateReports.
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
       01 FD-TRANSACTION-DETAILS.
           05 TR-KEY.
               10 TR-ID              PIC 9(5).               
               10 TR-ACC-ID          PIC 9(4).
           05 TR-TYPE                PIC X(6).
           05 TR-AMOUNT              PIC 9(9)V99.
           05 TR-DESC                PIC X(20).
           05 TR-DATE                PIC X(10).
           05 TR-STATUS              PIC X(8).

       WORKING-STORAGE SECTION.

       01  WS-REPORTS-MENU.        
           02 WS-REPT-MENU-LINE    PIC X(34) VALUE
                                   "----------------------------------".
           02 WS-REPT-MENU-OPT1    PIC X(16)
                                   VALUE "1 - Daily Report".
           02 WS-REPT-MENU-OPT2    PIC X(18)
                                   VALUE "2 - Monthly Report".     
           02 WS-REPT-MENU-OPT3    PIC X(18)
                                   VALUE "3 - Account Report".     
           02 WS-REPT-MENU-OPT0    PIC X(8)
                                   VALUE "0 - Exit".      

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

       01  WS-EOF-TRANSACTIONS       PIC X VALUE "N".
           88  EOF-TRANSACTIONS     VALUE "Y".
           88  NOT-EOF-TRANSACTIONS VALUE "N".
       01  WS-TOTAL-AMOUNT           PIC 9(9)V99.
       01  WS-MENU_OPT               PIC 9.

       01  WS-SEARCH-ID              PIC 9(4).

       01  TRANSACTIONS-CHECK-STATUS PIC XX.

       PROCEDURE DIVISION.
       
       0100-START.

           DISPLAY WS-REPT-MENU-LINE
           DISPLAY WS-REPT-MENU-OPT1
           DISPLAY WS-REPT-MENU-OPT2
           DISPLAY WS-REPT-MENU-OPT3
           DISPLAY WS-REPT-MENU-OPT0
           DISPLAY WS-REPT-MENU-LINE
           ACCEPT WS-MENU_OPT.

           EVALUATE WS-MENU_OPT
              WHEN 0 
                 GO TO 0900-END-PROGRAM
              WHEN OTHER
                 GO TO 0150-MAIN
           END-EVALUATE.
           
       0100-END.

       0150-MAIN.
          
           OPEN INPUT TRANSACTIONS
           MOVE ZEROES TO WS-TOTAL-AMOUNT
           MOVE ZEROES TO WS-SEARCH-ID
           SET NOT-EOF-TRANSACTIONS TO TRUE

           IF TRANSACTIONS-CHECK-STATUS NOT = "00"
              DISPLAY "ERROR OPENING FILE: " TRANSACTIONS-CHECK-STATUS
              PERFORM 0100-START
           END-IF

           IF WS-MENU_OPT = 3
              DISPLAY "ENTER ACCOUNT ID TO GENERATE REPORT:"
              ACCEPT WS-SEARCH-ID
           END-IF

           DISPLAY WS-HEADER-01
           DISPLAY WS-HEADER-02

           PERFORM UNTIL EOF-TRANSACTIONS
               READ TRANSACTIONS NEXT RECORD
                   AT END
                       SET EOF-TRANSACTIONS TO TRUE
                   NOT AT END
                       EVALUATE WS-MENU_OPT
                          WHEN 1                   
                             PERFORM 0200-DISPLAY-DAILY-REPORT
                          WHEN 2
                             PERFORM 0210-DISPLAY-MONTHLY-REPORT
                          WHEN 3
                             PERFORM 0220-DISPLAY-ACCOUNT-REPORT
                          WHEN OTHER
                             PERFORM 0100-START
                       END-EVALUATE
               END-READ
           END-PERFORM

           DISPLAY "TOTAL AMOUNT: " WS-TOTAL-AMOUNT

           CLOSE TRANSACTIONS

           PERFORM 0100-START.

       0150-END.

       0200-DISPLAY-DAILY-REPORT.
           
           IF FUNCTION CURRENT-DATE(7:2) = TR-DATE(9:2)
              PERFORM 0250-MOVE-DATA
              DISPLAY WS-DETAIL-LINE
              PERFORM 0300-COMPUTE-VALUE
           END-IF.

       0200-END.

       0210-DISPLAY-MONTHLY-REPORT.

           IF FUNCTION CURRENT-DATE(5:2) = TR-DATE(6:2)
              PERFORM 0250-MOVE-DATA
              DISPLAY WS-DETAIL-LINE
              PERFORM 0300-COMPUTE-VALUE
           END-IF.

       0210-END.

       0220-DISPLAY-ACCOUNT-REPORT.

           IF WS-SEARCH-ID = TR-ACC-ID
              PERFORM 0250-MOVE-DATA
              DISPLAY WS-DETAIL-LINE
              PERFORM 0300-COMPUTE-VALUE
           END-IF.

       0220-END.

       0250-MOVE-DATA.
           MOVE TR-ID     TO WS-ID
           MOVE TR-ACC-ID TO WS-ACCOUNT-ID
           MOVE TR-TYPE   TO WS-TYPE
           MOVE TR-AMOUNT TO WS-AMOUNT
           MOVE TR-DESC   TO WS-DESCRIPTION
           MOVE TR-DATE   TO WS-CREATED-AT
           MOVE TR-STATUS TO WS-STATUS.
       0250-END.

       0300-COMPUTE-VALUE.
           IF TR-STATUS NOT = "CANCELED"
              IF TR-TYPE = "DEBIT"
                    COMPUTE 
                    WS-TOTAL-AMOUNT = WS-TOTAL-AMOUNT - TR-AMOUNT
              ELSE IF TR-TYPE = "CREDIT"
                    COMPUTE 
                    WS-TOTAL-AMOUNT = WS-TOTAL-AMOUNT + TR-AMOUNT
              END-IF
           END-IF.
       0300-END.

       0900-END-PROGRAM.
           CLOSE TRANSACTIONS
           EXIT PROGRAM.
       0900-END.
       