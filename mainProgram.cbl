       IDENTIFICATION DIVISION.
       PROGRAM-ID. mainProgram.
       AUTHOR. Pedro Rossi.
       DATE-WRITTEN. 29/04/2026.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-MENU_OPT             PIC 9.

       01  WS-MAIN-MENU.        
           02 WS-MAIN-MENU-LINE    PIC X(34) VALUE
                                   "----------------------------------".
           02 WS-MAIN-MENU-TITLE   PIC X(28)
                                   VALUE "Financial Transaction Ledger".
           02 WS-MAIN-MENU-OPT1    PIC X(22)
                                   VALUE "1 - Account Management".   
           02 WS-MAIN-MENU-OPT2    PIC X(27)
                                   VALUE "2 - Transactions Management".   
           02 WS-MAIN-MENU-OPT3    PIC X(11)
                                   VALUE "3 - Reports".     
           02 WS-MAIN-MENU-OPT0    PIC X(8)
                                   VALUE "0 - Exit".                

       01  WS-ACCOUNT-MANAGEMENT-MENU.        
           02 WS-ACC-MENU-LINE    PIC X(34) VALUE
                                   "----------------------------------".
           02 WS-ACC-MENU-OPT1    PIC X(18)
                                   VALUE "1 - Create Account".
           02 WS-ACC-MENU-OPT2    PIC X(17)
                                   VALUE "2 - List Accounts".   
           02 WS-ACC-MENU-OPT3    PIC X(18)
                                   VALUE "3 - Search Account".   
           02 WS-ACC-MENU-OPT4    PIC X(17)
                                   VALUE "4 - Close Account".     
           02 WS-ACC-MENU-OPT0    PIC X(8)
                                   VALUE "0 - Exit".           

       01  WS-TRANSACTION-ENTRY-MENU.        
           02 WS-TRAN-MENU-LINE    PIC X(34) VALUE
                                   "----------------------------------".
           02 WS-TRAN-MENU-OPT1    PIC X(19)
                                   VALUE "1 - Add Transaction".
           02 WS-TRAN-MENU-OPT2    PIC X(21)
                                   VALUE "2 - List Transactions".                                  
           02 WS-TRAN-MENU-OPT3    PIC X(22)
                                   VALUE "3 - Modify Description".   
           02 WS-TRAN-MENU-OPT4    PIC X(22)
                                   VALUE "4 - Cancel Transaction".     
           02 WS-TRAN-MENU-OPT0    PIC X(8)
                                   VALUE "0 - Exit".                                                                                                                                           

       PROCEDURE DIVISION.

       0100-MAIN-MENU.
           DISPLAY WS-MAIN-MENU-LINE
           DISPLAY WS-MAIN-MENU-TITLE
           DISPLAY WS-MAIN-MENU-LINE
           DISPLAY WS-MAIN-MENU-OPT1
           DISPLAY WS-MAIN-MENU-OPT2
           DISPLAY WS-MAIN-MENU-OPT3
           DISPLAY WS-MAIN-MENU-OPT0
           DISPLAY WS-MAIN-MENU-LINE           
           ACCEPT WS-MENU_OPT

           EVALUATE WS-MENU_OPT
              WHEN 0 
                 DISPLAY "CLOSING PROGRAM..."
                 PERFORM 0900-END-PROGRAM
              WHEN 1
                 PERFORM 0150-ACCOUNT-MANAGEMENT-MENU
              WHEN 2 
                 PERFORM 0200-TRANSACTION-ENTRY-MENU
              WHEN 3 
                 CALL "generateReports"
                 PERFORM 0100-MAIN-MENU
              WHEN OTHER
                 DISPLAY "INVALID ENTRY! TYPE A VALID NUMBER."
                 PERFORM 0100-MAIN-MENU
           END-EVALUATE.
       0100-END.

       0150-ACCOUNT-MANAGEMENT-MENU.
           PERFORM 0300-DISPLAY-ACCOUNT-MENU
           ACCEPT WS-MENU_OPT

           EVALUATE WS-MENU_OPT
              WHEN 0 
                 PERFORM 0100-MAIN-MENU
              WHEN 1 
                 CALL "createAccount"
                 PERFORM 0150-ACCOUNT-MANAGEMENT-MENU
              WHEN 2
                 CALL "listAccounts"
                 PERFORM 0150-ACCOUNT-MANAGEMENT-MENU
              WHEN 3 
                 CALL "searchAccount"
                 PERFORM 0150-ACCOUNT-MANAGEMENT-MENU
              WHEN 4
                 CALL "closeAccount"
                 PERFORM 0150-ACCOUNT-MANAGEMENT-MENU
              WHEN OTHER
                 DISPLAY "INVALID ENTRY! TYPE A VALID NUMBER."
                 PERFORM 0150-ACCOUNT-MANAGEMENT-MENU
           END-EVALUATE.
       0150-END.

       0200-TRANSACTION-ENTRY-MENU.
           PERFORM 0350-DISPLAY-TRANSACTION-MENU
           ACCEPT WS-MENU_OPT

           EVALUATE WS-MENU_OPT
              WHEN 0 
                 PERFORM 0100-MAIN-MENU
              WHEN 1 
                 CALL "addTransaction"
                 PERFORM 0200-TRANSACTION-ENTRY-MENU
              WHEN 2 
                 CALL "listTransactions"
                 PERFORM 0200-TRANSACTION-ENTRY-MENU
             WHEN 3
                 CALL "modifyDescription"
                 PERFORM 0200-TRANSACTION-ENTRY-MENU
              WHEN 4
                 CALL "cancelTransaction"
                 PERFORM 0200-TRANSACTION-ENTRY-MENU
              WHEN OTHER
                 DISPLAY "INVALID ENTRY! TYPE A VALID NUMBER."
                 PERFORM 0200-TRANSACTION-ENTRY-MENU
           END-EVALUATE.
       0200-END.

       0300-DISPLAY-ACCOUNT-MENU.
           DISPLAY WS-ACC-MENU-LINE
           DISPLAY WS-ACC-MENU-OPT1
           DISPLAY WS-ACC-MENU-OPT2
           DISPLAY WS-ACC-MENU-OPT3
           DISPLAY WS-ACC-MENU-OPT4
           DISPLAY WS-ACC-MENU-OPT0
           DISPLAY WS-ACC-MENU-LINE.
       0300-END.
       
       0350-DISPLAY-TRANSACTION-MENU.
           DISPLAY WS-TRAN-MENU-LINE
           DISPLAY WS-TRAN-MENU-OPT1
           DISPLAY WS-TRAN-MENU-OPT2
           DISPLAY WS-TRAN-MENU-OPT3
           DISPLAY WS-TRAN-MENU-OPT4
           DISPLAY WS-TRAN-MENU-OPT0
           DISPLAY WS-TRAN-MENU-LINE.
       0350-END.

       0900-END-PROGRAM.
           STOP RUN.
       0900-END.
