# First COBOL Project - Simplified Financial Ledger

This repository contains my introductory project in the **COBOL** language. The primary objective was to put into practice the concepts of indexed file manipulation and procedural programming logic, simulating the essential operations of a financial transaction ledger.

This project is a simplified version of a ledger system, developed entirely for study purposes and to solidify syntax knowledge.

## 🧠 What did I learn with this project?

Below, I describe the main COBOL and Mainframe concepts I explored during development:

* **Indexed Files (.IDX):** I practiced creating and manipulating files that use unique keys for fast data access. Unlike simple sequential files, here I understood how COBOL manages indices to search for records without traversing the entire file.
* **COBOL Syntax and Structure:** I implemented COBOL syntax and core concepts such as the division structure, variable definitions and manipulations, functions, flag usage, loops and more.
* **Modularization and Calls (CALL):** To keep the code organized, I divided the functionalities into different programs (such as account creation, reporting, and cancellations) and used the `CALL` command to integrate them into the main menu. Every program is also internaly divided
* between organized paragraphs.
* **Basic Data Validation:** I implemented checks to ensure a better standard of data validity, such as preventing transactions on closed accounts and handling file status codes to catch execution errors.

## 🛠️ System Overview

The project is structured around three main learning pillars:

1.  **Account Management:** Allows creating, listing, closing and searching accounts.
2.  **Financial Entries:** Allows to use the existing active accounts to perform transactions, as well as searching and editing them.
3.  **Reports generation:** Implementation of daily, monthly, and account-specific reports to summarize the data on an organized format.

## 🚀 How to test in your environment

To run this project, I used **GNUCobol**. Here are the notes I made to configure the environment on Windows:

### Initial Configuration
1.  Install GNUCobol (via SuperBOL or similar).
2.  Add the binaries to your Windows PATH:
    `setx PATH "C:\Users\username\tools\gnucobol\bin"`
3.  Verify the installation in the terminal with: `cobc -v`.

### Compilation and Execution
Since the project is modular, the subprograms must be compiled as modules (`.dll` or `.so`) so that the `mainProgram` can access them:

1.  **Generate the modules:**
    ```bash
    cobc -m addTransaction.cbl cancelTransaction.cbl closeAccount.cbl createAccount.cbl generateReports.cbl listAccounts.cbl listTransactions.cbl modifyDescription.cbl searchAccount.cbl
    ```
2.  **Compile the main program:**
    ```bash
    cobc -x mainProgram.cbl
    ```
3.  **Run:**
    ```bash
    .\mainProgram.exe
    ```

---

*This project is part of my learning journey in Computer Science and Backend development.*
