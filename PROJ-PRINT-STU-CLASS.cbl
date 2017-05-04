       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-PRINT-STU-CLASS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       SELECT SF-FILE-DESC ASSIGN TO 'PROJ-STU-FILE.DAT'
              ORGANIZATION IS INDEXED
              ACCESS IS DYNAMIC
              RECORD KEY IS SF-STU-NUM.
       SELECT EF-FILE-DES ASSIGN TO 'PROJ-ENR.REL'
              ORGANIZATION IS RELATIVE
              ACCESS IS DYNAMIC
              RELATIVE KEY IS X.
       SELECT CS-SECT ASSIGN TO 'PROJ-CLASS-SECT.DAT'
              ORGANIZATION IS INDEXED
              ACCESS IS DYNAMIC
              RECORD KEY IS CS-CRN.
       DATA DIVISION.
       FILE SECTION.
       FD  SF-FILE-DESC IS EXTERNAL RECORD CONTAINS 63 CHARACTERS.
           COPY SF-FILE-DESC.
       FD  EF-FILE-DES.
       01  EF-RECORD PIC X(19).
       FD  CS-SECT IS EXTERNAL RECORD CONTAINS 23 CHARACTERS.
           COPY CS-SECT.
       WORKING-STORAGE SECTION.
       01  ANS                      PIC X.
       01  X PIC 9(5).
       01  MORE-CLS               PIC X.
       01  ENR-REC.
           05 RF-STUDENT-NUM      PIC 9(9).
           05 RF-CRN              PIC X(5).
           05 RF-NXT-CLS-PNTR     PIC 9(5).
       COPY COLORS.
       SCREEN SECTION.
       01  BLANK-SCREEN.
           05  BLANK SCREEN.
       01  CLASS-INFO.
           05  BACKGROUND-COLOR WHITE FOREGROUND-COLOR BLUE HIGHLIGHT.
           05  COLUMN 15 VALUE 'COURSE INFO:'.
           05  COLUMN 15 VALUE 'COURSE CRN: '.
           05  COLUMN 28 PIC 9(5) FROM CS-CRN.
           05  COLUMN 15 VALUE 'COURSE CODE: '.
           05  COLUMN 29 PIC X(6) FROM CS-CRS-CODE.
           05  COLUMN 15 VALUE 'COURSE DESCRIPTION: '.
           05  COLUMN 36 PIC X(10) FROM CS-DESC.
           05  COLUMN 15 VALUE 'CREDIT HOURS: '.
           05  COLUMN 30 PIC 99 FROM CS-CRED-HR.
       01  STUDENT-NUMBER.
           05  BLANK SCREEN.
           05  BACKGROUND-COLOR WHITE FOREGROUND-COLOR BLUE HIGHLIGHT.
           05  LINE 2  COLUMN 15 VALUE 'STUDENT NUMBER: '.
           05  LINE 2  COLUMN 31 PIC 9(9) TO SF-STU-NUM.
           05  LINE 3  VALUE ' '.
       01  ANOTHER-CLASS.
           05 BLANK SCREEN.
           05  BACKGROUND-COLOR WHITE FOREGROUND-COLOR BLUE HIGHLIGHT.
           05  LINE 2  COLUMN 15 VALUE 'ENTER MORE (Y) OR EXIT (X): '.
           05  LINE 2  COLUMN 44 PIC X TO MORE-CLS.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT SF-FILE-DESC
                      EF-FILE-DES
                      CS-SECT
           MOVE SPACES TO MORE-CLS
           PERFORM UNTIL MORE-CLS = 'X'
               DISPLAY STUDENT-NUMBER
               ACCEPT STUDENT-NUMBER
               READ SF-FILE-DESC
                   INVALID KEY PERFORM 300-NOT-EXISTING-ROUTINE
                   NOT INVALID KEY PERFORM 200-PRINT-ROUTINE
               END-READ
           END-PERFORM
           CLOSE SF-FILE-DESC
                 EF-FILE-DES
                 CS-SECT
           EXIT PROGRAM.
       200-PRINT-ROUTINE.
           IF SF-ENR-REC-PNTR > 0 THEN
             MOVE SF-ENR-REC-PNTR TO X
             READ EF-FILE-DES
             MOVE EF-RECORD TO ENR-REC
             MOVE RF-CRN TO CS-CRN
             READ CS-SECT
             DISPLAY ' '
             DISPLAY ' '
             DISPLAY 'COURSE INFO:'
             DISPLAY 'COURSE CRN: ', CS-CRN
             DISPLAY 'COURSE CODE: ', CS-CRS-CODE
             DISPLAY 'COURSE DESCRIPTION: ', CS-DESC
             DISPLAY 'CREDIT HOURS: ', CS-CRED-HR
             DISPLAY ' '
             PERFORM UNTIL RF-NXT-CLS-PNTR = 0
               MOVE RF-NXT-CLS-PNTR TO X
               READ EF-FILE-DES
               MOVE EF-RECORD TO ENR-REC
               MOVE RF-CRN TO CS-CRN
               READ CS-SECT
               DISPLAY 'COURSE CRN: ', CS-CRN
               DISPLAY 'COURSE CODE: ', CS-CRS-CODE
               DISPLAY 'COURSE DESCRIPTION: ', CS-DESC
               DISPLAY 'CREDIT HOURS: ', CS-CRED-HR
               DISPLAY ' '
             END-PERFORM
             ACCEPT ANS
           ELSE
             DISPLAY BLANK-SCREEN
             DISPLAY 'STUDENT HAS NO CLASSES'
             DISPLAY 'HIT ENTER TO GO BACK TO MAIN SCREEN'
             MOVE 'X' TO MORE-CLS
             ACCEPT ANS
           END-IF.
       300-NOT-EXISTING-ROUTINE.
           DISPLAY BLANK-SCREEN
           DISPLAY 'STUDENT NOT IN SYSTEM'
           DISPLAY 'HIT ENTER TO GO BACK TO MAIN SCREEN'
           MOVE 'X' TO MORE-CLS
           ACCEPT ANS.
