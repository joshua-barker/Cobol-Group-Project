       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLASS-ROSTER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENROLLMENT-FILE ASSIGN 'PROJ-ENR.REL'
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS RANDOM
               RELATIVE KEY IS ENR-KEY.
           SELECT STUDENT-FILE ASSIGN TO 'PROJ-STU-FILE.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY SF-STU-NUM.
           SELECT CLASS-FILE ASSIGN TO 'PROJ-CLASS-SECT.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY CS-CRN.
       DATA DIVISION.
       FILE SECTION.
       FD ENROLLMENT-FILE.
           01 CONTROL-REC.
               05 CONTROL-EOF                  PIC 9(5).
               05 FILLER                       PIC X(13).
           01 ENROLL-REC.
               05 EN-STUDENT                   PIC 9(9).
               05 EN-CRN                       PIC X(5).
               05 EN-NEXT-PTR                  PIC 9(5).
       FD STUDENT-FILE IS EXTERNAL RECORD CONTAINS 63 CHARACTERS.
       COPY SF-FILE-DESC.
       FD CLASS-FILE IS EXTERNAL RECORD CONTAINS 23 CHARACTERS.
       COPY CS-SECT.
       WORKING-STORAGE SECTION.
       01 STORED-VALUES.
           05 MORE-RECORDS                     PIC X VALUE 'Y'.
           05 MORE                             PIC X VALUE 'Y'.
           05 ENR-KEY                          PIC 9(5).
           05 WS-NAME                          PIC X(10).
           05 WS-CRN                           PIC 9(5).
           05 WS-CLASS-HEADER                  PIC X(36).
           05 WS-COURSE                        PIC X(6).
           05 WS-FILLER                        PIC X.
           05 WS-DESCR                         PIC X(10).
           05 WS-CLASS-TITLE                   PIC X(13)
               VALUE ' CLASS ROSTER'.
           05 QUIT                             PIC X.
       SCREEN SECTION.
       01 CLEAR-SCREEN.
          05 BLANK SCREEN.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
       OPEN INPUT STUDENT-FILE
                  ENROLLMENT-FILE
                  CLASS-FILE
       DISPLAY CLEAR-SCREEN
       DISPLAY '  CLASS ROSTER'
       DISPLAY '-----------------'
       PERFORM UNTIL MORE-RECORDS = 'N'
           MOVE 0000 TO ENR-KEY
           DISPLAY ' '
           DISPLAY 'ENTER CRN # OR 0000 TO QUIT: '
           ACCEPT WS-CRN
           IF WS-CRN = 0000
               EXIT PROGRAM
           END-IF
           MOVE WS-CRN TO CS-CRN
           READ CLASS-FILE
               INVALID MOVE 'N' TO MORE-RECORDS
               NOT INVALID
                   MOVE CS-DESC TO WS-DESCR
                   MOVE CS-CRS-CODE TO WS-COURSE
                   MOVE SPACES TO WS-CLASS-HEADER
                   STRING WS-COURSE DELIMITED BY SIZE
                       ' '   DELIMITED BY SIZE
                       WS-FILLER DELIMITED BY SIZE
                       ' '   DELIMITED BY SIZE
                       WS-DESCR DELIMITED BY SIZE
                       ' '   DELIMITED BY SIZE
                       WS-CLASS-TITLE DELIMITED BY SIZE
                       INTO WS-CLASS-HEADER
                   END-STRING
           END-READ
           DISPLAY '------------------------------------'
           DISPLAY WS-CLASS-HEADER
           DISPLAY '------------------------------------'
           MOVE 0002 TO ENR-KEY
           MOVE 'Y' TO MORE
           PERFORM UNTIL MORE = 'N'
               READ ENROLLMENT-FILE
                   INVALID KEY MOVE 'N' TO MORE
                   NOT INVALID KEY
                       PERFORM 200-TRANSFER
               END-READ.
       200-TRANSFER.
           IF WS-CRN = EN-CRN
               MOVE EN-STUDENT TO SF-STU-NUM
               READ STUDENT-FILE
                   NOT INVALID KEY
                       MOVE SPACES TO WS-NAME
                       STRING SF-NAME DELIMITED BY SPACE
                           INTO WS-NAME
                       END-STRING
                       DISPLAY WS-NAME
               END-READ
           END-IF
           ADD 1 TO ENR-KEY.
