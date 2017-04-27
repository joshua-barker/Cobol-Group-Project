       IDENTIFICATION DIVISION.
      *PROGRAM-ID. PROJ-ENROLLMENT.CBL.
      *CALEB STEVENS.
      *20170417.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
       01  ARE-THERE-MORE-RECORDS PIC X VALUE 'Y'.
       01  X PIC 9(5).
       01  EOF-REC.
           05  EOF-POINTER       PIC 9(5).
           05  FILLER            PIC X(14).
       01  ENR-REC.
           05 RF-STUDENT-NUM      PIC 9(9).
           05 RF-CRN              PIC X(5).
           05 RF-NXT-CLS-PNTR     PIC 9(5).
       01  WS-SF-RECORD.
           05  WS-SF-STU-NUM      PIC 9(9).
           05  FILLER             PIC X(49).
           05  WS-SF-ENR-REC-PNTR PIC 9(5).
       01  WS-TMP-SNO             PIC 9(9).
       01  WS-TMP-CRN             PIC 9(5).
       01  WS-TMP-CRS-CODE        PIC X(6).
       01  WS-TMP-CS-DESC         PIC X(10).
       01  WS-TMP-CS-CRED-HR      PIC 99.
       01  WS-TMP-EOF-PNTR        PIC 9(5).
       01  WS-SNO                 PIC 9(9).
       01  CLS-CONFIRM            PIC X.
       01  MORE-CLS               PIC X.
       01  ANS                    PIC X.
       COPY COLORS.
       SCREEN SECTION.
       01  BLANK-SCREEN.
           05  BLANK SCREEN.
       01  SCREEN-ENROLLMENT-INFO.
           05  BLANK SCREEN.
           05  BACKGROUND-COLOR WHITE FOREGROUND-COLOR BLUE HIGHLIGHT.
           05  LINE 2  COLUMN 15 VALUE 'STUDENT INFO:'.
           05  LINE 3  COLUMN 15 VALUE 'STUDENT NUMBER: '.
           05  LINE 3  COLUMN 31 PIC 9(9) FROM SF-STU-NUM.
           05  LINE 5  COLUMN 15 VALUE 'STUDENT NAME: '.
           05  LINE 5  COLUMN 29 PIC X(10) FROM SF-NAME.
           05  LINE 6  COLUMN 15 VALUE '-------------------------'.
           05  LINE 8  COLUMN 15 VALUE 'ENTER CRN NUMBER: '.
           05  LINE 8  COLUMN 34 PIC X(5) TO WS-TMP-CRN.
       01  CLASS-INFO.
           05  BLANK SCREEN.
           05  BACKGROUND-COLOR WHITE FOREGROUND-COLOR BLUE HIGHLIGHT.
           05  LINE 2  COLUMN 15 VALUE 'COURSE INFO:'.
           05  LINE 3  COLUMN 15 VALUE 'COURSE CRN: '.
           05  LINE 3  COLUMN 28 PIC 9(5) FROM CS-CRN.
           05  LINE 5  COLUMN 15 VALUE 'COURSE CODE: '.
           05  LINE 5  COLUMN 29 PIC X(6) FROM CS-CRS-CODE.
           05  LINE 7  COLUMN 15 VALUE 'COURSE DESCRIPTION: '.
           05  LINE 7  COLUMN 36 PIC X(10) FROM CS-DESC.
           05  LINE 9  COLUMN 15 VALUE 'CREDIT HOURS: '.
           05  LINE 9  COLUMN 30 PIC 99 FROM CS-CRED-HR.
           05  LINE 12 COLUMN 15 VALUE 'ACCEPT THIS CLASS (Y OR N): '.
           05  LINE 12 COLUMN 44 PIC X TO CLS-CONFIRM.
       01  ANOTHER-CLASS.
           05 BLANK SCREEN.
           05  BACKGROUND-COLOR WHITE FOREGROUND-COLOR BLUE HIGHLIGHT.
           05  LINE 2  COLUMN 15 VALUE 'ENTER MORE (Y) OR EXIT (X): '.
           05  LINE 2  COLUMN 44 PIC X TO MORE-CLS.
       PROCEDURE DIVISION USING WS-SNO.
       100-MAIN-MODULE.
      *    CLOSE CS-SECT
      *    CLOSE SF-FILE-DESC
      *    CLOSE EF-FILE-DES
           OPEN I-O EF-FILE-DES
           OPEN INPUT SF-FILE-DESC
           OPEN INPUT CS-SECT
           MOVE SPACES TO MORE-CLS
           PERFORM UNTIL MORE-CLS = 'X'
               MOVE SPACES TO SF-RECORD
               MOVE WS-SNO TO SF-STU-NUM
               READ SF-FILE-DESC
                 INVALID KEY PERFORM 300-EXISTING-ROUTINE
                 NOT INVALID KEY PERFORM 200-ENROLL-ROUTINE
               END-READ
           END-PERFORM
           CLOSE SF-FILE-DESC
           CLOSE EF-FILE-DES
           CLOSE CS-SECT
           EXIT PROGRAM.
       200-ENROLL-ROUTINE.
           DISPLAY SCREEN-ENROLLMENT-INFO
           ACCEPT SCREEN-ENROLLMENT-INFO
           MOVE WS-TMP-CRN TO CS-CRN
           READ CS-SECT
             INVALID KEY DISPLAY 'CRN DOES NOT EXIST'
             NOT INVALID KEY CONTINUE
           DISPLAY CLASS-INFO
           ACCEPT CLASS-INFO
           IF CLS-CONFIRM = 'Y' THEN
               MOVE SPACES TO EF-RECORD
               MOVE 1 TO X
               READ EF-FILE-DES
               MOVE EF-RECORD TO EOF-REC
               MOVE EOF-POINTER TO WS-TMP-EOF-PNTR
               ADD 1 TO EOF-POINTER
               REWRITE EF-RECORD FROM EOF-REC
               IF SF-ENR-REC-PNTR > 0 THEN
                 MOVE SF-ENR-REC-PNTR TO X
                 READ EF-FILE-DES
                 MOVE EF-RECORD TO ENR-REC
      * THIS BIT DISPLAYS THE LIST OF CLASSES FOR A STUDENT
      *      DISPLAY 'STUDENT CLASSES: '
                 PERFORM UNTIL RF-NXT-CLS-PNTR = 0
                   MOVE RF-NXT-CLS-PNTR TO X
                   READ EF-FILE-DES
                   MOVE EF-RECORD TO ENR-REC
      *            MOVE RF-CRN TO CS-CRN
      *            READ CL
      *            DISPLAY 'CRN NUMBER:   ', RF-CRN
      *            DISPLAY 'COURSE NUMBER: ', RF-COURSE-NUMBER
      *            DISPLAY ' '
                 END-PERFORM
                 MOVE WS-TMP-EOF-PNTR TO RF-NXT-CLS-PNTR
                 REWRITE EF-RECORD FROM ENR-REC
               ELSE
                 CLOSE SF-FILE-DESC
                 OPEN I-O SF-FILE-DESC
                 MOVE SF-RECORD TO WS-SF-RECORD
                 MOVE WS-TMP-EOF-PNTR TO WS-SF-ENR-REC-PNTR
                 REWRITE SF-RECORD FROM WS-SF-RECORD
                 CLOSE SF-FILE-DESC
                 OPEN INPUT SF-FILE-DESC
               END-IF
               MOVE WS-TMP-EOF-PNTR TO X
               MOVE WS-TMP-CRN TO RF-CRN
               MOVE WS-SNO TO RF-STUDENT-NUM
               MOVE ZERO TO RF-NXT-CLS-PNTR
               WRITE EF-RECORD FROM ENR-REC
           END-IF
      *    DISPLAY 'LINE NUMBER:   ', RF-LINE-NUMBER
      *    DISPLAY 'COURSE NUMBER: ', RF-COURSE-NUMBER
      *    DISPLAY ' '
           DISPLAY ANOTHER-CLASS
           ACCEPT ANOTHER-CLASS.
       300-EXISTING-ROUTINE.
           DISPLAY BLANK-SCREEN
           DISPLAY 'STUDENT EXISTS'
           DISPLAY 'HIT ENTER TO GO BACK TO MAIN SCREEN'
           MOVE 'X' TO MORE-CLS
           ACCEPT ANS.
