       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-RC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT RC-FILE-DESC ASSIGN TO 'PROJ-REC.REL'
                ORGANIZATION IS RELATIVE
                ACCESS IS RANDOM
                RELATIVE KEY IS WS-KEY.
            SELECT SF-FILE-DESC ASSIGN TO 'PROJ-STU-FILE.DAT'
                ORGANIZATION IS INDEXED
                ACCESS IS RANDOM
                RECORD KEY IS SF-STU-NUM.
       DATA DIVISION.
       FILE SECTION.       
       FD RC-FILE-DESC IS EXTERNAL RECORD CONTAINS 36 CHARACTERS.
       COPY RC-FILE-DESC.
       FD SF-FILE-DESC IS EXTERNAL RECORD CONTAINS 63 CHARACTERS.
       COPY SF-FILE-DESC.
       WORKING-STORAGE SECTION.
       01  WS-KEY                    PIC 9(5)  VALUE ZEROS.
       01  WS-CONT-REC               PIC 9(5)  VALUE ZEROS.
       01  WS-OPTION                 PIC X.
       01  MORE-DATA                 PIC XXX.
       01  WS-KEY-COUNTER            PIC 9(5)  VALUE 00001.
       01  WS-TMP-EOF-PNTR           PIC 9(5).
       01  RC-EOF-REC.
           05  RC-EOF PIC 9(5).
           05  FILLER PIC X(31).
       01  WS-RC-DATA-REC.
           05  WS-RC-RCT-TYPE  PIC X(10).
           05  WS-RC-STU-NUM   PIC 9(9).
           05  WS-RC-AMT-OWED  PIC 9(4)V99.
           05  WS-RC-AMT-PAID  PIC 9(4)V99.
           05  WS-RC-NEXT-PNTR PIC 9(4).
       01  WS-SF-RECORD.
           05  WS-SF-STU-NUM      PIC 9(9).
           05  FILLER             PIC X(44).
           05  WS-SF-RCT-REC-PNTR PIC 9(5).
           05  FILLER             PIC X(5).
       01  WS-STATUS                 PIC XX.
       01  WS-SNO                    PIC 9(9).
       01  WS-NEW-AMT-OWED          PIC 9(4)V99.
       01  WS-NEW-AMT-PAID          PIC 9(4)V99.
       01  ANS   PIC X.
       SCREEN SECTION.
       01 SCRN-SID.
          05 LINE 3 COLUMN 1 VALUE 'STUDENT ID NUMBER: '.
          05 LINE 3 COLUMN 27 PIC 9(9) FROM WS-SNO.
       01 SCRN-TYPE.
          05 LINE 5 COLUMN 1 VALUE 'ENTER RECEIPT TYPE: '.
          05 LINE 5 COLUMN 28 PIC X(10) TO WS-RC-RCT-TYPE.
       01 SCRN-OWED.
          05 LINE 7 COLUMN 1 VALUE 'ENTER NEW DEBIT (OR 0): '.
          05 LINE 7 COLUMN 28 PIC 9(4)V99 TO WS-NEW-AMT-OWED.
       01 SCRN-PAID.
          05 LINE 9 COLUMN 1 VALUE 'ENTER AMOUNT PAID (OR 0): '.
          05 LINE 9 COLUMN 30 PIC 9(4)V99 TO WS-NEW-AMT-PAID.
       01 SCRN-MORE-DATA.
          05 BLANK SCREEN.
          05 LINE 1 COLUMN 1 VALUE
                  'DO YOU WANT TO ADD MORE DATA? (YES/NO): '.
          05 LINE 1 COLUMN 41 PIC X(3) TO MORE-DATA.
       01 SCRN-CLEAR.
          05 BLANK SCREEN.
       PROCEDURE DIVISION USING WS-SNO.
       100-MAIN-MODULE.
           OPEN I-O RC-FILE-DESC
           OPEN INPUT SF-FILE-DESC
           MOVE SPACES TO MORE-DATA
           PERFORM UNTIL MORE-DATA = 'NO '
             MOVE SPACES TO SF-RECORD
             MOVE WS-SNO TO SF-STU-NUM
             READ SF-FILE-DESC
               INVALID KEY PERFORM 300-NOT-EXISTING-ROUTINE
               NOT INVALID KEY PERFORM 200-INPUT-ROUTINE
             END-READ
           END-PERFORM
           CLOSE SF-FILE-DESC
           CLOSE RC-FILE-DESC
           EXIT PROGRAM.     
       200-INPUT-ROUTINE.
           DISPLAY SCRN-CLEAR
           DISPLAY SCRN-SID
           DISPLAY SCRN-TYPE
           ACCEPT SCRN-TYPE
           DISPLAY SCRN-OWED
           ACCEPT SCRN-OWED
           DISPLAY SCRN-PAID
           ACCEPT SCRN-PAID
           DISPLAY SCRN-CLEAR
           MOVE SPACES TO RC-REC
           MOVE 1 TO WS-KEY
           READ RC-FILE-DESC
           MOVE RC-REC TO RC-EOF-REC
           MOVE RC-EOF TO WS-TMP-EOF-PNTR
           ADD 1 TO RC-EOF
           REWRITE RC-REC FROM RC-EOF-REC
           IF SF-RCT-REC-PNTR > 0 THEN
             MOVE SF-RCT-REC-PNTR TO WS-KEY
             READ RC-FILE-DESC
             PERFORM UNTIL RC-NEXT-PNTR = 0
               MOVE RC-NEXT-PNTR TO WS-KEY
               READ RC-FILE-DESC
             END-PERFORM
             MOVE WS-TMP-EOF-PNTR TO RC-NEXT-PNTR
             REWRITE RC-REC
           ELSE
             CLOSE SF-FILE-DESC
             OPEN I-O SF-FILE-DESC
             MOVE SF-RECORD TO WS-SF-RECORD
             MOVE WS-TMP-EOF-PNTR TO WS-SF-RCT-REC-PNTR
             REWRITE SF-RECORD FROM WS-SF-RECORD
             CLOSE SF-FILE-DESC
             OPEN INPUT SF-FILE-DESC
           END-IF
           MOVE WS-TMP-EOF-PNTR TO WS-KEY
           MOVE WS-RC-RCT-TYPE TO RC-TYPE
           MOVE WS-SNO TO WS-RC-STU-NUM
           ADD WS-NEW-AMT-OWED TO WS-RC-AMT-OWED
           ADD WS-NEW-AMT-PAID TO WS-RC-AMT-PAID
      *    MOVE WS-FA-AWARD-AMNT TO FA-AWARD-AMNT
      *    MOVE WS-SNO TO WS-FA-STU-NUM
           MOVE WS-NEW-AMT-OWED TO WS-RC-AMT-OWED
           MOVE WS-NEW-AMT-PAID TO WS-RC-AMT-PAID
           MOVE ZEROS TO WS-RC-NEXT-PNTR
           WRITE RC-REC FROM WS-RC-DATA-REC
           DISPLAY SCRN-MORE-DATA
           ACCEPT SCRN-MORE-DATA.
       300-NOT-EXISTING-ROUTINE.
           DISPLAY SCRN-CLEAR
           DISPLAY 'STUDENT NOT IN SYSTEM'
           DISPLAY 'HIT ENTER TO GO BACK TO MAIN SCREEN'
           MOVE 'NO ' TO MORE-DATA
           ACCEPT ANS.
