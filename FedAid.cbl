       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-FIN-AID.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT FA-FILE-DESC ASSIGN TO 'PROJ-FIN-AID.REL'
                ORGANIZATION IS RELATIVE
                ACCESS IS RANDOM
                RELATIVE KEY IS WS-KEY.
            SELECT STU-FILE-DESC ASSIGN TO 'STU-REC.DAT'
                ORGANIZATION IS INDEXED
                ACCESS IS RANDOM
                FILE STATUS IS WS-STATUS
                RECORD KEY IS SF-STU-NUM.
       DATA DIVISION.
       FILE SECTION.
       FD FA-FILE-DESC IS EXTERNAL RECORD CONTAINS 24 CHARACTERS.
       COPY FA-FILE-DESC.
       FD STU-FILE-DESC IS EXTERNAL RECORD CONTAINS 63 CHARACTERS.
       COPY SF-FILE-DESC.
       WORKING-STORAGE SECTION.
       01  WS-KEY                    PIC 9(5)  VALUE ZEROS.
       01  WS-CONT-REC               PIC 9(5)  VALUE ZEROS.
       01  WS-OPTION                 PIC X.
       01  MORE-DATA                 PIC XXX.
       01  WS-KEY-COUNTER            PIC 9(5)  VALUE 00001.
       01  WS-TMP-EOF-PNTR           PIC 9(5).
       01  FA-EOF-REC.
           05  FA-EOF PIC 9(5).
           05  FILLER PIC X(19).
       01  WS-FA-DATA-REC.
           05  WS-FA-AWARD-CODE PIC 9(4).
           05  WS-FA-AWARD-AMNT PIC 9(4)V99.
           05  WS-FA-STU-NUM PIC 9(9).
           05  WS-FA-NEXT-PNTR  PIC 9(5).
       01  WS-SF-RECORD.
           05  WS-SF-STU-NUM      PIC 9(9).
           05  FILLER             PIC X(39).
           05  WS-SF-FIN-AID-PNTR PIC 9(5).
           05  FILLER             PIC X(10).
       01  WS-STATUS                 PIC XX.
       01  WS-SNO                    PIC 9(9).
       01  ANS   PIC X.
       SCREEN SECTION.
       01 SCRN-SID.
          05 LINE 3 COLUMN 1 VALUE 'STUDENT ID NUMBER: '.
          05 LINE 3 COLUMN 27 PIC 9(9) FROM WS-SNO.
       01 SCRN-NAME.
          05 LINE 5 COLUMN 1 VALUE 'ENTER FINANCIAL AID NAME: '.
          05 LINE 5 COLUMN 28 PIC 9(4) TO WS-FA-AWARD-CODE.
       01 SCRN-AMOUNT.
          05 LINE 7 COLUMN 1 VALUE 'ENTER FINANCIAL AID AMOUNT: '.
          05 LINE 7 COLUMN 30 PIC 9(5) TO WS-FA-AWARD-AMNT.
       01 SCRN-MORE-DATA.
          05 BLANK SCREEN.
          05 LINE 1 COLUMN 1 VALUE
                  'DO YOU WANT TO ADD MORE DATA? (YES/NO): '.
          05 LINE 1 COLUMN 41 PIC X(3) TO MORE-DATA.
       01 SCRN-CLEAR.
          05 BLANK SCREEN.
       PROCEDURE DIVISION USING WS-SNO.
       100-MAIN-MODULE.
           OPEN I-O FA-FILE-DESC
           OPEN INPUT SF-FILE-DESC
           MOVE SPACES TO MORE-DATA
           PERFORM UNTIL MORE-DATA = 'NO '
             MOVE SPACES TO SF-RECORD
             MOVE WS-SNO TO SF-STU-NUM
             READ SF-FILE-DESC
               INVALID KEY PERFORM 300-NOT-EXISTING-ROUTINE
               NO INVALID KEY PERFORM 200-INPUT-ROUTINE
             END-READ
           END-PERFORM
           CLOSE SF-FILE-DESC
           CLOSE FA-FILE-DESC
           EXIT PROGRAM.
      *    IF WS-OPION = "A"
      *    PERFORM 300-WRITE-ROUTINE
      *    END-IF
      *    IF WS-OPION = "P"
      *    PERFORM 700-PNTR-ROUTINE
      *    END-IF
       200-INPUT-ROUTINE.
           DISPLAY SCRN-SID
           ACCEPT SCRN-SID
           DISPLAY SCRN-NAME
           ACCEPT SCRN-NAME
           DISPLAY SCRN-AMOUNT
           ACCEPT SCRN-AMOUNT
           DISPLAY SCRN-CLEAR
           MOVE SPACES TO FA-DATA-REC
           MOVE 1 TO WS-KEY
           READ FA-FILE-DESC
           MOVE FA-DATA-REC TO FA-EOF-REC
           MOVE FA-EOF TO WS-TMP-EOF-PNTR
           ADD 1 TO FA-EOF
           REWRITE FA-FILE-DESC FROM FA-EOF-REC
           IF WS-SF-FIN-AID-PNTR > 0 THEN
             MOVE SF-FIN-AID-PNTR TO WS-KEY
             READ FA-FILE-DESC
             PERFORM UNTIL FA-NEXT-PNTR = 0
               MOVE FA-NEXT-PNTR TO WS-KEY
               READ FA-FILE-DESC
             END-PERFORM
             MOVE WS-TMP-EOF-PNTR TO FA-NEXT-PNTR
             REWRITE FA-DATA-REC
           ELSE
             CLOSE SF-FILE-DESC
             OPEN I-O SF-FILE-DESC
             MOVE SF-RECORD TO WS-SF-RECORD
             MOVE WS-TMP-EOF-PNTR TO WS-SF-FIN-AID-PNTR
             REWRITE SF-RECORD FROM WS-SF-RECORD
             CLOSE SF-FILE-DESC
             OPEN INPUT SF-FILE-DESC
           END-IF
           MOVE WS-TMP-EOF-PNTR TO WS-KEY
           MOVE WS-FA-AWARD-CODE TO FA-AWARD-CODE
           MOVE WS-FA-AWARD-AMNT TO FA-AWARD-AMNT
           MOVE WS-SNO TO WS-FA-STU-NUM
           MOVE ZEROS TO WS-FA-NEXT-PNTR
           WRITE FA-DATA-REC FROM WS-FA-DATA-REC
           DISPLAY SCRN-MORE-DATA
           ACCEPT SCRN-MORE-DATA.
       300-NOT-EXISTING-ROUTINE.
           DISPLAY BLANK-SCREEN
           DISPLAY 'STUDENT NOT IN SYSTEM'
           DISPLAY 'HIT ENTER TO GO BACK TO MAIN SCREEN'
           MOVE 'NO ' TO MORE-DATA
           ACCEPT ANS.
      *500-ERROR-ROUTINE.
      *    DISPLAY 'ERROR'
      *    CLOSE FA-FILE-DESC
      *          STU-FILE-DESC
      *    EXIT PROGRAM.
      *600-REWRITE-ROUTINE.
      *    IF SF-FIN-AID-PNTR IS EQUAL TO 00000
      *       MOVE FA-EOF TO SF-FIN-AID-PNTR
      *       REWRITE SF-RECORD
      *       MOVE FA-EOF TO WS-KEY
      *       PERFORM 300-WRITE-ROUTINE
      *    ELSE
      *       MOVE SF-FIN-AID-PNTR TO WS-KEY
      *       PERFORM 700-PNTR-ROUTINE.
       700-PNTR-ROUTINE.
           READ FA-FILE-DESC
           IF  FA-NEXT-PNTR IS EQUAL TO 00000
               MOVE WS-CONT-REC TO FA-NEXT-PNTR
               REWRITE FA-REC
               MOVE WS-CONT-REC TO WS-KEY
               MOVE WS-AWRD-CD TO FA-AWARD-CODE
               MOVE WS-AWRD-AMNT TO FA-AWARD-AMT
               MOVE 00000 TO FA-NEXT-PNTR
               WRITE FA-REC
                 INVALID KEY DISPLAY 'INVALID KEY' WS-KEY
               END-WRITE
               ADD 1 TO WS-CONT-REC
               MOVE 1 TO WS-KEY
               MOVE WS-CONT-REC TO FA-EOF
               REWRITE FA-CONTROL
           ELSE
               MOVE FA-NEXT-PNTR TO WS-KEY
               PERFORM 800-NEXT-ROUTINE
           END-IF.
       800-NEXT-ROUTINE.
           PERFORM 700-PNTR-ROUTINE.
