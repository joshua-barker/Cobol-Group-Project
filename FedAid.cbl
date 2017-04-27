       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIN-AID.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT FA-FILE-DESC ASSIGN TO 'FINAID.REL'
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
       FD FA-FILE-DESC.
       01 FA-CONTROL.
          05 FA-EOF             PIC 9(5).
      *   05 FILLER             PIC 9(20).
       COPY FA-FILE-DESC.
       FD STU-FILE-DESC.
       COPY SF-FILE-DESC.
       WORKING-STORAGE SECTION.
       01  WS-KEY                    PIC 9(5)  VALUE 00001.
       01  WS-CONT-REC               PIC 9(5)  VALUE 00000.
       01  WS-OPION                  PIC X.
       01  MORE-DATA                 PIC XXX.
       01  WS-KEY-COUNTER            PIC 9(5)  VALUE 00001.
       01  WS-INPUT-DAT.
           05  WS-SSN                PIC 9(9).
           05  WS-AWRD-CD            PIC 9(4).
           05  WS-AWRD-AMNT         PIC 9999V99.
       01  WS-STATUS                 PIC XX.
       SCREEN SECTION.

       01 SCRN-SID.
          05 LINE 3 COLUMN 1 VALUE 'ENTER STUDENT ID NUMBER: '.
          05 LINE 3 COLUMN 27 PIC 9(9) TO WS-SSN.
       01 SCRN-NAME.
          05 LINE 5 COLUMN 1 VALUE 'ENTER FINANCIAL AID NAME: '.
          05 LINE 5 COLUMN 28 PIC 9(4) TO WS-AWRD-CD.
       01 SCRN-AMOUNT.
          05 LINE 7 COLUMN 1 VALUE 'ENTER FINANCIAL AID AMOUNT: '.
          05 LINE 7 COLUMN 30 PIC 9(5) TO WS-AWRD-AMNT.
       01 SCRN-MENU.
          05 LINE 1 COLUMN 1 VALUE 'ENTER (A)DD (P)RINT: '.
          05 LINE 1 COLUMN 30 PIC X TO WS-OPION.
       01 SCRN-MORE-DATA.
          05 BLANK SCREEN.
          05 LINE 1 COLUMN 1 VALUE
                  'DO YOU WANT TO ADD MORE DATA?: '.
          05 LINE 1 COLUMN 32 PIC X TO MORE-DATA.
       01 SCRN-CLEAR.
          05 BLANK SCREEN.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN I-O FA-FILE-DESC
           READ FA-FILE-DESC
           DISPLAY SCRN-CLEAR
           DISPLAY SCRN-MENU
           ACCEPT SCRN-MENU
           PERFORM 200-INPUT-ROUTINE
           PERFORM 400-SEARCH-ROUTINE
      *    IF WS-OPION = "A"
      *    PERFORM 300-WRITE-ROUTINE
      *    END-IF
      *    IF WS-OPION = "P"
      *    PERFORM 700-PNTR-ROUTINE
      *    END-IF
           CLOSE FA-FILE-DESC
                 STU-FILE-DESC
           EXIT PROGRAM.
       200-INPUT-ROUTINE.
           DISPLAY SCRN-SID
           ACCEPT SCRN-SID
           IF WS-OPION = 'A'
             DISPLAY SCRN-NAME
             ACCEPT SCRN-NAME
             DISPLAY SCRN-AMOUNT
             ACCEPT SCRN-AMOUNT
           END-IF
           DISPLAY SCRN-CLEAR.
       300-WRITE-ROUTINE.
           MOVE WS-AWRD-CD TO FA-AWARD-CODE
           MOVE WS-AWRD-AMNT TO FA-AWARD-AMT
           MOVE 00000 TO FA-NEXT-PNTR
           WRITE FA-REC
             INVALID KEY DISPLAY 'INVALID KEY' WS-KEY
           END-WRITE.
           ADD 1 TO WS-CONT-REC
           MOVE 1 TO WS-KEY
           MOVE WS-CONT-REC TO FA-EOF
           REWRITE FA-CONTROL.
       400-SEARCH-ROUTINE.
           OPEN I-O STU-FILE-DESC
           MOVE WS-SSN TO SF-STU-NUM
           READ STU-FILE-DESC
               INVALID KEY PERFORM 300-WRITE-ROUTINE
               NOT INVALID KEY PERFORM 600-REWRITE-ROUTINE
           END-READ.
       500-ERROR-ROUTINE.
           DISPLAY 'ERROR'
           CLOSE FA-FILE-DESC
                 STU-FILE-DESC
           EXIT PROGRAM.
       600-REWRITE-ROUTINE.
           IF SF-FIN-AID-PNTR IS EQUAL TO 00000
              MOVE FA-EOF TO SF-FIN-AID-PNTR
              REWRITE SF-RECORD
              MOVE FA-EOF TO WS-KEY
              PERFORM 300-WRITE-ROUTINE
           ELSE
              MOVE SF-FIN-AID-PNTR TO WS-KEY
              PERFORM 700-PNTR-ROUTINE.
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
