       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-PRINT-RECEIPTS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           SELECT RC-FILE-DESC
               ASSIGN TO "PROJ-REC.REL"
               ORGANIZATION IS RELATIVE
               ACCESS IS DYNAMIC
               RELATIVE KEY IS WS-KEY.
           SELECT RECEIPT-REPORT
               ASSIGN TO "RC-REPORT.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD RC-FILE-DESC IS EXTERNAL RECORD CONTAINS 35 CHARACTERS.
       COPY RC-FILE-DESC.
       FD RECEIPT-REPORT
          DATA RECORD IS REPORT-REC.
       01 REPORT-REC               PIC X(57).
       WORKING-STORAGE SECTION.
       01 ANS                      PIC X.
       01 WS-KEY                   PIC 9(5)     VALUE ZERO.
       01 ARE-THERE-MORE-RECORDS   PIC X(3)     VALUE "YES".
          88 NO-MORE-RECORDS                    VALUE "NO".
       01 WS-LINE-CT               PIC 99       VALUE ZERO.
       01 DATE-WS.
          05 YEAR-WS               PIC XXXX.
          05 MONTH-WS              PIC XX.
          05 DAY-WS                PIC XX.
       01 WS-PAGE                  PIC 99       VALUE ZERO.
       01 WS-CONTROL-REC.
          05 WS-TYPE               PIC X(11).
          05 WS-STU-NUM            PIC 9(9).
          05 WS-AMT-OWED           PIC 9(4)V99.
          05 WS-AMT-PAID           PIC 9(4)V99.
          05 WS-NEXT-PNTR          PIC 9(4).
       01 HEADING-1.
          05                       PIC X(6)     VALUE SPACES.
          05                       PIC X(28)
               VALUE "COMPLETE RECEIPT LISTING".
          05 MONTH-H1              PIC X(2).
          05                       PIC X        VALUE "/".
          05 DAY-H1                PIC X(2).
          05                       PIC X        VALUE "/".
          05 YEAR-H1               PIC X(4).
          05                       PIC X(9)
               VALUE "   PAGE ".
          05 PAGE-NO-H1            PIC 9(2).
          05                       PIC X(2)     VALUE SPACES.
       01 HEADING-2.
          05                       PIC X(2)     VALUE SPACES.
          05                       PIC X(10)    VALUE "TYPE".
          05                       PIC X(4)     VALUE SPACES.
          05                       PIC X(9)     VALUE "STUD S NO".
          05                       PIC X(4)     VALUE SPACES.
          05                       PIC X(8)     VALUE "AMT OWED".
          05                       PIC X(4)     VALUE SPACES.
          05                       PIC X(8)     VALUE "AMT PAID".
          05                       PIC X(8)     VALUE SPACES.
       01 DETAIL-LINE.
          05                       PIC X(2)     VALUE SPACES.
          05 TYPE-OUT              PIC X(11).
          05                       PIC X(4)     VALUE SPACES.
          05 S-NO-OUT              PIC 9(9).
          05                       PIC X(4)     VALUE SPACES.
          05 AMT-OWED-OUT          PIC 9(4)V99.
          05                       PIC X(6)     VALUE SPACES.
          05 AMT-PAID-OUT          PIC 9(4)V99.
          05                       PIC X(9)    VALUE SPACES.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT RC-FILE-DESC
                OUTPUT RECEIPT-REPORT
           PERFORM 200-HEADING-RTN.
           MOVE "YES" TO ARE-THERE-MORE-RECORDS
           MOVE 2 TO WS-KEY
           PERFORM UNTIL NO-MORE-RECORDS
               READ RC-FILE-DESC INTO WS-CONTROL-REC
                   INVALID KEY MOVE "NO " TO ARE-THERE-MORE-RECORDS
                   NOT INVALID KEY PERFORM 300-PRINT-RTN
               END-READ
           END-PERFORM
           CLOSE RC-FILE-DESC
                 RECEIPT-REPORT
           EXIT PROGRAM.
       200-HEADING-RTN.
           ADD 1 TO WS-PAGE
           MOVE WS-PAGE TO PAGE-NO-H1
           MOVE FUNCTION CURRENT-DATE TO DATE-WS
           MOVE MONTH-WS TO MONTH-H1
           MOVE DAY-WS TO DAY-H1
           MOVE YEAR-WS TO YEAR-H1
           MOVE SPACES TO REPORT-REC
           WRITE REPORT-REC AFTER PAGE
           WRITE REPORT-REC FROM HEADING-1 AFTER 6
           WRITE REPORT-REC FROM HEADING-2 AFTER 2
           MOVE SPACES TO REPORT-REC
           WRITE REPORT-REC AFTER 1
           MOVE 0 TO WS-LINE-CT
           ADD 10 TO WS-LINE-CT.
       300-PRINT-RTN.
           MOVE WS-TYPE TO TYPE-OUT
           MOVE WS-STU-NUM TO S-NO-OUT
           MOVE WS-AMT-OWED TO AMT-OWED-OUT
           MOVE WS-AMT-PAID TO AMT-PAID-OUT
           IF WS-LINE-CT > 55
               PERFORM 200-HEADING-RTN
           END-IF
           WRITE REPORT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO WS-LINE-CT
           ADD 1 TO WS-KEY.
