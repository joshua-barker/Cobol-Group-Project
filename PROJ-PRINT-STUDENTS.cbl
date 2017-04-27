       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-PRINT-CLASSES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           SELECT SF-FILE-DESC
               ASSIGN TO "PROJ-STU-FILE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS SF-STU-NUM.
           SELECT STUDENT-REPORT
               ASSIGN TO "SF-REPORT.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD SF-FILE-DESC IS EXTERNAL RECORD CONTAINS 63 CHARACTERS.
       COPY SF-FILE-DESC.
       FD STUDENT-REPORT
          DATA RECORD IS REPORT-REC.
       01 REPORT-REC               PIC X(70).
       WORKING-STORAGE SECTION.
       01 ANS                      PIC X.
       01 ARE-THERE-MORE-RECORDS   PIC X(3)     VALUE "YES".
          88 NO-MORE-RECORDS                    VALUE "NO".
       01 WS-LINE-CT               PIC 99       VALUE ZERO.
       01 DATE-WS.
          05 YEAR-WS               PIC XXXX.
          05 MONTH-WS              PIC XX.
          05 DAY-WS                PIC XX.
       01 WS-PAGE                  PIC 99       VALUE ZERO.
       01 HEADING-1.
          05                       PIC X(6)     VALUE SPACES.
          05                       PIC X(28)
               VALUE "COMPLETE STUDENT LISTING".
          05 MONTH-H1              PIC X(2).
          05                       PIC X        VALUE "/".
          05 DAY-H1                PIC X(2).
          05                       PIC X        VALUE "/".
          05 YEAR-H1               PIC X(4).
          05                       PIC X(9)
               VALUE "   PAGE ".
          05 PAGE-NO-H1            PIC 9(2).
          05                       PIC X(15)    VALUE SPACES.
       01 HEADING-2.
          05                       PIC X(2)     VALUE SPACES.
          05                       PIC X(9)     VALUE "STUD NUM".
          05                       PIC X(4)     VALUE SPACES.
          05                       PIC X(10)    VALUE "STUD NAME".
          05                       PIC X(3)     VALUE SPACES.
          05                       PIC X(12)    VALUE "STUD ADDRESS".
          05                       PIC X(3)     VALUE SPACES.
          05                       PIC X(10)    VALUE "STUD PHONE".
          05                       PIC X(4)     VALUE SPACES.
          05                       PIC X(9)     VALUE "STUD SSN".
          05                       PIC X(4)     VALUE SPACES.
       01 DETAIL-LINE.
          05                       PIC X(2)     VALUE SPACES.
          05 STU-NUM-OUT           PIC 9(9).
          05                       PIC X(4)     VALUE SPACES.
          05 NAME-OUT              PIC X(10).
          05                       PIC X(4)     VALUE SPACES.
          05 ADDRESS-OUT           PIC X(10).
          05                       PIC X(4)     VALUE SPACES.
          05 PHONE-OUT             PIC 9(10).
          05                       PIC X(4)     VALUE SPACES.
          05 SSN-OUT               PIC 9(9).
          05                       PIC X(4)     VALUE SPACES.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT SF-FILE-DESC
                OUTPUT STUDENT-REPORT
           PERFORM 200-HEADING-RTN.
           MOVE "YES" TO ARE-THERE-MORE-RECORDS
           PERFORM UNTIL NO-MORE-RECORDS
               READ SF-FILE-DESC NEXT RECORD
                   AT END MOVE "NO " TO ARE-THERE-MORE-RECORDS
                   NOT AT END PERFORM 300-PRINT-RTN
               END-READ
           END-PERFORM
           CLOSE SF-FILE-DESC
                 STUDENT-REPORT
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
           MOVE SF-STU-NUM TO STU-NUM-OUT
           MOVE SF-NAME TO NAME-OUT
           MOVE SF-ADDRESS TO ADDRESS-OUT
           MOVE SF-PHONE TO PHONE-OUT
           MOVE SF-SSN TO SSN-OUT
           IF WS-LINE-CT > 55
               PERFORM 200-HEADING-RTN
           END-IF
           WRITE REPORT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO WS-LINE-CT.
