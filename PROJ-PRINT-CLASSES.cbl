       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-PRINT-CLASSES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           SELECT CS-FILE-DESC
               ASSIGN TO "PROJ-CLASS-SECT.DAT"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS CS-CRN.
           SELECT CLASS-REPORT
               ASSIGN TO "CS-REPORT.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD CS-FILE-DESC IS EXTERNAL RECORD CONTAINS 23 CHARACTERS.
       COPY CS-SECT.
       FD CLASS-REPORT
          DATA RECORD IS REPORT-REC.
       01 REPORT-REC               PIC X(50).
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
          05                       PIC X(2)     VALUE SPACES.
          05                       PIC X(24)
               VALUE "COMPLETE CLASS LISTING".
          05 MONTH-H1              PIC X(2).
          05                       PIC X        VALUE "/".
          05 DAY-H1                PIC X(2).
          05                       PIC X        VALUE "/".
          05 YEAR-H1               PIC X(4).
          05                       PIC X(9)
               VALUE "   PAGE ".
          05 PAGE-NO-H1            PIC 9(2).
          05                       PIC X(3)    VALUE SPACES.
       01 HEADING-2.
          05                       PIC X(2)     VALUE SPACES.
          05                       PIC X(5)     VALUE "CRN".
          05                       PIC X(3)     VALUE SPACES.
          05                       PIC X(8)     VALUE "CRS CODE".
          05                       PIC X(3)     VALUE SPACES.
          05                       PIC X(11)    VALUE "COURSE DESC".
          05                       PIC X(3)     VALUE SPACES.
          05                       PIC X(10)    VALUE "CRED HOURS".
          05                       PIC X(5)     VALUE SPACES.
       01 DETAIL-LINE.
          05                       PIC X(2)     VALUE SPACES.
          05 CRN-OUT               PIC 9(5).
          05                       PIC X(4)     VALUE SPACES.
          05 CRS-CODE-OUT          PIC X(6).
          05                       PIC X(5)     VALUE SPACES.
          05 DESC-OUT              PIC X(10).
          05                       PIC X(6)     VALUE SPACES.
          05 CRED-HR-OUT           PIC 9(2).
          05                       PIC X(10)    VALUE SPACES.
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT CS-FILE-DESC
                OUTPUT CLASS-REPORT
           PERFORM 200-HEADING-RTN.
           MOVE "YES" TO ARE-THERE-MORE-RECORDS
           PERFORM UNTIL NO-MORE-RECORDS
               READ CS-FILE-DESC NEXT RECORD
                   AT END MOVE "NO " TO ARE-THERE-MORE-RECORDS
                   NOT AT END PERFORM 300-PRINT-RTN
               END-READ
           END-PERFORM
           CLOSE CS-FILE-DESC
                 CLASS-REPORT
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
           MOVE CS-CRN TO CRN-OUT
           MOVE CS-CRS-CODE TO CRS-CODE-OUT
           MOVE CS-DESC TO DESC-OUT
           MOVE CS-CRED-HR TO CRED-HR-OUT
           IF WS-LINE-CT > 55
               PERFORM 200-HEADING-RTN
           END-IF
           WRITE REPORT-REC FROM DETAIL-LINE AFTER 1
           ADD 1 TO WS-LINE-CT.
