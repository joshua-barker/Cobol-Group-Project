       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MORE-TASKS   PIC X(3) VALUE 'YES'.
       01 OPTION       PIC X VALUE SPACES.
       01 WS-SNO       PIC 9(9).
       SCREEN SECTION.
       01 CLEAR-SCREEN.
           05 BLANK SCREEN.
       01 SNO-SCREEN.
           05 LINE 22 COL 15 VALUE "ENTER STUDENT SSN: ".
           05 LINE 22 COL 50 PIC 9(9) TO WS-SNO.
       01 MENU-SCREEN.
           05 LINE 3  COL 35 VALUE "MAIN MENU SCREEN".
           05 LINE 6  COL 15 VALUE "ADD STUDENT      :A".
           05 LINE 7  COL 15 VALUE "ADD CLASS        :C".
           05 LINE 8  COL 15 VALUE "FINANCIAL AID    :F".
           05 LINE 9  COL 15 VALUE "ADD RECEIPTS     :R".
           05 LINE 10 COL 15 VALUE "ENROLLMENT       :E".
           05 LINE 11 COL 15 VALUE "PRINT A REPORT   :P".
           05 LINE 12 COL 15 VALUE "EXIT             :X".
           05 LINE 14 COL 15 VALUE "ENTER LETTER OF SELECTION: ".
           05 LINE 14 COL 43 PIC X TO OPTION.
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM UNTIL OPTION = 'X' OR 'x'
               DISPLAY CLEAR-SCREEN
               DISPLAY MENU-SCREEN
               ACCEPT MENU-SCREEN
               IF OPTION = 'A' OR 'F' OR 'R' OR 'E'
                   DISPLAY SNO-SCREEN
                   ACCEPT SNO-SCREEN
                   IF OPTION = 'A'
                       CALL 'PROJ-ADD-STUDENT' USING WS-SNO                   
                   ELSE IF OPTION = 'F'
                       CALL 'PROJ-FIN-AID' USING WS-SNO
                   ELSE IF OPTION = 'R'
                       CALL 'PROJ-RC' USING WS-SNO
                   ELSE 
                       CALL 'PROJ-ENROLLMENT' USING WS-SNO                   
                   END-IF
               ELSE IF OPTION = 'C'
                       CALL 'PROJ-ADD-CLASS'
               ELSE IF OPTION = 'P'
                       CALL 'PROJ-PRINT-MASTER'
               END-IF
               DISPLAY CLEAR-SCREEN
           END-PERFORM
           STOP RUN.
