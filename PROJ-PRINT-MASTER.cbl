       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJ-PRINT-MASTER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MORE-TASKS   PIC X(3) VALUE 'YES'.
       01 OPTION       PIC X VALUE SPACES.
       SCREEN SECTION.
       01 CLEAR-SCREEN.
           05 BLANK SCREEN.
       01 MENU-SCREEN.
           05 LINE 3  COL 35 VALUE "PRINT MENU SCREEN".
           05 LINE 6  COL 15 VALUE "PRINT STUDENTS      :S".
           05 LINE 7  COL 15 VALUE "PRINT CLASSES       :C".
           05 LINE 8  COL 15 VALUE "PRINT FIN AID       :F".
           05 LINE 9  COL 15 VALUE "PRINT RECEIPTS      :R".
           05 LINE 10 COL 15 VALUE "PRINT CLASS ROSTER  :E".
           05 LINE 11 COL 15 VALUE "PRINT STUD CLASSES  :A".
           05 LINE 12 COL 15 VALUE "RETURN TO MAIN MENU :X".
           05 LINE 13 COL 15 VALUE "ENTER LETTER OF SELECTION: ".
           05 LINE 13 COL 43 PIC X TO OPTION.
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM UNTIL OPTION = 'X' OR 'x'
               DISPLAY CLEAR-SCREEN
               DISPLAY MENU-SCREEN
               ACCEPT MENU-SCREEN
               IF OPTION = 'S'
                   CALL 'PROJ-PRINT-STUDENTS'
               ELSE IF OPTION = 'C'
                   CALL 'PROJ-PRINT-CLASSES'
               ELSE IF OPTION = 'F'
                   CALL 'PROJ-PRINT-FIN-AID'
               ELSE IF OPTION = 'R'
                   CALL '(PROJ-PRINT-RECEIPTS)'
               ELSE IF OPTION = 'E'
                   CALL '(PROJ-PRINT-ROSTER)'
               ELSE IF OPTION = 'A'
                   CALL 'PROJ-PRINT-STU-CLASS'
               ELSE
                   DISPLAY 'RETURNING TO MAIN MENU'
               END-IF
           END-PERFORM.
