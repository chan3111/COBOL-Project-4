       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT-4.
       AUTHOR. Chandler Newman-Reed.

      *  This program produces a Payroll Register based on the
      *  data contained in an Employee Record (EMPFILE5.TXT)

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *     SELECT  EMPLOYEE-FILE-IN
      *         ASSIGN  "EMPFIL3.TXT"
      *             ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT EMPLOYEE-FILE-IN 
               ASSIGN TO "EMPFILE5.txt"
               ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL
               RECORD KEY IS EMP-NUMBER-IN.
       	   
                   
           SELECT PAY-RATE-FILE-IN
               ASSIGN  "PAYRATES.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT  PAYROLL-RECORDS-FILE-OUT
               ASSIGN  "PAYRECORDS.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       FD  EMPLOYEE-FILE-IN.
       01  PAYROLL-RECORD-IN.
           05  EMP-NUMBER-IN       PIC 9(9).
           05  EMP-LAST-NAME-IN    PIC X(13).
           05  EMP-INITIALS-IN     PIC X(2).
           05  JOB-TYPE-IN         PIC X(3).
           05  HOURS-WORKED-IN     PIC 9(2)V99.
           05  TAX-IND-IN          PIC 9(1).
           05  PENSION-RATE        PIC V99.
           05  HEALTH-RATE         PIC V99.

       FD  PAYROLL-RECORDS-FILE-OUT.
       01  RECORD-OUT    PIC X(80).
       
       FD PAY-RATE-FILE-IN.
       01  PAY-RATE-IN.
           05  CLASS-IN    PIC X(3).
           05  RATE-IN PIC 9(2)V99.

       WORKING-STORAGE SECTION.

       01  REPORT-HEADER.
           05  FILLER          PIC X(9)  VALUE SPACES.
           05  FILLER          PIC X(16) VALUE "PAYROLL REGISTER".
           05  FILLER          PIC X(10) VALUE SPACES.
           05  DATE-REGISTER   PIC X(13).
           
       01  REPORT-HEADER-SUMMARY.
           05  FILLER          PIC X(9)  VALUE SPACES.
           05  FILLER          PIC X(15) VALUE "PAYROLL SUMMARY".
           05  FILLER          PIC X(10) VALUE SPACES.
           05  DATE-SUMMARY    PIC X(13).
           
       01  DATE-OUT.
           05 WEEK-OUT     PIC X(3).
           05 FILLER       PIC X(1) VALUE SPACE.
           05 DAY-OUT      PIC 9(2).
           05 FILLER       PIC X(1) VALUE SPACE.
           05 MONTH-OUT    PIC X(3).
           05 FILLER       PIC X(1) VALUE SPACE.
           05 YEAR-OUT     PIC 9(2).
           
           
       COPY "C:\COBOLPROJ4\TAXTBL.DAT".

       01  PAY-RATE-TABLE OCCURS 5 TIMES.
           05  CLASS-NAME  PIC X(3).
           05  PAY-RATE    PIC 9(2)V99.

       01  COLUMN-HEADER.
           05  FILLER  PIC X(5)   VALUE   SPACES.
           05  FILLER  PIC X(4)   VALUE  "NAME".
           05  FILLER  PIC X(23)  VALUE   SPACES.
           05  FILLER  PIC X(5)   VALUE  "GROSS".
           05  FILLER  PIC X(8)   VALUE   SPACES.
           05  FILLER  PIC X(3)   VALUE  "TAX".
           05  FILLER  PIC X(3)   VALUE   SPACES.
           05  FILLER  PIC X(7)   VALUE  "PENSION".
           05  FILLER  PIC X(4)   VALUE   SPACES.
           05  FILLER  PIC X(6)   VALUE  "HEALTH".
           05  FILLER  PIC X(3)   VALUE   SPACES.
           05  FILLER  PIC X(3)   VALUE   "NET".
           
       01  DATE-IN.
           05 YEAR-IN  PIC 9(2).
           05 MONTH-IN PIC 9(2).
           05 DAY-IN   PIC 9(2).
       
       01  WEEK-DAY.
           05  WEEK-IN PIC 9(1).
           
       01  WEEK-NAMES.
           05  FILLER  PIC X(3) VALUE "MON".
           05  FILLER  PIC X(3) VALUE "TUE".
           05  FILLER  PIC X(3) VALUE "WED".
           05  FILLER  PIC X(3) VALUE "THU".
           05  FILLER  PIC X(3) VALUE "FRI".
           05  FILLER  PIC X(3) VALUE "SAT".
           05  FILLER  PIC X(3) VALUE "SUN".
           
       01  WEEK-NAME-TABLE REDEFINES WEEK-NAMES.
           05  WEEK-NAME OCCURS 7 TIMES PIC X(3).
           
           
       COPY "C:\COBOLPROJ4\MONTHTBL.DAT".
       
       01  FLAGS.
           05  EOF-FLAG    PIC X(3) VALUE "NO ".
           05  FOUND-FLAG  PIC X(1) VALUE "F".

       01  COUNTERS.
           05  EMPLOYEE-RECORDS-READ-CTR       PIC 9(3).
           05  REGISTER-RECORDS-WRITTEN-CTR    PIC 9(3).
           05  SUB PIC 9(1).
           05  COUNTER PIC 9(2).
           05  PAY-RATE-INDEX  PIC 9(1).

       01  PAY-ACCUMULATORS.
           05  TOTAL-GROSS-PAY     PIC 9(6)V99.
           05  TOTAL-NET-PAY       PIC 9(6)V99.
           05  TOTAL-HEALTH        PIC 9(6)V99.
           05  TOTAL-TAX           PIC 9(6)V99.
           05  TOTAL-PENSION       PIC 9(6)V99.

       01  REGISTER-PAY-VALUES.
           05  GROSS-PAY-WS    PIC 9(4)V99.
           05  NET-PAY-WS      PIC 9(4)V99.
           05  TAX-WS          PIC 9(4)V99.
           05  HEALTH-WS       PIC 9(3)V99.
           05  OT-HOURS        PIC 9(2)V99.
           05  PENSION-WS      PIC 9(4)V99.


       01  DEDUCTION-PAY-VALUES.
           05  REGULAR-HOURS   PIC 9(2)     VALUE  40.
           05  OT-FACTOR       PIC 9(1)V9   VALUE  1.5.

       01  REGISTER-DETAIL-RECORD.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  EMP-INITIALS-OUT        PIC X(2).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  EMP-LAST-NAME-OUT       PIC X(13).
           05  FILLER                  PIC X(7) VALUES SPACES.
           05  GROSS-PAY-OUT           PIC Z,ZZ9.99.
           05  FILLER                  PIC X(3) VALUES SPACES.
           05  TAX-DEDUCTION-OUT       PIC Z,ZZ9.99.
           05  FILLER                  PIC X(4) VALUES SPACES.
           05  PENSION-OUT             PIC ZZ9.99.
           05  FILLER                  PIC X(4) VALUES SPACES.
           05  HEALTH-INS-OUT          PIC ZZ9.99.
           05  FILLER                  PIC X(1) VALUE SPACES.
           05  NET-PAY-OUT             PIC Z,ZZ9.99.

       01  SUMMARY-RECORDS.
           05  TOTAL-GROSS-SUMMARY.
               10  FILLER              PIC X(6) VALUE SPACES.
               10  FILLER              PIC X(11) VALUE "TOTAL GROSS".
               10  FILLER              PIC X(4)  VALUE SPACES.
               10  TOTAL-GROSS-PAY-OUT  PIC $$,$$$,$$9.99.

           05  TOTAL-NET-SUMMARY.
               10 FILLER               PIC X(6)  VALUE SPACES.
               10 FILLER               PIC X(9)  VALUE "TOTAL NET".
               10 FILLER               PIC X(6)  VALUE SPACES.
               10 TOTAL-NET-PAY-OUT    PIC $$,$$$,$$9.99.

           05  TOTAL-TAX-SUMMARY.
               10 FILLER               PIC X(6)  VALUE SPACES.
               10 FILLER               PIC X(9)  VALUE "TOTAL TAX".
               10 FILLER               PIC X(6)  VALUE SPACES.
               10 TOTAL-TAX-OUT        PIC $$,$$$,$$9.99.

           05  TOTAL-HEALTH-SUMMARY.
               10 FILLER               PIC X(6)  VALUE SPACES.
               10 FILLER               PIC X(12)  VALUE "TOTAL HEALTH".
               10 FILLER               PIC X(3)  VALUE SPACES.
               10 TOTAL-HEALTH-OUT     PIC $$,$$$,$$9.99.

           05  TOTAL-PENSION-SUMMARY.
               10 FILLER               PIC X(6)  VALUE SPACES.
               10 FILLER               PIC X(13)  VALUE "TOTAL PENSION".
               10 FILLER               PIC X(2)  VALUE SPACES.
               10 TOTAL-PENSION-OUT    PIC $$,$$$,$$9.99.

           05  RECORDS-READ-SUMMARY.
               10 FILLER   PIC X(6)    VALUE SPACES.
               10 FILLER   PIC X(12)   VALUE "RECORDS READ".
               10 FILLER   PIC X(5)    VALUE SPACES.
               10 RECORDS-READ-CTR-OUT PIC ZZ9.

           05  RECORDS-WRITTEN-SUMMARY.
               10 FILLER   PIC X(6)    VALUE SPACES.
               10 FILLER   PIC X(15)   VALUE "RECORDS WRITTEN".
               10 FILLER   PIC X(2)    VALUE SPACES.
               10 RECORDS-WRITTEN-CTR-OUT  PIC ZZ9.

       PROCEDURE DIVISION.

       100-PRODUCE-PAY-REGISTER.
      *  ------------------------------------------------------------
      *  This module controls the program execution.
      *  The Initialization establishes the files(open), writes report
      *  and column headers and reads the first record from the input
      *  file (EMPLOYEE-FILE-IN)
      *  establishes the file(open)
      *
      *  Then the mainline (200-PRODVCE-PAY-RECORDS) is executed until
      *  the last record is read and processed.
      *
      *  The termination routine (200-TERMINATE-PAY-REGISTER) is
      *  executed once all pay records have been processed. This
      *  routine writes the Summary File and then closes all files.
      * -------------------------------------------------------------

           PERFORM 200-INIT-PAY-REGISTER.
           PERFORM 200-PRODUCE-PAY-RECORDS
               UNTIL EOF-FLAG  = "YES".
           PERFORM  200-TERMINATE-PAY-REGISTER.
           STOP RUN.


       200-INIT-PAY-REGISTER.
           PERFORM  700-GET-DATE.
           PERFORM  700-MOVE-DATE.
           PERFORM  700-OPEN-FILES.
           PERFORM  700-INITIALIZE-FIELDS.
           PERFORM 700-LOAD-PAY-RATE-TABLE
               VARYING SUB FROM 1 BY 1
               UNTIL SUB IS GREATER THAN 5
               OR EOF-FLAG IS EQUAL TO "YES".
           PERFORM  700-WRITE-REPORT-HDR.
           PERFORM  700-WRITE-COLUMN-HDR.
           PERFORM  700-READ-EMPLOYEE-RECORD.

      * --------------------------------------------------------------
      * The module 200-PRODUCE-PAY-RECORDS is the main line
      * of the program and executes the major finctions of the main
      * line
      * --------------------------------------------------------------

       200-PRODUCE-PAY-RECORDS.
           PERFORM 700-FIND-PAY-RATE
               VARYING SUB FROM 1 BY 1
               UNTIL SUB IS GREATER THAN 5
               OR FOUND-FLAG IS EQUAL TO "T".
           PERFORM 700-RESET-FOUND-FLAG.
           IF HOURS-WORKED-IN GREATER THAN 40
               PERFORM  700-CALC-OT-GROSS-PAY
           ELSE PERFORM  700-CALC-REGULAR-GROSS-PAY.
           PERFORM  700-CALC-TAX.
           PERFORM  700-CALC-INSURANCE.
           PERFORM  700-CALC-PENSION.
           PERFORM  700-CALC-NET-PAY.
           PERFORM  700-WRITE-PAY-REGISTER-RECORD.
           PERFORM  700-CALC-PAY-TOTALS.
           PERFORM  700-READ-EMPLOYEE-RECORD.

       700-GET-DATE.
           ACCEPT DATE-IN FROM DATE.
           ACCEPT WEEK-IN FROM DAY-OF-WEEK.
           
       700-MOVE-DATE.
           MOVE YEAR-IN TO YEAR-OUT.
           MOVE DAY-IN TO DAY-OUT.
           MOVE WEEK-NAME(WEEK-IN) TO WEEK-OUT.
           MOVE MONTH-NAME(MONTH-IN) TO MONTH-OUT.
           MOVE DATE-OUT TO DATE-SUMMARY
                            DATE-REGISTER.
           
       200-TERMINATE-PAY-REGISTER.
           PERFORM  700-WRITE-SUMMARY.
           PERFORM  700-CLOSE-FILES.

       700-OPEN-FILES.
           OPEN  INPUT  EMPLOYEE-FILE-IN.
           OPEN  INPUT  PAY-RATE-FILE-IN.
           OPEN  OUTPUT PAYROLL-RECORDS-FILE-OUT.

       700-INITIALIZE-FIELDS.
           INITIALIZE  COUNTERS
                       PAY-ACCUMULATORS
                       REGISTER-PAY-VALUES.

       700-WRITE-REPORT-HDR.
           WRITE RECORD-OUT
                   FROM  REPORT-HEADER BEFORE ADVANCING PAGE.


       700-WRITE-COLUMN-HDR.
           WRITE RECORD-OUT
                   FROM COLUMN-HEADER 
                       AFTER ADVANCING 2 LINES.

       700-READ-EMPLOYEE-RECORD.
           READ  EMPLOYEE-FILE-IN
               AT END  MOVE "YES"  TO  EOF-FLAG
                   NOT AT END ADD 1 TO EMPLOYEE-RECORDS-READ-CTR.

       700-CALC-OT-GROSS-PAY.
           SUBTRACT REGULAR-HOURS FROM  HOURS-WORKED-IN
               GIVING OT-HOURS.
           MULTIPLY OT-FACTOR  BY  OT-HOURS.
           ADD OT-HOURS  TO  REGULAR-HOURS
               GIVING HOURS-WORKED-IN.
           MULTIPLY HOURS-WORKED-IN BY PAY-RATE(PAY-RATE-INDEX)
               GIVING GROSS-PAY-WS.

       700-CALC-REGULAR-GROSS-PAY.
           MULTIPLY HOURS-WORKED-IN  BY  PAY-RATE(PAY-RATE-INDEX)
               GIVING GROSS-PAY-WS.

       700-CALC-TAX.
           CALL "TAXCALC" USING GROSS-PAY-WS, TAX-RATES(TAX-IND-IN), 
           TAX-WS.

       700-CALC-INSURANCE.
           MULTIPLY  GROSS-PAY-WS  BY  HEALTH-RATE
               GIVING HEALTH-WS.
               
       700-CALC-PENSION.
           MULTIPLY  GROSS-PAY-WS  BY  PENSION-RATE
               GIVING PENSION-WS.

       700-CALC-NET-PAY.
           CALL "C:\COBOLPROJ4\NETCALC" USING TAX-WS, HEALTH-WS,
               PENSION-WS, GROSS-PAY-WS, NET-PAY-WS.

       700-WRITE-PAY-REGISTER-RECORD.
           MOVE  EMP-INITIALS-IN  TO EMP-INITIALS-OUT.
           MOVE  EMP-LAST-NAME-IN  TO EMP-LAST-NAME-OUT.
           MOVE  GROSS-PAY-WS  TO  GROSS-PAY-OUT.
           MOVE  TAX-WS  TO  TAX-DEDUCTION-OUT.
           MOVE  HEALTH-WS  TO  HEALTH-INS-OUT.
           MOVE  PENSION-WS TO  PENSION-OUT.
           MOVE  NET-PAY-WS  TO  NET-PAY-OUT.
           IF  COUNTER IS EQUAL TO 9
           WRITE RECORD-OUT
                   FROM   REGISTER-DETAIL-RECORD
               WRITE RECORD-OUT
                   FROM COLUMN-HEADER AFTER ADVANCING PAGE
               MOVE 0 TO COUNTER
           ELSE IF COUNTER IS EQUAL TO 0
           WRITE RECORD-OUT
                   FROM REGISTER-DETAIL-RECORD AFTER ADVANCING 2 LINES
           ADD 1 TO COUNTER
           ELSE WRITE RECORD-OUT FROM REGISTER-DETAIL-RECORD
                ADD 1 TO COUNTER.
           ADD  1 TO  REGISTER-RECORDS-WRITTEN-CTR.
           

       700-CALC-PAY-TOTALS.
           ADD  GROSS-PAY-WS  TO  TOTAL-GROSS-PAY.
           ADD  NET-PAY-WS    TO  TOTAL-NET-PAY.
           ADD  HEALTH-WS     TO  TOTAL-HEALTH.
           ADD  TAX-WS        TO  TOTAL-TAX.
           ADD  PENSION-WS    TO  TOTAL-PENSION.

       700-WRITE-SUMMARY.
       
           MOVE REPORT-HEADER-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT 
               AFTER ADVANCING PAGE.
               
           MOVE TOTAL-GROSS-PAY
               TO  TOTAL-GROSS-PAY-OUT.
           MOVE TOTAL-GROSS-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT 
               AFTER ADVANCING 2 LINES.

           MOVE TOTAL-NET-PAY
               TO TOTAL-NET-PAY-OUT.
           MOVE TOTAL-NET-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT.
           
           MOVE TOTAL-TAX
               TO TOTAL-TAX-OUT.
           MOVE TOTAL-TAX-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT.
           
           MOVE TOTAL-PENSION
               TO TOTAL-PENSION-OUT.
           MOVE TOTAL-PENSION-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT.
           
           MOVE TOTAL-HEALTH
               TO TOTAL-HEALTH-OUT.
           MOVE TOTAL-HEALTH-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT.

           MOVE EMPLOYEE-RECORDS-READ-CTR
               TO RECORDS-READ-CTR-OUT.
           MOVE RECORDS-READ-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT 
               AFTER ADVANCING 2 LINES.

           MOVE REGISTER-RECORDS-WRITTEN-CTR
               TO RECORDS-WRITTEN-CTR-OUT.
           MOVE RECORDS-WRITTEN-SUMMARY
               TO RECORD-OUT.
           WRITE RECORD-OUT.
           
       700-LOAD-PAY-RATE-TABLE.
           READ PAY-RATE-FILE-IN
               AT END MOVE "YES" TO EOF-FLAG
               NOT AT END MOVE PAY-RATE-IN TO PAY-RATE-TABLE(SUB).
               
       700-FIND-PAY-RATE.
           IF CLASS-NAME(SUB) = JOB-TYPE-IN
               MOVE "T" TO FOUND-FLAG.
               MOVE SUB TO PAY-RATE-INDEX.
               
       700-RESET-FOUND-FLAG.
           MOVE "F" TO FOUND-FLAG.

       700-CLOSE-FILES.
           CLOSE  EMPLOYEE-FILE-IN.
           CLOSE  PAYROLL-RECORDS-FILE-OUT.
           CLOSE  PAY-RATE-FILE-IN.

