       PROGRAM-ID. NETCALC AS "NETCALC".

       DATA DIVISION.
       LINKAGE SECTION.
       01  GROSS-PAY-LS    PIC 9(4)V99.
       01  NET-PAY-LS      PIC 9(4)V99.
       01  TAX-LS          PIC 9(4)V99.
       01  HEALTH-LS       PIC 9(3)V99.
       01  PENSION-LS      PIC 9(4)V99.
       

       PROCEDURE DIVISION 
           USING TAX-LS, HEALTH-LS, PENSION-LS, GROSS-PAY-LS, 
               NET-PAY-LS.
           SUBTRACT  TAX-LS HEALTH-LS PENSION-LS
               FROM  GROSS-PAY-LS  GIVING  NET-PAY-LS.
       END PROGRAM NETCALC.
