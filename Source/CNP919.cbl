       IDENTIFICATION DIVISION.
       PROGRAM-ID. CNP919.
      *****************************************************************
      *             E M P L O Y E E  "C A L L"                        *
      *****************************************************************
      *  DATE   INITIAL  LOG#   DESCRIPTION
      *-------- ------- ------  ---------------------------------------
      *09/23/91   MOO           ORIGINAL.
      *12/23/93   LMB           UNAVAILABLE STATUS MODIFICATIONS.
      *04/28/94   GER           TIMEZONE UPGRADES
      *05/06/94   DLO           MAINTAIN SYSTEM AVAIL LIST WHEN STATUS
      *                         CHANGES
      *12/12/94   LMB           PASS EMPLOYEE'S CRAFT TO P918.
      *05/01/95   PLS           ADD PROCESSING FOR FASTSLOW SPAREBOARD.
      *06/22/95   PLS           FOR FASTSLOW BOARDS, PASS ON THE SIDE
      *                         USED IN THE SELECTION PROCESS, WHEN
      *                         AVAILABLE.
      *07/12/95   FHK           GO TRAIN CHANGES.
      *07/17/95   PLS  CNC0079  SEND STATION TO CNP925 WHEN
      *                         CALCULATING PENALTIES.
      *09/18/95   PLS   A-134   RETAIN DATA THAT MAY POSSIBLY BE
      *                         NEEDED FOR HELDAWY CALCULATIONS, IN
      *                         CASE A CALL AND CANCEL IS DONE.
      *11/20/95   FHK   C-232   CHANGE ODT TO ORDERED FOR TIE BREAKER
      *12/11/95   CK    C-134   MODS TO DCAN PENALTY PROCESS.
      *01/18/96   JES   C-225   REVAMP DCAN.
      *04/18/1996 SRP CNC0086   SYSTEM GEN'ED VAC CREDIT PROCESSING.  *
      *05/07/96   FLW  CNC0006  DEFIFINTION OF WORK WEEK.
      *05/15/1996 SRP CNC0086   CHANGE TASK LIST MSG.                 *
      *05/21/1996 SRP  CNC0086  VAC EDITS -- CHANGE DATE ON WHICH TO  *
      *                         START CREDITING TICKETS.              *
      *11/27/96   FLW  BUGC329  BYPASS POINTER ADVANCE WHEN REJT      *
      *01/27/97   JES  YEAR2000 MODIFY FOR Y2K IN TIMEKEEPING.        *
      *                         ENLARGED TIMESLIP REC TO 1536.        *
      *03/07/97   FLW  BUGC341  DO NOT TAKE XB TURN OFF BOARD WHEN    *
      *                         GUARANTEE LOST WORK.                  *
      *04/30/97   JES  CNC0113  U.S.PAYROLL - HELDAWAY ELAPSED TIME   *
      *09/10/94   FLW  CNC0135  AV LIST FLAG DEFINITIONS              *
      *03/11/98   LMB  CNC0134  IF EMPLOYEE IS ALREADY WORKING AN     *
      *                         ASSIGNED JOB AND QUICK TIEUP IS       *
      *                         ALLOWED, QUICK TIE THE PERSON BEFORE  *
      *                         CALLING THE NEW JOB.                  *
      *07/09/98   LMB  C-425    PASS AJ SCHED END TIME TO P926.       *
      *08/05/98   LMB  C-426    NEW VERSION OF P925COMM.              *
      *08/26/98   LMB  CNC0160  CAPTURE EXTENDED RUN INFO.            *
      *                         CREATE ASGN FN HISTORY RECORD.        *
      *09/10/98   BLD  CNC0165  PASS TIMEKEEPING PROFILE FROM ALT CREW*
      *                         PROFILE TO P926-PGM.                  *
      *10/09/98   LMB  CNC0160  PASS HOME/AWAY TO P943.               *
      *11/20/98   RRR  C447     P1104 : CHANGE IF STATEMENT ALLOWING  *
      *                                 CHECK OF REST DAYS            *
      *12/14/98   AMF           YEAR 2000 SWEEP.                      *
      *04/23/99   AJK  CNC0228  ADDED SNAPSHOT LOGIC.                 *
      *09/15/99   NJB  CNC0190  JOB TYPE OVERRIDE FROM APCN
      *01/16/99   MOO  CNC0183  ADD CODE TO UPDATE REST STATUS.
      *02/28/00   NJB  CNC0270  REMOVED INTERNAL ORDER, ADDED RUN
      *                         NUMBER, FORCE ROUTE, AND BUSINESS AREA
      *03/24/00   AJK  CNC0275  ADD MERGER CLASSIFICATION LOGIC.
      *04/05/00   GER  CNC0261  IF ASGN-POOL FLAG SET AT HOME DONT CALC
      *                         ANY HELDAWAY AT ANY AWAY POINTS.
      *06/24/02   BLD  A-261    ONLY CHANGE CALL/SCAL TO CAO*/SCO* ON
      *                         EMPLOYEE'S FIRST CALL ON A HOLIDAY.
      *09/25/03   GER  C513     RELEASE TRACKING.
      * 03/31/04  NRL  CNC0331F RECOMPILE.
      *04/05/04   AJK  CNC0331G RESET LEAD-TIME ON MSTR3 RECORD AT CALL.
      *04/16/04   NRL  CNC0331G PASS THE EMP REST STATUS TO P926.
      *10/20/04   TRK  CNC0386F ADD YEAR TO HOLIDAY CONTROL READ
      *04/11/08   BLD  CNC0475  ACCEPT COMMENTS FROM TOPC.
      *02/04/09   JES  CNC0436B CHANGE LEAD-TIME FROM MSTR3 TO WSTELE.
      *07/08/09   JES  CNC0436B SPACE SHORT-CALL (REQ-CALL-TIME) ON
      *                         WSTELE AT SAME TIME AS LEAD-TIME (REQ-
      *                         LEAD-TIME).
      *07/11/09   AJK  CNC0484  HR 2095 - US REST STATUS
      *08/14/09   JES  CNC0436B NEW VALUE FOR LAYOFF-CODE-2
      *10/07/09   JES  CNC0436B #846 IVR-CALL ALLOWED WITH NOT-NOTIFIED
      *                         (LAYOFF-CODE-2 = '1')
      *04/01/10   JES  CNC0436I PASS TAXI TIME TO P926
      *07/14/10   JES  C869     PASS DEC-RULE-NO TO P926
      *03/16/11   AXG  CNC0505  PASSED 2 NEW COMMAREA VARIABLES
      *                         P919-CREW-RPT-LOCATION AND
      *                         P919-ALLOW-VCLL-FL TO P926 PGM
      *01/17/13   AXK  CNC0520  PASSED NEW COMMAREA VARIABLE
      *                         P919-ROUTE-DESIGNATION
      *07/17/13   AXK  CNC0534  PASSED NEW COMMAREA VARIABLE
      *                         P919-CALLED-FROM-HOME-FL
      *05/27/15   BXS  CNC0562  PASS CORRECT SERVICE TYPE FOR CROSS
      *                         SERVICE(LOCAL BEEN WORKED BY YARD RELIEF
      *                         OR VICE VERSA).
      *08/08/17   MFO  CNC0554  UPDATE AFFECTED EMP FOR TRACKING MU-TURN
      *04/18/18   MFO  C1194    ONLY RESET EMPLOYEES SHORT CALL LEAD
      *                         TIME WHEN IVR CALL OR MANUAL CALL.
      *03/16/20   MFO  C1212    SET HELDAWAY FLAG BASED ON ON-DUTY ASGN
      *                         INSTEAD OF POOL EMPLOYEE CALLED ON.
      *04/03/23   SXP  CNC0600  WRR PHASE 2 CHANGES
      *04/18/23   RJA  CNLD-193 FIX STORING MSTR3-PREV-7DAY-END
      *04/23/23   RRS  CNLD-133 CALL TO P919 FOR NATURAL RESET
      *05/01/23   DXC  CNLD-207 CHECK (ORD TIME-LEAD) BFR NATURAL RESET
      *05/08/23   RJA  CNLD-214 CHECK PREVIOUS HOS' NATURAL RESETS
      *05/27/23   RJA  CNC0600  CHANGE MARKED WITH 05/27/23
      *06/06/23   RJA  CNLD-244 EMPLOYEES SHOULD BE RESTED FOR ON-DUTY
      *                      #3 TIME (PER EARLIER SPEC THEY NEEDED TO BE
      *                         RESTED FOR ORDER TIME)
      *08/24/23   RJA  CNLD-259 IF RESET STARTS AT 0000, IT SHOULD END
      *                         AT 0759 THE NEXT DAY - DURATION IN THIS
      *                         CASE WILL BE 31 HR 59 MIN INSTEAD OF A
      *                         32 HR MINIMUM.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-9370.
       OBJECT-COMPUTER. IBM-9370.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER                          PIC X(010) VALUE 'P919 W/S'.

       01  WS-SUBSCRIPTS.
           05  SUB1                        PIC 9(002) VALUE ZEROS.
           05  SUB2                        PIC 9(002) VALUE ZEROS.
           05  ARRAY-MAX                   PIC 9(002) VALUE 05.
      *CNC0600-B
           05  I                           PIC 99    VALUE ZERO.
      *CNC0600-E

       01  WS-FLAGS.
           05  WS-DAILY-MARK-CODE          PIC 9(001) VALUE ZEROS.
               88  DAILY-MARK-YARD                    VALUE 1.
           05  WS-DONE-CODE                PIC 9(001) VALUE ZEROS.
               88  DONE                               VALUE 1.
           05  WS-ASGN-DONE-CODE           PIC X(001) VALUE SPACES.
               88  ASGN-DONE                          VALUE 'Y'.
           05  WS-POINTR-DONE-CODE         PIC X(001) VALUE SPACES.
               88  POINTR-DONE                        VALUE 'Y'.
           05  TEMP-ASGN-XB-AUG-FLAG       PIC X(001) VALUE SPACES.
               88  TEMP-ASGN-XB-AUG                   VALUE 'Y'.
           05  ON-DUTY-OUT-TOWN-CODE       PIC X(10) VALUE '9999999999'.
               88  OUT-TOWN                          VALUE '0000000000'.
           05  WS-SEND-EM-DATA             PIC 9(001) VALUE ZEROS.
               88  SEND-EM-DATA                       VALUE 1.
           05  WS-PROCESS-QUAL-FLAG        PIC X(001) VALUE SPACES.
               88  PROCESS-QUAL                       VALUE '1'.
           05  WS-ALLOW-QUICK-TIE-FLAG     PIC X(001) VALUE SPACES.
               88  ALLOW-QUICK-TIEUP                  VALUE 'Y' 'B'.
           05  WS-EMP-ON-REST-DAY-FLAG     PIC X(001) VALUE SPACES.
               88  EMP-NOT-ON-REST-DAY                VALUE SPACE.
               88  EMP-ON-REST-DAY                    VALUE 'Y'.
           05  WS-HOME-AWAY-FLAG           PIC X(001) VALUE SPACES.
               88  CALLED-AT-HOME-TERM                VALUE 'H'.
               88  CALLED-AT-AWAY-TERM                VALUE 'A'.
           05  HOLIDAY-CALL-FLAG           PIC X(001) VALUE 'Y'.
               88  FIRST-HOLIDAY-CALL                 VALUE 'Y'.
               88  NOT-FIRST-HOLIDAY-CALL             VALUE 'N'.
           05  HOLIDAY-CALL-FOUND-FLAG     PIC X(001) VALUE 'N'.
               88  HOLIDAY-CALL-NOT-FOUND             VALUE 'N'.
               88  HOLIDAY-CALL-FOUND                 VALUE 'Y'.
      *CNC0600-B
           05  WS-CAN-WRR-FLAG             PIC X(001) VALUE ' '.
               88  WS-CAN-WRR-NEW                     VALUE 'N'.
           05  WS-MSTR3-FOUND-FLAG         PIC X(01)  VALUE 'N'.
               88  WS-MSTR3-FOUND                     VALUE 'Y'.
               88  WS-MSTR3-NOT-FOUND                 VALUE 'N'.
           05  WS-TOD-DONE-CODE            PIC X(01)  VALUE 'N'.
               88  TOD-DONE                           VALUE 'Y'.
               88  TOD-NOT-DONE                       VALUE 'N'.
           05  WS-READ-DONE-CODE           PIC X(001) VALUE SPACES.
               88  READ-DONE                          VALUE 'Y'.
      *CNC0600-E

       01  WS-MISC.
           05  WS-TIME-1.
               10  WS-HR-1                 PIC 9(002) VALUE ZEROS.
               10  WS-MN-1                 PIC 9(002) VALUE ZEROS.
           05  WS-TIME-2                   PIC 9(004) VALUE ZEROS.
           05  DAY1                        PIC 9(002) VALUE ZEROS.
           05  DAY1-PICX REDEFINES DAY1.
               10  FILLER                  PIC X(001).
               10  DAY1-X                  PIC X(001).
           05  WS-ASGN-COUNT               PIC 9(002) VALUE ZEROS.
           05  WS-EMP-NBR                  PIC X(009) VALUE SPACES.
           05  WS-EMP-NUM REDEFINES
               WS-EMP-NBR                  PIC 9(009).
           05  WS-AVAILABLE                PIC X(001) VALUE 'A'.
           05  WS-FURLOUGHED               PIC X(001) VALUE 'F'.
           05  XB-COMPARE-MILES            PIC X(003) VALUE SPACES.
           05  WS-SAVE-HA-JOB-TYPE         PIC X(002) VALUE SPACES.
           05  WS-SAVE-HA-HELDAWAY         PIC X(045) VALUE SPACES.
           05  SAVE-ASGN-POOL-FLAG         PIC X(001) VALUE SPACES.
           05  WS-CALL-FROM-LO-CODE        PIC X(001).
           05  WS-CALL-FROM-ECC-CODE       PIC X(002).
           05  WS-PROFILE-KEY.
               10  FILLER                  PIC X(004) VALUE SPACES.
               10  WS-ASGN-TYPE            PIC X(001) VALUE SPACES.
                   88  WS-LOCAL-REC                   VALUE 'O'.
               10  FILLER                  PIC X(013) VALUE SPACES.

       01  NORMAL-ASGNMT-FLAG              PIC X(001) VALUE SPACES.
           88  NORM-ASGN-UFP                          VALUE 'U'.
           88  NORM-ASGN-XB                           VALUE 'X'.
           88  NORM-ASGN-AJ                           VALUE 'A'.
       01  NORMAL-ASGNMT                              VALUE SPACES.
           05  NA-DIST                     PIC X(002).
           05  NA-SUB-DIST                 PIC X(002).
           05  NA-AREA.
               10  NA-1                    PIC X(006).
               10  NA-2 REDEFINES NA-1.
                   15  NA-POOL             PIC X(002).
                   15  NA-TURN             PIC X(004).
               10  NA-3 REDEFINES NA-1.
                   15  NA-FILLER           PIC X(002).
                   15  NA-XB-TURN          PIC X(004).
               10  NA-CC                   PIC X(002).
                   88 NORM-ASGN-BRAKEMAN VALUES 'BK', 'B1' THRU 'B2'.

       01  TEMPORARY-ASGNMT-FLAG           PIC X(001) VALUE SPACES.
           88  TEMP-ASGN-UFP                          VALUE 'U'.
           88  TEMP-ASGN-XB                           VALUE 'X'.
           88  TEMP-ASGN-AJ                           VALUE 'A'.
       01  TEMPORARY-ASGNMT                           VALUE SPACES.
           05  TA-DIST                     PIC X(002).
           05  TA-SUB-DIST                 PIC X(002).
           05  TA-AREA.
               10  TA-1                    PIC X(006).
               10  TA-2 REDEFINES TA-1.
                   15  TA-POOL             PIC X(002).
                   15  TA-TURN             PIC X(004).
               10  TA-3 REDEFINES TA-1.
                   15  TA-FILLER           PIC X(002).
                   15  TA-XB-TURN          PIC X(004).
               10  TA-CC                   PIC X(002).

       01  ON-DUTY-ASGNMT-FLAG             PIC X(001) VALUE SPACES.
           88  ON-DUTY-UFP                            VALUE 'U'.
           88  ON-DUTY-AJ                             VALUE 'A'.
       01  ON-DUTY-ASGNMT                             VALUE SPACES.
           05  OD-DIST                     PIC X(002).
           05  OD-SUB-DIST                 PIC X(002).
           05  OD-AREA.
               10  OD-1                    PIC X(006).
               10  OD-2 REDEFINES OD-1.
                   15  OD-POOL             PIC X(002).
                   15  OD-TURN             PIC X(004).
               10  OD-CC                   PIC X(002).

       01  WORK-HIST-TIMEX.
           05  WORK-HIST-TIME              PIC 9(012) VALUE ZEROS.
           05  WORK-HIST-TIME-TIE          PIC 9(002) VALUE ZEROS.

       01  WS-EMP-EFF-DATE-TIME                       VALUE SPACES.
           05  WS-EMP-EFF-DATE             PIC X(006).
           05  WS-EMP-EFF-TIME             PIC X(004).

       01  WS-VAC-EFF-DATE-TIME                       VALUE SPACES.
           05  WS-VAC-EFF-DATE             PIC X(006).
           05  WS-VAC-EFF-TIME             PIC X(004).

       01  WS-NEW-ON-DUTY-DATE-TIME.
           05  WS-NEW-ON-DUTY-DATE         PIC X(006).
           05  WS-NEW-ON-DUTY-TIME         PIC X(004).
       01  WS-PREV-ON-DUTY-DATE-TIME.
           05  WS-PREV-ON-DUTY-DATE        PIC X(006).
           05  WS-PREV-ON-DUTY-TIME        PIC X(004).
       01  WS-PREV-OFF-DUTY-DATE-TIME.
           05  WS-PREV-OFF-DUTY-DATE       PIC X(006).
           05  WS-PREV-OFF-DUTY-TIME       PIC X(004).

       01  WS-EMP-TIME-ZONE                PIC X(001) VALUE SPACES.
       01  WS-ASGN-TIME-ZONE               PIC X(001) VALUE SPACES.
       01  WS-CALL-TIME-ZONE               PIC X(001) VALUE SPACES.
       01  WS-PREV-ASGN-TIME-ZONE          PIC X(001) VALUE SPACES.
       01  WS-TRAIN-DATE-TIME              PIC X(010) VALUE SPACES.
      *CNLD-207-DXC-B
       01  WS-ORDER-DATE-TIME.
           05  WS-ORDER-DATE               PIC X(006) VALUE SPACES.
           05  WS-ORDER-TIME               PIC X(004) VALUE SPACES.
      *CNLD-207-DXC-E
       01  WS-AHIST-DATE-TIME              PIC X(010) VALUE SPACES.

       01  HOLD-TRAIN-DATE-TIME            PIC X(010) VALUE SPACES.

       01  WS-HOLIDAY-DATE-TIME.
           05  WS-HOLIDAY-DATE.
               10  WS-HOLIDAY-YR               PIC X(002).
               10  WS-HOLIDAY-MO               PIC X(002).
               10  WS-HOLIDAY-DY               PIC X(002).
           05  WS-HOLIDAY-TIME                 PIC X(004).
       01  WS-HOLIDAY-FROM-CE-DATE-TIME.
           05  WS-HOLIDAY-FROM-CE              PIC X(002).
           05  WS-HOLIDAY-FROM-DATE-TIME.
               10  WS-HOLIDAY-FROM-DATE        PIC X(006).
               10  WS-HOLIDAY-FROM-TIME        PIC X(004).
       01  WS-HOLIDAY-TO-CE-DATE-TIME.
           05  WS-HOLIDAY-TO-CE                PIC X(002).
           05  WS-HOLIDAY-TO-DATE-TIME.
               10  WS-HOLIDAY-TO-DATE          PIC X(006).
               10  WS-HOLIDAY-TO-TIME          PIC X(004).

       01  WORK-CNTLKEY                                   VALUE SPACES.
           05  WK-CNTL-REC-TYPE                PIC X(002).
           05  WK-CNTL-DIST                    PIC X(002).
           05  WK-CNTL-SUB-DIST                PIC X(002).
           05  WK-CNTL-ASSIGN.
               10  WK-CNTL-AJ-JOB-NO.
                   15  WK-CNTL-AJ-1-4          PIC X(004).
                   15  WK-CNTL-AJ-5-6          PIC X(002).
               10  FILLER                      PIC X(003).
           05  WK-CNTL-XB REDEFINES
               WK-CNTL-ASSIGN.
               10  WK-CNTL-XB-CODE             PIC X(002).
               10  FILLER                      PIC X(007).
           05  WK-CNTL-UFP REDEFINES
               WK-CNTL-ASSIGN.
               10  WK-CNTL-POOL                PIC X(002).
               10  WK-CNTL-POOL-TYPE           PIC X(001).
               10  FILLER                      PIC X(006).
           05  WK-CNTL-ASSIGN-QUAL
               REDEFINES WK-CNTL-ASSIGN.
               10  WK-CNTL-ASGN-QUAL-TYPE      PIC X(001).
               10  WK-CNTL-ASGN-QUAL.
                   15  WK-CNTL-ASGN-QUAL-1-2   PIC X(002).
                   15  WK-CNTL-ASGN-QUAL-3-4   PIC X(002).
                   15  WK-CNTL-ASGN-QUAL-5-6   PIC X(002).
                   15  WK-CNTL-ASGN-QUAL-7-8   PIC X(002).
           05  FILLER                          PIC X(005).

       01  WORK-ASGNKEY1.
           05  WK-ASGN-JOB-TYPE            PIC X(001) VALUE SPACES.
           05  WK-ASGN-ASSIGNMENT                     VALUE SPACES.
               10  WK-ASGN-DIST            PIC X(002).
               10  WK-ASGN-SUB-DIST        PIC X(002).
               10  WK-ASGN-ASSIGN.
                   15  WK-ASGN-POOL        PIC X(002).
                   15  WK-ASGN-TURN        PIC X(004).
                   15  WK-ASGN-CC          PIC X(002).
           05  WK-ASGN-REC-TYPE            PIC X(001) VALUE '1'.
           05  WK-ASGN-DATE-TIME           PIC 9(010) VALUE ZEROS.

       01  WORK-ASGNKEY2.
           05  WK-ASGN-EMP-NO              PIC 9(009) VALUE ZEROS.
           05  WK-ASGN-EMP-REC-TYPE        PIC X(001) VALUE SPACES.
           05  WK-ASGN-EMP-DATE-TIME       PIC 9(010) VALUE ZEROS.

       01  WORK-QUALEMP-KEY                           VALUE SPACES.
           05  WK-QUAL-EMP-NO              PIC X(009).
           05  WK-QUALIFICATION            PIC X(004).

       01  WS-SAVE-ASGN-FILE               PIC X(128) VALUE SPACES.
       01  WS-HOLD-ASGN-FILE               PIC X(128) VALUE SPACES.

       01  WORK-XB-TURN                               VALUE SPACES.
           05  WORK-XB-DIST                PIC X(002).
           05  WORK-XB-SUB-DIST            PIC X(002).
           05  WORK-XB-CRAFT-CODE          PIC X(002).
           05  WORK-XB-TURN-NBR            PIC X(004).

       01  WS-LIST-IDENTIFIERS.
           05  WS-LIST-ID                  PIC X(002) VALUE SPACE.
               88 SYSTEM-LIST-ID                      VALUE '#L'.
           05  WS-SCHEDULE-LIST-ID         PIC X(002) VALUE '#S'.
      *SRP, 4/18/1996, START
       01  WS-SAVE-PAY-VAC-AND-WORK-SW     PIC  X     VALUE 'N'.
           88  WS-SAVE-PAY-VAC-AND-WORK               VALUE 'Y'.
           88  WS-SAVE-CREDIT-UNUSED-VAC              VALUE 'N'.
       01  WS-VAC-DONE-CODE                PIC  9(01) VALUE ZEROS.
           88  VAC-DONE                               VALUE 1.
       01  VAC-SUB1                        PIC  9(02) VALUE ZEROS.
       01  WS-TS-NUMBER                     PIC X(09).
       01  WS-TS-NUM REDEFINES WS-TS-NUMBER PIC 9(09).
       01  WS-UNMARK-VACATION-CODE         PIC  X(01) VALUE 'N'.
           88  UNMARK-VACATION                        VALUE 'Y'.
      *SRP, 4/18/1996, END
      *CNC0600-B
      *RJA  5/17/2023 B CNC0600
       01  WS-WRR-START-DTTM.
           05  WS-WRR-START-DATE        PIC X(06)  VALUE SPACES.
           05  WS-WRR-START-TIME        PIC X(04)  VALUE SPACES.
       01  WS-8DAYS-AGO-DTTM.
           05  WS-8DAYS-AGO-DATE        PIC X(06)  VALUE SPACES.
           05  WS-8DAYS-AGO-TIME        PIC X(04)  VALUE SPACES.
      *RJA  5/17/2023 E
      * CNLD-193 - B
       01  WS-WRR-PREV-7DAY-END.
           05  WS-WRR-PREV-7DAY-DATE    PIC X(06)  VALUE SPACES.
           05  WS-WRR-PREV-7DAY-TIME    PIC X(04)  VALUE SPACES.
      * CNLD-193 - E
       01  WS-WRR-7DAY-END.
           05  WS-WRR-7DAY-END-DATE     PIC X(06)  VALUE SPACES.
           05  WS-WRR-7DAY-END-TIME     PIC X(04)  VALUE SPACES.
       01  WS-7DAY-START-DTTM.
           05  WS-7DAY-START-DATE       PIC X(06)  VALUE SPACES.
           05  WS-7DAY-START-TIME       PIC X(04)  VALUE SPACES.
       01  WS-DAY1-DATE-TIME.
           05  WS-DAY1-DATE             PIC X(06)  VALUE SPACES.
           05  WS-DAY1-TIME             PIC X(04)  VALUE SPACES.
       01  WS-DAY2-DATE-TIME.
           05  WS-DAY2-DATE             PIC X(06)  VALUE SPACES.
           05  WS-DAY2-TIME             PIC X(04)  VALUE SPACES.
       01  WS-DAY3-DATE-TIME.
           05  WS-DAY3-DATE             PIC X(06)  VALUE SPACES.
           05  WS-DAY3-TIME             PIC X(04)  VALUE SPACES.
       01  WS-UPD-PROJ-DATE-TIME.
           05  WS-UPD-PROJ-DATE         PIC X(06)  VALUE SPACES.
           05  WS-UPD-PROJ-TIME         PIC X(04)  VALUE SPACES.
       01  WS-UPD-PROJ-UD1-FROM         PIC X(10)  VALUE SPACES.
       01  WS-UPD-PROJ-UD1-TO           PIC X(10)  VALUE SPACES.
       01  WS-UPD-PROJ-UD2-FROM         PIC X(10)  VALUE SPACES.
       01  WS-UPD-PROJ-UD2-TO           PIC X(10)  VALUE SPACES.
      *CNC0600-E
      * CNLD-214 B CNC0600
       01  WS-PREV-7DAY-START-DTTM.
           05  WS-PREV-7DAY-START-DT    PIC X(06)  VALUE SPACES.
           05  WS-PREV-7DAY-START-TM    PIC X(04)  VALUE SPACES.
       01  LAST-EMPTOD-OFFD-YYMMDD-HRMN PIC X(10)  VALUE SPACES.
       01  NATURAL-RESET-AT-CALL-FL          PIC X(01)  VALUE SPACES.
           88  NATURAL-RESET-AT-CALL                    VALUE 'A'.
       01  NATURAL-RESET-BEFORE-CALL-FL      PIC X(01)  VALUE SPACES.
           88  NATURAL-RESET-BEFORE-CALL                VALUE 'B'.
       01  ENDED-BROWSE-FL              PIC X(01)  VALUE SPACES.
           88  ENDED-BROWSE-AT-THIS-REC            VALUE 'Y'.
      * CNLD-214 E
      *CNLD-244-B #3 CNC0600
      **CNLD-207-DXC-B
      * 01  WS-ORDER-MINUS-LEAD-DTTM.
      *     05  WS-ORDER-MINUS-LEAD-DT   PIC X(06)  VALUE SPACES.
      *     05  WS-ORDER-MINUS-LEAD-TM   PIC X(04)  VALUE SPACES.
      **CNLD-207-DXC-E
       01  WS-OD-MINUS-LEAD-DTTM.
           05  WS-OD-MINUS-LEAD-DT      PIC X(06)  VALUE SPACES.
           05  WS-OD-MINUS-LEAD-TM      PIC X(04)  VALUE SPACES.
      *CNLD-244-E
      *****************************************************************
      ***                 I/O STATUS CHECK FIELDS
      *****************************************************************
       01  WS-RESPONSE                     PIC S9(008) COMP VALUE ZEROS.
       01  FILE-STATUS                     PIC  9(004) VALUE ZEROS.
           COPY IOCODES.
      *****************************************************************
      ***                   COMMAREA COPYBOOKS
      *****************************************************************
           COPY P919COMM.
      *****************************************************************
      ***                  CALLED ROUTINES COPYBOOKS.
      *****************************************************************
           COPY PSTERAR.
           COPY P903COMM.
           COPY P913COMM.
           COPY P915COMM.
           COPY P918COMM.
           COPY P922COMM.
           COPY P925COMM.
           COPY P926COMM.
           COPY P927COMM.
           COPY P929COMM.
           COPY P938COMM.
           COPY P943COMM.
           COPY P963COMM.
           COPY P977COMM.
      *CNC0600-B
           COPY PS08COMM.
      *CNC0600-E
      *****************************************************************
      ***                       FILE COPYBOOKS
      *****************************************************************
           COPY WSCARR.
           COPY WSCNTL.
           COPY WSMSTR.
      *CNC0600-B
           COPY WSMSTR3.
           COPY WSEMPTOD.
      *CNC0600-E
           COPY WSTELE.
           COPY WSAHIST.
           COPY WSASGN.
           COPY WSDEC.
           COPY WSQUAL.
           COPY WSEB.
           COPY WSTRCN.
           COPY WSUFP.
           COPY WSAJ.
           COPY WSSWASGN.
           COPY WSAL.
           COPY WSJS.
           COPY WSTMSLIP.
           COPY WSPOINTR.
      *****************************************************************
      ***                       MISC COPYBOOKS
      *****************************************************************
           COPY PSTCCRFT.
           COPY WSEDDATE.
           COPY WSEDTIME.
           COPY WSZONE.
           COPY WSOFFSET.
           COPY WSAJDEFN.
           COPY WSSYDTTM.
           COPY WSLOCODE.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05  FILLER            PIC X(350).

       PROCEDURE DIVISION.
      *
       P0000-MAINLINE.
      *
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC
           EXEC CICS HANDLE ABEND
                     LABEL(P9999-GOT-PROBLEM)
           END-EXEC
           COPY ABSTIME.
           IF EIBCALEN = ZERO
              PERFORM P9999-GOT-PROBLEM
           END-IF
           EXEC CICS ASKTIME
                     ABSTIME(WS-ABSTIME)
           END-EXEC
           ADD WS-ABSTIME-OFFSET  TO WS-ABSTIME
           EXEC CICS FORMATTIME
                     ABSTIME(WS-ABSTIME)
                     YYYYMMDD(WS-SYSTEM-DATE-CENT)
                     TIME(WS-SYSTEM-TIME-AREA)
           END-EXEC
      *
      *    INSTALL APPLICATION DATE/TIME
      *
           PERFORM P9800-GET-TIME-OFFSET
           IF WS-DATE-TIME-OFFSET > SPACES
              MOVE ZEROS          TO DATE-CONVERSION-PARMS
              MOVE WS-SYSTEM-DATE TO PARM-PRI-DATE-GREG
              MOVE WS-SYSTEM-TIME TO PARM-PRI-HRMN
              PERFORM P9810-PROCESS-OFFSET
              MOVE PARM-RES-GREG-CENT
                                  TO WS-SYSTEM-CENT
              MOVE PARM-RES-DATE-GREG
                                  TO WS-SYSTEM-DATE
              MOVE PARM-RES-HRMN
                                  TO WS-SYSTEM-TIME
           END-IF
      *
           MOVE WS-SYSTEM-DATE-TIME TO WORK-HIST-TIME
           MOVE ZEROS TO WORK-HIST-TIME-TIE
           MOVE DFHCOMMAREA TO P919-COMMAREA-PARMS
           PERFORM P1000-PROCESS.
           PERFORM P9000-RETURN.
      *
       P1000-PROCESS.
      *
           IF NOT P919-HAVE-ON-DUTY-EMP
              MOVE 'P1000-1'              TO ERR-PARAGRAPH
              MOVE P919-ASGN-ON-DUTY-EMP  TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE P919-ASGN-ON-DUTY-EMP TO MSTRNBRK
           PERFORM P8500-READ-MASTER

           PERFORM P9820-SNAPSHOT-XB
           PERFORM P9830-SNAPSHOT-UFP
      *
      *    READ THE 'SUB DISTRICT' CONTROL RECORD
      *
           MOVE SPACES              TO WORK-CNTLKEY
           MOVE '02'                TO WK-CNTL-REC-TYPE
           MOVE DIST OF WS-MSTR     TO WK-CNTL-DIST
           MOVE SUB-DIST OF WS-MSTR TO WK-CNTL-SUB-DIST
           PERFORM P8640-READ-CNTLFILE
           IF NOT SUCCESS
              MOVE 'P1000-2'        TO ERR-PARAGRAPH
              MOVE CNTLKEY          TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE CNTL-TIME-ZONE       TO WS-EMP-TIME-ZONE
      *CNC0600-B
           MOVE CNTL-CAN-WRR-FLAG    TO WS-CAN-WRR-FLAG
      *CNC0600-E
      *RJA  5/17/2023 B CNC0600
           MOVE CNTL-02-WRR-BEGINNING-DTTM TO WS-WRR-START-DTTM
      *RJA  5/17/2023 E

      *    RJA, 5/19/2023  B
           IF NOT P919-DH-FUNCTION
      *    RJA, 5/19/2023  E

      *    SRP, 4/18/1996, START
              IF  PAY-VAC-AND-WORK
                  MOVE 'Y'              TO WS-SAVE-PAY-VAC-AND-WORK-SW
              ELSE
                  MOVE 'N'              TO WS-SAVE-PAY-VAC-AND-WORK-SW
              END-IF
      *    SRP, 4/18/1996, END

      *
      *    READ THE 'SUB DISTRICT' CONTROL RECORD FOR ASSIGNMENT
      *
              MOVE SPACES             TO WORK-CNTLKEY
              MOVE '02'               TO WK-CNTL-REC-TYPE
              MOVE P919-ASGN-DIST     TO WK-CNTL-DIST
              MOVE P919-ASGN-SUB-DIST TO WK-CNTL-SUB-DIST
              PERFORM P8640-READ-CNTLFILE
              IF NOT SUCCESS
                 MOVE 'P1000-3' TO ERR-PARAGRAPH
                 MOVE CNTLKEY TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE CNTL-TIME-ZONE     TO WS-ASGN-TIME-ZONE
              IF DAILY-MARKUP-ALLOWED
                 SET DAILY-MARK-YARD TO TRUE
              END-IF

              IF WORKING
                 IF ON-DUTY-ASGNMT > SPACES AND
                    ON-DUTY-AJ AND
                    EMP-TRHIST-KEY > SPACE
                    PERFORM P1050-QUICK-TIE
                 ELSE
                    MOVE 'P1000-4'   TO ERR-PARAGRAPH
                    MOVE LAYOFF-CODE TO ERR-KEY
                    PERFORM P9999-GOT-PROBLEM
                 END-IF
              END-IF
      *    RJA, 5/19/2023  B
           END-IF
      *    RJA, 5/19/2023  E

      *
      *    CONVERT THE EFFECTIVE DATE/TIME TO THE EMPLOYEES TIME ZONE
      *
           MOVE SPACES               TO TZ-PARAMETERS
           MOVE P919-TIME-ZONE       TO TZ-IN-ZONE
           MOVE P919-EFF-DATE-TIME   TO TZ-IN-DATE-TIME
           MOVE WS-EMP-TIME-ZONE     TO TZ-OUT-ZONE
           PERFORM P8996-TIMEZONE
           IF TZ-INVALID-PARAMETERS
              MOVE 'P1000TZ'         TO ERR-PARAGRAPH
              PERFORM P8996-TZERROR
           END-IF
           MOVE TZ-OUT-DATE-TIME     TO WS-EMP-EFF-DATE-TIME
      *CNLD-207-DXC-B
      *
      *    CONVERT THE TRAIN/ORDER DATE/TIME TO THE EMPLOYEES TIME ZONE
      *
           MOVE SPACES               TO TZ-PARAMETERS
           MOVE P919-TIME-ZONE       TO TZ-IN-ZONE
           MOVE P919-TRAIN-DATE-TIME TO TZ-IN-DATE-TIME
           MOVE WS-EMP-TIME-ZONE     TO TZ-OUT-ZONE
           PERFORM P8996-TIMEZONE
           IF TZ-INVALID-PARAMETERS
              MOVE 'P1001TZ'         TO ERR-PARAGRAPH
              PERFORM P8996-TZERROR
           END-IF
           MOVE TZ-OUT-DATE-TIME     TO WS-ORDER-DATE-TIME
      *
      *CNLD-207-DXC-E
      *

      *    RJA, 5/19/2023  B
           IF NOT P919-DH-FUNCTION
      *    RJA, 5/19/2023  E
              IF P919-ASGN-PARM > SPACES
                 SET NO-RECORD-FND      TO TRUE
                 IF P919-ASGN-UFP
                    MOVE P919-ASGN-PARM TO UFPTURN
                    PERFORM P8600-READ-UFPTURN
                 END-IF
                 IF P919-ASGN-AJ
                    MOVE P919-ASGN-PARM TO AJJOBKEY
                    PERFORM P8600-READ-AJJOBKEY
                 END-IF
                 IF NOT SUCCESS
                    MOVE 'P1000-5' TO ERR-PARAGRAPH
                    MOVE P919-ASGN-PARM TO ERR-PARAGRAPH
                    PERFORM P9999-GOT-PROBLEM
                 END-IF
              ELSE
                 MOVE 'P1000-6'   TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE SPACES             TO TZ-PARAMETERS
              MOVE P919-TIME-ZONE     TO TZ-IN-ZONE
              MOVE P919-TRAIN-DATE-TIME TO TZ-IN-DATE-TIME
              MOVE WS-ASGN-TIME-ZONE    TO TZ-OUT-ZONE
              PERFORM P8996-TIMEZONE
              IF TZ-INVALID-PARAMETERS
                 MOVE 'P1002TZ' TO ERR-PARAGRAPH
                 PERFORM P8996-TZERROR
              END-IF
              MOVE TZ-OUT-DATE-TIME     TO WS-TRAIN-DATE-TIME

      *
      *    READ THE 'SUB DISTRICT' CONTROL REC CONVERT TO P919-CALL-TZ
      *
              MOVE SPACES             TO WORK-CNTLKEY
              MOVE '02'               TO WK-CNTL-REC-TYPE
              MOVE P919-CALL-DIST     TO WK-CNTL-DIST
              MOVE P919-CALL-SUB-DIST TO WK-CNTL-SUB-DIST
              PERFORM P8640-READ-CNTLFILE
              IF NOT SUCCESS
                 MOVE 'P1003TZ' TO ERR-PARAGRAPH
                 MOVE CNTLKEY TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE CNTL-TIME-ZONE     TO WS-CALL-TIME-ZONE
              IF WS-CALL-TIME-ZONE NOT = P919-TIME-ZONE
                 MOVE SPACES             TO TZ-PARAMETERS
                 MOVE P919-TIME-ZONE     TO TZ-IN-ZONE
                 MOVE P919-TRAIN-DATE-TIME TO TZ-IN-DATE-TIME
                 MOVE WS-CALL-TIME-ZONE    TO TZ-OUT-ZONE
                 PERFORM P8996-TIMEZONE
                 IF TZ-INVALID-PARAMETERS
                    MOVE 'P1004TZ' TO ERR-PARAGRAPH
                    PERFORM P8996-TZERROR
                 END-IF
                 MOVE TZ-OUT-DATE-TIME     TO WS-AHIST-DATE-TIME
              ELSE
                 MOVE P919-TRAIN-DATE-TIME TO WS-AHIST-DATE-TIME
              END-IF
      *
      *    CONVERT THE JOB CALL DATE TO A DAY OF THE WEEK
      *
              MOVE ZEROS TO DATE-CONVERSION-PARMS
              SET PARM-CONV  TO TRUE
              MOVE P919-EFF-DATE TO PARM-PRI-DATE-GREG
              EXEC CICS LINK
                        PROGRAM(P903-PGM)
                        COMMAREA(DATE-CONVERSION-PARMS)
                        LENGTH(P903-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1000-7' TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE PARM-PRI-DAY-OF-WEEK TO DAY1
      *    RJA, 5/19/2023  B
           END-IF
      *    RJA, 5/19/2023  E
      *
      *    PROCESS ACCORDING TO FUNCTION
      *
           EVALUATE TRUE
              WHEN P919-CALL-FUNCTION
                 PERFORM P1100-CALL-FUNCTION
      *CNLD-133-B
              WHEN P919-DH-FUNCTION
                 PERFORM P1200-DH-CALL-FUNCTION
      *CNLD-133-E
              WHEN OTHER
                 MOVE 'P1000-8' TO ERR-PARAGRAPH
                 MOVE 'BADFUNC' TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
           END-EVALUATE.
      *
       P1050-QUICK-TIE.
           MOVE ASGN-ON-DUTY-DATE-TIME     TO WS-PREV-ON-DUTY-DATE-TIME
           MOVE SPACES                     TO WORK-CNTLKEY
           MOVE '2A'                       TO WK-CNTL-REC-TYPE
           MOVE OD-DIST                    TO WK-CNTL-DIST
           MOVE OD-SUB-DIST                TO WK-CNTL-SUB-DIST
           PERFORM P8640-READ-CNTLFILE
           IF NOT SUCCESS
              MOVE 'P1050-1'               TO ERR-PARAGRAPH
              MOVE CNTLKEY                 TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE CNTL-RESTRICT-TIE-FLAG     TO WS-ALLOW-QUICK-TIE-FLAG

           IF ALLOW-QUICK-TIEUP
              MOVE EMP-TRHIST-KEY          TO AH1KEY
              MOVE AH-FILE-VIA-K1          TO AH-DATASET
              PERFORM P8200-READ-AHIST
              IF NOT SUCCESS
                 MOVE 'P1050-2'            TO ERR-PARAGRAPH
                 MOVE AH1KEY               TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
      *
      *    READ THE 'SUB DISTRICT' CONTROL RECORD FOR PREV ASSIGNMENT
      *
              MOVE SPACES                  TO WORK-CNTLKEY
              MOVE '02'                    TO WK-CNTL-REC-TYPE
              MOVE OD-DIST                 TO WK-CNTL-DIST
              MOVE OD-SUB-DIST             TO WK-CNTL-SUB-DIST
              PERFORM P8640-READ-CNTLFILE
              IF NOT SUCCESS
                 MOVE 'P1050-3'            TO ERR-PARAGRAPH
                 MOVE CNTLKEY              TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE CNTL-TIME-ZONE          TO WS-PREV-ASGN-TIME-ZONE

              MOVE ZEROS                   TO DATE-CONVERSION-PARMS
              SET PARM-ADD                 TO TRUE
              MOVE WS-PREV-ON-DUTY-DATE    TO PARM-PRI-DATE-GREG
              MOVE WS-PREV-ON-DUTY-TIME    TO PARM-PRI-HRMN
              MOVE 0800                    TO PARM-SEC-HRMN
              EXEC CICS LINK
                        PROGRAM(P903-PGM)
                        COMMAREA(DATE-CONVERSION-PARMS)
                        LENGTH(P903-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE             TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1050-4'            TO ERR-PARAGRAPH
                 MOVE 'P903LINK'           TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE PARM-RES-DATE-GREG      TO WS-PREV-OFF-DUTY-DATE
              MOVE PARM-RES-HRMN           TO WS-PREV-OFF-DUTY-TIME
              SET DE-YYMMDD-FORMAT         TO TRUE

              IF P919-TIME-ZONE NOT = WS-PREV-ASGN-TIME-ZONE
                 MOVE SPACES                 TO TZ-PARAMETERS
                 MOVE P919-TIME-ZONE         TO TZ-IN-ZONE
                 MOVE P919-EFF-DATE-TIME     TO TZ-IN-DATE-TIME
                 MOVE WS-PREV-ASGN-TIME-ZONE TO TZ-OUT-ZONE
                 PERFORM P8996-TIMEZONE
                 IF TZ-INVALID-PARAMETERS
                    MOVE 'P1050TZ'         TO ERR-PARAGRAPH
                    PERFORM P8996-TZERROR
                 END-IF
                 MOVE TZ-OUT-DATE-TIME     TO WS-NEW-ON-DUTY-DATE-TIME
              ELSE
                 MOVE P919-EFF-DATE-TIME   TO WS-NEW-ON-DUTY-DATE-TIME
              END-IF

              SET DE-YYMMDD-FORMAT         TO TRUE
              MOVE WS-NEW-ON-DUTY-DATE     TO DE-YYMMDD
              PERFORM P8998-DATEEDIT
              MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE
              MOVE WS-NEW-ON-DUTY-TIME     TO DE-COMPARE1-TIME

              SET DE-YYMMDD-FORMAT         TO TRUE
              MOVE WS-PREV-OFF-DUTY-DATE   TO DE-YYMMDD
              PERFORM P8998-DATEEDIT
              MOVE DE-CCYYMMDD             TO DE-COMPARE2-DATE
              MOVE WS-PREV-OFF-DUTY-TIME   TO DE-COMPARE2-TIME
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*
      * COMPARE1 HAS CALL TIME OF NEW ASSIGNMENT                    *
      * COMPARE2 HAS OFFD TIME OF PREV ASSIGNMENT CURRENTLY WORKING *
      *:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*
              MOVE SPACES                   TO P922-COMMAREA-PARMS
              SET P922-QUICK-TIE-UP         TO TRUE
              MOVE OD-DIST                  TO P922-AJ-DIST
              MOVE OD-SUB-DIST              TO P922-AJ-SUB-DIST
              MOVE OD-AREA                  TO P922-AJ-ASSIGN
              MOVE OD-CC                    TO P922-AT-CC
              MOVE WS-PREV-ON-DUTY-DATE     TO P922-JOB-OD-DATE
              MOVE WS-PREV-ON-DUTY-TIME     TO P922-JOB-OD-TIME
              MOVE WS-PREV-ASGN-TIME-ZONE   TO P922-TIME-ZONE
              IF DE-COMPARE1-DATE-TIME > DE-COMPARE2-DATE-TIME
                 MOVE DE-COMPARE2-DATE-TIME(3:10)
                                           TO P922-OFFD-DATE-TIME
                                              P922-ARR-DATE-TIME
              ELSE
                 MOVE DE-COMPARE1-DATE-TIME(3:10)
                                           TO P922-OFFD-DATE-TIME
                                              P922-ARR-DATE-TIME
              END-IF
              MOVE AH-MISC-JOB-TYPE        TO P922-JOB-TYPE
              MOVE AH-MISC-WRKG-UNITS      TO P922-WORKING-UNITS
              MOVE AH-CALL-XREF-ID         TO P922-CALL-XREF-ID
              MOVE AH-MISC-ROUTE-DESIGNATION
                                           TO P922-ROUTE-DESIGNATION
              MOVE EMP-NBR OF WS-MSTR      TO P922-TU-EMP-NBR
              EXEC CICS LINK
                        PROGRAM(P922-PGM)
                        COMMAREA(P922-COMMAREA-PARMS)
                        LENGTH(P922-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE             TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1050-5'            TO ERR-PARAGRAPH
                 MOVE 'P922LINK'           TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
      *::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*
      * AFTER RETURNING WE NEED TO REREAD THE MASTER FILE & MSTR JOBS TO
      * RESET THE EMPLOYEE STATS FOR THE NEW CALL
      *::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*
              MOVE P919-ASGN-ON-DUTY-EMP   TO MSTRNBRK
              PERFORM P8500-READ-MASTER
           ELSE
              MOVE 'P1050-6'               TO ERR-PARAGRAPH
              MOVE LAYOFF-CODE             TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.

       P1100-CALL-FUNCTION.
      *
      *CNC0600-B
           IF P919-ASGN-UFP
              IF  DIST      OF WS-UFP = DIST2
              AND SUB-DIST  OF WS-UFP = SUB-DIST2
              AND POOL-NAME OF WS-UFP = POOL-NAME2
              AND IN-TOWN
                 SET CALLED-AT-HOME-TERM  TO TRUE
              ELSE
                 SET CALLED-AT-AWAY-TERM  TO TRUE
              END-IF
           ELSE
              SET CALLED-AT-HOME-TERM     TO TRUE
           END-IF

      * USING CALLED-AT-HOME-TERM CHECK WITHIN P1130
      *    IF WS-CAN-WRR-NEW AND CALLED-AT-HOME-TERM
           IF WS-CAN-WRR-NEW
      *
              PERFORM P1130-CHECK-NATURAL-RESET-BRK
           END-IF
      *CNC0600-E
      *    DETERMINE HOW MUCH U.S. PREVIOUS DUTY THE EMPLOYEE MAY HAVE
      *    DETERMINE HOW MUCH PENALTY-TIME THE EMPLOYEE MAY HAVE
      *
           MOVE SPACES                    TO P925-COMMAREA-PARMS
           SET P925-CALC-US-CONT-DUTY     TO TRUE
           IF P919-ASGN-UFP
              IF  DIST      OF WS-UFP = DIST2
              AND SUB-DIST  OF WS-UFP = SUB-DIST2
              AND POOL-NAME OF WS-UFP = POOL-NAME2
              AND IN-TOWN
                 SET CALLED-AT-HOME-TERM      TO TRUE
              ELSE
      *------------------
                 SET CALLED-AT-AWAY-TERM      TO TRUE
                 MOVE SPACES                  TO WS-CARRIED-TURN
                 SET  CARRIED-DCAN-PLACEMENT  TO TRUE
                 MOVE UFPTURN-AREA            TO CARRIED-BY-TURN-AREA
                 MOVE CARRIED-BY-TURN-KEY     TO CARRTURNBY
                 EXEC CICS READ
                           DATASET(CARRTURN-BY-DATASET)
                           INTO(WS-CARRIED-TURN)
                           LENGTH(CARRTURN-BY-RLGTH)
                           RIDFLD(CARRTURNBY)
                           KEYLENGTH(CARRTURN-BY-KLGTH)
                           RESP(WS-RESPONSE)
                 END-EXEC
                 MOVE WS-RESPONSE             TO FILE-STATUS
                 IF SUCCESS
      *
      *             AND "CALLING" FROM THE DCAN'D TO BOARD
      *
                 AND DIST      OF WS-UFP = CARRIED-DCAN-TO-DIST
                 AND SUB-DIST  OF WS-UFP = CARRIED-DCAN-TO-SUB-DIST
                 AND POOL-NAME OF WS-UFP = CARRIED-DCAN-TO-POOL
                 AND IN-OUT-TERMINAL     = CARRIED-DCAN-TO-TERM
      *
      *             AND DCAN'D FROM THE HOME BOARD
      *
                 AND DIST2               = CARRIED-DCAN-FROM-DIST
                 AND SUB-DIST2           = CARRIED-DCAN-FROM-SUB-DIST
                 AND POOL-NAME2          = CARRIED-DCAN-FROM-POOL
                 AND CARRIED-DCAN-FROM-TERM = '0'
                    CONTINUE
                 ELSE
                    SET  P925-CALC-PENALTY    TO TRUE
                    PERFORM P8000-READ-TRCN
                    IF SUCCESS
                       MOVE TRCN-ORIGIN-STN   TO P925-STATION
                    END-IF
                 END-IF
      *------------------
              END-IF
           ELSE
              SET CALLED-AT-HOME-TERM     TO TRUE
           END-IF
           MOVE EMP-NBR OF WS-MSTR        TO P925-EMP-NO
           MOVE P919-TIME-ZONE            TO P925-TIME-ZONE
           MOVE P919-EFF-DATE-TIME        TO P925-OD-DATE-TIME
           EXEC CICS LINK
                     PROGRAM(P925-PGM)
                     COMMAREA(P925-COMMAREA-PARMS)
                     LENGTH(P925-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1100-1' TO ERR-PARAGRAPH
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE SPACES TO WORK-DEC-TABLE
           IF P919-DECKEY > SPACES
              MOVE P919-DECKEY TO DECKEY
              EXEC CICS READ
                        DATASET(DEC-TABLE-VIA-DECKEY)
                        INTO(WORK-DEC-TABLE)
                        LENGTH(DECTABLE-RLGTH)
                        RIDFLD(DECKEY)
                        KEYLENGTH(DECTABLE-KLGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1100-2' TO ERR-PARAGRAPH
                 MOVE DECKEY    TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF
           MOVE P919-ASGN-ON-DUTY-EMP TO MSTRNBRK
           PERFORM P8520-READ-MASTER-UPDATE
           IF NOT SUCCESS
              MOVE 'P1100-3' TO ERR-PARAGRAPH
              MOVE MSTRNBRK  TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE LAST-ON-DUTY-TIME    TO SAVE-PREV-ODT
           MOVE EMP-PREV-DUTY        TO SAVE-PREV-DUTY
           MOVE WS-EMP-EFF-DATE-TIME TO LAST-ON-DUTY-TIME
           MOVE P925-US-CONT-DUTY    TO EMP-PREV-DUTY
           MOVE SPACES               TO EMP-EMERGENCY-COND-CODE
           MOVE SPACES               TO WS-PROCESS-QUAL-FLAG
           MOVE LAYOFF-CODE-1 OF WS-MSTR
                                     TO WS-CALL-FROM-LO-CODE
           MOVE LAYOFF-EM-CODE OF WS-MSTR
                                     TO WS-CALL-FROM-ECC-CODE
           IF LAYOFF-CODE-1 OF WS-MSTR > 'B'
              MOVE LAYOFF-CODE-1  OF WS-MSTR  TO UNAVAILABLE-LAST-FLAG
              MOVE LAYOFF-EM-CODE OF WS-MSTR  TO MSTR-LAST-STATUS-REASON
              IF LAYOFF-CODE-1 OF WS-MSTR = 'X'
                AND P919-DECKEY > SPACES
                 MOVE SPACES                 TO UNAVAILABLE-LAST-FLAG
                                                MSTR-LAST-STATUS-REASON
              ELSE
      *          SRP, 4/18/1996, START
      *          IF CALLED FROM VACATION, AND PAY-VAC-AND-WORK
      *          THEN THE EMPLOYEE SHOULD BE PAYED FOR BOTH WORKING
      *          AND VACATION, AND END UP BACK ON VACATION.
      *          IF CALLED FROM VACATION, AND NOT PAY-VAC-AND-WORK
      *          THEN THEIR REMAINING VACATION TIME SLIPS WILL BE
      *          CREDITED BACK IN '917, AND AFTER THEIR SHIFT THEY
      *          SHOULD REMAIN IN 'AVAILABLE' STATUS.
      *          SRP, 4/19/1996, START
      *          THIS PROGRAM DOES NOT CALL '917, SO THE VACATION CREDIT
      *          PROCESS WILL BE FRONT-ENDED HERE INSTEAD OF THERE.
                 IF      VACATION            OF LAYOFF-CODE-1
                 AND NOT WS-SAVE-PAY-VAC-AND-WORK
                    MOVE SPACES              TO UNAVAILABLE-LAST-FLAG
                                                MSTR-LAST-STATUS-REASON
                    SET  UNMARK-VACATION     TO TRUE
                 ELSE
      *          SRP, 4/18/1996, END
                    SET PROCESS-QUAL         TO TRUE
                 END-IF
              END-IF
           END-IF
           SET WORKING OF WS-MSTR TO TRUE
           IF P919-FROM-IVR-CALL
              IF LAYOFF-CODE-2 NOT = '1'
                 MOVE '8'                  TO LAYOFF-CODE-2
              END-IF
           ELSE
              IF  LAYOFF-CODE-2 NOT = '1'
              AND LAYOFF-CODE-2 NOT = '9'
                 MOVE '0'               TO LAYOFF-CODE-2
              END-IF
           END-IF

           STRING P919-CALL-DIST
                  P919-CALL-SUB-DIST
                  P919-CALL-POOL
                  WS-AHIST-DATE-TIME
                  P919-TRAIN
                  DELIMITED BY SIZE
                  INTO EMP-TRHIST-KEY
           IF DEC-FURTHER-NOTICE AND EMP-FTV-DATE NOT > SPACES
              MOVE WS-EMP-EFF-DATE            TO EMP-FTV-DATE
           END-IF
           IF DEC-STT-STARTS-EXCPT-FLAG        > SPACES
              MOVE DEC-STT-STARTS-EXCPT-FLAG TO
                                               EMP-STT-STARTS-EXCPT-FLAG
           ELSE
              MOVE SPACES                   TO EMP-STT-STARTS-EXCPT-FLAG
           END-IF
           IF DEC-OVT-STARTS-EXCPT-FLAG         > SPACES
              MOVE DEC-OVT-STARTS-EXCPT-FLAG TO
                                               EMP-OVT-STARTS-EXCPT-FLAG
           ELSE
              MOVE SPACES                   TO EMP-OVT-STARTS-EXCPT-FLAG
           END-IF
           IF CALLED-AT-HOME-TERM
              MOVE SPACES                   TO EMP-EXTENDED-RUN-INFO
              IF  TEMPORARY-ASGNMT > SPACES
                 MOVE TEMPORARY-ASGNMT-FLAG TO EMP-XR-ASGN-TYPE
                 MOVE TEMPORARY-ASGNMT      TO EMP-XR-ASSIGNMENT
              ELSE
                 IF NORMAL-ASGNMT > SPACES
                    MOVE NORMAL-ASGNMT-FLAG TO EMP-XR-ASGN-TYPE
                    MOVE NORMAL-ASGNMT      TO EMP-XR-ASSIGNMENT
                 ELSE
                    SET EMP-XR-NO-JOB       TO TRUE
                 END-IF
              END-IF
           END-IF
           PERFORM P8550-REWRITE-MASTER
      *
      *    RESET THE EMPLOYEE'S SHORT CALL LEAD TIME, WHEN APPLICABLE
      *
      *C1194 - BEG
      *** SYSTEM WAS REMOVING THE CALL LEAD/SHORT CALL FROM EMPLOYEES
      *** FILE WHEN IVR CALL IS INITIATED (IVR INIT). IT SHOULD NOT BE
      *** REMOVED UNTIL AFTER THE CALL IS PLACED IN THE SYSTEM, EITHER
      *** BY SUCESSFUL IVR CALL OR A MANUAL CALL.
           IF P919-FROM-IVR-CALL
              CONTINUE
           ELSE
              MOVE P919-ASGN-ON-DUTY-EMP    TO TELENBRK
              PERFORM P8521-READ-TELE-UPDATE
              IF NOT SUCCESS
                 IF NOT NO-RECORD-FND
                    MOVE 'P1100-4'          TO ERR-PARAGRAPH
                    MOVE TELENBRK           TO ERR-KEY
                    PERFORM P9999-GOT-PROBLEM
                 END-IF
              ELSE
                 MOVE SPACES                TO TELE-REQ-LEAD-TIME
                                               TELE-REQ-CALL-TIME
                 PERFORM P8551-REWRITE-TELE
              END-IF
           END-IF
      *C1194 - END

      *    SRP, 4/22/1996, START
           IF  UNMARK-VACATION
               PERFORM P2700-SPLIT-VAC-SCHEDULE
           END-IF
      *    SRP, 4/22/1996, END
      *
      *    WHEN YARD JOB, AND CALL OR SCAL FUNCTION, SEE IF HOLIDAY.
      *    WHEN HOLIDAY, CHANGE FUNC CODE TO CAO* OR SCO* ON FIRST CALL
      *    OF THE HOLIDAY.  ALL SUBSEQUENT CALLS DO NOT CHANGE.
      *
           IF P919-CALL-POOL = JOB-DEF-YARD-POOL
              IF P919-CALL-FUNC-CODE = 'CALL' OR 'VCLL' OR 'SCAL'
                 MOVE SPACES                  TO WS-CNTL-FILE
                 MOVE '20'                    TO CNTL-REC-TYPE
                 MOVE P919-CALL-DIST          TO CNTL-DIST
                 MOVE P919-CALL-SUB-DIST      TO CNTL-SUB-DIST
                 MOVE 'Y'                     TO CNTL-YARD-ROAD
                 MOVE WS-AHIST-DATE-TIME      TO WS-HOLIDAY-DATE-TIME
                 MOVE WS-HOLIDAY-MO           TO CNTL-HOLIDAY-MO
                 MOVE WS-HOLIDAY-DY           TO CNTL-HOLIDAY-DY
                 MOVE WS-HOLIDAY-YR           TO CNTL-HOLIDAY-YR
                 MOVE CNTLKEY-AREA            TO WORK-CNTLKEY
                 PERFORM P8640-READ-CNTLFILE
                 IF SUCCESS
                    IF WS-HOLIDAY-TIME < CNTL-HOLIDAY-FROM
                    OR WS-HOLIDAY-TIME > CNTL-HOLIDAY-TO
                       CONTINUE
                    ELSE
                       MOVE WS-HOLIDAY-DATE-TIME TO
                                               WS-HOLIDAY-FROM-DATE-TIME
                                               WS-HOLIDAY-TO-DATE-TIME
                       MOVE CNTL-HOLIDAY-FROM    TO WS-HOLIDAY-FROM-TIME
                       MOVE CNTL-HOLIDAY-TO      TO WS-HOLIDAY-TO-TIME
                       IF WS-EMP-TIME-ZONE    NOT = TZ-SYSTEM-TIME-ZONE
                          MOVE SPACES            TO TZ-PARAMETERS
                          MOVE WS-EMP-TIME-ZONE  TO TZ-IN-ZONE
                          MOVE WS-HOLIDAY-FROM-DATE-TIME
                                                 TO TZ-IN-DATE-TIME
                          SET TZ-OUT-SYSTEM-ZONE TO TRUE
                          PERFORM P8996-TIMEZONE
                          IF TZ-INVALID-PARAMETERS
                             MOVE 'P1100TZ' TO ERR-PARAGRAPH
                             PERFORM P8996-TZERROR
                          END-IF
                          MOVE TZ-OUT-DATE-TIME  TO
                                               WS-HOLIDAY-FROM-DATE-TIME

                          MOVE SPACES            TO TZ-PARAMETERS
                          MOVE WS-EMP-TIME-ZONE  TO TZ-IN-ZONE
                          MOVE WS-HOLIDAY-TO-DATE-TIME
                                                 TO TZ-IN-DATE-TIME
                          SET TZ-OUT-SYSTEM-ZONE TO TRUE
                          PERFORM P8996-TIMEZONE
                          IF TZ-INVALID-PARAMETERS
                             MOVE 'P1100TZ'      TO ERR-PARAGRAPH
                             PERFORM P8996-TZERROR
                          END-IF
                          MOVE TZ-OUT-DATE-TIME  TO
                                               WS-HOLIDAY-TO-DATE-TIME
                       END-IF

                       SET  DE-YYMMDD-FORMAT     TO TRUE
                       MOVE WS-HOLIDAY-FROM-DATE-TIME TO DE-YYMMDD
                       PERFORM P8998-DATEEDIT
                       MOVE DE-YYMMDD-CE         TO WS-HOLIDAY-FROM-CE

                       SET  DE-YYMMDD-FORMAT     TO TRUE
                       MOVE WS-HOLIDAY-TO-DATE-TIME TO DE-YYMMDD
                       PERFORM P8998-DATEEDIT
                       MOVE DE-YYMMDD-CE         TO WS-HOLIDAY-TO-CE

                       PERFORM P1101-CHECK-POINTER
                       IF FIRST-HOLIDAY-CALL
                          EVALUATE P919-CALL-FUNC-CODE
                             WHEN 'CALL'
                                  MOVE 'CAO*' TO P919-CALL-FUNC-CODE
      * #1489
                             WHEN 'VCLL'
                                  MOVE 'VAO*' TO P919-CALL-FUNC-CODE
                             WHEN OTHER
                                  MOVE 'SCO*' TO P919-CALL-FUNC-CODE
                          END-EVALUATE
                       END-IF
                    END-IF
                 ELSE
                    IF NOT NO-RECORD-FND
                       MOVE 'P1100-5' TO ERR-PARAGRAPH
                       MOVE CNTLKEY   TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                 END-IF
              END-IF
           END-IF
      *
      *    WHEN NEEDED UPDATE QUALIFICATION FILE AND/OR EMP MASTER
      *    DOS STARTED TIME
      *
           IF PROCESS-QUAL
              MOVE SPACES                TO P963-COMMAREA-PARMS
              MOVE P919-ASGN-ON-DUTY-EMP TO P963-EMP-NO
      *       MOVE WS-FURLOUGHED         TO P963-FROM-STATUS-CODE
              MOVE UNAVAILABLE-LAST-FLAG TO P963-FROM-STATUS-CODE
              MOVE WS-AVAILABLE          TO P963-TO-STATUS-CODE
              MOVE WS-EMP-EFF-DATE-TIME  TO P963-EFF-DATE-TIME
              SET P963-NOT-US-DOMICILED  TO TRUE
              SET P963-DOS-REQUIRED      TO TRUE
              EXEC CICS LINK
                        PROGRAM(P963-PGM)
                        COMMAREA(P963-COMMAREA-PARMS)
                        LENGTH(P963-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1100-6' TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF
           IF EMP-FROM-EM AND SEND-EM-DATA
             PERFORM P8990-SEND-TO-EM
           END-IF
           PERFORM P1110-WRITE-ON-DUTY-RECORD
           IF P919-DECKEY > SPACES
              IF DEC-FURTHER-NOTICE
                 AND NOT TEMPORARY-TURN
                 PERFORM P1120-ASGN-EMP-TEMPORARY
      *
      *          THE PERSON'S EXTENDED RUN ASSIGNMENT IS NOW THE JOB
      *          ASSIGNED FURTHER NOTICE, SO UPDATE MSTR RECORD AGAIN.
      *
                 IF CALLED-AT-HOME-TERM
                    MOVE P919-ASGN-ON-DUTY-EMP TO MSTRNBRK
                    PERFORM P8520-READ-MASTER-UPDATE
                    IF NOT SUCCESS
                       MOVE 'P1100-Z' TO ERR-PARAGRAPH
                       MOVE MSTRNBRK  TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                    MOVE SPACES                TO EMP-EXTENDED-RUN-INFO
                    MOVE ASGN-JOB-TYPE         TO EMP-XR-ASGN-TYPE
                    MOVE ASGN-ASSIGNMENT       TO EMP-XR-ASSIGNMENT
                    PERFORM P8550-REWRITE-MASTER
                 END-IF
                 MOVE SPACES                   TO P943-COMMAREA-PARMS
                 SET P943-ASSIGNED-FURTHER-NOT-FUN TO TRUE
                 MOVE EMP-NBR OF WS-MSTR       TO P943-EMP-NBR
                 IF P919-ASGN-UFP
                    MOVE DIST OF WS-UFP        TO P943-DIST
                    MOVE SUB-DIST OF WS-UFP    TO P943-SDIST
                    MOVE POOL-NAME OF WS-UFP   TO P943-POOL-ASG
                    MOVE IN-OUT-TERMINAL       TO P943-IN-OUT
                 ELSE
                    MOVE AJ-JOB-DIST           TO P943-DIST
                    MOVE AJ-JOB-SUB-DIST       TO P943-SDIST
                    IF P919-TRAIN > SPACES
                       IF AJ-JOB-ASGN-ID(1:2) = 'EX'
                          MOVE P919-ASGN-PROFILE-KEY  TO WS-PROFILE-KEY
                          IF WS-LOCAL-REC
                             MOVE JOB-DEF-LOCAL-POOL
                                                  TO P943-POOL-ASG
                          ELSE
                             MOVE JOB-DEF-YARD-POOL
                                                  TO P943-POOL-ASG
                          END-IF
                       ELSE
      *CNC0562 - BEG
                          MOVE P919-CALL-POOL     TO P943-POOL-ASG
      *                   MOVE AJ-JOB-ASGN-ID  TO JOB-DEF-CHECK
      *                   IF JOB-DEF-LOCAL-ASGN
      *                      MOVE JOB-DEF-LOCAL-POOL
      *                                           TO P943-POOL-ASG
      *                   ELSE
      *                      MOVE JOB-DEF-YARD-POOL
      *                                           TO P943-POOL-ASG
      *                   END-IF
      *CNC0562 - END
                       END-IF
                    END-IF
                    MOVE '0'                   TO P943-IN-OUT
                 END-IF
      *----------------------------------------------------------------*
      *          CONVERT HIST TIME-ZONE  FROM EMP-TZ TO TZ OF JOB
      *----------------------------------------------------------------*
                 IF P919-TIME-ZONE  NOT = WS-EMP-TIME-ZONE
                    PERFORM P8994-CONVERT-HIST-TIMEZONE
                 ELSE
                    MOVE WS-EMP-EFF-DATE-TIME  TO P943-EFF-DATE-TIME
                 END-IF

                 MOVE LAYOFF-CODE OF WS-MSTR   TO P943-LO
                 MOVE ASGN-ASSIGNMENT          TO P943-TAT
                 MOVE CRAFT OF WS-MSTR         TO P943-CRAFT
                 IF  TEMPORARY-ASGNMT > SPACES
                    MOVE TEMPORARY-ASGNMT      TO P943-NORM-ASGN
                 ELSE
                    MOVE NORMAL-ASGNMT         TO P943-NORM-ASGN
                 END-IF
                 IF P919-HAVE-TEMP-EMP
                    MOVE P919-ASGN-TEMP-EMP    TO P943-EMP-NBR-AFFECTED
                 ELSE
                    IF P919-HAVE-OWNER
                       MOVE P919-ASGN-OWNER    TO P943-EMP-NBR-AFFECTED
                    ELSE
                       MOVE '999999998'        TO P943-EMP-NBR-AFFECTED
                    END-IF
                 END-IF
                 SET P943-NOT-REG-EMP          TO TRUE
                 PERFORM P8900-WRITE-HISTORY
              END-IF
           END-IF
           PERFORM P1150-UPDATE-ASGN-HIST
           MOVE SPACES                        TO P943-COMMAREA-PARMS
           IF  P919-FROM-IVR-CALL
      *    AND IVR-CALL-IN-PROCESS
              SET  P943-IVR-CALL-INIT-FUN     TO TRUE
           ELSE
              SET  P943-CALL-FUN              TO TRUE
           END-IF
           IF P919-ASGN-UFP
              MOVE DIST OF WS-UFP             TO P943-DIST
              MOVE SUB-DIST OF WS-UFP         TO P943-SDIST
              MOVE POOL-NAME OF WS-UFP        TO P943-POOL-ASG
              MOVE IN-OUT-TERMINAL            TO P943-IN-OUT
           ELSE
              MOVE AJ-JOB-DIST                TO P943-DIST
              MOVE AJ-JOB-SUB-DIST            TO P943-SDIST
              IF P919-TRAIN > SPACES
                 IF AJ-JOB-ASGN-ID(1:2) = 'EX'
                    MOVE P919-ASGN-PROFILE-KEY TO WS-PROFILE-KEY
                    IF WS-LOCAL-REC
                       MOVE JOB-DEF-LOCAL-POOL TO P943-POOL-ASG
                    ELSE
                       MOVE JOB-DEF-YARD-POOL TO P943-POOL-ASG
                    END-IF
                 ELSE
      *CNC0562 - BEG
                    MOVE P919-CALL-POOL       TO P943-POOL-ASG
      *             MOVE AJ-JOB-ASGN-ID       TO JOB-DEF-CHECK
      *             IF JOB-DEF-LOCAL-ASGN
      *                MOVE JOB-DEF-LOCAL-POOL TO P943-POOL-ASG
      *             ELSE
      *                MOVE JOB-DEF-YARD-POOL TO P943-POOL-ASG
      *             END-IF
      *CNC0562 - END
                 END-IF
              END-IF
              MOVE '0'                        TO P943-IN-OUT
           END-IF
           MOVE WS-HOME-AWAY-FLAG           TO P943-FUN01-HOME-AWAY-FLAG
      *------------------------------------------------------------*
      *     CONVERT HIST TIME-ZONE  FROM EMP-TZ  TO TZ OF JOB      *
      *------------------------------------------------------------*
           IF P919-TIME-ZONE  NOT = WS-EMP-TIME-ZONE
              PERFORM P8994-CONVERT-HIST-TIMEZONE
           ELSE
              MOVE WS-EMP-EFF-DATE-TIME       TO P943-EFF-DATE-TIME
           END-IF
      *
           MOVE P919-ASGN-ON-DUTY-EMP         TO P943-EMP-NBR
           MOVE LAYOFF-CODE OF WS-MSTR        TO P943-LO
           MOVE CRAFT OF WS-MSTR              TO P943-CRAFT
           IF P919-TRAIN > SPACES
              MOVE P919-TRAIN                 TO P943-TEMP-ASGN-TRAIN
           ELSE
              MOVE P919-ASGN-ASGN             TO P943-TEMP-ASGN-NO
           END-IF
           MOVE P919-CALL-CC                  TO P943-TEMP-ASGN-CC
           IF  TEMPORARY-ASGNMT > SPACES
              MOVE TEMPORARY-ASGNMT           TO P943-NORM-ASGN
           ELSE
              MOVE NORMAL-ASGNMT              TO P943-NORM-ASGN
           END-IF
           IF P919-HAVE-TEMP-EMP
              IF P919-ASGN-TEMP-EMP NOT = P919-ASGN-ON-DUTY-EMP
                 MOVE P919-ASGN-TEMP-EMP TO P943-EMP-NBR-AFFECTED
                 SET P943-NOT-REG-EMP    TO TRUE
              END-IF
           ELSE
              IF P919-HAVE-OWNER
                 IF P919-ASGN-OWNER NOT = P919-ASGN-ON-DUTY-EMP
                    MOVE P919-ASGN-OWNER TO P943-EMP-NBR-AFFECTED
                    SET P943-NOT-REG-EMP TO TRUE
                 END-IF
              ELSE
      *CNC0554 - BEG
                 IF (P919-ASGN-UFP
                 AND P919-EMPTRK-EMP-NO > SPACES
                 AND P919-EMPTRK-EMP-NO NOT = P919-ASGN-ON-DUTY-EMP)
                    MOVE P919-EMPTRK-EMP-NO TO P943-EMP-NBR-AFFECTED
                    SET P943-NOT-REG-EMP TO TRUE
                 ELSE
                    MOVE '999999998'     TO P943-EMP-NBR-AFFECTED
                    SET P943-NOT-REG-EMP TO TRUE
                 END-IF
      *CNC0554 - END
              END-IF
           END-IF
           MOVE P926-ASGN-HIST-KEY       TO P943-FUN01-XREF-KEY
           IF P919-DECKEY > SPACES
              MOVE P919-DEC-RULE-NO      TO P943-RULE-NO
           END-IF
           MOVE P919-CALL-FUNC-CODE      TO P943-FUN01-TYPE
           IF P919-IVR-CALL-FUNC
              SET  P943-FUN01-CALL-FR-IVR TO TRUE
           END-IF
           MOVE P919-ASGN-MERG-CLASS     TO P943-ASGN-MERG-CLASS
           MOVE WS-CALL-FROM-LO-CODE     TO P943-CALL-FROM-LO-CODE
           MOVE WS-CALL-FROM-ECC-CODE    TO P943-CALL-FROM-ECC-CODE
           MOVE P919-EMP-REST-STATUS     TO P943-FUN01-REST-STATUS
      *
      *    THIS IS FOR THE NORFOLK SOUTHERN
      *
      *    IF CALL-AND-AUGMENT
      *      SET CALL-AND-AUGMENT-TO-XB  TO TRUE
      *      MOVE WS-CAUG-XB-ID          TO P943-CALL-AUG-XB-ID
      *      MOVE WS-CAUG-XB-TURN        TO P943-CALL-AUG-TURN
      *    END-IF
      *
           PERFORM P8900-WRITE-HISTORY

           IF (TEMP-ASGN-XB
              OR (NORM-ASGN-XB AND TEMPORARY-ASGNMT NOT > SPACE))
              AND ON-DUTY-ASGNMT NOT > SPACE
      *       PERFORM P1140-CHECK-GUARANTEE
              MOVE SPACES TO P913-COMMAREA-PARMS
              IF DEC-RETAIN-POS
                 SET P913-RETAIN-POSITION TO TRUE
              END-IF
              SET P913-CALL-FUNCTION TO TRUE
              IF TEMP-ASGN-XB
                 MOVE TA-DIST     TO P913-TURN-DIST
                 MOVE TA-SUB-DIST TO P913-TURN-SUB-DIST
                 MOVE TA-CC       TO P913-TURN-CC
                 MOVE TA-XB-TURN  TO P913-TURN
              ELSE
                 MOVE NA-DIST     TO P913-TURN-DIST
                 MOVE NA-SUB-DIST TO P913-TURN-SUB-DIST
                 MOVE NA-CC       TO P913-TURN-CC
                 MOVE NA-XB-TURN  TO P913-TURN
              END-IF
              MOVE P919-EFF-DATE-TIME TO P913-EFF-DATE-TIME
              MOVE P919-TIME-ZONE     TO P913-TIME-ZONE
              MOVE P919-TRAIN-DATE-TIME TO P913-TRAIN-DATE-TIME
              IF AJ-JOB-ASGN-ID(1:2) = 'EX'
                 MOVE P919-ASGN-PROFILE-KEY  TO WS-PROFILE-KEY
                 IF WS-LOCAL-REC
                    SET JOB-DEF-LOCAL-ASGN   TO TRUE
                 ELSE
                    SET JOB-DEF-YARD-ASGN    TO TRUE
                 END-IF
              ELSE
                 MOVE P919-ASGN-ASGN  TO JOB-DEF-CHECK
              END-IF
      *-------------------------------------------------------------
      *       FOR FASTSLOW SPAREBOARDS THE 'FROM-BOARD' FLAG MAY BE
      *       SET IN PRIOR PROCESSING TO DETERMINE ROAD VS. YARD.
      *----------------------------------------------------------PLS
              IF P919-FROM-FAST-BOARD
                 SET P913-YARD-CALL   TO TRUE
              ELSE
                 IF P919-FROM-SLOW-BOARD
                    SET P913-ROAD-CALL   TO TRUE
                 ELSE
                    IF P919-ASGN-UFP
                    OR JOB-DEF-LOCAL-ASGN
                       SET P913-ROAD-CALL   TO TRUE
                    ELSE
                       SET P913-YARD-CALL   TO TRUE
                    END-IF
                 END-IF
              END-IF
              EXEC CICS LINK
                        PROGRAM(P913-PGM)
                        COMMAREA(P913-COMMAREA-PARMS)
                        LENGTH(P913-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1100-8' TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF

           IF P943-EMP-NBR-AFFECTED            > '000000000'
              AND P943-EMP-NBR-AFFECTED        < '999999995'
              SET  P943-LOST-WORK-FUN         TO TRUE
              MOVE P943-EMP-NBR-AFFECTED      TO MSTRNBRK
              MOVE P943-EMP-NBR               TO P943-EMP-NBR-AFFECTED
              PERFORM P8500-READ-MASTER
              MOVE LAYOFF-CODE                TO P943-LO
              MOVE LAYOFF-EM-CODE             TO P943-LAYOFF-EM-CODE
              MOVE CRAFT OF WS-MSTR           TO P943-CRAFT
              PERFORM P1104-CHECK-REST-DAY
              IF EMP-NOT-ON-REST-DAY
                 MOVE EMP-NBR OF WS-MSTR   TO P943-EMP-NBR
                 MOVE NORMAL-ASGNMT        TO P943-NORM-ASGN
                 IF P919-DISQUALIFIED-STARTS > ZERO
                    IF WORK-JS-KEY1 > SPACES
                       SET P943-PULLED-STARTS-FUN TO TRUE
                    END-IF
                    MOVE P919-DISQUALIFIED-STARTS TO
                                                    P943-FUN41-STARTS
                    PERFORM P8900-WRITE-HISTORY
                 END-IF
              END-IF
           END-IF
      *
           PERFORM P1160-REMOVE-I-TIME.
      *
      *    RELEASE ANY TRACKING THIS EMPLOYEE MAY HAVE ON THIS
      *    ASSIGNMENT C513  GER 9/25/03
      *
           MOVE SPACES                      TO P938-COMMAREA-PARMS
           SET P938-REL-EMP-FUNCTION        TO TRUE
           MOVE P919-ASGN-TYPE              TO P938-ASGN-TYPE
           MOVE P919-ASGN-PARM              TO P938-ASGN-PARM
           MOVE WS-EMP-EFF-DATE-TIME        TO P938-EFF-DATE-TIME
           MOVE P919-TIME-ZONE              TO P938-TIME-ZONE
           MOVE P919-ASGN-ON-DUTY-EMP       TO P938-REL-EMP-NO
           PERFORM P1105-CALL-938.

      *
       P1101-CHECK-POINTER.
      *
           SET  FIRST-HOLIDAY-CALL            TO TRUE
           SET  HOLIDAY-CALL-NOT-FOUND        TO TRUE
           MOVE SPACES                        TO POINT-NBR-KEY
                                                 WS-POINTR-DONE-CODE
           MOVE P919-ASGN-ON-DUTY-EMP         TO POINT-EMP-NBR
           SET  POINT-GUARANTEE               TO TRUE
           MOVE WS-HOLIDAY-FROM-CE-DATE-TIME  TO POINT-EFF-CE-DATE-TIME

           PERFORM UNTIL POINTR-DONE
              MOVE POINT-NBR-KEY               TO POINTKEY
              PERFORM P8020-READ-POINTER-REC-GTEQ
              IF  SUCCESS
              AND POINT-EMP-NBR                = P919-ASGN-ON-DUTY-EMP
              AND POINT-GUARANTEE
              AND POINT-EFF-CE-DATE-TIME   NOT <
                                            WS-HOLIDAY-FROM-CE-DATE-TIME
              AND POINT-EFF-CE-DATE-TIME   NOT >
                                            WS-HOLIDAY-TO-CE-DATE-TIME
                 IF POINT-CALL-FUN
                    SET  HOLIDAY-CALL-FOUND    TO TRUE
                 END-IF
                 IF  POINT-TIE-UP-FUN
                 AND HOLIDAY-CALL-FOUND
                    SET  NOT-FIRST-HOLIDAY-CALL TO TRUE
                    SET  POINTR-DONE           TO TRUE
                 END-IF
                 ADD  1                        TO POINT-SEQ
              ELSE
                 SET  POINTR-DONE              TO TRUE
              END-IF
           END-PERFORM.
      *
       P1104-CHECK-REST-DAY.
      *
           SET EMP-NOT-ON-REST-DAY TO TRUE
           MOVE SPACES                  TO WORK-JS-KEY1
      ** RRR, C-447, CHANGE START
      **   IF (TEMPORARY-ASGNMT        NOT  > SPACES
      **      AND ON-DUTY-ASGNMT       NOT  > SPACES)
      **      OR (NOT AVAILABLE AND NOT WORKING)
           IF ON-DUTY-ASGNMT > SPACES OR
              AVAILABLE OR
              WORKING
              CONTINUE
           ELSE
      ** RRR, C-447, CHANGE END
              IF TEMPORARY-ASGNMT           > SPACES
                 AND TEMP-ASGN-AJ
                 MOVE TEMPORARY-ASGNMT     TO WORK-JS-KEY1
              ELSE
                 IF NORMAL-ASGNMT           > SPACES
                    AND NORM-ASGN-AJ
                    MOVE NORMAL-ASGNMT     TO WORK-JS-KEY1
                 END-IF
              END-IF
              IF WORK-JS-KEY1               > SPACES
                 MOVE P919-EFF-DATE        TO WK-JSK1-EXP-DATE
                 MOVE WORK-JS-KEY1         TO JSKEY1
                 EXEC CICS READ
                           DATASET(JS-VIA-JSKEY1)
                           INTO(WS-JOB-SCHEDULE)
                           LENGTH(JSKEY1-RLGTH)
                           RIDFLD(JSKEY1)
                           KEYLENGTH(JSKEY1-KLGTH)
                           GTEQ
                           RESP(WS-RESPONSE)
                 END-EXEC
                 MOVE WS-RESPONSE          TO FILE-STATUS
                 IF SUCCESS
                    IF JSK1-ASGN-DIST         = WK-JSK1-ASGN-DIST
                       AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST
                       AND JSK1-ASSIGNMENT    = WK-JSK1-ASSIGNMENT
                       AND JSK1-ASGN-STATS
                       CONTINUE
                    ELSE
                       SET NO-RECORD-FND   TO TRUE
                    END-IF
                 END-IF
                 IF NOT SUCCESS
                    IF NOT (NO-RECORD-FND OR END-OF-FILE)
                       MOVE 'P1100-9'      TO ERR-PARAGRAPH
                       MOVE JSKEY1         TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                    MOVE SPACES            TO WS-JOB-SCHEDULE
                 END-IF
                 IF JOB-ON-REST-DAY(DAY1)
                    SET EMP-ON-REST-DAY TO TRUE
                 END-IF
              END-IF
           END-IF.
      *
       P1105-CALL-938.
      *
           EXEC CICS LINK
                     PROGRAM(P938-PGM)
                     COMMAREA(P938-COMMAREA-PARMS)
                     LENGTH(P938-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE                 TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1105-1'                TO ERR-PARAGRAPH
              MOVE 'P938LINK'               TO ERR-KEY
              MOVE P938-COMMAREA-PARMS      TO ERR-SENTENCE
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
       P1110-WRITE-ON-DUTY-RECORD.
      *
           MOVE P919-ASGN-TYPE TO WK-ASGN-JOB-TYPE
           MOVE P919-ASGN-PARM TO WK-ASGN-ASSIGNMENT
           PERFORM PXXXX-ON-DUTY-EMP
           IF ASGN-EMP-NO NOT > ZERO
              MOVE P919-ASGN-ON-DUTY-EMP TO WK-ASGN-EMP-NO
              PERFORM PXXXX-JOB-ON-DUTY
              IF ASGN-ASSIGNMENT > SPACES
      *-----------------------------------------------------------------
      *          SAVE ANY DATA THAT MAY BE REQUIRED FOR A HELDAWAY
      *          CLAIM, SHOULD A CALL AND CANCEL OCCUR.
      *-----------------------------------------------------------------
                 MOVE ASGN-ON-DUTY-JOB-TYPE TO WS-SAVE-HA-JOB-TYPE
                 MOVE ASGN-HELDAWAY-SECTION TO WS-SAVE-HA-HELDAWAY
                 MOVE ASGN-ASSIGNED-POOL-FLAG TO SAVE-ASGN-POOL-FLAG
                 MOVE ASGNKEY1 TO ASGNJOB
                 EXEC CICS DELETE
                           DATASET(ASGN-VIA-ASGNJOB)
                           RIDFLD(ASGNJOB)
                           RESP(WS-RESPONSE)
                 END-EXEC
                 MOVE WS-RESPONSE TO FILE-STATUS
                 IF NOT SUCCESS
                    MOVE 'P1110-1' TO ERR-PARAGRAPH
                    MOVE ASGNJOB   TO ERR-KEY
                    PERFORM P9999-GOT-PROBLEM
                 END-IF
              END-IF
              MOVE SPACES TO WS-ASGN-FILE
              IF  P919-ASGN-UFP
              AND CALLED-AT-HOME-TERM
                 MOVE SPACES              TO WORK-CNTLKEY
                 MOVE '03'                TO WK-CNTL-REC-TYPE
      *C1212 - BEG
      ***        MOVE P919-CALL-DIST      TO WK-CNTL-DIST
      ***        MOVE P919-CALL-SUB-DIST  TO WK-CNTL-SUB-DIST
      ***        MOVE P919-CALL-POOL      TO WK-CNTL-POOL
                 MOVE P919-ASGN-DIST      TO WK-CNTL-DIST
                 MOVE P919-ASGN-SUB-DIST  TO WK-CNTL-SUB-DIST
                 MOVE P919-ASGN-POOL      TO WK-CNTL-POOL
      *C1212 - END
                 MOVE 'F'                 TO WK-CNTL-POOL-TYPE
                 PERFORM P8640-READ-CNTLFILE
                 IF NOT SUCCESS
                    MOVE 'P1110-C'        TO ERR-PARAGRAPH
                    MOVE CNTLKEY          TO ERR-KEY
                    PERFORM P9999-GOT-PROBLEM
                 END-IF
                 MOVE CNTL-POOL-ASGN-POOL TO ASGN-ASSIGNED-POOL-FLAG
              ELSE
                 MOVE SAVE-ASGN-POOL-FLAG TO ASGN-ASSIGNED-POOL-FLAG
              END-IF

              MOVE P919-ASGN-TYPE        TO ASGN-JOB-TYPE
              MOVE P919-ASGN-PARM        TO ASGN-ASSIGNMENT
              MOVE P919-ASGN-ON-DUTY-EMP TO ASGN-EMP-NO
              MOVE '3' TO ASGN-REC-TYPE
                          ASGN-EMP-NO-REC-TYPE
              MOVE ZEROS TO ASGN-DATE-TIME
                            ASGN-EMP-DATE-TIME
              MOVE WS-TRAIN-DATE-TIME    TO ASGN-ON-DUTY-DATE-TIME
              MOVE P919-TRAIN            TO ASGN-TRAIN-ID
              MOVE P919-CALL-CC          TO ASGN-TRAIN-CC
              IF P919-DECKEY > SPACES AND
                 DEC-FN = '1'
                 MOVE 'Y'                TO ASGN-SAVE-ON-DUTY
              ELSE
                 MOVE SPACES             TO ASGN-SAVE-ON-DUTY
              END-IF
              MOVE WS-SAVE-HA-JOB-TYPE   TO ASGN-ON-DUTY-JOB-TYPE
              MOVE WS-SAVE-HA-HELDAWAY   TO ASGN-HELDAWAY-SECTION
              IF P919-ASGN-UFP
                 MOVE DIST     OF WS-UFP  TO ASGN-HOME-BOARD-DIST
                 MOVE SUB-DIST OF WS-UFP  TO ASGN-HOME-BOARD-SDIST
                 MOVE POOL-NAME OF WS-UFP TO ASGN-HOME-BOARD-POOL
                 MOVE IN-OUT-TERMINAL     TO ASGN-HOME-BOARD-TERM
              END-IF
              IF P919-DISQUALIFIED-STARTS > ZERO
                 SET ASGN-DISQUALIFIED-STARTS TO TRUE
              END-IF
              MOVE ASGNKEY1             TO ASGNJOB
              EXEC CICS WRITE
                        DATASET(ASGN-VIA-ASGNJOB)
                        FROM(WS-ASGN-FILE)
                        LENGTH(ASGNJOB-RLGTH)
                        RIDFLD(ASGNJOB)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1110-2' TO ERR-PARAGRAPH
                 MOVE ASGNJOB   TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           ELSE
              MOVE ASGNKEY1     TO WORK-ASGNKEY1
              IF P919-ASGN-ON-DUTY-EMP NOT = ASGN-EMP-NO
                 MOVE P919-ASGN-ON-DUTY-EMP TO WK-ASGN-EMP-NO
                 PERFORM PXXXX-JOB-ON-DUTY
                 IF ASGN-ASSIGNMENT > SPACES
                    MOVE ASGNKEY1 TO ASGNJOB
                    EXEC CICS DELETE
                              DATASET(ASGN-VIA-ASGNJOB)
                              RIDFLD(ASGNJOB)
                              RESP(WS-RESPONSE)
                    END-EXEC
                    MOVE WS-RESPONSE TO FILE-STATUS
                    IF NOT SUCCESS
                       MOVE 'P1110-3' TO ERR-PARAGRAPH
                       MOVE ASGNJOB   TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                 END-IF
              END-IF
              MOVE WORK-ASGNKEY1      TO ASGNJOB
              EXEC CICS READ
                        UPDATE
                        DATASET(ASGN-VIA-ASGNJOB)
                        INTO(WS-ASGN-FILE)
                        LENGTH(ASGNJOB-RLGTH)
                        RIDFLD(ASGNJOB)
                        KEYLENGTH(ASGNJOB-KLGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1110-4' TO ERR-PARAGRAPH
                 MOVE ASGNJOB   TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE WS-TRAIN-DATE-TIME   TO ASGN-ON-DUTY-DATE-TIME
              MOVE P919-TRAIN           TO ASGN-TRAIN-ID
              MOVE P919-CALL-CC         TO ASGN-TRAIN-CC
              MOVE EMP-NBR OF WS-MSTR   TO ASGN-EMP-NO
              IF P919-DECKEY > SPACES AND
                 DEC-FN = '1'
                 MOVE 'Y'               TO ASGN-SAVE-ON-DUTY
              ELSE
                 MOVE SPACES            TO ASGN-SAVE-ON-DUTY
              END-IF
              EXEC CICS REWRITE
                        DATASET(ASGN-VIA-ASGNJOB)
                        FROM(WS-ASGN-FILE)
                        LENGTH(ASGNJOB-RLGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1110-5' TO ERR-PARAGRAPH
                 MOVE ASGNJOB   TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF.
      *
       P1120-ASGN-EMP-TEMPORARY.
      *
           MOVE SPACES     TO WS-ASGN-FILE
           MOVE P919-ASGN-TYPE TO ASGN-JOB-TYPE
           MOVE P919-ASGN-PARM TO ASGN-ASSIGNMENT
           MOVE '2' TO ASGN-REC-TYPE
                       ASGN-EMP-NO-REC-TYPE
           MOVE WS-TRAIN-DATE-TIME   TO ASGN-DATE-TIME
                                        ASGN-EMP-DATE-TIME
                                        ASGN-ASSIGNMENT-DATE-TIME
           MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO
      *    MOVE ZEROS    TO ASGN-ON-DUTY-DATE-TIME
           MOVE 'N'      TO WS-ASGN-DONE-CODE
           MOVE ZEROS    TO WS-ASGN-COUNT
           MOVE DEC-FN   TO ASGN-DH-F-NOTICE-FLAG
           PERFORM UNTIL ASGN-DONE
              MOVE ASGNKEY1 TO ASGNJOB
              EXEC CICS WRITE
                        DATASET(ASGN-VIA-ASGNJOB)
                        FROM(WS-ASGN-FILE)
                        LENGTH(ASGNJOB-RLGTH)
                        RIDFLD(ASGNJOB)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF SUCCESS
                 SET ASGN-DONE TO TRUE
              ELSE
                 IF DUP-KEY-ERR
                    MOVE ZEROS      TO DATE-CONVERSION-PARMS
                    SET PARM-ADD    TO TRUE
                    MOVE ASGN-DATE  TO PARM-PRI-DATE-GREG
                    MOVE ASGN-TIME  TO PARM-PRI-HRMN
                    MOVE 0001       TO PARM-SEC-HRMN
                    EXEC CICS LINK
                              PROGRAM(P903-PGM)
                              COMMAREA(DATE-CONVERSION-PARMS)
                              LENGTH(P903-LGTH)
                              RESP(WS-RESPONSE)
                    END-EXEC
                    MOVE WS-RESPONSE TO FILE-STATUS
                    IF NOT SUCCESS
                       MOVE 'P1120-1' TO ERR-PARAGRAPH
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                    MOVE PARM-RES-DATE-GREG TO ASGN-DATE
                    MOVE PARM-RES-HRMN      TO ASGN-TIME
                    MOVE ASGN-DATE-TIME     TO ASGN-EMP-DATE-TIME
                    ADD 1 TO WS-ASGN-COUNT
                    IF WS-ASGN-COUNT > 5
                       SET ASGN-DONE  TO TRUE
                       MOVE 'P1120-2' TO ERR-PARAGRAPH
                       MOVE ASGNJOB   TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                 ELSE
                    SET ASGN-DONE TO TRUE
                    MOVE 'P1120-3' TO ERR-PARAGRAPH
                    MOVE ASGNJOB  TO ERR-KEY
                    PERFORM P9999-GOT-PROBLEM
                 END-IF
              END-IF
           END-PERFORM
           MOVE 'N' TO WS-ASGN-DONE-CODE
           IF DIST OF WS-MSTR NOT = ASGN-DIST
              OR SUB-DIST OF WS-MSTR NOT = ASGN-SUB-DIST
              MOVE SPACES TO P918-COMMAREA-PARMS
              MOVE EMP-NBR OF WS-MSTR TO P918-EMP-NO
              MOVE ASGN-DIST TO P918-DIST
              MOVE ASGN-SUB-DIST TO P918-SUB-DIST
              MOVE CRAFT OF WS-MSTR TO P918-OLD-CC
              EXEC CICS LINK
                        PROGRAM(P918-PGM)
                        COMMAREA(P918-COMMAREA-PARMS)
                        LENGTH(P918-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1120-2' TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF.
      *CNC0600-B
      *
       P1130-CHECK-NATURAL-RESET-BRK.
      *
      * CNLD-214 B CNC0600
           MOVE SPACES TO NATURAL-RESET-AT-CALL-FL
                          NATURAL-RESET-BEFORE-CALL-FL
      * CNLD-214 E
           MOVE P919-ASGN-ON-DUTY-EMP      TO MSTRNBRK
           PERFORM P8500-READ-MASTER
           MOVE EMP-NBR OF WS-MSTR         TO MSTR3NBRK
           PERFORM P8560-READ-MSTR3
      *CNLD-244-B #3 CNC0600
      ** CNLD-214 B CNC0600
      *     PERFORM P1141-ORD-MINUS-LEAD-AND-LOCAL
      ** CNLD-214 E
           PERFORM P1141-OD-MINUS-LEAD-AND-LOCAL
      *CNLD-244-E
           IF WS-MSTR3-FOUND AND
              MSTR3-PROJ-RESET-BRK = MSTR3-SYSTEM-RESET-BRK
              AND MSTR3-SYSTEM-RESET-BRK > '0000000000'
      *CALL IS HAPPENING AFTER A SYSTEM RESET
      *
      *SYSTEM-RESETS HAPPEN WHEN EMP IS AT HOME; IF FOR SOME REASON THIS
      *CALL IS NOT CALLED-AT-HOME-TERM, WE WILL STILL MARK THE HOS AFTER
      *SYSTEM-RESET AS 7DAY-START.
      *
      *CHECK IF ANY HOS RECORDS ADDED AFTER THE RESET BREAK & DETERMINE
      * THE START OF 7 DAY PERIOD

              PERFORM P1131-PRV-HOS-AFTER-SYS-RESET
              PERFORM P1140-CALCULATE-7DAY-END
              PERFORM P8562-READ-MASTER3-UPD
      * CNLD-214 B CNC0600
              IF NATURAL-RESET-AT-CALL
      *       IF MULTIPLE NATURAL RESETS WERE FOUND, RESET-NAT HOS GETS
      *       WRITTEN FOR EVERY NATURAL RESET.
      *       IF THE CALL GETS CC/RL, ONLY THE NATURAL-RESET-AT-CALL
      *       RESET-NAT (IF THERE IS ONE) WILL BE DELETED, ALL THE
      *       NATURAL-RESET-BEFORE-CALL RESET-NAT RECORDS (IF THERE ARE
      *       ANY) WILL BE RETAINED.
      *
                 PERFORM P1137-WRITE-RESET-BRK-WRK-HIST
                 PERFORM P1138-WRITE-ETOD-RESET-BREAK
      *
                 IF WS-PREV-7DAY-START-DTTM > '0000000000'
                    PERFORM P1140B-CALC-PREV-7DAY-END
                 ELSE
                    MOVE MSTR3-7DAY-END-PERIOD
                                           TO MSTR3-PREV-7DAY-END
                 END-IF
      *
              END-IF
      * CNLD-214 E
              MOVE WS-WRR-7DAY-END         TO MSTR3-7DAY-END-PERIOD
              PERFORM P8563-MASTER3-REWRITE
           ELSE
      *
      *CHECKING IF NATURAL RESET HAS HAPPENED ALREADY
              PERFORM P1132-RECALC-PROJ-RESET-BRK
      * CNLD-214 B CNC0600
              IF NATURAL-RESET-AT-CALL
      *       IF MULTIPLE NATURAL RESETS WERE FOUND, RESET-NAT HOS GETS
      *       WRITTEN FOR EVERY NATURAL RESET.
      *       IF THE CALL GETS CC/RL, ONLY THE NATURAL-RESET-AT-CALL
      *       RESET-NAT (IF THERE IS ONE) WILL BE DELETED, ALL THE
      *       NATURAL-RESET-BEFORE-CALL RESET-NAT RECORDS WILL BE
      *       RETAINED.
      *
                 PERFORM P1137-WRITE-RESET-BRK-WRK-HIST
                 PERFORM P1138-WRITE-ETOD-RESET-BREAK
      *
                 IF WS-PREV-7DAY-START-DTTM > '0000000000'
                    PERFORM P1140B-CALC-PREV-7DAY-END
                 ELSE
                    MOVE MSTR3-7DAY-END-PERIOD
                                           TO MSTR3-PREV-7DAY-END
                 END-IF
      *
              END-IF
      *
              PERFORM P1139-CALC-NEW-7DAY-END
      *
      * CNLD-214 E
           END-IF

           .
      *
       P1131-PRV-HOS-AFTER-SYS-RESET.
      *
      *    CONTROL REACHES HERE ONLY IF :
      *    MSTR3-SYSTEM-RESET-BRK = MSTR3-PROJ-RESET-BRK

597600     PERFORM P1132A-FETCH-WRR-START

602900     IF MSTR3-PROJ-RESET-BRK > WS-WRR-START-DTTM
603300        MOVE MSTR3-PROJ-RESET-BRK TO WS-7DAY-START-DTTM
603300     ELSE
600700        PERFORM P1132D-COMP-8DAYS-AGO
602800
602900        IF WS-8DAYS-AGO-DTTM > WS-WRR-START-DTTM
603200           MOVE WS-8DAYS-AGO-DTTM  TO WS-7DAY-START-DTTM
603100        ELSE
603000*          START READING FROM (WS-WRR-START-DTTM - 1 DAY)
603000           PERFORM P1132B-WRR-START-MINUS-1DY
603300        END-IF
603000     END-IF

           MOVE SPACES                     TO WS-EMP-TOD
           MOVE MSTR3-EMP-NBR              TO EMPTOD-EMP-NBR
           MOVE WS-7DAY-START-DATE         TO DE-YYMMDD
           SET DE-YYMMDD-FORMAT            TO TRUE
           PERFORM P8998-DATEEDIT
           MOVE DE-CCYYMMDD                TO EMPTOD-OD-DATE-CENT
           MOVE WS-7DAY-START-TIME         TO EMPTOD-OD-TIME
           MOVE EMPTOD-KEY-AREA            TO EMPTOD-EMPKEY

           PERFORM P8570-EMPTOD-STARTBR
           MOVE 'N'                        TO WS-READ-DONE-CODE
      * CNLD-214 B CNC0600
           MOVE '0000000000'               TO WS-7DAY-START-DTTM
      * CNLD-214 E
           PERFORM UNTIL READ-DONE
              PERFORM P8580-EMPTOD-READNEXT
              IF SUCCESS
                 IF EMPTOD-EMP-NBR = MSTR3-EMP-NBR
      * CNLD-214 B CNC0600
                    AND WS-EMP-EFF-DATE-TIME > EMPTOD-OD-YYMMDD-HRMN
                    IF (NOT EMPTOD-VIRTUAL-ENTRY) AND
      *
                       (NOT EMPTOD-IMPOSED-REST-REC)
      *             EMPTOD-IMPOSED-REST-REC IS UNLIKELY TO BE FOUND WITH
      *             NEW CAN WRR, ADDING THIS AS A SANITY CHECK
                       IF EMPTOD-RESET-BREAK-REC
                          MOVE '0000000000'   TO WS-7DAY-START-DTTM
                       END-IF
                       IF EMPTOD-CONTACT AND
                          WS-7DAY-START-DTTM > '0000000000'
                          IF ((EMPTOD-OD-YYMMDD-HRMN >=
                                   WS-UPD-PROJ-UD1-FROM
                                AND EMPTOD-OD-YYMMDD-HRMN <=
                                     WS-UPD-PROJ-UD1-TO)
                              OR
                              (EMPTOD-OD-YYMMDD-HRMN >=
                                  WS-UPD-PROJ-UD2-FROM
                                AND EMPTOD-OD-YYMMDD-HRMN <=
                                        WS-UPD-PROJ-UD2-TO)
                              OR
                              (EMPTOD-OFFD-YYMMDD-HRMN >=
                                  WS-UPD-PROJ-UD1-FROM
                                AND EMPTOD-OFFD-YYMMDD-HRMN <=
                                        WS-UPD-PROJ-UD1-TO)
                              OR
                              (EMPTOD-OFFD-YYMMDD-HRMN >=
                                  WS-UPD-PROJ-UD2-FROM
                                AND EMPTOD-OFFD-YYMMDD-HRMN <=
                                        WS-UPD-PROJ-UD2-TO))
                             PERFORM P1136E-EXTEND-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                         TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
                       END-IF
      * CNLD-214 E
                       IF (NOT EMPTOD-CONTACT) AND
                          (NOT EMPTOD-RESET-BREAK-REC)
      * CNLD-214 B CNC0600
611200
611300                    IF WS-7DAY-START-DTTM NOT > '0000000000'
611300                    AND EMPTOD-OD-YYMMDD-HRMN <=
611300                                              WS-WRR-START-DTTM
611300                    AND EMPTOD-OFFD-YYMMDD-HRMN >
611300                                              WS-WRR-START-DTTM
611400                       MOVE WS-WRR-START-DTTM
611500                                   TO WS-7DAY-START-DTTM
                                            WS-PREV-7DAY-START-DTTM
611600                       PERFORM P1136-COMPUTE-PROJ-RESET
611700                       MOVE EMPTOD-OFFD-YYMMDD-HRMN
611800                                   TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
611900                    END-IF
611200
                          IF WS-7DAY-START-DTTM NOT > '0000000000'
      * CNLD-214 E
611300                    AND EMPTOD-OD-YYMMDD-HRMN >=
611300                                              WS-WRR-START-DTTM
                             MOVE EMPTOD-OD-YYMMDD-HRMN
                                              TO WS-7DAY-START-DTTM
                                              WS-PREV-7DAY-START-DTTM
      * CNLD-214 B CNC0600
                             PERFORM P1136-COMPUTE-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                      TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
                          IF WS-7DAY-START-DTTM  > '0000000000'
                          AND EMPTOD-OD-YYMMDD-HRMN >
                              WS-UPD-PROJ-DATE-TIME
                          AND (EMPTOD-HOS-CLAIM OR
                               EMPTOD-COMMUTE   OR
                               EMPTOD-MANUAL)
      *                      TRAIN RELATED HOS SHOULD BE SKIPPED:
      *                      WITH TRAINS, (ORDER - LEAD) SHOULD BE MORE
      *                      THAN PROJECTED-RESET WHICH IS CHECKED AT
      *                      THE TIME OF CALL; HERE, WE ONLY LOOK FOR
      *                      RESETS BEFORE HOS-CLAIMS AND MANUAL/COMMUTE
      *
      *                      THERE WILL BE AT LEAST 1 HOS RECORD BETWEEN
      *                      SYSTEM RESET AND THIS NATURAL RESET
                             SET NATURAL-RESET-BEFORE-CALL TO TRUE
      *
                             PERFORM P8600-EMPTOD-ENDBR
                             SET ENDED-BROWSE-AT-THIS-REC TO TRUE
      *
                             PERFORM P1137-WRITE-RESET-BRK-WRK-HIST
                             PERFORM P1138-WRITE-ETOD-RESET-BREAK
      *
                             MOVE EMPTOD-OD-YYMMDD-HRMN
                                              TO WS-7DAY-START-DTTM
                                                 WS-PREV-7DAY-START-DTTM
                             PERFORM P1136-COMPUTE-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                         TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
                          IF WS-7DAY-START-DTTM  > '0000000000'
                          AND EMPTOD-OFFD-YYMMDD-HRMN >
                              LAST-EMPTOD-OFFD-YYMMDD-HRMN
      *                      THERE CAN BE OVERLAPPING HOS, PROJ-RESET
      *                      SHOULD BE COMPUTED W/ GREATEST OFFD READ
                             PERFORM P1136-COMPUTE-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                         TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
      * CNLD-214 E
                       END-IF
      * CNLD-214 B CNC0600
                    END-IF
      * CNLD-214 E
                 ELSE
                    SET READ-DONE          TO TRUE
                 END-IF
              ELSE
                 SET READ-DONE             TO TRUE
              END-IF
      * CNLD-214 B CNC0600
              IF ENDED-BROWSE-AT-THIS-REC
                 MOVE EMPTOD-KEY-AREA TO EMPTOD-EMPKEY
                 PERFORM P8570-EMPTOD-STARTBR
      *          IF THIS LOOP IS MODIFIED, PLEASE NOTE THIS SAME RECORD
      *          WE JUST PROCESSED WILL BE READ AGAIN AT READNEXT.

                 MOVE SPACES          TO ENDED-BROWSE-FL
              END-IF
      * CNLD-214 E
           END-PERFORM
           PERFORM P8600-EMPTOD-ENDBR

      * CNLD-214 B CNC0600
           IF (WS-7DAY-START-DTTM NOT > '0000000000')
      *       NO HOS RECORDS WERE FOUND AFTER SYSTEM RESET
      * CNLD-214 E
              MOVE WS-EMP-EFF-DATE-TIME    TO WS-7DAY-START-DTTM
      * CNLD-214 B CNC0600
           ELSE
      *       HOS RECORDS WERE FOUND AFTER SYSTEM RESET;

      *    NO NEED FOR 'MSTR3-PROJ-RESET-BRK > WS-UPD-PROJ-DATE-TIME'
      *    CHECK HERE AS MSTR3-SYSTEM-RESET-BRK = MSTR3-PROJ-RESET-BRK
      *    AND WE STARTED READING EMPTOD FROM THERE

      *CNLD-244-B #3 CNC0600
      *       IF WS-ORDER-MINUS-LEAD-DTTM > WS-UPD-PROJ-DATE-TIME
      *       AND WS-EMP-EFF-DATE-TIME    > WS-UPD-PROJ-DATE-TIME
              IF WS-OD-MINUS-LEAD-DTTM    > WS-UPD-PROJ-DATE-TIME
      *CNLD-244-E
      *       'LOCAL-TIME > WS-UPD-PROJ-DATE-TIME' CHECK WILL HAPPEN
      *       DURING THE P925 EDITS FOR EVERYONE APPLICABLE (THIS EDIT
      *       IS NOT APPLICABLE FOR SHIFT-CALL REGULAR OWNERS)
595800
617300*       NATURAL-RESET-AT-CALL CAN HAPPEN ONLY WHEN CALLED AT HOME;
617300*       THIS FIRST TRAIN-CALL AFTER A SYS RESET SHOULD BE AT HOME;
617300*       IF FOR SOME REASON IT ISN'T, WE WILL STILL NEED TO RECORD
617300*       THIS AS A NATURAL-RESET FOR CALCULATION PURPOSES, SO THE
617300*       CALLED-AT-HOME-TERM CHECK SHOULD NOT BE ADDED HERE.
617300
                 SET NATURAL-RESET-AT-CALL TO TRUE
      *
                 MOVE WS-EMP-EFF-DATE-TIME TO WS-7DAY-START-DTTM
              END-IF
      * CNLD-214 E
           END-IF
           .
      *
       P1132-RECALC-PROJ-RESET-BRK.
      *
597600     PERFORM P1132A-FETCH-WRR-START

      *RJA  5/17/2023 B CNC0600
           IF MSTR3-7DAY-END-PERIOD > '0000000000'
      *       COMPUTE 7DAY START FROM MSTR3
              PERFORM P1140A-CALCULATE-7DAY-START

602900        IF WS-7DAY-START-DTTM > WS-WRR-START-DTTM
603300           CONTINUE
603300        ELSE
600700           PERFORM P1132D-COMP-8DAYS-AGO
602800
602900           IF WS-8DAYS-AGO-DTTM > WS-WRR-START-DTTM
603200              MOVE WS-8DAYS-AGO-DTTM  TO WS-7DAY-START-DTTM
603100           ELSE
603000*             START READING FROM (WS-WRR-START-DTTM - 1 DAY)
603000              PERFORM P1132B-WRR-START-MINUS-1DY
603300           END-IF
603000        END-IF
           ELSE
600700        PERFORM P1132D-COMP-8DAYS-AGO

602900        IF WS-8DAYS-AGO-DTTM > WS-WRR-START-DTTM
603200           MOVE WS-8DAYS-AGO-DTTM  TO WS-7DAY-START-DTTM
603100        ELSE
603000*          START READING FROM (WS-WRR-START-DTTM - 1 DAY)
603000           PERFORM P1132B-WRR-START-MINUS-1DY
603300        END-IF
           END-IF

           PERFORM P1133-PRV-HOS-AFTER-7DY-START
      *RJA  5/17/2023 E
           .
      *RJA  5/22/2023 B CNC0600
603800*
603900 P1132A-FETCH-WRR-START.
604000*
400        IF WS-WRR-START-DTTM > SPACES
597500* VALIDATE DTTM ON SUB-DISTRICT CONTROL
600           PERFORM P1132C-CHECK-WRR-START
700        END-IF
800
900        IF WS-WRR-START-DTTM NOT > SPACES
598000* NO DTTM/VALID-DTTM ON SUB-DISTRICT CHECK THE CORPORATE CONTROL
100           MOVE SPACES          TO CNTLKEY-AREA
200           SET COMPANY-TYPE-REC TO TRUE
300           EXEC CICS READ
400                     DATASET(CNTL-FILE-VIA-CNTLKEY)
500                     INTO(WS-CNTL-FILE)
600                     LENGTH(CNTLFILE-RLGTH)
700                     RIDFLD(CNTLKEY-AREA)
800                     KEYLENGTH(CNTLFILE-KLGTH)
900                     RESP(WS-RESPONSE)
000           END-EXEC
100           MOVE WS-RESPONSE TO FILE-STATUS
200           IF NOT SUCCESS
300             MOVE 'P1132A-1' TO ERR-PARAGRAPH
400             PERFORM P9999-GOT-PROBLEM
500           END-IF
600           IF CNTL-00-WRR-BEGINNING-DTTM > SPACES
700              MOVE CNTL-00-WRR-BEGINNING-DTTM
800                                   TO WS-WRR-START-DTTM
900              PERFORM P1132C-CHECK-WRR-START
000           END-IF
100        END-IF
200
300        IF WS-WRR-START-DTTM NOT > SPACES
600400* NO DATE ON CONTROLS DEFAULT TO GOVERNMENT DATE
500           MOVE '2305250001' TO WS-WRR-START-DTTM
600        END-IF
603700     .
603800
603800*
603900 P1132B-WRR-START-MINUS-1DY.
604000*
050600* SUBTRACT 1 DAY FROM THE ABOVE RESULT - THE THEORY HERE IS THAT
050700*  NO TOUR OF DUTY WILL HAVE LASTED MORE THAN 24 HOURS, SO WE CAN
050800*  GO BACK 1 DAY AND START LOOKING AT THE RECORDS STARTING FROM
050900*  THAT DATE TO DETERMINE IF ANY OF THE TIME ON DUTY SHOULD COUNT
051100     MOVE ZEROS              TO DATE-CONVERSION-PARMS
051200     SET PARM-SUBTRACT       TO TRUE
051300     MOVE WS-WRR-START-DATE  TO PARM-PRI-DATE-GREG
051400     MOVE 01                 TO PARM-SEC-GREG-DAY
449400     EXEC CICS LINK
449500               PROGRAM(P903-PGM)
449600               COMMAREA(DATE-CONVERSION-PARMS)
449700               LENGTH(P903-LGTH)
449800               RESP(WS-RESPONSE)
449900     END-EXEC
450000     MOVE WS-RESPONSE             TO FILE-STATUS
450100     IF NOT SUCCESS
450200        MOVE 'P1132B-1'           TO ERR-PARAGRAPH
450300        MOVE 'P903LINK'           TO ERR-KEY
450400        PERFORM P9999-GOT-PROBLEM
450500     END-IF
051600
051800     MOVE PARM-RES-DATE-GREG TO WS-7DAY-START-DATE
052100     MOVE WS-WRR-START-TIME  TO WS-7DAY-START-TIME
603700     .
603800
      *RJA  5/22/2023 E CNC0600
603800
      *RJA  5/17/2023 B
      *
       P1132C-CHECK-WRR-START.
      *
           MOVE WS-WRR-START-DATE   TO DE-YYMMDD
           SET DE-YYMMDD-FORMAT     TO TRUE
           PERFORM P8998-DATEEDIT
           IF DE-INVALID-DATE
              MOVE SPACES TO WS-WRR-START-DTTM
           END-IF
           IF WS-WRR-START-TIME > SPACES
              MOVE WS-WRR-START-TIME   TO TE-MILITARY-TIME
              SET TE-MILITARY-FORMAT   TO TRUE
              PERFORM P8997-TIMEEDIT
              IF TE-INVALID-TIME
                 MOVE SPACES TO WS-WRR-START-DTTM
              END-IF
           END-IF
           .
603800*
603900 P1132D-COMP-8DAYS-AGO.
604000*
600800* COMPUTE THE DATE 8 DAYS AGO FROM CURRENT ON-DUTY TIME
900        MOVE ZEROS                 TO DATE-CONVERSION-PARMS
000        SET PARM-SUBTRACT          TO TRUE
100        MOVE WS-EMP-EFF-DATE       TO PARM-PRI-DATE-GREG
200        MOVE WS-EMP-EFF-TIME       TO PARM-PRI-HRMN
300        MOVE 000008                TO PARM-SEC-GREG-DAY
400        EXEC CICS LINK
500                  PROGRAM(P903-PGM)
600                  COMMAREA(DATE-CONVERSION-PARMS)
700                  LENGTH(P903-LGTH)
800                  RESP(WS-RESPONSE)
900        END-EXEC
000        MOVE WS-RESPONSE           TO FILE-STATUS
100        IF NOT SUCCESS
200           MOVE 'P1132D-1'         TO ERR-PARAGRAPH
300           MOVE 'P903LINK'         TO ERR-KEY
400           PERFORM P9999-GOT-PROBLEM
500        END-IF
600        MOVE PARM-RES-DATE-GREG    TO WS-8DAYS-AGO-DATE
700        MOVE PARM-RES-HRMN         TO WS-8DAYS-AGO-TIME
605500     .
      *RJA  5/17/2023 E

      * CNLD-214 B CNC0600
      *
       P1133-PRV-HOS-AFTER-7DY-START.
      *
           MOVE SPACES                     TO WS-EMP-TOD
           MOVE MSTR3-EMP-NBR              TO EMPTOD-EMP-NBR
           MOVE WS-7DAY-START-DATE         TO DE-YYMMDD
           SET DE-YYMMDD-FORMAT            TO TRUE
           PERFORM P8998-DATEEDIT
           MOVE DE-CCYYMMDD                TO EMPTOD-OD-DATE-CENT
           MOVE WS-7DAY-START-TIME         TO EMPTOD-OD-TIME
           MOVE EMPTOD-KEY-AREA            TO EMPTOD-EMPKEY

           PERFORM P8570-EMPTOD-STARTBR
           MOVE 'N'                        TO WS-READ-DONE-CODE
           MOVE '0000000000'               TO WS-7DAY-START-DTTM
           PERFORM UNTIL READ-DONE
              PERFORM P8580-EMPTOD-READNEXT
              IF SUCCESS
                 IF EMPTOD-EMP-NBR = MSTR3-EMP-NBR
                    AND WS-EMP-EFF-DATE-TIME > EMPTOD-OD-YYMMDD-HRMN
                    IF (NOT EMPTOD-VIRTUAL-ENTRY) AND
      *
                       (NOT EMPTOD-IMPOSED-REST-REC)
      *             EMPTOD-IMPOSED-REST-REC IS UNLIKELY TO BE FOUND WITH
      *             NEW CAN WRR, ADDING THIS AS A SANITY CHECK
                       IF EMPTOD-RESET-BREAK-REC
                          MOVE '0000000000'   TO WS-7DAY-START-DTTM
                       END-IF
                       IF EMPTOD-CONTACT AND
                          WS-7DAY-START-DTTM > '0000000000'
                          IF ((EMPTOD-OD-YYMMDD-HRMN >=
                                   WS-UPD-PROJ-UD1-FROM
                                AND EMPTOD-OD-YYMMDD-HRMN <=
                                     WS-UPD-PROJ-UD1-TO)
                              OR
                              (EMPTOD-OD-YYMMDD-HRMN >=
                                  WS-UPD-PROJ-UD2-FROM
                                AND EMPTOD-OD-YYMMDD-HRMN <=
                                        WS-UPD-PROJ-UD2-TO)
                              OR
                              (EMPTOD-OFFD-YYMMDD-HRMN >=
                                  WS-UPD-PROJ-UD1-FROM
                                AND EMPTOD-OFFD-YYMMDD-HRMN <=
                                        WS-UPD-PROJ-UD1-TO)
                              OR
                              (EMPTOD-OFFD-YYMMDD-HRMN >=
                                  WS-UPD-PROJ-UD2-FROM
                                AND EMPTOD-OFFD-YYMMDD-HRMN <=
                                        WS-UPD-PROJ-UD2-TO))
                             PERFORM P1136E-EXTEND-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                         TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
                       END-IF
                       IF (NOT EMPTOD-CONTACT) AND
                          (NOT EMPTOD-RESET-BREAK-REC)
611200
611300                    IF WS-7DAY-START-DTTM NOT > '0000000000'
611300                    AND EMPTOD-OD-YYMMDD-HRMN <=
611300                                              WS-WRR-START-DTTM
611300                    AND EMPTOD-OFFD-YYMMDD-HRMN >
611300                                              WS-WRR-START-DTTM
611400                       MOVE WS-WRR-START-DTTM
611500                                   TO WS-7DAY-START-DTTM
                                            WS-PREV-7DAY-START-DTTM
611600                       PERFORM P1136-COMPUTE-PROJ-RESET
611700                       MOVE EMPTOD-OFFD-YYMMDD-HRMN
611800                                   TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
611900                    END-IF
611200
                          IF WS-7DAY-START-DTTM NOT > '0000000000'
611300                    AND EMPTOD-OD-YYMMDD-HRMN >=
611300                                              WS-WRR-START-DTTM
                             MOVE EMPTOD-OD-YYMMDD-HRMN
                                              TO WS-7DAY-START-DTTM
                                             WS-PREV-7DAY-START-DTTM
                             PERFORM P1136-COMPUTE-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                      TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
611200
                          IF WS-7DAY-START-DTTM  > '0000000000'
                          AND EMPTOD-OD-YYMMDD-HRMN >
                              WS-UPD-PROJ-DATE-TIME
                          AND (EMPTOD-HOS-CLAIM OR
                               EMPTOD-COMMUTE   OR
                               EMPTOD-MANUAL)
      *                      TRAIN RELATED HOS SHOULD BE SKIPPED :
      *                      WITH TRAINS, (ORDER - LEAD) SHOULD BE MORE
      *                      THAN PROJECTED-RESET WHICH IS CHECKED AT
      *                      THE TIME OF CALL; HERE, WE ONLY LOOK FOR
      *                      RESETS BEFORE HOS-CLAIMS AND MANUAL/COMMUTE
      *
                             SET NATURAL-RESET-BEFORE-CALL TO TRUE
      *
                             PERFORM P8600-EMPTOD-ENDBR
                             SET ENDED-BROWSE-AT-THIS-REC TO TRUE
      *
                             PERFORM P1137-WRITE-RESET-BRK-WRK-HIST
                             PERFORM P1138-WRITE-ETOD-RESET-BREAK
      *
                             MOVE EMPTOD-OD-YYMMDD-HRMN
                                              TO WS-7DAY-START-DTTM
                                                 WS-PREV-7DAY-START-DTTM
                             PERFORM P1136-COMPUTE-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                         TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
611200
                          IF WS-7DAY-START-DTTM  > '0000000000'
                          AND EMPTOD-OFFD-YYMMDD-HRMN >
                             LAST-EMPTOD-OFFD-YYMMDD-HRMN
      *                      THERE CAN BE OVERLAPPING HOS, PROJ-RESET
      *                      SHOULD BE COMPUTED W/ GREATEST OFFD READ
                             PERFORM P1136-COMPUTE-PROJ-RESET
                             MOVE EMPTOD-OFFD-YYMMDD-HRMN
                                         TO LAST-EMPTOD-OFFD-YYMMDD-HRMN
                          END-IF
                       END-IF
                    END-IF
                 ELSE
                    SET READ-DONE          TO TRUE
                 END-IF
              ELSE
                 SET READ-DONE             TO TRUE
              END-IF
      *
              IF ENDED-BROWSE-AT-THIS-REC
                 MOVE EMPTOD-KEY-AREA TO EMPTOD-EMPKEY
                 PERFORM P8570-EMPTOD-STARTBR
      *          IF THIS LOOP IS MODIFIED, PLEASE NOTE THIS SAME RECORD
      *          WE JUST PROCESSED WILL BE READ AGAIN AT READNEXT.

                 MOVE SPACES          TO ENDED-BROWSE-FL
              END-IF
      *
           END-PERFORM
           PERFORM P8600-EMPTOD-ENDBR

           IF (WS-7DAY-START-DTTM NOT > '0000000000')
      *       NO HOS RECORDS WERE FOUND; CURRENT ON-DUTY TIME WILL MARK
      *       THE START OF 7DAY RESET WINDOW.
              MOVE WS-EMP-EFF-DATE-TIME    TO WS-7DAY-START-DTTM
           ELSE
      *      ?WILL HAVE TO RETHINK THE IVR-NOTIFY-PROJECTED-RESET
      *      ?EXTENSION PROCESS
              IF MSTR3-PROJ-RESET-BRK-CH > WS-UPD-PROJ-DATE-TIME
                 MOVE MSTR3-PROJ-RESET-BRK-CH TO WS-UPD-PROJ-DATE-TIME
              END-IF
      *
      *CNLD-244-B #3 CNC0600
      *       IF WS-ORDER-MINUS-LEAD-DTTM > WS-UPD-PROJ-DATE-TIME
      *       AND WS-EMP-EFF-DATE-TIME    > WS-UPD-PROJ-DATE-TIME
              IF WS-OD-MINUS-LEAD-DTTM    > WS-UPD-PROJ-DATE-TIME
      *CNLD-244-E
      *RJA  5/17/2023 B CNC0600
      *
      *       NATURAL-RESET-AT-CALL IS WRITTEN ONLY WHEN CALLED AT HOME
      *
              AND CALLED-AT-HOME-TERM
      *RJA  5/17/2023 E

      *       'LOCAL-TIME > WS-UPD-PROJ-DATE-TIME' CHECK WILL HAPPEN
      *       DURING THE P925 EDITS FOR EVERYONE APPLICABLE (THIS EDIT
      *       IS NOT APPLICABLE FOR SHIFT-CALL REGULAR OWNERS)
      *
                 SET NATURAL-RESET-AT-CALL TO TRUE
      *
                 MOVE WS-EMP-EFF-DATE-TIME TO WS-7DAY-START-DTTM
              END-IF
           END-IF
           .
      * CNLD-214 E
      *
       P1136-COMPUTE-PROJ-RESET.
      *
           MOVE EMPTOD-OFFD-DATE        TO WS-DAY1-DATE
           MOVE EMPTOD-OFFD-TIME        TO WS-DAY1-TIME

           MOVE ZEROS                   TO DATE-CONVERSION-PARMS
           SET PARM-ADD                 TO TRUE
           MOVE WS-DAY1-DATE            TO PARM-PRI-DATE-GREG
           MOVE 01                      TO PARM-SEC-GREG-DAY
           EXEC CICS LINK
                     PROGRAM(P903-PGM)
                     COMMAREA(DATE-CONVERSION-PARMS)
                     LENGTH(P903-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE             TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1136-1'            TO ERR-PARAGRAPH
              MOVE 'P903LINK'           TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE PARM-RES-DATE-GREG      TO WS-DAY2-DATE

           MOVE ZEROS                   TO DATE-CONVERSION-PARMS
           SET PARM-ADD                 TO TRUE
           MOVE WS-DAY1-DATE            TO PARM-PRI-DATE-GREG
           MOVE 02                      TO PARM-SEC-GREG-DAY
           EXEC CICS LINK
                     PROGRAM(P903-PGM)
                     COMMAREA(DATE-CONVERSION-PARMS)
                     LENGTH(P903-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE             TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1136-2'            TO ERR-PARAGRAPH
              MOVE 'P903LINK'           TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE PARM-RES-DATE-GREG      TO WS-DAY3-DATE

           IF WS-DAY1-TIME >= 2200 AND WS-DAY1-TIME <= 2359
              MOVE ZEROS                TO DATE-CONVERSION-PARMS
              SET PARM-ADD              TO TRUE
              MOVE WS-DAY1-DATE         TO PARM-PRI-DATE-GREG
              MOVE WS-DAY1-TIME         TO PARM-PRI-HRMN
              MOVE 3200                 TO PARM-SEC-HRMN
              EXEC CICS LINK
                        PROGRAM(P903-PGM)
                        COMMAREA(DATE-CONVERSION-PARMS)
                        LENGTH(P903-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE          TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1136-3'         TO ERR-PARAGRAPH
                 MOVE 'P903LINK'        TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE PARM-RES-DATE-GREG   TO WS-UPD-PROJ-DATE
              MOVE PARM-RES-HRMN        TO WS-UPD-PROJ-TIME
              MOVE WS-DAY1-DATE-TIME    TO WS-UPD-PROJ-UD1-FROM
              MOVE WS-DAY2-DATE         TO WS-UPD-PROJ-UD1-TO(1:6)
              MOVE '0800'               TO WS-UPD-PROJ-UD1-TO(7:4)
              MOVE WS-DAY2-DATE         TO WS-UPD-PROJ-UD2-FROM(1:6)
              MOVE '2200'               TO WS-UPD-PROJ-UD2-FROM(7:4)
              MOVE WS-DAY3-DATE         TO WS-UPD-PROJ-UD2-TO(1:6)
              MOVE '0559'               TO WS-UPD-PROJ-UD2-TO(7:4)
           ELSE
              IF WS-DAY1-TIME = 0000
                 MOVE WS-DAY2-DATE      TO WS-UPD-PROJ-DATE
114000*CNLD-259 B CNC0600
114000*          RESET DURATION WILL BE LESS THAN THE REQUIRED MINIMUM
114000*          OF 32 HOURS IN THIS CASE; WILL BE 31H 59M.
      *          MOVE '0800'            TO WS-UPD-PROJ-TIME
                 MOVE '0759'            TO WS-UPD-PROJ-TIME
114000*CNLD-259 E CNC0600
                 MOVE WS-DAY1-DATE      TO WS-UPD-PROJ-UD1-FROM(1:6)
                                          WS-UPD-PROJ-UD1-TO(1:6)
                                          WS-UPD-PROJ-UD2-FROM(1:6)
                 MOVE '0000'            TO WS-UPD-PROJ-UD1-FROM(7:4)
                 MOVE '0800'            TO WS-UPD-PROJ-UD1-TO(7:4)
                 MOVE '2200'            TO WS-UPD-PROJ-UD2-FROM(7:4)
                 MOVE WS-DAY2-DATE      TO WS-UPD-PROJ-UD2-TO(1:6)
                 MOVE '0559'            TO WS-UPD-PROJ-UD2-TO(7:4)
              ELSE
                 MOVE WS-DAY3-DATE      TO WS-UPD-PROJ-DATE
                 MOVE '0559'            TO WS-UPD-PROJ-TIME
                 MOVE WS-DAY1-DATE      TO WS-UPD-PROJ-UD1-FROM(1:6)
                 MOVE '2200'            TO WS-UPD-PROJ-UD1-FROM(7:4)
                 MOVE WS-DAY2-DATE      TO WS-UPD-PROJ-UD1-TO(1:6)
                 MOVE '0800'            TO WS-UPD-PROJ-UD1-TO(7:4)
                 MOVE WS-DAY2-DATE      TO WS-UPD-PROJ-UD2-FROM(1:6)
                 MOVE '2200'            TO WS-UPD-PROJ-UD2-FROM(7:4)
                 MOVE WS-DAY3-DATE      TO WS-UPD-PROJ-UD2-TO(1:6)
                 MOVE '0559'            TO WS-UPD-PROJ-UD2-TO(7:4)
              END-IF
           END-IF
           .
      *CNLD-214 - B
      *
       P1136E-EXTEND-PROJ-RESET.
      *
              MOVE ZEROS                   TO DATE-CONVERSION-PARMS
              SET PARM-ADD                 TO TRUE
              MOVE WS-UPD-PROJ-DATE        TO PARM-PRI-DATE-GREG
              MOVE 01                      TO PARM-SEC-GREG-DAY
              EXEC CICS LINK
                        PROGRAM(P903-PGM)
                        COMMAREA(DATE-CONVERSION-PARMS)
                        LENGTH(P903-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE             TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1136-1'            TO ERR-PARAGRAPH
                 MOVE 'P903LINK'           TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE PARM-RES-DATE-GREG      TO WS-UPD-PROJ-DATE
              MOVE '0559'                  TO WS-UPD-PROJ-TIME
           .
      *CNLD-214 - E
      *
       P1137-WRITE-RESET-BRK-WRK-HIST.
      *

           PERFORM VARYING I FROM 1 BY 1
           UNTIL I > 3
              MOVE SPACES               TO P943-COMMAREA-PARMS
              IF I = 1
                 STRING 'NATURAL RESET BREAK END: '
                 WS-UPD-PROJ-DATE-TIME DELIMITED
                 BY SIZE INTO P943-FUN27-COMMENTS
              END-IF
              IF I = 2
                 STRING 'UNDISTURB-1: ' WS-UPD-PROJ-UD1-FROM
                 ' TO ' WS-UPD-PROJ-UD1-TO   DELIMITED
                 BY SIZE INTO P943-FUN27-COMMENTS
              END-IF
              IF I = 3
                 STRING 'UNDISTURB-2: ' WS-UPD-PROJ-UD2-FROM
                 ' TO ' WS-UPD-PROJ-UD2-TO   DELIMITED
                 BY SIZE INTO P943-FUN27-COMMENTS
              END-IF
      *05/27/23 B  WRITE RESET BRK HISTORY EFF TIME IN EMP TZ ALWAYS
      *
      *       IF P919-TIME-ZONE  NOT = WS-EMP-TIME-ZONE
      *          PERFORM P8994-CONVERT-HIST-TIMEZONE
      *       ELSE
      *          MOVE WS-EMP-EFF-DATE-TIME
      *                                  TO P943-EFF-DATE-TIME
      *       END-IF
              MOVE WS-EMP-EFF-DATE-TIME  TO P943-EFF-DATE-TIME
      *05/27/23 E
              MOVE EMP-NBR OF WS-MSTR    TO P943-EMP-NBR
              SET P943-RESET-BREAK-FUN   TO TRUE
              MOVE DIST OF WS-MSTR       TO P943-DIST
              MOVE SUB-DIST OF WS-MSTR   TO P943-SDIST
      *05/27/23 B
      *       MOVE WORK-HIST-TIME        TO P943-CLOCK-TIME
      *05/27/23 E
              PERFORM P8900-WRITE-HISTORY
           END-PERFORM
           .
      *
       P1138-WRITE-ETOD-RESET-BREAK.
      *

           INITIALIZE PS08-COMMAREA-PARMS
           SET PS08-RESET-BREAK-FUN      TO TRUE
      * CNLD-214 B
           SET PS08-RESET-BREAK-NAT      TO TRUE
      * CNLD-214 E

           MOVE P919-ASGN-ON-DUTY-EMP    TO PS08-EMP-NBR
           MOVE WS-UPD-PROJ-DATE-TIME    TO PS08-OD-DATE-TIME
                                            PS08-ORDER-DATE-TIME
           SET DE-YYMMDD-FORMAT          TO TRUE
           MOVE PS08-OD-DATE             TO DE-YYMMDD
           PERFORM P8998-DATEEDIT
           MOVE DE-CCYYMMDD              TO PS08-OD-DATE-CENT
                                            PS08-ORDER-DATE-CENT
           MOVE PS08-OD-DATE-TIME-CENT   TO PS08-OFFD-DATE-TIME-CENT

           MOVE P919-CALL-DIST           TO PS08-DIST
           MOVE P919-CALL-SUB-DIST       TO PS08-SDIST
           MOVE P919-TRAIN               TO PS08-TRAIN-ASGN
           MOVE P919-EMP-REST-STATUS     TO PS08-EMP-REST-STATUS
           MOVE P919-CALL-CC             TO PS08-CRAFT


           EXEC CICS LINK
                     PROGRAM(PS08-PGM)
                     COMMAREA(PS08-COMMAREA-PARMS)
                     LENGTH(PS08-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE              TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1138-1'             TO ERR-PARAGRAPH
              PERFORM P9999-GOT-PROBLEM
           END-IF
           IF NOT PS08-NO-ERRORS
              MOVE 'P1138-2'             TO ERR-PARAGRAPH
              MOVE PS08-RETURN-ERRORS    TO ERR-KEY
              MOVE 'CHECK S08 INPUT PARAMETERS'
                                         TO ERR-SENTENCE
              PERFORM P9999-GOT-PROBLEM
           END-IF
           .

      *
       P1139-CALC-NEW-7DAY-END.
      *

           IF MSTR3-7DAY-END-PERIOD IS NUMERIC
              AND MSTR3-7DAY-END-PERIOD-NUM > ZEROS
      * CNLD-214 - B
              IF NATURAL-RESET-AT-CALL
                 MOVE MSTR3-PREV-7DAY-END
                                         TO WS-WRR-PREV-7DAY-END
              END-IF
      * CNLD-214 - E
           ELSE
      * TO INDICATE MSTR3-7DAY-END-PERIOD SHOULD BE RESTORED TO SPACES
      * IN CASE THE CALL IS CANCELED WITH CC/RL
              MOVE '0000000001'          TO
      * CNLD-193 - B
      *                            MSTR3-PREV-7DAY-END
                                   WS-WRR-PREV-7DAY-END
      * CNLD-193 - E
           END-IF

           PERFORM P1140-CALCULATE-7DAY-END

           IF WS-MSTR3-FOUND
              PERFORM P8562-READ-MASTER3-UPD
      * CNLD-193 - B
              MOVE WS-WRR-PREV-7DAY-END  TO MSTR3-PREV-7DAY-END
      * CNLD-193 - E
              MOVE WS-WRR-7DAY-END       TO MSTR3-7DAY-END-PERIOD-CH
      * CNLD-214 B CNC0600
      *       MOVE WS-UPD-PROJ-DATE-TIME TO MSTR3-PROJ-RESET-BRK
      *       MOVE WS-UPD-PROJ-UD1-FROM  TO MSTR3-PROJ-UD1-FROM
      *       MOVE WS-UPD-PROJ-UD1-TO    TO MSTR3-PROJ-UD1-TO
      *       MOVE WS-UPD-PROJ-UD2-FROM  TO MSTR3-PROJ-UD2-FROM
      *       MOVE WS-UPD-PROJ-UD2-TO    TO MSTR3-PROJ-UD2-TO
              IF WS-UPD-PROJ-DATE-TIME > '0000000000'
                 MOVE WS-UPD-PROJ-DATE-TIME TO MSTR3-PROJ-RESET-BRK
              END-IF
              IF WS-UPD-PROJ-UD1-FROM  > '0000000000'
                 MOVE WS-UPD-PROJ-UD1-FROM  TO MSTR3-PROJ-UD1-FROM
              END-IF
              IF WS-UPD-PROJ-UD1-TO    > '0000000000'
                 MOVE WS-UPD-PROJ-UD1-TO    TO MSTR3-PROJ-UD1-TO
              END-IF
              IF WS-UPD-PROJ-UD2-FROM  > '0000000000'
                 MOVE WS-UPD-PROJ-UD2-FROM  TO MSTR3-PROJ-UD2-FROM
              END-IF
              IF WS-UPD-PROJ-UD2-TO    > '0000000000'
                 MOVE WS-UPD-PROJ-UD2-TO    TO MSTR3-PROJ-UD2-TO
              END-IF
      * CNLD-214 E
              PERFORM P8563-MASTER3-REWRITE

           ELSE
      * CNLD-193 - B
              MOVE WS-WRR-PREV-7DAY-END  TO MSTR3-PREV-7DAY-END
      * CNLD-193 - E
              MOVE WS-WRR-7DAY-END       TO MSTR3-7DAY-END-PERIOD-CH
              PERFORM P8561-WRITE-MSTR3
           END-IF

           .
      *
       P1140-CALCULATE-7DAY-END.
      *
           MOVE SPACES                   TO WS-WRR-7DAY-END
           MOVE ZEROS                    TO DATE-CONVERSION-PARMS
           SET PARM-ADD                  TO TRUE
           MOVE WS-7DAY-START-DATE       TO PARM-PRI-DATE-GREG
           MOVE WS-7DAY-START-TIME       TO PARM-PRI-HRMN
           MOVE 000006                   TO PARM-SEC-GREG-DAY
           MOVE 2359                     TO PARM-SEC-HRMN
           EXEC CICS LINK
                     PROGRAM(P903-PGM)
                     COMMAREA(DATE-CONVERSION-PARMS)
                     LENGTH(P903-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE              TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1140-1'             TO ERR-PARAGRAPH
              MOVE 'P903LINK'            TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE PARM-RES-DATE-GREG       TO WS-WRR-7DAY-END-DATE
           MOVE PARM-RES-HRMN            TO WS-WRR-7DAY-END-TIME
           .
      *
      *CNC0600-E
      * CNLD-214 B CNC0600
      *
       P1140A-CALCULATE-7DAY-START.
      *
           MOVE ZEROS                    TO DATE-CONVERSION-PARMS
           SET PARM-SUBTRACT             TO TRUE
           MOVE MSTR3-7DAY-END-DATE      TO PARM-PRI-DATE-GREG
           MOVE MSTR3-7DAY-END-TIME      TO PARM-PRI-HRMN
           MOVE 000006                   TO PARM-SEC-GREG-DAY
           MOVE 2359                     TO PARM-SEC-HRMN
           EXEC CICS LINK
                     PROGRAM(P903-PGM)
                     COMMAREA(DATE-CONVERSION-PARMS)
                     LENGTH(P903-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE              TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1140A-1'            TO ERR-PARAGRAPH
              MOVE 'P903LINK'            TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE PARM-RES-DATE-GREG       TO WS-7DAY-START-DATE
           MOVE PARM-RES-HRMN            TO WS-7DAY-START-TIME
           .
      *
       P1140B-CALC-PREV-7DAY-END.
      *
           MOVE ZEROS                    TO DATE-CONVERSION-PARMS
           SET PARM-ADD                  TO TRUE
           MOVE WS-PREV-7DAY-START-DT    TO PARM-PRI-DATE-GREG
           MOVE WS-PREV-7DAY-START-TM    TO PARM-PRI-HRMN
           MOVE 000006                   TO PARM-SEC-GREG-DAY
           MOVE 2359                     TO PARM-SEC-HRMN
           EXEC CICS LINK
                     PROGRAM(P903-PGM)
                     COMMAREA(DATE-CONVERSION-PARMS)
                     LENGTH(P903-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE              TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1140-1'             TO ERR-PARAGRAPH
              MOVE 'P903LINK'            TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE PARM-RES-DATE-GREG       TO MSTR3-PREV-7DAY-END-DATE
           MOVE PARM-RES-HRMN            TO MSTR3-PREV-7DAY-END-TIME
           .
      * CNLD-214 E
      *
      *CNLD-207-DXC-B
      *CNLD-244-B #3 CNC0600
      *P1141-ORD-MINUS-LEAD-AND-LOCAL.
       P1141-OD-MINUS-LEAD-AND-LOCAL.
      *CNLD-244-E
      *
              MOVE ZEROS                  TO DATE-CONVERSION-PARMS
              SET PARM-SUBTRACT           TO TRUE
      *CNLD-244-B #3 CNC0600
      *       MOVE WS-ORDER-DATE          TO PARM-PRI-DATE-GREG
      *       MOVE WS-ORDER-TIME          TO PARM-PRI-HRMN
              MOVE WS-EMP-EFF-DATE        TO PARM-PRI-DATE-GREG
              MOVE WS-EMP-EFF-TIME        TO PARM-PRI-HRMN
      *CNLD-244-E
              MOVE P919-CAN-WRR-LEAD-TIME TO PARM-SEC-HRMN
              EXEC CICS LINK
                        PROGRAM(P903-PGM)
                        COMMAREA(DATE-CONVERSION-PARMS)
                        LENGTH(P903-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE            TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1141-1'           TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
      *CNLD-244-B #3 CNC0600
      *       MOVE PARM-RES-DATE-GREG     TO WS-ORDER-MINUS-LEAD-DT
      *       MOVE PARM-RES-HRMN          TO WS-ORDER-MINUS-LEAD-TM
              MOVE PARM-RES-DATE-GREG     TO WS-OD-MINUS-LEAD-DT
              MOVE PARM-RES-HRMN          TO WS-OD-MINUS-LEAD-TM
      *CNLD-244-E
      *
              EXEC CICS ASKTIME
                        ABSTIME(WS-ABSTIME)
              END-EXEC
              ADD WS-ABSTIME-OFFSET       TO WS-ABSTIME
              EXEC CICS FORMATTIME
                        ABSTIME(WS-ABSTIME)
                        YYYYMMDD(WS-SYSTEM-DATE-CENT)
                        TIME(WS-SYSTEM-TIME-AREA)
              END-EXEC
      *
      *       CONVERT SYSTEM DATE/TIME TO LOCAL DATE/TIME
      *
              MOVE ZEROS                  TO TZ-PARAMETERS
              MOVE SPACES                 TO TZ-STATUS-FLAG
              MOVE TZ-SYSTEM-TIME-ZONE    TO TZ-IN-ZONE
              MOVE WS-EMP-TIME-ZONE       TO TZ-OUT-ZONE
              MOVE WS-SYSTEM-DATE-TIME    TO TZ-IN-DATE-TIME
              PERFORM P8996-TIMEZONE
              MOVE TZ-OUT-DATE-TIME-CENT  TO WS-LOCAL-DATE-TIME-CENT
      *
           .
      *CNLD-207-DXC-E
      *
       P1144-FIND-SLOW-XB-TURN.
      *                                                          **PLS
           MOVE ZERO  TO WS-DONE-CODE
           MOVE ZEROS TO EB-SLOW-POS-DATE-TIME-TIE
           PERFORM UNTIL DONE
              MOVE EB-SLOW-POS-AREA TO EBSLOW-POS
              EXEC CICS READ
                        DATASET(EB-VIA-SLOW-POSITION)
                        INTO(WS-EXTRA-BOARD)
                        LENGTH(EBSLPOS-RLGTH)
                        RIDFLD(EBSLOW-POS)
                        KEYLENGTH(EBSLPOS-KLGTH)
                        GTEQ
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1144-1'  TO ERR-PARAGRAPH
                 MOVE EBSLOW-POS TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              IF DIST-REPEAT = WORK-XB-DIST
                 AND SUBDIST-REPEAT = WORK-XB-SUB-DIST
                 AND CRAFT-CODE-REPEAT = WORK-XB-CRAFT-CODE
                 AND EBTURN-AREA NOT = WORK-XB-TURN
                 MOVE 'X'                        TO WK-ASGN-JOB-TYPE
                 MOVE DIST-REPEAT                TO WK-ASGN-DIST
                 MOVE SUBDIST-REPEAT             TO WK-ASGN-SUB-DIST
                 MOVE 'EX'                       TO WK-ASGN-POOL
                 MOVE TURN-NBR OF WS-EXTRA-BOARD TO WK-ASGN-TURN
                 MOVE CRAFT-CODE-REPEAT          TO WK-ASGN-CC
                 PERFORM PXXXX-LATEST-TEMP
                 IF ASGN-EMP-NO NOT > ZERO
                    PERFORM PXXXX-JOB-OWNER
                 END-IF
                 IF ASGN-EMP-NO > ZERO
                    MOVE ASGN-EMP-NO             TO MSTRNBRK
                    EXEC CICS READ
                              DATASET(MSTR-VIA-EMP-NBR)
                              INTO(WS-MSTR)
                              LENGTH(MSTRENBR-RLGTH)
                              RIDFLD(MSTRNBRK)
                              KEYLENGTH(MSTRENBR-KLGTH)
                              RESP(WS-RESPONSE)
                    END-EXEC
                    MOVE WS-RESPONSE             TO FILE-STATUS
                    IF NOT SUCCESS
                       MOVE 'P1146-2'            TO ERR-PARAGRAPH
                       MOVE MSTRNBRK             TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                    IF NOT (AVAILABLE OR WORKING)
      *                MOVE SPACES               TO P913-COMMAREA-PARMS
      *                IF DEC-RETAIN-POS
      *                   SET P913-RETAIN-POSITION TO TRUE
      *                END-IF
      *                SET P913-CALL-FUNCTION      TO TRUE
      *                MOVE EBTURN-AREA          TO P913-TURN-PARM
      *                MOVE P919-EFF-DATE-TIME   TO P913-EFF-DATE-TIME
      *                MOVE P919-TIME-ZONE       TO P913-TIME-ZONE
      *                MOVE P919-TRAIN-DATE-TIME TO P913-TRAIN-DATE-TIME
      *                MOVE P919-ASGN-ASGN       TO JOB-DEF-CHECK
      *PLS06/21        SET P913-ROAD-CALL     TO TRUE
      *                EXEC CICS LINK
      *                     PROGRAM(P913-PGM)
      *                     COMMAREA(P913-COMMAREA-PARMS)
      *                     LENGTH(P913-LGTH)
      *                     RESP(WS-RESPONSE)
      *                END-EXEC
      *                MOVE WS-RESPONSE          TO FILE-STATUS
      *                IF NOT SUCCESS
      *                   MOVE 'P1146-3'         TO ERR-PARAGRAPH
      *                   PERFORM P9999-GOT-PROBLEM
      *                END-IF
      *
      *                DO NOT CLEAR OUT THE WSHIST AREA, WE USE THE
      *                INFORMATION FROM THE CALL RECORD AS A BASE, AND
      *                ONLY REPLACE THOSE FIELDS AS NECESSARY.
      *
                       SET P943-GUARANTEE-LOST-FUN  TO TRUE
                       MOVE EMP-NBR OF WS-MSTR TO P943-EMP-NBR
                       MOVE P919-ASGN-ON-DUTY-EMP
                                               TO P943-EMP-NBR-AFFECTED
                       MOVE LAYOFF-CODE        TO P943-LO
                       MOVE CRAFT OF WS-MSTR   TO P943-CRAFT
                       MOVE DIST-REPEAT        TO P943-NA-DIST
                       MOVE SUBDIST-REPEAT     TO P943-NA-SUB-DIST
                       STRING 'EX'
                              TURN-NBR OF WS-EXTRA-BOARD
                              DELIMITED BY SIZE
                              INTO P943-NORM-ASGN-NO
                       MOVE CRAFT-CODE-REPEAT TO P943-NORM-ASGN-CC
                       MOVE NORMAL-ASGNMT     TO P943-NORM-ASGN
                       PERFORM P8900-WRITE-HISTORY
      *
                    END-IF
                 END-IF
                 ADD 1 TO EB-SLOW-POS-TIE-BREAKER-NUM
              ELSE
                 SET DONE TO TRUE
              END-IF
           END-PERFORM.
      *
       P1146-FIND-XB-TURN.
      *                                                          **PLS
           MOVE ZERO TO WS-DONE-CODE
           MOVE ZEROS TO EB-POS-DATE-TIME-TIE
           PERFORM UNTIL DONE
              MOVE EBPOS-AREA TO EBPOS
              EXEC CICS READ
                        DATASET(EB-VIA-CRAFT-POSITION)
                        INTO(WS-EXTRA-BOARD)
                        LENGTH(EBCRPOS-RLGTH)
                        RIDFLD(EBPOS)
                        KEYLENGTH(EBCRPOS-KLGTH)
                        GTEQ
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P1146-1' TO ERR-PARAGRAPH
                 MOVE EBPOS     TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              IF DIST-REPEAT = WORK-XB-DIST
                 AND SUBDIST-REPEAT = WORK-XB-SUB-DIST
                 AND CRAFT-CODE-REPEAT = WORK-XB-CRAFT-CODE
                 AND EBTURN-AREA NOT = WORK-XB-TURN
                 MOVE 'X'                        TO WK-ASGN-JOB-TYPE
                 MOVE DIST-REPEAT                TO WK-ASGN-DIST
                 MOVE SUBDIST-REPEAT             TO WK-ASGN-SUB-DIST
                 MOVE 'EX'                       TO WK-ASGN-POOL
                 MOVE TURN-NBR OF WS-EXTRA-BOARD TO WK-ASGN-TURN
                 MOVE CRAFT-CODE-REPEAT          TO WK-ASGN-CC
                 PERFORM PXXXX-LATEST-TEMP
                 IF ASGN-EMP-NO NOT > ZERO
                    PERFORM PXXXX-JOB-OWNER
                 END-IF
                 IF ASGN-EMP-NO > ZERO
                    MOVE ASGN-EMP-NO             TO MSTRNBRK
                    EXEC CICS READ
                              DATASET(MSTR-VIA-EMP-NBR)
                              INTO(WS-MSTR)
                              LENGTH(MSTRENBR-RLGTH)
                              RIDFLD(MSTRNBRK)
                              KEYLENGTH(MSTRENBR-KLGTH)
                              RESP(WS-RESPONSE)
                    END-EXEC
                    MOVE WS-RESPONSE             TO FILE-STATUS
                    IF NOT SUCCESS
                       MOVE 'P1146-2'            TO ERR-PARAGRAPH
                       MOVE MSTRNBRK             TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                    IF NOT (AVAILABLE OR WORKING)
      *                MOVE SPACES               TO P913-COMMAREA-PARMS
      *                IF DEC-RETAIN-POS
      *                   SET P913-RETAIN-POSITION TO TRUE
      *                END-IF
      *                SET P913-CALL-FUNCTION      TO TRUE
      *                MOVE EBTURN-AREA          TO P913-TURN-PARM
      *                MOVE P919-EFF-DATE-TIME   TO P913-EFF-DATE-TIME
      *                MOVE P919-TIME-ZONE       TO P913-TIME-ZONE
      *                MOVE P919-TRAIN-DATE-TIME TO P913-TRAIN-DATE-TIME
      *                MOVE P919-ASGN-ASGN       TO JOB-DEF-CHECK
      *-------------------------------------------------------------
      *       FOR FASTSLOW SPAREBOARDS THE 'FROM-BOARD' FLAG MAY BE
      *       SET IN PRIOR PROCESSING TO DETERMINE ROAD VS. YARD.
      *----------------------------------------------------------PLS
      *                IF P919-FROM-FAST-BOARD
      *                   SET P913-YARD-CALL   TO TRUE
      *                ELSE
      *                   IF P919-FROM-SLOW-BOARD
      *                      SET P913-ROAD-CALL   TO TRUE
      *                   ELSE
      *                      IF P919-ASGN-UFP
      *                         OR JOB-DEF-LOCAL-ASGN
      *                         SET P913-ROAD-CALL     TO TRUE
      *                      ELSE
      *                         SET P913-YARD-CALL     TO TRUE
      *                      END-IF
      *                   END-IF
      *                END-IF
      *                EXEC CICS LINK
      *                     PROGRAM(P913-PGM)
      *                     COMMAREA(P913-COMMAREA-PARMS)
      *                     LENGTH(P913-LGTH)
      *                     RESP(WS-RESPONSE)
      *                END-EXEC
      *                MOVE WS-RESPONSE          TO FILE-STATUS
      *                IF NOT SUCCESS
      *                   MOVE 'P1146-3'         TO ERR-PARAGRAPH
      *                   PERFORM P9999-GOT-PROBLEM
      *                END-IF
      *
      *                DO NOT CLEAR OUT THE WSHIST AREA, WE USE THE
      *                INFORMATION FROM THE CALL RECORD AS A BASE, AND
      *                ONLY REPLACE THOSE FIELDS AS NECESSARY.
      *
                       SET P943-GUARANTEE-LOST-FUN  TO TRUE
                       MOVE EMP-NBR OF WS-MSTR TO P943-EMP-NBR
                       MOVE P919-ASGN-ON-DUTY-EMP
                                               TO P943-EMP-NBR-AFFECTED
                       MOVE LAYOFF-CODE        TO P943-LO
                       MOVE CRAFT OF WS-MSTR   TO P943-CRAFT
                       MOVE DIST-REPEAT        TO P943-NA-DIST
                       MOVE SUBDIST-REPEAT     TO P943-NA-SUB-DIST
                       STRING 'EX'
                              TURN-NBR OF WS-EXTRA-BOARD
                              DELIMITED BY SIZE
                              INTO P943-NORM-ASGN-NO
                       MOVE CRAFT-CODE-REPEAT TO P943-NORM-ASGN-CC
                       MOVE NORMAL-ASGNMT     TO P943-NORM-ASGN
                       PERFORM P8900-WRITE-HISTORY
      *
                    END-IF
                 END-IF
                 ADD 1 TO EB-POS-TIE-BREAKER-NUM
              ELSE
                 SET DONE TO TRUE
              END-IF
           END-PERFORM.
      *
       P1150-UPDATE-ASGN-HIST.
      *
           MOVE SPACES                       TO P926-COMMAREA-PARMS
           SET P926-CALL-FUNCTION            TO TRUE
           MOVE P919-CALL-DIST               TO P926-AH-DIST
           MOVE P919-CALL-SUB-DIST           TO P926-AH-SUB-DIST
           MOVE P919-CALL-POOL               TO P926-AH-POOL
           MOVE P919-TRAIN-DATE-TIME         TO P926-AH-EFF-DATE-TIME
           MOVE P919-TIME-ZONE               TO P926-TIME-ZONE
           MOVE P919-TRAIN                   TO P926-AH-TRAIN
           MOVE P919-ASGN-PROFILE-KEY        TO P926-ASGN-PROFILE-KEY
           MOVE P919-ASGN-TYPE               TO P926-ASGN-TYPE
           MOVE P919-ASGN-PARM               TO P926-ASGN-PARM
           MOVE P919-JOB-TYPE                TO P926-CALL-JOB-TYPE
      *NJB MOVE P919-INTERNAL-ORDER          TO P926-CALL-INTERNAL-ORDER
           MOVE P919-RUN-NUMBER              TO P926-CALL-RUN-NUMBER
           MOVE P919-FORCE-ROUTE             TO P926-CALL-FORCE-ROUTE
           MOVE P919-BUSINESS-AREA           TO P926-CALL-BUSINESS-AREA
           MOVE P919-CREW-RPT-LOCATION       TO
                                               P926-CREW-RPT-LOCATION
           MOVE P919-ALLOW-VCLL-FL           TO P926-ALLOW-VCLL-FL
      *CNC0520-BEG
           MOVE P919-ROUTE-DESIGNATION       TO P926-CALL-ROUTE-DSGN
      *CNC0520-END
      *CNC0534-BEG
           MOVE P919-CALLED-FROM-HOME-FL     TO P926-CALLED-FROM-HOME-FL
      *CNC0534-END
           MOVE P919-DEP-DIRECTION           TO P926-DEP-DIRECTION
           MOVE P919-CALL-COMMENTS           TO P926-CALL-COMMENTS
           IF P919-ASGN-UFP
              MOVE DIST OF WS-UFP            TO P926-OD-DIST
              MOVE SUB-DIST OF WS-UFP        TO P926-OD-SUB-DIST
              MOVE POOL-NAME OF WS-UFP       TO P926-OD-POOL
              MOVE IN-OUT-TERMINAL           TO P926-OD-TERMINAL
           ELSE
              MOVE AJ-JOB-DIST               TO P926-OD-DIST
              MOVE AJ-JOB-SUB-DIST           TO P926-OD-SUB-DIST
              IF AJ-JOB-ASGN-ID(1:2) = 'EX'
                 MOVE P919-ASGN-PROFILE-KEY  TO WS-PROFILE-KEY
                 IF WS-LOCAL-REC
                    MOVE JOB-DEF-LOCAL-POOL  TO P926-OD-POOL
                 ELSE
                    MOVE JOB-DEF-YARD-POOL   TO P926-OD-POOL
                 END-IF
              ELSE
      *CNC0562 - BEG
                 MOVE P919-CALL-POOL         TO P926-OD-POOL
      *          MOVE AJ-JOB-ASGN-ID         TO JOB-DEF-CHECK
      *          IF JOB-DEF-LOCAL-ASGN
      *             MOVE JOB-DEF-LOCAL-POOL  TO P926-OD-POOL
      *          ELSE
      *             MOVE JOB-DEF-YARD-POOL   TO P926-OD-POOL
      *          END-IF
      *CNC0562 - END
              END-IF
              MOVE ZEROS                     TO P926-OD-TERMINAL
           END-IF
           MOVE P919-ASGN-OWNER              TO P926-ASGN-OWNER
           MOVE P919-ASGN-TEMP-EMP           TO P926-ASGN-TEMP-EMP
           MOVE P919-ASGN-ON-DUTY-EMP        TO P926-ASGN-ON-DUTY-EMP
           MOVE P919-EFF-DATE-TIME           TO P926-EMP-OD-DATE-TIME
           MOVE P919-CALL-FUNC-CODE          TO P926-CALL-FUNC-CODE
           IF P919-DECKEY > SPACES
              MOVE P919-DEC-RULE-NO          TO P926-DEC-RULE-NO
           END-IF
           MOVE P919-EMP-REST-STATUS         TO P926-EMP-REST-STATUS
           MOVE P919-US-REST-STATUS          TO P926-US-REST-STATUS
           IF P925-CALC-PENALTY
              MOVE P925-PENALTY              TO P926-EMP-PENALTY
              MOVE P925-HELDAWAY-MISC-CODE   TO P926-HELDAWAY-MISC-CODE
              MOVE P925-HELDAWAY-CALC        TO P926-HELDAWAY-CALC
              MOVE P925-HELDAWAY-HOURS       TO P926-HELDAWAY-HOURS
              MOVE P925-HELDAWAY-EXTRN-HRS   TO P926-HELDAWAY-EXTRN-HRS
              MOVE P925-HAHT-ELAPSED-TIME    TO P926-HAHT-ELAPSED-TIME
           END-IF
           MOVE P919-CALL-CC                 TO P926-CALL-CC
           MOVE P919-SCHED-KEY               TO P926-CALL-SCHED-KEY
           MOVE P919-CALL-CATCHUP-DATE       TO P926-CALL-CATCHUP-DATE
           MOVE P919-DH-RSN                  TO P926-CALL-DH-RSN
      *    BUGC329 - FLW, 11/27/96, START
           MOVE P919-BYPASS-POINTER-FLAG     TO P926-BYPASS-POINTER-FLAG
      *    BUGC329 - FLW, 11/27/96, END
           MOVE P919-SCHED-END-TIME          TO P926-SCHED-END-TIME
           MOVE P919-ALT-EN-TK-PROFILE       TO P926-ALT-EN-TK-PROFILE
           MOVE P919-ALT-TR-TK-PROFILE       TO P926-ALT-TR-TK-PROFILE
           MOVE P919-SIDE-TRIP               TO P926-SIDE-TRIP
           MOVE P919-RJHS-EMPNO              TO P926-CALL-RJHS-EMPNO
           MOVE P919-TAXI-TIME               TO P926-CALL-TAXI-TIME
           EXEC CICS LINK
                     PROGRAM(P926-PGM)
                     COMMAREA(P926-COMMAREA-PARMS)
                     LENGTH(P926-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P1150-1' TO ERR-PARAGRAPH
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
       P1160-REMOVE-I-TIME.
      *
           PERFORM VARYING SUB2 FROM 1 BY 1
                   UNTIL SUB2 > ARRAY-MAX
              IF DEC-QUAL(SUB2) > SPACES
                 MOVE SPACES             TO CNTLKEY-AREA
                 MOVE '12'               TO CNTL-REC-TYPE
                 MOVE DEC-QUAL(SUB2)     TO CNTL-QUAL-CODE
                 MOVE CNTLKEY-AREA       TO WORK-CNTLKEY
                 PERFORM P8640-READ-CNTLFILE
                 IF SUCCESS
                    IF CNTL-ONE-TYPE-ONLY
                       MOVE SPACES                TO WORK-QUALEMP-KEY
                       MOVE P919-ASGN-ON-DUTY-EMP TO WK-QUAL-EMP-NO
                       MOVE DEC-QUAL(SUB2)        TO WK-QUALIFICATION
                       PERFORM P8650-DELETE-QUALIFICATION
                       IF NOT SUCCESS
                          IF NOT (NO-RECORD-FND OR END-OF-FILE)
                             MOVE 'P1160-1'       TO ERR-PARAGRAPH
                             MOVE QUALEMP-KEY     TO ERR-KEY
                             PERFORM P9999-GOT-PROBLEM
                          END-IF
                       END-IF
                    END-IF
                 ELSE
                    IF NOT (NO-RECORD-FND OR END-OF-FILE)
                       MOVE 'P1160-2'  TO ERR-PARAGRAPH
                       MOVE CNTLKEY    TO ERR-KEY
                       PERFORM P9999-GOT-PROBLEM
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.
      *
      *CNLD-133-B
       P1200-DH-CALL-FUNCTION.
      *

      *    RJA, 5/19/2023  B
      *
      *    IF P919-ASGN-UFP
      *       IF  DIST      OF WS-UFP = DIST2
      *       AND SUB-DIST  OF WS-UFP = SUB-DIST2
      *       AND POOL-NAME OF WS-UFP = POOL-NAME2
      *       AND IN-TOWN
      *          SET CALLED-AT-HOME-TERM  TO TRUE
      *       ELSE
      *          SET CALLED-AT-AWAY-TERM  TO TRUE
      *       END-IF
      *    ELSE
      *       SET CALLED-AT-HOME-TERM     TO TRUE
      *    END-IF

           IF P919-CALLED-FROM-HOME
              SET CALLED-AT-HOME-TERM  TO TRUE
           ELSE
              SET CALLED-AT-AWAY-TERM  TO TRUE
           END-IF
      *    RJA, 5/19/2023  E

      * USING CALLED-AT-HOME-TERM CHECK WITHIN P1130
      *    IF WS-CAN-WRR-NEW AND CALLED-AT-HOME-TERM
           IF WS-CAN-WRR-NEW
      *
              PERFORM P1130-CHECK-NATURAL-RESET-BRK
           END-IF.
      *CNLD-133-E
      ****  SRP, 3/29/1996, START
       P2700-SPLIT-VAC-SCHEDULE.
      * PLEASE NOTE - THIS CODE ALSO EXISTS IN 917, ANY CHANGES
      * MADE HERE MUST ALSO BE MADE THERE
      *
           MOVE SPACES                    TO P977-COMMAREA-PARMS
           SET P977-SCHEDULE              TO TRUE
           SET P977-SPLIT-FUNCTION        TO TRUE
           MOVE EMP-NBR OF WS-MSTR        TO P977-EMP-NO
      *    IF THE EMPLOYEE IS BEING MARKED UP AFTER 2000PM
      *    CONSIDER IT TO BEGIN THE NEXT DAY
      *
           IF P919-EFF-TIME > 1959
              MOVE ZEROS                    TO DATE-CONVERSION-PARMS
              SET PARM-ADD                  TO TRUE
              MOVE P919-EFF-DATE            TO PARM-PRI-DATE-GREG
              MOVE '000001'                 TO PARM-SEC-DATE-GREG
              EXEC CICS LINK
                        PROGRAM(P903-PGM)
                        COMMAREA(DATE-CONVERSION-PARMS)
                        LENGTH(P903-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE              TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P2700-1'             TO ERR-PARAGRAPH
                 MOVE 'P903LINK'            TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
              MOVE PARM-RES-DATE-GREG       TO P977-MID-DATE
           ELSE
              MOVE P919-EFF-DATE            TO P977-MID-DATE
           END-IF
      *
           IF P977-MID-YR > '90'
              MOVE '19'                   TO P977-MID-CENT
           ELSE
              MOVE '20'                   TO P977-MID-CENT
           END-IF
           PERFORM P8700-LINK-TO-P977
      *
      *    THE SPLIT FUNCTION WILL RETURN THE END DATE OF THE
      *    ORIGINAL SCHEDULE IN P977-END-CENT-DATE. ALL WE
      *    NEED TO DO IS RESET THE FLAG ON THE UNTAKEN LAST PART
      *    OF THE SCHEDULE. BECAUSE ALL OF THE KEY INFORMATION IS
      *    ALREADY IN THE COMMAREA, ALL WE NEED TO DO IS RESET
      *    THE FUNCTION CODE AND RE-CALL 977.
      *
           SET P977-UNTAKE-FUNCTION       TO TRUE
           PERFORM P8700-LINK-TO-P977.
      *
      * AV START
       P8000-READ-TRCN.
      ******************************************************************
           MOVE P919-ASGN-PROFILE-KEY TO TRCNKEY1
           EXEC CICS READ
                     DATASET(TRAIN-CN-VIA-TRCNKEY1)
                     INTO(WS-TRCN-FILE)
                     LENGTH(TRAINCN-RLGTH)
                     RIDFLD(TRCNKEY1)
                     KEYLENGTH(TRAINCN-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS.
      *
      ******************************************************************
       P8020-READ-POINTER-REC-GTEQ.
      ******************************************************************
           EXEC CICS READ
                     GTEQ
                     DATASET(POINT-FILE-VIA-EMP)
                     INTO(WS-POINTER)
                     LENGTH(POINT-RLGTH)
                     RIDFLD(POINTKEY)
                     KEYLENGTH(POINT-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE                TO FILE-STATUS.
      ******************************************************************
       P8200-READ-AHIST.
      ******************************************************************
           EXEC CICS READ
                     DATASET(AH-DATASET)
                     INTO(WS-AHIST)
                     LENGTH(AH1-RLGTH)
                     RIDFLD(AH1KEY)
                     KEYLENGTH(AH1-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8500-READ-MASTER.
      ******************************************************************
           EXEC CICS READ
                     DATASET(MSTR-VIA-EMP-NBR)
                     INTO(WS-MSTR)
                     LENGTH(MSTRENBR-RLGTH)
                     RIDFLD(MSTRNBRK)
                     KEYLENGTH(MSTRENBR-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF SUCCESS
              PERFORM P8510-READ-MASTER-JOBS
           ELSE
              MOVE 'P8500' TO ERR-PARAGRAPH
              MOVE MSTRNBRK TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      ******************************************************************
       P8510-READ-MASTER-JOBS.
      ******************************************************************
           MOVE SPACES                 TO WS-ASGN-FILE
           MOVE EMP-NBR OF WS-MSTR     TO WK-ASGN-EMP-NO
           PERFORM PXXXX-JOB-OWNED
           MOVE ASGN-JOB-TYPE          TO NORMAL-ASGNMT-FLAG
           MOVE ASGN-ASSIGNMENT        TO NORMAL-ASGNMT
           MOVE SPACES                 TO WS-ASGN-FILE
           PERFORM PXXXX-LATEST-TEMP-JOB
           MOVE ASGN-JOB-TYPE          TO TEMPORARY-ASGNMT-FLAG
           MOVE SPACES                 TO TEMP-ASGN-XB-AUG-FLAG
           IF ASGN-JOB-TYPE = 'X'
              AND AUGMENTED-TO-EXTRA-BOARD
              SET TEMP-ASGN-XB-AUG    TO TRUE
           END-IF
           MOVE ASGN-ASSIGNMENT        TO TEMPORARY-ASGNMT
           MOVE SPACES                 TO WS-ASGN-FILE
           PERFORM PXXXX-JOB-ON-DUTY
           MOVE ASGN-JOB-TYPE          TO ON-DUTY-ASGNMT-FLAG
           MOVE ASGN-ASSIGNMENT        TO ON-DUTY-ASGNMT
           MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE.
      *
      ******************************************************************
       P8520-READ-MASTER-UPDATE.
      ******************************************************************
           EXEC CICS READ
                     UPDATE
                     DATASET(MSTR-VIA-EMP-NBR)
                     INTO(WS-MSTR)
                     LENGTH(MSTRENBR-RLGTH)
                     RIDFLD(MSTRNBRK)
                     KEYLENGTH(MSTRENBR-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8521-READ-TELE-UPDATE.
      ******************************************************************
           EXEC CICS READ
                     UPDATE
                     DATASET(TELE-VIA-EMP-NBR)
                     INTO(WS-TELE-FILE)
                     LENGTH(TELEENBR-RLGTH)
                     RIDFLD(TELENBRK)
                     KEYLENGTH(TELEENBR-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE                   TO FILE-STATUS
           .
      *
      ******************************************************************
       P8550-REWRITE-MASTER.
      ******************************************************************
           EXEC CICS REWRITE
                     DATASET(MSTR-VIA-EMP-NBR)
                     FROM(WS-MSTR)
                     LENGTH(MSTRENBR-RLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8550'  TO ERR-PARAGRAPH
              MOVE MSTRNBRK TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      ******************************************************************
       P8551-REWRITE-TELE.
      ******************************************************************
           EXEC CICS REWRITE
                     DATASET(TELE-VIA-EMP-NBR)
                     FROM(WS-TELE-FILE)
                     LENGTH(TELEENBR-RLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE                   TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8551'                    TO ERR-PARAGRAPH
              MOVE TELENBRK                   TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           .
      *
      *CNC0600-B
      ******************************************************************
       P8560-READ-MSTR3.
      ******************************************************************

           EXEC CICS READ
                     DATASET(MSTR3-VIA-EMP-NBR)
                     INTO(WS-MSTR3)
                     LENGTH(MSTR3ENBR-RLGTH)
                     RIDFLD(MSTR3NBRK)
                     KEYLENGTH(MSTR3ENBR-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT SUCCESS
              IF NOT (NO-RECORD-FND OR END-OF-FILE)
                 MOVE 'P8560-1'       TO ERR-PARAGRAPH
                 MOVE MSTR3NBRK       TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           ELSE
              SET WS-MSTR3-FOUND      TO TRUE
           END-IF
           .
      *=================================================================
       P8561-WRITE-MSTR3.
      *=================================================================
           MOVE P919-ASGN-ON-DUTY-EMP TO MSTR3-EMP-NBR
      *CNLD-133-B
                                         MSTR3NBRK
      *CNLD-133-E
           EXEC CICS WRITE
                     DATASET(MSTR3-VIA-EMP-NBR)
                     FROM(WS-MSTR3)
                     LENGTH(MSTR3ENBR-RLGTH)
                     RIDFLD(MSTR3NBRK)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8561-1'          TO ERR-PARAGRAPH
              MOVE MSTR3NBRK          TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *=================================================================
       P8562-READ-MASTER3-UPD.
      *=================================================================
           MOVE MSTR3-EMP-NBR         TO MSTR3NBRK
           EXEC CICS READ
                     UPDATE
                     DATASET(MSTR3-VIA-EMP-NBR)
                     INTO(WS-MSTR3)
                     LENGTH(MSTR3ENBR-RLGTH)
                     RIDFLD(MSTR3NBRK)
                     KEYLENGTH(MSTR3ENBR-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT SUCCESS AND NOT(NO-RECORD-FND OR END-OF-FILE)
              MOVE 'P8562-1'          TO ERR-PARAGRAPH
              MOVE MSTR3NBRK          TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *=================================================================
       P8563-MASTER3-REWRITE.
      *=================================================================
           MOVE MSTR3-EMP-NBR         TO MSTR3NBRK
           EXEC CICS REWRITE
                     DATASET(MSTR3-VIA-EMP-NBR)
                     FROM(WS-MSTR3)
                     LENGTH(MSTR3ENBR-RLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8563-1'          TO ERR-PARAGRAPH
              MOVE MSTR3NBRK          TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      *=================================================================
       P8570-EMPTOD-STARTBR.
      *=================================================================
           EXEC CICS STARTBR
                     DATASET(EMPTOD-VIA-EMP)
                     RIDFLD(EMPTOD-EMPKEY)
                     GTEQ
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8570-1'          TO ERR-PARAGRAPH
              MOVE EMPTOD-EMPKEY      TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           .
      *=================================================================
       P8580-EMPTOD-READNEXT.
      *=================================================================
           EXEC CICS READNEXT
                     DATASET(EMPTOD-VIA-EMP)
                     INTO(WS-EMP-TOD)
                     LENGTH(EMPTOD-EMP-RLGTH)
                     RIDFLD(EMPTOD-EMPKEY)
                     KEYLENGTH(EMPTOD-EMP-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT (SUCCESS OR NO-RECORD-FND OR END-OF-FILE)
              MOVE 'P8580-1'          TO ERR-PARAGRAPH
              MOVE EMPTOD-EMPKEY      TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           .
      *=================================================================
       P8590-EMPTOD-READPREV.
      *=================================================================
           EXEC CICS READPREV
                     DATASET(EMPTOD-VIA-EMP)
                     INTO(WS-EMP-TOD)
                     LENGTH(EMPTOD-EMP-RLGTH)
                     RIDFLD(EMPTOD-EMPKEY)
                     KEYLENGTH(EMPTOD-EMP-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           .
      *=================================================================
       P8600-EMPTOD-ENDBR.
      *=================================================================
           EXEC CICS ENDBR
                     DATASET(EMPTOD-VIA-EMP)
                     RESP(WS-RESPONSE)
           END-EXEC
           .
      *CNC0600-E
      ******************************************************************
       P8600-READ-UFPTURN.
      ******************************************************************
           EXEC CICS READ
                     DATASET(UFP-VIA-TURN-NBR)
                     INTO(WS-UFP)
                     LENGTH(UFPTURN-RLGTH)
                     RIDFLD(UFPTURN)
                     KEYLENGTH(UFPTURN-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8650-DELETE-QUALIFICATION.
      ******************************************************************
           MOVE WORK-QUALEMP-KEY TO QUALEMP-KEY
           EXEC CICS DELETE
                     DATASET(QUAL-FILE-VIA-QUALEMP)
                     RIDFLD(QUALEMP-KEY)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8600-READ-AJJOBKEY.
      ******************************************************************
           EXEC CICS READ
                     DATASET(AJ-VIA-JNAME-JCRAFT)
                     INTO(WS-ASGNED-JOBS)
                     LENGTH(AJNAMECR-RLGTH)
                     RIDFLD(AJJOBKEY)
                     KEYLENGTH(AJNAMECR-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8610-READ-UFPTURN-UPDATE.
      ******************************************************************
           EXEC CICS READ
                     UPDATE
                     DATASET(UFP-VIA-TURN-NBR)
                     INTO(WS-UFP)
                     LENGTH(UFPTURN-RLGTH)
                     RIDFLD(UFPTURN)
                     KEYLENGTH(UFPTURN-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8620-WRITE-UFPTURN.
      ******************************************************************
           MOVE UFPTURN-AREA TO UFPTURN
           EXEC CICS WRITE
                     DATASET(UFP-VIA-TURN-NBR)
                     FROM(WS-UFP)
                     LENGTH(UFPTURN-RLGTH)
                     RIDFLD(UFPTURN)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8620'      TO ERR-PARAGRAPH
              MOVE UFPTURN-AREA TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      ******************************************************************
       P8630-REWRITE-UFPTURN.
      ******************************************************************
           EXEC CICS REWRITE
                     DATASET(UFP-VIA-TURN-NBR)
                     FROM(WS-UFP)
                     LENGTH(UFPTURN-RLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8630'      TO ERR-PARAGRAPH
              MOVE UFPTURN-AREA TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      ******************************************************************
       P8640-READ-CNTLFILE.
      ******************************************************************
           MOVE WORK-CNTLKEY TO CNTLKEY
           EXEC CICS READ
                     DATASET(CNTL-FILE-VIA-CNTLKEY)
                     INTO(WS-CNTL-FILE)
                     LENGTH(CNTLFILE-RLGTH)
                     RIDFLD(CNTLKEY)
                     KEYLENGTH(CNTLFILE-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS.
      *
      ******************************************************************
       P8700-LINK-TO-P977.
      *
           EXEC CICS LINK
                     PROGRAM(P977-PGM)
                     COMMAREA(P977-COMMAREA-PARMS)
                     LENGTH(P977-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE        TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8700-1'       TO ERR-PARAGRAPH
              MOVE 'P977-LINK'     TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *    IF P977-ERROR-FOUND
      *       MOVE P977-MSGLOG-CODE       TO MSGLOG-CODE
      *       SET P919-DATE-ERROR         TO TRUE
      *    END-IF.
      *
      *
       P8800-WRITE-TASK.
      ******************************************************************
           EXEC CICS LINK
                     PROGRAM(P927-PGM)
                     COMMAREA(P927-COMMAREA-PARMS)
                     LENGTH(P927-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8800-1' TO ERR-PARAGRAPH
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      ******************************************************************
       P8900-WRITE-HISTORY.
      ******************************************************************
      *
           SET P943-EMPLOYEE-FUNCTION      TO TRUE
           EXEC CICS ASSIGN
                     USERID(P943-USERID)
           END-EXEC

           EXEC CICS LINK
                     PROGRAM(P943-PGM)
                     COMMAREA(P943-COMMAREA-PARMS)
                     LENGTH(P943-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE                TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8900-1'               TO ERR-PARAGRAPH
              MOVE 'P943'                  TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           IF P943-RETURN-STATUS > ZEROES
              MOVE 'P8900-2'               TO ERR-PARAGRAPH
              MOVE P943-RETURN-STATUS      TO FILE-STATUS
              MOVE 'P943-RETURN'           TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
      ******************************************************************
       P8994-CONVERT-HIST-TIMEZONE.
      ******************************************************************
           IF P919-ASGN-UFP
              MOVE DIST OF WS-UFP        TO P943-DIST
              MOVE SUB-DIST OF WS-UFP    TO P943-SDIST
           ELSE
              MOVE AJ-JOB-DIST           TO P943-DIST
              MOVE AJ-JOB-SUB-DIST       TO P943-SDIST
           END-IF
      *
           MOVE SPACES                   TO WORK-CNTLKEY
           MOVE '02'                     TO WK-CNTL-REC-TYPE
           MOVE P943-DIST                TO WK-CNTL-DIST
           MOVE P943-SDIST               TO WK-CNTL-SUB-DIST
           MOVE WORK-CNTLKEY             TO CNTLKEY
           EXEC CICS READ
                     DATASET(CNTL-FILE-VIA-CNTLKEY)
                     INTO(WS-CNTL-FILE)
                     LENGTH(CNTLFILE-RLGTH)
                     RIDFLD(CNTLKEY)
                     KEYLENGTH(CNTLFILE-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE  TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P8994'   TO ERR-PARAGRAPH
              MOVE CNTLKEY   TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF
           MOVE SPACES                   TO TZ-PARAMETERS
           MOVE CNTL-TIME-ZONE           TO TZ-OUT-ZONE
           MOVE WS-EMP-EFF-DATE-TIME     TO TZ-IN-DATE-TIME
           MOVE WS-EMP-TIME-ZONE         TO TZ-IN-ZONE
           PERFORM P8996-TIMEZONE
           IF TZ-INVALID-PARAMETERS
              MOVE 'P8994TZ'             TO ERR-PARAGRAPH
              PERFORM P8996-TZERROR
           END-IF
           MOVE TZ-OUT-DATE-TIME         TO P943-EFF-DATE-TIME.
      *
       COPY TIMEZONE.
      *
       COPY TZERROR.
      *
       COPY DATEEDIT.
      *
       COPY TIMEEDIT.
      ******************************************************************
       PXXXX-JOB-OWNER.
      ******************************************************************
           MOVE WORK-ASGNKEY1 TO ASGNKEY1
           SET ASGN-OWNER-REC TO TRUE
           MOVE ZERO          TO ASGN-DATE-TIME
           MOVE ASGNKEY1      TO ASGNJOB
           EXEC CICS READ
                     DATASET(ASGN-VIA-ASGNJOB)
                     INTO(ASGN-AREA)
                     LENGTH(ASGNJOB-RLGTH)
                     RIDFLD(ASGNJOB)
                     KEYLENGTH(ASGNJOB-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE ZEROS TO ASGN-EMP-NO
           END-IF.
      *
      ******************************************************************
       PXXXX-LATEST-TEMP.
      ******************************************************************
           MOVE SPACES        TO WS-SAVE-ASGN-FILE
           MOVE WORK-ASGNKEY1 TO ASGNKEY1
           SET ASGN-TEMP-REC  TO TRUE
           MOVE ZERO          TO ASGN-DATE-TIME
           MOVE ASGNKEY1      TO ASGNJOB
           EXEC CICS STARTBR
                     DATASET(ASGN-VIA-ASGNJOB)
                     RIDFLD(ASGNJOB)
                     GTEQ
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF SUCCESS
              MOVE 'N' TO WS-ASGN-DONE-CODE
              PERFORM UNTIL ASGN-DONE
                 EXEC CICS READNEXT
                           DATASET(ASGN-VIA-ASGNJOB)
                           INTO(ASGN-AREA)
                           LENGTH(ASGNJOB-RLGTH)
                           RIDFLD(ASGNJOB)
                           KEYLENGTH(ASGNJOB-KLGTH)
                           RESP(WS-RESPONSE)
                 END-EXEC
                 MOVE WS-RESPONSE TO FILE-STATUS
                 IF SUCCESS
                    IF WK-ASGN-DIST = ASGN-DIST
                       AND WK-ASGN-SUB-DIST = ASGN-SUB-DIST
                       AND WK-ASGN-ASSIGN = ASGN-AJ-JOB
                           OF ASGN-ASSIGNMENT
                       AND ASGN-TEMP-REC
                       MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE
                    ELSE
                       SET ASGN-DONE TO TRUE
                    END-IF
                 ELSE
                    SET ASGN-DONE TO TRUE
                 END-IF
              END-PERFORM
              EXEC CICS ENDBR
                        DATASET(ASGN-VIA-ASGNJOB)
                        RESP(WS-RESPONSE)
              END-EXEC
           END-IF
           IF WS-SAVE-ASGN-FILE > SPACE
              MOVE WS-SAVE-ASGN-FILE TO ASGN-AREA
           ELSE
              MOVE ZEROS TO ASGN-EMP-NO
           END-IF.
      *
      ******************************************************************
       PXXXX-ON-DUTY-EMP.
      ******************************************************************
           MOVE WORK-ASGNKEY1   TO ASGNKEY1
           SET ASGN-ON-DUTY-REC TO TRUE
           MOVE ZERO            TO ASGN-DATE-TIME
           MOVE ASGNKEY1        TO ASGNJOB
           EXEC CICS READ
                     DATASET(ASGN-VIA-ASGNJOB)
                     INTO(ASGN-AREA)
                     LENGTH(ASGNJOB-RLGTH)
                     RIDFLD(ASGNJOB)
                     KEYLENGTH(ASGNJOB-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE ZEROS TO ASGN-EMP-NO
           END-IF.
      *
      ******************************************************************
       PXXXX-JOB-OWNED.
      ******************************************************************
           MOVE WORK-ASGNKEY2 TO ASGNKEY2
           MOVE '1'           TO ASGN-EMP-NO-REC-TYPE
           MOVE ZERO          TO ASGN-EMP-DATE-TIME
           MOVE ASGNKEY2      TO ASGNEMP
           EXEC CICS READ
                     DATASET(ASGN-VIA-ASGNEMP)
                     INTO(WS-ASGN-FILE)
                     LENGTH(ASGNEMP-RLGTH)
                     RIDFLD(ASGNEMP)
                     KEYLENGTH(ASGNEMP-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE SPACES TO WS-ASGN-FILE
           END-IF.
      *
      ******************************************************************
       PXXXX-LATEST-TEMP-JOB.
      ******************************************************************
           MOVE WORK-ASGNKEY2 TO ASGNKEY2
           MOVE '2'           TO ASGN-EMP-NO-REC-TYPE
           MOVE ZERO          TO ASGN-EMP-DATE-TIME
           MOVE ASGNKEY2      TO ASGNEMP
           MOVE SPACES        TO WS-ASGN-FILE
                                 WS-SAVE-ASGN-FILE
           EXEC CICS STARTBR
                     DATASET(ASGN-VIA-ASGNEMP)
                     RIDFLD(ASGNEMP)
                     GTEQ
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF SUCCESS
              MOVE 'N' TO WS-ASGN-DONE-CODE
              PERFORM UNTIL ASGN-DONE
                 EXEC CICS READNEXT
                           DATASET(ASGN-VIA-ASGNEMP)
                           INTO(ASGN-AREA)
                           LENGTH(ASGNEMP-RLGTH)
                           RIDFLD(ASGNEMP)
                           KEYLENGTH(ASGNEMP-KLGTH)
                           RESP(WS-RESPONSE)
                 END-EXEC
                 MOVE WS-RESPONSE TO FILE-STATUS
                 IF SUCCESS
                    IF ASGN-EMP-NO = WK-ASGN-EMP-NO
                       AND ASGN-EMP-NO-REC-TYPE = '2'
                       MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE
                    ELSE
                       SET ASGN-DONE TO TRUE
                    END-IF
                 ELSE
                    SET ASGN-DONE TO TRUE
                 END-IF
              END-PERFORM
              EXEC CICS ENDBR
                        DATASET(ASGN-VIA-ASGNEMP)
                        RESP(WS-RESPONSE)
              END-EXEC
           END-IF
           IF WS-SAVE-ASGN-FILE > SPACES
              MOVE WS-SAVE-ASGN-FILE TO WS-ASGN-FILE
           ELSE
              MOVE SPACES TO WS-ASGN-FILE
           END-IF.
      *
      ******************************************************************
       PXXXX-JOB-ON-DUTY.
      ******************************************************************
           MOVE WORK-ASGNKEY2 TO ASGNKEY2
           MOVE '3' TO ASGN-EMP-NO-REC-TYPE
           MOVE ZERO TO ASGN-EMP-DATE-TIME
           MOVE ASGNKEY2 TO ASGNEMP
           EXEC CICS READ
                     DATASET(ASGN-VIA-ASGNEMP)
                     INTO(ASGN-AREA)
                     LENGTH(ASGNEMP-RLGTH)
                     RIDFLD(ASGNEMP)
                     KEYLENGTH(ASGNEMP-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              MOVE SPACES TO WS-ASGN-FILE
           END-IF.
      *
      ******************************************************************
       P8990-SEND-TO-EM.
      ******************************************************************
           IF EMP-NBR      OF WS-MSTR NUMERIC
              MOVE EMP-NBR OF WS-MSTR TO WS-EMP-NUM
              MOVE WS-EMP-NUM         TO P929-TO-EMP-NUM
              SET P929-TO-EM          TO TRUE
              EXEC CICS LINK
                        PROGRAM(P929-PGM)
                        COMMAREA(P929-COMMAREA-PARMS)
                        LENGTH(P929-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P8990-1' TO ERR-PARAGRAPH
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF.
      *
      ******************************************************************
       P9000-RETURN.
      ******************************************************************
           MOVE P919-COMMAREA-PARMS TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.
      *
      ******************************************************************
       P9800-GET-TIME-OFFSET.
      ******************************************************************
           MOVE SPACES                TO CNTLKEY-AREA
           SET DATE-TIME-OFFSET-REC   TO TRUE
           EXEC CICS ASSIGN
                     USERID(CNTL-DT-USERID)
           END-EXEC
           MOVE CNTLKEY-AREA          TO CNTLKEY
           EXEC CICS READ
                     DATASET(CNTL-FILE-VIA-CNTLKEY)
                     INTO(WS-CNTL-FILE)
                     LENGTH(CNTLFILE-RLGTH)
                     RIDFLD(CNTLKEY)
                     KEYLENGTH(CNTLFILE-KLGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE TO FILE-STATUS
           IF NOT SUCCESS
              IF (NO-RECORD-FND OR END-OF-FILE)
                 MOVE '********'      TO CNTL-DT-USERID
                 MOVE CNTLKEY-AREA    TO CNTLKEY
                 EXEC CICS READ
                           DATASET(CNTL-FILE-VIA-CNTLKEY)
                           INTO(WS-CNTL-FILE)
                           LENGTH(CNTLFILE-RLGTH)
                           RIDFLD(CNTLKEY)
                           KEYLENGTH(CNTLFILE-KLGTH)
                           RESP(WS-RESPONSE)
                 END-EXEC
                 MOVE WS-RESPONSE TO FILE-STATUS
              ELSE
                 MOVE 'P9800-1'       TO ERR-PARAGRAPH
                 MOVE CNTLKEY         TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF
           IF SUCCESS
              MOVE CNTL-DT-ADD-SUBTRACT
                                      TO WS-DT-OS-FUN
              MOVE CNTL-DT-DAYS-OFFSET
                                      TO WS-DT-OS-DAYS
              MOVE CNTL-DT-HRMN-OFFSET
                                      TO WS-DT-OS-HRMN
           ELSE
              MOVE SPACES             TO WS-DATE-TIME-OFFSET
              IF NOT (NO-RECORD-FND OR END-OF-FILE)
                 MOVE 'P9800-2'       TO ERR-PARAGRAPH
                 MOVE CNTLKEY         TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF.
      *
      ******************************************************************
       P9810-PROCESS-OFFSET.
      ******************************************************************
           MOVE WS-DT-OS-FUN          TO PARM-CONV-TYPE
           MOVE WS-DT-OS-DAYS         TO PARM-SEC-JULIAN-DAY
           MOVE WS-DT-OS-HRMN         TO PARM-SEC-HRMN
           EXEC CICS LINK
                     PROGRAM(P903-PGM)
                     COMMAREA(DATE-CONVERSION-PARMS)
                     LENGTH(P903-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           MOVE WS-RESPONSE           TO FILE-STATUS
           IF NOT SUCCESS
              MOVE 'P9810-1'          TO ERR-PARAGRAPH
              MOVE 'P903'             TO ERR-KEY
              PERFORM P9999-GOT-PROBLEM
           END-IF.
      *
       P9820-SNAPSHOT-XB.
      *
           IF TEMP-ASGN-XB
              OR (NORM-ASGN-XB AND TEMPORARY-ASGNMT NOT > SPACE)
              MOVE SPACES             TO P913-COMMAREA-PARMS
              SET P913-SNAPSHOT-FUNCTION
                                      TO TRUE
              IF TEMP-ASGN-XB
                 MOVE TA-DIST         TO P913-TURN-DIST
                 MOVE TA-SUB-DIST     TO P913-TURN-SUB-DIST
                 MOVE TA-CC           TO P913-TURN-CC
              ELSE
                 MOVE NA-DIST         TO P913-TURN-DIST
                 MOVE NA-SUB-DIST     TO P913-TURN-SUB-DIST
                 MOVE NA-CC           TO P913-TURN-CC
              END-IF
              EXEC CICS LINK
                        PROGRAM(P913-PGM)
                        COMMAREA(P913-COMMAREA-PARMS)
                        LENGTH(P913-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE        TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P9820-1'       TO ERR-PARAGRAPH
                 MOVE 'P913LINK'      TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF.
      *
       P9830-SNAPSHOT-UFP.
      *
           IF TEMP-ASGN-UFP
              OR (NORM-ASGN-UFP AND TEMPORARY-ASGNMT NOT > SPACE)
              MOVE SPACES             TO P915-COMMAREA-PARMS
              SET P915-SNAPSHOT-FUNCTION
                                      TO TRUE
              IF TEMP-ASGN-UFP
                 MOVE TA-DIST         TO P915-TURN-DIST
                 MOVE TA-SUB-DIST     TO P915-TURN-SUB-DIST
                 MOVE TA-POOL         TO P915-TURN-POOL
              ELSE
                 MOVE NA-DIST         TO P915-TURN-DIST
                 MOVE NA-SUB-DIST     TO P915-TURN-SUB-DIST
                 MOVE NA-POOL         TO P915-TURN-POOL
              END-IF
              EXEC CICS LINK
                        PROGRAM(P915-PGM)
                        COMMAREA(P915-COMMAREA-PARMS)
                        LENGTH(P915-LGTH)
                        RESP(WS-RESPONSE)
              END-EXEC
              MOVE WS-RESPONSE        TO FILE-STATUS
              IF NOT SUCCESS
                 MOVE 'P9830-1'       TO ERR-PARAGRAPH
                 MOVE 'P915LINK'      TO ERR-KEY
                 PERFORM P9999-GOT-PROBLEM
              END-IF
           END-IF.
      *
      ******************************************************************
       P9999-GOT-PROBLEM.
      ******************************************************************
           MOVE P919-PGM TO ERR-PROGRAM
           MOVE DFHEIBLK TO ERR-EIBLK
           EXEC CICS XCTL
                     PROGRAM(PSTERR-PGM)
                     COMMAREA(PSTERAR-AREA)
                     LENGTH(PSTERAR-LGTH)
                     RESP(WS-RESPONSE)
           END-EXEC
           EXEC CICS ABEND
                     ABCODE(PSTERR-ABCODE)
                     CANCEL
           END-EXEC.
      *
       X9999-GOBACK.
           GOBACK.
      *
