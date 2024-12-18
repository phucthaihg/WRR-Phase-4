000100 IDENTIFICATION DIVISION.                                         00010003
000200 PROGRAM-ID. CNP917.                                              00020003
000300***************************************************************** 00030003
000400*           CHANGE OF STATUS  - SUBROUTINE                        00040003
000410*                                                                 00041003
000420*           !!!!  USE DB2 COMPILER  !!!!                          00042003
000430*                                                                 00043003
000440***************************************************************** 00044003
000450*  DATE   INITIAL  LOG#   DESCRIPTION                             00045003
000460*-------- ------- ------  --------------------------------------- 00046003
000470*05/19/91   ERW           CICS CONVERSION.                        00047003
000480*11/28/93   LMB    N/A    VACATION MODIFICATIONS.                 00048003
000490*12/22/93   LMB    N/A    UNAVAILABLE STATUS MODIFICATIONS.       00049003
000500*01/17/94   LMB    N/A    CLEAR FIRST TEMPORARY VACANCY DATE IF   00050003
000600*                         EMPLOYEE MARKING UP ONTO PERMANENT      00060003
000700*                         ASSIGNMENT.                             00070003
000800*05/06/94   DLO    N/A    MAINTAIN SYSTEM AVAIL LIST WHEN STATUS  00080003
000900*                         CHANGES                                 00090003
001000*05/09/95   PLS           MAKE CHANGES TO APPLY MISSED CALL       00100003
001100*                         PENALTIES TO BOTH SIDES OF A FAST/SLOW  00110003
001200*                         SPAREBOARD, WHEN REQUESTED.             00120003
001300*05/11/95   LMB    N/A    ADDED WAITING TURN PENDED STATUS.       00130003
001400*05/30/95   LMB    N/A    WHEN INVOKED BY THE EMPLOYEE TRACKING   00140003
001500*                         PROCESS TO MARKUP AN EMPLOYEE WHO OWNS  00150003
001600*                         A POOL TURN, DO NOT LINK BACK TO 916.   00160003
001700*                         INSTEAD LINK TO 916A.                   00170003
001800*06/16/95   PLS    N/A    FIX PROBLEM WITH LAYOFF ON FASTSLOW-XB  00180003
001900*                         WHERE ONLY YARD SIDE GETS TAKEN 'OFF'.  00190003
002000*08/04/95   PLS    C0085  SET PERSONAL REST WHEN REQUESTED        00200003
002100*                         IN A STATUS CHANGE.                     00210003
002200*10/23/95   FHK    C0072  ADD VRU CONF NUMBER TO WSHIST           00220003
002300*01/08/96   CK     C134   IF UFP EMP MARKING UP PUT EMP BACK ON   00230003
002400*                         TURN IF TURN WAS DCAN'D WHILE MARKED    00240003
002500*                         OFF.                                    00250003
002600*02/06/96   SRP  CNC0101  ADD KILLER-B LOGIC                    * 00260003
002700*02/26/96   SRP  CNC0086  CHANGE VAC PAY RATE CALC.             * 00270003
002800*02/29/96   SRP  CNC0086  CHANGE VAC PAY RATE CALC, PART 2.     * 00280003
002900*03/06/96   SRP  BUG C246 CORRECT EMERGENCY MILEAGE ON TS.      * 00290003
003000*03/21/96   FLW  BUG C270 FIX DISPLACEMENT PROBLEM ON SPAREBOARD* 00300003
003100*03/28/1996 SRP  CNC0086  VAC EDITS -- CREDIT FOR UNUSED VAC IF * 00310003
003200*                         EMP IS MARKED UP EARLY(EXCEPT FOR GTW)* 00320003
003300*05/15/1996 SRP  CNC0086  VAC EDITS -- CHANGE MESSAGE SENT TO   * 00330003
003400*                         TASK LIST TO INCLUDE EMP NBR.         * 00340003
003500*05/21/1996 SRP  CNC0086  VAC EDITS -- CHANGE DATE ON WHICH TO  * 00350003
003600*                         START CREDITING TICKETS.              * 00360003
003700*06/06/96   PLS  C-285    DO NOT ALLOW EMPLOYEE WORKING SPLIT     00370003
003800*                         SHIFTS TO BOOK-ON JUST FOR SECOND SHIFT 00380003
003900*07/29/96   FLW  CNC0006  DEFINITION OF WORK WEEK                 00390003
004000*11/14/96   FLW  BUG XXXX FUTURE BOOKON DELETION - USE P917-EMP-NO00400003
004100*                         INSTEAD OF EMP-NO IN WS-MSTR            00410003
004200*12/11/96   CK    C0317   PROCESS XB LAST POSITIONED MISSED CALL  00420003
004300*                         FOR AWARDS.                             00430003
004400*01/27/97   JES  YEAR2000 MODIFY FOR Y2K IN TIMEKEEPING.          00440003
004500*                         ENLARGED TIMESLIP RECORD TO 1536.       00450003
004600*08/04/97   FLW  BUG C379 MISSED CALL TIME ZONE PROBLEM.          00460003
004700*08/06/97   RCW  BUG C375 TEMP XB EMP 'OFF' BOARD WHEN NORM EMP   00470003
004800*                         BOOKS ON AND HASN'T BEEN NOTIFIED.      00480003
004900*08/08/97   RDH           YR 2000 DATE LOGIC UPDATE TO            00490003
005000*                         WK-JSK1-EXP-DATE.                       00500003
005100*09/03/97   FLW  BUG C367 AWARD BUG ECC CODES.                    00510003
005200*09/29/97   NJB  BUG C379 MISSED CALL TIME ZONE PROBLEM.          00520003
005300*09/10/94   FLW  CNC0135  AV LIST FLAG DEFINITIONS                00530003
005400*01/09/98   NJB           NEW VACATION SYSTEM CHANGES             00540003
005500*02/10/98   LMB  CNC0154  VACATION TIMEKEEPING CHANGES.           00550003
005600*03/25/98   AMF  CNC0155  ENHANCED TRACKING CHANGES.              00560003
005700*05/08/98   LPS           Y2K MODIFICATIONS FOR THE TASK LIST.    00570003
005800*08/12/98   RDH  BUG C436 MC-REALIGN-XB FLAG WAS BEING SET        00580003
005900*                         IN WRONG PLACE.  FIX IS IN P2020.       00590003
006000*08/27/98   LMB  CNC0160  ADDED P943-FUN12-NTFY-HOLD-FLAG.        00600003
006100*09/17/98   STP  BUG 406  ADDED BOOKON REJECT REASON CODE.        00610003
006200*09/24/98   RRR  BUG C439 ADDED CODE IN P2000- TO WRITE TRACKING  00620003
006300*                         RECORD ONLY IF NOT MC-TRACK.            00630003
006400*10/16/98   LMB  CNC0160  SET P943 TIMEZONE. IF PERSON DISPLACED  00640003
006500*                         BY EMPLOYEE MARKING UP WAS DISPLACED    00650003
006600*                         BEFORE ASSIGNED (DUE TO TIME TRAVEL)    00660003
006700*                         SET HIST SEQ ERROR.                     00670003
006800*10/30/98   RRR  CNC0173  NEW AVLIST PROCESSING INSTALL           00680003
006900*11/11/98   SWS  BUG C450 MIDNIGHT CALLER MISSED CALL BUG.        00690003
007000*12/14/98   FLW           Y2K SWEEP.                              00700003
007100*01/29/99   DCK           ADDITIONAL CODE FOR NEW AVLIST          00710003
007200*02/16/99   AJK           PASS 'CORRECTION RECORD' FLAG TO 943.   00720003
007300*03/16/99   LMB CNC0208   PASS INIT REASON TO P938.               00730003
007400*04/06/99   NJB           FIX AVLIST                              00740003
007500*04/23/99   AJK CNC0228   ADD SNAPSHOT LOGIC.                     00750003
007600*05/05/99   SAM CNC0216   STATUS/REASON ENHANCEMENT.              00760003
007700*06/10/99   LMB C-414     WHEN MARKUP FROM TRACKING:              00770003
007800*                          - PASS POSITIONING TIME TO P916A.      00780003
007900*                          - IF PERSON WHO OWNS POOL TURN REMAINS 00790003
008000*                            IN OFF STATUS THAT RETAINS POSITION  00800003
008100*                            ON BOARD, PUT TURN ON BOARD.         00810003
008200*07/22/99   AJK CNC0216   WHEN MARKING UP, CHECK LAST LAYOFF      00820003
008300*                         STATUS' STATUS/REASON RECORD TO SEE IF  00830003
008400*                         STAFF FORM E-MAIL SHOULD BE SENT OUT.   00840003
008500*10/27/99   AJK CNC0256   ADD NEW VRU FUNCTION (CHECK-LM-EDITS)   00850003
008600*                         TO DETERMINE IF AN EMPLOYEE CAN LAYOFF/ 00860003
008700*                         MARK UP FROM THE VRU.                   00870003
008800*05/12/00   AJK           DONT BLANK OUT VACATION-TAKEN FLAG WHEN 00880003
008900*                         EMPLOYEE IS MARKED OFF TWICE FOR THE    00890003
009000*                         SAME VACATION.                          00900003
009100*05/15/00   AJK           FIX PERSONAL LEAVE PROCESSING TO POINT  00910003
009200*                         TO PERSONAL LEAVE FILE INSTEAD OF WSMSTR00920003
009300*                         COUNTERS WHEN CALCULATING REMAINING DAYS00930003
009400*06/14/00   GER CNC0300   CALC PERSONAL LEAVE TMSLIP USING LAST   00940003
009500*                         JOB WORKED.                             00950003
009600*07/11/00   AJK           PENDED PERSONAL LEAVE FIX.              00960003
009700*07/24/00   AJK CNC0317   REMOVE PERSONAL LEAVE EDITS - 05E HAS   00970003
009800*                         ALREADY MADE SURE THE EMPLOYEE HAS      00980003
009900*                         SUFFICIENT DAYS LEFT, ETC.              00990003
010000*10/02/00   AJK           DO NOT CREATE PLD TIMESLIP IF CLAIM CODE01000003
010100*                         IS A 'TYPE CODE 1' (ON THE 514 SCREEN). 01010003
010200*                                                                 01020003
010300*02/13/01   TJR           WRITE NEW EMPLOYEE HISTORY RECORDS IF   01030003
010400*                         BOOKOFF/BOOKON IS IN THE FUTURE         01040003
010500*03/28/01   TJR           ADD RESP CHECK AFTER STARTBR IN RTN     01050003
010600*                         P2050-DELETE-PENDED-MARKUP              01060003
010700*05/31/01   GER CNC0339   ADD MINUTES TO CNTL-MC-HOURS            01070003
010800*06/15/01   AJK           DONT ADJUST MARKUP TIME (STATUS REASON  01080003
010900*                         MARKUP +/- TIME) WHEN COMING FROM THE   01090003
011000*                         TASK LIST.  THE MARKUP TIME WAS ALREADY 01100003
011010*                         ADJUSTED WHEN THE TASK WAS CREATED.     01101003
011020*07/19/01   BLD C506      UPDATE REST ON FUTURE CHANGE OF STATUS. 01102003
011030*10/11/01   AJK C466      CLEAR OUT ECC CODE WHEN EXECUTING BOOKON01103003
011031*                         TASKS FOR EMPLOYEES WITH ON-DUTY RECORDS01103103
011032*10/15/01   AJK C477      DONT MARKUP EMPLOYEES FROM TRACKING IF  01103203
011033*                         THEIR CURRENT ASSIGNMENT IS A YARD ASGN 01103303
011034*                         OR THEY HAVE NO CURRENT ASSIGNMENT.     01103403
011035*03/20/02   AJK CNC0246A  BIDS AND BULLETINS.                     01103503
011036*07/23/02   AJK C516      DELETE DUE BACK TASKS FOR UNASSIGNED    01103603
011037*                         EMPLOYEES WHEN THEY MARK UP EARLY.      01103703
011038*08/12/02   AJK CNC0345B  WC MERGER ENHANCEMENT.                  01103803
011039*09/06/02   MET BUG       SUPERVISORS INITIALS.                   01103903
011040*03/04/03   SAM C517      CHANGED HISTORY WRITE FOR MISSED CALLS  01104003
011041*                         FROM SYSTEM TIME TO MC TABLE TIME.      01104103
011042*03/14/03   SAM C536      RESET THE MSTR2 PROCESSING FIELDS WHEN  01104203
011043*                         GOING TO VACATION FROM ANOTHER OFF      01104303
011044*                         STATUS.                                 01104403
011045*04/03/03   SAM C544      ADDED PROCESSING FOR FEBRUARY FOR OFF   01104503
011046*                         FOR EMPLOYEE MILES.                     01104603
011047*06/04/03   AJK           NEVER ACTIVATE EMPLOYEE'S BID CARD ON   01104703
011048*                         MARKUP.                                 01104803
011049*07/30/03   AJK C526      BOOK-ON TASK BUG FIX FOR SPAREBOARD     01104903
011050*                         MISSED CALL PROCESSING (PENDED BOOKON). 01105003
011051*10/10/03   AJK CNC0367H  DON'T INITIATE TRACKING FOR LAYOFF-ON-  01105103
011052*                         CALL WHEN COMING FROM THE CHANGE OF     01105203
011053*                         STATUS SCREEN.                          01105303
011054*01/09/04   NRL CNC0367H  CHANGE THE MESSAGE NUMBER FOR INVALID   01105403
011055*                         REASON CODE.                            01105503
011056*01/16/04   NRL CNC0367H  STORE THE MSGLOG CODE IN P917COMM AND   01105603
011057*                         REMOVE THE ON STATUS/REASON EDIT.       01105703
011058*01/22/04   AJK CNC0367A  DELETE ALL 'VIRTUAL' STARTS RECORDS     01105803
011059*                         FOR EMPLOYEE WHEN MARKING UP.           01105903
011060*03/17/04   AJK C578      DELETE DUEBACK TASK WHEN NON-NOTIFIED   01106003
011061*                         EMPLOYEE BOOKS ON EARLY.                01106103
011062*08/02/04   AJK C597      CORRECT ABEND WHEN XB EMPLOYEE MARKS OFF01106203
011063*                         ON PERS LEAVE.                          01106303
011064*11/04/04   SAM CNC0386D  ADDED BANKTIME PROCESSING.              01106403
011065***************************************************************** 01106503
011066 ENVIRONMENT DIVISION.                                            01106603
011067 CONFIGURATION SECTION.                                           01106703
011068 SOURCE-COMPUTER. IBM-3090.                                       01106803
011069 OBJECT-COMPUTER. IBM-3090.                                       01106903
011070 DATA DIVISION.                                                   01107003
011071 WORKING-STORAGE SECTION.                                         01107103
011072 01  FILLER                          PIC  X(010) VALUE 'P917 W/S'.01107203
011073                                                                  01107303
011074 01  WS-FLAGS.                                                    01107403
011075     05  WS-DONE-CODE                PIC  9(001) VALUE ZEROS.     01107503
011076         88  DONE                                VALUE 1.         01107603
011077     05  WS-AHM-DONE-CODE            PIC  9(001) VALUE ZEROS.     01107703
011078         88  AHM-DONE                            VALUE 1.         01107803
011079         88  AHM-FOUND                           VALUE 2.         01107903
011080     05  WS-VAC-DONE-CODE            PIC  9(001) VALUE ZEROS.     01108003
011081         88 VAC-DONE                             VALUE 1.         01108103
011082     05  WS-OPEN-TURN-WHEN-V0        PIC  9(001) VALUE ZEROS.     01108203
011083         88  OPEN-TURN-WHEN-V0                   VALUE 1.         01108303
011084     05  ASGN-DONE-CODE              PIC  9(001) VALUE ZEROS.     01108403
011085         88  ASGN-DONE                           VALUE 1.         01108503
011086     05  WS-SEND-EM-DATA             PIC  9(001) VALUE ZEROS.     01108603
011087         88  SEND-EM-DATA                        VALUE 1.         01108703
011088     05  WS-DONE-DAYS-UPDATE         PIC  9(001) VALUE ZEROS.     01108803
011089         88  DONE-DAYS-UPDATE                    VALUE 1.         01108903
011090     05  WS-APPLY-TO-OPP-BD-FLAG     PIC  9(001) VALUE ZEROS.     01109003
011091         88  APPLYING-TO-OPP-BOARD               VALUE 1.         01109103
011092     05  WS-MISSED-CALL-FLAGS.                                    01109203
011093         88  MC-NO-ACTION                        VALUE SPACES.    01109303
011094         10  WS-MC-LAYOFF-FLAG       PIC  X(001) VALUE SPACES.    01109403
011095             88  MC-LAYOFF                       VALUE 'Y'.       01109503
011096         10  WS-MC-TRACK-FLAG        PIC  X(001) VALUE SPACES.    01109603
011097             88  MC-TRACK                        VALUE 'Y'.       01109703
011098         10  WS-MC-PEND-MARKUP-FLAG  PIC  X(001) VALUE SPACES.    01109803
011099             88  MC-PEND-MARKUP                  VALUE 'Y'.       01109903
011100         10  WS-MC-REALIGN-XB-FLAG   PIC  X(001) VALUE SPACES.    01110003
011101             88  MC-REALIGN-XB                   VALUE 'Y'.       01110103
011102         10  WS-MC-STARTS-FLAG       PIC  X(001) VALUE SPACES.    01110203
011103             88  MC-ADD-START                    VALUE '1'.       01110303
011104             88  MC-RESET-START                  VALUE '2'.       01110403
011105     05  WS-STAFF-FORM-FLAG          PIC  X(001) VALUE 'N'.       01110503
011106         88  WS-STAFF-FORM-YES                   VALUE 'Y'.       01110603
011107     05  SCHED-DONE-CODE             PIC 9(001) VALUE ZEROES.     01110703
011108         88  SCHED-DONE                         VALUE 1.          01110803
011109     05  SCHED-FIRST-CODE            PIC 9(001) VALUE ZEROES.     01110903
011110         88  SCHED-FIRST                        VALUE 1.          01111003
011111     05  WS-GOTRAIN-SPLIT-SHIFT-FLAG PIC X(001) VALUE 'N'.        01111103
011112         88  NO-SPLIT-SHIFT                     VALUE 'N'.        01111203
011113         88  IT-IS-SPLIT-SHIFT                  VALUE 'Y'.        01111303
011114     05  FUTURE-EFF-TIME-FLAG        PIC X(001) VALUE SPACE.      01111403
011115         88  FUTURE-EFF-TIME                    VALUE 'F'.        01111503
011116         88  NOT-FUTURE-EFF-TIME                VALUE ' '.        01111603
011117     05  NEW-TO-PLACE-FLAG           PIC X(001) VALUE SPACE.      01111703
011118         88  NOT-NEW-TO-PLACE                   VALUE SPACE.      01111803
011119         88  NEW-TO-PLACE                       VALUE 'Y'.        01111903
011120     05  ACCUM-DONE-CODE             PIC 9(001) VALUE 0.          01112003
011121         88  ACCUM-NOT-DONE                     VALUE 0.          01112103
011122         88  ACCUM-DONE                         VALUE 1.          01112203
011123     05  FULL-VAC-TAKEN-FLAG         PIC X(001) VALUE ' '.        01112303
011124         88  FULL-VAC-TAKEN                     VALUE ' '.        01112403
011125         88  FULL-VAC-NOT-TAKEN                 VALUE '1'.        01112503
011126     05  TYPE-CODE-CHECK             PIC X(001) VALUE ' '.        01112603
011127         88  GTW-TYPE                           VALUE '1'.        01112703
011128     05  CREATE-TIMESLIP-FLAG        PIC X(001) VALUE 'Y'.        01112803
011129         88  CREATE-TIMESLIP                    VALUE 'Y'.        01112903
011130         88  DONT-CREATE-TIMESLIP               VALUE 'N'.        01113003
011131     05  EBC-DONE-FLAG               PIC X(001) VALUE 'N'.        01113103
011132         88  EBC-NOT-DONE                       VALUE 'N'.        01113203
011133         88  EBC-DONE                           VALUE 'Y'.        01113303
011134     05  POINTER-DONE-FLAG           PIC X(001) VALUE 'N'.        01113403
011135         88  POINTER-NOT-DONE                   VALUE 'N'.        01113503
011136         88  POINTER-DONE                       VALUE 'Y'.        01113603
011137     05  WRITE-TIMESLIP-FLAG         PIC X(001) VALUE ' '.        01113703
011138         88  WRITE-TIMESLIP                     VALUE ' '.        01113803
011139         88  DONT-WRITE-TIMESLIP                VALUE 'Y'.        01113903
011140                                                                  01114003
011141 01  WS-SYSTEM-NAME                  PIC X(008) VALUE SPACES.     01114103
011142     88  WS-CICS1                               VALUE 'CICS1'.    01114203
011143     88  WS-CATSUSR                             VALUE 'CATSUSR '. 01114303
011144                                                                  01114403
011145 01  WS-SUBSCRIPTS.                                               01114503
011146     05  I                           PIC  9(002) VALUE ZEROS.     01114603
011147     05  J                           PIC  9(002) VALUE ZEROS.     01114703
011148     05  K                           PIC  9(002) VALUE ZEROS.     01114803
011149     05  L                           PIC  9(003) VALUE ZEROS.     01114903
011150     05  M                           PIC  9(003) VALUE ZEROS.     01115003
011151     05  VAC-SUB1                    PIC  9(002) VALUE ZEROS.     01115103
011152                                                                  01115203
011153 01  WS-MISC.                                                     01115303
011154     05  WS-EMP-NBR                  PIC  X(009) VALUE SPACES.    01115403
011155     05  WS-EMP-NUM REDEFINES                                     01115503
011156         WS-EMP-NBR                  PIC  9(009).                 01115603
011157     05  WS-VAC-DIST                  PIC  X(002) VALUE SPACES.   01115703
011158     05  WS-VAC-SUB-DIST              PIC  X(002) VALUE SPACES.   01115803
011159     05  WS-DAYS-REQUESTED.                                       01115903
011160         10  WS-VAC-REQUESTED         PIC  9(002)    VALUE ZEROS. 01116003
011161     05  WS-VAC-DAYS-TAKEN            PIC  9(002)V9  VALUE ZEROS. 01116103
011162     05  WS-VAC-DAYS-DUE              PIC  9(002)V9  VALUE ZEROS. 01116203
011163     05  WS-VAC-CARRYOVER-DAYS        PIC  9(002)V9  VALUE ZEROS. 01116303
011164     05  WS-VAC-DAILY-AMT             PIC  9(003)V99 VALUE ZEROS. 01116403
011165     05  WS-VAC-YEAR                  PIC  9(002)    VALUE ZEROS. 01116503
011166     05  WS-VAC-EFF-CENT-DATE                     VALUE SPACES.   01116603
011167         10  WS-VAC-EFF-CENT          PIC  X(002).                01116703
011168         10  WS-VAC-EFF-DATE.                                     01116803
011169             15  WS-VAC-EFF-YR        PIC  X(002).                01116903
011170             15  WS-VAC-EFF-MO        PIC  X(002).                01117003
011171             15  WS-VAC-EFF-DY        PIC  X(002).                01117103
011172     05  WS-VAC-END-CENT-DATE                     VALUE SPACES.   01117203
011173         10  WS-VAC-END-CENT          PIC  X(002).                01117303
011174         10  WS-VAC-END-DATE.                                     01117403
011175             15  WS-VAC-END-YR        PIC  X(002).                01117503
011176             15  WS-VAC-END-MO        PIC  X(002).                01117603
011177             15  WS-VAC-END-DY        PIC  X(002).                01117703
011178     05  WS-BEFORE-STATUS-ECC                    VALUE SPACES.    01117803
011179         10  WS-BEFORE-STATUS        PIC  X(001).                 01117903
011180         10  WS-BEFORE-ECC           PIC  X(002).                 01118003
011190     05  WS-AFTER-STATUS-ECC                     VALUE SPACES.    01119003
011200         10  WS-AFTER-STATUS         PIC  X(001).                 01120003
011300         10  WS-AFTER-ECC            PIC  X(002).                 01130003
011400     05  WS-BEFORE-STATUS-2          PIC  X(002) VALUE SPACES.    01140003
011500     05  WS-AFTER-STATUS-2           PIC  X(002) VALUE SPACES.    01150003
011600     05  WS-LAST-ASSGN                           VALUE SPACES.    01160003
011700         10  WS-LAST-ASSGN1          PIC  X(010).                 01170003
011800         10  WS-LAST-ASSGN2          PIC  X(002).                 01180003
011900     05  WS-NUMBER-OF-HOURS              PIC 9(5) VALUE 0.        01190003
012000     05  WS-NUMBER-OF-DAYS-REST          PIC 9(2) VALUE 0.        01200003
012100     05  WS-NUMBER-OF-HOURS-REST         PIC 9(4) VALUE 0.        01210003
012200     05  WS-MARKUP-OK-AT.                                         01220003
012300         10  WS-MARKUP-OK-AT-DATE        PIC 9(06) VALUE 0.       01230003
012400         10  WS-MARKUP-OK-AT-TIME        PIC 9(04) VALUE 0.       01240003
012500     05  WS-LAST-LAYOFF-CODE             PIC X      VALUE SPACE.  01250003
012600     05  WS-LAST-LAYOFF-REAS             PIC X(02)  VALUE SPACE.  01260003
012700     05  WS-LAST-LAYOFF-DT               PIC X(10)  VALUE SPACE.  01270003
012800     05  WS-LAST-LAYOFF-LEAD             PIC X(04)  VALUE SPACE.  01280003
012900     05  WS-OWA-HIST-START               PIC X(10)  VALUE SPACE.  01290003
013000     05  WS-NEW-LAYOFF-PT                PIC X(02)  VALUE SPACE.  01300003
013100     05  WS-LAST-MU-FROM-HRMN            PIC X(04)  VALUE SPACES. 01310003
013200     05  WS-LAST-MU-TO-HRMN              PIC X(04)  VALUE SPACES. 01320003
013300     05  WS-LAST-REST-HRMN               PIC 9(04)  VALUE ZEROES. 01330003
013400     05  WS-LAST-SUPV-INIT-X             PIC X      VALUE SPACE.  01340003
013500         88  WS-LAST-SUPV-INIT                      VALUE 'Y'.    01350003
013600     05  WS-LAST-AUTO-EXEC-TASK          PIC X      VALUE SPACE.  01360003
013700         88  WS-LAST-AUTO-EXEC-LO-TASK              VALUE 'L'.    01370003
013800         88  WS-LAST-AUTO-EXEC-MU-TASK              VALUE 'M'.    01380003
013900         88  WS-LAST-AUTO-EXEC-BOTH-TASK            VALUE 'B'.    01390003
014000     05  WS-LAST-REST-OK-X               PIC X      VALUE SPACE.  01400003
014100         88  WS-LAST-REST-OK                        VALUE 'Y'.    01410003
014200     05  WS-LAST-MU-TIME                 PIC X(04)  VALUE SPACES. 01420003
014300     05  WS-LAST-MU-PLUS-MINUS           PIC X(01)  VALUE SPACES. 01430003
014400     05  WS-LAST-AJ-MU-LEAD-TIME         PIC X(04)  VALUE SPACES. 01440003
014500     05  WS-LAST-RETAIN-POS-POOL-X       PIC X(01)  VALUE SPACES. 01450003
014600         88  WS-LAST-RETAIN-POS-POOL                VALUE 'Y'.    01460003
014700     05  WS-LAST-COUNT-PERS-FLAG         PIC X(01)  VALUE SPACES. 01470003
014800         88  WS-LAST-COUNT-PERSONAL                 VALUE 'P'.    01480003
014900         88  WS-LAST-COUNT-SICK                     VALUE 'S'.    01490003
015000         88  WS-LAST-COUNT-BOTH                     VALUE 'B'.    01500003
015100     05  WORK-EXP-YR                     PIC 9(02)  VALUE ZEROES. 01510003
015200     05  WORK-EXP-MO                     PIC 9(02)  VALUE ZEROES. 01520003
015300     05  WS-YEAR-CHECK.                                           01530003
015400         10  FILLER                      PIC X(01)  VALUE SPACES. 01540003
015500         10  FILLER                      PIC X(01)  VALUE SPACES. 01550003
015600             88  WS-EVEN-YR                  VALUE '0', '2', '4', 01560003
015700                                                   '6', '8'.      01570003
015800     05  WS-DAYS-PEND-ACCUM-NUM      PIC S9(04) COMP VALUE ZEROS. 01580003
015900     05  WS-PERSLV-DAYS-REM              PIC S9(02) VALUE ZEROS.  01590003
016000     05  WS-NBR-DAYS-X                   PIC X(02)  VALUE SPACES. 01600003
016100     05  WS-NBR-DAYS-NUM REDEFINES WS-NBR-DAYS-X                  01610003
016200                                         PIC 9(02).               01620003
016300     05  WS-WHEN-WORKED-AREA             PIC 9(05)  VALUE ZEROES. 01630003
016400         88  WORKED-PRIOR-TO-PLD                    VALUE 00001.  01640003
016500     05  WORK-JOB-TYPE                   PIC X(02)  VALUE SPACES. 01650003
016600     05  WORK-RATE-CODE                  PIC X(01)  VALUE SPACES. 01660003
016700     05  WS-OCC-CC-CODE                  PIC X(02)  VALUE SPACES. 01670003
016800                                                                  01680003
016900     02  WS-CALL-AREA.                                            01690003
017000         03  WS-CALL-DAY                 PIC 9(03).               01700003
017100         03  WS-CALL-FROM                PIC 9(04).               01710003
017200         03  WS-CALL-TO                  PIC 9(04).               01720003
017300         03  WS-CALL-DATE                PIC 9(06).               01730003
017400     02  WS-CALL-AREA-1.                                          01740003
017500         03  WS-CALL-DAY1                PIC 9(03).               01750003
017600         03  WS-CALL-FROM1               PIC 9(04).               01760003
017700         03  WS-CALL-TO1                 PIC 9(04).               01770003
017800         03  WS-CALL-DATE1               PIC 9(06).               01780003
017900     02  WS-CALL-AREA-2.                                          01790003
018000         03  WS-CALL-DAY2                PIC 9(03).               01800003
018100         03  WS-CALL-FROM2               PIC 9(04).               01810003
018200         03  WS-CALL-TO2                 PIC 9(04).               01820003
018300         03  WS-CALL-DATE2               PIC 9(06).               01830003
018400     02  INQ-LIMIT-DATE-TIME-CENT.                                01840003
018500         05  INQ-LIMIT-CE            PIC  X(002) VALUE SPACES.    01850003
018600         05  INQ-LIMIT-DATE-TIME.                                 01860003
018700             10  INQ-LIMIT-DATE      PIC  X(006) VALUE SPACES.    01870003
018800             10  INQ-LIMIT-TIME      PIC  X(004) VALUE SPACES.    01880003
018900                                                                  01890003
019000 01  BT-WORK-AREA.                                                01900003
019100     05  WORK-BASIC-DAY-RATE         PIC 9(06) VALUE ZEROS.       01910003
019200     05  WORK-BASIC-DAY-RATE-X   REDEFINES WORK-BASIC-DAY-RATE    01920003
019300                                     PIC X(06).                   01930003
019400     05  WORK-CURRENT-BANKTIME       PIC 9(07) VALUE ZEROS.       01940003
019500     05  WORK-CURRENT-BANKTIME-X REDEFINES WORK-CURRENT-BANKTIME  01950003
019600                                     PIC X(07).                   01960003
019700     05  WORK-TOTAL-BANKTIME         PIC 9(07) VALUE ZEROS.       01970003
019800     05  WORK-TOTAL-BANKTIME-X   REDEFINES WORK-TOTAL-BANKTIME    01980003
019810                                     PIC X(07).                   01981003
019820     05  WORK-BANKTIME-DIFF          PIC 9(07) VALUE ZEROS.       01982003
019830     05  PIC-9-DATA-VALUE            PIC 9(04) VALUE ZEROS.       01983003
019840     05  PIC-X-DATA-VALUE REDEFINES PIC-9-DATA-VALUE.             01984003
019841         06  FILLER                  PIC X(01).                   01984103
019842         06  PIC-X-VALUE             PIC X(03).                   01984203
019843                                                                  01984303
019844 01  WS-HOLIDAY-DATE-TIME.                                        01984403
019845     05 WS-HOLIDAY-DATE.                                          01984503
019846        10 WS-HOLIDAY-YR             PIC X(002).                  01984603
019847        10 WS-HOLIDAY-MO             PIC X(002).                  01984703
019848        10 WS-HOLIDAY-DY             PIC X(002).                  01984803
019849     05 WS-HOLIDAY-TIME              PIC X(004).                  01984903
019850                                                                  01985003
019851 01  WS-TS-DATA.                                                  01985103
019852     02  WS-DATE-ARRAY OCCURS 15 TIMES.                           01985203
019853         03  WS-TS-EFF-DATE.                                      01985303
019854             05  WS-TS-EFF-YR  PIC X(02) VALUE SPACES.            01985403
019855             05  WS-TS-EFF-MO  PIC X(02) VALUE SPACES.            01985503
019856             05  WS-TS-EFF-DY  PIC X(02) VALUE SPACES.            01985603
019857                                                                  01985703
019858 01  NORMAL-ASGNMT-FLAG              PIC  X(001) VALUE SPACES.    01985803
019859     88  NORM-ASGN-UFP                           VALUE 'U'.       01985903
019860     88  NORM-ASGN-XB                            VALUE 'X'.       01986003
019870     88  NORM-ASGN-AJ                            VALUE 'A'.       01987003
019880 01  NORMAL-ASGNMT                               VALUE SPACES.    01988003
019890     05  NA-DIST                     PIC  X(002).                 01989003
019900     05  NA-SUB-DIST                 PIC  X(002).                 01990003
020000     05  NA-AREA.                                                 02000003
020100         10  NA-1                    PIC  X(006).                 02010003
020200         10  NA-2 REDEFINES NA-1.                                 02020003
020300             15  NA-POOL             PIC  X(002).                 02030003
020400             15  NA-TURN             PIC  X(004).                 02040003
020500         10  NA-3 REDEFINES NA-1.                                 02050003
020600             15  NA-FILLER           PIC  X(002).                 02060003
020700             15  NA-XB-TURN          PIC  X(004).                 02070003
020800         10  NA-CC                   PIC  X(002).                 02080003
020900                                                                  02090003
021000 01  TEMPORARY-ASGNMT-FLAG           PIC  X(001) VALUE SPACES.    02100003
021100     88  TEMP-ASGN-UFP                           VALUE 'U'.       02110003
021200     88  TEMP-ASGN-AJ                            VALUE 'A'.       02120003
021300     88  TEMP-ASGN-XB                            VALUE 'X'.       02130003
021400 01  TEMPORARY-ASGNMT                            VALUE SPACES.    02140003
021500     05  TA-DIST                     PIC  X(002).                 02150003
021600     05  TA-SUB-DIST                 PIC  X(002).                 02160003
021700     05  TA-AREA.                                                 02170003
021800         10  TA-1                    PIC  X(006).                 02180003
021900         10  TA-2 REDEFINES TA-1.                                 02190003
022000             15  TA-POOL             PIC  X(002).                 02200003
022100             15  TA-TURN             PIC  X(004).                 02210003
022200         10  TA-3 REDEFINES TA-1.                                 02220003
022300             15  TA-FILLER           PIC  X(002).                 02230003
022400             15  TA-XB-TURN          PIC  X(004).                 02240003
022500         10  TA-CC                   PIC  X(002).                 02250003
022600                                                                  02260003
022700 01  ON-DUTY-ASGNMT-FLAG             PIC  X(001) VALUE SPACES.    02270003
022800     88  ON-DUTY-UFP                             VALUE 'U'.       02280003
022900     88  ON-DUTY-AJ                              VALUE 'A'.       02290003
023000 01  ON-DUTY-OUT-TOWN-CODE           PIC X(10) VALUE '9999999999'.02300003
023100     88  OUT-TOWN                              VALUE '0000000000'.02310003
023200 01  ON-DUTY-ASGNMT                              VALUE SPACES.    02320003
023300     05  OD-DIST                     PIC  X(002).                 02330003
023400     05  OD-SUB-DIST                 PIC  X(002).                 02340003
023500     05  OD-AREA.                                                 02350003
023600         10  OD-1                    PIC  X(006).                 02360003
023700         10  OD-2 REDEFINES OD-1.                                 02370003
023800             15  OD-POOL             PIC  X(002).                 02380003
023900             15  OD-TURN             PIC  X(004).                 02390003
024000         10  OD-CC                   PIC  X(002).                 02400003
024100                                                                  02410003
024200 01  XXXX-ASGNKEY1-PREV              PIC  X(24)  VALUE SPACES.    02420003
024300 01  XXXX-ASGNKEY1.                                               02430003
024400     05  XX-ASGN-JOB-TYPE            PIC  X(001) VALUE SPACES.    02440003
024500         88  XX-ASGN-JOB-XB                      VALUE 'X'.       02450003
024600     05  XX-ASGN-DIST                PIC  X(002) VALUE SPACES.    02460003
024700     05  XX-ASGN-SUB-DIST            PIC  X(002) VALUE SPACES.    02470003
024800     05  XX-ASGN-JOB.                                             02480003
024900         06  XX-ASGN-JOB-ID.                                      02490003
025000             08  XX-FIRST-2-BYTES    PIC  X(002) VALUE SPACES.    02500003
025100             08  XX-LAST-4-BYTES     PIC  X(004) VALUE SPACES.    02510003
025200         06  XX-ASGN-JOB-CC          PIC  X(002) VALUE SPACES.    02520003
025300     05  XX-ASGN-REC-TYPE            PIC  X(001) VALUE SPACES.    02530003
025400     05  XX-ASGN-DATE-TIME           PIC  9(010) VALUE ZEROS.     02540003
025500                                                                  02550003
025600 01  XXXX-ASGNKEY2.                                               02560003
025700     05  XX-ASGN-EMP                 PIC  9(009) VALUE ZEROS.     02570003
025800     05  XX-ASGN2-REC-TYPE           PIC  X(001) VALUE SPACES.    02580003
025900     05  XX-ASGN2-DATE-TIME          PIC  9(010) VALUE ZEROS.     02590003
026000                                                                  02600003
026100 01  REL-ASGNKEY1.                                                02610003
026200     05  FILLER                      PIC  X(014) VALUE SPACES.    02620003
026300     05  REL-ASGN-DATE-TIME          PIC  X(010) VALUE SPACES.    02630003
026400                                                                  02640003
026500 01  REL-ASGNKEY2                    PIC  X(020) VALUE SPACES.    02650003
026600                                                                  02660003
026700 01  SAVE-JSK1-ASGN-START-TIME       PIC  X(006) VALUE SPACES.    02670003
026800 01  SAVE-TEMP-ASGN-KEY              PIC  X(024) VALUE SPACES.    02680003
026900 01  SAVE-NORM-ASGN-KEY              PIC  X(024) VALUE SPACES.    02690003
027000                                                                  02700003
027100 01  WS-TIME-ZONE                    PIC  X(001) VALUE SPACES.    02710003
027200 01  WS-SAVE-LO                      PIC  X(001) VALUE SPACES.    02720003
027300 01  WS-SAVE-EDB                     PIC  X(002) VALUE SPACES.    02730003
027400 01  SAVE-ASGN-TYPE                  PIC  X(001) VALUE SPACES.    02740003
027500 01  SAVE-ASGN                       PIC  X(006) VALUE SPACES.    02750003
027600                                                                  02760003
027700*01  WS-SYSTEM-DATE-TIME.                                         02770003
027800*    05  WS-SYSTEM-DATE              PIC  9(006) VALUE ZEROS.     02780003
027900*    05  WS-SYSTEM-TIME.                                          02790003
028000*        10  WS-SYSTEM-HRMN          PIC  9(004) VALUE ZEROS.     02800003
028100*        10  FILLER                  PIC  9(004) VALUE ZEROS.     02810003
028200*01  WS-CURRENT-DATE-TIME REDEFINES                               02820003
028300*    WS-SYSTEM-DATE-TIME.                                         02830003
028400*    05  WS-CURR-DATE-TIME           PIC  X(010).                 02840003
028500*    05  FILLER                      PIC  X(004).                 02850003
028600*01  WS-WORK-SYSTEM-DATE  REDEFINES                               02860003
028700*    WS-SYSTEM-DATE-TIME.                                         02870003
028800*    05  WORK-DATE.                                               02880003
028900*        10  WORK-YY                 PIC  9(002).                 02890003
029000*        10  WORK-MM                 PIC  9(002).                 02900003
029100*        10  WORK-DD                 PIC  9(002).                 02910003
029200*    05  FILLER                      PIC  X(008).                 02920003
029300                                                                  02930003
029400 01  WORK-HIST-TIMEX.                                             02940003
029500     05  WORK-HIST-TIME              PIC  9(012) VALUE ZEROS.     02950003
029600     05  WORK-HIST-TIME-TIE          PIC  9(002) VALUE ZEROS.     02960003
029700                                                                  02970003
029800*01  WS-LOCAL-DATE-TIME                          VALUE SPACES.    02980003
029900*    05  WS-LOCAL-DATE               PIC  X(006).                 02990003
030000*    05  WS-LOCAL-TIME               PIC  X(004).                 03000003
030100                                                                  03010003
030200 01  WS-COMPARE-DATE-TIME-CENT                   VALUE ZEROS.     03020003
030300     05  WS-COMPARE-DATE-CENT.                                    03030003
030400         10  WS-COMPARE-CE           PIC  9(002).                 03040003
030500         10  WS-COMPARE-DATE         PIC  9(006).                 03050003
030600     05  WS-COMPARE-TIME             PIC  9(004).                 03060003
030700                                                                  03070003
030800 01  WS-EFF-DATE-TIME-CE                         VALUE SPACES.    03080003
030900     05  WS-EFF-CE                   PIC  X(002).                 03090003
031000     05  WS-EFF-DATE-TIME.                                        03100003
031100         10  WS-EFF-DATE             PIC  X(006).                 03110003
031200         10  WS-EFF-TIME             PIC  X(004).                 03120003
031300                                                                  03130003
031400 01  WS-MC-MU-DATE-TIME                          VALUE SPACES.    03140003
031500     05  WS-MC-MU-DATE               PIC  X(006).                 03150003
031600     05  WS-MC-MU-TIME               PIC  X(004).                 03160003
031700                                                                  03170003
031800 01  WS-MC-LO-DATE-TIME                          VALUE SPACES.    03180003
031900     05  WS-MC-LO-DATE               PIC  X(006).                 03190003
032000     05  WS-MC-LO-TIME               PIC  X(004).                 03200003
032100                                                                  03210003
032200 01  WS-MU-DATE-TIME                             VALUE SPACES.    03220003
032300     05  WS-MU-DATE                  PIC  X(006).                 03230003
032400     05  WS-MU-TIME                  PIC  X(004).                 03240003
032500                                                                  03250003
032600 01  WS-NEW-DOS-DATE-TIME                        VALUE SPACES.    03260003
032700     05  WS-NEW-DOS-DATE             PIC  X(006).                 03270003
032800     05  WS-NEW-DOS-TIME             PIC  X(004).                 03280003
032900                                                                  03290003
033000 01  WS-DAYS-DIFF                    PIC  9(005) VALUE ZEROS.     03300003
033100 01  WS-100-PERCENT                  PIC  X(004) VALUE '100%'.    03310003
033200                                                                  03320003
033300 01  WS-STAY-UNAVAILABLE-FLAG        PIC  X      VALUE ' '.       03330003
033400     88  WS-STAY-UNAVAILABLE                     VALUE 'U'.       03340003
033500                                                                  03350003
033600 01  WORK-CNTLKEY                                VALUE SPACES.    03360003
033700     05  WK-CNTL-REC-TYPE            PIC  X(002).                 03370003
033800     05  WK-CNTLKEY-AREA.                                         03380003
033900         10  WK-CNTL-DIST            PIC  X(002).                 03390003
034000         10  WK-CNTL-SUB-DIST        PIC  X(002).                 03400003
034100         10  WK-CNTL-XB-CODE         PIC  X(002).                 03410003
034200         10  WK-CNTL-POOL-TYPE       PIC  X(001).                 03420003
034300         10  FILLER                  PIC  X(011).                 03430003
034400     05  WK-CNTL-MC-KEY REDEFINES                                 03440003
034500         WK-CNTLKEY-AREA.                                         03450003
034600         10  WK-CNTL-MC-CODE         PIC  X(004).                 03460003
034700         10  WK-CNTL-MC-TYPE-SVC     PIC  X(002).                 03470003
034800         10  WK-CNTL-MC-DIST         PIC  X(002).                 03480003
034900         10  WK-CNTL-MC-SDIST        PIC  X(002).                 03490003
035000         10  WK-CNTL-MC-XB           PIC  X(002).                 03500003
035100         10  WK-CNTL-MC-CONS         PIC  X(002).                 03510003
035200         10  FILLER                  PIC  X(004).                 03520003
035300     05  WK-CNTL-XB-KEY REDEFINES                                 03530003
035400         WK-CNTLKEY-AREA.                                         03540003
035500         10  WK-CNTL-XB-DIST         PIC  X(002).                 03550003
035600         10  WK-CNTL-XB-SDIST        PIC  X(002).                 03560003
035700         10  WK-CNTL-XB-CC           PIC  X(002).                 03570003
035800         10  FILLER                  PIC  X(012).                 03580003
035900                                                                  03590003
036000 01  SAVE-CNTL-FILE                  PIC  X(256) VALUE SPACES.    03600003
036100 01  SAVE-ASGN-AREA                  PIC  X(128) VALUE SPACES.    03610003
036200 01  SAVE-P956-COMMAREA              PIC  X(500) VALUE SPACES.    03620003
036300 01  SAVE-DEACT-BID-CD               PIC  X(001) VALUE SPACES.    03630003
036400     88  SAVE-DEACT-NO                           VALUE 'N', ' '.  03640003
036500     88  SAVE-DEACT-PERM                         VALUE 'P'.       03650003
036600     88  SAVE-DEACT-WEEKLY                       VALUE 'W'.       03660003
036700     88  SAVE-DEACT-MILES                        VALUE 'M'.       03670003
036800                                                                  03680003
036900 01  WS-LIST-ID                      PIC  X(002) VALUE SPACES.    03690003
037000     88 SYSTEM-LIST-ID                           VALUE '#L'.      03700003
037100                                                                  03710003
037200 01  WS-GOTRAIN-MISC-AREA.                                        03720003
037300     05  WS-GOTRAIN-ASGNMT.                                       03730003
037400         10  WS-GOTRAIN-DIST         PIC X(002) VALUE SPACES.     03740003
037500         10  WS-GOTRAIN-SUB-DIST     PIC X(002) VALUE SPACES.     03750003
037600         10  WS-GOTRAIN-ASGN         PIC X(008) VALUE SPACES.     03760003
037700     05  WS-GOTRAIN-SHIFT1-START     PIC X(004) VALUE SPACES.     03770003
037800     05  WS-GOTRAIN-SHIFT2-START     PIC X(004) VALUE SPACES.     03780003
037900     05  WS-GOTRAIN-DAY              PIC 9(003) VALUE ZEROES.     03790003
038000 01  TEMP-FIELDS-FOR-OLD-VAC-EDITS.                               03800003
038100     05  WS-TEMP-DAYS-AVAIL              PIC S99.                 03810003
038200     05  WS-TEMP-AMT-ALLOWED-SPENT       PIC S9(7)V99.            03820003
038300     05  WS-TEMP-VAC-AMT-ALLOWED          PIC S9(7)V99.           03830003
038400 01  WS-TS-NUMBER                      PIC  X(009).               03840003
038500 01  WS-TS-NUM REDEFINES WS-TS-NUMBER  PIC  9(009).               03850003
038600 01  WS-SAVE-PAY-VAC-AND-WORK-SW        PIC  X      VALUE 'N'.    03860003
038700     88  WS-SAVE-PAY-VAC-AND-WORK                   VALUE 'Y'.    03870003
038800     88  WS-SAVE-CREDIT-UNUSED-VAC                  VALUE 'N'.    03880003
038900 01  WS-VAC-CR-LAST-UNAVAIL-FLAG        PIC  X      VALUE '0'.    03890003
039000     88  WS-BLANK-LAST-UNAVAIL                      VALUE '1'.    03900003
039100     88  WS-SET-LAST-UNAVAIL-TO-VAC                 VALUE '2'.    03910003
039200 01  WS-WORK-AREA.                                                03920003
039300     05  WS-WORK-DATE-TIME-X             PIC X(010).              03930003
039400     05  WS-WORK-DATE-TIME REDEFINES                              03940003
039500         WS-WORK-DATE-TIME-X             PIC 9(010).              03950003
039600*                                                                 03960003
039700                                                                  03970003
039800***************************************************************** 03980003
039900***                 I/O STATUS CHECK FIELDS                       03990003
040000***************************************************************** 04000003
040100 01  WS-RESPONSE                     PIC S9(008) COMP VALUE ZEROS.04010003
040200 01  FILE-STATUS                     PIC  9(004) VALUE ZEROS.     04020003
040300     EXEC SQL INCLUDE IOCODES END-EXEC.                           04030003
040400 01  DISPLAY-FILE-STATUS             PIC S9(8) VALUE ZERO.        04040003
040500     EXEC SQL INCLUDE WSCOMMON END-EXEC.                          04050003
040600***************************************************************** 04060003
040700***                   COMMAREA COPYBOOKS                          04070003
040800***************************************************************** 04080003
040900     EXEC SQL INCLUDE P917COMM END-EXEC.                          04090003
041000***************************************************************** 04100003
041100***                  CALLED ROUTINES COPYBOOKS.                   04110003
041200***************************************************************** 04120003
041300     EXEC SQL INCLUDE PSTERAR END-EXEC.                           04130003
041400     EXEC SQL INCLUDE PS45COMM END-EXEC.                          04140003
041500     EXEC SQL INCLUDE P902COMM END-EXEC.                          04150003
041600     EXEC SQL INCLUDE P903COMM END-EXEC.                          04160003
041700     EXEC SQL INCLUDE P913COMM END-EXEC.                          04170003
041800     EXEC SQL INCLUDE P914COMM END-EXEC.                          04180003
041900     EXEC SQL INCLUDE P915COMM END-EXEC.                          04190003
042000     EXEC SQL INCLUDE P916COMM END-EXEC.                          04200003
042100     EXEC SQL INCLUDE P927COMM END-EXEC.                          04210003
042200     EXEC SQL INCLUDE P929COMM END-EXEC.                          04220003
042300     EXEC SQL INCLUDE P930COMM END-EXEC.                          04230003
042400     EXEC SQL INCLUDE P931COMM END-EXEC.                          04240003
042500     EXEC SQL INCLUDE P937COMM END-EXEC.                          04250003
042600     EXEC SQL INCLUDE P938COMM END-EXEC.                          04260003
042700     EXEC SQL INCLUDE P942COMM END-EXEC.                          04270003
042800     EXEC SQL INCLUDE P943COMM END-EXEC.                          04280003
042900     EXEC SQL INCLUDE P956COMM END-EXEC.                          04290003
043000     EXEC SQL INCLUDE P963COMM END-EXEC.                          04300003
043100     EXEC SQL INCLUDE P970COMM END-EXEC.                          04310003
043200     EXEC SQL INCLUDE P977COMM END-EXEC.                          04320003
043300     EXEC SQL INCLUDE P983COMM END-EXEC.                          04330003
043400     EXEC SQL INCLUDE PSTCB00I END-EXEC.                          04340003
043500     EXEC SQL INCLUDE PSTCB00G END-EXEC.                          04350003
043600     EXEC SQL INCLUDE PSTCB05E END-EXEC.                          04360003
043700     EXEC SQL INCLUDE PSTCB16  END-EXEC.                          04370003
043800***************************************************************** 04380003
043900***                       FILE COPYBOOKS                          04390003
044000***************************************************************** 04400003
044100     EXEC SQL INCLUDE EBCV END-EXEC.                              04410003
044200     EXEC SQL INCLUDE EBCWS END-EXEC.                             04420003
044300     EXEC SQL INCLUDE WSAJ END-EXEC.                              04430003
044400     EXEC SQL INCLUDE WSAL END-EXEC.                              04440003
044500     EXEC SQL INCLUDE WSAHIST END-EXEC.                           04450003
044600     EXEC SQL INCLUDE WSAHMISC END-EXEC.                          04460003
044700     EXEC SQL INCLUDE WSASGN END-EXEC.                            04470003
044800     EXEC SQL INCLUDE WSCARR END-EXEC.                            04480003
044900     EXEC SQL INCLUDE WSCNTL END-EXEC.                            04490003
045000     EXEC SQL INCLUDE WSDEC END-EXEC.                             04500003
045100     EXEC SQL INCLUDE WSEB END-EXEC.                              04510003
045200     EXEC SQL INCLUDE WSJS END-EXEC.                              04520003
045300     EXEC SQL INCLUDE WSMSTR END-EXEC.                            04530003
045400     EXEC SQL INCLUDE WSMSTR2 END-EXEC.                           04540003
045500     EXEC SQL INCLUDE WSPOINTR END-EXEC.                          04550003
045510     EXEC SQL INCLUDE WSTASK END-EXEC.                            04551003
045520     EXEC SQL INCLUDE WSTKCR END-EXEC.                            04552003
045530     EXEC SQL INCLUDE WSVACEMP END-EXEC.                          04553003
045540     EXEC SQL INCLUDE WSTMSLIP END-EXEC.                          04554003
045550     EXEC SQL INCLUDE WSTRCN END-EXEC.                            04555003
045560     EXEC SQL INCLUDE WSUFP END-EXEC.                             04556003
045570     EXEC SQL INCLUDE WSPERS END-EXEC.                            04557003
045580                                                                  04558003
045590***************************************************************** 04559003
045600***                       MISC COPYBOOKS                          04560003
045700***************************************************************** 04570003
045800     EXEC SQL INCLUDE PSTCCRFT END-EXEC.                          04580003
045900     EXEC SQL INCLUDE WSAJDEFN END-EXEC.                          04590003
045910     EXEC SQL INCLUDE WSBIF END-EXEC.                             04591003
045920     EXEC SQL INCLUDE WSEDDATE END-EXEC.                          04592003
045930     EXEC SQL INCLUDE WSSYDTTM END-EXEC.                          04593003
045940     EXEC SQL INCLUDE WSEDTIME END-EXEC.                          04594003
045950     EXEC SQL INCLUDE WSLOCODE END-EXEC.                          04595003
045960     EXEC SQL INCLUDE WSMSG END-EXEC.                             04596003
045970     EXEC SQL INCLUDE WSOFFSET END-EXEC.                          04597003
045980     EXEC SQL INCLUDE WSSTFORM END-EXEC.                          04598003
045990     EXEC SQL INCLUDE WSZONE END-EXEC.                            04599003
046000     EXEC SQL INCLUDE WSTRKRSN END-EXEC.                          04600003
046100*                                                                 04610003
046200     EXEC SQL INCLUDE SQLCA END-EXEC.                             04620003
046300                                                                  04630003
046400 LINKAGE SECTION.                                                 04640003
046500 01  DFHCOMMAREA.                                                 04650003
046600     05  FILLER            PIC X(250).                            04660003
046700                                                                  04670003
046800 PROCEDURE DIVISION.                                              04680003
046900*                                                                 04690003
047000 P0000-MAINLINE.                                                  04700003
047100*                                                                 04710003
047200     EXEC CICS IGNORE CONDITION                                   04720003
047300               ERROR                                              04730003
047400     END-EXEC                                                     04740003
047500     EXEC CICS HANDLE ABEND                                       04750003
047600               LABEL(P9999-GOT-PROBLEM)                           04760003
047700     END-EXEC                                                     04770003
047800     EXEC SQL INCLUDE ABSTIME END-EXEC.                           04780003
047900     IF EIBCALEN = ZERO                                           04790003
048000        PERFORM P9999-GOT-PROBLEM                                 04800003
048100     END-IF                                                       04810003
048200     EXEC CICS ASKTIME                                            04820003
048300               ABSTIME(WS-ABSTIME)                                04830003
048400     END-EXEC                                                     04840003
048500     ADD WS-ABSTIME-OFFSET TO WS-ABSTIME                          04850003
048600     EXEC CICS FORMATTIME                                         04860003
048700               ABSTIME(WS-ABSTIME)                                04870003
048800               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      04880003
048900               TIME(WS-SYSTEM-TIME-AREA)                          04890003
049000     END-EXEC                                                     04900003
049100*                                                                 04910003
049200*    INSTALL APPLICATION DATE/TIME                                04920003
049300*                                                                 04930003
049400     PERFORM P9800-GET-TIME-OFFSET                                04940003
049500     IF WS-DATE-TIME-OFFSET > SPACES                              04950003
049600        MOVE ZEROS          TO DATE-CONVERSION-PARMS              04960003
049700        MOVE WS-SYSTEM-DATE TO PARM-PRI-DATE-GREG                 04970003
049800        MOVE WS-SYSTEM-TIME TO PARM-PRI-HRMN                      04980003
049900        PERFORM P9810-PROCESS-OFFSET                              04990003
050000        MOVE PARM-RES-DATE-GREG                                   05000003
050100                            TO WS-SYSTEM-DATE                     05010003
050200        MOVE PARM-RES-HRMN                                        05020003
050300                            TO WS-SYSTEM-TIME                     05030003
050400     END-IF                                                       05040003
050500*                                                                 05050003
050600     SET DE-YYMMDD-FORMAT     TO TRUE                             05060003
050700     MOVE WS-SYSTEM-DATE      TO DE-YYMMDD                        05070003
050800     PERFORM P8998-DATEEDIT                                       05080003
050900     MOVE DE-YYMMDD-CE        TO WS-SYSTEM-CENT                   05090003
051000*                                                                 05100003
051100     MOVE WS-SYSTEM-DATE-TIME TO WORK-HIST-TIME                   05110003
051200     MOVE ZEROS               TO WORK-HIST-TIME-TIE               05120003
051300     MOVE DFHCOMMAREA TO P917-COMMAREA-PARMS                      05130003
051400     IF P917-VRU-FUNCTION NOT > SPACES                            05140003
051500        PERFORM P1000-CHECK-FOR-ERRORS                            05150003
051600     ELSE                                                         05160003
051700        IF NOT P917-VRU-CHECK-LM-EDITS                            05170003
051800           PERFORM P1000-CHECK-FOR-ERRORS                         05180003
051900           PERFORM P1550-CHECK-VRU-ERRORS                         05190003
052000        ELSE                                                      05200003
052100           PERFORM P1700-CHECK-EDITS-ONLY                         05210003
052200        END-IF                                                    05220003
052300     END-IF                                                       05230003
052400     IF NOT P917-ERROR-FOUND                                      05240003
052500        AND NOT P917-VRU-CHECK-LM-EDITS                           05250003
052600        IF P917-PAY-IN-LIEU OR P917-ADVANCE-PAY                   05260003
052700*         'UPDATE WAS SUCCESSFUL'                                 05270003
052800          MOVE 'U001'        TO MSGLOG-CODE                       05280003
052900        ELSE                                                      05290003
053000           IF P917-VRU-FUNCTION NOT > SPACES                      05300003
053100           OR NOT P917-VRU-EDIT-MARKUP                            05310003
053200              IF P917-STATUS-CODE1  = 'A'                         05320003
053300                 IF UNAVAILABLE-LAST-FLAG > SPACES                05330003
053400                  AND P917-FROM-TASK-LIST                         05340003
053500                  AND ( WORKING   OF LAYOFF-CODE-1                05350003
053600                    OR AVAILABLE OF LAYOFF-CODE-1)                05360003
053700                    PERFORM P1500-UPDATE-UNAVAIL-LAST             05370003
053800*                   'UPDATE WAS SUCCESSFUL'                       05380003
053900                    MOVE 'U001'     TO MSGLOG-CODE                05390003
054000                 ELSE                                             05400003
054100                    PERFORM P2000-UPDATE                          05410003
054200                 END-IF                                           05420003
054300              ELSE                                                05430003
054400                 IF EIBTRNID = P00I-TRAN                          05440003
054500                    IF P956-ST-RSN-AUTO-EXEC-TASK > SPACES        05450003
054600                       OR P917-STATUS-CODE1 = 'K'                 05460003
054700                       PERFORM P2000-UPDATE                       05470003
054800                    END-IF                                        05480003
054900                 ELSE                                             05490003
055000                    PERFORM P2000-UPDATE                          05500003
055100                 END-IF                                           05510003
055200              END-IF                                              05520003
055300           END-IF                                                 05530003
055400        END-IF                                                    05540003
055500     END-IF                                                       05550003
055600     PERFORM P9000-RETURN.                                        05560003
055700*                                                                 05570003
055800 P1000-CHECK-FOR-ERRORS.                                          05580003
055900*                                                                 05590003
056000     MOVE SPACES           TO P917-ERRORCODE                      05600003
056100     MOVE P917-EMP-NO TO MSTRNBRK                                 05610003
056200     EXEC CICS READ                                               05620003
056300               DATASET(MSTR-VIA-EMP-NBR)                          05630003
056400               INTO(WS-MSTR)                                      05640003
056500               LENGTH(MSTRENBR-RLGTH)                             05650003
056600               RIDFLD(MSTRNBRK)                                   05660003
056700               KEYLENGTH(MSTRENBR-KLGTH)                          05670003
056800               RESP(WS-RESPONSE)                                  05680003
056900     END-EXEC                                                     05690003
057000     MOVE WS-RESPONSE TO FILE-STATUS                              05700003
057100     IF NOT SUCCESS                                               05710003
057200        MOVE 'P1000-1' TO ERR-PARAGRAPH                           05720003
057300        MOVE MSTRNBRK  TO ERR-KEY                                 05730003
057400        PERFORM P9999-GOT-PROBLEM                                 05740003
057500     END-IF                                                       05750003
057600*                                                                 05760003
057700*    GET ALL OF THE EMPLOYEES CURRENT ASSIGNMENTS                 05770003
057800*                                                                 05780003
057900     PERFORM P8510-MASTER-JOBS                                    05790003
058000     PERFORM P9820-SNAPSHOT-XB                                    05800003
058100     PERFORM P9830-SNAPSHOT-UFP                                   05810003
058200*                                                                 05820003
058300     MOVE SPACE               TO WORK-CNTLKEY                     05830003
058400     MOVE '02'                TO WK-CNTL-REC-TYPE                 05840003
058500     MOVE DIST     OF WS-MSTR TO WK-CNTL-DIST                     05850003
058600     MOVE SUB-DIST OF WS-MSTR TO WK-CNTL-SUB-DIST                 05860003
058700     MOVE WORK-CNTLKEY        TO CNTLKEY                          05870003
058800     EXEC CICS READ                                               05880003
058900               DATASET(CNTL-FILE-VIA-CNTLKEY)                     05890003
059000               INTO(WS-CNTL-FILE)                                 05900003
059100               LENGTH(CNTLFILE-RLGTH)                             05910003
059200               RIDFLD(CNTLKEY)                                    05920003
059300               KEYLENGTH(CNTLFILE-KLGTH)                          05930003
059400               RESP(WS-RESPONSE)                                  05940003
059500     END-EXEC                                                     05950003
059600     MOVE WS-RESPONSE  TO FILE-STATUS                             05960003
059700     IF NOT SUCCESS                                               05970003
059800        MOVE 'P1000-2' TO ERR-PARAGRAPH                           05980003
059900        MOVE CNTLKEY   TO ERR-KEY                                 05990003
060000        PERFORM P9999-GOT-PROBLEM                                 06000003
060100     END-IF                                                       06010003
060200     IF  PAY-VAC-AND-WORK                                         06020003
060300         MOVE 'Y'      TO WS-SAVE-PAY-VAC-AND-WORK-SW             06030003
060400     ELSE                                                         06040003
060500         MOVE 'N'      TO WS-SAVE-PAY-VAC-AND-WORK-SW             06050003
060600     END-IF                                                       06060003
060700     MOVE CNTL-TIME-ZONE      TO WS-TIME-ZONE                     06070003
060800*                                                                 06080003
060900*    CONVERT SYSTEM TIME TO LOCAL TIME                            06090003
061000*                                                                 06100003
061100     MOVE SPACES              TO TZ-PARAMETERS                    06110003
061200     SET  TZ-IN-EASTERN-ZONE  TO TRUE                             06120003
061300*    MOVE WS-CURR-DATE-TIME   TO TZ-IN-DATE-TIME                  06130003
061400     MOVE WS-PRESENT-TIME     TO TZ-IN-DATE-TIME                  06140003
061500     MOVE WS-TIME-ZONE        TO TZ-OUT-ZONE                      06150003
061600     PERFORM P8996-TIMEZONE                                       06160003
061700     MOVE TZ-OUT-DATE-TIME    TO WS-LOCAL-DATE-TIME               06170003
061800     MOVE TZ-OUT-CE           TO WS-LOCAL-CENT                    06180003
061900*                                                                 06190003
062000*    CONVERT THE EFFECTIVE DATE/TIME TO THE EMPLOYEES TIME ZONE   06200003
062100*                                                                 06210003
062200     IF P917-EFF-DATE-TIME      NOT > '0000000000'                06220003
062300        IF P917-ACTION-DATE-TIME > '0000000000'                   06230003
062400           MOVE P917-ACTION-DATE-TIME TO P917-EFF-DATE-TIME       06240003
062500        ELSE                                                      06250003
062600           MOVE WS-LOCAL-DATE-TIME    TO P917-EFF-DATE-TIME       06260003
062700           MOVE WS-TIME-ZONE          TO P917-TIME-ZONE           06270003
062800        END-IF                                                    06280003
062900     END-IF                                                       06290003
063000     IF P917-ON-CALL                                              06300003
063100        MOVE WS-LOCAL-DATE-TIME    TO WS-EFF-DATE-TIME            06310003
063200        MOVE WS-LOCAL-CENT         TO WS-EFF-CE                   06320003
063300     ELSE                                                         06330003
063400        MOVE SPACES TO TZ-PARAMETERS                              06340003
063500        MOVE P917-TIME-ZONE        TO TZ-IN-ZONE                  06350003
063600        MOVE P917-EFF-DATE-TIME    TO TZ-IN-DATE-TIME             06360003
063700        MOVE WS-TIME-ZONE          TO TZ-OUT-ZONE                 06370003
063800        PERFORM P8996-TIMEZONE                                    06380003
063900        MOVE TZ-OUT-DATE-TIME      TO WS-EFF-DATE-TIME            06390003
064000        MOVE TZ-OUT-CE             TO WS-EFF-CE                   06400003
064100     END-IF                                                       06410003
064200*                                                                 06420003
064300     MOVE SPACES              TO WS-MISSED-CALL-FLAGS             06430003
064400     IF P917-STATUS-CODE1 NOT > SPACE                             06440003
064500        IF P917-MISSED-CALL-FUNC > SPACES                         06450003
064600           MOVE SPACES        TO WS-LAYOFF-CODE-CHECK             06460003
064700           PERFORM WITH TEST AFTER VARYING LO-TABLE-SUB           06470003
064800              FROM 1 BY 1 UNTIL LO-TABLE-SUB > LO-ARRAY-MAX       06480003
064900              OR LOC-MISSED-CALL                                  06490003
065000              MOVE WS-LO-CODE(LO-TABLE-SUB)                       06500003
065100                              TO WS-LAYOFF-CODE-CHECK             06510003
065200           END-PERFORM                                            06520003
065300           IF LOC-MISSED-CALL                                     06530003
065400              MOVE WS-LAYOFF-CODE-CHECK                           06540003
065500                              TO P917-STATUS-CODE1                06550003
065600              MOVE WS-CNTL-FILE                                   06560003
065700                              TO SAVE-CNTL-FILE                   06570003
065800              PERFORM P2010-SETUP-MISSED-CALL                     06580003
065900*-------------------------------------------------------------    06590003
066000*           FOR FASTSLOW SPAREBOARDS, A MISSED CALL PENALTY       06600003
066100*           MAY BE APPLIED TO BOTH THE FAST AND SLOW BOARDS.      06610003
066200*           SET THE FLAG, SO PROPER PROCESSING CAN OCCUR          06620003
066300*           WHEN MARKING UP TO THE EXTRABOARD.                    06630003
066400*----------------------------------------------------------PLS    06640003
066500              IF CNTL-MC-APPLY-TO-OPP-BD                          06650003
066600                 SET APPLYING-TO-OPP-BOARD TO TRUE                06660003
066700              END-IF                                              06670003
066800              MOVE SAVE-CNTL-FILE          TO WS-CNTL-FILE        06680003
066900           ELSE                                                   06690003
067000              MOVE 'P1000-3'  TO ERR-PARAGRAPH                    06700003
067100              MOVE 'STATCODE' TO ERR-KEY                          06710003
067200              PERFORM P9999-GOT-PROBLEM                           06720003
067300           END-IF                                                 06730003
067400        ELSE                                                      06740003
067500           MOVE 'P1000-4'     TO ERR-PARAGRAPH                    06750003
067600           MOVE 'STATCODE'    TO ERR-KEY                          06760003
067700           PERFORM P9999-GOT-PROBLEM                              06770003
067800        END-IF                                                    06780003
067900     END-IF                                                       06790003
068000*                                                                 06800003
068100*      SEE IF THE CHANGE OF STATUS CODE IS VALID  AND             06810003
068200*      IF THE CHANGE OF STATUS REQUIRES A CPRAIL EDB CODE OR      06820003
068300*      REQUIRES A SUPERVISORS INITIAL                             06830003
068400*                                                                 06840003
068500     PERFORM VARYING LO-TABLE-SUB FROM 1 BY 1                     06850003
068600         UNTIL LO-TABLE-SUB > LO-ARRAY-MAX                        06860003
068700            OR WS-LO-CODE(LO-TABLE-SUB) = P917-STATUS-CODE1       06870003
068800     END-PERFORM                                                  06880003
068900     IF LO-TABLE-SUB > LO-ARRAY-MAX                               06890003
069000     OR SYSTEM-ONLY-LO(LO-TABLE-SUB)                              06900003
069100     OR (P917-REG-LAYOFF AND NOT LO-OK-NORMAL(LO-TABLE-SUB) )     06910003
069200         SET P917-STATUS-CODE-ERROR TO TRUE                       06920003
069300*             'INVALID BOOKOFF / BOOKON CODE ENTERED'             06930003
069400         MOVE 'I083' TO MSGLOG-CODE                               06940003
069500         MOVE '12'      TO P917-BOOKON-REJECT-CODE                06950003
069600         PERFORM P9000-RETURN                                     06960003
069700     END-IF                                                       06970003
069800     MOVE WS-LO-CODE(LO-TABLE-SUB)                                06980003
069900                        TO WS-LAYOFF-CODE-CHECK                   06990003
070000*                                                                 07000003
070100     IF P917-STATUS-CODE2 NOT > SPACE                             07010003
070200        MOVE ZEROS TO P917-STATUS-CODE2                           07020003
070300     END-IF                                                       07030003
070400     MOVE SPACE               TO WORK-CNTLKEY                     07040003
070500     MOVE '02'                TO WK-CNTL-REC-TYPE                 07050003
070600     MOVE DIST OF WS-MSTR     TO WK-CNTL-DIST                     07060003
070700     MOVE SUB-DIST OF WS-MSTR TO WK-CNTL-SUB-DIST                 07070003
070800     MOVE WORK-CNTLKEY    TO CNTLKEY                              07080003
070900     PERFORM P8400-READ-CNTLFILE                                  07090003
071000     IF NOT SUCCESS                                               07100003
071100        MOVE 'P1000-6' TO ERR-PARAGRAPH                           07110003
071200        MOVE CNTLKEY   TO ERR-KEY                                 07120003
071300        PERFORM P9999-GOT-PROBLEM                                 07130003
071400     END-IF                                                       07140003
071500     MOVE CNTL-TIME-ZONE      TO WS-TIME-ZONE                     07150003
071600*                                                                 07160003
071700*    GET CURRENT STATUS TABLE INFO AND SAVE                       07170003
071800*                                                                 07180003
071900     MOVE SPACES                   TO P956-COMMAREA-PARMS         07190003
072000     SET P956-GET-CNTL-STATUS-REASON                              07200003
072100                                   TO TRUE                        07210003
072200     MOVE LAYOFF-CODE-1            TO P956-STATUS-CODE            07220003
072300     MOVE LAYOFF-EM-CODE           TO P956-REASON-CODE            07230003
072400     MOVE DIST     OF WS-MSTR      TO P956-DIST                   07240003
072500     MOVE SUB-DIST OF WS-MSTR      TO P956-SDIST                  07250003
072600     MOVE CRAFT OF WS-MSTR         TO P956-CC                     07260003
072700     IF TEMPORARY-ASGNMT > SPACE                                  07270003
072800        MOVE TEMPORARY-ASGNMT-FLAG TO P956-ASGN-TYPE              07280003
072900        MOVE TA-1                  TO P956-ASGN                   07290003
073000        MOVE TA-DIST               TO P956-DIST                   07300003
073100        MOVE TA-SUB-DIST           TO P956-SDIST                  07310003
073200        IF TEMP-ASGN-XB                                           07320003
073300           MOVE TA-CC              TO P956-XB                     07330003
073400        END-IF                                                    07340003
073500     ELSE                                                         07350003
073600        IF NORMAL-ASGNMT > SPACES                                 07360003
073700           MOVE NORMAL-ASGNMT-FLAG TO P956-ASGN-TYPE              07370003
073800           MOVE NA-1               TO P956-ASGN                   07380003
073900           MOVE NA-DIST            TO P956-DIST                   07390003
074000           MOVE NA-SUB-DIST        TO P956-SDIST                  07400003
074100           IF NORM-ASGN-XB                                        07410003
074200              MOVE NA-CC           TO P956-XB                     07420003
074300           END-IF                                                 07430003
074400        END-IF                                                    07440003
074500     END-IF                                                       07450003
074600     PERFORM P9840-RETRIEVE-CNTL-INFO                             07460003
074700     IF P956-ERROR-FOUND                                          07470003
074800        MOVE 'P1000-5'              TO ERR-PARAGRAPH              07480003
074900        STRING 'P956 LINK...ERROR-FLAG='                          07490003
075000               P956-ERROR-FLAG                                    07500003
075100               DELIMITED BY SIZE  INTO ERR-SENTENCE               07510003
075200        MOVE P956-INPUT-PARMS       TO ERR-KEY                    07520003
075300        PERFORM P9999-GOT-PROBLEM                                 07530003
075400     END-IF                                                       07540003
075500     MOVE P956-ST-RSN-MU-TIME        TO WS-LAST-MU-FROM-HRMN      07550003
075600     MOVE P956-ST-RSN-SUPV-INIT-X    TO WS-LAST-SUPV-INIT-X       07560003
075700     MOVE P956-ST-RSN-REST-OK-X      TO WS-LAST-REST-OK-X         07570003
075800     MOVE P956-ST-RSN-AUTO-EXEC-TASK TO WS-LAST-AUTO-EXEC-TASK    07580003
075900     MOVE P956-ST-RSN-MU-TIME        TO WS-LAST-MU-TIME           07590003
076000     MOVE P956-ST-RSN-MU-PLUS-MINUS  TO WS-LAST-MU-PLUS-MINUS     07600003
076100     MOVE P956-ST-RSN-AJ-MU-LEAD-TIME                             07610003
076200                                     TO WS-LAST-AJ-MU-LEAD-TIME   07620003
076300     MOVE P956-ST-RSN-RETAIN-POS-POOL-X                           07630003
076400                                     TO WS-LAST-RETAIN-POS-POOL-X 07640003
076500     MOVE P956-ST-COUNT-PERSONAL-FLAG                             07650003
076600                                     TO WS-LAST-COUNT-PERS-FLAG   07660003
076700*                                                                 07670003
076800*    GET NEW STATUS TABLE INFO                                    07680003
076900*                                                                 07690003
077000     MOVE SPACES                   TO P956-COMMAREA-PARMS         07700003
077100     SET P956-GET-CNTL-STATUS-REASON                              07710003
077200                                   TO TRUE                        07720003
077300     MOVE P917-STATUS-CODE1        TO P956-STATUS-CODE            07730003
077400     MOVE P917-EDB-CODE            TO P956-REASON-CODE            07740003
077500     PERFORM P9840-RETRIEVE-CNTL-INFO                             07750003
077600     IF P956-STATUS-REASON-CODE (1) > SPACES                      07760003
077700        PERFORM VARYING I FROM 1 BY 1                             07770003
077800          UNTIL I > 30                                            07780003
077900            OR (P956-STATUS-REASON-CODE (I) = P917-EDB-CODE       07790003
078000           AND  P956-STATUS-REASON-CODE (I) > SPACES)             07800003
078100        END-PERFORM                                               07810003
078200        IF  I > 30                                                07820003
078300            SET P917-EDB-CODE-ERROR  TO TRUE                      07830003
078400            IF P917-ON-CALL                                       07840003
078500               IF P917-EDB-CODE > SPACES                          07850003
078600*                 'INVALID REASON CODE'                           07860003
078700                  MOVE 'R097'        TO MSGLOG-CODE               07870003
078800               ELSE                                               07880003
078900*                 'REASON CODE REQUIRED'                          07890003
079000                  MOVE 'R071'        TO MSGLOG-CODE               07900003
079100               END-IF                                             07910003
079200            ELSE                                                  07920003
079300*             'INVALID RC CODE ENTERED, VALID CODES LISTED ABOVE' 07930003
079400               MOVE 'I327'        TO MSGLOG-CODE                  07940003
079500               PERFORM P9000-RETURN                               07950003
079600            END-IF                                                07960003
079700        END-IF                                                    07970003
079800     ELSE                                                         07980003
079900        IF  P917-EDB-CODE > SPACES                                07990003
080000            SET P917-EDB-CODE-ERROR  TO TRUE                      08000003
080100*           MOVE 'ECC CODE NOT ALLOWED FOR THIS STATUS'           08010003
080200            MOVE 'E089'        TO MSGLOG-CODE                     08020003
080300            PERFORM P9000-RETURN                                  08030003
080400        END-IF                                                    08040003
080500     END-IF                                                       08050003
080600     IF P917-STATUS-CODE2 NOT > SPACE                             08060003
080700        MOVE ZEROS TO P917-STATUS-CODE2                           08070003
080800     END-IF                                                       08080003
080900     MOVE P917-STATUS-CODE1        TO P956-STATUS-CODE            08090003
081000     MOVE P917-EDB-CODE            TO P956-REASON-CODE            08100003
081100     MOVE DIST     OF WS-MSTR      TO P956-DIST                   08110003
081200     MOVE SUB-DIST OF WS-MSTR      TO P956-SDIST                  08120003
081300     MOVE CRAFT OF WS-MSTR         TO P956-CC                     08130003
081400     IF TEMPORARY-ASGNMT > SPACE                                  08140003
081500        MOVE TEMPORARY-ASGNMT-FLAG TO P956-ASGN-TYPE              08150003
081600        MOVE TA-1                  TO P956-ASGN                   08160003
081700        MOVE TA-DIST               TO P956-DIST                   08170003
081800        MOVE TA-SUB-DIST           TO P956-SDIST                  08180003
081900        IF TEMP-ASGN-XB                                           08190003
082000           MOVE TA-CC              TO P956-XB                     08200003
082100        END-IF                                                    08210003
082200     ELSE                                                         08220003
082300        IF NORMAL-ASGNMT > SPACES                                 08230003
082400           MOVE NORMAL-ASGNMT-FLAG TO P956-ASGN-TYPE              08240003
082500           MOVE NA-1               TO P956-ASGN                   08250003
082600           MOVE NA-DIST            TO P956-DIST                   08260003
082700           MOVE NA-SUB-DIST        TO P956-SDIST                  08270003
082800           IF NORM-ASGN-XB                                        08280003
082900              MOVE NA-CC           TO P956-XB                     08290003
083000           END-IF                                                 08300003
083100        END-IF                                                    08310003
083200     END-IF                                                       08320003
083300     MOVE P956-ASGN                TO SAVE-ASGN                   08330003
083400     PERFORM P9840-RETRIEVE-CNTL-INFO                             08340003
083500     IF P956-ERROR-FOUND                                          08350003
083600       SET P917-STATUS-CODE-ERROR TO TRUE                         08360003
083700*         'NO STATUS/REASON CONTROL RECORD FOR CODE '             08370003
083800       MOVE 'N140'        TO MSGLOG-CODE                          08380003
083900        PERFORM P9000-RETURN                                      08390003
084000     END-IF                                                       08400003
084100     IF P917-EDB-CODE NOT > SPACES                                08410003
084200        IF P956-STATUS-REASON-CODE (2) = SPACES                   08420003
084300           MOVE P956-STATUS-REASON-CODE (1)                       08430003
084400                                     TO P917-EDB-CODE             08440003
084500        END-IF                                                    08450003
084600     END-IF                                                       08460003
084700*                                                                 08470003
084800     IF EIBTRNID = P16-TRAN                                       08480003
084900        OR EIBTRNID = P00I-TRAN                                   08490003
085000        CONTINUE                                                  08500003
085100     ELSE                                                         08510003
085200        IF P917-STATUS-CODE1 NOT = '*'                            08520003
085300           IF P917-ON-CALL AND NOT P956-ST-RSN-ON-CALL            08530003
085400              SET P917-STATUS-CODE-ERROR TO TRUE                  08540003
085500*             'INVALID BOOKOFF/ON CODE - ST/RSN MUST BE ONCALL'   08550003
085600               MOVE 'I393'              TO MSGLOG-CODE            08560003
085700*              MOVE '12'                TO P917-BOOKON-REJECT-CODE08570003
085800               PERFORM P9000-RETURN                               08580003
085900           END-IF                                                 08590003
086000        END-IF                                                    08600003
086100     END-IF                                                       08610003
086200*                                                                 08620003
086300     IF P917-SUPV-INIT NOT > SPACE                                08630003
086400        IF P956-ST-RSN-SUPV-INIT                                  08640003
086500*MET    IF P956-ST-RSN-SUPV-INIT OR                               08650003
086600*          WS-LAST-SUPV-INIT                                      08660003
086700            SET P917-SUPV-INIT-ERROR TO TRUE                      08670003
086800*           MOVE 'SUPERVISORS INITIALS ARE REQUIRED'              08680003
086900            MOVE 'S109' TO MSGLOG-CODE                            08690003
087000            PERFORM P9000-RETURN                                  08700003
087100        END-IF                                                    08710003
087200     END-IF                                                       08720003
087300*                                                                 08730003
087400*    SEE IF THE CHANGE OF STATUS REQUIRES HIGH LEVEL AUTHORITY    08740003
087500*                                                                 08750003
087600     IF P956-ST-RSN-HL-AUTH                                       08760003
087700        PERFORM P1200-CHECK-AUTHORITY                             08770003
087800        IF NOT HIGH-LEVEL-AUTHORIZED                              08780003
087900           SET P917-STATUS-CODE-ERROR TO TRUE                     08790003
088000*          MOVE 'YOU ARE NOT AUTHORIZED TO USE THIS STATUS CODE'  08800003
088100           MOVE 'Y014' TO MSGLOG-CODE                             08810003
088200           PERFORM P9000-RETURN                                   08820003
088300        END-IF                                                    08830003
088400     END-IF                                                       08840003
088500******************************************************************08850003
088600*    WHEN PUTTING EMPLOYEE ON VACATION, CHECK FOR SPECIAL VACATION08860003
088700*    CALCULATION FLAG ON THE EMPLOYEE VACATION RECORD.            08870003
088800*       READ THE EMPLOYEE VACATION RECORD                         08880003
088900******************************************************************08890003
089000     IF P917-STATUS-CODE1 = 'V'                                   08900003
089100        PERFORM P1030-VAC-SPEC-CALC                               08910003
089200     END-IF                                                       08920003
089300*                                                                 08930003
089400*                                                                 08940003
089500*    SEE IF THE CHANGE OF STATUS REQUIRES NBR OF DAYS             08950003
089600*                                                                 08960003
089700     IF P917-NBR-DAYS                NOT > '00'                   08970003
089800        IF P956-ST-RSN-NBR-DAYS-REQ                               08980003
089900           IF  P917-EFF-DATE-TIME        > SPACES                 08990003
090000           AND P917-EXP-DATE-TIME        > SPACES                 09000003
090100              MOVE ZERO                 TO DATE-CONVERSION-PARMS  09010003
090200              SET  PARM-DIFF            TO TRUE                   09020003
090300              MOVE P917-EFF-DATE        TO PARM-PRI-DATE-GREG     09030003
090400              MOVE P917-EFF-TIME        TO PARM-PRI-HRMN          09040003
090500              MOVE P917-EXP-DATE        TO PARM-SEC-DATE-GREG     09050003
090600              MOVE P917-EXP-TIME        TO PARM-SEC-HRMN          09060003
090700              EXEC CICS LINK                                      09070003
090800                        PROGRAM (P903-PGM)                        09080003
090900                        COMMAREA(DATE-CONVERSION-PARMS)           09090003
091000                        LENGTH  (P903-LGTH)                       09100003
091100                        RESP    (WS-RESPONSE)                     09110003
091200              END-EXEC                                            09120003
091300              MOVE WS-RESPONSE          TO FILE-STATUS            09130003
091400              IF NOT SUCCESS                                      09140003
091500                 MOVE 'P1000-7'         TO ERR-PARAGRAPH          09150003
091600                 PERFORM P9999-GOT-PROBLEM                        09160003
091700              END-IF                                              09170003
091800              IF PARM-RES-TOT-DAYS       > ZEROES                 09180003
091900                 IF VAC-USE-CALC                                  09190003
092000                    COMPUTE PARM-RES-TOT-DAYS ROUNDED =           09200003
092100                           (PARM-RES-TOT-DAYS + 2) / 7 * 5        09210003
092200                 END-IF                                           09220003
092300                 MOVE PARM-RES-TOT-DAYS (4:2) TO P917-NBR-DAYS    09230003
092400              ELSE                                                09240003
092500                 SET P917-NBR-DAYS-ERROR  TO TRUE                 09250003
092600*                STRING 'NUMBER OF DAYS IS REQUIRED '             09260003
092700*                'FOR THIS STATUS CODE'                           09270003
092800                  MOVE 'N046' TO MSGLOG-CODE                      09280003
092900                 PERFORM P9000-RETURN                             09290003
093000              END-IF                                              09300003
093100           ELSE                                                   09310003
093200              SET P917-NBR-DAYS-ERROR  TO TRUE                    09320003
093300*             STRING 'NUMBER OF DAYS IS REQUIRED '                09330003
093400*             'FOR THIS STATUS CODE'                              09340003
093500               MOVE 'N046' TO MSGLOG-CODE                         09350003
093600              PERFORM P9000-RETURN                                09360003
093700           END-IF                                                 09370003
093710           IF P917-STATUS-CODE1 = 'P'                             09371003
093720              AND P917-PAID-DAYS NOT > SPACES                     09372003
093730              SET P917-NBR-DAYS-ERROR TO TRUE                     09373003
093740*                NUMBER OF DAYS IS REQUIRED                       09374003
093750              MOVE 'N046'         TO MSGLOG-CODE                  09375003
093760              PERFORM P9000-RETURN                                09376003
093770           END-IF                                                 09377003
093780        END-IF                                                    09378003
093790     ELSE                                                         09379003
093800        IF  P917-EFF-DATE-TIME           > SPACES                 09380003
093900        AND P917-EXP-DATE-TIME       NOT > SPACES                 09390003
094000           MOVE ZERO                    TO DATE-CONVERSION-PARMS  09400003
094100           SET  PARM-ADD                TO TRUE                   09410003
094200           MOVE P917-EFF-DATE           TO PARM-PRI-DATE-GREG     09420003
094300           MOVE P917-EFF-TIME           TO PARM-PRI-HRMN          09430003
094400           MOVE P917-NBR-DAYS           TO PARM-SEC-DATE-GREG     09440003
094500           IF VAC-USE-CALC                                        09450003
094600              COMPUTE PARM-SEC-DATE-GREG ROUNDED =                09460003
094700                     (PARM-SEC-DATE-GREG / 5 * 7) - 2             09470003
094800           END-IF                                                 09480003
094900           EXEC CICS LINK                                         09490003
095000                     PROGRAM (P903-PGM)                           09500003
095100                     COMMAREA(DATE-CONVERSION-PARMS)              09510003
095200                     LENGTH  (P903-LGTH)                          09520003
095300                     RESP    (WS-RESPONSE)                        09530003
095400           END-EXEC                                               09540003
095500           MOVE WS-RESPONSE             TO FILE-STATUS            09550003
095600           IF NOT SUCCESS                                         09560003
095700              MOVE 'P1000-10'           TO ERR-PARAGRAPH          09570003
095800              PERFORM P9999-GOT-PROBLEM                           09580003
095900           END-IF                                                 09590003
095910           IF PARM-RES-DATE-GREG         > ZEROES                 09591003
095920              MOVE PARM-RES-DATE-GREG   TO P917-EXP-DATE          09592003
095930              MOVE PARM-RES-HRMN        TO P917-EXP-TIME          09593003
095940           END-IF                                                 09594003
095950        END-IF                                                    09595003
095960     END-IF                                                       09596003
095970*                                                                 09597003
095980*    SEE IF THE CHANGE OF STATUS REQUIRES EXPIRATION DATE         09598003
095990*                                                                 09599003
096000     IF P917-EXP-DATE-TIME NOT > SPACES                           09600003
096100        IF P956-ST-RSN-EXP-DATE-REQ                               09610003
096200           IF P917-EFF-DATE-TIME > SPACES AND                     09620003
096300              P917-NBR-DAYS > '00'                                09630003
096400              MOVE ZERO    TO DATE-CONVERSION-PARMS               09640003
096500              SET PARM-ADD            TO TRUE                     09650003
096600              MOVE P917-EFF-DATE      TO PARM-PRI-DATE-GREG       09660003
096700              MOVE P917-EFF-TIME      TO PARM-PRI-HRMN            09670003
096800              MOVE P917-NBR-DAYS      TO PARM-SEC-DATE-GREG       09680003
096900              EXEC CICS LINK                                      09690003
097000                        PROGRAM(P903-PGM)                         09700003
097100                        COMMAREA(DATE-CONVERSION-PARMS)           09710003
097200                        LENGTH(P903-LGTH)                         09720003
097300                        RESP(WS-RESPONSE)                         09730003
097400              END-EXEC                                            09740003
097500              MOVE WS-RESPONSE TO FILE-STATUS                     09750003
097600              IF NOT SUCCESS                                      09760003
097700                 MOVE 'P1000-8' TO ERR-PARAGRAPH                  09770003
097800                 PERFORM P9999-GOT-PROBLEM                        09780003
097900              END-IF                                              09790003
098000              IF PARM-RES-DATE-GREG > ZEROES                      09800003
098100                 MOVE PARM-RES-DATE-GREG TO P917-EXP-DATE         09810003
098200                 MOVE PARM-RES-HRMN TO P917-EXP-TIME              09820003
098300              ELSE                                                09830003
098400                 SET P917-EXP-DATE-ERROR  TO TRUE                 09840003
098500*                STRING 'EXPIRATION DATE IS REQUIRED '            09850003
098600*                'FOR THIS STATUS CODE'                           09860003
098700                  MOVE 'E231' TO MSGLOG-CODE                      09870003
098800                 PERFORM P9000-RETURN                             09880003
098900              END-IF                                              09890003
099000           ELSE                                                   09900003
099100              SET P917-EXP-DATE-ERROR  TO TRUE                    09910003
099200*             STRING 'EXPIRATION DATE IS REQUIRED '               09920003
099300*             'FOR THIS STATUS CODE'                              09930003
099400               MOVE 'E231' TO MSGLOG-CODE                         09940003
099500              PERFORM P9000-RETURN                                09950003
099600           END-IF                                                 09960003
099700        END-IF                                                    09970003
099800     ELSE                                                         09980003
099900        IF P917-NBR-DAYS NOT > '00'                               09990003
100000           SET PARM-DIFF           TO TRUE                        10000003
100100           MOVE P917-EFF-DATE      TO PARM-PRI-DATE-GREG          10010003
100200           MOVE P917-EFF-TIME      TO PARM-PRI-HRMN               10020003
100300           MOVE P917-EXP-DATE      TO PARM-SEC-DATE-GREG          10030003
100400           MOVE P917-EXP-TIME      TO PARM-SEC-HRMN               10040003
100500           EXEC CICS LINK                                         10050003
100600                     PROGRAM(P903-PGM)                            10060003
100700                     COMMAREA(DATE-CONVERSION-PARMS)              10070003
100800                     LENGTH(P903-LGTH)                            10080003
100900                     RESP(WS-RESPONSE)                            10090003
101000           END-EXEC                                               10100003
101100           MOVE WS-RESPONSE TO FILE-STATUS                        10110003
101200           IF NOT SUCCESS                                         10120003
101300              MOVE 'P1000-9' TO ERR-PARAGRAPH                     10130003
101400              PERFORM P9999-GOT-PROBLEM                           10140003
101500           END-IF                                                 10150003
101600           IF PARM-RES-DATE-GREG > ZEROES                         10160003
101700              MOVE PARM-RES-TOT-DAYS TO P917-NBR-DAYS             10170003
101800           END-IF                                                 10180003
101900        END-IF                                                    10190003
102000     END-IF                                                       10200003
102100*                                                                 10210003
102200*    SEE IF THE CHANGE OF STATUS REQUIRES MILES                   10220003
102300*                                                                 10230003
102400     IF P956-ST-RSN-MILES-REQ                                     10240003
102500        IF EMP-MILES-DATE NUMERIC                                 10250003
102600         AND EMP-MILES-DATE-NUM > 0                               10260003
102700         AND EMP-MILES-DATE-NUM < 32                              10270003
102800           MOVE WS-SYS-YR          TO WORK-EXP-YR                 10280003
102900           MOVE WS-SYS-MO          TO WORK-EXP-MO                 10290003
103000           MOVE EMP-MILES-DATE-NUM TO P917-EXP-DY                 10300003
103100           IF EMP-MILES-DATE-NUM < WS-SYS-DY                      10310003
103200              ADD 1 TO WORK-EXP-MO                                10320003
103300              IF WORK-EXP-MO > 12                                 10330003
103400                 MOVE 01 TO WORK-EXP-MO                           10340003
103500                 ADD  01 TO WORK-EXP-YR                           10350003
103600              END-IF                                              10360003
103700           ELSE                                                   10370003
103800              IF WORK-EXP-MO = 2                                  10380003
103900               AND P917-EXP-DY > '28'                             10390003
104000                 MOVE WORK-EXP-YR TO DE-YYMMDD-YY                 10400003
104100                 DIVIDE DE-YYMMDD-YY BY 4                         10410003
104200                        GIVING DE-TEMP-YEAR                       10420003
104300                        REMAINDER DE-LEAP-YEAR-FLAG               10430003
104400                 IF DE-LEAP-YEAR                                  10440003
104500                  AND P917-EXP-DY = '29'                          10450003
104600                    CONTINUE                                      10460003
104700                 ELSE                                             10470003
104800                    MOVE '01' TO P917-EXP-DY                      10480003
104900                    MOVE 3 TO WORK-EXP-MO                         10490003
105000                 END-IF                                           10500003
105100              END-IF                                              10510003
105200           END-IF                                                 10520003
105300           MOVE WORK-EXP-YR        TO P917-EXP-YR                 10530003
105400           MOVE WORK-EXP-MO        TO P917-EXP-MO                 10540003
105500        ELSE                                                      10550003
105600           MOVE EMP-MILES-DATE     TO P917-EXP-DATE               10560003
105700        END-IF                                                    10570003
105800        MOVE '0001'                TO P917-EXP-TIME               10580003
105900     END-IF                                                       10590003
106000*                                                                 10600003
106100*                                                                 10610003
106200*    SEE IF THE CHANGE OF STATUS IS PROHIBITED                    10620003
106300*                                                                 10630003
106400     IF P956-ST-RSN-REFUSE-LAYOFF                                 10640003
106500        PERFORM P4000-WRITE-REF-BKOFF-HIST                        10650003
106600        SET P917-STATUS-CODE-ERROR TO TRUE                        10660003
106700*       STRING 'BOOK-OFF FOR THIS STATUS/REASON '                 10670003
106800*              'CURRENTLY NOT ALLOWED'                            10680003
106900        MOVE 'B163' TO MSGLOG-CODE                                10690003
107000        PERFORM P9000-RETURN                                      10700003
107100     END-IF                                                       10710003
107200*                                                                 10720003
107300*    IF SUPV LOCKING EMP OFF, MAKE SURE EMPLOYEE IS OFF           10730003
107400*       FOR 'ON DUTY INJURY'.                                     10740003
107500*                                                                 10750003
107600     IF P917-STATUS-CODE1 = '*'                                   10760003
107700***     IF NOT LOC-ON-DUTY-INJURY                                 10770003
107710        IF NOT ON-DUTY-INJURY                                     10771003
107720           SET P917-STATUS-CODE-ERROR TO TRUE                     10772003
107721*          MOVE 'EMPLOYEE MUST BE OFF "ON DUTY INJURY" BEFORE USIN10772103
107722*               'G THIS CODE'   TO P917-ERRORMSG                  10772203
107723           MOVE 'E073' TO MSGLOG-CODE                             10772303
107724           PERFORM P9000-RETURN                                   10772403
107725        ELSE                                                      10772503
107726           IF EMP-HI-LVL-OK                                       10772603
107727              SET P917-STATUS-CODE-ERROR TO TRUE                  10772703
107728*             MOVE 'EMPLOYEE IS ALREADY IN LOCKED STATUS'         10772803
107729              MOVE 'E091' TO MSGLOG-CODE                          10772903
107730              PERFORM P9000-RETURN                                10773003
107740           END-IF                                                 10774003
107750        END-IF                                                    10775003
107760     END-IF                                                       10776003
107770*                                                                 10777003
107780*    CHECK TO SEE IF MARKUP FROM ON DUTY INJURY AND               10778003
107790*    NEEDS HI-LEVEL AUTHORIZATION                                 10779003
107800*                                                                 10780003
107900     IF  LOC-AVAILABLE                                            10790003
108000     AND EMP-HI-LVL-OK                                            10800003
108100        PERFORM P1200-CHECK-AUTHORITY                             10810003
108200        IF NOT HIGH-LEVEL-AUTHORIZED                              10820003
108300           SET P917-STATUS-CODE-ERROR TO TRUE                     10830003
108400*          MOVE 'MARKUP RESTRICTED - CONTACT SUPERVISOR'          10840003
108500           MOVE 'B007' TO MSGLOG-CODE                             10850003
108600           PERFORM P9000-RETURN                                   10860003
108700        END-IF                                                    10870003
108800     END-IF                                                       10880003
108900*                                                                 10890003
109000*    EDIT THE DATE & TIME                                         10900003
109100*                                                                 10910003
109200     PERFORM P1100-EDIT-DATE-TIME                                 10920003
109300*                                                                 10930003
109400     MOVE P917-STATUS-CODE1          TO WS-LAYOFF-CODE-CHECK      10940003
109500*                                                                 10950003
109600*    IF EMPLOYEE IS MARKING UP TO AVAILABLE FROM A STATUS OTHER   10960003
109700*    THAN 'B' (WORKING), THEN MAKE SURE IT IS NOT A GOTRAIN       10970003
109800*    EMPLOYEE TRYING TO MARKUP BETWEEN SHIFTS.                    10980003
109900*                                                                 10990003
110000     IF P917-STATUS-CODE1 = 'A' AND                               11000003
110100        NOT WORKING OF LAYOFF-CODE-1                              11010003
110200        PERFORM P1150-CHECK-FOR-GOTRAIN-SPLIT                     11020003
110300*                                                                 11030003
110400*    CHECK TO SEE IF MARKUP FOR ASGN JOB IS WITHIN LEAD TIME      11040003
110500*                                                                 11050003
110600        IF  SAVE-ASGN-TYPE = 'A'                                  11060003
110700        AND WS-LAST-AJ-MU-LEAD-TIME > SPACES                      11070003
110800           MOVE ZERO       TO DATE-CONVERSION-PARMS               11080003
110900           SET PARM-SUBTRACT          TO TRUE                     11090003
111000           MOVE P917-EXP-DATE         TO PARM-PRI-DATE-GREG       11100003
111100           MOVE SAVE-JSK1-ASGN-START-TIME                         11110003
111200                                         TO PARM-PRI-HRMN         11120003
111300           MOVE WS-LAST-AJ-MU-LEAD-TIME                           11130003
111400                                         TO PARM-SEC-HRMN         11140003
111500           EXEC CICS LINK                                         11150003
111600                     PROGRAM(P903-PGM)                            11160003
111700                     COMMAREA(DATE-CONVERSION-PARMS)              11170003
111800                     LENGTH(P903-LGTH)                            11180003
111900                     RESP(WS-RESPONSE)                            11190003
112000           END-EXEC                                               11200003
112100           MOVE WS-RESPONSE TO FILE-STATUS                        11210003
112200           IF NOT SUCCESS                                         11220003
112300              MOVE 'P1000-9' TO ERR-PARAGRAPH                     11230003
112400              PERFORM P9999-GOT-PROBLEM                           11240003
112500           END-IF                                                 11250003
112600           MOVE PARM-RES-DATE-GREG    TO WS-MU-DATE               11260003
112700           MOVE PARM-RES-TIME         TO WS-MU-TIME               11270003
112800           PERFORM P1200-CHECK-AUTHORITY                          11280003
112900           IF (NOT HIGH-LEVEL-AUTHORIZED)                         11290003
113000            AND (WS-MU-DATE-TIME NOT > P917-EFF-DATE-TIME)        11300003
113100              SET P917-STATUS-CODE-ERROR TO TRUE                  11310003
113200*             MOVE 'BOOKON TIME RESTRICTION - CONTACT SUPV'       11320003
113300              MOVE 'M254'             TO MSGLOG-CODE              11330003
113400              PERFORM P9000-RETURN                                11340003
113500           END-IF                                                 11350003
113600        END-IF                                                    11360003
113700     END-IF                                                       11370003
113800*                                                                 11380003
113900*    IF THIS CHANGE OF STATUS IS IN THE FUTURE, WE CAN DO NO      11390003
114000*    MORE EDITING AT THIS TIME, SO PLACE THE RECORD IN THE        11400003
114100*    TASK FILE AND RETURN TO THE CALLING PROGRAM.                 11410003
114200*                                                                 11420003
114300*    EXCEPTION: IF THE PERSON IS PENDING WAITING TURN AND         11430003
114400*    MARKING UP FROM EMPLOYEE TRACKING, CLEAR THE PENDING         11440003
114500*    WAITING TURN FLAG AND DO NOT WRITE A TASK RECORD.            11450003
114600*                                                                 11460003
114700*    IF THE CALLER PROCESSES A CHANGE OF STATUS AT EXACTLY        11470003
114800*    MIDNIGHT, LOCAL TIME, THEN WE GET THE INFAMOUS               11480003
114900*    MIDNIGHT CALLER BUG: WS-LOCAL-DATE-TIME IS MOVED TO          11490003
115000*    WS-EFF-DATE-TIME AND IF WS-EFF-TIME = '0000' THEN '0001'     11500003
115100*    IS MOVED TO WS-EFF-TIME. THIS CAUSES THE FOLLOWING           11510003
115200*    DATE-TIMES TO BE COMPARED INCORRECTLY AND SUBSEQUENTLY       11520003
115300*    DOES NOT PROCESS THE CHANGE OF STATUS CORRECTLY.             11530003
115400*                                                                 11540003
115500*    IF  WS-EFF-DATE-TIME > WS-LOCAL-DATE-TIME                    11550003
115600*                                                                 11560003
115700     SET NOT-FUTURE-EFF-TIME         TO TRUE                      11570003
115800     MOVE ZEROS                      TO WS-COMPARE-DATE-TIME-CENT 11580003
115900     IF WS-LOCAL-TIME = 0000                                      11590003
116000        MOVE WS-LOCAL-DATE-TIME-CENT TO WS-COMPARE-DATE-TIME-CENT 11600003
116100        MOVE 0001                    TO WS-COMPARE-TIME           11610003
116200        IF WS-EFF-DATE-TIME-CE > WS-COMPARE-DATE-TIME-CENT        11620003
116300        AND NOT P917-ADVANCE-PAY                                  11630003
116400        AND NOT P917-PAY-IN-LIEU                                  11640003
116500            SET FUTURE-EFF-TIME      TO TRUE                      11650003
116600        END-IF                                                    11660003
116700     END-IF                                                       11670003
116800     IF NOT-FUTURE-EFF-TIME                                       11680003
116900     AND WS-COMPARE-DATE-TIME-CENT NOT > ZEROS                    11690003
117000        IF WS-EFF-DATE-TIME-CE > WS-LOCAL-DATE-TIME-CENT          11700003
117100        AND NOT P917-ADVANCE-PAY                                  11710003
117200        AND NOT P917-PAY-IN-LIEU                                  11720003
117300            SET FUTURE-EFF-TIME      TO TRUE                      11730003
117400        END-IF                                                    11740003
117500     END-IF                                                       11750003
117600                                                                  11760003
117700     IF FUTURE-EFF-TIME AND P917-VRU-FUNCTION NOT > SPACES        11770003
117800        IF P917-MKUP-FROM-TRK                                     11780003
117900           AND (MSTR-PENDED-WAIT-TURN                             11790003
118000           OR (NORMAL-ASGNMT NOT > SPACES                         11800003
118100           OR NORM-ASGN-AJ))                                      11810003
118200           IF MSTR-PENDED-WAIT-TURN                               11820003
118300              PERFORM P1025-CLEAR-PEND-WTRN                       11830003
118400           END-IF                                                 11840003
118500        ELSE                                                      11850003
118600           MOVE SPACES                  TO P927-COMMAREA-PARMS    11860003
118700**         IF P956-STATUS-CODE NOT = 'A'                          11870003
118800**            IF P917-EXP-DATE-TIME > SPACES OR                   11880003
118900**               P956-ST-RSN-EXP-DATE-REQ OR                      11890003
119000**               P956-ST-RSN-NBR-DAYS-REQ                         11900003
119100**               PERFORM P3500-FUTURE-MARKUP-DUEBACK              11910003
119200**            END-IF                                              11920003
119300**         END-IF                                                 11930003
119400           SET P927-CHG-STATUS-FUNCTION TO TRUE                   11940003
119500           MOVE P917-EFF-DATE-TIME      TO P927-EFF-DATE-TIME     11950003
119600           MOVE P917-TIME-ZONE          TO P927-TIME-ZONE         11960003
119700           MOVE DIST OF WS-MSTR         TO P927-DIST              11970003
119800           MOVE SUB-DIST OF WS-MSTR     TO P927-SUB-DIST          11980003
119900           MOVE EMP-NBR  OF WS-MSTR     TO P927-CS-EMP-NO         11990003
120000           MOVE P917-STATUS-CODE        TO P927-CS-LO-CODE        12000003
120100           MOVE P917-SUPV-INIT          TO P927-CS-SUPV-INIT      12010003
120200           MOVE P917-EDB-CODE           TO P927-CS-EDB-CODE       12020003
120300           MOVE P917-NBR-DAYS           TO P927-CS-NUM-DAYS       12030003
120400           MOVE P917-PAID-DAYS          TO P927-CS-PAID-DAYS      12040003
120500           MOVE P917-EXP-DATE-TIME      TO P927-CS-EXP-DATE-TIME  12050003
120600           MOVE P917-REST               TO P927-CS-REST           12060003
120700           PERFORM P8800-WRITE-TASK                               12070003
120800                                                                  12080003
120900* TJR SET UP P943 COMMAREA TO WRITE EMP HISTORY REC               12090003
121000                                                                  12100003
121100           MOVE SPACES TO P943-COMMAREA-PARMS                     12110003
121200           IF P917-STATUS-CODE1 = 'A'                             12120003
121300*                  'FUTURE  " BOOKON "  HAS BEEN RECORDED'        12130003
121400              MOVE 'F011' TO MSGLOG-CODE                          12140003
121500              SET P943-PENDING-MARKUP-FUN  TO TRUE                12150003
121600              MOVE LAYOFF-CODE  TO P943-LO-CODE                   12160003
121700              MOVE LAYOFF-EM-CODE TO P943-FUN37-PREV-ECC-CODE     12170003
121800              IF P917-REST > SPACES                               12180003
121900                 MOVE P917-REST TO P943-FUN37-ADD-REST            12190003
122000              END-IF                                              12200003
122100              IF P917-VRU-CONF-NUMBER > SPACES                    12210003
122200                 MOVE P917-VRU-CONF-NUMBER                        12220003
122300                   TO P943-FUN37-VRU-NUMBER                       12230003
122400              END-IF                                              12240003
122500              IF WS-BEFORE-STATUS = 'C' AND                       12250003
122600                 LAYOFF-CODE-1 = 'A' AND                          12260003
122700                 UNAVAILABLE-LAST-FLAG NOT = 'C'                  12270003
122800                 SET P943-FUN37-END-NTFY-HOLD TO TRUE             12280003
122900              END-IF                                              12290003
123000           ELSE                                                   12300003
123100*                  'FUTURE  " BOOKOFF "  HAS BEEN RECORDED'       12310003
123200              MOVE 'F012' TO MSGLOG-CODE                          12320003
123300              SET P943-PENDING-LAYOFF-FUN  TO TRUE                12330003
123400              MOVE P917-STATUS-CODE  TO P943-LO-CODE              12340003
123500              MOVE LAYOFF-CODE-1   TO P943-FUN36-PREV-LO-CODE     12350003
123600              MOVE LAYOFF-EM-CODE  TO P943-FUN36-PREV-ECC-CODE    12360003
123700           END-IF                                                 12370003
123800           PERFORM P2005-WRITE-EMPLOYEE-HISTORY                   12380003
123900        END-IF                                                    12390003
124000        PERFORM P9000-RETURN                                      12400003
124100     END-IF                                                       12410003
124200*                                                                 12420003
124300     IF  WORKING OF LAYOFF-CODE-1                                 12430003
124400     AND NOT P917-ADVANCE-PAY                                     12440003
124500     AND NOT P917-PAY-IN-LIEU                                     12450003
124600        IF  P917-STATUS-CODE1     = 'A'                           12460003
124700        AND UNAVAILABLE-LAST-FLAG > SPACES                        12470003
124800        AND P917-FROM-TASK-LIST                                   12480003
124900           CONTINUE                                               12490003
125000        ELSE                                                      12500003
125100           SET P917-EMPLOYEE-ERROR   TO TRUE                      12510003
125200*               'CANNOT UPDATE - STATUS IS WORKING'               12520003
125300           MOVE 'C033'               TO MSGLOG-CODE               12530003
125400           MOVE '11'                 TO P917-BOOKON-REJECT-CODE   12540003
125500           PERFORM P9000-RETURN                                   12550003
125600        END-IF                                                    12560003
125700     END-IF                                                       12570003
125800*                                                                 12580003
125900*    MAKE SURE NOT CHANGING STATUS TO 'SAME' STATUS UNLESS        12590003
126000*    THIS IS ONE OF THE 3 SITUATIONS WHERE THE PERSON'S STATUS    12600003
126100*    ISN'T ACTUALLY GOING TO CHANGE: ADVANCE PAY, PAY IN LIEU,    12610003
126200*    AND CLEAR THE UNAVAILABLE-LAST-FLAG.                         12620003
126300*                                                                 12630003
126400*    CNC0284  LOGIC COMMENTED OUT TO ALLOW LAYOFF TO SAME         12640003
126500*    STATUS CODE FOR CN/IC MERGER.                                12650003
126600*                                                                 12660003
126700*    IF  P917-STATUS-CODE1    = LAYOFF-CODE-1                     12670003
126800*    AND NOT P917-ADVANCE-PAY                                     12680003
126900*    AND NOT P917-PAY-IN-LIEU                                     12690003
127000*       IF  P917-STATUS-CODE1     = 'A'                           12700003
127100*       AND UNAVAILABLE-LAST-FLAG > SPACES                        12710003
127200*       AND P917-FROM-TASK-LIST                                   12720003
127300*          CONTINUE                                               12730003
127400*       ELSE                                                      12740003
127500*          SET P917-STATUS-CODE-ERROR TO TRUE                     12750003
127600**              'CURRENT STATUS EQUALS NEW STATUS'                12760003
127700*          MOVE 'C034' TO MSGLOG-CODE                             12770003
127800*          MOVE '15'   TO P917-BOOKON-REJECT-CODE                 12780003
127900*          PERFORM P9000-RETURN                                   12790003
128000*       END-IF                                                    12800003
128100*    END-IF                                                       12810003
128200*                                                                 12820003
128300     IF P917-STATUS-CODE1 = 'A' AND                               12830003
128400        AVAILABLE OF LAYOFF-CODE-1                                12840003
128500        IF UNAVAILABLE-LAST-FLAG > SPACES                         12850003
128600           AND P917-FROM-TASK-LIST                                12860003
128700           CONTINUE                                               12870003
128800        ELSE                                                      12880003
128900           SET P917-STATUS-CODE-ERROR TO TRUE                     12890003
129000**              'EMPLOYEE ALREADY IN AVAILABLE STATUS'            12900003
129100           MOVE 'C034' TO MSGLOG-CODE                             12910003
129200           MOVE '15'   TO P917-BOOKON-REJECT-CODE                 12920003
129300           PERFORM P9000-RETURN                                   12930003
129400        END-IF                                                    12940003
129500     END-IF                                                       12950003
129600*                                                                 12960003
129700     IF P956-STATUS-CODE = 'A'                                    12970003
129800        IF P917-VRU-FUNCTION NOT > SPACES                         12980003
129900        OR P917-VRU-UPDATE-MARKUP                                 12990003
130000           PERFORM P1600-EDIT-RESTRICTIONS                        13000003
130100        END-IF                                                    13010003
130200     END-IF                                                       13020003
130300*                                                                 13030003
130400     IF P917-STATUS-CODE1 = 'V'                                   13040003
130500        PERFORM P1050-VACATION-EDITS                              13050003
130600     END-IF.                                                      13060003
130700*                                                                 13070003
130800 P1025-CLEAR-PEND-WTRN.                                           13080003
130900*                                                                 13090003
131000     MOVE P917-EMP-NO                TO MSTRNBRK                  13100003
131100     PERFORM P8200-READ-MSTR-UPDATE                               13110003
131200     IF NOT SUCCESS                                               13120003
131300        MOVE 'P1025-1'               TO ERR-PARAGRAPH             13130003
131400        MOVE MSTRNBRK                TO ERR-KEY                   13140003
131500        PERFORM P9999-GOT-PROBLEM                                 13150003
131600     END-IF                                                       13160003
131700     MOVE SPACES                     TO MSTR-WAIT-TURN-FLAG       13170003
131800     PERFORM P8210-REWRITE-MSTR                                   13180003
131900     IF NOT SUCCESS                                               13190003
132000        MOVE 'P1025-2'               TO ERR-PARAGRAPH             13200003
132100        MOVE MSTRNBRK                TO ERR-KEY                   13210003
132200        PERFORM P9999-GOT-PROBLEM                                 13220003
132300     END-IF.                                                      13230003
132400*                                                                 13240003
132500 P1030-VAC-SPEC-CALC.                                             13250003
132600*                                                                 13260003
132700*    IF THE EMPLOYEE IS BEING MARKED OFF ON VACATION AFTER 2000PM 13270003
132800*    CONSIDER IT TO BEGIN THE NEXT DAY                            13280003
132900*                                                                 13290003
133000     IF WS-EFF-TIME                      > 1959                   13300003
133100        MOVE ZEROS                      TO DATE-CONVERSION-PARMS  13310003
133200        SET  PARM-ADD                   TO TRUE                   13320003
133300        MOVE WS-EFF-DATE                TO PARM-PRI-DATE-GREG     13330003
133400        MOVE '000001'                   TO PARM-SEC-DATE-GREG     13340003
133500        EXEC CICS LINK                                            13350003
133600                  PROGRAM (P903-PGM)                              13360003
133700                  COMMAREA(DATE-CONVERSION-PARMS)                 13370003
133800                  LENGTH  (P903-LGTH)                             13380003
133900                  RESP    (WS-RESPONSE)                           13390003
134000        END-EXEC                                                  13400003
134100        MOVE WS-RESPONSE                TO FILE-STATUS            13410003
134200        IF NOT SUCCESS                                            13420003
134300           MOVE 'P1030-1'               TO ERR-PARAGRAPH          13430003
134400           MOVE 'P903LINK'              TO ERR-KEY                13440003
134500           PERFORM P9999-GOT-PROBLEM                              13450003
134600        END-IF                                                    13460003
134700        MOVE PARM-RES-DATE-GREG         TO WS-VAC-EFF-DATE        13470003
134800     ELSE                                                         13480003
134900        MOVE WS-EFF-DATE                TO WS-VAC-EFF-DATE        13490003
135000     END-IF                                                       13500003
135100     IF WS-VAC-EFF-YR < '90'                                      13510003
135200        MOVE '20'                       TO WS-VAC-EFF-CENT        13520003
135300     ELSE                                                         13530003
135400        MOVE '19'                       TO WS-VAC-EFF-CENT        13540003
135500     END-IF                                                       13550003
135600******************************************************************13560003
135700* READ THE EMPLOYEE'S VACATION RECORD                             13570003
135800******************************************************************13580003
135900     MOVE SPACES                        TO VAC-KEY-1              13590003
136000     MOVE EMP-NBR IN WS-MSTR            TO VAC-K1-EMPLOYEE-NO     13600003
136100     MOVE DIST OF WS-MSTR               TO WS-VAC-DIST            13610003
136200     MOVE SUB-DIST OF WS-MSTR           TO WS-VAC-SUB-DIST        13620003
136300     MOVE WS-VAC-EFF-YR                 TO VAC-K1-YEAR            13630003
136400     IF VAC-K1-YEAR                      < 90                     13640003
136500        MOVE '20'                       TO VAC-K1-CENT            13650003
136600     ELSE                                                         13660003
136700        MOVE '19'                       TO VAC-K1-CENT            13670003
136800     END-IF                                                       13680003
136900     MOVE VAC-KEY-1                     TO VACKEY1                13690003
137000     PERFORM P8000-READ-VACEMP-FILE                               13700003
137100     .                                                            13710003
137200 P1050-VACATION-EDITS.                                            13720003
137300*                                                                 13730003
137400*    IF THE EMPLOYEE IS BEING MARKED OFF ON VACATION AFTER 2000PM 13740003
137500*    CONSIDER IT TO BEGIN THE NEXT DAY                            13750003
137600*                                                                 13760003
137700     IF WS-EFF-TIME > 1959                                        13770003
137800        MOVE ZEROS                    TO DATE-CONVERSION-PARMS    13780003
137900        SET PARM-ADD                  TO TRUE                     13790003
138000        MOVE WS-EFF-DATE              TO PARM-PRI-DATE-GREG       13800003
138100        MOVE '000001'                 TO PARM-SEC-DATE-GREG       13810003
138200        EXEC CICS LINK                                            13820003
138300                  PROGRAM(P903-PGM)                               13830003
138400                  COMMAREA(DATE-CONVERSION-PARMS)                 13840003
138500                  LENGTH(P903-LGTH)                               13850003
138600                  RESP(WS-RESPONSE)                               13860003
138700        END-EXEC                                                  13870003
138800        MOVE WS-RESPONSE              TO FILE-STATUS              13880003
138900        IF NOT SUCCESS                                            13890003
139000           MOVE 'P1050-1'             TO ERR-PARAGRAPH            13900003
139100           MOVE 'P903LINK'            TO ERR-KEY                  13910003
139200           PERFORM P9999-GOT-PROBLEM                              13920003
139300        END-IF                                                    13930003
139400        MOVE PARM-RES-DATE-GREG       TO WS-VAC-EFF-DATE          13940003
139500     ELSE                                                         13950003
139600        MOVE WS-EFF-DATE              TO WS-VAC-EFF-DATE          13960003
139700     END-IF                                                       13970003
139800     IF WS-VAC-EFF-YR < '90'                                      13980003
139900        MOVE '20'                     TO WS-VAC-EFF-CENT          13990003
140000     ELSE                                                         14000003
140100        MOVE '19'                     TO WS-VAC-EFF-CENT          14010003
140200     END-IF                                                       14020003
140300*                                                                 14030003
140400     IF  OD-AREA > SPACE                                          14040003
140500     AND NOT P917-ADVANCE-PAY                                     14050003
140600     AND NOT P917-PAY-IN-LIEU                                     14060003
140700        IF OUT-TOWN                                               14070003
140800*                 'EMPLOYEE OUT OF TOWN - '                       14080003
140900*                 'CANNOT BOOKOFF ON VACATION '                   14090003
141000*                 'UNTIL TIED UP AT HOME TERMINAL '               14100003
141100           MOVE 'E075' TO MSGLOG-CODE                             14110003
141200        ELSE                                                      14120003
141300*                 'EMPLOYEE HAS AN ON-DUTY ASSIGNMENT '           14130003
141400*                 'CANNOT BOOKOFF ON VACATION '                   14140003
141500*                 'UNTIL RELEASED FROM ASSIGNMENT'                14150003
141600           MOVE 'E076' TO MSGLOG-CODE                             14160003
141700        END-IF                                                    14170003
141800        SET P917-STATUS-CODE-ERROR TO TRUE                        14180003
141900        PERFORM P9000-RETURN                                      14190003
142000     END-IF                                                       14200003
142100*                                                                 14210003
142200******************************************************************14220003
142300* READ THE VACATION CONTROL RECORD                                14230003
142400******************************************************************14240003
142500     MOVE SPACES                    TO VAC-KEY-1                  14250003
142600     MOVE EMP-NBR IN WS-MSTR        TO VAC-K1-EMPLOYEE-NO         14260003
142700     MOVE DIST OF WS-MSTR           TO WS-VAC-DIST                14270003
142800     MOVE SUB-DIST OF WS-MSTR       TO WS-VAC-SUB-DIST            14280003
142900     MOVE WS-VAC-EFF-YR             TO VAC-K1-YEAR                14290003
143000     IF VAC-K1-YEAR < 90                                          14300003
143100        MOVE '20'                   TO VAC-K1-CENT                14310003
143200     ELSE                                                         14320003
143300        MOVE '19'                   TO VAC-K1-CENT                14330003
143400     END-IF                                                       14340003
143500     PERFORM P1060-READ-VAC-CONTROL                               14350003
143501                                                                  14350103
143502******************************************************************14350203
143503* CALCULATE THE VACATION END DATE                                 14350303
143504******************************************************************14350403
143505     IF VAC-USE-CALC                                              14350503
143506        MOVE P917-NBR-DAYS          TO WS-VAC-REQUESTED           14350603
143507        COMPUTE WS-VAC-REQUESTED ROUNDED =                        14350703
143508               (WS-VAC-REQUESTED / 5 * 7) - 2                     14350803
143509     ELSE                                                         14350903
143510        MOVE P917-NBR-DAYS          TO WS-VAC-REQUESTED           14351003
143511     END-IF                                                       14351103
143512     MOVE ZEROS                     TO DATE-CONVERSION-PARMS      14351203
143513     SET  PARM-ADD                  TO TRUE                       14351303
143514     MOVE WS-VAC-EFF-DATE           TO PARM-PRI-DATE-GREG         14351403
143515     MOVE WS-VAC-REQUESTED          TO PARM-SEC-GREG-DAY          14351503
143516     SUBTRACT 1                   FROM PARM-SEC-GREG-DAY          14351603
143517     EXEC CICS LINK                                               14351703
143518               PROGRAM(P903-PGM)                                  14351803
143519               COMMAREA(DATE-CONVERSION-PARMS)                    14351903
143520               LENGTH(P903-LGTH)                                  14352003
143521               RESP(WS-RESPONSE)                                  14352103
143522     END-EXEC                                                     14352203
143523     MOVE WS-RESPONSE               TO FILE-STATUS                14352303
143524     IF NOT SUCCESS                                               14352403
143525        MOVE 'P1050-2'              TO ERR-PARAGRAPH              14352503
143526        PERFORM P9999-GOT-PROBLEM                                 14352603
143527     END-IF                                                       14352703
143528                                                                  14352803
143529     MOVE SPACES                    TO P977-COMMAREA-PARMS        14352903
143530     SET P977-SCHEDULE              TO TRUE                       14353003
143531     SET P977-CHECK-FUNCTION        TO TRUE                       14353103
143532     SET P977-EXACT-SCHEDULE        TO TRUE                       14353203
143533     MOVE EMP-NBR OF WS-MSTR        TO P977-EMP-NO                14353303
143534     MOVE WS-VAC-EFF-CENT-DATE      TO P977-START-CENT-DATE       14353403
143535     MOVE PARM-RES-DATE-GREG        TO P977-END-DATE              14353503
143536     MOVE PARM-RES-GREG-CENT        TO P977-END-CENT              14353603
143537     MOVE P977-END-CENT-DATE        TO WS-VAC-END-CENT-DATE       14353703
143538*                                                                 14353803
143539     PERFORM P8700-LINK-TO-P977                                   14353903
143540*                                                                 14354003
143541     IF NOT P917-ERROR-FOUND                                      14354103
143542        MOVE P917-NBR-DAYS          TO WS-VAC-REQUESTED           14354203
143543        ADD  WS-VAC-REQUESTED       TO WS-VAC-DAYS-TAKEN          14354303
143544        IF WS-VAC-DAYS-TAKEN         > WS-VAC-DAYS-DUE            14354403
143545*          'VACATION DAYS REQUESTED EXCEED TOTAL ALLOWED          14354503
143546           MOVE 'V007'              TO MSGLOG-CODE                14354603
143547           SET P917-NBR-DAYS-ERROR  TO TRUE                       14354703
143548        END-IF                                                    14354803
143549     END-IF                                                       14354903
143550*                                                                 14355003
143560     IF P917-ERROR-FOUND                                          14356003
143570        PERFORM P9000-RETURN                                      14357003
143580     END-IF                                                       14358003
143590*                                                                 14359003
143600     IF P917-ADVANCE-PAY OR P917-PAY-IN-LIEU                      14360003
143700        PERFORM P1070-UPDATE-VACEMP                               14370003
143800     ELSE                                                         14380003
143900        PERFORM P1080-SET-VAC-FLAGS                               14390003
144000     END-IF.                                                      14400003
144100*                                                                 14410003
144200 P1060-READ-VAC-CONTROL.                                          14420003
144300*                                                                 14430003
144400     MOVE VAC-KEY-1                    TO VACKEY1                 14440003
144500     PERFORM P8000-READ-VACEMP-FILE                               14450003
144600     IF NOT SUCCESS                                               14460003
144700*       'EMPLOYEE NOT FOUND IN VACATION CONTROL FILE              14470003
144800        MOVE 'E078' TO MSGLOG-CODE                                14480003
144900        SET P917-EMPLOYEE-ERROR        TO TRUE                    14490003
145000        PERFORM P9000-RETURN                                      14500003
145100     END-IF                                                       14510003
145200     IF VAC-CARRYOVER-AREA NOT NUMERIC                            14520003
145300        MOVE ZEROS                     TO VAC-CARRYOVER-AREA      14530003
145400     END-IF                                                       14540003
145500     ADD VAC-TAKEN-DAYS VAC-ADVANCE-DAYS VAC-IN-LIEU-DAYS         14550003
145600        GIVING WS-VAC-DAYS-TAKEN                                  14560003
145700     ADD VAC-DAYS-DUE VAC-CARRYOVER-DAYS                          14570003
145800        GIVING WS-VAC-DAYS-DUE                                    14580003
145900     IF WS-VAC-DAYS-TAKEN > VAC-CARRYOVER-DAYS                    14590003
146000        MOVE ZEROS                     TO WS-VAC-CARRYOVER-DAYS   14600003
146100     ELSE                                                         14610003
146200        SUBTRACT WS-VAC-DAYS-TAKEN FROM VAC-CARRYOVER-DAYS        14620003
146300           GIVING WS-VAC-CARRYOVER-DAYS                           14630003
146400     END-IF.                                                      14640003
146500*                                                                 14650003
146600 P1070-UPDATE-VACEMP.                                             14660003
146700*                                                                 14670003
146800     MOVE SPACES                    TO VAC-KEY-1                  14680003
146900     MOVE EMP-NBR IN WS-MSTR        TO VAC-K1-EMPLOYEE-NO         14690003
147000     MOVE DIST OF WS-MSTR           TO WS-VAC-DIST                14700003
147100     MOVE SUB-DIST OF WS-MSTR       TO WS-VAC-SUB-DIST            14710003
147200     MOVE WS-VAC-EFF-YR             TO VAC-K1-YEAR                14720003
147300     IF VAC-K1-YEAR < 90                                          14730003
147400        MOVE '20'                   TO VAC-K1-CENT                14740003
147500     ELSE                                                         14750003
147600        MOVE '19'                   TO VAC-K1-CENT                14760003
147700     END-IF                                                       14770003
147800     MOVE VAC-KEY-1                 TO VACKEY1                    14780003
147900     EXEC CICS READ                                               14790003
148000               UPDATE                                             14800003
148100               DATASET(VACATION-VIA-EMP-DATE)                     14810003
148200               INTO(WS-VACATION-RECORD)                           14820003
148300               LENGTH(VACEMPD-RLGTH)                              14830003
148400               RIDFLD(VACKEY1)                                    14840003
148500               KEYLENGTH(VACEMPD-KLGTH)                           14850003
148600               RESP(WS-RESPONSE)                                  14860003
148700     END-EXEC                                                     14870003
148800     MOVE WS-RESPONSE TO FILE-STATUS                              14880003
148900     IF NOT SUCCESS                                               14890003
149000        MOVE 'P1070-1'  TO ERR-PARAGRAPH                          14900003
149100        MOVE VACKEY1     TO ERR-KEY                               14910003
149200        PERFORM P9999-GOT-PROBLEM                                 14920003
149300     END-IF                                                       14930003
149400                                                                  14940003
149500     IF  VAC-DAYS-DUE      > ZERO                                 14950003
149600     AND VAC-GROSS-EARN    > ZERO                                 14960003
149700     AND VAC-PCT-OF-GROSS > ZERO                                  14970003
149800         COMPUTE WS-TEMP-DAYS-AVAIL        = VAC-CARRYOVER-DAYS   14980003
149900                                           - VAC-TAKEN-DAYS       14990003
150000                                           - VAC-IN-LIEU-DAYS     15000003
150100         IF  WS-TEMP-DAYS-AVAIL            < 0                    15010003
150200             ADD VAC-DAYS-DUE             TO WS-TEMP-DAYS-AVAIL   15020003
150300         ELSE                                                     15030003
150400             MOVE VAC-DAYS-DUE             TO WS-TEMP-DAYS-AVAIL  15040003
150500         END-IF                                                   15050003
150600         COMPUTE WS-TEMP-VAC-AMT-ALLOWED ROUNDED                  15060003
150700                                           = VAC-GROSS-EARN       15070003
150800                                           * VAC-PCT-OF-GROSS     15080003
150900         COMPUTE WS-TEMP-AMT-ALLOWED-SPENT = VAC-TAKEN-AMT        15090003
151000                                           + VAC-IN-LIEU-AMT      15100003
151100                                           - VAC-CARRYOVER-AMT    15110003
151200         IF  WS-TEMP-AMT-ALLOWED-SPENT < 0                        15120003
151300             MOVE 0                   TO WS-TEMP-AMT-ALLOWED-SPENT15130003
151400         END-IF                                                   15140003
151500         COMPUTE WS-TEMP-VAC-AMT-ALLOWED = WS-TEMP-VAC-AMT-ALLOWED15150003
151600                                       - WS-TEMP-AMT-ALLOWED-SPENT15160003
151700         IF WS-TEMP-DAYS-AVAIL         = 0                        15170003
151800         OR WS-TEMP-DAYS-AVAIL         < 0                        15180003
151900         OR  WS-TEMP-VAC-AMT-ALLOWED    < 0                       15190003
152000             IF VAC-DAYS-DUE            = 0                       15200003
152100                 MOVE 0               TO WS-VAC-DAILY-AMT         15210003
152200             ELSE                                                 15220003
152300                 COMPUTE WS-TEMP-VAC-AMT-ALLOWED ROUNDED          15230003
152400                                       = VAC-GROSS-EARN           15240003
152500                                       * VAC-PCT-OF-GROSS         15250003
152600                 COMPUTE WS-VAC-DAILY-AMT ROUNDED                 15260003
152700                                       = WS-TEMP-VAC-AMT-ALLOWED  15270003
152800                                       / VAC-DAYS-DUE             15280003
152900             END-IF                                               15290003
153000         ELSE                                                     15300003
153100             COMPUTE WS-VAC-DAILY-AMT ROUNDED                     15310003
153200                                       = WS-TEMP-VAC-AMT-ALLOWED  15320003
153300                                       / WS-TEMP-DAYS-AVAIL       15330003
153400         END-IF                                                   15340003
153500     END-IF                                                       15350003
153600     IF WS-VAC-DAILY-AMT NOT > VAC-ADVANCE-AMT                    15360003
153700        SUBTRACT WS-VAC-DAILY-AMT     FROM VAC-ADVANCE-AMT        15370003
153800*       ADD WS-VAC-DAILY-AMT          TO VAC-TAKEN-AMT            15380003
153900     ELSE                                                         15390003
154000        IF VAC-ADVANCE-AMT > ZERO                                 15400003
154100*          ADD VAC-ADVANCE-AMT        TO VAC-TAKEN-AMT            15410003
154200           MOVE ZEROES               TO VAC-ADVANCE-AMT           15420003
154300        END-IF                                                    15430003
154400     END-IF                                                       15440003
154500     IF VAC-ADVANCE-DAYS > ZERO                                   15450003
154600        SUBTRACT 1                   FROM VAC-ADVANCE-DAYS        15460003
154700*       ADD 1                        TO VAC-TAKEN-DAYS            15470003
154800     ELSE                                                         15480003
154900        MOVE 'P1070-2'               TO ERR-PARAGRAPH             15490003
155000        MOVE 'VAC ADVANCE'           TO ERR-KEY                   15500003
155100        PERFORM P9999-GOT-PROBLEM                                 15510003
155200     END-IF                                                       15520003
155300     EXEC CICS REWRITE                                            15530003
155400               DATASET(VACATION-VIA-EMP-DATE)                     15540003
155500               FROM(WS-VACATION-RECORD)                           15550003
155600               LENGTH(VACEMPD-RLGTH)                              15560003
155700               RESP(WS-RESPONSE)                                  15570003
155800     END-EXEC                                                     15580003
155900     MOVE WS-RESPONSE                TO FILE-STATUS               15590003
156000     IF NOT SUCCESS                                               15600003
156100        MOVE 'P1070-3'               TO ERR-PARAGRAPH             15610003
156200        MOVE VACKEY1                 TO ERR-KEY                   15620003
156300        PERFORM P9999-GOT-PROBLEM                                 15630003
156400     END-IF.                                                      15640003
156500*                                                                 15650003
156600 P1080-SET-VAC-FLAGS.                                             15660003
156700*                                                                 15670003
156800*    READ THE VACATION PROFILE RECORD                             15680003
156900*                                                                 15690003
157000     MOVE SPACES                     TO P977-COMMAREA-PARMS       15700003
157100     SET P977-VAC-YEAR               TO TRUE                      15710003
157200     SET P977-CHECK-FUNCTION         TO TRUE                      15720003
157300     MOVE EMP-NBR OF WS-MSTR         TO P977-EMP-NO               15730003
157400     MOVE WS-VAC-EFF-CENT-DATE       TO P977-START-DATE-TIME      15740003
157500     EXEC CICS LINK                                               15750003
157600               PROGRAM(P977-PGM)                                  15760003
157700               COMMAREA(P977-COMMAREA-PARMS)                      15770003
157800               LENGTH(P977-LGTH)                                  15780003
157900               RESP(WS-RESPONSE)                                  15790003
158000     END-EXEC                                                     15800003
158100     MOVE WS-RESPONSE                TO FILE-STATUS               15810003
158200     IF NOT SUCCESS                                               15820003
158300        MOVE 'P1080-1'               TO ERR-PARAGRAPH             15830003
158400        MOVE 'P977'                  TO ERR-KEY                   15840003
158500        PERFORM P9999-GOT-PROBLEM                                 15850003
158600     END-IF                                                       15860003
158700     IF P977-ERROR-FOUND                                          15870003
158800        MOVE P977-MSGLOG-CODE        TO MSGLOG-CODE               15880003
158900        SET P917-EMPLOYEE-ERROR      TO TRUE                      15890003
159000        PERFORM P9000-RETURN                                      15900003
159100     END-IF                                                       15910003
159200     MOVE SPACES                     TO WS-TKCR-FILE              15920003
159300     SET VAC-PROFILE-REC             TO TRUE                      15930003
159400     SET VAC-PROFILE-TYPE-REC        TO TRUE                      15940003
159500     MOVE P977-VAC-PROFILE           TO TKCR-PROF-VAC-PROFILE     15950003
159600     MOVE P977-MID-CENT-DATE         TO TKCR-EXP-DATE-CENT        15960003
159700     MOVE TKCRKEY-AREA               TO TKCRKEY                   15970003
159800     PERFORM P8100-READ-TKCRFILE                                  15980003
159900     IF SUCCESS                                                   15990003
160000        AND VAC-PROFILE-REC                                       16000003
160100        AND VAC-PROFILE-TYPE-REC                                  16010003
160200        AND TKCR-PROF-VAC-PROFILE = P977-VAC-PROFILE              16020003
160300        CONTINUE                                                  16030003
160400     ELSE                                                         16040003
160500        IF SUCCESS OR NO-RECORD-FND                               16050003
160600*          'NO VACATION PROFILE EXISTS FOR ROSTER                 16060003
160700           MOVE 'N115'               TO MSGLOG-CODE               16070003
160800           SET P917-EMPLOYEE-ERROR   TO TRUE                      16080003
160900           PERFORM P9000-RETURN                                   16090003
161000        ELSE                                                      16100003
161100           MOVE 'P1080-2'            TO ERR-PARAGRAPH             16110003
161200           MOVE TKCRKEY              TO ERR-KEY                   16120003
161300           PERFORM P9999-GOT-PROBLEM                              16130003
161400        END-IF                                                    16140003
161500     END-IF                                                       16150003
161600                                                                  16160003
161700     MOVE EMP-NBR OF WS-MSTR TO MSTR2NBRK                         16170003
161800     PERFORM P8300-READ-MSTR2-UPDATE                              16180003
161900     IF NOT SUCCESS                                               16190003
162000        MOVE 'P1080-3'               TO ERR-PARAGRAPH             16200003
162100        MOVE MSTR2NBRK               TO ERR-KEY                   16210003
162200        PERFORM P9999-GOT-PROBLEM                                 16220003
162300     END-IF                                                       16230003
162400*                                                                 16240003
162500*    ONLY UPDATE VACATION INFO IN MSTR2 IF VACATION FIELDS ARE    16250003
162600*    BLANK - ALSO                                                 16260003
162700*    CHECK FOR SCENARIO OF GOING FROM LAYOFF STATUS TO VACATION   16270003
162800*    STATUS WITHOUG RUNNING MABY                                  16280003
162900*SAM                                                              16290003
163000*                                                                 16300003
163100     IF (MSTR2-SPECIAL-PAY-STATUS NOT > SPACES)                   16310003
163200      OR (LAYOFF-CODE-1 > 'C')                                    16320003
163300        MOVE P917-STATUS-CODE1    TO MSTR2-SPECIAL-PAY-STATUS     16330003
163400        MOVE TKCR-PROF-GEN-TMSLIP TO MSTR2-GEN-TMSLIP             16340003
163500        MOVE WS-VAC-EFF-CENT-DATE TO MSTR2-NEXT-PAY-DATE-CENT     16350003
163600        MOVE WS-VAC-EFF-CENT-DATE TO MSTR2-NEXT-PROCESS-DATE-CENT 16360003
163700        PERFORM P8310-REWRITE-MSTR2                               16370003
163800        IF NOT SUCCESS                                            16380003
163900           MOVE 'P1080-4'            TO ERR-PARAGRAPH             16390003
164000           MOVE MSTR2NBRK            TO ERR-KEY                   16400003
164100           PERFORM P9999-GOT-PROBLEM                              16410003
164200        END-IF                                                    16420003
164300     ELSE                                                         16430003
164400        EXEC CICS UNLOCK                                          16440003
164500                  DATASET(MSTR2-VIA-EMP-NBR)                      16450003
164600        END-EXEC                                                  16460003
164700     END-IF                                                       16470003
164800*                                                                 16480003
164900     MOVE SPACES                    TO P977-COMMAREA-PARMS        16490003
165000     SET P977-SCHEDULE              TO TRUE                       16500003
165100     SET P977-TAKE-FUNCTION         TO TRUE                       16510003
165200     MOVE EMP-NBR OF WS-MSTR        TO P977-EMP-NO                16520003
165300     MOVE WS-VAC-EFF-CENT-DATE      TO P977-START-CENT-DATE       16530003
165400     MOVE WS-VAC-END-CENT-DATE      TO P977-END-CENT-DATE         16540003
165500     PERFORM P8700-LINK-TO-P977.                                  16550003
165600*                                                                 16560003
165700 P1100-EDIT-DATE-TIME.                                            16570003
165800*                                                                 16580003
165900*    EDIT DATE                                                    16590003
166000*                                                                 16600003
166100     MOVE WS-EFF-DATE     TO DE-YYMMDD                            16610003
166200     SET DE-YYMMDD-FORMAT TO TRUE                                 16620003
166300     PERFORM P8998-DATEEDIT                                       16630003
166400     IF DE-INVALID-DATE                                           16640003
166500        SET P917-DATE-ERROR TO TRUE                               16650003
166600*            INVALID-DATE-MSG                                     16660003
166700        MOVE 'I034' TO MSGLOG-CODE                                16670003
166710        MOVE '16'   TO P917-BOOKON-REJECT-CODE                    16671003
166720        PERFORM P9000-RETURN                                      16672003
166730     END-IF                                                       16673003
166740*                                                                 16674003
166750*    EDIT TIME                                                    16675003
166760*                                                                 16676003
166770     IF WS-EFF-TIME                     = '0000'                  16677003
166780        MOVE '0001'                    TO WS-EFF-TIME             16678003
166790     END-IF                                                       16679003
166800     MOVE WS-EFF-TIME TO TE-MILITARY-TIME                         16680003
166900     SET TE-MILITARY-FORMAT TO TRUE                               16690003
167000     PERFORM P8997-TIMEEDIT                                       16700003
167100     IF TE-INVALID-TIME                                           16710003
167200        SET P917-TIME-ERROR TO TRUE                               16720003
167300*            INVALID-TIME-MSG                                     16730003
167400        MOVE 'I022' TO MSGLOG-CODE                                16740003
167500        MOVE '19'   TO P917-BOOKON-REJECT-CODE                    16750003
167600        PERFORM P9000-RETURN                                      16760003
167700     END-IF                                                       16770003
167800*                                                                 16780003
167900*    IF THIS IS NOT A FUTURE BOOKON/BOOKOFF OR PAYMENT IN LIEU,   16790003
168000*    SEE IF IT IS MORE THAN 24 HOURS AGO                          16800003
168100*                                                                 16810003
168200*    IF WS-EFF-DATE-TIME NOT > WS-LOCAL-DATE-TIME                 16820003
168300     IF WS-EFF-DATE-TIME-CE NOT > WS-LOCAL-DATE-TIME-CENT         16830003
168400        IF P917-PAY-IN-LIEU                                       16840003
168500           CONTINUE                                               16850003
168600        ELSE                                                      16860003
168700           MOVE ZEROES          TO DATE-CONVERSION-PARMS          16870003
168800           MOVE 'D'             TO PARM-CONV-TYPE                 16880003
168900           MOVE WS-LOCAL-DATE   TO PARM-PRI-DATE-GREG             16890003
169000           MOVE WS-LOCAL-TIME   TO PARM-PRI-TIME                  16900003
169100           MOVE WS-EFF-DATE     TO PARM-SEC-DATE-GREG             16910003
169200           MOVE WS-EFF-TIME     TO PARM-SEC-TIME                  16920003
169300           EXEC CICS LINK                                         16930003
169400                     PROGRAM(P903-PGM)                            16940003
169500                     COMMAREA(DATE-CONVERSION-PARMS)              16950003
169600                     LENGTH(P903-LGTH)                            16960003
169700                     RESP(WS-RESPONSE)                            16970003
169800           END-EXEC                                               16980003
169900           MOVE WS-RESPONSE TO FILE-STATUS                        16990003
170000           IF NOT SUCCESS                                         17000003
170100              MOVE 'P1100-1' TO ERR-PARAGRAPH                     17010003
170200              PERFORM P9999-GOT-PROBLEM                           17020003
170300           END-IF                                                 17030003
170400           IF PARM-RES-HRMN > 2400                                17040003
170500              OR PARM-RES-TOT-DAYS > 000                          17050003
170600              SET P917-DATE-ERROR TO TRUE                         17060003
170700*             MOVE 'BOOKON/BOOKOFF CANNOT BE OVER 24 HRS IN PAST' 17070003
170800              MOVE 'B005' TO MSGLOG-CODE                          17080003
170900              MOVE '17'   TO P917-BOOKON-REJECT-CODE              17090003
171000              PERFORM P9000-RETURN                                17100003
171100           END-IF                                                 17110003
171200        END-IF                                                    17120003
171300     END-IF.                                                      17130003
171400*                                                                 17140003
171500 P1150-CHECK-FOR-GOTRAIN-SPLIT.                                   17150003
171600*                                                                 17160003
171700     MOVE SPACES               TO WS-GOTRAIN-ASGNMT               17170003
171800     IF TEMPORARY-ASGNMT > SPACES                                 17180003
171900        IF TEMP-ASGN-AJ                                           17190003
172000           MOVE TEMPORARY-ASGNMT TO WS-GOTRAIN-ASGNMT             17200003
172100        END-IF                                                    17210003
172200     ELSE                                                         17220003
172300        IF NORMAL-ASGNMT > SPACES                                 17230003
172400           IF NORM-ASGN-AJ                                        17240003
172500              MOVE NORMAL-ASGNMT TO WS-GOTRAIN-ASGNMT             17250003
172600           END-IF                                                 17260003
172700        END-IF                                                    17270003
172800     END-IF                                                       17280003
172900     IF WS-GOTRAIN-ASGNMT > SPACES                                17290003
173000        MOVE WS-GOTRAIN-ASGN     TO JOB-DEF-CHECK                 17300003
173100        IF JOB-DEF-COMMUTER-ASGN                                  17310003
173200           PERFORM P1155-CHECK-FOR-SPLIT-SHIFT                    17320003
173300           IF IT-IS-SPLIT-SHIFT                                   17330003
173400              IF WS-EFF-TIME   NOT < WS-GOTRAIN-SHIFT1-START AND  17340003
173500                 WS-EFF-TIME   NOT > WS-GOTRAIN-SHIFT2-START      17350003
173600                    SET P917-DATE-ERROR TO TRUE                   17360003
173700                    MOVE 'B052' TO MSGLOG-CODE                    17370003
173800                    MOVE '18'   TO P917-BOOKON-REJECT-CODE        17380003
173900                    PERFORM P9000-RETURN                          17390003
174000              END-IF                                              17400003
174100           END-IF                                                 17410003
174200        ELSE                                                      17420003
174300           PERFORM P1175-GET-AJ-START-TIMES                       17430003
174400        END-IF                                                    17440003
174500     END-IF.                                                      17450003
174600*                                                                 17460003
174700 P1155-CHECK-FOR-SPLIT-SHIFT.                                     17470003
174800*                                                                 17480003
174900     SET NO-SPLIT-SHIFT        TO TRUE                            17490003
175000     MOVE ZEROES               TO SCHED-DONE-CODE                 17500003
175100                                  SCHED-FIRST-CODE                17510003
175200     MOVE SPACES               TO WORK-JS-KEY1                    17520003
175300     MOVE WS-GOTRAIN-DIST      TO WK-JSK1-ASGN-DIST               17530003
175400     MOVE WS-GOTRAIN-SUB-DIST  TO WK-JSK1-ASGN-SUB-DIST           17540003
175500     MOVE WS-GOTRAIN-ASGN      TO WK-JSK1-ASSIGNMENT              17550003
175600     MOVE WS-EFF-DATE          TO WK-JSK1-EXP-DATE                17560003
175700                                                                  17570003
175800     MOVE ZEROS                TO DATE-CONVERSION-PARMS           17580003
175900     SET PARM-CONV             TO TRUE                            17590003
176000     MOVE WS-EFF-DATE          TO PARM-PRI-DATE-GREG              17600003
176100     EXEC CICS LINK                                               17610003
176200               PROGRAM(P903-PGM)                                  17620003
176300               COMMAREA(DATE-CONVERSION-PARMS)                    17630003
176400               LENGTH(P903-LGTH)                                  17640003
176500               RESP(WS-RESPONSE)                                  17650003
176600     END-EXEC                                                     17660003
176700     MOVE WS-RESPONSE          TO FILE-STATUS                     17670003
176800     IF NOT SUCCESS                                               17680003
176900        MOVE 'P1155-1'         TO ERR-PARAGRAPH                   17690003
177000        MOVE 'P903LINK'        TO ERR-KEY                         17700003
177100        PERFORM P9999-GOT-PROBLEM                                 17710003
177200     END-IF                                                       17720003
177300                                                                  17730003
177400     MOVE PARM-PRI-DAY-OF-WEEK TO WK-JSK1-ASGN-DAY-NUM            17740003
177500                                                                  17750003
177600     MOVE WORK-JS-KEY1         TO JSKEY1                          17760003
177700                                                                  17770003
177800     EXEC CICS STARTBR                                            17780003
177900               DATASET(JS-VIA-JSKEY1)                             17790003
178000               RIDFLD(JSKEY1)                                     17800003
178100               GTEQ                                               17810003
178200               RESP(WS-RESPONSE)                                  17820003
178300     END-EXEC                                                     17830003
178400     MOVE WS-RESPONSE          TO FILE-STATUS                     17840003
178500                                                                  17850003
178600     IF NOT SUCCESS                                               17860003
178700        SET SCHED-DONE         TO TRUE                            17870003
178800        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     17880003
178900           MOVE 'P1155-2'           TO ERR-PARAGRAPH              17890003
179000           MOVE JSKEY1              TO ERR-KEY                    17900003
179100           PERFORM P9999-GOT-PROBLEM                              17910003
179200        END-IF                                                    17920003
179300     END-IF                                                       17930003
179400                                                                  17940003
179500     PERFORM UNTIL SCHED-DONE                                     17950003
179600        EXEC CICS READNEXT                                        17960003
179700                  DATASET(JS-VIA-JSKEY1)                          17970003
179800                  INTO(WS-JOB-SCHEDULE)                           17980003
179900                  LENGTH(JSKEY1-RLGTH)                            17990003
180000                  RIDFLD(JSKEY1)                                  18000003
180100                  KEYLENGTH(JSKEY1-KLGTH)                         18010003
180200                  RESP(WS-RESPONSE)                               18020003
180300        END-EXEC                                                  18030003
180400        MOVE WS-RESPONSE               TO FILE-STATUS             18040003
180500        IF SUCCESS                                                18050003
180600          SET DE-YYMMDD-FORMAT TO TRUE                            18060003
180700          MOVE JSK1-EXP-DATE TO DE-YYMMDD                         18070003
180800          PERFORM P8998-DATEEDIT                                  18080003
180900          MOVE DE-CCYYMMDD TO DE-COMPARE1-DATE                    18090003
181000                                                                  18100003
181100          SET DE-YYMMDD-FORMAT TO TRUE                            18110003
181200          MOVE WK-JSK1-EXP-DATE TO DE-YYMMDD                      18120003
181300          PERFORM P8998-DATEEDIT                                  18130003
181400          MOVE DE-CCYYMMDD TO DE-COMPARE2-DATE                    18140003
181500          IF JSK1-ASGN-DIST     = WK-JSK1-ASGN-DIST     AND       18150003
181600             JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST AND       18160003
181700             JSK1-ASSIGNMENT    = WK-JSK1-ASSIGNMENT    AND       18170003
181800             DE-COMPARE1-DATE NOT < DE-COMPARE2-DATE              18180003
181900             IF JSK1-ASGN-DAY   < WK-JSK1-ASGN-DAY                18190003
182000               CONTINUE                                           18200003
182100             ELSE                                                 18210003
182200               IF JSK1-ASGN-DAY      = WK-JSK1-ASGN-DAY           18220003
182300                 IF JOB-SCHED-ASGN-OFF                            18230003
182400                    SET SCHED-DONE TO TRUE                        18240003
182500                 ELSE                                             18250003
182600                    IF NOT SCHED-FIRST                            18260003
182700                       SET SCHED-FIRST    TO TRUE                 18270003
182800                       MOVE JSK1-ASGN-START-TIME                  18280003
182900                                     TO WS-GOTRAIN-SHIFT1-START   18290003
183000                                        SAVE-JSK1-ASGN-START-TIME 18300003
183100                       MOVE JSK1-ASGN-START-TIME                  18310003
183200                                     TO WS-GOTRAIN-SHIFT2-START   18320003
183300                    ELSE                                          18330003
183400                       SET IT-IS-SPLIT-SHIFT TO TRUE              18340003
183500                       MOVE JSK1-ASGN-START-TIME                  18350003
183600                                     TO WS-GOTRAIN-SHIFT2-START   18360003
183700                    END-IF                                        18370003
183800                 END-IF                                           18380003
183900               ELSE                                               18390003
184000                  SET SCHED-DONE     TO TRUE                      18400003
184100               END-IF                                             18410003
184200             END-IF                                               18420003
184300          ELSE                                                    18430003
184400             SET SCHED-DONE     TO TRUE                           18440003
184500          END-IF                                                  18450003
184600        ELSE                                                      18460003
184700           SET SCHED-DONE         TO TRUE                         18470003
184800           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  18480003
184900              MOVE 'P1155-3'           TO ERR-PARAGRAPH           18490003
185000              MOVE JSKEY1              TO ERR-KEY                 18500003
185100              PERFORM P9999-GOT-PROBLEM                           18510003
185200           END-IF                                                 18520003
185300        END-IF                                                    18530003
185400     END-PERFORM                                                  18540003
185500                                                                  18550003
185600     EXEC CICS ENDBR                                              18560003
185700               DATASET(JS-VIA-JSKEY1)                             18570003
185800               RESP(WS-RESPONSE)                                  18580003
185900     END-EXEC.                                                    18590003
186000*                                                                 18600003
186100 P1175-GET-AJ-START-TIMES.                                        18610003
186200*                                                                 18620003
186300     SET NO-SPLIT-SHIFT        TO TRUE                            18630003
186400     MOVE ZEROES               TO SCHED-DONE-CODE                 18640003
186500                                  SCHED-FIRST-CODE                18650003
186600     MOVE SPACES               TO WORK-JS-KEY1                    18660003
186700     MOVE DIST OF WS-MSTR      TO WK-JSK1-ASGN-DIST               18670003
186800     MOVE SUB-DIST OF WS-MSTR  TO WK-JSK1-ASGN-SUB-DIST           18680003
186900     MOVE SAVE-ASGN            TO WK-JSK1-ASGN                    18690003
187000     MOVE CRAFT OF WS-MSTR     TO WK-JSK1-ASGN-CC                 18700003
187100     MOVE P917-EFF-DATE        TO WK-JSK1-EXP-DATE                18710003
187200                                                                  18720003
187300     MOVE ZEROS                TO DATE-CONVERSION-PARMS           18730003
187400     SET PARM-CONV             TO TRUE                            18740003
187500     MOVE WS-EFF-DATE          TO PARM-PRI-DATE-GREG              18750003
187600     EXEC CICS LINK                                               18760003
187700               PROGRAM(P903-PGM)                                  18770003
187800               COMMAREA(DATE-CONVERSION-PARMS)                    18780003
187900               LENGTH(P903-LGTH)                                  18790003
188000               RESP(WS-RESPONSE)                                  18800003
188100     END-EXEC                                                     18810003
188200     MOVE WS-RESPONSE          TO FILE-STATUS                     18820003
188300     IF NOT SUCCESS                                               18830003
188400        MOVE 'P1175-1'         TO ERR-PARAGRAPH                   18840003
188500        MOVE 'P903LINK'        TO ERR-KEY                         18850003
188600        PERFORM P9999-GOT-PROBLEM                                 18860003
188700     END-IF                                                       18870003
188800                                                                  18880003
188900     MOVE PARM-PRI-DAY-OF-WEEK TO WK-JSK1-ASGN-DAY-NUM            18890003
189000                                                                  18900003
189100     MOVE WORK-JS-KEY1         TO JSKEY1                          18910003
189200                                                                  18920003
189300     EXEC CICS STARTBR                                            18930003
189400               DATASET(JS-VIA-JSKEY1)                             18940003
189500               RIDFLD(JSKEY1)                                     18950003
189600               GTEQ                                               18960003
189700               RESP(WS-RESPONSE)                                  18970003
189800     END-EXEC                                                     18980003
189900     MOVE WS-RESPONSE          TO FILE-STATUS                     18990003
190000                                                                  19000003
190100     IF NOT SUCCESS                                               19010003
190200        SET SCHED-DONE         TO TRUE                            19020003
190300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     19030003
190400           MOVE 'P1175-2'           TO ERR-PARAGRAPH              19040003
190500           MOVE JSKEY1              TO ERR-KEY                    19050003
190600           PERFORM P9999-GOT-PROBLEM                              19060003
190700        END-IF                                                    19070003
190800     END-IF                                                       19080003
190900                                                                  19090003
191000     PERFORM UNTIL SCHED-DONE                                     19100003
191100        EXEC CICS READNEXT                                        19110003
191200                  DATASET(JS-VIA-JSKEY1)                          19120003
191300                  INTO(WS-JOB-SCHEDULE)                           19130003
191400                  LENGTH(JSKEY1-RLGTH)                            19140003
191500                  RIDFLD(JSKEY1)                                  19150003
191600                  KEYLENGTH(JSKEY1-KLGTH)                         19160003
191700                  RESP(WS-RESPONSE)                               19170003
191800        END-EXEC                                                  19180003
191900        MOVE WS-RESPONSE               TO FILE-STATUS             19190003
192000        IF SUCCESS                                                19200003
192100          SET DE-YYMMDD-FORMAT TO TRUE                            19210003
192200          MOVE JSK1-EXP-DATE TO DE-YYMMDD                         19220003
192300          PERFORM P8998-DATEEDIT                                  19230003
192400          MOVE DE-CCYYMMDD TO DE-COMPARE1-DATE                    19240003
192500                                                                  19250003
192600          SET DE-YYMMDD-FORMAT TO TRUE                            19260003
192700          MOVE WK-JSK1-EXP-DATE TO DE-YYMMDD                      19270003
192800          PERFORM P8998-DATEEDIT                                  19280003
192900          MOVE DE-CCYYMMDD TO DE-COMPARE2-DATE                    19290003
193000          IF JSK1-ASGN-DIST     = WK-JSK1-ASGN-DIST     AND       19300003
193100             JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST AND       19310003
193200             JSK1-ASSIGNMENT    = WK-JSK1-ASSIGNMENT    AND       19320003
193300             DE-COMPARE1-DATE NOT < DE-COMPARE2-DATE              19330003
193400             IF JSK1-ASGN-DAY   < WK-JSK1-ASGN-DAY                19340003
193500                CONTINUE                                          19350003
193600             ELSE                                                 19360003
193700                IF JSK1-ASGN-DAY = WK-JSK1-ASGN-DAY               19370003
193800                   IF JOB-SCHED-ASGN-OFF                          19380003
193900                      MOVE '1200'                                 19390003
194000                               TO SAVE-JSK1-ASGN-START-TIME       19400003
194100                   ELSE                                           19410003
194200                      MOVE JSK1-ASGN-START-TIME                   19420003
194300                               TO SAVE-JSK1-ASGN-START-TIME       19430003
194400                   END-IF                                         19440003
194500                   SET SCHED-DONE     TO TRUE                     19450003
194600                ELSE                                              19460003
194700                   SET SCHED-DONE     TO TRUE                     19470003
194800                END-IF                                            19480003
194900             END-IF                                               19490003
195000          ELSE                                                    19500003
195100             SET SCHED-DONE     TO TRUE                           19510003
195200          END-IF                                                  19520003
195300        ELSE                                                      19530003
195400           SET SCHED-DONE         TO TRUE                         19540003
195500           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  19550003
195600              MOVE 'P1175-3'           TO ERR-PARAGRAPH           19560003
195700              MOVE JSKEY1              TO ERR-KEY                 19570003
195800              PERFORM P9999-GOT-PROBLEM                           19580003
195900           END-IF                                                 19590003
196000        END-IF                                                    19600003
196100     END-PERFORM                                                  19610003
196200                                                                  19620003
196300     EXEC CICS ENDBR                                              19630003
196400               DATASET(JS-VIA-JSKEY1)                             19640003
196500               RESP(WS-RESPONSE)                                  19650003
196600     END-EXEC.                                                    19660003
196700*                                                                 19670003
196800 P1200-CHECK-AUTHORITY.                                           19680003
196900*                                                                 19690003
197000     MOVE SPACE           TO WS-AUTHORITY                         19700003
197100     EXEC CICS ASSIGN                                             19710003
197200               USERID(USER-ID)                                    19720003
197300     END-EXEC                                                     19730003
197400     MOVE DIST OF WS-MSTR     TO USERID-DIST-1                    19740003
197500     MOVE SUB-DIST OF WS-MSTR TO USERID-SDIST-1                   19750003
197600     EXEC CICS LINK                                               19760003
197700               PROGRAM(P902-PGM)                                  19770003
197800               COMMAREA(WS-AUTHORITY)                             19780003
197900               LENGTH(P902-LGTH)                                  19790003
198000               RESP(WS-RESPONSE)                                  19800003
198100     END-EXEC                                                     19810003
198200     MOVE WS-RESPONSE TO FILE-STATUS                              19820003
198300     IF NOT SUCCESS                                               19830003
198400        MOVE 'P1200-1'  TO ERR-PARAGRAPH                          19840003
198500        PERFORM P9999-GOT-PROBLEM                                 19850003
198600     END-IF.                                                      19860003
198700*                                                                 19870003
198800 P1500-UPDATE-UNAVAIL-LAST.                                       19880003
198900*                                                                 19890003
199000     PERFORM P8200-READ-MSTR-UPDATE                               19900003
199100     IF NOT SUCCESS                                               19910003
199200        MOVE 'P1500-1'               TO ERR-PARAGRAPH             19920003
199300        MOVE MSTRNBRK                TO ERR-KEY                   19930003
199400        PERFORM P9999-GOT-PROBLEM                                 19940003
199500     END-IF                                                       19950003
199600     MOVE SPACES                     TO UNAVAILABLE-LAST-FLAG     19960003
199700     MOVE SPACES                     TO LAYOFF-EM-CODE            19970003
199800     PERFORM P8210-REWRITE-MSTR                                   19980003
199900     IF NOT SUCCESS                                               19990003
200000        MOVE 'P1500-2'               TO ERR-PARAGRAPH             20000003
200100        MOVE MSTRNBRK                TO ERR-KEY                   20010003
200200        PERFORM P9999-GOT-PROBLEM                                 20020003
200300     END-IF.                                                      20030003
200400*                                                                 20040003
200500*    WRITE MARKUP EMP HISTORY RECORD TO SHOW THE PERSON IS NO     20050003
200600*    LONGER NOTIFY HELD                                           20060003
200700*                                                                 20070003
200800     MOVE SPACES                     TO P943-COMMAREA-PARMS       20080003
200900     SET P943-END-NTFY-HOLD-FUN      TO TRUE                      20090003
201000     MOVE P917-EMP-NO                TO P943-EMP-NBR              20100003
201100     MOVE WS-EFF-DATE-TIME           TO P943-EFF-DATE-TIME        20110003
201200     MOVE WS-TIME-ZONE               TO P943-EMP-TIME-ZONE        20120003
201300     MOVE DIST     OF WS-MSTR        TO P943-DIST                 20130003
201400     MOVE SUB-DIST OF WS-MSTR        TO P943-SDIST                20140003
201500     MOVE CRAFT    OF WS-MSTR        TO P943-CRAFT                20150003
201600     MOVE LAYOFF-CODE                TO P943-LO-CODE              20160003
201700     MOVE LAYOFF-EM-CODE             TO P943-FUN17-PREV-ECC-CODE  20170003
201800     IF TEMPORARY-ASGNMT > SPACE                                  20180003
201900        MOVE TEMPORARY-ASGNMT        TO P943-NORM-ASGN            20190003
202000        IF TEMP-ASGN-UFP                                          20200003
202100           MOVE TA-POOL              TO P943-POOL-ASG             20210003
202200        END-IF                                                    20220003
202300     ELSE                                                         20230003
202400        MOVE NORMAL-ASGNMT           TO P943-NORM-ASGN            20240003
202500        IF NORM-ASGN-UFP                                          20250003
202600           MOVE NA-POOL              TO P943-POOL-ASG             20260003
202700        END-IF                                                    20270003
202800     END-IF                                                       20280003
202900     PERFORM P8900-WRITE-HISTORY                                  20290003
203000     PERFORM P2500-RELEASE-EMPS.                                  20300003
203100*                                                                 20310003
203200 P1550-CHECK-VRU-ERRORS.                                          20320003
203300*                                                                 20330003
203400     MOVE SPACES      TO P917-ERRORCODE                           20340003
203500     MOVE P917-EMP-NO TO MSTRNBRK                                 20350003
203600     EXEC CICS READ                                               20360003
203700               DATASET(MSTR-VIA-EMP-NBR)                          20370003
203800               INTO(WS-MSTR)                                      20380003
203900               LENGTH(MSTRENBR-RLGTH)                             20390003
204000               RIDFLD(MSTRNBRK)                                   20400003
204100               KEYLENGTH(MSTRENBR-KLGTH)                          20410003
204200               RESP(WS-RESPONSE)                                  20420003
204300     END-EXEC                                                     20430003
204400     MOVE WS-RESPONSE TO FILE-STATUS                              20440003
204500     IF NOT SUCCESS                                               20450003
204600        MOVE 'P1550-1' TO ERR-PARAGRAPH                           20460003
204700        MOVE MSTRNBRK  TO ERR-KEY                                 20470003
204800        PERFORM P9999-GOT-PROBLEM                                 20480003
204900     END-IF                                                       20490003
205000     IF P917-STATUS-CODE NOT > SPACE                              20500003
205100        SET P917-STATUS-CODE-ERROR TO TRUE                        20510003
205200*       MOVE 'P1550-2'  TO ERR-PARAGRAPH                          20520003
205300*       MOVE 'STATCODE' TO ERR-KEY                                20530003
205400        MOVE '13'      TO P917-BOOKON-REJECT-CODE                 20540003
205500        PERFORM P9000-RETURN                                      20550003
205600     END-IF                                                       20560003
205700                                                                  20570003
205800     IF (AVAILABLE OR WORKING)                                    20580003
205900      AND (WS-BEFORE-STATUS = 'A' OR 'B')                         20590003
206000        SET P917-MISC-ERROR TO TRUE                               20600003
206100*       MOVE 'ALREADY AVAIL/WORKING' TO P917-ERRORMSG             20610003
206200        MOVE 'A100' TO MSGLOG-CODE                                20620003
206300        MOVE '14'      TO P917-BOOKON-REJECT-CODE                 20630003
206400        PERFORM P9000-RETURN                                      20640003
206500     END-IF                                                       20650003
206600                                                                  20660003
206700     PERFORM P8510-MASTER-JOBS                                    20670003
206800                                                                  20680003
206900     IF NORMAL-ASGNMT NOT > SPACES                                20690003
207000      AND TEMPORARY-ASGNMT NOT > SPACES                           20700003
207100      AND NOT P917-VRU-UPDATE-LAYOFF                              20710003
207200        SET P917-MISC-ERROR TO TRUE                               20720003
207300*       MOVE 'NOT ALLOWED - NO ASSIGNMENT' TO P917-ERRORMSG       20730003
207400        MOVE 'N141' TO MSGLOG-CODE                                20740003
207500        MOVE '15'      TO P917-BOOKON-REJECT-CODE                 20750003
207600        PERFORM P9000-RETURN                                      20760003
207700     ELSE                                                         20770003
207800        IF TEMPORARY-ASGNMT > SPACE                               20780003
207900           MOVE TEMPORARY-ASGNMT-FLAG TO ASGN-JOB-TYPE            20790003
208000           MOVE TEMPORARY-ASGNMT      TO ASGN-ASSIGNMENT          20800003
208100        ELSE                                                      20810003
208200           MOVE NORMAL-ASGNMT-FLAG    TO ASGN-JOB-TYPE            20820003
208300           MOVE NORMAL-ASGNMT         TO ASGN-ASSIGNMENT          20830003
208400        END-IF                                                    20840003
208500     END-IF                                                       20850003
208600                                                                  20860003
208700*                                                                 20870003
208800*    SEE IF THE EMPLOYEE IS PENDING NOTIFICATION                  20880003
208900*    IF SO HE CANNOT MARKUP.                                      20890003
209000*                                                                 20900003
209100     MOVE SPACES                        TO WS-TASK                20910003
209200     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                20920003
209300     MOVE 'N' TO TASK-TYPE                                        20930003
209400     MOVE TASK-EMPLOYEE-KEY  TO TASKEMPK                          20940003
209500     EXEC CICS READ                                               20950003
209600               DATASET(TASK-VIA-EMP-NBR)                          20960003
209700               INTO(WS-TASK)                                      20970003
209800               LENGTH(TASKENBR-RLGTH)                             20980003
209900               RIDFLD(TASKEMPK)                                   20990003
210000               KEYLENGTH(TASKENBR-KLGTH)                          21000003
210100               RESP(WS-RESPONSE)                                  21010003
210200     END-EXEC                                                     21020003
210300     MOVE WS-RESPONSE TO FILE-STATUS                              21030003
210400     IF NOT SUCCESS                                               21040003
210500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     21050003
210600           MOVE 'P1550-3' TO ERR-PARAGRAPH                        21060003
210700           MOVE TASKEMPK  TO ERR-KEY                              21070003
210800           PERFORM P9999-GOT-PROBLEM                              21080003
210900        END-IF                                                    21090003
211000     ELSE                                                         21100003
211100        SET P917-MISC-ERROR TO TRUE                               21110003
211200*       MOVE 'NOT ALLOWED - PENDING NOTIFICATION(N)'              21120003
211300        MOVE 'N142' TO MSGLOG-CODE                                21130003
211400        MOVE '16'      TO P917-BOOKON-REJECT-CODE                 21140003
211500        PERFORM P9000-RETURN                                      21150003
211600     END-IF                                                       21160003
211700                                                                  21170003
211800     MOVE SPACES                        TO WS-TASK                21180003
211900     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                21190003
212000     MOVE 'T' TO TASK-TYPE                                        21200003
212100     MOVE TASK-EMPLOYEE-KEY  TO TASKEMPK                          21210003
212200     EXEC CICS READ                                               21220003
212300               DATASET(TASK-VIA-EMP-NBR)                          21230003
212400               INTO(WS-TASK)                                      21240003
212500               LENGTH(TASKENBR-RLGTH)                             21250003
212600               RIDFLD(TASKEMPK)                                   21260003
212700               KEYLENGTH(TASKENBR-KLGTH)                          21270003
212800               RESP(WS-RESPONSE)                                  21280003
212900     END-EXEC                                                     21290003
213000     MOVE WS-RESPONSE TO FILE-STATUS                              21300003
213100     IF NOT SUCCESS                                               21310003
213200        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     21320003
213300           MOVE 'P1550-4' TO ERR-PARAGRAPH                        21330003
213400           MOVE TASKEMPK  TO ERR-KEY                              21340003
213500           PERFORM P9999-GOT-PROBLEM                              21350003
213600        END-IF                                                    21360003
213700     ELSE                                                         21370003
213800        SET P917-MISC-ERROR TO TRUE                               21380003
213900*       MOVE 'NOT ALLOWED - PENDING NOTIFICATION(T)'              21390003
214000        MOVE 'N142' TO MSGLOG-CODE                                21400003
214100        MOVE '17'      TO P917-BOOKON-REJECT-CODE                 21410003
214200        PERFORM P9000-RETURN                                      21420003
214300     END-IF                                                       21430003
214400                                                                  21440003
214500*                                                                 21450003
214600*    SEE IF THE EMPLOYEE ALREADY HAS A FUTURE MARKUP/LAYOFF       21460003
214700*    IF SO HE CANNOT MARKUP.                                      21470003
214800*                                                                 21480003
214900     MOVE SPACES                        TO WS-TASK                21490003
215000     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                21500003
215100     MOVE 'F' TO TASK-TYPE                                        21510003
215200     MOVE TASK-EMPLOYEE-KEY  TO TASKEMPK                          21520003
215300     EXEC CICS READ                                               21530003
215400               DATASET(TASK-VIA-EMP-NBR)                          21540003
215500               INTO(WS-TASK)                                      21550003
215600               LENGTH(TASKENBR-RLGTH)                             21560003
215700               RIDFLD(TASKEMPK)                                   21570003
215800               KEYLENGTH(TASKENBR-KLGTH)                          21580003
215900               RESP(WS-RESPONSE)                                  21590003
216000     END-EXEC                                                     21600003
216100     MOVE WS-RESPONSE TO FILE-STATUS                              21610003
216200     IF NOT SUCCESS                                               21620003
216300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     21630003
216400           MOVE 'P1550-5' TO ERR-PARAGRAPH                        21640003
216500           MOVE TASKEMPK  TO ERR-KEY                              21650003
216600           PERFORM P9999-GOT-PROBLEM                              21660003
216700        END-IF                                                    21670003
216800     ELSE                                                         21680003
216900        IF TASK-LO1 = 'F'                                         21690003
217000           MOVE EFF-DATE IN WS-TASK   TO WS-MARKUP-OK-AT-DATE     21700003
217100           MOVE EFF-HR-MN IN WS-TASK  TO WS-MARKUP-OK-AT-TIME     21710003
217200                                                                  21720003
217300           SET DE-YYMMDD-FORMAT       TO TRUE                     21730003
217400           MOVE WS-MARKUP-OK-AT-DATE  TO DE-YYMMDD                21740003
217500           PERFORM P8998-DATEEDIT                                 21750003
217600           MOVE DE-CCYYMMDD           TO DE-COMPARE1-DATE         21760003
217700           MOVE WS-MARKUP-OK-AT-TIME  TO DE-COMPARE1-TIME         21770003
217800                                                                  21780003
217900           IF DE-COMPARE1-DATE-TIME > WS-EFF-DATE-TIME-CE         21790003
218000              SET P917-MISC-ERROR     TO TRUE                     21800003
218100*             STRING 'NOT ALLOWED - HAS NOT MET MINIMUM OFF '     21810003
218200*                    'REQUIREMENTS'                               21820003
218300              MOVE 'N143' TO MSGLOG-CODE                          21830003
218400              MOVE '18'      TO P917-BOOKON-REJECT-CODE           21840003
218500              PERFORM P9000-RETURN                                21850003
218600           END-IF                                                 21860003
218700        ELSE                                                      21870003
218800           SET P917-MISC-ERROR TO TRUE                            21880003
218900*          MOVE 'NOT ALLOWED - FUTURE LO/MU'                      21890003
219000           MOVE 'N144' TO MSGLOG-CODE                             21900003
219100           MOVE '19'      TO P917-BOOKON-REJECT-CODE              21910003
219200           PERFORM P9000-RETURN                                   21920003
219300        END-IF                                                    21930003
219400     END-IF                                                       21940003
219500                                                                  21950003
219600*                                                                 21960003
219700*    READ THE EMPLOYEES UFP OR AJ RECORD INTO WORKING STORAGE     21970003
219800*                                                                 21980003
219900     IF ASGN-JOB-TYPE = 'A'                                       21990003
220000        MOVE SPACES             TO WS-ASGNED-JOBS                 22000003
220100        MOVE ASGN-ASSIGNMENT    TO AJJOBKEY                       22010003
220200        EXEC CICS READ                                            22020003
220300                  DATASET(AJ-VIA-JNAME-JCRAFT)                    22030003
220400                  INTO(WS-ASGNED-JOBS)                            22040003
220500                  LENGTH(AJNAMECR-RLGTH)                          22050003
220600                  RIDFLD(AJJOBKEY)                                22060003
220700                  KEYLENGTH(AJNAMECR-KLGTH)                       22070003
220800                  RESP(WS-RESPONSE)                               22080003
220900        END-EXEC                                                  22090003
221000        MOVE WS-RESPONSE TO FILE-STATUS                           22100003
221100        IF NOT SUCCESS                                            22110003
221200           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  22120003
221300              MOVE 'P1550-6' TO ERR-PARAGRAPH                     22130003
221400              MOVE AJJOBKEY  TO ERR-KEY                           22140003
221500              PERFORM P9999-GOT-PROBLEM                           22150003
221600           ELSE                                                   22160003
221700              SET P917-MISC-ERROR TO TRUE                         22170003
221800*             MOVE 'NOT ALLOWED - AJ NOT FOUND' TO P917-ERRORMSG  22180003
221900              MOVE 'N145' TO MSGLOG-CODE                          22190003
222000              MOVE '20'      TO P917-BOOKON-REJECT-CODE           22200003
222100           END-IF                                                 22210003
222200        END-IF                                                    22220003
222300     ELSE                                                         22230003
222400        IF ASGN-JOB-TYPE = 'U'                                    22240003
222500           MOVE SPACES             TO WS-UFP                      22250003
222600           MOVE ASGN-DIST          TO DIST2                       22260003
222700           MOVE ASGN-SUB-DIST      TO SUB-DIST2                   22270003
222800           MOVE ASGN-UFP-POOL      TO POOL-NAME2                  22280003
222900           MOVE ASGN-UFP-TURN      TO TURN-NBR OF WS-UFP          22290003
223000           MOVE ASGN-UFP-CC        TO POOL-CRAFT-CODE2            22300003
223100           MOVE UFPTURN-AREA       TO UFPTURN                     22310003
223200           EXEC CICS READ                                         22320003
223300                     DATASET(UFP-VIA-TURN-NBR)                    22330003
223400                     INTO(WS-UFP)                                 22340003
223500                     LENGTH(UFPTURN-RLGTH)                        22350003
223600                     RIDFLD(UFPTURN)                              22360003
223700                     KEYLENGTH(UFPTURN-KLGTH)                     22370003
223800                     RESP(WS-RESPONSE)                            22380003
223900           END-EXEC                                               22390003
224000           MOVE WS-RESPONSE TO FILE-STATUS                        22400003
224100           IF NOT SUCCESS                                         22410003
224200              IF NOT (NO-RECORD-FND OR END-OF-FILE)               22420003
224300                 MOVE 'P1550-7' TO ERR-PARAGRAPH                  22430003
224400                 MOVE UFPTURN   TO ERR-KEY                        22440003
224500                 PERFORM P9999-GOT-PROBLEM                        22450003
224600              ELSE                                                22460003
224700                 SET P917-MISC-ERROR TO TRUE                      22470003
224800*                MOVE 'NOT ALLOWED - UFP NOT FOUND'               22480003
224900                 MOVE 'N146' TO MSGLOG-CODE                       22490003
225000                 MOVE '21'      TO P917-BOOKON-REJECT-CODE        22500003
225100                 PERFORM P9000-RETURN                             22510003
225200              END-IF                                              22520003
225300           END-IF                                                 22530003
225400        END-IF                                                    22540003
225500     END-IF                                                       22550003
225600*                                                                 22560003
225700     IF P917-VRU-UPDATE-MARKUP                                    22570003
225800        PERFORM P1600-EDIT-RESTRICTIONS                           22580003
225900     END-IF.                                                      22590003
226000*                                                                 22600003
226100 P1560-CHECK-JOB-SCHED.                                           22610003
226200*                                                                 22620003
226300      MOVE AJJOBKEY-AREA            TO WORK-JS-KEY1               22630003
226400      MOVE WS-CALL-DATE             TO WK-JSK1-EXP-DATE           22640003
226500      MOVE WORK-JS-KEY1             TO JSKEY1                     22650003
226600      EXEC CICS READ                                              22660003
226700                DATASET(JS-VIA-JSKEY1)                            22670003
226800                INTO(WS-JOB-SCHEDULE)                             22680003
226900                LENGTH(JSKEY1-RLGTH)                              22690003
227000                RIDFLD(JSKEY1)                                    22700003
227100                KEYLENGTH(JSKEY1-KLGTH)                           22710003
227200                GTEQ                                              22720003
227300                RESP(WS-RESPONSE)                                 22730003
227400      END-EXEC                                                    22740003
227500      MOVE WS-RESPONSE              TO FILE-STATUS                22750003
227600      IF  SUCCESS                                                 22760003
227700      AND JSK1-ASGN-DIST     = WK-JSK1-ASGN-DIST                  22770003
227800      AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST              22780003
227900      AND JSK1-ASSIGNMENT    = WK-JSK1-ASSIGNMENT                 22790003
228000      AND JSK1-ASGN-STATS                                         22800003
228100         IF JOB-ON-REST-DAY(WS-CALL-DAY)                          22810003
228200            SET NO-RECORD-FND       TO TRUE                       22820003
228300         END-IF                                                   22830003
228400      ELSE                                                        22840003
228500         IF SUCCESS                                               22850003
228600            SET NO-RECORD-FND       TO TRUE                       22860003
228700         END-IF                                                   22870003
228800      END-IF                                                      22880003
228900      IF SUCCESS                                                  22890003
229000         MOVE JSK1-EXP-DATE         TO WK-JSK1-EXP-DATE           22900003
229100         MOVE WS-CALL-DAY           TO WK-JSK1-ASGN-DAY-NUM       22910003
229200         MOVE WS-CALL-FROM          TO WK-JSK1-ASGN-START-TIME    22920003
229300         MOVE WORK-JS-KEY1          TO JSKEY1                     22930003
229400         EXEC CICS READ                                           22940003
229500                   DATASET(JS-VIA-JSKEY1)                         22950003
229600                   INTO(WS-JOB-SCHEDULE)                          22960003
229700                   LENGTH(JSKEY1-RLGTH)                           22970003
229800                   RIDFLD(JSKEY1)                                 22980003
229900                   KEYLENGTH(JSKEY1-KLGTH)                        22990003
230000                   GTEQ                                           23000003
230100                   RESP(WS-RESPONSE)                              23010003
230200         END-EXEC                                                 23020003
230300         MOVE WS-RESPONSE           TO FILE-STATUS                23030003
230400         IF  SUCCESS                                              23040003
230500         AND JSK1-ASGN-DIST     = WK-JSK1-ASGN-DIST               23050003
230600         AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST           23060003
230700         AND JSK1-ASSIGNMENT    = WK-JSK1-ASSIGNMENT              23070003
230800         AND JSK1-EXP-DATE      = WK-JSK1-EXP-DATE                23080003
230900         AND JSK1-ASGN-DAY-NUM  = WK-JSK1-ASGN-DAY                23090003
231000         AND JSK1-ASGN-START-TIME NOT > WS-CALL-TO                23100003
231100            CONTINUE                                              23110003
231200         ELSE                                                     23120003
231300            IF SUCCESS                                            23130003
231400               SET NO-RECORD-FND    TO TRUE                       23140003
231500            END-IF                                                23150003
231600         END-IF                                                   23160003
231700      END-IF                                                      23170003
231800      IF NOT SUCCESS                                              23180003
231900         IF NOT (NO-RECORD-FND OR END-OF-FILE)                    23190003
232000            MOVE 'P1560-1'          TO ERR-PARAGRAPH              23200003
232100            MOVE JSKEY1             TO ERR-KEY                    23210003
232200            PERFORM P9999-GOT-PROBLEM                             23220003
232300         END-IF                                                   23230003
232400      END-IF.                                                     23240003
232500*                                                                 23250003
232600*                                                                 23260003
232700 P1600-EDIT-RESTRICTIONS.                                         23270003
232800*                                                                 23280003
232900*    SEE IF THE EMPLOYEE IS ALLOWED TO MARKUP FROM HIS PARTICULAR 23290003
233000*    LAYOFF IF ON THE VRU AND SEE IF HE HAS BEEN OFF FOR THE      23300003
233100*    MINIMUM HOURS REQUIRED.                                      23310003
233200****                                                              23320003
233300**   GET THE STATUS/REASON CNTL RECORD VIA PGM-956                23330003
233400****                                                              23340003
233500     MOVE SPACES                      TO P956-COMMAREA-PARMS      23350003
233600     SET  P956-GET-CNTL-STATUS-REASON TO TRUE                     23360003
233700     IF P917-VRU-UPDATE-MARKUP                                    23370003
233800        MOVE P917-STATUS-CODE1        TO P956-STATUS-CODE         23380003
233900        MOVE P917-EDB-CODE            TO P956-REASON-CODE         23390003
234000     ELSE                                                         23400003
234100        MOVE LAYOFF-CODE-1            TO P956-STATUS-CODE         23410003
234200        MOVE LAYOFF-EM-CODE           TO P956-REASON-CODE         23420003
234300     END-IF                                                       23430003
234400     MOVE DIST     OF WS-MSTR         TO P956-DIST                23440003
234500     MOVE SUB-DIST OF WS-MSTR         TO P956-SDIST               23450003
234600     MOVE CRAFT OF WS-MSTR            TO P956-CC                  23460003
234700     IF TEMPORARY-ASGNMT > SPACE                                  23470003
234800        MOVE TEMPORARY-ASGNMT-FLAG TO P956-ASGN-TYPE              23480003
234900        MOVE TA-1                  TO P956-ASGN                   23490003
235000        MOVE TA-DIST               TO P956-DIST                   23500003
235100        MOVE TA-SUB-DIST           TO P956-SDIST                  23510003
235200        IF TEMP-ASGN-XB                                           23520003
235300           MOVE TA-CC              TO P956-XB                     23530003
235400        END-IF                                                    23540003
235500     ELSE                                                         23550003
235600        IF NORMAL-ASGNMT > SPACES                                 23560003
235700           MOVE NORMAL-ASGNMT-FLAG TO P956-ASGN-TYPE              23570003
235800           MOVE NA-1               TO P956-ASGN                   23580003
235900           MOVE NA-DIST            TO P956-DIST                   23590003
236000           MOVE NA-SUB-DIST        TO P956-SDIST                  23600003
236100           IF NORM-ASGN-XB                                        23610003
236200              MOVE NA-CC           TO P956-XB                     23620003
236300           END-IF                                                 23630003
236400        END-IF                                                    23640003
236500     END-IF                                                       23650003
236600     PERFORM P9840-RETRIEVE-CNTL-INFO                             23660003
236700     IF  P956-ERROR-FOUND                                         23670003
236800         MOVE 'P1600-1'              TO ERR-PARAGRAPH             23680003
236900         MOVE 'P956 LINK'            TO ERR-SENTENCE              23690003
237000         MOVE P956-INPUT-PARMS       TO ERR-KEY                   23700003
237100         PERFORM P9999-GOT-PROBLEM                                23710003
237200     END-IF                                                       23720003
237300     IF  P956-NO-ERROR                                            23730003
237400      OR P956-WARNING-FOUND                                       23740003
237500        IF (P917-VRU-FUNCTION > SPACES) AND                       23750003
237600            ((NOT P917-VRU-UPDATE-LAYOFF) AND                     23760003
237700             (NOT P956-ST-RSN-VRU-MARKUP AND                      23770003
237800              NOT P956-ST-RSN-VRU-OK))                            23780003
237900                         OR                                       23790003
238000            ((P917-VRU-UPDATE-LAYOFF) AND                         23800003
238100             (NOT P956-ST-RSN-VRU-LAYOFF AND                      23810003
238200              NOT P956-ST-RSN-VRU-OK))                            23820003
238300               SET P917-MISC-ERROR TO TRUE                        23830003
238400               STRING 'VRU MKUP/LAYOFF NOT ALLOWED, CNTL: '       23840003
238500                   P956-STATUS-CODE                               23850003
238600                   '/'                                            23860003
238700                   P956-REASON-CODE                               23870003
238800                   '/'                                            23880003
238900                   P956-CC                                        23890003
239000                   '/'                                            23900003
239100                   P956-DIST                                      23910003
239200                   '/'                                            23920003
239300                   P956-SDIST                                     23930003
239400                   '/'                                            23940003
239500                   P956-ASGN-TYPE                                 23950003
239600                   '/'                                            23960003
239700                   P956-ASGN                                      23970003
239800                   DELIMITED BY SIZE                              23980003
239900                   INTO P917-ERRORMSG                             23990003
240000                   MOVE '22'      TO P917-BOOKON-REJECT-CODE      24000003
240100                   SET P917-MISC-ERROR TO TRUE                    24010003
240200                   PERFORM P9000-RETURN                           24020003
240300        ELSE                                                      24030003
240400           MOVE ZEROS TO DATE-CONVERSION-PARMS                    24040003
240500           SET PARM-ADD TO TRUE                                   24050003
240600           MOVE LAYOFF-DATE TO PARM-PRI-DATE-GREG                 24060003
240700           MOVE LAYOFF-TIME-OF-DAY TO PARM-PRI-HRMN               24070003
240800           IF  P956-ST-RSN-MIN-OFF-REGULAR = SPACES               24080003
240900               MOVE ZEROS      TO P956-ST-RSN-MIN-OFF-REGULAR     24090003
241000           END-IF                                                 24100003
241100           IF  P956-ST-RSN-MIN-OFF-ONCALL  = SPACES               24110003
241200               MOVE ZEROS      TO P956-ST-RSN-MIN-OFF-ONCALL      24120003
241300           END-IF                                                 24130003
241400           IF LAYOFF-CODE-2 = '2'                                 24140003
241500               MOVE P956-ST-RSN-MIN-OFF-ONCALL                    24150003
241600                   TO WS-NUMBER-OF-HOURS                          24160003
241700           ELSE                                                   24170003
241800               MOVE P956-ST-RSN-MIN-OFF-REGULAR                   24180003
241900                   TO WS-NUMBER-OF-HOURS                          24190003
242000           END-IF                                                 24200003
242100           IF WS-NUMBER-OF-HOURS > ZERO                           24210003
242200              IF WS-NUMBER-OF-HOURS > 02400                       24220003
242300                 PERFORM UNTIL WS-NUMBER-OF-HOURS < 02400         24230003
242400                       SUBTRACT 02400 FROM WS-NUMBER-OF-HOURS     24240003
242500                       ADD 1 TO WS-NUMBER-OF-DAYS-REST            24250003
242600                 END-PERFORM                                      24260003
242700                 MOVE WS-NUMBER-OF-HOURS TO                       24270003
242800                               WS-NUMBER-OF-HOURS-REST            24280003
242900              ELSE                                                24290003
243000                 MOVE WS-NUMBER-OF-HOURS TO                       24300003
243100                                WS-NUMBER-OF-HOURS-REST           24310003
243200                 MOVE ZEROES TO WS-NUMBER-OF-DAYS-REST            24320003
243300              END-IF                                              24330003
243400              MOVE WS-NUMBER-OF-DAYS-REST TO                      24340003
243500                                   PARM-SEC-DATE-GREG             24350003
243600              MOVE WS-NUMBER-OF-HOURS-REST TO                     24360003
243700                                    PARM-SEC-HRMN                 24370003
243800              EXEC CICS LINK                                      24380003
243900                        PROGRAM(P903-PGM)                         24390003
244000                        COMMAREA(DATE-CONVERSION-PARMS)           24400003
244100                        LENGTH(P903-LGTH)                         24410003
244200                        RESP(WS-RESPONSE)                         24420003
244300              END-EXEC                                            24430003
244400              MOVE WS-RESPONSE          TO FILE-STATUS            24440003
244500              IF NOT SUCCESS                                      24450003
244600                 MOVE 'P1600-4'         TO ERR-PARAGRAPH          24460003
244700                 PERFORM P9999-GOT-PROBLEM                        24470003
244800              END-IF                                              24480003
244900              MOVE PARM-RES-DATE-GREG   TO WS-MARKUP-OK-AT-DATE   24490003
245000              MOVE PARM-RES-HRMN        TO WS-MARKUP-OK-AT-TIME   24500003
245100                                                                  24510003
245200              SET DE-YYMMDD-FORMAT      TO TRUE                   24520003
245300              MOVE WS-MARKUP-OK-AT-DATE TO DE-YYMMDD              24530003
245400              PERFORM P8998-DATEEDIT                              24540003
245500              MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE       24550003
245600              MOVE WS-MARKUP-OK-AT-TIME TO DE-COMPARE1-TIME       24560003
245700                                                                  24570003
245800              IF DE-COMPARE1-DATE-TIME > WS-EFF-DATE-TIME-CE      24580003
245900                 IF P917-VRU-FUNCTION > SPACES                    24590003
246000                    IF P917-VRU-UPDATE-MARKUP                     24600003
246100                       SET P917-MISC-ERROR TO TRUE                24610003
246200*                      STRING '    NOT ALLOWED - '                24620003
246300*                             'CANNOT MARKUP UNTIL '              24630003
246400*                              WS-MARKUP-OK-AT                    24640003
246500                       MOVE 'N147' TO MSGLOG-CODE                 24650003
246600                       MOVE '23'   TO P917-BOOKON-REJECT-CODE     24660003
246700                    END-IF                                        24670003
246800                 ELSE                                             24680003
246900                    IF P917-SUPV-INIT NOT > SPACE                 24690003
247000                       SET P917-SUPV-INIT-ERROR TO TRUE           24700003
247100*                      STRING 'M/U ATTEMPT PRIOR TO MIN '         24710003
247200*                            'OFF - CANNOT MARKUP '               24720003
247300*                                WS-MARKUP-OK-AT-DATE             24730003
247400*                                '-'  WS-MARKUP-OK-AT-TIME        24740003
247500*                            ' - SUPV REQUIRED'                   24750003
247600                       MOVE 'C133' TO MSGLOG-CODE                 24760003
247700                       MOVE '24'   TO P917-BOOKON-REJECT-CODE     24770003
247800                    END-IF                                        24780003
247900                 END-IF                                           24790003
248000              END-IF                                              24800003
248100           END-IF                                                 24810003
248200        END-IF                                                    24820003
248300     END-IF.                                                      24830003
248400*                                                                 24840003
248500 P1700-CHECK-EDITS-ONLY.                                          24850003
248600*                                                                 24860003
248700*    IN THIS PARAGRAPH, P917-STATUS-CODE IS THE EMPLOYEE'S CURRENT24870003
248800*    STATUS (SO IF 'P917-MARKUP' IS TRUE, THE EMP IS AVAILABLE).  24880003
248900*                                                                 24890003
249000     MOVE SPACES           TO P917-ERRORCODE                      24900003
249100     MOVE P917-EMP-NO      TO MSTRNBRK                            24910003
249200     EXEC CICS READ                                               24920003
249300               DATASET(MSTR-VIA-EMP-NBR)                          24930003
249400               INTO(WS-MSTR)                                      24940003
249500               LENGTH(MSTRENBR-RLGTH)                             24950003
249600               RIDFLD(MSTRNBRK)                                   24960003
249700               KEYLENGTH(MSTRENBR-KLGTH)                          24970003
249800               RESP(WS-RESPONSE)                                  24980003
249900     END-EXEC                                                     24990003
250000     MOVE WS-RESPONSE      TO FILE-STATUS                         25000003
250100     IF NOT SUCCESS                                               25010003
250200        MOVE 'P1700-1'     TO ERR-PARAGRAPH                       25020003
250300        MOVE MSTRNBRK      TO ERR-KEY                             25030003
250400        PERFORM P9999-GOT-PROBLEM                                 25040003
250500     END-IF                                                       25050003
250600*                                                                 25060003
250700*    GET ALL OF THE EMPLOYEES CURRENT ASSIGNMENTS                 25070003
250800*                                                                 25080003
250900     PERFORM P8510-MASTER-JOBS                                    25090003
251000     MOVE P917-TIME-ZONE      TO WS-TIME-ZONE                     25100003
251100*                                                                 25110003
251200*    CONVERT SYSTEM TIME TO LOCAL TIME                            25120003
251300*                                                                 25130003
251400     MOVE SPACES              TO TZ-PARAMETERS                    25140003
251500     SET  TZ-IN-EASTERN-ZONE  TO TRUE                             25150003
251600     MOVE WS-PRESENT-TIME     TO TZ-IN-DATE-TIME                  25160003
251700     MOVE WS-TIME-ZONE        TO TZ-OUT-ZONE                      25170003
251800     PERFORM P8996-TIMEZONE                                       25180003
251900     MOVE TZ-OUT-DATE-TIME    TO WS-LOCAL-DATE-TIME               25190003
252000     MOVE TZ-OUT-CE           TO WS-LOCAL-CENT                    25200003
252100*                                                                 25210003
252200*    CONVERT THE EFFECTIVE DATE/TIME TO THE EMPLOYEES TIME ZONE   25220003
252300*                                                                 25230003
252400     MOVE SPACES TO TZ-PARAMETERS                                 25240003
252500     MOVE P917-TIME-ZONE           TO TZ-IN-ZONE                  25250003
252600     MOVE P917-EFF-DATE-TIME       TO TZ-IN-DATE-TIME             25260003
252700     MOVE WS-TIME-ZONE             TO TZ-OUT-ZONE                 25270003
252800     PERFORM P8996-TIMEZONE                                       25280003
252900     MOVE TZ-OUT-DATE-TIME         TO WS-EFF-DATE-TIME            25290003
253000     MOVE TZ-OUT-CE                TO WS-EFF-CE                   25300003
253100*                                                                 25310003
253200     MOVE SPACES                      TO P956-COMMAREA-PARMS      25320003
253300     SET  P956-GET-CNTL-STATUS-REASON TO TRUE                     25330003
253400     MOVE P917-STATUS-CODE1           TO P956-STATUS-CODE         25340003
253500     MOVE P917-EDB-CODE               TO P956-REASON-CODE         25350003
253600     MOVE DIST     OF WS-MSTR         TO P956-DIST                25360003
253700     MOVE SUB-DIST OF WS-MSTR         TO P956-SDIST               25370003
253800     MOVE CRAFT OF WS-MSTR            TO P956-CC                  25380003
253900     IF TEMPORARY-ASGNMT > SPACE                                  25390003
254000        MOVE TEMPORARY-ASGNMT-FLAG    TO P956-ASGN-TYPE           25400003
254100        MOVE TA-1                     TO P956-ASGN                25410003
254200        MOVE TA-DIST                  TO P956-DIST                25420003
254300        MOVE TA-SUB-DIST              TO P956-SDIST               25430003
254400        IF TEMP-ASGN-XB                                           25440003
254500           MOVE TA-CC                 TO P956-XB                  25450003
254600        END-IF                                                    25460003
254700     ELSE                                                         25470003
254800        IF NORMAL-ASGNMT > SPACES                                 25480003
254900           MOVE NORMAL-ASGNMT-FLAG    TO P956-ASGN-TYPE           25490003
255000           MOVE NA-1                  TO P956-ASGN                25500003
255100           MOVE NA-DIST               TO P956-DIST                25510003
255200           MOVE NA-SUB-DIST           TO P956-SDIST               25520003
255300           IF NORM-ASGN-XB                                        25530003
255400              MOVE NA-CC              TO P956-XB                  25540003
255500           END-IF                                                 25550003
255600        ELSE                                                      25560003
255700           IF NOT P917-MARKUP                                     25570003
255800              SET P917-MISC-ERROR TO TRUE                         25580003
255900*             MOVE 'NOT ALLOWED - NO ASSIGNMENT' TO P917-ERRORMSG 25590003
256000              MOVE 'N141' TO MSGLOG-CODE                          25600003
256100              MOVE '15' TO P917-BOOKON-REJECT-CODE                25610003
256200              PERFORM P9000-RETURN                                25620003
256300           END-IF                                                 25630003
256400        END-IF                                                    25640003
256500     END-IF                                                       25650003
256600     PERFORM P9840-RETRIEVE-CNTL-INFO                             25660003
256700     IF  P956-ERROR-FOUND                                         25670003
256800         MOVE 'P1700-2'            TO ERR-PARAGRAPH               25680003
256900         MOVE 'P956 LINK'          TO ERR-SENTENCE                25690003
257000         MOVE P956-INPUT-PARMS     TO ERR-KEY                     25700003
257100         PERFORM P9999-GOT-PROBLEM                                25710003
257200     END-IF                                                       25720003
257300     IF ((P956-NO-ERROR                                           25730003
257400        OR P956-WARNING-FOUND)                                    25740003
257500        AND NOT P917-MARKUP)                                      25750003
257600        MOVE ZEROS                 TO DATE-CONVERSION-PARMS       25760003
257700        SET PARM-ADD               TO TRUE                        25770003
257800        MOVE LAYOFF-DATE           TO PARM-PRI-DATE-GREG          25780003
257900        MOVE LAYOFF-TIME-OF-DAY    TO PARM-PRI-HRMN               25790003
258000        IF P956-ST-RSN-MIN-OFF-REGULAR = SPACES                   25800003
258100           MOVE ZEROS              TO P956-ST-RSN-MIN-OFF-REGULAR 25810003
258200        END-IF                                                    25820003
258300        IF P956-ST-RSN-MIN-OFF-ONCALL  = SPACES                   25830003
258400           MOVE ZEROS              TO P956-ST-RSN-MIN-OFF-ONCALL  25840003
258500        END-IF                                                    25850003
258600        IF LAYOFF-CODE-2 = '2'                                    25860003
258700           MOVE P956-ST-RSN-MIN-OFF-ONCALL  TO WS-NUMBER-OF-HOURS 25870003
258800        ELSE                                                      25880003
258900           MOVE P956-ST-RSN-MIN-OFF-REGULAR TO WS-NUMBER-OF-HOURS 25890003
259000        END-IF                                                    25900003
259100        IF WS-NUMBER-OF-HOURS > ZERO                              25910003
259200           IF WS-NUMBER-OF-HOURS > 02400                          25920003
259300              PERFORM UNTIL WS-NUMBER-OF-HOURS < 02400            25930003
259400                    SUBTRACT 02400 FROM WS-NUMBER-OF-HOURS        25940003
259500                    ADD 1               TO WS-NUMBER-OF-DAYS-REST 25950003
259600              END-PERFORM                                         25960003
259700              MOVE WS-NUMBER-OF-HOURS   TO WS-NUMBER-OF-HOURS-REST25970003
259800           ELSE                                                   25980003
259900              MOVE WS-NUMBER-OF-HOURS   TO WS-NUMBER-OF-HOURS-REST25990003
260000              MOVE ZEROES               TO WS-NUMBER-OF-DAYS-REST 26000003
260100           END-IF                                                 26010003
260200           MOVE WS-NUMBER-OF-DAYS-REST  TO PARM-SEC-DATE-GREG     26020003
260300           MOVE WS-NUMBER-OF-HOURS-REST TO PARM-SEC-HRMN          26030003
260400           EXEC CICS LINK                                         26040003
260500                     PROGRAM(P903-PGM)                            26050003
260600                     COMMAREA(DATE-CONVERSION-PARMS)              26060003
260700                     LENGTH(P903-LGTH)                            26070003
260800                     RESP(WS-RESPONSE)                            26080003
260900           END-EXEC                                               26090003
261000           MOVE WS-RESPONSE TO FILE-STATUS                        26100003
261100           IF NOT SUCCESS                                         26110003
261200              MOVE 'P1700-3'            TO ERR-PARAGRAPH          26120003
261300              PERFORM P9999-GOT-PROBLEM                           26130003
261400           END-IF                                                 26140003
261500           MOVE PARM-RES-DATE-GREG      TO WS-MARKUP-OK-AT-DATE   26150003
261600           MOVE PARM-RES-HRMN           TO WS-MARKUP-OK-AT-TIME   26160003
261700                                                                  26170003
261800           SET DE-YYMMDD-FORMAT         TO TRUE                   26180003
261900           MOVE WS-MARKUP-OK-AT-DATE    TO DE-YYMMDD              26190003
262000           PERFORM P8998-DATEEDIT                                 26200003
262100           MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE       26210003
262200           MOVE WS-MARKUP-OK-AT-TIME    TO DE-COMPARE1-TIME       26220003
262300                                                                  26230003
262400           IF DE-COMPARE1-DATE-TIME > WS-EFF-DATE-TIME-CE         26240003
262500              IF P917-VRU-FUNCTION > SPACES                       26250003
262600                 SET P917-MISC-ERROR    TO TRUE                   26260003
262700                 MOVE '23'              TO P917-BOOKON-REJECT-CODE26270003
262800                 PERFORM P9000-RETURN                             26280003
262900              ELSE                                                26290003
263000                 IF P917-SUPV-INIT NOT > SPACE                    26300003
263100                    SET P917-SUPV-INIT-ERROR TO TRUE              26310003
263200*             CANNOT BOOKON PRIOR TO MIN OFF - SUPV INIT REQUIRED 26320003
263300                    MOVE 'C133'         TO MSGLOG-CODE            26330003
263400                    MOVE '24'           TO P917-BOOKON-REJECT-CODE26340003
263500                    PERFORM P9000-RETURN                          26350003
263600                 END-IF                                           26360003
263700              END-IF                                              26370003
263800           END-IF                                                 26380003
263900        END-IF                                                    26390003
264000     END-IF                                                       26400003
264100*                                                                 26410003
264200*    SEE IF THE EMPLOYEE IS PENDING NOTIFICATION                  26420003
264300*    IF SO HE CANNOT MARKUP.                                      26430003
264400*                                                                 26440003
264500     IF NOT P917-MARKUP                                           26450003
264600        MOVE SPACES                     TO WS-TASK                26460003
264700        MOVE EMP-NBR OF WS-MSTR         TO EMP-NBR OF WS-TASK     26470003
264800        MOVE 'N' TO TASK-TYPE                                     26480003
264900        MOVE TASK-EMPLOYEE-KEY          TO TASKEMPK               26490003
265000        EXEC CICS READ                                            26500003
265100                  DATASET(TASK-VIA-EMP-NBR)                       26510003
265200                  INTO(WS-TASK)                                   26520003
265300                  LENGTH(TASKENBR-RLGTH)                          26530003
265400                  RIDFLD(TASKEMPK)                                26540003
265500                  KEYLENGTH(TASKENBR-KLGTH)                       26550003
265600                  RESP(WS-RESPONSE)                               26560003
265700        END-EXEC                                                  26570003
265800        MOVE WS-RESPONSE  TO FILE-STATUS                          26580003
265900        IF NOT SUCCESS                                            26590003
266000           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  26600003
266100              MOVE 'P1700-4' TO ERR-PARAGRAPH                     26610003
266200              MOVE TASKEMPK TO ERR-KEY                            26620003
266300              PERFORM P9999-GOT-PROBLEM                           26630003
266400           END-IF                                                 26640003
266500        ELSE                                                      26650003
266600           SET P917-MISC-ERROR TO TRUE                            26660003
266700*          MOVE 'NOT ALLOWED - PENDING NOTIFICATION(N)'           26670003
266800           MOVE 'N142' TO MSGLOG-CODE                             26680003
266900           MOVE '16'   TO P917-BOOKON-REJECT-CODE                 26690003
267000           PERFORM P9000-RETURN                                   26700003
267100        END-IF                                                    26710003
267200                                                                  26720003
267300        MOVE SPACES                     TO WS-TASK                26730003
267400        MOVE EMP-NBR OF WS-MSTR         TO EMP-NBR OF WS-TASK     26740003
267500        MOVE 'T'                        TO TASK-TYPE              26750003
267600        MOVE TASK-EMPLOYEE-KEY          TO TASKEMPK               26760003
267700        EXEC CICS READ                                            26770003
267800                  DATASET(TASK-VIA-EMP-NBR)                       26780003
267900                  INTO(WS-TASK)                                   26790003
268000                  LENGTH(TASKENBR-RLGTH)                          26800003
268100                  RIDFLD(TASKEMPK)                                26810003
268200                  KEYLENGTH(TASKENBR-KLGTH)                       26820003
268300                  RESP(WS-RESPONSE)                               26830003
268400        END-EXEC                                                  26840003
268500        MOVE WS-RESPONSE       TO FILE-STATUS                     26850003
268600        IF NOT SUCCESS                                            26860003
268700           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  26870003
268800              MOVE 'P1700-5'   TO ERR-PARAGRAPH                   26880003
268900              MOVE TASKEMPK    TO ERR-KEY                         26890003
269000              PERFORM P9999-GOT-PROBLEM                           26900003
269100           END-IF                                                 26910003
269200        ELSE                                                      26920003
269300           SET P917-MISC-ERROR TO TRUE                            26930003
269400*          MOVE 'NOT ALLOWED - PENDING NOTIFICATION(T)'           26940003
269500           MOVE 'N142'         TO MSGLOG-CODE                     26950003
269600           MOVE '17'           TO P917-BOOKON-REJECT-CODE         26960003
269700           PERFORM P9000-RETURN                                   26970003
269800        END-IF                                                    26980003
269900                                                                  26990003
270000*                                                                 27000003
270100*       SEE IF THE EMPLOYEE ALREADY HAS A FUTURE MARKUP/LAYOFF    27010003
270200*       IF SO HE CANNOT MARKUP.                                   27020003
270300*                                                                 27030003
270400        MOVE SPACES                     TO WS-TASK                27040003
270500        MOVE EMP-NBR OF WS-MSTR         TO EMP-NBR OF WS-TASK     27050003
270600        MOVE 'F' TO TASK-TYPE                                     27060003
270700        MOVE TASK-EMPLOYEE-KEY          TO TASKEMPK               27070003
270800        EXEC CICS READ                                            27080003
270900                  DATASET(TASK-VIA-EMP-NBR)                       27090003
271000                  INTO(WS-TASK)                                   27100003
271100                  LENGTH(TASKENBR-RLGTH)                          27110003
271200                  RIDFLD(TASKEMPK)                                27120003
271300                  KEYLENGTH(TASKENBR-KLGTH)                       27130003
271400                  RESP(WS-RESPONSE)                               27140003
271500        END-EXEC                                                  27150003
271600        MOVE WS-RESPONSE     TO FILE-STATUS                       27160003
271700        IF NOT SUCCESS                                            27170003
271800           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  27180003
271900              MOVE 'P1700-6' TO ERR-PARAGRAPH                     27190003
272000              MOVE TASKEMPK  TO ERR-KEY                           27200003
272100              PERFORM P9999-GOT-PROBLEM                           27210003
272200           END-IF                                                 27220003
272300        ELSE                                                      27230003
272400           IF TASK-LO1 = 'F'                                      27240003
272500              MOVE EFF-DATE IN WS-TASK  TO WS-MARKUP-OK-AT-DATE   27250003
272600              MOVE EFF-HR-MN IN WS-TASK TO WS-MARKUP-OK-AT-TIME   27260003
272700                                                                  27270003
272800              SET DE-YYMMDD-FORMAT      TO TRUE                   27280003
272900              MOVE WS-MARKUP-OK-AT-DATE TO DE-YYMMDD              27290003
273000              PERFORM P8998-DATEEDIT                              27300003
273100              MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE       27310003
273200              MOVE WS-MARKUP-OK-AT-TIME TO DE-COMPARE1-TIME       27320003
273300                                                                  27330003
273400              IF DE-COMPARE1-DATE-TIME > WS-EFF-DATE-TIME-CE      27340003
273500                 SET P917-MISC-ERROR    TO TRUE                   27350003
273600*                STRING 'NOT ALLOWED - HAS NOT MET MINIMUM OFF '  27360003
273700*                       'REQUIREMENTS'                            27370003
273800                 MOVE 'N143'            TO MSGLOG-CODE            27380003
273900                 MOVE '18'              TO P917-BOOKON-REJECT-CODE27390003
274000                 PERFORM P9000-RETURN                             27400003
274100              END-IF                                              27410003
274200           ELSE                                                   27420003
274300              SET P917-VRU-NO-DUEBACK TO TRUE                     27430003
274400              IF TASK-LO1 = 'A'                                   27440003
274500                 AND TASK-DUE-BACK                                27450003
274600                 SET P917-VRU-DUEBACK TO TRUE                     27460003
274700              ELSE                                                27470003
274800                 SET P917-MISC-ERROR    TO TRUE                   27480003
274900*                MOVE 'NOT ALLOWED - FUTURE LO/MU'                27490003
275000                 MOVE 'N144'            TO MSGLOG-CODE            27500003
275100                 MOVE '19'              TO P917-BOOKON-REJECT-CODE27510003
275200                 PERFORM P9000-RETURN                             27520003
275300              END-IF                                              27530003
275400           END-IF                                                 27540003
275500        END-IF                                                    27550003
275600     END-IF                                                       27560003
275700*                                                                 27570003
275800     IF P917-MARKUP                                               27580003
275900        IF P956-ST-RSN-SUPV-INIT                                  27590003
276000            SET P917-SUPV-INIT-ERROR    TO TRUE                   27600003
276100*           MOVE 'SUPERVISORS INITIALS ARE REQUIRED'              27610003
276200            MOVE 'S109'                 TO MSGLOG-CODE            27620003
276300            PERFORM P9000-RETURN                                  27630003
276400        END-IF                                                    27640003
276500*                                                                 27650003
276600        IF P956-ST-RSN-REFUSE-LAYOFF                              27660003
276700            SET P917-MISC-ERROR         TO TRUE                   27670003
276800            PERFORM P9000-RETURN                                  27680003
276900        END-IF                                                    27690003
277000     END-IF                                                       27700003
277100*                                                                 27710003
277200*    SEE IF THE CHANGE OF STATUS REQUIRES HIGH LEVEL AUTHORITY    27720003
277300*                                                                 27730003
277400     IF P956-ST-RSN-HL-AUTH                                       27740003
277500        PERFORM P1200-CHECK-AUTHORITY                             27750003
277600        IF NOT HIGH-LEVEL-AUTHORIZED                              27760003
277700           SET P917-STATUS-CODE-ERROR   TO TRUE                   27770003
277800           PERFORM P9000-RETURN                                   27780003
277900        END-IF                                                    27790003
278000     END-IF.                                                      27800003
278100*                                                                 27810003
278200 P2000-UPDATE.                                                    27820003
278300*                                                                 27830003
278400     MOVE P917-EMP-NO TO MSTRNBRK                                 27840003
278500     EXEC CICS READ                                               27850003
278600               DATASET(MSTR-VIA-EMP-NBR)                          27860003
278700               INTO(WS-MSTR)                                      27870003
278800               LENGTH(MSTRENBR-RLGTH)                             27880003
278900               RIDFLD(MSTRNBRK)                                   27890003
279000               KEYLENGTH(MSTRENBR-KLGTH)                          27900003
279100               RESP(WS-RESPONSE)                                  27910003
279200     END-EXEC                                                     27920003
279300     MOVE WS-RESPONSE TO FILE-STATUS                              27930003
279400     IF NOT SUCCESS                                               27940003
279500        MOVE 'P2000-0' TO ERR-PARAGRAPH                           27950003
279600        MOVE MSTRNBRK  TO ERR-KEY                                 27960003
279700        PERFORM P9999-GOT-PROBLEM                                 27970003
279800     END-IF                                                       27980003
279900*    6/7/96 - IF WE ARE COMING FROM EMPLOYEE                      27990003
280000*    TRACKING AND THE GUY'S TURN WAS WORKED BY SOMEONE ELSE AND   28000003
280100*    THE GUY IS STILL UNAVAILBLE AND HAS A PENDED-WAIT-TURN       28010003
280200*    FLAG SET.  IN OTHER WORDS, HE WAS UNAVAILBLE, HIS TURN WAS   28020003
280300*    WORKED BY SOMEONE ELSE AND IT FINALLY TIED BACK HOME.  EMP   28030003
280400*    TRACKING (CNP938) SAYS TO MARK UP OUR HERO, BUT REALLY ALL   28040003
280500*    WE WANT TO DO IS CLEAR THE PENDED-WAIT-TURN AND LEAVE HIM    28050003
280600*    UNAVAILABLE.  ALSO, LET'S NOT CUT A BOOK-ON HISTORY RECORD.  28060003
280700*          *    4/18/1996 -- NEW LOGIC -- (CONTINUED)             28070003
280800*    IF THE EMPLOYEE IS CALLED TO WORK, AND THE EMPLOYEE IS NOT IN28080003
280900*    A GTW SUB-DISTRICT PLACE THE                                 28090003
281000*    EMPLOYEE IN 'AVAILABLE' STATUS AT THE END OF THEIR           28100003
281100*    SHIFT/TRIP.                                                  28110003
281200*                                                                 28120003
281300*    IF THE EMPLOYEE IS CALLED TO WORK, AND THE EMPLOYEE IS IN A  28130003
281400*    A GTW SUB-DISTRICT PUT                                       28140003
281500*    THE EMPLOYEE BACK IN 'VACATION' STATUS AT THE END OF THEIR   28150003
281600*    SHIFT/TRIP.                                                  28160003
281700*                                                                 28170003
281800*    PLEASE NOTE:  THE FLAGS 'WS-BLANK-LAST-UNAVAIL' AND          28180003
281900*    'WS-SET-LAST-UNAVAIL-TO-VAC' ARE HERE AS COMMENTS.  THEY ARE 28190003
282000*    NOT PASSED BACK TO CNP919, WHICH IS WHERE THE ACTUAL         28200003
282100*    UNAVAILABLE-LAST FLAG IS REALLY SET.                         28210003
282200*                                                                 28220003
282300     IF VACATION OF LAYOFF-CODE-1                                 28230003
282400       MOVE SPACE             TO WORK-CNTLKEY                     28240003
282500                                 FULL-VAC-TAKEN-FLAG              28250003
282600       MOVE '01'              TO WK-CNTL-REC-TYPE                 28260003
282700       MOVE DIST   OF WS-MSTR TO WK-CNTL-DIST                     28270003
282800       MOVE WORK-CNTLKEY      TO CNTLKEY                          28280003
282900       EXEC CICS READ                                             28290003
283000                 DATASET(CNTL-FILE-VIA-CNTLKEY)                   28300003
283100                 INTO(WS-CNTL-FILE)                               28310003
283200                 LENGTH(CNTLFILE-RLGTH)                           28320003
283300                 RIDFLD(CNTLKEY)                                  28330003
283400                 KEYLENGTH(CNTLFILE-KLGTH)                        28340003
283500                 RESP(WS-RESPONSE)                                28350003
283600       END-EXEC                                                   28360003
283700       MOVE WS-RESPONSE TO FILE-STATUS                            28370003
283800       IF NOT SUCCESS                                             28380003
283900          MOVE 'P2000-1' TO ERR-PARAGRAPH                         28390003
284000          MOVE CNTLKEY TO ERR-KEY                                 28400003
284100          PERFORM P9999-GOT-PROBLEM                               28410003
284200       END-IF                                                     28420003
284300       IF P917-STATUS-CODE1 = 'B'                                 28430003
284400**           (THE STATUS IS CHANGING TO 'WORKING'                 28440003
284500         IF NOT WS-SAVE-PAY-VAC-AND-WORK                          28450003
284600           PERFORM P2700-SPLIT-VAC-SCHEDULE                       28460003
284700           IF CNTL-COMPANY-CODE = 'I'                             28470003
284800              AND FULL-VAC-NOT-TAKEN                              28480003
284900              MOVE 'B170'                  TO MSGLOG-CODE         28490003
285000           END-IF                                                 28500003
285100           SET WS-BLANK-LAST-UNAVAIL       TO TRUE                28510003
285200         ELSE                                                     28520003
285300           SET WS-SET-LAST-UNAVAIL-TO-VAC  TO TRUE                28530003
285400         END-IF                                                   28540003
285500       ELSE                                                       28550003
285600         IF P917-MKUP-FROM-TRK                                    28560003
285700            IF MSTR-PENDED-WAIT-TURN                              28570003
285800               OR (NORMAL-ASGNMT NOT > SPACES                     28580003
285900               OR NORM-ASGN-AJ)                                   28590003
286000               SET WS-STAY-UNAVAILABLE     TO TRUE                28600003
286100            END-IF                                                28610003
286200         ELSE                                                     28620003
286300           IF P917-STATUS-CODE1 NOT = 'V'                         28630003
286400              PERFORM P2700-SPLIT-VAC-SCHEDULE                    28640003
286500              IF CNTL-COMPANY-CODE = 'I'                          28650003
286600                 AND FULL-VAC-NOT-TAKEN                           28660003
286700                 MOVE 'B170'               TO MSGLOG-CODE         28670003
286800              END-IF                                              28680003
286900              SET WS-BLANK-LAST-UNAVAIL    TO TRUE                28690003
287000           END-IF                                                 28700003
287100         END-IF                                                   28710003
287200       END-IF                                                     28720003
287300     END-IF                                                       28730003
287400                                                                  28740003
287500     IF NOT AVAILABLE AND                                         28750003
287600        NOT WAIT-TURN AND                                         28760003
287700        NOT MISSED-CALL AND                                       28770003
287800        P917-MKUP-FROM-TRK                                        28780003
287900        IF MSTR-PENDED-WAIT-TURN                                  28790003
288000           OR (NORMAL-ASGNMT NOT > SPACES                         28800003
288100           OR NORM-ASGN-AJ)                                       28810003
288200           SET WS-STAY-UNAVAILABLE TO TRUE                        28820003
288300        END-IF                                                    28830003
288400     END-IF                                                       28840003
288500                                                                  28850003
288600     MOVE SPACES                   TO P956-COMMAREA-PARMS         28860003
288700     SET P956-GET-CNTL-STATUS-REASON                              28870003
288800                                   TO TRUE                        28880003
288900     MOVE P917-STATUS-CODE1        TO P956-STATUS-CODE            28890003
289000     MOVE P917-EDB-CODE            TO P956-REASON-CODE            28900003
289100     MOVE DIST     OF WS-MSTR      TO P956-DIST                   28910003
289200     MOVE SUB-DIST OF WS-MSTR      TO P956-SDIST                  28920003
289300     MOVE CRAFT OF WS-MSTR         TO P956-CC                     28930003
289400     IF TEMPORARY-ASGNMT > SPACE                                  28940003
289500        MOVE TEMPORARY-ASGNMT-FLAG TO P956-ASGN-TYPE              28950003
289600        MOVE TA-1                  TO P956-ASGN                   28960003
289700        MOVE TA-DIST               TO P956-DIST                   28970003
289800        MOVE TA-SUB-DIST           TO P956-SDIST                  28980003
289900        IF TEMP-ASGN-XB                                           28990003
290000           MOVE TA-CC              TO P956-XB                     29000003
290100        END-IF                                                    29010003
290200     ELSE                                                         29020003
290300        IF NORMAL-ASGNMT > SPACES                                 29030003
290400           MOVE NORMAL-ASGNMT-FLAG TO P956-ASGN-TYPE              29040003
290500           MOVE NA-1               TO P956-ASGN                   29050003
290600           MOVE NA-DIST            TO P956-DIST                   29060003
290700           MOVE NA-SUB-DIST        TO P956-SDIST                  29070003
290800           IF NORM-ASGN-XB                                        29080003
290900              MOVE NA-CC           TO P956-XB                     29090003
291000           END-IF                                                 29100003
291100        END-IF                                                    29110003
291200     END-IF                                                       29120003
291300     PERFORM P9840-RETRIEVE-CNTL-INFO                             29130003
291400     IF P956-ERROR-FOUND                                          29140003
291500        MOVE 'P2000-2'              TO ERR-PARAGRAPH              29150003
291600        STRING 'P956 LINK...ERROR-FLAG='                          29160003
291700               P956-ERROR-FLAG                                    29170003
291800               DELIMITED BY SIZE  INTO ERR-SENTENCE               29180003
291900        MOVE P956-INPUT-PARMS       TO ERR-KEY                    29190003
292000        PERFORM P9999-GOT-PROBLEM                                 29200003
292100     END-IF                                                       29210003
292200     MOVE P956-COMMAREA-PARMS       TO SAVE-P956-COMMAREA         29220003
292300     MOVE P956-ST-RSN-DEACT-BID-CD  TO SAVE-DEACT-BID-CD          29230003
292400*                                                                 29240003
292500*    PROCESSING FOR MISSED CALLS                                  29250003
292600*                                                                 29260003
292700     MOVE P917-EMP-NO TO MSTRNBRK                                 29270003
292800     PERFORM P8200-READ-MSTR-UPDATE                               29280003
292900     IF NOT SUCCESS                                               29290003
293000        MOVE 'P2000-3' TO ERR-PARAGRAPH                           29300003
293100        MOVE MSTRNBRK  TO ERR-KEY                                 29310003
293200        PERFORM P9999-GOT-PROBLEM                                 29320003
293300     END-IF                                                       29330003
293400     MOVE P917-STATUS-CODE1         TO WS-LAYOFF-CODE-CHECK       29340003
293500     IF LOC-MISSED-CALL                                           29350003
293600        IF MSTR-CONS-MISSED-CALLS  NOT NUMERIC                    29360003
293700           MOVE ZEROS               TO MSTR-CONS-MISSED-CALLS     29370003
293800        END-IF                                                    29380003
293900        ADD 1                       TO MSTR-CONS-MISSED-CALLS-NUM 29390003
294000     ELSE                                                         29400003
294100        IF NOT LOC-AVAILABLE                                      29410003
294200           MOVE ZEROS               TO MSTR-CONS-MISSED-CALLS     29420003
294300        END-IF                                                    29430003
294400     END-IF                                                       29440003
294500*                                                                 29450003
294600     MOVE LAYOFF-CODE                  TO WS-BEFORE-STATUS-2      29460003
294700     MOVE LAYOFF-CODE-1                TO WS-BEFORE-STATUS        29470003
294710     MOVE LAYOFF-EM-CODE               TO WS-BEFORE-ECC           29471003
294711     MOVE P917-STATUS-CODE             TO WS-AFTER-STATUS-2       29471103
294712     MOVE P917-STATUS-CODE1            TO WS-AFTER-STATUS         29471203
294713     MOVE P917-EDB-CODE                TO WS-AFTER-ECC            29471303
294714     MOVE SPACES                       TO P943-COMMAREA-PARMS     29471403
294715     MOVE LAYOFF-CODE-1                TO WS-SAVE-LO              29471503
294716     MOVE LAYOFF-EM-CODE               TO WS-SAVE-EDB             29471603
294717     MOVE LAYOFF-CODE                  TO P943-LO-CODE            29471703
294718     MOVE P917-STATUS-CODE1            TO LAYOFF-CODE-1           29471803
294719     SET NOT-NEW-TO-PLACE               TO TRUE                   29471903
294720     IF P917-STATUS-CODE1               = 'A'                     29472003
294730        IF (     NORMAL-ASGNMT     NOT  > SPACE                   29473003
294740             AND TEMPORARY-ASGNMT  NOT  > SPACE                   29474003
294750             AND ON-DUTY-ASGNMT    NOT  > SPACE)                  29475003
294760        OR NOT-NOTIFIED                                           29476003
294770           SET NEW-TO-PLACE             TO TRUE                   29477003
294780           SET TO-PLACE                 TO TRUE                   29478003
294790           IF NOT WS-STAY-UNAVAILABLE                             29479003
294800              MOVE WS-EFF-DATE-TIME     TO LAYOFF-TIME            29480003
294900           END-IF                                                 29490003
295000        END-IF                                                    29500003
295100        IF  WS-BEFORE-STATUS             > 'C'                    29510003
295200        AND UNAVAILABLE-LAST-FLAG        = 'C'                    29520003
295300           SET NEW-TO-PLACE             TO TRUE                   29530003
295400           SET TO-PLACE                 TO TRUE                   29540003
295500           MOVE SPACES                  TO UNAVAILABLE-LAST-FLAG  29550003
295600           MOVE WS-EFF-DATE-TIME        TO LAYOFF-TIME            29560003
295700        END-IF                                                    29570003
295800        IF WS-BEFORE-STATUS = 'C' AND                             29580003
295900           LAYOFF-CODE-1 = 'A' AND                                29590003
296000           UNAVAILABLE-LAST-FLAG NOT = 'C'                        29600003
296100           SET P943-FUN12-END-NTFY-HOLD TO TRUE                   29610003
296200        END-IF                                                    29620003
296300*-----------------------------------------------------------------29630003
296400*       IF ADDITIONAL REST WAS CLAIMED, RECORD IT.                29640003
296500*--------------------------------------------------------------PLS29650003
296600        IF P917-REST > SPACES                                     29660003
296700           MOVE P917-REST               TO P943-FUN12-ADD-REST    29670003
296800        END-IF                                                    29680003
296900        IF P917-VRU-CONF-NUMBER > SPACES                          29690003
297000           MOVE P917-VRU-CONF-NUMBER TO P943-FUN12-VRU-NUMBER     29700003
297100        END-IF                                                    29710003
297200*-----------------------------------------------------------------29720003
297300*       MKUP-FROM-TRK IS SET WHEN A TRACKED EMPLOYEE IS BEING     29730003
297400*       MARKED UP BECAUSE THE TURN HE DIDN'T WORK IS TYING UP AT  29740003
297500*       OR DEADHEADING HOME, OR THE JOB HE DIDN'T WORK WAS        29750003
297600*       CANCELLED.  IF HE IS PENDING WAITING TURN WHEN THIS OCCURS29760003
297700*       WE SHOULD LEAVE HIM IN HIS OLD STATUS UNLESS THE OLD      29770003
297800*       STATUS WAS MISSED CALL. OTHERWISE, IF HE IS MARKING UP    29780003
297900*       AND IS ALSO PENDING WAITING TURN, HE SHOULD BE PLACED IN  29790003
298000*       WAITING TURN STATUS.  IN ANY CASE, CLEAR THE PEND WAITING 29800003
298100*       TURN FLAG.                                                29810003
298200*-----------------------------------------------------------------29820003
298300        IF MSTR-PENDED-WAIT-TURN                                  29830003
298400           OR WS-STAY-UNAVAILABLE                                 29840003
298500           MOVE SPACES                  TO MSTR-WAIT-TURN-FLAG    29850003
298600           IF P917-MKUP-FROM-TRK                                  29860003
298700              IF WS-BEFORE-STATUS        NOT = 'X'                29870003
298800                 MOVE WS-BEFORE-STATUS-2 TO LAYOFF-CODE           29880003
298900                 MOVE WS-BEFORE-ECC     TO LAYOFF-EM-CODE         29890003
299000                 IF WS-BEFORE-STATUS     NOT = 'C'                29900003
299100                    SET NOT-NEW-TO-PLACE TO TRUE                  29910003
299200                 END-IF                                           29920003
299300              END-IF                                              29930003
299400           ELSE                                                   29940003
299500              SET WAIT-TURN             TO TRUE                   29950003
299600           END-IF                                                 29960003
299700        END-IF                                                    29970003
299800        IF (NORMAL-ASGNMT                > SPACE) AND             29980003
299900           (TEMPORARY-ASGNMT        NOT  > SPACE) AND             29990003
300000           (AVAILABLE)                                            30000003
300100           MOVE SPACES                  TO EMP-FTV-DATE           30010003
300200        END-IF                                                    30020003
300300        SET P943-MARKUP-FUN             TO TRUE                   30030003
300400        IF NOT WS-STAY-UNAVAILABLE                                30040003
300500           MOVE WS-EFF-DATE-TIME        TO MARKUP-TIME            30050003
300600        END-IF                                                    30060003
300700        IF NOT NOT-NOTIFIED                                       30070003
300800           MOVE '0'                     TO LAYOFF-CODE-2          30080003
300900        END-IF                                                    30090003
301000        MOVE WS-BEFORE-ECC             TO P943-FUN12-PREV-ECC-CODE30100003
301100        MOVE SPACES TO EMP-HI-LVL-RESTRICTION                     30110003
301200     ELSE                                                         30120003
301300        IF P917-STATUS-CODE1 = '*'                                30130003
301400        OR (LOC-MISSED-CALL AND NOT MC-LAYOFF)                    30140003
301500           MOVE WS-SAVE-LO         TO LAYOFF-CODE-1               30150003
301600           IF P917-STATUS-CODE1 = '*'                             30160003
301700              SET EMP-HI-LVL-OK        TO TRUE                    30170003
301800           ELSE                                                   30180003
301900              SET P943-REJECT-EMPLOYEE-FUN  TO TRUE               30190003
302000              MOVE P917-TRAIN          TO MSTR-LVR-TRAIN          30200003
302100              MOVE P917-CALL-DATE-TIME TO MSTR-LVR-DATE-TIME      30210003
302200              MOVE P917-TRAIN-CC       TO MSTR-LVR-CRAFT          30220003
302300              MOVE P917-MISSED-CALL-FUNC                          30230003
302400                                       TO MSTR-LVR-FUNC           30240003
302500           END-IF                                                 30250003
302600        ELSE                                                      30260003
302700           SET P943-LAYOFF-FUN          TO TRUE                   30270003
302800           IF WS-BEFORE-STATUS = '#'                              30280003
302900              SET MSTR-PENDED-WAIT-TURN TO TRUE                   30290003
303000           END-IF                                                 30300003
303100           MOVE WS-SAVE-LO         TO P943-PREV-LO-CODE           30310003
303200           MOVE WS-BEFORE-ECC      TO P943-PREV-ECC-CODE          30320003
303300           MOVE WS-EFF-DATE-TIME   TO LAYOFF-TIME                 30330003
303400           MOVE P917-STATUS-CODE1  TO P943-LO-CODE-1              30340003
303500           IF LAYOFF-CODE-2 NOT = '1'                             30350003
303600              IF P917-ON-CALL                                     30360003
303700                 SET ON-CALL       TO TRUE                        30370003
303800              END-IF                                              30380003
303900           END-IF                                                 30390003
304000           MOVE LAYOFF-CODE-2      TO P943-LO-CODE-2              30400003
304100           IF P917-VRU-CONF-NUMBER > SPACES                       30410003
304200              MOVE P917-VRU-CONF-NUMBER                           30420003
304300                                   TO P943-FUN11-VRU-NUMBER       30430003
304400           END-IF                                                 30440003
304500        END-IF                                                    30450003
304600     END-IF                                                       30460003
304700*-----------------------------------------------------------------30470003
304800*      ONLY SEND EM DATA IF PREVIOUS LAYOFF HAD AN EM CODE        30480003
304900*        OR PRESENT LAYOFF HAS AN EM CODE                         30490003
305000*        AND SET LAYOFF-EM-CODE IN MASTER FILE                    30500003
305100*-----------------------------------------------------------------30510003
305110     IF P917-EDB-CODE > SPACES OR LAYOFF-EM-CODE > SPACES         30511003
305120        SET SEND-EM-DATA   TO TRUE                                30512003
305130     END-IF                                                       30513003
305140     IF P917-EDB-CODE > SPACES                                    30514003
305150        AND P917-STATUS-CODE1 NOT = 'A'                           30515003
305160        MOVE P917-EDB-CODE TO LAYOFF-EM-CODE                      30516003
305170     ELSE                                                         30517003
305180        IF P917-MKUP-FROM-TRK AND                                 30518003
305190           P917-MARKUP                                            30519003
305200           CONTINUE                                               30520003
305300        ELSE                                                      30530003
305400           MOVE SPACES     TO LAYOFF-EM-CODE                      30540003
305500        END-IF                                                    30550003
305600     END-IF                                                       30560003
305700*-----------------------------------------------------------------30570003
305800*    IF REST REQUESTED, SET PERSONAL REST DATE/TIME ON MASTER     30580003
305900*--------------------------------------------------------------PLS30590003
306000*                                                                 30600003
306100     IF P917-REST > SPACES                                        30610003
306200        MOVE ZEROES                TO DATE-CONVERSION-PARMS       30620003
306300        MOVE WS-EFF-DATE           TO PARM-PRI-DATE-GREG          30630003
306400        MOVE WS-EFF-TIME           TO PARM-PRI-HRMN               30640003
306500        MOVE P917-REST             TO PARM-SEC-HR-MN              30650003
306600        SET PARM-ADD               TO TRUE                        30660003
306700        EXEC CICS LINK                                            30670003
306800                  PROGRAM(P903-PGM)                               30680003
306900                  COMMAREA(DATE-CONVERSION-PARMS)                 30690003
307000                  LENGTH(P903-LGTH)                               30700003
307100                  RESP(WS-RESPONSE)                               30710003
307200        END-EXEC                                                  30720003
307300        MOVE WS-RESPONSE      TO FILE-STATUS                      30730003
307400        IF NOT SUCCESS                                            30740003
307500           MOVE 'P2000-4'    TO ERR-PARAGRAPH                     30750003
307600           MOVE 'P903LINK'   TO ERR-KEY                           30760003
307700           PERFORM P9999-GOT-PROBLEM                              30770003
307800        END-IF                                                    30780003
307900        MOVE PARM-RES-DATE-GREG     TO EMP-PERS-REST-DATE         30790003
308000        MOVE PARM-RES-HRMN          TO EMP-PERS-REST-TIME         30800003
308100     END-IF                                                       30810003
308200*                                                                 30820003
308300*    SEE IF THE CHANGE OF STATUS ACTIVATES KILLER-B STATUS        30830003
308400*                                                                 30840003
308500     IF  P917-STATUS-CODE1 NOT = 'A'                              30850003
308600        IF P956-ST-RSN-SPAWN-KILLER-B                             30860003
308700           SET EMP-KILLER-B         TO TRUE                       30870003
308800        END-IF                                                    30880003
308900     END-IF                                                       30890003
309000*                                                                 30900003
309100     PERFORM P8210-REWRITE-MSTR                                   30910003
309200     IF NOT SUCCESS                                               30920003
309300        MOVE 'P2000-5'  TO ERR-PARAGRAPH                          30930003
309400        MOVE MSTRNBRK   TO ERR-KEY                                30940003
309500        PERFORM P9999-GOT-PROBLEM                                 30950003
309600     END-IF                                                       30960003
309700*                                                                 30970003
309800     IF EMP-FROM-EM AND SEND-EM-DATA                              30980003
309900        PERFORM P8990-SEND-TO-EM                                  30990003
310000     END-IF                                                       31000003
310100*                                                                 31010003
310200*    IF NEEDED UPDATE QUALIFICATION FILE AND/OR EMP MASTER        31020003
310300*    DOS STARTED TIME                                             31030003
310400*                                                                 31040003
310500     MOVE SPACES                TO P963-COMMAREA-PARMS            31050003
310600     MOVE P917-EMP-NO           TO P963-EMP-NO                    31060003
310700     MOVE WS-SAVE-LO            TO P963-FROM-STATUS-CODE          31070003
310800     MOVE P917-STATUS-CODE1     TO P963-TO-STATUS-CODE            31080003
310900     MOVE WS-EFF-DATE-TIME      TO P963-EFF-DATE-TIME             31090003
311000     SET  P963-NOT-US-DOMICILED TO TRUE                           31100003
311100     SET  P963-DOS-REQUIRED     TO TRUE                           31110003
311200     EXEC CICS LINK                                               31120003
311300               PROGRAM(P963-PGM)                                  31130003
311400               COMMAREA(P963-COMMAREA-PARMS)                      31140003
311500               LENGTH(P963-LGTH)                                  31150003
311600               RESP(WS-RESPONSE)                                  31160003
311700     END-EXEC                                                     31170003
311800     MOVE WS-RESPONSE TO FILE-STATUS                              31180003
311900     IF NOT SUCCESS                                               31190003
312000        MOVE 'P2000-6' TO ERR-PARAGRAPH                           31200003
312100        PERFORM P9999-GOT-PROBLEM                                 31210003
312200     END-IF                                                       31220003
312300*                                                                 31230003
312400*    DON'T DO ANYTHING ELSE IF STATUS CODE = "*"                  31240003
312500*                                                                 31250003
312600     IF P917-STATUS-CODE1 = '*'                                   31260003
312700*            'EMPLOYEE PLACED IN LOCK STATUS'                     31270003
312800        MOVE 'E004' TO MSGLOG-CODE                                31280003
312900        PERFORM P9000-RETURN                                      31290003
313000     END-IF                                                       31300003
313100                                                                  31310003
313200                                                                  31320003
313300*                                                                 31330003
313400*    UPDATE MSTR2 FILE RECORD.                                    31340003
313500*                                                                 31350003
313600     MOVE P917-EMP-NO TO MSTR2NBRK                                31360003
313700     PERFORM P8300-READ-MSTR2-UPDATE                              31370003
313800     IF NOT SUCCESS                                               31380003
313900        MOVE 'P2000-7' TO ERR-PARAGRAPH                           31390003
314000        MOVE MSTR2NBRK TO ERR-KEY                                 31400003
314100        PERFORM P9999-GOT-PROBLEM                                 31410003
314200     END-IF                                                       31420003
314300*                                                                 31430003
314400*    UPDATE YTD "OFF" COUNTERS IN MASTER RECORD 2                 31440003
314500*                                                                 31450003
314600     IF MSTR2-YTD-INFO NOT NUMERIC                                31460003
314700        MOVE ZEROS              TO MSTR2-YTD-INFO                 31470003
314800     END-IF                                                       31480003
314900     IF P917-STATUS-CODE1 = 'A'                                   31490003
315000        AND ((WS-SAVE-LO = ('S' OR 'P'))                          31500003
315100             OR WS-LAST-COUNT-PERSONAL                            31510003
315200             OR WS-LAST-COUNT-SICK                                31520003
315300             OR WS-LAST-COUNT-BOTH)                               31530003
315400*                                                                 31540003
315500*       CALCULATE DURATION OF LAYOFF                              31550003
315600*                                                                 31560003
315700        IF LAYOFF-TIME IS NUMERIC                                 31570003
315800           AND LAYOFF-TIME > '0000000000'                         31580003
315900           MOVE ZEROS              TO DATE-CONVERSION-PARMS       31590003
316000           SET  PARM-DIFF          TO TRUE                        31600003
316100           MOVE WS-EFF-DATE        TO PARM-PRI-DATE-GREG          31610003
316200           MOVE WS-EFF-TIME        TO PARM-PRI-HRMN               31620003
316300           MOVE LAYOFF-DATE        TO PARM-SEC-DATE-GREG          31630003
316400           MOVE LAYOFF-TIME-OF-DAY TO PARM-SEC-HRMN               31640003
316500           EXEC CICS LINK                                         31650003
316600                     PROGRAM(P903-PGM)                            31660003
316700                     COMMAREA(DATE-CONVERSION-PARMS)              31670003
316800                     LENGTH(P903-LGTH)                            31680003
316900                     RESP(WS-RESPONSE)                            31690003
317000           END-EXEC                                               31700003
317100           MOVE WS-RESPONSE TO FILE-STATUS                        31710003
317200           IF NOT SUCCESS                                         31720003
317300              MOVE 'P2000-8' TO ERR-PARAGRAPH                     31730003
317400              PERFORM P9999-GOT-PROBLEM                           31740003
317500           END-IF                                                 31750003
317600           IF PARM-RES-TOT-DAYS = ZERO                            31760003
317700              ADD 1          TO PARM-RES-TOT-DAYS                 31770003
317800           ELSE                                                   31780003
317900              IF PARM-RES-HRMN > 1159                             31790003
318000                 ADD 1          TO PARM-RES-TOT-DAYS              31800003
318100              END-IF                                              31810003
318200           END-IF                                                 31820003
318300        ELSE                                                      31830003
318400           MOVE 1               TO PARM-RES-TOT-DAYS              31840003
318500        END-IF                                                    31850003
318600        IF WS-SAVE-LO = 'S'                                       31860003
318700           OR WS-LAST-COUNT-SICK                                  31870003
318800           OR WS-LAST-COUNT-BOTH                                  31880003
318900           ADD PARM-RES-TOT-DAYS TO MSTR2-SICK-COUNT              31890003
319000        END-IF                                                    31900003
319100        IF WS-LAST-COUNT-PERSONAL                                 31910003
319200           OR WS-LAST-COUNT-BOTH                                  31920003
319300           ADD PARM-RES-TOT-DAYS TO MSTR2-PERSONAL-COUNT          31930003
319400        END-IF                                                    31940003
319500     END-IF                                                       31950003
319600     IF SICK AND ON-CALL                                          31960003
319700        ADD 1                TO MSTR2-ON-CALL-COUNT               31970003
319800     END-IF                                                       31980003
319900     IF LOC-MISSED-CALL                                           31990003
320000        ADD 1                TO MSTR2-MISSED-CALL-COUNT           32000003
320100     END-IF                                                       32010003
320200*                                                                 32020003
320300*    ACTIVATE/DEACTIVATE EMPLOYEE'S BIDS, IF NEEDED.              32030003
320400*                                                                 32040003
320500     IF SAVE-DEACT-PERM                                           32050003
320600     OR SAVE-DEACT-WEEKLY                                         32060003
320700*                                                                 32070003
320800*       PROCESS TEMP BID CARDS FIRST                              32080003
320900*                                                                 32090003
321000        INITIALIZE WS-EBC-STORAGE                                 32100003
321100        MOVE P917-EMP-NO                TO EBC-EMP-NBR            32110003
321200        MOVE C-MAX-TS                   TO EBC-CLS-TS             32120003
321300        MOVE 'T'                        TO EBC-PRM-TMP-CD         32130003
321400        PERFORM PSQL-EBCOPN02                                     32140003
321500        SET EBC-NOT-DONE                TO TRUE                   32150003
321600        PERFORM UNTIL EBC-DONE                                    32160003
321700           PERFORM PSQL-EBCFET02                                  32170003
321800           IF SQL-SUCCESS                                         32180003
321900              SET DE-YYMMDD-FORMAT      TO TRUE                   32190003
322000              SET DE-REFORMAT-ONLY      TO TRUE                   32200003
322100              MOVE EBC-CLS-TS-DT        TO DE-YYMMDD              32210003
322200              PERFORM P8998-DATEEDIT                              32220003
322300              MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE       32230003
322400              MOVE EBC-CLS-TS-TM        TO DE-COMPARE1-TIME       32240003
322500                                                                  32250003
322600              IF DE-COMPARE1-DATE-TIME >= WS-SYSTEM-DATE-TIME-CENT32260003
322700                 AND EBC-STATUS NOT = 'D'                         32270003
322800                 MOVE 'D'               TO EBC-STATUS             32280003
322900                 PERFORM PSQL-EBCINS                              32290003
323000                 MOVE P917-STATUS-CODE  TO MSTR2-BID-DEACT        32300003
323100              ELSE                                                32310003
323200                 SET EBC-DONE           TO TRUE                   32320003
323300              END-IF                                              32330003
323400           ELSE                                                   32340003
323500              SET EBC-DONE              TO TRUE                   32350003
323600           END-IF                                                 32360003
323700        END-PERFORM                                               32370003
323800        PERFORM PSQL-EBCCLO02                                     32380003
323900*                                                                 32390003
324000*       PROCESS PERM BID CARDS, WHEN APPLICABLE                   32400003
324100*                                                                 32410003
324200        IF SAVE-DEACT-PERM                                        32420003
324300           INITIALIZE WS-EBC-STORAGE                              32430003
324400           MOVE P917-EMP-NO             TO EBC-EMP-NBR            32440003
324500           MOVE C-MAX-TS                TO EBC-CLS-TS             32450003
324600           MOVE 'P'                     TO EBC-PRM-TMP-CD         32460003
324700           PERFORM PSQL-EBCOPN02                                  32470003
324800           SET EBC-NOT-DONE             TO TRUE                   32480003
324900           PERFORM UNTIL EBC-DONE                                 32490003
325000              PERFORM PSQL-EBCFET02                               32500003
325100              IF SQL-SUCCESS                                      32510003
325200                 SET DE-YYMMDD-FORMAT   TO TRUE                   32520003
325300                 MOVE EBC-CLS-TS-DT     TO DE-YYMMDD              32530003
325400                 PERFORM P8998-DATEEDIT                           32540003
325500                 MOVE DE-CCYYMMDD       TO DE-COMPARE1-DATE       32550003
325600                 MOVE EBC-CLS-TS-TM     TO DE-COMPARE1-TIME       32560003
325700                                                                  32570003
325800                 IF DE-COMPARE1-DATE-TIME                         32580003
325900                                     >= WS-SYSTEM-DATE-TIME-CENT  32590003
326000                    AND EBC-STATUS NOT = 'D'                      32600003
326100                    MOVE 'D'            TO EBC-STATUS             32610003
326110                    PERFORM PSQL-EBCINS                           32611003
326120                    MOVE P917-STATUS-CODE TO MSTR2-BID-DEACT      32612003
326130                 ELSE                                             32613003
326140                    SET EBC-DONE        TO TRUE                   32614003
326150                 END-IF                                           32615003
326160              ELSE                                                32616003
326170                 SET EBC-DONE           TO TRUE                   32617003
326180              END-IF                                              32618003
326190           END-PERFORM                                            32619003
326200           PERFORM PSQL-EBCCLO02                                  32620003
326210        END-IF                                                    32621003
326211     END-IF                                                       32621103
326212*                                                                 32621203
326213     PERFORM P8310-REWRITE-MSTR2                                  32621303
326214     IF NOT SUCCESS                                               32621403
326215        MOVE 'P8310-8'  TO ERR-PARAGRAPH                          32621503
326216        MOVE MSTR2NBRK  TO ERR-KEY                                32621603
326217        PERFORM P9999-GOT-PROBLEM                                 32621703
326218     END-IF.                                                      32621803
326219*                                                                 32621903
326220*   CHECK IF STAFF FORM E-MAIL NEEDS TO BE SENT - IF MARKING UP,  32622003
326221*   CHECK STATUS/REASON RECORD OF THE STATUS THE EMPLOYEE IS      32622103
326222*   MARKING UP FROM TO DETERMINE IF STAFF FORM E-MAIL NEEDS TO BE 32622203
326223*   SENT.                                                         32622303
326224*                                                                 32622403
326225     IF P917-STATUS-CODE1 NOT = '*'                               32622503
326226        MOVE SPACES                 TO WS-SYSTEM-NAME             32622603
326227        EXEC CICS ASSIGN                                          32622703
326228                  APPLID (WS-SYSTEM-NAME)                         32622803
326229        END-EXEC                                                  32622903
326230        IF P917-STATUS-CODE1 = 'A'                                32623003
326240           MOVE P956-COMMAREA-PARMS TO SAVE-P956-COMMAREA         32624003
326241           MOVE SPACES              TO P956-COMMAREA-PARMS        32624103
326242           SET P956-GET-CNTL-STATUS-REASON                        32624203
326243                                    TO TRUE                       32624303
326244           MOVE WS-BEFORE-STATUS-2  TO P956-STATUS-CODE           32624403
326245           MOVE WS-BEFORE-ECC       TO P956-REASON-CODE           32624503
326246           MOVE DIST OF WS-MSTR     TO P956-DIST                  32624603
326247           MOVE SUB-DIST OF WS-MSTR TO P956-SDIST                 32624703
326248           MOVE CRAFT OF WS-MSTR    TO P956-CC                    32624803
326249           IF TEMPORARY-ASGNMT > SPACE                            32624903
326250              MOVE TEMPORARY-ASGNMT-FLAG TO P956-ASGN-TYPE        32625003
326251              MOVE TA-1             TO P956-ASGN                  32625103
326252              MOVE TA-DIST          TO P956-DIST                  32625203
326253              MOVE TA-SUB-DIST      TO P956-SDIST                 32625303
326254              IF TEMP-ASGN-XB                                     32625403
326255                 MOVE TA-CC         TO P956-XB                    32625503
326256              END-IF                                              32625603
326257           ELSE                                                   32625703
326258              IF NORMAL-ASGNMT > SPACES                           32625803
326259                 MOVE NORMAL-ASGNMT-FLAG TO P956-ASGN-TYPE        32625903
326260                 MOVE NA-1          TO P956-ASGN                  32626003
326261                 MOVE NA-DIST       TO P956-DIST                  32626103
326262                 MOVE NA-SUB-DIST   TO P956-SDIST                 32626203
326263                 IF NORM-ASGN-XB                                  32626303
326264                    MOVE NA-CC      TO P956-XB                    32626403
326265                 END-IF                                           32626503
326266              END-IF                                              32626603
326267           END-IF                                                 32626703
326268           PERFORM P9840-RETRIEVE-CNTL-INFO                       32626803
326269           IF P956-ERROR-FOUND                                    32626903
326270              MOVE 'P2000-9'        TO ERR-PARAGRAPH              32627003
326271              STRING 'P956 LINK...ERROR-FLAG='                    32627103
326272                     P956-ERROR-FLAG                              32627203
326273                     DELIMITED BY SIZE INTO ERR-SENTENCE          32627303
326274              MOVE P956-INPUT-PARMS TO ERR-KEY                    32627403
326275              PERFORM P9999-GOT-PROBLEM                           32627503
326276           END-IF                                                 32627603
326277           IF P956-ST-RSN-RTE-STAFF-FORM                          32627703
326278              IF  NOT WS-CICS1                                    32627803
326279              AND NOT WS-CATSUSR                                  32627903
326280                 MOVE SAVE-P956-COMMAREA TO P956-COMMAREA-PARMS   32628003
326290                 SET WS-STAFF-FORM-YES TO TRUE                    32629003
326300                 PERFORM P3000-SEND-STAFF-FORM                    32630003
326400              END-IF                                              32640003
326500           ELSE                                                   32650003
326600              MOVE SAVE-P956-COMMAREA TO P956-COMMAREA-PARMS      32660003
326700           END-IF                                                 32670003
326800        ELSE                                                      32680003
326900           IF P956-ST-RSN-RTE-STAFF-FORM                          32690003
327000              IF  NOT WS-CICS1                                    32700003
327100              AND NOT WS-CATSUSR                                  32710003
327200                 SET WS-STAFF-FORM-YES TO TRUE                    32720003
327300                 PERFORM P3000-SEND-STAFF-FORM                    32730003
327400              END-IF                                              32740003
327500           END-IF                                                 32750003
327600        END-IF                                                    32760003
327700     END-IF                                                       32770003
327800*                                                                 32780003
327900*   IF THE EMPLOYEE IS OUT OF TOWN AND LAYING OFF DELETE HIS ON   32790003
328000*   DUTY ASSIGNMENT NOW.                                          32800003
328100*                                                                 32810003
328200     MOVE SPACES TO P938-COMMAREA-PARMS                           32820003
328300     IF OUT-TOWN                                                  32830003
328400        IF (P917-STATUS-CODE1 NOT = 'A' AND                       32840003
328500           ON-DUTY-ASGNMT > SPACE)                                32850003
328600           MOVE ASGNKEY2 TO ASGNEMP                               32860003
328700           EXEC CICS READ                                         32870003
328800                     DATASET(ASGN-VIA-ASGNEMP)                    32880003
328900                     INTO(WS-ASGN-FILE)                           32890003
329000                     LENGTH(ASGNEMP-RLGTH)                        32900003
329100                     RIDFLD(ASGNEMP)                              32910003
329200                     KEYLENGTH(ASGNEMP-KLGTH)                     32920003
329300                     RESP(WS-RESPONSE)                            32930003
329400           END-EXEC                                               32940003
329500           MOVE WS-RESPONSE TO FILE-STATUS                        32950003
329600           IF SUCCESS                                             32960003
329700              MOVE ASGNKEY1 TO ASGNJOB                            32970003
329800              EXEC CICS DELETE                                    32980003
329810                        DATASET(ASGN-VIA-ASGNJOB)                 32981003
329820                        RIDFLD(ASGNJOB)                           32982003
329830                        RESP(WS-RESPONSE)                         32983003
329840              END-EXEC                                            32984003
329850              MOVE WS-RESPONSE TO FILE-STATUS                     32985003
329860              IF NOT SUCCESS                                      32986003
329870                 MOVE 'P2000-10' TO ERR-PARAGRAPH                 32987003
329880                 MOVE ASGNJOB    TO ERR-KEY                       32988003
329890                 PERFORM P9999-GOT-PROBLEM                        32989003
329900              END-IF                                              32990003
330000           ELSE                                                   33000003
330100              MOVE 'P2000-11' TO ERR-PARAGRAPH                    33010003
330200              MOVE ASGNEMP    TO ERR-KEY                          33020003
330300              PERFORM P9999-GOT-PROBLEM                           33030003
330400           END-IF                                                 33040003
330500           SET P938-INITIATE-FUNCTION TO TRUE                     33050003
330600           MOVE ON-DUTY-ASGNMT-FLAG TO P938-ASGN-TYPE             33060003
330700           MOVE ON-DUTY-ASGNMT      TO P938-ASGN-PARM             33070003
330800           MOVE P917-EFF-DATE-TIME  TO P938-EFF-DATE-TIME         33080003
330900           MOVE P917-CALL-DATE-TIME TO P938-FUNCTION-DATE-TIME    33090003
331000           MOVE P917-TIME-ZONE      TO P938-TIME-ZONE             33100003
331100           SET P938-INIT-OFF-OUT-TOWN TO TRUE                     33110003
331200           IF LOC-MISSED-CALL                                     33120003
331300              SET TRKRSN-MISSCALL   TO TRUE                       33130003
331400           ELSE                                                   33140003
331500              SET TRKRSN-UNAVAIL    TO TRUE                       33150003
331600           END-IF                                                 33160003
331700           MOVE TRKRSN-CHECK        TO P938-INIT-REASON           33170003
331800           MOVE P917-EMP-NO         TO P938-INIT-EMP-NO           33180003
331900           EXEC CICS LINK                                         33190003
332000                     PROGRAM(P938-PGM)                            33200003
332100                     COMMAREA(P938-COMMAREA-PARMS)                33210003
332200                     LENGTH(P938-LGTH)                            33220003
332300                     RESP(WS-RESPONSE)                            33230003
332400           END-EXEC                                               33240003
332500           MOVE WS-RESPONSE        TO FILE-STATUS                 33250003
332600           IF NOT SUCCESS                                         33260003
332700              MOVE 'P2000-12'      TO ERR-PARAGRAPH               33270003
332800              MOVE 'P938LINK'      TO ERR-KEY                     33280003
332900              PERFORM P9999-GOT-PROBLEM                           33290003
333000           END-IF                                                 33300003
333100        END-IF                                                    33310003
333200     END-IF                                                       33320003
333300     IF P938-COMMAREA-PARMS NOT > SPACES                          33330003
333400        IF P917-ON-CALL                                           33340003
333500           AND P917-STATUS-CODE1 NOT = 'A'                        33350003
333600           AND NOT MC-TRACK                                       33360003
333700           AND EIBTRNID NOT = P16-TRAN                            33370003
333800           SET P938-INITIATE-FUNCTION TO TRUE                     33380003
333900           MOVE P917-OC-ASGN-TYPE    TO P938-ASGN-TYPE            33390003
334000           MOVE P917-OC-ASGN         TO P938-ASGN-PARM            33400003
334100           MOVE P917-DECKEY          TO P938-DECKEY               33410003
334200           MOVE P917-EFF-DATE-TIME   TO P938-EFF-DATE-TIME        33420003
334300           MOVE P917-CALL-DATE-TIME  TO P938-FUNCTION-DATE-TIME   33430003
334400           MOVE P917-CALL-LEAD-TIME  TO P938-CALL-LEAD-TIME       33440003
334500           MOVE P917-TIME-ZONE       TO P938-TIME-ZONE            33450003
334600           SET P938-INIT-OFF-ON-CALL TO TRUE                      33460003
334700           IF LOC-MISSED-CALL                                     33470003
334800              SET TRKRSN-MISSCALL   TO TRUE                       33480003
334900           ELSE                                                   33490003
335000              SET TRKRSN-LAYOFF-ON-CALL                           33500003
335100                                     TO TRUE                      33510003
335200           END-IF                                                 33520003
335300           MOVE TRKRSN-CHECK         TO P938-INIT-REASON          33530003
335400           MOVE P917-EMP-NO          TO P938-INIT-EMP-NO          33540003
335500           MOVE P917-TRAIN           TO P938-INIT-TRAIN           33550003
335600           EXEC CICS LINK                                         33560003
335700                     PROGRAM(P938-PGM)                            33570003
335800                     COMMAREA(P938-COMMAREA-PARMS)                33580003
335900                     LENGTH(P938-LGTH)                            33590003
336000                     RESP(WS-RESPONSE)                            33600003
336100           END-EXEC                                               33610003
336200           MOVE WS-RESPONSE          TO FILE-STATUS               33620003
336300           IF NOT SUCCESS                                         33630003
336400              MOVE 'P2000-13'        TO ERR-PARAGRAPH             33640003
336500              MOVE 'P938LINK'        TO ERR-KEY                   33650003
336600              PERFORM P9999-GOT-PROBLEM                           33660003
336700           END-IF                                                 33670003
336800        END-IF                                                    33680003
336900     END-IF                                                       33690003
337000*                                                                 33700003
337100*     WRITE EMPLOYEE HISTORY.                                     33710003
337200*                                                                 33720003
337300* 2/13/01 TJR MOVE CODE INTO ITS OWN RTN, CAUSE ITS NOW           33730003
337400*             GOING TO BE EXECUTED FROM HERE AND FROM RTN         33740003
337500*             P1000-CHECK-FOR-ERRORS                              33750003
337600                                                                  33760003
337700     IF LOC-MISSED-CALL                                           33770003
337800        PERFORM P2020-PROCESS-MISSED-CALLS                        33780003
337900     END-IF                                                       33790003
338000                                                                  33800003
338100     PERFORM P2005-WRITE-EMPLOYEE-HISTORY                         33810003
338200                                                                  33820003
338300     IF P917-STATUS-CODE1 = 'P'                                   33830003
338400        PERFORM P2600-PERSONAL-LEAVE                              33840003
338500     ELSE                                                         33850003
338600        IF P917-STATUS-CODE1 = 'L'                                33860003
338700         AND P917-EDB-CODE = 'BT'                                 33870003
338800           PERFORM P2605-BT-DAYOFF                                33880003
338900        END-IF                                                    33890003
339000     END-IF                                                       33900003
339100                                                                  33910003
339200     IF P917-STATUS-CODE1                = 'A'                    33920003
339300        IF NOT NOT-NOTIFIED                                       33930003
339400           IF AVAILABLE OF LAYOFF-CODE-1                          33940003
339500              IF (TEMP-ASGN-XB                                    33950003
339600                 OR (NORM-ASGN-XB                                 33960003
339700                    AND TEMPORARY-ASGNMT NOT > SPACE))            33970003
339800                 AND ON-DUTY-ASGNMT NOT  > SPACE                  33980003
339900                 PERFORM P2100-MARKUP-XB                          33990003
340000                 PERFORM P2500-RELEASE-EMPS                       34000003
340100              ELSE                                                34010003
340200                 IF TEMP-ASGN-UFP                                 34020003
340300                    OR (NORM-ASGN-UFP                             34030003
340400                    AND TEMPORARY-ASGNMT NOT > SPACE)             34040003
340500                    AND ON-DUTY-ASGNMT NOT > SPACE                34050003
340600                    PERFORM P2300-MARKUP-UFP                      34060003
340700                 END-IF                                           34070003
340800                 PERFORM P2500-RELEASE-EMPS                       34080003
340900              END-IF                                              34090003
341000*                                                                 34100003
341100*             IF EMPLOYEE BOOKED ON FROM VACATION BEFORE HIS LAST 34110003
341200*             SCHEDULED VACATION DAY, SEND A WARNING TO CALLER.   34120003
341300*                                                                 34130003
341400              IF MSGLOG-CODE NOT = 'B170'                         34140003
341500*               'EMPLOYEE HAS BEEN BOOKED ON'                     34150003
341600                 MOVE 'E003' TO MSGLOG-CODE                       34160003
341700              END-IF                                              34170003
341800           ELSE                                                   34180003
341900              IF TO-PLACE                                         34190003
342000*                  'EMPLOYEE TO-PLACE'                            34200003
342100                 MOVE 'E093' TO MSGLOG-CODE                       34210003
342200              ELSE                                                34220003
342300                 IF WAIT-TURN                                     34230003
342400*                     'EMPLOYEE IS WAITING TURN'                  34240003
342500                    MOVE 'E176' TO MSGLOG-CODE                    34250003
342600                 ELSE                                             34260003
342700*                                                                 34270003
342800*                   IF THE PERSON STAYED UNAVAILABLE WHEN MARKING 34280003
342900*                   UP FROM TRACKING AND OWNS A POOL TURN WHICH   34290003
343000*                   STAYS ON BOARD FOR THE CURRENT STATUS, PUT THE34300003
343100*                   TURN BACK ON BOARD (UNLESS THERE IS A MORE    34310003
343200*                   RECENT TEMPORARY OWNER).                      34320003
343300*                                                                 34330003
343400                    IF WS-STAY-UNAVAILABLE AND                    34340003
343500                       (TEMP-ASGN-UFP OR                          34350003
343600                        (NORM-ASGN-UFP AND                        34360003
343700                         TEMPORARY-ASGNMT NOT > SPACE))           34370003
343800                       AND ON-DUTY-ASGNMT NOT > SPACE             34380003
343900                       AND WS-LAST-RETAIN-POS-POOL                34390003
344000                       PERFORM P2040-UNAVL-MKUP-FROM-TRK          34400003
344100                    END-IF                                        34410003
344200                 END-IF                                           34420003
344300              END-IF                                              34430003
344400           END-IF                                                 34440003
344500        END-IF                                                    34450003
344600        PERFORM P2050-DELETE-PENDED-MARKUP                        34460003
344700     ELSE                                                         34470003
344800*       IF THE XB TURN HAS A TEMPORARY EMPLOYEE, DO NOT REMOVE    34480003
344900*       THE TURN FROM THE EXTRA BOARD                             34490003
345000        IF NORM-ASGN-XB                                           34500003
345100           AND TEMPORARY-ASGNMT <= SPACE                          34510003
345200           MOVE XXXX-ASGNKEY1     TO XXXX-ASGNKEY1-PREV           34520003
345300           MOVE SPACES            TO ASGNKEY1                     34530003
345400           SET ASGN-XB-JOB OF ASGN-JOB-TYPE TO TRUE               34540003
345500           MOVE NA-DIST           TO ASGN-DIST                    34550003
345600           MOVE NA-SUB-DIST       TO ASGN-SUB-DIST                34560003
345700           MOVE 'EX'              TO ASGN-XB-PREFIX               34570003
345800           MOVE NA-TURN           TO ASGN-XB-TURN                 34580003
345900           MOVE NA-CC             TO ASGN-XB-CC                   34590003
346000           PERFORM PXXXX-LATEST-TEMP                              34600003
346100           MOVE XXXX-ASGNKEY1-PREV TO XXXX-ASGNKEY1               34610003
346200        END-IF                                                    34620003
346300        IF TEMP-ASGN-XB                                           34630003
346400           OR (NORM-ASGN-XB                                       34640003
346500               AND TEMPORARY-ASGNMT <= SPACE                      34650003
346600               AND (ASGN-EMP-NO = ZERO                            34660003
346700                    OR ASGN-EMP-NO = EMP-NBR OF WS-MSTR))         34670003
346800           IF LOC-MISSED-CALL                                     34680003
346900              IF MC-NO-ACTION OR MC-REALIGN-XB                    34690003
347000*                                                                 34700003
347100                 PERFORM P2060-SET-XB-MC-FLAG                     34710003
347200*                                                                 34720003
347300              ELSE                                                34730003
347400                 PERFORM P2200-LAYOFF-XB                          34740003
347500              END-IF                                              34750003
347600           ELSE                                                   34760003
347700              PERFORM P2200-LAYOFF-XB                             34770003
347800           END-IF                                                 34780003
347900        END-IF                                                    34790003
348000        IF TEMP-ASGN-UFP                                          34800003
348100           OR (NORM-ASGN-UFP AND TEMPORARY-ASGNMT NOT > SPACE)    34810003
348200           IF P917-STATUS-CODE1 NOT = 'V'                         34820003
348300              PERFORM P2400-LAYOFF-UFP                            34830003
348400           END-IF                                                 34840003
348500        END-IF                                                    34850003
348600        IF (P917-EXP-DATE    > SPACES                             34860003
348700        AND P917-EXP-TIME    > SPACES)                            34870003
348800           PERFORM P3500-FUTURE-MARKUP-DUEBACK                    34880003
348900           MOVE SPACES                  TO P927-COMMAREA-PARMS    34890003
349000           SET P927-CHG-STATUS-FUNCTION TO TRUE                   34900003
349100           MOVE P917-EXP-DATE           TO P927-EFF-DATE          34910003
349200           MOVE P917-EXP-TIME           TO P927-EFF-TIME          34920003
349300           MOVE WS-TIME-ZONE            TO P927-TIME-ZONE         34930003
349400           MOVE DIST OF WS-MSTR         TO P927-DIST              34940003
349500           MOVE SUB-DIST OF WS-MSTR     TO P927-SUB-DIST          34950003
349600           MOVE EMP-NBR  OF WS-MSTR     TO P927-CS-EMP-NO         34960003
349700           MOVE 'A0'                    TO P927-CS-LO-CODE        34970003
349800           MOVE P917-EXP-DATE-TIME      TO P927-CS-EXP-DATE-TIME  34980003
349900           IF P956-ST-RSN-AUTO-MU                                 34990003
350000              MOVE SPACES               TO P927-CS-DUE-BACK-CODE  35000003
350100           ELSE                                                   35010003
350200              SET P927-CS-DUE-BACK      TO TRUE                   35020003
350300           END-IF                                                 35030003
350400           PERFORM P8800-WRITE-TASK                               35040003
350500        END-IF                                                    35050003
350600        PERFORM P2800-CHECK-RELEASE-FROM-ASGN                     35060003
350700*       MOVE 'EMPLOYEE HAS BEEN MARKED OFF' TO P917-ERRORMSG      35070003
350800        MOVE 'E001'                     TO MSGLOG-CODE            35080003
350900     END-IF                                                       35090003
351000*                                                                 35100003
351100*       PROCESS AVAILABILITY LIST                                 35110003
351200*                                                                 35120003
351300     IF NOT P917-ON-CALL                                          35130003
351400        MOVE SPACES                        TO P937-COMMAREA-PARMS 35140003
351500        MOVE P917-EMP-NO                   TO P937-AVL-EMP-NO     35150003
351600        MOVE DEC-ALT-DIST                  TO P937-AVL-DIST       35160003
351700        MOVE DEC-ALT-SUB-DIST              TO P937-AVL-SDIST      35170003
351800        MOVE DEC-POOL-XB(1)                TO P937-AVL-ID         35180003
351900        IF P917-ON-CALL                                           35190003
352000           SET P937-CONNECTED-PROCESS      TO TRUE                35200003
352100        ELSE                                                      35210003
352200        SET P937-DISCONNECTED-PROCESS   TO TRUE                   35220003
352300        END-IF                                                    35230003
352400        MOVE P917-STATUS-CODE1             TO P937-LAYOFF-CODE    35240003
352500        IF TEMPORARY-ASGNMT > SPACE                               35250003
352600           MOVE TEMPORARY-ASGNMT           TO P937-ASGN           35260003
352700           MOVE TEMPORARY-ASGNMT-FLAG      TO P937-ASGN-TYPE      35270003
352800        ELSE                                                      35280003
352900           MOVE NORMAL-ASGNMT              TO P937-ASGN           35290003
353000           MOVE NORMAL-ASGNMT-FLAG         TO P937-ASGN-TYPE      35300003
353100        END-IF                                                    35310003
353200        MOVE P917-TIME-ZONE                TO P937-TIME-ZONE      35320003
353300        MOVE SPACES                        TO TZ-PARAMETERS       35330003
353400        MOVE P917-TIME-ZONE                TO TZ-IN-ZONE          35340003
353500        MOVE P917-EFF-DATE-TIME            TO TZ-IN-DATE-TIME     35350003
353600        MOVE TZ-SYSTEM-TIME-ZONE           TO TZ-OUT-ZONE         35360003
353700        PERFORM P8996-TIMEZONE                                    35370003
353800        IF TZ-INVALID-PARAMETERS                                  35380003
353900           MOVE 'P2000-14'                 TO ERR-PARAGRAPH       35390003
354000           PERFORM P8996-TZERROR                                  35400003
354100        END-IF                                                    35410003
354200        MOVE TZ-OUT-DATE-TIME              TO P937-CALL-DATE-TIME 35420003
354300        MOVE SPACES                        TO TZ-PARAMETERS       35430003
354400        MOVE P917-TIME-ZONE                TO TZ-IN-ZONE          35440003
354500        MOVE WS-EFF-DATE-TIME              TO TZ-IN-DATE-TIME     35450003
354600        MOVE TZ-SYSTEM-TIME-ZONE           TO TZ-OUT-ZONE         35460003
354700        PERFORM P8996-TIMEZONE                                    35470003
354800        IF TZ-INVALID-PARAMETERS                                  35480003
354900           MOVE 'P2000-15'                 TO ERR-PARAGRAPH       35490003
355000           PERFORM P8996-TZERROR                                  35500003
355100        END-IF                                                    35510003
355200        MOVE TZ-OUT-DATE-TIME-CENT      TO P937-EFF-DATE-TIME-CENT35520003
355300        IF P917-MISSED-CALL-FUNC > SPACES                         35530003
355400           MOVE P917-MISSED-CALL-FUNC      TO P937-FUNCTION-PARM  35540003
355500        ELSE                                                      35550003
355600           SET P937-LAYOFF-FUNCTION        TO TRUE                35560003
355700        END-IF                                                    35570003
355800        MOVE P917-DECKEY                   TO P937-DECISION-KEY   35580003
355900        PERFORM P8600-LINK-TO-P937                                35590003
356000     END-IF                                                       35600003
356100*                                                                 35610003
356200     IF NEW-TO-PLACE                                              35620003
356300        MOVE SPACES                     TO P937-COMMAREA-PARMS    35630003
356400        MOVE P917-EMP-NO                TO P937-AVL-EMP-NO        35640003
356500        SET P937-DISCONNECTED-PROCESS   TO TRUE                   35650003
356600        MOVE P917-STATUS-CODE1          TO P937-LAYOFF-CODE       35660003
356700        MOVE P917-TIME-ZONE             TO P937-TIME-ZONE         35670003
356800                                           TZ-IN-ZONE             35680003
356900        MOVE P917-EFF-DATE-TIME         TO TZ-IN-DATE-TIME        35690003
357000        MOVE TZ-SYSTEM-TIME-ZONE        TO TZ-OUT-ZONE            35700003
357100        PERFORM P8996-TIMEZONE                                    35710003
357200        IF TZ-INVALID-PARAMETERS                                  35720003
357300           MOVE 'P2000-16'              TO ERR-PARAGRAPH          35730003
357400           PERFORM P8996-TZERROR                                  35740003
357500        END-IF                                                    35750003
357600        MOVE TZ-OUT-DATE-TIME           TO P937-CALL-DATE-TIME    35760003
357700        MOVE WS-SYSTEM-DATE-TIME-CENT   TO P937-EFF-DATE-TIME-CENT35770003
357800        SET P937-TOPLACE                TO TRUE                   35780003
357900        PERFORM P8600-LINK-TO-P937                                35790003
358000     END-IF                                                       35800003
358100     IF P917-STATUS-CODE1 = 'A'                                   35810003
358200        PERFORM P2070-DELETE-VIRTUAL-STARTS                       35820003
358300     END-IF                                                       35830003
358400     .                                                            35840003
358500*                                                                 35850003
358600 P2005-WRITE-EMPLOYEE-HISTORY.                                    35860003
358700                                                                  35870003
358800     MOVE P917-EMP-NO         TO P943-EMP-NBR                     35880003
358900     MOVE WS-EFF-DATE-TIME    TO P943-EFF-DATE-TIME               35890003
359000     MOVE WS-TIME-ZONE        TO P943-EMP-TIME-ZONE               35900003
359100     MOVE DIST     OF WS-MSTR TO P943-DIST                        35910003
359200     MOVE SUB-DIST OF WS-MSTR TO P943-SDIST                       35920003
359300     MOVE CRAFT    OF WS-MSTR TO P943-CRAFT                       35930003
359400     IF P917-ON-CALL                                              35940003
359401        MOVE P917-TRAIN-AREA  TO P943-TEMP-ASGN-TRAIN             35940103
359402        IF TEMPORARY-ASGNMT > SPACE                               35940203
359403           MOVE TEMPORARY-ASGNMT TO P943-NORM-ASGN                35940303
359404           IF TEMP-ASGN-UFP                                       35940403
359405              MOVE TA-POOL       TO P943-POOL-ASG                 35940503
359406           END-IF                                                 35940603
359407        ELSE                                                      35940703
359408           MOVE NORMAL-ASGNMT    TO P943-NORM-ASGN                35940803
359409           IF NORM-ASGN-UFP                                       35940903
359410              MOVE NA-POOL       TO P943-POOL-ASG                 35941003
359420           END-IF                                                 35942003
359430        END-IF                                                    35943003
359440     ELSE                                                         35944003
359450        IF TEMPORARY-ASGNMT > SPACE                               35945003
359460           MOVE TEMPORARY-ASGNMT TO P943-NORM-ASGN                35946003
359470           IF TEMP-ASGN-UFP                                       35947003
359480              MOVE TA-POOL       TO P943-POOL-ASG                 35948003
359490           END-IF                                                 35949003
359500        ELSE                                                      35950003
359600           MOVE NORMAL-ASGNMT    TO P943-NORM-ASGN                35960003
359700           IF NORM-ASGN-UFP                                       35970003
359800              MOVE NA-POOL       TO P943-POOL-ASG                 35980003
359900           END-IF                                                 35990003
360000        END-IF                                                    36000003
360100     END-IF                                                       36010003
360200                                                                  36020003
360300     IF NOT P943-MARKUP-FUN           AND                         36030003
360400        NOT P943-PENDING-MARKUP-FUN                               36040003
360500        IF LOC-MISSED-CALL                                        36050003
360600           IF WS-MC-LO-DATE-TIME > SPACES                         36060003
360700              MOVE WS-MC-LO-DATE-TIME    TO P943-EFF-DATE-TIME    36070003
360800           END-IF                                                 36080003
360900           IF P943-REJECT-EMPLOYEE-FUN                            36090003
361000              MOVE P917-MISSED-CALL-FUNC TO P943-FUN16-TYPE       36100003
361100              MOVE P917-CALL-DATE-TIME   TO                       36110003
361200                                         P943-FUN16-CALL-DATE-TIME36120003
361300              IF P917-DECKEY > SPACES                             36130003
361400                 MOVE P917-DEC-RULE-NO   TO P943-FUN16-RULE-NO    36140003
361500              END-IF                                              36150003
361600           ELSE                                                   36160003
361700                                                                  36170003
361800              IF  P943-LAYOFF-FUN                                 36180003
361900                  MOVE P917-MISSED-CALL-FUNC                      36190003
362000                    TO P943-MISSED-CALL-FUNC                      36200003
362100                  MOVE P917-CALL-DATE-TIME                        36210003
362200                    TO P943-FUN11-CALL-DATE-TIME                  36220003
362300              ELSE                                                36230003
362400                  IF  P943-PENDING-LAYOFF-FUN                     36240003
362500                      MOVE P917-MISSED-CALL-FUNC                  36250003
362600                        TO P943-FUN36-MISSED-CALL-FUNC            36260003
362700                      MOVE P917-CALL-DATE-TIME                    36270003
362800                        TO P943-FUN36-CALL-DATE-TIME              36280003
362900                  END-IF                                          36290003
363000              END-IF                                              36300003
363100                                                                  36310003
363200              IF P917-DECKEY > SPACES                             36320003
363300                 IF  P943-LAYOFF-FUN                              36330003
363400                     MOVE P917-DEC-RULE-NO TO P943-FUN11-RULE-NO  36340003
363500                 ELSE                                             36350003
363600                     IF  P943-PENDING-LAYOFF-FUN                  36360003
363700                         MOVE P917-DEC-RULE-NO                    36370003
363800                           TO P943-FUN36-RULE-NO                  36380003
363900                     END-IF                                       36390003
364000                 END-IF                                           36400003
364100              END-IF                                              36410003
364200           END-IF                                                 36420003
364300        ELSE                                                      36430003
364400           IF P917-DECKEY > SPACES                                36440003
364500              IF  P943-LAYOFF-FUN                                 36450003
364600                  MOVE P917-DEC-RULE-NO TO P943-FUN11-RULE-NO     36460003
364700              ELSE                                                36470003
364800                  IF  P943-PENDING-LAYOFF-FUN                     36480003
364900                      MOVE P917-DEC-RULE-NO                       36490003
365000                        TO P943-FUN36-RULE-NO                     36500003
365100                  END-IF                                          36510003
365200              END-IF                                              36520003
365300           END-IF                                                 36530003
365400        END-IF                                                    36540003
365500                                                                  36550003
365600        IF NOT P943-REJECT-EMPLOYEE-FUN                           36560003
365700           IF P917-EDB-CODE > SPACES                              36570003
365800              IF  P943-LAYOFF-FUN                                 36580003
365900                  MOVE P917-EDB-CODE       TO P943-ECC-CODE       36590003
366000              ELSE                                                36600003
366100                  IF  P943-PENDING-LAYOFF-FUN                     36610003
366200                      MOVE P917-EDB-CODE   TO P943-FUN36-ECC-CODE 36620003
366300                 END-IF                                           36630003
366400              END-IF                                              36640003
366500           END-IF                                                 36650003
366600                                                                  36660003
366700           IF P917-NBR-DAYS > SPACES                              36670003
366800              IF  P943-LAYOFF-FUN                                 36680003
366900                  MOVE P917-NBR-DAYS       TO P943-NBR-DAYS       36690003
367000              ELSE                                                36700003
367100                 IF  P943-PENDING-LAYOFF-FUN                      36710003
367200                     MOVE P917-NBR-DAYS    TO P943-FUN36-NBR-DAYS 36720003
367300                 END-IF                                           36730003
367400              END-IF                                              36740003
367500           END-IF                                                 36750003
367600        END-IF                                                    36760003
367700     END-IF                                                       36770003
367800                                                                  36780003
367900     IF WS-STAY-UNAVAILABLE                                       36790003
368000       CONTINUE                                                   36800003
368100     ELSE                                                         36810003
368200       PERFORM P8900-WRITE-HISTORY                                36820003
368300     END-IF.                                                      36830003
368400*                                                                 36840003
368500 P2010-SETUP-MISSED-CALL.                                         36850003
368600*                                                                 36860003
368700     IF MSTR-CONS-MISSED-CALLS NOT NUMERIC                        36870003
368800        MOVE ZEROS                     TO MSTR-CONS-MISSED-CALLS  36880003
368900     END-IF                                                       36890003
369000     ADD 1                             TO                         36900003
369100                                       MSTR-CONS-MISSED-CALLS-NUM 36910003
369200     MOVE SPACES                       TO WS-MC-MU-DATE-TIME      36920003
369300                                          WS-MC-LO-DATE-TIME      36930003
369400     IF (TEMP-ASGN-XB                                             36940003
369500        OR (NORM-ASGN-XB AND TEMPORARY-ASGNMT NOT > SPACE))       36950003
369600        AND ON-DUTY-ASGNMT NOT > SPACE                            36960003
369700        MOVE SPACES                    TO WORK-CNTLKEY            36970003
369800        MOVE '69'                      TO WK-CNTL-REC-TYPE        36980003
369900        MOVE P917-MISSED-CALL-FUNC     TO WK-CNTL-MC-CODE         36990003
370000*-------------------------------------------------------------    37000003
370100*     FOR FASTSLOW SPAREBOARDS, MISSED CALL PROCESSING MAY        37010003
370200*     OCCUR ON BOTH SIDES OF THE BOARD.  IN THIS CASE, WHEN       37020003
370300*     APPLYING THE SECOND WAVE OF PENALTIES, THE YARD/ROAD        37030003
370400*     DESIGNATION MUST BE REVERSED TO GET THE CORRECT             37040003
370500*     CONTROL RECORD.                                             37050003
370600*----------------------------------------------------------PLS    37060003
370700        IF APPLYING-TO-OPP-BOARD                                  37070003
370800           IF P917-YARD                                           37080003
370900              MOVE 'RD'                TO WK-CNTL-MC-TYPE-SVC     37090003
371000           ELSE                                                   37100003
371100              MOVE 'YD'                TO WK-CNTL-MC-TYPE-SVC     37110003
371200           END-IF                                                 37120003
371300        ELSE                                                      37130003
371400           IF P917-YARD                                           37140003
371500              MOVE 'YD'                   TO WK-CNTL-MC-TYPE-SVC  37150003
371600           ELSE                                                   37160003
371700             MOVE 'RD'                   TO WK-CNTL-MC-TYPE-SVC   37170003
371800           END-IF                                                 37180003
371900        END-IF                                                    37190003
372000        IF TEMPORARY-ASGNMT > SPACES                              37200003
372100           MOVE TA-DIST                TO WK-CNTL-MC-DIST         37210003
372200           MOVE TA-SUB-DIST            TO WK-CNTL-MC-SDIST        37220003
372300           MOVE TA-CC                  TO WK-CNTL-MC-XB           37230003
372400        ELSE                                                      37240003
372500           MOVE NA-DIST                TO WK-CNTL-MC-DIST         37250003
372600           MOVE NA-SUB-DIST            TO WK-CNTL-MC-SDIST        37260003
372700           MOVE NA-CC                  TO WK-CNTL-MC-XB           37270003
372800        END-IF                                                    37280003
372900        MOVE MSTR-CONS-MISSED-CALLS    TO WK-CNTL-MC-CONS         37290003
373000        PERFORM P2030-READ-MC-CONTROL                             37300003
373100        IF NOT SUCCESS                                            37310003
373200           MOVE '**'                   TO WK-CNTL-MC-XB           37320003
373300           PERFORM P2030-READ-MC-CONTROL                          37330003
373400           IF NOT SUCCESS                                         37340003
373500              MOVE '**'                TO WK-CNTL-MC-SDIST        37350003
373600              PERFORM P2030-READ-MC-CONTROL                       37360003
373700              IF NOT SUCCESS                                      37370003
373800                 MOVE '**'             TO WK-CNTL-MC-DIST         37380003
373900                 PERFORM P2030-READ-MC-CONTROL                    37390003
374000              END-IF                                              37400003
374100           END-IF                                                 37410003
374200        END-IF                                                    37420003
374300        IF SUCCESS                                                37430003
374400           IF CNTL-MC-ADD-START                                   37440003
374500              SET MC-ADD-START          TO TRUE                   37450003
374600           END-IF                                                 37460003
374700           IF CNTL-MC-RESET-START                                 37470003
374800              SET MC-RESET-START        TO TRUE                   37480003
374900           END-IF                                                 37490003
375000           IF NOT CNTL-MC-RETAIN-POSITION                         37500003
375100              IF CNTL-MC-ECC-CODE > SPACES                        37510003
375200                 MOVE CNTL-MC-ECC-CODE TO P917-EDB-CODE           37520003
375300              END-IF                                              37530003
375400              IF CNTL-MC-U-OPTION = '1'                           37540003
375500                 SET MC-LAYOFF         TO TRUE                    37550003
375600              END-IF                                              37560003
375700              IF CNTL-MC-U-OPTION = '2'                           37570003
375800                 SET MC-LAYOFF         TO TRUE                    37580003
375900                 SET MC-TRACK          TO TRUE                    37590003
376000              END-IF                                              37600003
376100              IF CNTL-MC-U-OPTION = '3'                           37610003
376200                 SET MC-LAYOFF         TO TRUE                    37620003
376300                 SET MC-TRACK          TO TRUE                    37630003
376400                 SET MC-PEND-MARKUP    TO TRUE                    37640003
376500                 MOVE ZEROS            TO DATE-CONVERSION-PARMS   37650003
376600                 MOVE WS-LOCAL-DATE    TO PARM-PRI-DATE-GREG      37660003
376700                 MOVE WS-LOCAL-TIME    TO PARM-PRI-HRMN           37670003
376800                 MOVE '2400'           TO PARM-SEC-HRMN           37680003
376900              END-IF                                              37690003
377000              IF CNTL-MC-U-OPTION = '4'                           37700003
377100                 IF CNTL-MC-HOURS = '0000'                        37710003
377200                    SET MC-REALIGN-XB         TO TRUE             37720003
377300                    IF CNTL-MC-T-OPTION = '1'                     37730003
377400*NB BEG                                                           37740003
377500*                      MOVE WS-LOCAL-DATE-TIME                    37750003
377600*                                      TO WS-MC-MU-DATE-TIME      37760003
377700                       MOVE SPACES            TO TZ-PARAMETERS    37770003
377800*                      MOVE WS-CURR-DATE-TIME TO TZ-IN-DATE-TIME  37780003
377900                       MOVE WS-PRESENT-TIME   TO TZ-IN-DATE-TIME  37790003
378000                       SET TZ-IN-SYSTEM-ZONE  TO TRUE             37800003
378100                       MOVE P917-TIME-ZONE    TO TZ-OUT-ZONE      37810003
378200                       PERFORM P8996-TIMEZONE                     37820003
378300                       MOVE TZ-OUT-DATE-TIME TO WS-MC-MU-DATE-TIME37830003
378400                                                WS-MC-LO-DATE-TIME37840003
378500*NB END                                                           37850003
378600                    END-IF                                        37860003
378700                    IF CNTL-MC-T-OPTION = '2'                     37870003
378800                       MOVE P917-ACTION-DATE-TIME                 37880003
378900                                       TO WS-MC-MU-DATE-TIME      37890003
379000                                          WS-MC-LO-DATE-TIME      37900003
379100                    END-IF                                        37910003
379200                    IF CNTL-MC-T-OPTION = '3'                     37920003
379300                       MOVE P917-CALL-DATE-TIME                   37930003
379400                                       TO WS-MC-MU-DATE-TIME      37940003
379500                                          WS-MC-LO-DATE-TIME      37950003
379600                    END-IF                                        37960003
379700*                   IF CNTL-MC-T-OPTION = '2' OR '3'              37970003
379800*                      MOVE SPACES     TO TZ-PARAMETERS           37980003
379900*                      MOVE P917-TIME-ZONE                        37990003
380000*                                      TO TZ-IN-ZONE              38000003
380100*                      MOVE WS-MC-MU-DATE-TIME                    38010003
380200*                                            TO TZ-IN-DATE-TIME   38020003
380300*                      MOVE WS-TIME-ZONE     TO TZ-OUT-ZONE       38030003
380400*                      SET TZ-OUT-SYSTEM-ZONE TO TRUE             38040003
380500*                      PERFORM P8996-TIMEZONE                     38050003
380600*                      MOVE TZ-OUT-DATE-TIME TO WS-MC-MU-DATE-TIME38060003
380700*                   END-IF                                        38070003
380800                 ELSE                                             38080003
380900                    SET MC-LAYOFF      TO TRUE                    38090003
381000                    SET MC-PEND-MARKUP TO TRUE                    38100003
381100                    MOVE ZEROS         TO DATE-CONVERSION-PARMS   38110003
381200                    IF CNTL-MC-T-OPTION = '1'                     38120003
381300*NB BEG                                                           38130003
381400*                      MOVE WS-LOCAL-DATE                         38140003
381500*                                      TO PARM-PRI-DATE-GREG      38150003
381600*                      MOVE WS-LOCAL-TIME                         38160003
381700*                                      TO PARM-PRI-HRMN           38170003
381800                       MOVE SPACES            TO TZ-PARAMETERS    38180003
381900*                      MOVE WS-CURR-DATE-TIME TO TZ-IN-DATE-TIME  38190003
382000                       MOVE WS-PRESENT-TIME   TO TZ-IN-DATE-TIME  38200003
382100                       SET TZ-IN-SYSTEM-ZONE  TO TRUE             38210003
382200                       MOVE P917-TIME-ZONE    TO TZ-OUT-ZONE      38220003
382300                       PERFORM P8996-TIMEZONE                     38230003
382400                       MOVE TZ-OUT-DATE TO PARM-PRI-DATE-GREG     38240003
382500                                           WS-MC-LO-DATE          38250003
382600                       MOVE TZ-OUT-TIME TO PARM-PRI-HRMN          38260003
382700                                           WS-MC-LO-TIME          38270003
382800*NB END                                                           38280003
382900                    END-IF                                        38290003
383000                    IF CNTL-MC-T-OPTION = '2'                     38300003
383100                       MOVE P917-ACTION-DATE                      38310003
383200                                       TO PARM-PRI-DATE-GREG      38320003
383300                                          WS-MC-LO-DATE           38330003
383400                       MOVE P917-ACTION-TIME                      38340003
383500                                       TO PARM-PRI-HRMN           38350003
383600                                          WS-MC-LO-TIME           38360003
383700                    END-IF                                        38370003
383800                    IF CNTL-MC-T-OPTION = '3'                     38380003
383900                       MOVE P917-CALL-DATE                        38390003
384000                                       TO PARM-PRI-DATE-GREG      38400003
384100                                          WS-MC-LO-DATE           38410003
384200                       MOVE P917-CALL-TIME                        38420003
384300                                       TO PARM-PRI-HRMN           38430003
384400                                          WS-MC-LO-TIME           38440003
384500                    END-IF                                        38450003
384600*                   STRING CNTL-MC-HOURS '00'                     38460003
384700*                          DELIMITED BY SIZE                      38470003
384800*                          INTO PARM-SEC-HR-MN                    38480003
384900                    MOVE CNTL-MC-HOURS TO PARM-SEC-HR-MN          38490003
385000                 END-IF                                           38500003
385100              END-IF                                              38510003
385200              IF MC-PEND-MARKUP                                   38520003
385300                 SET PARM-ADD           TO TRUE                   38530003
385400                 EXEC CICS LINK                                   38540003
385500                           PROGRAM(P903-PGM)                      38550003
385600                           COMMAREA(DATE-CONVERSION-PARMS)        38560003
385700                           LENGTH(P903-LGTH)                      38570003
385800                           RESP(WS-RESPONSE)                      38580003
385900                 END-EXEC                                         38590003
386000                 MOVE WS-RESPONSE      TO FILE-STATUS             38600003
386100                 IF NOT SUCCESS                                   38610003
386200                    MOVE 'P2010-1'     TO ERR-PARAGRAPH           38620003
386300                    MOVE 'P903LINK'    TO ERR-KEY                 38630003
386400                    PERFORM P9999-GOT-PROBLEM                     38640003
386500                 END-IF                                           38650003
386600                 MOVE SPACES           TO TZ-PARAMETERS           38660003
386700                 MOVE P917-TIME-ZONE   TO TZ-IN-ZONE              38670003
386800                 MOVE PARM-RES-DATE-GREG                          38680003
386900                                       TO TZ-IN-DATE              38690003
387000                 MOVE PARM-RES-HRMN    TO TZ-IN-TIME              38700003
387100                 MOVE WS-TIME-ZONE     TO TZ-OUT-ZONE             38710003
387200                 PERFORM P8996-TIMEZONE                           38720003
387300                 MOVE TZ-OUT-DATE-TIME TO WS-MC-MU-DATE-TIME      38730003
387400              END-IF                                              38740003
387500           END-IF                                                 38750003
387600        ELSE                                                      38760003
387700           SET MC-LAYOFF               TO TRUE                    38770003
387800        END-IF                                                    38780003
387900     ELSE                                                         38790003
388000        SET MC-LAYOFF                  TO TRUE                    38800003
388100     END-IF.                                                      38810003
388200*                                                                 38820003
388300 P2020-PROCESS-MISSED-CALLS.                                      38830003
388400*                                                                 38840003
388500     IF MC-TRACK                                                  38850003
388600        MOVE SPACES                  TO P938-COMMAREA-PARMS       38860003
388700        SET P938-INITIATE-FUNCTION   TO TRUE                      38870003
388800        MOVE P917-OC-ASGN-TYPE       TO P938-ASGN-TYPE            38880003
388900        MOVE P917-OC-ASGN            TO P938-ASGN-PARM            38890003
389000        MOVE P917-DECKEY             TO P938-DECKEY               38900003
389100        MOVE P917-EFF-DATE-TIME      TO P938-EFF-DATE-TIME        38910003
389200        MOVE P917-CALL-DATE-TIME     TO P938-FUNCTION-DATE-TIME   38920003
389300        MOVE P917-CALL-LEAD-TIME     TO P938-CALL-LEAD-TIME       38930003
389400        MOVE P917-TIME-ZONE          TO P938-TIME-ZONE            38940003
389500        SET P938-INIT-OFF-ON-CALL    TO TRUE                      38950003
389600        MOVE P917-EMP-NO             TO P938-INIT-EMP-NO          38960003
389700        MOVE P917-TRAIN              TO P938-INIT-TRAIN           38970003
389800        SET P938-INIT-MARKUP-ON-ARR  TO TRUE                      38980003
389900        SET P938-INIT-MCALL          TO TRUE                      38990003
390000        EXEC CICS LINK                                            39000003
390100                  PROGRAM(P938-PGM)                               39010003
390200                  COMMAREA(P938-COMMAREA-PARMS)                   39020003
390300                  LENGTH(P938-LGTH)                               39030003
390400                  RESP(WS-RESPONSE)                               39040003
390500        END-EXEC                                                  39050003
390600        MOVE WS-RESPONSE             TO FILE-STATUS               39060003
390700        IF NOT SUCCESS                                            39070003
390800           MOVE 'P2020-2'            TO ERR-PARAGRAPH             39080003
390900           MOVE 'P938LINK'           TO ERR-KEY                   39090003
391000           PERFORM P9999-GOT-PROBLEM                              39100003
391100        END-IF                                                    39110003
391200        SET P917-EMP-WAS-TRACKED     TO TRUE                      39120003
391300     END-IF                                                       39130003
391400*                                                                 39140003
391500     IF MC-PEND-MARKUP                                            39150003
391600        MOVE SPACES                  TO P927-COMMAREA-PARMS       39160003
391700        SET P927-CHG-STATUS-FUNCTION TO TRUE                      39170003
391800        SET P927-PEND-MC             TO TRUE                      39180003
391900        MOVE WS-MC-MU-DATE           TO P927-EFF-DATE             39190003
392000        MOVE WS-MC-MU-TIME           TO P927-EFF-TIME             39200003
392100        MOVE WS-TIME-ZONE            TO P927-TIME-ZONE            39210003
392200        MOVE DIST IN WS-MSTR         TO P927-DIST                 39220003
392300        MOVE SUB-DIST IN WS-MSTR     TO P927-SUB-DIST             39230003
392400        MOVE EMP-NBR  IN WS-MSTR     TO P927-CS-EMP-NO            39240003
392500        MOVE 'A0'                    TO P927-CS-LO-CODE           39250003
392600        PERFORM P8800-WRITE-TASK                                  39260003
392700     END-IF                                                       39270003
392800*                                                                 39280003
392900     IF MC-REALIGN-XB                                             39290003
393000        MOVE SPACES                  TO P914-COMMAREA-PARMS       39300003
393100        SET P914-REALIGN-FUNCTION    TO TRUE                      39310003
393200        IF TEMP-ASGN-XB                                           39320003
393300           MOVE TA-DIST              TO P914-TURN-DIST            39330003
393400           MOVE TA-SUB-DIST          TO P914-TURN-SUB-DIST        39340003
393500           MOVE TA-CC                TO P914-TURN-CC              39350003
393600           MOVE TA-XB-TURN           TO P914-TURN                 39360003
393700        ELSE                                                      39370003
393800           MOVE NA-DIST              TO P914-TURN-DIST            39380003
393900           MOVE NA-SUB-DIST          TO P914-TURN-SUB-DIST        39390003
394000           MOVE NA-CC                TO P914-TURN-CC              39400003
394100           MOVE NA-XB-TURN           TO P914-TURN                 39410003
394200        END-IF                                                    39420003
394300        MOVE WS-MC-MU-DATE-TIME      TO P914-EFF-DATE-TIME        39430003
394400        MOVE P917-TIME-ZONE          TO P914-TIME-ZONE            39440003
394500        MOVE WS-SAVE-LO              TO P914-STATUS-CODE          39450003
394600        MOVE WS-SAVE-EDB             TO P914-REASON-CODE          39460003
394700        IF P917-ROAD                                              39470003
394800           SET P914-ROAD       TO TRUE                            39480003
394900        ELSE                                                      39490003
395000           SET P914-YARD       TO TRUE                            39500003
395100        END-IF                                                    39510003
395200        EXEC CICS LINK                                            39520003
395300                  PROGRAM(P914-PGM)                               39530003
395400                  COMMAREA(P914-COMMAREA-PARMS)                   39540003
395500                  LENGTH(P914-LGTH)                               39550003
395600                  RESP(WS-RESPONSE)                               39560003
395700        END-EXEC                                                  39570003
395800        MOVE WS-RESPONSE             TO FILE-STATUS               39580003
395900        IF NOT SUCCESS                                            39590003
396000           MOVE 'P2020-3'            TO ERR-PARAGRAPH             39600003
396100           MOVE 'P914LINK'           TO ERR-KEY                   39610003
396200           PERFORM P9999-GOT-PROBLEM                              39620003
396300        END-IF                                                    39630003
396400*-------------------------------------------------------------    39640003
396500*     FOR FASTSLOW SPAREBOARDS, MISSED CALL PROCESSING MAY        39650003
396600*     OCCUR ON BOTH SIDES OF THE BOARD.  IN THIS CASE, WHEN       39660003
396700*     APPLYING THE SECOND WAVE OF PENALTIES, THE YARD/ROAD        39670003
396800*     DESIGNATION MUST BE REVERSED TO PROCESS THE CORRECT         39680003
396900*     BOARD IN CNP914.                                            39690003
397000*----------------------------------------------------------PLS    39700003
397100        IF APPLYING-TO-OPP-BOARD                                  39710003
397200           MOVE WS-CNTL-FILE            TO SAVE-CNTL-FILE         39720003
397300           PERFORM P2010-SETUP-MISSED-CALL                        39730003
397400           MOVE SAVE-CNTL-FILE          TO WS-CNTL-FILE           39740003
397500           MOVE SPACES                  TO P914-COMMAREA-PARMS    39750003
397600           SET P914-REALIGN-FUNCTION    TO TRUE                   39760003
397700           IF TEMP-ASGN-XB                                        39770003
397800              MOVE TA-DIST              TO P914-TURN-DIST         39780003
397900              MOVE TA-SUB-DIST          TO P914-TURN-SUB-DIST     39790003
398000              MOVE TA-CC                TO P914-TURN-CC           39800003
398100              MOVE TA-XB-TURN           TO P914-TURN              39810003
398200           ELSE                                                   39820003
398300              MOVE NA-DIST              TO P914-TURN-DIST         39830003
398400              MOVE NA-SUB-DIST          TO P914-TURN-SUB-DIST     39840003
398500              MOVE NA-CC                TO P914-TURN-CC           39850003
398600              MOVE NA-XB-TURN           TO P914-TURN              39860003
398700           END-IF                                                 39870003
398800           MOVE WS-MC-MU-DATE-TIME      TO P914-EFF-DATE-TIME     39880003
398900           MOVE P917-TIME-ZONE          TO P914-TIME-ZONE         39890003
399000           IF P917-ROAD                                           39900003
399100              SET P914-YARD       TO TRUE                         39910003
399200           ELSE                                                   39920003
399300              SET P914-ROAD       TO TRUE                         39930003
399400           END-IF                                                 39940003
399500           EXEC CICS LINK                                         39950003
399600                     PROGRAM(P914-PGM)                            39960003
399700                     COMMAREA(P914-COMMAREA-PARMS)                39970003
399800                     LENGTH(P914-LGTH)                            39980003
399900                     RESP(WS-RESPONSE)                            39990003
400000           END-EXEC                                               40000003
400100           MOVE WS-RESPONSE             TO FILE-STATUS            40010003
400200           IF NOT SUCCESS                                         40020003
400300              MOVE 'P2020-4'            TO ERR-PARAGRAPH          40030003
400400              MOVE 'P914LINK'           TO ERR-KEY                40040003
400500              PERFORM P9999-GOT-PROBLEM                           40050003
400600           END-IF                                                 40060003
400700        END-IF                                                    40070003
400800     END-IF                                                       40080003
400900*                                                                 40090003
401000     IF MC-ADD-START                                              40100003
401100        MOVE SPACES                     TO P931-COMMAREA-PARMS    40110003
401200        SET  P931-ADD-FUN               TO TRUE                   40120003
401300        MOVE P917-EMP-NO                TO P931-UPD-EMP-NO        40130003
401400*     CNC0006 - FLW, 5/8/96, START                                40140003
401500        SET P931-STRAIGHT-START         TO TRUE                   40150003
401600        MOVE P917-EFF-DATE-TIME        TO P931-EFFECTIVE-DATE-TIME40160003
401700*       MOVE P917-EFF-DATE              TO P931-UPD-DATE          40170003
401800*     CNC0006 - FLW, 5/8/96, END                                  40180003
401900        EXEC CICS LINK                                            40190003
402000                  PROGRAM(P931-PGM)                               40200003
402100                  COMMAREA(P931-COMMAREA-PARMS)                   40210003
402200                  LENGTH(P931-LGTH)                               40220003
402300                  RESP(WS-RESPONSE)                               40230003
402400        END-EXEC                                                  40240003
402500        MOVE WS-RESPONSE                TO FILE-STATUS            40250003
402600        IF NOT SUCCESS                                            40260003
402700           MOVE 'P2020-5'               TO ERR-PARAGRAPH          40270003
402800           MOVE 'P931LINK'              TO ERR-KEY                40280003
402900           PERFORM P9999-GOT-PROBLEM                              40290003
403000        END-IF                                                    40300003
403100     END-IF                                                       40310003
403200     IF MC-RESET-START                                            40320003
403300        MOVE SPACES                     TO P931-COMMAREA-PARMS    40330003
403400        SET  P931-RESET-STARTS-FUN      TO TRUE                   40340003
403500        MOVE P917-EMP-NO                TO P931-RS-EMP-NO         40350003
403600*     CNC0006 - FLW, 5/8/96, START                                40360003
403700        SET P931-RS-STT-OVT-ST          TO TRUE                   40370003
403800        MOVE P917-EFF-DATE-TIME        TO P931-EFFECTIVE-DATE-TIME40380003
403900*     CNC0006 - FLW, 5/8/96, END                                  40390003
404000        EXEC CICS LINK                                            40400003
404100                  PROGRAM(P931-PGM)                               40410003
404200                  COMMAREA(P931-COMMAREA-PARMS)                   40420003
404300                  LENGTH(P931-LGTH)                               40430003
404400                  RESP(WS-RESPONSE)                               40440003
404500        END-EXEC                                                  40450003
404600        MOVE WS-RESPONSE                TO FILE-STATUS            40460003
404700        IF NOT SUCCESS                                            40470003
404800           MOVE 'P2020-6'               TO ERR-PARAGRAPH          40480003
404900           MOVE 'P931LINK'              TO ERR-KEY                40490003
405000           PERFORM P9999-GOT-PROBLEM                              40500003
405100        END-IF                                                    40510003
405200     END-IF.                                                      40520003
405300*                                                                 40530003
405400 P2030-READ-MC-CONTROL.                                           40540003
405500*                                                                 40550003
405600     MOVE WORK-CNTLKEY              TO CNTLKEY                    40560003
405700     EXEC CICS READ                                               40570003
405800               DATASET(CNTL-FILE-VIA-CNTLKEY)                     40580003
405900               INTO(WS-CNTL-FILE)                                 40590003
406000               LENGTH(CNTLFILE-RLGTH)                             40600003
406100               RIDFLD(CNTLKEY)                                    40610003
406200               KEYLENGTH(CNTLFILE-KLGTH)                          40620003
406300               GTEQ                                               40630003
406400               RESP(WS-RESPONSE)                                  40640003
406500     END-EXEC                                                     40650003
406600     MOVE WS-RESPONSE               TO FILE-STATUS                40660003
406700     IF SUCCESS                                                   40670003
406800        IF CNTL-REC-TYPE NOT = '69'                               40680003
406900           OR CNTL-MC-CODE NOT = WK-CNTL-MC-CODE                  40690003
407000           OR CNTL-MC-TYPE-SVC NOT = WK-CNTL-MC-TYPE-SVC          40700003
407100           OR CNTL-MC-DIST NOT = WK-CNTL-MC-DIST                  40710003
407200           OR CNTL-MC-SDIST NOT = WK-CNTL-MC-SDIST                40720003
407300           OR CNTL-MC-XB NOT = WK-CNTL-MC-XB                      40730003
407400           SET NO-RECORD-FND        TO TRUE                       40740003
407500        END-IF                                                    40750003
407600     ELSE                                                         40760003
407700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     40770003
407800           MOVE 'P2030-1'           TO ERR-PARAGRAPH              40780003
407900           MOVE CNTLKEY             TO ERR-KEY                    40790003
408000           PERFORM P9999-GOT-PROBLEM                              40800003
408100        END-IF                                                    40810003
408200     END-IF.                                                      40820003
408300*                                                                 40830003
408400 P2040-UNAVL-MKUP-FROM-TRK.                                       40840003
408500*                                                                 40850003
408600*    SEE IF THERE IS A MORE RECENT TEMPORARY OWNER OF THE TURN.   40860003
408700*    IF NOT, PUT THE TURN BACK ON BOARD.                          40870003
408800*                                                                 40880003
408900     MOVE SPACES                TO ASGNKEY1                       40890003
409000     SET ASGN-UFP-JOB           TO TRUE                           40900003
409100     IF TEMPORARY-ASGNMT > SPACES                                 40910003
409200        MOVE TEMPORARY-ASGNMT   TO ASGN-ASSIGNMENT                40920003
409300     ELSE                                                         40930003
409400        MOVE NORMAL-ASGNMT      TO ASGN-ASSIGNMENT                40940003
409500     END-IF                                                       40950003
409600     PERFORM PXXXX-LATEST-TEMP                                    40960003
409700     IF ASGN-EMP-NO = ZERO                                        40970003
409800        OR ASGN-EMP-NO = EMP-NBR OF WS-MSTR                       40980003
409900        PERFORM P2300-MARKUP-UFP                                  40990003
410000     END-IF.                                                      41000003
410100*                                                                 41010003
410200 P2050-DELETE-PENDED-MARKUP.                                      41020003
410300*                                                                 41030003
410400     MOVE '0'                   TO WS-DONE-CODE                   41040003
410500     MOVE SPACES                TO TASK-EMPLOYEE-KEY              41050003
410600*    MOVE EMP-NBR OF WS-MSTR    TO EMP-NBR OF WS-TASK             41060003
410700     MOVE P917-EMP-NO           TO EMP-NBR OF WS-TASK             41070003
410800     MOVE 'F'                   TO TASK-TYPE                      41080003
410900     MOVE TASK-EMPLOYEE-KEY     TO TASKEMPK                       41090003
411000     EXEC CICS STARTBR                                            41100003
411100               DATASET(TASK-VIA-EMP-NBR)                          41110003
411200               RIDFLD(TASKEMPK)                                   41120003
411300               RESP(WS-RESPONSE)                                  41130003
411400     END-EXEC                                                     41140003
411500                                                                  41150003
411600     MOVE WS-RESPONSE          TO FILE-STATUS                     41160003
411700                                                                  41170003
411800     IF NOT SUCCESS                                               41180003
411900        SET DONE TO TRUE                                          41190003
412000        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     41200003
412100           MOVE 'P2050-4'  TO ERR-PARAGRAPH                       41210003
412200           MOVE TASKEMPK   TO ERR-KEY                             41220003
412300           PERFORM P9999-GOT-PROBLEM                              41230003
412400        END-IF                                                    41240003
412500     ELSE                                                         41250003
412600         PERFORM UNTIL DONE                                       41260003
412700            EXEC CICS READNEXT                                    41270003
412800                      DATASET(TASK-VIA-EMP-NBR)                   41280003
412900                      INTO(WS-TASK)                               41290003
413000                      LENGTH(TASKENBR-RLGTH)                      41300003
413100                      RIDFLD(TASKEMPK)                            41310003
413200                      KEYLENGTH(TASKENBR-KLGTH)                   41320003
413300                      RESP(WS-RESPONSE)                           41330003
413400            END-EXEC                                              41340003
413500            MOVE WS-RESPONSE        TO FILE-STATUS                41350003
413600            IF SUCCESS                                            41360003
413700*              IF EMP-NBR OF WS-TASK = EMP-NBR OF WS-MSTR         41370003
413800               IF EMP-NBR OF WS-TASK = P917-EMP-NO                41380003
413900                  IF  TASK-TYPE = 'F'                             41390003
414000                  AND TASK-LO1 = 'A'                              41400003
414100                     EXEC CICS ENDBR                              41410003
414200                               DATASET(TASK-VIA-EMP-NBR)          41420003
414300                               RESP(WS-RESPONSE)                  41430003
414400                     END-EXEC                                     41440003
414500                     MOVE TASK-LOCATION-KEY TO TASKTIME           41450003
414600                     EXEC CICS DELETE                             41460003
414700                               DATASET(TASK-VIA-EFF-TIME)         41470003
414800                               RIDFLD(TASKTIME)                   41480003
414900                               RESP(WS-RESPONSE)                  41490003
415000                     END-EXEC                                     41500003
415100                     MOVE WS-RESPONSE   TO FILE-STATUS            41510003
415200                     IF NOT (SUCCESS OR NO-RECORD-FND)            41520003
415300                        MOVE 'P2050-1'  TO ERR-PARAGRAPH          41530003
415400                        MOVE TASKEMPK   TO ERR-KEY                41540003
415500                        PERFORM P9999-GOT-PROBLEM                 41550003
415600                     END-IF                                       41560003
415700                     MOVE TASK-EMPLOYEE-KEY  TO TASKEMPK          41570003
415800                     EXEC CICS STARTBR                            41580003
415900                               DATASET(TASK-VIA-EMP-NBR)          41590003
416000                               RIDFLD(TASKEMPK)                   41600003
416100                               RESP(WS-RESPONSE)                  41610003
416200                     END-EXEC                                     41620003
416300                     MOVE WS-RESPONSE          TO FILE-STATUS     41630003
416400                                                                  41640003
416500                     IF NOT SUCCESS                               41650003
416600                        SET DONE TO TRUE                          41660003
416700                        IF NOT (NO-RECORD-FND OR END-OF-FILE)     41670003
416800                           MOVE 'P2050-5'  TO ERR-PARAGRAPH       41680003
416900                           MOVE TASKEMPK   TO ERR-KEY             41690003
417000                           PERFORM P9999-GOT-PROBLEM              41700003
417100                        END-IF                                    41710003
417200                     END-IF                                       41720003
417300                  END-IF                                          41730003
417400               ELSE                                               41740003
417500                  SET DONE TO TRUE                                41750003
417600               END-IF                                             41760003
417700            ELSE                                                  41770003
417800               SET DONE TO TRUE                                   41780003
417900               IF NOT (NO-RECORD-FND OR END-OF-FILE)              41790003
418000                  MOVE 'P2050-2'     TO ERR-PARAGRAPH             41800003
418100                  MOVE TASKEMPK      TO ERR-KEY                   41810003
418200                  PERFORM P9999-GOT-PROBLEM                       41820003
418300               END-IF                                             41830003
418400            END-IF                                                41840003
418500         END-PERFORM                                              41850003
418600         EXEC CICS ENDBR                                          41860003
418700                   DATASET(TASK-VIA-EMP-NBR)                      41870003
418800                   RESP(WS-RESPONSE)                              41880003
418900         END-EXEC                                                 41890003
419000         IF NOT P917-MKUP-FROM-TRK                                41900003
419100            MOVE SPACES                 TO P938-COMMAREA-PARMS    41910003
419200            SET P938-REL-TRACKING-COND  TO TRUE                   41920003
419300*           MOVE EMP-NBR OF WS-MSTR     TO P938-RTC-EMP-NO        41930003
419400            MOVE P917-EMP-NO            TO P938-RTC-EMP-NO        41940003
419500            SET P938-RTC-MARKUPS        TO TRUE                   41950003
419600            EXEC CICS LINK                                        41960003
419700                      PROGRAM(P938-PGM)                           41970003
419800                      COMMAREA(P938-COMMAREA-PARMS)               41980003
419900                      LENGTH(P938-LGTH)                           41990003
420000                      RESP(WS-RESPONSE)                           42000003
420100            END-EXEC                                              42010003
420200            MOVE WS-RESPONSE            TO FILE-STATUS            42020003
420300            IF NOT SUCCESS                                        42030003
420400               MOVE 'P2050-3'           TO ERR-PARAGRAPH          42040003
420500               MOVE 'P938LINK'          TO ERR-KEY                42050003
420600               PERFORM P9999-GOT-PROBLEM                          42060003
420700            END-IF                                                42070003
420800         END-IF                                                   42080003
420900     END-IF                                                       42090003
421000     .                                                            42100003
421100*                                                                 42110003
421200 P2060-SET-XB-MC-FLAG.                                            42120003
421300*                                                                 42130003
421400*    THIS IS AN XB MISSED CALL AND IT IS NOT A LAYOFF SO WE       42140003
421500*    STILL HAVE TO SET A FLAG IN THE TURN RECORD.  THIS FLAG WILL 42150003
421600*    INDICATE THAT THIS TURN WAS LAST POSITIONED DUE TO A MISSED  42160003
421700*    CALL EVEN IF IT WAS NOT TAKEN OFF BOARD.  THIS WILL ENABLE   42170003
421800*    THE AWARDS PROCESS TO POSITION THE TURN CORRECTLY IF THE     42180003
421900*    OWNER HAPPENS TO GET AWARDED TO THE SAME XB. OTHERWISE, THE  42190003
422000*    P2200-LAYOFF-XB ROUTINE WILL CALL 913 WHICH WILL SET THE FLAG42200003
422100*    WHEN IT TAKES THE TURN OFF BOARD.                            42210003
422200*                                                                 42220003
422300     MOVE SPACES                 TO EBTURN-AREA                   42230003
422400     IF TEMP-ASGN-XB                                              42240003
422500        MOVE TA-DIST             TO DIST-REPEAT                   42250003
422600        MOVE TA-SUB-DIST         TO SUBDIST-REPEAT                42260003
422700        MOVE TA-XB-TURN                                           42270003
422800                         TO TURN-NBR OF WS-EXTRA-BOARD            42280003
422900        MOVE TA-CC               TO CRAFT-CODE-REPEAT             42290003
423000     ELSE                                                         42300003
423100        MOVE NA-DIST             TO DIST-REPEAT                   42310003
423200        MOVE NA-SUB-DIST         TO SUBDIST-REPEAT                42320003
423210        MOVE NA-XB-TURN                                           42321003
423220                         TO TURN-NBR OF WS-EXTRA-BOARD            42322003
423230        MOVE NA-CC               TO CRAFT-CODE-REPEAT             42323003
423240     END-IF                                                       42324003
423250                                                                  42325003
423260     MOVE EBTURN-AREA            TO EBTURN                        42326003
423270     EXEC CICS READ                                               42327003
423280               UPDATE                                             42328003
423290               DATASET(EB-VIA-TURN-NBR)                           42329003
423300               INTO(WS-EXTRA-BOARD)                               42330003
423400               LENGTH(EBTURNNO-RLGTH)                             42340003
423500               RIDFLD(EBTURN)                                     42350003
423600               KEYLENGTH(EBTURNNO-KLGTH)                          42360003
423700               RESP(WS-RESPONSE)                                  42370003
423800     END-EXEC                                                     42380003
423900     MOVE WS-RESPONSE            TO FILE-STATUS                   42390003
424000     IF SUCCESS                                                   42400003
424100       SET EB-POS-MISSED-CALL    TO TRUE                          42410003
424200       EXEC CICS REWRITE                                          42420003
424300                 DATASET(EB-VIA-TURN-NBR)                         42430003
424400                 FROM(WS-EXTRA-BOARD)                             42440003
424500                 LENGTH(EBTURNNO-RLGTH)                           42450003
424600                 RESP(WS-RESPONSE)                                42460003
424700       END-EXEC                                                   42470003
424800       IF NOT SUCCESS                                             42480003
424900         MOVE 'P2060-1'          TO ERR-PARAGRAPH                 42490003
425000         MOVE  EBTURN            TO ERR-KEY                       42500003
425100         PERFORM P9999-GOT-PROBLEM                                42510003
425200       END-IF                                                     42520003
425300     ELSE                                                         42530003
425400*                                                                 42540003
425500*    IF THE TURN RECORD IS NOT THERE THEN SOMETHING IS GOOFY BUT  42550003
425600*    WE'LL LET IT SLIDE FOR NOW.                                  42560003
425700*                                                                 42570003
425800       IF NOT NO-RECORD-FND                                       42580003
425900         MOVE 'P2060-2'          TO ERR-PARAGRAPH                 42590003
426000         MOVE EBTURN             TO ERR-KEY                       42600003
426100         PERFORM P9999-GOT-PROBLEM                                42610003
426200       END-IF                                                     42620003
426300     END-IF.                                                      42630003
426400*                                                                 42640003
426500 P2070-DELETE-VIRTUAL-STARTS.                                     42650003
426600*                                                                 42660003
426700*    GET EMPLOYEE'S LAST TIE-UP TIME                              42670003
426800*                                                                 42680003
426900*                                                                 42690003
427000*    FIND THE MOST RECENT TIE-UP TIME, FOR THE LAST 60 DAYS,      42700003
427100*    IF ANY.                                                      42710003
427200*                                                                 42720003
427300     MOVE ZEROS                         TO DATE-CONVERSION-PARMS  42730003
427400     SET PARM-SUBTRACT                  TO TRUE                   42740003
427500     MOVE WS-SYSTEM-DATE                TO PARM-PRI-DATE-GREG     42750003
427600     MOVE WS-SYSTEM-TIME                TO PARM-PRI-HRMN          42760003
427700                                           PARM-SEC-HRMN          42770003
427800     MOVE '60'                          TO PARM-SEC-DATE-GREG     42780003
427900     EXEC CICS LINK                                               42790003
428000               PROGRAM(P903-PGM)                                  42800003
428100               COMMAREA(DATE-CONVERSION-PARMS)                    42810003
428200               LENGTH(P903-LGTH)                                  42820003
428300               RESP(WS-RESPONSE)                                  42830003
428400     END-EXEC                                                     42840003
428500     MOVE WS-RESPONSE                   TO FILE-STATUS            42850003
428501     IF NOT SUCCESS                                               42850103
428502        MOVE 'P2070-1'                  TO ERR-PARAGRAPH          42850203
428503        MOVE 'P903LINK'                 TO ERR-KEY                42850303
428504        PERFORM P9999-GOT-PROBLEM                                 42850403
428505     END-IF                                                       42850503
428506     MOVE PARM-RES-GREG-CENT            TO INQ-LIMIT-CE           42850603
428507     MOVE PARM-RES-DATE-GREG            TO INQ-LIMIT-DATE         42850703
428508     MOVE WS-SYSTEM-TIME                TO INQ-LIMIT-TIME         42850803
428509                                                                  42850903
428510     MOVE SPACES                        TO WS-POINTER             42851003
428511     MOVE EMP-NBR IN WS-MSTR            TO POINT-EMP-NBR          42851103
428512     SET POINT-GUARANTEE                TO TRUE                   42851203
428513     MOVE WS-SYSTEM-DATE-TIME-CENT      TO POINT-EFF-CE-DATE-TIME 42851303
428514     MOVE POINT-NBR-KEY                 TO POINTKEY               42851403
428515     PERFORM P8630-STARTBR-POINTER                                42851503
428516     IF SUCCESS                                                   42851603
428517        PERFORM P8640-READPREV-POINTER                            42851703
428518        IF SUCCESS                                                42851803
428519           SET POINTER-NOT-DONE         TO TRUE                   42851903
428520           PERFORM UNTIL POINTER-DONE                             42852003
428521              PERFORM P8640-READPREV-POINTER                      42852103
428522              IF SUCCESS                                          42852203
428523                 IF POINT-EMP-NBR = P917-EMP-NO                   42852303
428524                    AND POINT-EFF-CE-DATE-TIME                    42852403
428525                                     >= INQ-LIMIT-DATE-TIME-CENT  42852503
428526                    IF POINT-TIE-UP-FUN                           42852603
428527                       SET POINTER-DONE TO TRUE                   42852703
428528                       MOVE POINT-EFF-CE-DATE-TIME                42852803
428529                                     TO INQ-LIMIT-DATE-TIME-CENT  42852903
428530                    END-IF                                        42853003
428540                 ELSE                                             42854003
428541                    SET POINTER-DONE    TO TRUE                   42854103
428542                 END-IF                                           42854203
428543              ELSE                                                42854303
428544                 SET POINTER-DONE       TO TRUE                   42854403
428545                 IF NOT NO-RECORD-FND OR END-OF-FILE              42854503
428546                    MOVE 'P2070-2'      TO ERR-PARAGRAPH          42854603
428547                    MOVE POINTKEY       TO ERR-KEY                42854703
428548                    PERFORM P9999-GOT-PROBLEM                     42854803
428549                 END-IF                                           42854903
428550              END-IF                                              42855003
428560           END-PERFORM                                            42856003
428561        ELSE                                                      42856103
428562           IF NOT NO-RECORD-FND OR END-OF-FILE                    42856203
428563              MOVE 'P2070-3'            TO ERR-PARAGRAPH          42856303
428564              MOVE POINTKEY             TO ERR-KEY                42856403
428565              PERFORM P9999-GOT-PROBLEM                           42856503
428566           END-IF                                                 42856603
428567        END-IF                                                    42856703
428568        PERFORM P8650-ENDBR-POINTER                               42856803
428569     ELSE                                                         42856903
428570        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     42857003
428571           MOVE 'P2070-4'               TO ERR-PARAGRAPH          42857103
428572           MOVE POINTKEY                TO ERR-KEY                42857203
428573           PERFORM P9999-GOT-PROBLEM                              42857303
428574        END-IF                                                    42857403
428575     END-IF                                                       42857503
428576*                                                                 42857603
428577*    LINK TO P931 TO DELETE ALL VIRTUAL STARTS RECORDS AFTER      42857703
428578*    THE LAST TIE-UP TIME.                                        42857803
428579*                                                                 42857903
428580     MOVE SPACES                     TO P931-COMMAREA-PARMS       42858003
428581     SET P931-DEL-VIRTUAL-FUN        TO TRUE                      42858103
428582     MOVE P917-EMP-NO                TO P931-RS-EMP-NO            42858203
428583     MOVE INQ-LIMIT-DATE-TIME        TO P931-EFFECTIVE-DATE-TIME  42858303
428584     PERFORM P9850-LINK-TO-P931                                   42858403
428585     .                                                            42858503
428586*                                                                 42858603
428587 P2100-MARKUP-XB.                                                 42858703
428588*                                                                 42858803
428589     MOVE SPACES TO P914-COMMAREA-PARMS                           42858903
428590     SET P914-MARKUP-FUNCTION TO TRUE                             42859003
428591     IF TEMP-ASGN-XB                                              42859103
428592        MOVE TA-DIST         TO P914-TURN-DIST                    42859203
428593        MOVE TA-SUB-DIST     TO P914-TURN-SUB-DIST                42859303
428594        MOVE TA-XB-TURN      TO P914-TURN                         42859403
428595        MOVE TA-CC           TO P914-TURN-CC                      42859503
428596     ELSE                                                         42859603
428597        MOVE NA-DIST         TO P914-TURN-DIST                    42859703
428598        MOVE NA-SUB-DIST     TO P914-TURN-SUB-DIST                42859803
428599        MOVE NA-XB-TURN      TO P914-TURN                         42859903
428600        MOVE NA-CC           TO P914-TURN-CC                      42860003
428601     END-IF                                                       42860103
428602     MOVE P917-EFF-DATE-TIME TO P914-EFF-DATE-TIME                42860203
428603     MOVE P917-TIME-ZONE     TO P914-TIME-ZONE                    42860303
428604*-------------------------------------------------------------    42860403
428605*    WHEN PUTTING A TURN BACK ON THE EXTRABOARD, SET WHETHER      42860503
428606*    IT'S A ROAD JOB OR A YARD JOB.  THIS IS SIGNIFICANT IN       42860603
428607*    CASE OF FASTSLOW BOARDS.                                     42860703
428608*----------------------------------------------------------PLS    42860803
428609     IF P917-ROAD                                                 42860903
428610        SET P914-ROAD       TO TRUE                               42861003
428620     ELSE                                                         42862003
428630        SET P914-YARD       TO TRUE                               42863003
428640     END-IF                                                       42864003
428650     MOVE WS-SAVE-LO        TO P914-STATUS-CODE                   42865003
428660     MOVE WS-SAVE-EDB       TO P914-REASON-CODE                   42866003
428670     EXEC CICS LINK                                               42867003
428680               PROGRAM(P914-PGM)                                  42868003
428690               COMMAREA(P914-COMMAREA-PARMS)                      42869003
428700               LENGTH(P914-LGTH)                                  42870003
428800               RESP(WS-RESPONSE)                                  42880003
428900     END-EXEC                                                     42890003
429000     MOVE WS-RESPONSE TO FILE-STATUS                              42900003
429100     IF NOT SUCCESS                                               42910003
429200        MOVE 'P2100-1' TO ERR-PARAGRAPH                           42920003
429300        PERFORM P9999-GOT-PROBLEM                                 42930003
429400     END-IF.                                                      42940003
429500*                                                                 42950003
429600 P2200-LAYOFF-XB.                                                 42960003
429700*                                                                 42970003
429800     MOVE SPACES                        TO P913-COMMAREA-PARMS    42980003
429900                                           WORK-CNTLKEY           42990003
430000     SET P913-LAYOFF-FUNCTION           TO TRUE                   43000003
430100     IF TEMP-ASGN-XB                                              43010003
430200        MOVE TA-DIST                    TO P913-TURN-DIST         43020003
430300                                           WK-CNTL-XB-DIST        43030003
430400        MOVE TA-SUB-DIST                TO P913-TURN-SUB-DIST     43040003
430500                                           WK-CNTL-XB-SDIST       43050003
430600        MOVE TA-XB-TURN                 TO P913-TURN              43060003
430700        MOVE TA-CC                      TO P913-TURN-CC           43070003
430800                                           WK-CNTL-XB-CC          43080003
430900     ELSE                                                         43090003
431000        MOVE NA-DIST                    TO P913-TURN-DIST         43100003
431100                                           WK-CNTL-XB-DIST        43110003
431200        MOVE NA-SUB-DIST                TO P913-TURN-SUB-DIST     43120003
431300                                           WK-CNTL-XB-SDIST       43130003
431400        MOVE NA-XB-TURN                 TO P913-TURN              43140003
431500        MOVE NA-CC                      TO P913-TURN-CC           43150003
431600                                           WK-CNTL-XB-CC          43160003
431700     END-IF                                                       43170003
431800     MOVE P917-STATUS-CODE1             TO P913-STATUS-CODE       43180003
431900     MOVE P917-EDB-CODE                 TO P913-REASON-CODE       43190003
432000     MOVE P917-EFF-DATE-TIME            TO P913-EFF-DATE-TIME     43200003
432100     MOVE P917-TIME-ZONE                TO P913-TIME-ZONE         43210003
432200     IF P917-ON-CALL                                              43220003
432300        SET P913-LAYOFF-ON-CALL         TO TRUE                   43230003
432400        MOVE SPACES                     TO WORK-DEC-TABLE         43240003
432500        IF P917-DECKEY > SPACES                                   43250003
432600           MOVE P917-DECKEY             TO DEC-KEY                43260003
432700                                           DECKEY                 43270003
432800           EXEC CICS READ                                         43280003
432900                     DATASET(DEC-TABLE-VIA-DECKEY)                43290003
433000                     INTO(WORK-DEC-TABLE)                         43300003
433100                     LENGTH(DECTABLE-RLGTH)                       43310003
433200                     RIDFLD(DECKEY)                               43320003
433300                     KEYLENGTH(DECTABLE-KLGTH)                    43330003
433400                     RESP(WS-RESPONSE)                            43340003
433500           END-EXEC                                               43350003
433600           MOVE WS-RESPONSE             TO FILE-STATUS            43360003
433700           IF SUCCESS                                             43370003
433800              IF NOT DEC-RETAIN-POS                               43380003
433900                 SET P913-DONT-RETAIN-POSITION                    43390003
434000                                        TO TRUE                   43400003
434100              END-IF                                              43410003
434200           ELSE                                                   43420003
434300              IF NOT NO-RECORD-FND                                43430003
434400                 MOVE 'P2200-1'         TO ERR-PARAGRAPH          43440003
434500                 MOVE DECKEY            TO ERR-KEY                43450003
434600                 PERFORM P9999-GOT-PROBLEM                        43460003
434700              END-IF                                              43470003
434800           END-IF                                                 43480003
434900        END-IF                                                    43490003
435000     END-IF                                                       43500003
435100*-------------------------------------------------------------    43510003
435200*    WHEN PUTTING A TURN BACK ON THE EXTRABOARD, SET WHETHER      43520003
435210*    IT'S A ROAD JOB OR A YARD JOB.  THIS IS SIGNIFICANT IN       43521003
435220*    CASE OF FASTSLOW BOARDS.                                     43522003
435230*----------------------------------------------------------PLS    43523003
435240     IF P917-ROAD                                                 43524003
435250        SET P913-ROAD-CALL              TO TRUE                   43525003
435260     ELSE                                                         43526003
435270        SET P913-YARD-CALL              TO TRUE                   43527003
435280     END-IF                                                       43528003
435290     EXEC CICS LINK                                               43529003
435300               PROGRAM(P913-PGM)                                  43530003
435310               COMMAREA(P913-COMMAREA-PARMS)                      43531003
435320               LENGTH(P913-LGTH)                                  43532003
435330               RESP(WS-RESPONSE)                                  43533003
435340     END-EXEC                                                     43534003
435350     MOVE WS-RESPONSE                   TO FILE-STATUS            43535003
435360     IF NOT SUCCESS                                               43536003
435370        MOVE 'P2200-2'                  TO ERR-PARAGRAPH          43537003
435380        PERFORM P9999-GOT-PROBLEM                                 43538003
435390     END-IF                                                       43539003
435400*-------------------------------------------------------------    43540003
435500*    CHECK TO SEE OF THIS IS A FASTSLOW-XB.  IF SO, THEN THE      43550003
435600*    OPPOSITE BOARD SIDE MAY ALSO BE AFFECTED.                    43560003
435700*----------------------------------------------------------PLS    43570003
435800     MOVE WS-CNTL-FILE              TO SAVE-CNTL-FILE             43580003
435900     PERFORM P2250-GET-XB-CNTL-REC                                43590003
436000                                                                  43600003
436100*-------------------------------------------------------------    43610003
436200*    FOR A FASTSLOW SPAREBOARD, A MISSED CALL WITH THE            43620003
436300*    'APPLY TO OPPOSITE BOARD' SET, OR ANY LAYOFF OTHER THAN      43630003
436400*    MISSED CALL SHOULD TURN THE OTHER/OPPOSITE SIDE OF THE       43640003
436500*    SPAREBOARD OFF.                                              43650003
436600*----------------------------------------------------------PLS    43660003
436700     IF FASTSLOW-XB                                               43670003
436800        IF (P917-STATUS-CODE1 = 'X' AND APPLYING-TO-OPP-BOARD)    43680003
436900           OR P917-STATUS-CODE1 NOT = 'X'                         43690003
437000           IF P917-ROAD                                           43700003
437100              SET P913-YARD-CALL    TO TRUE                       43710003
437200           ELSE                                                   43720003
437300              SET P913-ROAD-CALL    TO TRUE                       43730003
437400           END-IF                                                 43740003
437500           EXEC CICS LINK                                         43750003
437600                     PROGRAM(P913-PGM)                            43760003
437700                     COMMAREA(P913-COMMAREA-PARMS)                43770003
437800                     LENGTH(P913-LGTH)                            43780003
437900                     RESP(WS-RESPONSE)                            43790003
438000           END-EXEC                                               43800003
438100           MOVE WS-RESPONSE         TO FILE-STATUS                43810003
438200           IF NOT SUCCESS                                         43820003
438300              MOVE 'P2200-3'        TO ERR-PARAGRAPH              43830003
438400              PERFORM P9999-GOT-PROBLEM                           43840003
438500           END-IF                                                 43850003
438600        END-IF                                                    43860003
438700     END-IF                                                       43870003
438800     MOVE SAVE-CNTL-FILE            TO WS-CNTL-FILE.              43880003
438900*                                                                 43890003
439000 P2250-GET-XB-CNTL-REC.                                           43900003
439100*                                                                 43910003
439200     MOVE '08'                      TO WK-CNTL-REC-TYPE           43920003
439300     MOVE WORK-CNTLKEY              TO CNTLKEY                    43930003
439400     EXEC CICS READ                                               43940003
439500               DATASET(CNTL-FILE-VIA-CNTLKEY)                     43950003
439600               INTO(WS-CNTL-FILE)                                 43960003
439700               LENGTH(CNTLFILE-RLGTH)                             43970003
439800               RIDFLD(CNTLKEY)                                    43980003
439900               KEYLENGTH(CNTLFILE-KLGTH)                          43990003
440000               RESP(WS-RESPONSE)                                  44000003
440100     END-EXEC                                                     44010003
440200     MOVE WS-RESPONSE               TO FILE-STATUS                44020003
440300     IF NOT SUCCESS                                               44030003
440400        MOVE 'P2250-2'              TO ERR-PARAGRAPH              44040003
440500        MOVE CNTLKEY                TO ERR-KEY                    44050003
440600        PERFORM P9999-GOT-PROBLEM                                 44060003
440700     END-IF.                                                      44070003
440800*                                                                 44080003
440900 P2300-MARKUP-UFP.                                                44090003
441000*                                                                 44100003
441100     MOVE SPACES TO P916-COMMAREA-PARMS                           44110003
441200     SET P916-MARKUP-FUNCTION TO TRUE                             44120003
441300     IF TEMP-ASGN-UFP                                             44130003
441400        MOVE TEMPORARY-ASGNMT TO P916-TURN-PARM                   44140003
441500     ELSE                                                         44150003
441600        MOVE NORMAL-ASGNMT    TO P916-TURN-PARM                   44160003
441700     END-IF                                                       44170003
441800     IF P917-MKUP-FROM-TRK AND                                    44180003
441900        P917-POSITION-DATE-TIME > SPACES                          44190003
442000        MOVE P917-POSITION-DATE-TIME TO P916-EFF-DATE-TIME        44200003
442100     ELSE                                                         44210003
442200        MOVE P917-EFF-DATE-TIME      TO P916-EFF-DATE-TIME        44220003
442300     END-IF                                                       44230003
442400     MOVE P917-TIME-ZONE        TO P916-TIME-ZONE                 44240003
442500     IF WS-STAY-UNAVAILABLE                                       44250003
442600        MOVE LAYOFF-CODE-1      TO P916-STATUS-CODE               44260003
442700        MOVE LAYOFF-EM-CODE     TO P916-REASON-CODE               44270003
442800     ELSE                                                         44280003
442900        MOVE P917-STATUS-CODE1  TO P916-STATUS-CODE               44290003
443000        MOVE P917-EDB-CODE      TO P916-REASON-CODE               44300003
443100     END-IF                                                       44310003
443200*                                                                 44320003
443300*    IF WE CAME FROM EMPLOYEE TRACKING WE CAN'T CALL THE REGULAR  44330003
443400*    916 PROGRAM BECAUSE IT IS THE PROGRAM THAT CALLED 938 WHICH  44340003
443500*    CALLED 917.  INSTEAD, LINK TO 916A WHICH IS IDENTICAL TO     44350003
443600*    916.                                                         44360003
443700*                                                                 44370003
443800     IF P917-MKUP-FROM-TRK                                        44380003
443900        EXEC CICS LINK                                            44390003
444000                  PROGRAM(P916A-PGM)                              44400003
444100                  COMMAREA(P916-COMMAREA-PARMS)                   44410003
444200                  LENGTH(P916-LGTH)                               44420003
444300                  RESP(WS-RESPONSE)                               44430003
444400        END-EXEC                                                  44440003
444500     ELSE                                                         44450003
444600        EXEC CICS LINK                                            44460003
444700                  PROGRAM(P916-PGM)                               44470003
444800                  COMMAREA(P916-COMMAREA-PARMS)                   44480003
444900                  LENGTH(P916-LGTH)                               44490003
445000                  RESP(WS-RESPONSE)                               44500003
445100        END-EXEC                                                  44510003
445200     END-IF                                                       44520003
445300     MOVE WS-RESPONSE TO FILE-STATUS                              44530003
445400     IF NOT SUCCESS                                               44540003
445500        MOVE 'P2300-1' TO ERR-PARAGRAPH                           44550003
445600        PERFORM P9999-GOT-PROBLEM                                 44560003
445700     END-IF.                                                      44570003
445800*                                                                 44580003
445900*    CHECK TO SEE IF THIS EMPLOYEE'S TURN HAS BEEN DCAN'D WHILE   44590003
446000*    HE WAS MARKED OFF.  (SKIP DCAN STUFF IF THE PERSON STAYED    44600003
446100*    UNAVAILABLE).                                                44610003
446200*                                                                 44620003
446300     IF WS-STAY-UNAVAILABLE                                       44630003
446400        SET NO-RECORD-FND        TO TRUE                          44640003
446500     ELSE                                                         44650003
446600        MOVE P916-TURN-PARM      TO CARRTURN                      44660003
446700        EXEC CICS READ                                            44670003
446800                  DATASET(CARRTURN-DATASET)                       44680003
446900                  INTO(WS-CARRIED-TURN)                           44690003
447000                  LENGTH(CARRTURN-RLGTH)                          44700003
447100                  RIDFLD(CARRTURN)                                44710003
447200                  KEYLENGTH(CARRTURN-KLGTH)                       44720003
447300                  RESP(WS-RESPONSE)                               44730003
447400        END-EXEC                                                  44740003
447500        MOVE WS-RESPONSE         TO FILE-STATUS                   44750003
447600     END-IF                                                       44760003
447700     IF SUCCESS AND CARRIED-DCAN-PLACEMENT                        44770003
447800*                                                                 44780003
447900*    LOOKS LIKE WE GOT ONE.  HIS TURN HAS BEEN DCAN'D SO SEE      44790003
448000*    IF IT HAS BEEN CALLED YET.                                   44800003
448100*                                                                 44810003
448200       IF CARRIED-DCAN-CALLED-FLAG NOT = 'C'                      44820003
448300*                                                                 44830003
448400*    IT HASN'T BEEN CALLED SO LET'S MAKE SURE NO ONE ELSE IS      44840003
448500*    ON HIS TURN (THERE SHOULDN'T BE AN ON-DUTY RECORD FOR HIS    44850003
448600*    TURN).                                                       44860003
448700*                                                                 44870003
448800         MOVE P916-TURN-PARM     TO ASGN-ASSIGNMENT               44880003
448900         SET ASGN-UFP-JOB        TO TRUE                          44890003
449000         PERFORM PXXXX-ON-DUTY-EMP                                44900003
449100         IF ASGN-EMP-NO NOT > ZEROES                              44910003
449200*                                                                 44920003
449300*    OK.  THERE'S NO ON-DUTY RECORD FOR THIS GUY'S TURN SO LET'S  44930003
449400*    PUT HIM BACK ON HIS TURN.                                    44940003
449500*                                                                 44950003
449600           MOVE SPACES                TO WS-ASGN-FILE             44960003
449700           MOVE 'U'                   TO ASGN-JOB-TYPE            44970003
449800           MOVE P916-TURN-PARM        TO ASGN-ASSIGNMENT          44980003
449900           MOVE P917-EMP-NO           TO ASGN-EMP-NO              44990003
450000           MOVE '3'                   TO ASGN-REC-TYPE            45000003
450100                                         ASGN-EMP-NO-REC-TYPE     45010003
450200           MOVE ZEROS                 TO ASGN-DATE-TIME           45020003
450300                                         ASGN-EMP-DATE-TIME       45030003
450400           MOVE WS-LOCAL-DATE-TIME    TO ASGN-ON-DUTY-DATE-TIME   45040003
450500           MOVE ASGNKEY1              TO ASGNJOB                  45050003
450600           EXEC CICS WRITE                                        45060003
450700                     DATASET(ASGN-VIA-ASGNJOB)                    45070003
450800                     FROM(WS-ASGN-FILE)                           45080003
450900                     LENGTH(ASGNJOB-RLGTH)                        45090003
451000                     RIDFLD(ASGNJOB)                              45100003
451100                     RESP(WS-RESPONSE)                            45110003
451200           END-EXEC                                               45120003
451300           MOVE WS-RESPONSE           TO FILE-STATUS              45130003
451400           IF NOT SUCCESS                                         45140003
451500             MOVE 'P2300-2'           TO ERR-PARAGRAPH            45150003
451600             MOVE ASGNJOB             TO ERR-KEY                  45160003
451700             PERFORM P9999-GOT-PROBLEM                            45170003
451800           END-IF                                                 45180003
451900*                                                                 45190003
452000*    NOW LET'S CUT A DCAN HISTORY RECORD TO SHOW THAT HE GOT      45200003
452100*    DCAN'D AS SOON AS HE MARKED UP.                              45210003
452200*                                                                 45220003
452300           MOVE SPACES                TO P943-COMMAREA-PARMS      45230003
452400           MOVE P917-EMP-NO           TO P943-EMP-NBR             45240003
452500           MOVE WS-EFF-DATE-TIME      TO P943-EFF-DATE-TIME       45250003
452600           MOVE WS-TIME-ZONE          TO P943-EMP-TIME-ZONE       45260003
452700           MOVE DIST OF WS-MSTR       TO P943-DIST                45270003
452800           MOVE SUB-DIST OF WS-MSTR   TO P943-SDIST               45280003
452900           MOVE CRAFT OF WS-MSTR      TO P943-CRAFT               45290003
453000           MOVE P916-TURN-PARM        TO P943-NORM-ASGN           45300003
453100           MOVE LAYOFF-CODE OF WS-MSTR TO P943-LO                 45310003
453200           SET  P943-DCAN-TURN-FUN     TO TRUE                    45320003
453300           MOVE CARRIED-TURN-DCAN-FROM-KEY                        45330003
453400                                      TO P943-FUN59-FROM-ASGN     45340003
453500           MOVE CARRIED-TURN-DCAN-TO-KEY                          45350003
453600                                      TO P943-FUN59-TO-ASGN       45360003
453700           MOVE CARRIED-TURN-POOL     TO P943-POOL-ASG            45370003
453800           MOVE CARRIED-DCAN-TO-TERM  TO P943-IN-OUT              45380003
453900           PERFORM P8900-WRITE-HISTORY                            45390003
454000         END-IF                                                   45400003
454100       END-IF                                                     45410003
454200     END-IF.                                                      45420003
454300*                                                                 45430003
454400 P2400-LAYOFF-UFP.                                                45440003
454500*                                                                 45450003
454600     MOVE SPACES TO P915-COMMAREA-PARMS                           45460003
454700     SET P915-LAYOFF-FUNCTION TO TRUE                             45470003
454800     IF TEMP-ASGN-UFP                                             45480003
454900        MOVE TEMPORARY-ASGNMT TO P915-TURN-PARM                   45490003
455000     ELSE                                                         45500003
455100        MOVE NORMAL-ASGNMT    TO P915-TURN-PARM                   45510003
455200     END-IF                                                       45520003
455300     MOVE P917-STATUS-CODE1   TO P915-STATUS-CODE                 45530003
455400     MOVE P917-EDB-CODE       TO P915-REASON-CODE                 45540003
455500     MOVE P917-EFF-DATE-TIME  TO P915-EFF-DATE-TIME               45550003
455600     MOVE P917-TIME-ZONE      TO P915-TIME-ZONE                   45560003
455700     EXEC CICS LINK                                               45570003
455800               PROGRAM(P915-PGM)                                  45580003
455900               COMMAREA(P915-COMMAREA-PARMS)                      45590003
456000               LENGTH(P915-LGTH)                                  45600003
456100               RESP(WS-RESPONSE)                                  45610003
456200     END-EXEC                                                     45620003
456300     MOVE WS-RESPONSE TO FILE-STATUS                              45630003
456400     IF NOT SUCCESS                                               45640003
456500        MOVE 'P2400-1' TO ERR-PARAGRAPH                           45650003
456600        PERFORM P9999-GOT-PROBLEM                                 45660003
456700     END-IF.                                                      45670003
456800*                                                                 45680003
456900 P2500-RELEASE-EMPS.                                              45690003
457000*                                                                 45700003
457100     IF SAVE-TEMP-ASGN-KEY > SPACE                                45710003
457200        MOVE SAVE-TEMP-ASGN-KEY TO XXXX-ASGNKEY1                  45720003
457300     ELSE                                                         45730003
457400        IF SAVE-NORM-ASGN-KEY > SPACE                             45740003
457500           MOVE SAVE-NORM-ASGN-KEY TO XXXX-ASGNKEY1               45750003
457600        END-IF                                                    45760003
457700     END-IF                                                       45770003
457800     ADD 1                    TO XX-ASGN-DATE-TIME                45780003
457900     IF XXXX-ASGNKEY1 > SPACE                                     45790003
458000        MOVE '0' TO WS-DONE-CODE                                  45800003
458100        PERFORM UNTIL DONE                                        45810003
458200           MOVE XXXX-ASGNKEY1 TO ASGNJOB                          45820003
458300           EXEC CICS READ                                         45830003
458400                     DATASET(ASGN-VIA-ASGNJOB)                    45840003
458500                     INTO(WS-ASGN-FILE)                           45850003
458600                     LENGTH(ASGNJOB-RLGTH)                        45860003
458700                     RIDFLD(ASGNJOB)                              45870003
458800                     KEYLENGTH(ASGNJOB-KLGTH)                     45880003
458900                     GTEQ                                         45890003
459000                     RESP(WS-RESPONSE)                            45900003
459100           END-EXEC                                               45910003
459200           MOVE WS-RESPONSE TO FILE-STATUS                        45920003
459300           IF SUCCESS                                             45930003
459400              IF XX-ASGN-DIST = ASGN-DIST                         45940003
459500                 AND XX-ASGN-SUB-DIST = ASGN-SUB-DIST             45950003
459600                 AND XX-ASGN-JOB =                                45960003
459700                     ASGN-AJ-JOB OF ASGN-ASSIGNMENT               45970003
459800                 AND ASGN-TEMP-REC                                45980003
459900                 PERFORM P2510-DELETE-ASGNJOB                     45990003
460000              ELSE                                                46000003
460100                 SET DONE TO TRUE                                 46010003
460200              END-IF                                              46020003
460300           ELSE                                                   46030003
460400              SET DONE TO TRUE                                    46040003
460500              IF NOT (NO-RECORD-FND OR END-OF-FILE)               46050003
460600                 MOVE 'P2500-1' TO ERR-PARAGRAPH                  46060003
460700                 MOVE ASGNJOB   TO ERR-KEY                        46070003
460800                 PERFORM P9999-GOT-PROBLEM                        46080003
460900              END-IF                                              46090003
461000           END-IF                                                 46100003
461100        END-PERFORM                                               46110003
461200     END-IF.                                                      46120003
461300*                                                                 46130003
461400 P2510-DELETE-ASGNJOB.                                            46140003
461500*                                                                 46150003
461600     MOVE ASGNKEY1     TO ASGNJOB                                 46160003
461700     EXEC CICS DELETE                                             46170003
461800               DATASET(ASGN-VIA-ASGNJOB)                          46180003
461900               RIDFLD(ASGNJOB)                                    46190003
462000               RESP(WS-RESPONSE)                                  46200003
462100     END-EXEC                                                     46210003
462200     MOVE WS-RESPONSE TO FILE-STATUS                              46220003
462300     IF NOT SUCCESS                                               46230003
462400        MOVE 'P2510-1' TO ERR-PARAGRAPH                           46240003
462500        MOVE ASGNKEY2  TO ERR-KEY                                 46250003
462600        PERFORM P9999-GOT-PROBLEM                                 46260003
462700     END-IF                                                       46270003
462800                                                                  46280003
462900     MOVE SPACES                 TO P927-COMMAREA-PARMS           46290003
463000     SET P927-NOTIFY-FUNCTION    TO TRUE                          46300003
463100     MOVE SPACE                  TO WORK-CNTLKEY                  46310003
463200     MOVE '02'                   TO WK-CNTL-REC-TYPE              46320003
463300     MOVE ASGN-DIST              TO WK-CNTL-DIST                  46330003
463400     MOVE ASGN-SUB-DIST          TO WK-CNTL-SUB-DIST              46340003
463500     PERFORM P8400-READ-CNTLFILE                                  46350003
463600     IF NOT SUCCESS                                               46360003
463700        MOVE 'P2510-2'           TO ERR-PARAGRAPH                 46370003
463800        MOVE CNTLKEY             TO ERR-KEY                       46380003
463900        PERFORM P9999-GOT-PROBLEM                                 46390003
464000     END-IF                                                       46400003
464100     MOVE CNTL-TIME-ZONE         TO P927-TIME-ZONE                46410003
464200     MOVE SPACES                 TO TZ-PARAMETERS                 46420003
464300     MOVE P917-TIME-ZONE         TO TZ-IN-ZONE                    46430003
464400     MOVE P917-EFF-DATE-TIME     TO TZ-IN-DATE-TIME               46440003
464500     MOVE P927-TIME-ZONE         TO TZ-OUT-ZONE                   46450003
464600     PERFORM P8996-TIMEZONE                                       46460003
464700     MOVE TZ-OUT-DATE-TIME       TO P927-EFF-DATE-TIME            46470003
464800     MOVE ASGN-DIST              TO P927-DIST                     46480003
464900     MOVE ASGN-SUB-DIST          TO P927-SUB-DIST                 46490003
465000     MOVE ASGN-EMP-NO            TO P927-NTFY-EMP-NO              46500003
465100     MOVE ASGNKEY1               TO P927-NTFY-ASSIGNMENT-KEY      46510003
465200     MOVE P917-EMP-NO            TO P927-NTFY-DISP-BY             46520003
465300     MOVE 'T'                    TO P927-NTFY-MOVE-TYPE           46530003
465400     PERFORM P8800-WRITE-TASK                                     46540003
465500     MOVE ASGN-EMP-NO            TO MSTRNBRK                      46550003
465600                                                                  46560003
465700     EXEC CICS READ                                               46570003
465800               DATASET(MSTR-VIA-EMP-NBR)                          46580003
465900               INTO(WS-MSTR)                                      46590003
466000               LENGTH(MSTRENBR-RLGTH)                             46600003
466100               RIDFLD(MSTRNBRK)                                   46610003
466200               KEYLENGTH(MSTRENBR-KLGTH)                          46620003
466300               RESP(WS-RESPONSE)                                  46630003
466400     END-EXEC                                                     46640003
466500     MOVE WS-RESPONSE            TO FILE-STATUS                   46650003
466600     IF SUCCESS                                                   46660003
466700        MOVE SPACES              TO P943-COMMAREA-PARMS           46670003
466800        MOVE EMP-NBR OF WS-MSTR  TO P943-EMP-NBR                  46680003
466900        SET P943-DISPLACED-FUN   TO TRUE                          46690003
467000        IF DIST OF WS-MSTR = P927-DIST AND                        46700003
467100           SUB-DIST OF WS-MSTR = P927-SUB-DIST                    46710003
467200           MOVE P927-EFF-DATE-TIME TO P943-EFF-DATE-TIME          46720003
467300           MOVE P927-TIME-ZONE     TO P943-EMP-TIME-ZONE          46730003
467400        ELSE                                                      46740003
467500           MOVE SPACE               TO WORK-CNTLKEY               46750003
467600           MOVE '02'                TO WK-CNTL-REC-TYPE           46760003
467700           MOVE DIST OF WS-MSTR     TO WK-CNTL-DIST               46770003
467800           MOVE SUB-DIST OF WS-MSTR TO WK-CNTL-SUB-DIST           46780003
467900           PERFORM P8400-READ-CNTLFILE                            46790003
468000           IF NOT SUCCESS                                         46800003
468100              MOVE 'P2510-3'        TO ERR-PARAGRAPH              46810003
468200              MOVE CNTLKEY          TO ERR-KEY                    46820003
468300              PERFORM P9999-GOT-PROBLEM                           46830003
468400           END-IF                                                 46840003
468500           MOVE CNTL-TIME-ZONE      TO P943-EMP-TIME-ZONE         46850003
468600           MOVE SPACES              TO TZ-PARAMETERS              46860003
468700           MOVE P927-TIME-ZONE      TO TZ-IN-ZONE                 46870003
468800           MOVE P927-EFF-DATE-TIME  TO TZ-IN-DATE-TIME            46880003
468900           MOVE P943-EMP-TIME-ZONE  TO TZ-OUT-ZONE                46890003
469000           PERFORM P8996-TIMEZONE                                 46900003
469100           MOVE TZ-OUT-DATE-TIME    TO P943-EFF-DATE-TIME         46910003
469200        END-IF                                                    46920003
469300        MOVE DIST OF WS-MSTR     TO P943-DIST                     46930003
469400        MOVE SUB-DIST OF WS-MSTR TO P943-SDIST                    46940003
469500        MOVE CRAFT OF WS-MSTR    TO P943-CRAFT                    46950003
469600        MOVE LAYOFF-CODE         TO P943-LO                       46960003
469700        MOVE P917-EMP-NO         TO P943-EMP-NBR-AFFECTED         46970003
469800        MOVE ASGN-ASSIGNMENT     TO P943-NORM-ASGN                46980003
469900        MOVE P927-NTFY-MOVE-TYPE TO P943-MOVE-TYPE                46990003
470000*                                                                 47000003
470100*    SEQUENCE ERROR EXISTS IF THE EFFECTIVE TIME OF THE DISPLACE- 47010003
470200*    MENT PRECEDES THE PERSON'S ASSIGNMENT.                       47020003
470300*                                                                 47030003
470400        SET DE-YYMMDD-FORMAT      TO TRUE                         47040003
470500        MOVE P927-EFF-DATE        TO DE-YYMMDD                    47050003
470600        PERFORM P8998-DATEEDIT                                    47060003
470700        MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE             47070003
470800        MOVE P927-EFF-TIME        TO DE-COMPARE1-TIME             47080003
470900                                                                  47090003
471000        SET DE-YYMMDD-FORMAT      TO TRUE                         47100003
471100        MOVE ASGN-ASSIGNMENT-DATE TO DE-YYMMDD                    47110003
471200        PERFORM P8998-DATEEDIT                                    47120003
471300        MOVE DE-CCYYMMDD          TO DE-COMPARE2-DATE             47130003
471400        MOVE ASGN-ASSIGNMENT-TIME TO DE-COMPARE2-TIME             47140003
471500                                                                  47150003
471600*       IF P927-EFF-DATE-TIME    < ASGN-ASSIGNMENT-DATE-TIME      47160003
471700        IF DE-COMPARE1-DATE-TIME < DE-COMPARE2-DATE-TIME          47170003
471800           SET P943-SEQ-ERROR      TO TRUE                        47180003
471900        END-IF                                                    47190003
472000        PERFORM P8900-WRITE-HISTORY                               47200003
472100     ELSE                                                         47210003
472200        MOVE 'P2510-4'  TO ERR-PARAGRAPH                          47220003
472300        MOVE MSTRNBRK   TO ERR-KEY                                47230003
472400        PERFORM P9999-GOT-PROBLEM                                 47240003
472500     END-IF.                                                      47250003
472600*                                                                 47260003
472700 P2600-PERSONAL-LEAVE.                                            47270003
472800*                                                                 47280003
472900     MOVE SPACES                     TO P970-COMMAREA-PARMS       47290003
473000     SET P970-SYSTEM-VERSION         TO TRUE                      47300003
473100     SET P970-PERSONAL-FUNC          TO TRUE                      47310003
473200     SET P970-UPDATE                 TO TRUE                      47320003
473300     MOVE EMP-NBR IN WS-MSTR         TO P970-SYPR-EMP-NO          47330003
473400     MOVE '01'                       TO P970-SYPR-NO-DAYS         47340003
473500     IF EMP-TRHIST-KEY > SPACES                                   47350003
473600        PERFORM P2625-GET-LAST-JOB-WORKED                         47360003
473700        IF AHM-FOUND                                              47370003
473800           IF TEMPORARY-ASGNMT > SPACES                           47380003
473900              AND TEMP-ASGN-XB OR                                 47390003
474000              TEMPORARY-ASGNMT NOT > SPACES                       47400003
474100              AND NORM-ASGN-XB                                    47410003
474200              PERFORM P2630-DETERMINE-WHEN-WORKED                 47420003
474300              IF NOT WORKED-PRIOR-TO-PLD                          47430003
474400                 MOVE ZEROES            TO WS-AHM-DONE-CODE       47440003
474500              END-IF                                              47450003
474600           END-IF                                                 47460003
474700           IF AHM-FOUND                                           47470003
474800              IF AHM-POOL = JOB-DEF-YARD-POOL OR                  47480003
474900                            JOB-DEF-LOCAL-POOL                    47490003
475000                 MOVE 'A'               TO P970-SYPR-ASGN-TYPE    47500003
475100              ELSE                                                47510003
475200                 MOVE 'U'               TO P970-SYPR-ASGN-TYPE    47520003
475300              END-IF                                              47530003
475400              IF AHM-ODA-POOL = 'EX'                              47540003
475500                 PERFORM P2620-GET-REGULAR-ASGN                   47550003
475600                 MOVE AH-CALL-XREF-ID   TO P970-SYPR-ASSIGNMENT   47560003
475700                 MOVE AHM-CRAFT-CODE    TO P970-SYPR-ASGN-CC      47570003
475800              ELSE                                                47580003
475900                 MOVE AHM-OD-ASSIGN     TO P970-SYPR-ASSIGNMENT   47590003
476000              END-IF                                              47600003
476100           END-IF                                                 47610003
476200        END-IF                                                    47620003
476300     END-IF                                                       47630003
476400     IF P970-SYPR-ASSIGNMENT NOT > SPACES                         47640003
476500        IF TEMPORARY-ASGNMT > SPACES                              47650003
476600           MOVE TEMPORARY-ASGNMT        TO P970-SYPR-ASSIGNMENT   47660003
476700           MOVE TEMPORARY-ASGNMT-FLAG   TO P970-SYPR-ASGN-TYPE    47670003
476800        ELSE                                                      47680003
476900           IF NORMAL-ASGNMT > SPACES                              47690003
477000              MOVE NORMAL-ASGNMT        TO P970-SYPR-ASSIGNMENT   47700003
477100              MOVE NORMAL-ASGNMT-FLAG   TO P970-SYPR-ASGN-TYPE    47710003
477200           ELSE                                                   47720003
477300              MOVE DIST     OF WS-MSTR  TO P970-SYPR-ASGN-DIST    47730003
477400              MOVE SUB-DIST OF WS-MSTR  TO P970-SYPR-ASGN-SDIST   47740003
477500              MOVE CRAFT    OF WS-MSTR  TO P970-SYPR-ASGN-CC      47750003
477600           END-IF                                                 47760003
477700        END-IF                                                    47770003
477800     END-IF                                                       47780003
477900*    IF P970-SYPR-ASGN-TYPE NOT > SPACE                           47790003
478000**      (EMPLOYEE HAS NO CURRENT ASSIGNMENT)                      47800003
478100*       MOVE '319'                TO P970-SYPR-ROUTE-CODE(1)      47810003
478200*    END-IF                                                       47820003
478300*                                                                 47830003
478400*    IF THE EMPLOYEE IS BEING MARKED OFF ON VACATION AFTER 2000PM 47840003
478500*    CONSIDER IT TO BEGIN THE NEXT DAY                            47850003
478600*                                                                 47860003
478700     IF WS-EFF-TIME > 1959                                        47870003
478800        MOVE ZEROS                     TO DATE-CONVERSION-PARMS   47880003
478900        SET PARM-ADD                   TO TRUE                    47890003
479000        MOVE WS-EFF-DATE               TO PARM-PRI-DATE-GREG      47900003
479100        MOVE '000001'                  TO PARM-SEC-DATE-GREG      47910003
479200        EXEC CICS LINK                                            47920003
479300                  PROGRAM(P903-PGM)                               47930003
479400                  COMMAREA(DATE-CONVERSION-PARMS)                 47940003
479500                  LENGTH(P903-LGTH)                               47950003
479600                  RESP(WS-RESPONSE)                               47960003
479700        END-EXEC                                                  47970003
479800        MOVE WS-RESPONSE             TO FILE-STATUS               47980003
479900        IF NOT SUCCESS                                            47990003
480000           MOVE 'P2600-1'            TO ERR-PARAGRAPH             48000003
480100           PERFORM P9999-GOT-PROBLEM                              48010003
480200        END-IF                                                    48020003
480300        MOVE PARM-RES-DATE-GREG      TO P970-SYPR-START-DATE      48030003
480400     ELSE                                                         48040003
480500        MOVE WS-EFF-DATE             TO P970-SYPR-START-DATE      48050003
480600     END-IF                                                       48060003
480700                                                                  48070003
480800     SET P970-SYPR-PERSONAL-LEAVE    TO TRUE                      48080003
480900*                                                                 48090003
481000*    IF THE MISC. CLAIM'S 'TYPE CODE' = 1 (OFF OF THE 514 SCREEN),48100003
481100*    DONT CREATE THE TIMESLIP.  THIS IS FOR GTW EMPLOYEES.        48110003
481200*                                                                 48120003
481300     MOVE SPACES                     TO P983-COMMAREA-PARMS       48130003
481400     SET  P983-GET-MISC-VAL-CONTROL  TO TRUE                      48140003
481500     MOVE P970-SYPR-MC-CODE          TO P983-MISC-VAL-CLAIM-CODE  48150003
481600     MOVE P970-SYPR-ASGN-DIST        TO P983-MISC-VAL-DIST        48160003
481700     MOVE P970-SYPR-ASGN-SDIST       TO P983-MISC-VAL-SDIST       48170003
481800     SET  P983-MISC-VAL-RETURN-REC   TO TRUE                      48180003
481900     EXEC CICS LINK                                               48190003
482000               PROGRAM(P983-PGM)                                  48200003
482100               COMMAREA(P983-COMMAREA-PARMS)                      48210003
482200               LENGTH(P983-LGTH)                                  48220003
482300               RESP(WS-RESPONSE)                                  48230003
482400     END-EXEC                                                     48240003
482500     MOVE WS-RESPONSE                TO FILE-STATUS               48250003
482600     IF NOT SUCCESS                                               48260003
482700        MOVE 'P2600-2'               TO ERR-PARAGRAPH             48270003
482800        MOVE 'P983LINK'              TO ERR-KEY                   48280003
482900        PERFORM P9999-GOT-PROBLEM                                 48290003
483000     END-IF                                                       48300003
483100     IF P983-MISC-VAL-FOUND                                       48310003
483200        MOVE P983-MISC-VAL-RECORD    TO WS-TKCR-FILE              48320003
483300        IF TKCR-MISC-TYPE-CD            > SPACES                  48330003
483400           MOVE TKCR-MISC-TYPE-CD    TO TYPE-CODE-CHECK           48340003
483500           IF GTW-TYPE                                            48350003
483600              SET DONT-CREATE-TIMESLIP TO TRUE                    48360003
483700           ELSE                                                   48370003
483800              SET CREATE-TIMESLIP    TO TRUE                      48380003
483900           END-IF                                                 48390003
484000        ELSE                                                      48400003
484100           SET CREATE-TIMESLIP       TO TRUE                      48410003
484200        END-IF                                                    48420003
484300     ELSE                                                         48430003
484400        SET CREATE-TIMESLIP          TO TRUE                      48440003
484500     END-IF                                                       48450003
484600     IF CREATE-TIMESLIP                                           48460003
484700        PERFORM VARYING J FROM 1 BY 1 UNTIL J > P917-PAID-DAYS-NUM48470003
484800           IF J > 1                                               48480003
484900              MOVE ZEROS                TO DATE-CONVERSION-PARMS  48490003
485000              SET PARM-ADD              TO TRUE                   48500003
485100              MOVE P970-SYPR-START-DATE TO PARM-PRI-DATE-GREG     48510003
485200              MOVE WS-EFF-TIME          TO PARM-PRI-HRMN          48520003
485300              MOVE 1                    TO PARM-SEC-DATE-JULIAN   48530003
485400              EXEC CICS LINK                                      48540003
485500                        PROGRAM(P903-PGM)                         48550003
485600                        COMMAREA(DATE-CONVERSION-PARMS)           48560003
485700                        LENGTH(P903-LGTH)                         48570003
485800                        RESP(WS-RESPONSE)                         48580003
485900              END-EXEC                                            48590003
486000              MOVE WS-RESPONSE          TO FILE-STATUS            48600003
486100              IF NOT SUCCESS                                      48610003
486200                 MOVE 'P2600-3'         TO ERR-PARAGRAPH          48620003
486300                 PERFORM P9999-GOT-PROBLEM                        48630003
486400              END-IF                                              48640003
486500              MOVE PARM-RES-DATE-GREG   TO P970-SYPR-START-DATE   48650003
486600           END-IF                                                 48660003
486700           PERFORM P8650-WRITE-TMSLIP                             48670003
486800        END-PERFORM                                               48680003
486900     END-IF.                                                      48690003
486910*                                                                 48691003
486911 P2605-BT-DAYOFF.                                                 48691103
486912*                                                                 48691203
486913******************************************************************48691303
486914* READ THE EMPLOYEE'S VACATION RECORD                             48691403
486915******************************************************************48691503
486916     MOVE SPACES                        TO VAC-KEY-1              48691603
486917     MOVE P917-EMP-NO                   TO VAC-K1-EMPLOYEE-NO     48691703
486918     MOVE DIST OF WS-MSTR               TO WS-VAC-DIST            48691803
486919     MOVE SUB-DIST OF WS-MSTR           TO WS-VAC-SUB-DIST        48691903
486920     MOVE WS-EFF-DATE(1:2)              TO VAC-K1-YEAR            48692003
486930     IF VAC-K1-YEAR                      < 90                     48693003
486931        MOVE '20'                       TO VAC-K1-CENT            48693103
486932     ELSE                                                         48693203
486933        MOVE '19'                       TO VAC-K1-CENT            48693303
486934     END-IF                                                       48693403
486935     MOVE VAC-KEY-1                     TO VACKEY1                48693503
486936     PERFORM P8000-READ-VACEMP-FILE                               48693603
486937     MOVE SPACES                     TO P970-COMMAREA-PARMS       48693703
486938                                        P970-EMP-STAND-ALONE-AREA 48693803
486939     SET P970-EMPLOYEE-VERSION       TO TRUE                      48693903
486940     SET P970-STAND-ALONE-FUNC       TO TRUE                      48694003
486941     SET P970-UPDATE                 TO TRUE                      48694103
486942     SET P970-EMSA-BT-DAYOFF         TO TRUE                      48694203
486943     SET P970-EMSA-BT-DEBIT          TO TRUE                      48694303
486944     MOVE P917-EMP-NO                TO P970-EMSA-EMP-NO          48694403
486945     MOVE P917-NBR-DAYS              TO P917-PAID-DAYS-NUM        48694503
486946                                        PIC-9-DATA-VALUE          48694603
486947     MOVE P917-BT-TOTAL-AMT          TO WORK-TOTAL-BANKTIME       48694703
486948     DIVIDE WORK-TOTAL-BANKTIME BY PIC-9-DATA-VALUE               48694803
486949              GIVING WORK-BASIC-DAY-RATE                          48694903
486950     MOVE WORK-BASIC-DAY-RATE        TO P970-EMSA-CLM-AMT-NUM     48695003
486951     MOVE '$'                        TO P970-EMSA-CLM-AMT-TYPE    48695103
486952     MOVE 'BK'                       TO P970-EMSA-CLM-CODE        48695203
486953     IF ON-DUTY-ASGNMT > SPACES                                   48695303
486955        MOVE ON-DUTY-ASGNMT          TO P970-EMSA-ASSIGNMENT      48695403
486956        MOVE ON-DUTY-ASGNMT-FLAG     TO P970-EMSA-ASGN-TYPE       48695503
486957     ELSE                                                         48695603
486958        IF TEMPORARY-ASGNMT > SPACES                              48695703
486959           MOVE TEMPORARY-ASGNMT     TO P970-EMSA-ASSIGNMENT      48695803
486960           MOVE TEMPORARY-ASGNMT-FLAG TO P970-EMSA-ASGN-TYPE      48695903
486961        ELSE                                                      48696003
486962           IF NORMAL-ASGNMT > SPACES                              48696103
486963              MOVE NORMAL-ASGNMT     TO P970-EMSA-ASSIGNMENT      48696203
486964              MOVE NORMAL-ASGNMT-FLAG TO P970-EMSA-ASGN-TYPE      48696303
486965           ELSE                                                   48696403
486966              IF LAST-JOB-ASG OF WS-MSTR > SPACES                 48696503
486967                 MOVE LAST-JOB-ASG OF WS-MSTR                     48696603
486968                                      TO P970-EMSA-ASSIGNMENT     48696703
486969                 MOVE LAST-JOB-ASG-TYPE TO P970-EMSA-ASGN-TYPE    48696803
486970              END-IF                                              48696903
486971           END-IF                                                 48697003
486972        END-IF                                                    48697103
486973     END-IF                                                       48697203
486974     IF P970-EMSA-ASSIGNMENT > SPACES                             48697303
486975        PERFORM P2606-GET-RATES                                   48697403
486976        IF WORK-JOB-TYPE > SPACES                                 48697503
486980           MOVE WORK-JOB-TYPE        TO P970-EMSA-JOB-TYPE        48697603
486981        END-IF                                                    48697703
486982        IF WORK-RATE-CODE > SPACES                                48697803
486983           MOVE WORK-RATE-CODE       TO P970-EMSA-RATE-CODE       48697903
486984        END-IF                                                    48698003
486985     END-IF                                                       48698103
486986     IF P970-EMSA-ASSIGNMENT NOT > SPACES                         48698203
486987      OR P970-EMSA-JOB-TYPE  NOT > SPACES                         48698303
486988      OR P970-EMSA-RATE-CODE NOT > SPACES                         48698403
486989        SET P970-EMSA-WITH-NO-ASGN   TO TRUE                      48698503
486990        MOVE DIST OF WS-MSTR         TO P970-EMSA-ASGN-DIST       48698603
486991        MOVE SUB-DIST OF WS-MSTR     TO P970-EMSA-ASGN-SDIST      48698703
486992     END-IF                                                       48698803
486993*    MOVE P970-EMSA-ASGN-CC          TO P970-EMSA-CRAFT-CODE      48698903
486994     IF WS-EFF-TIME > 1959                                        48699003
486995        MOVE ZEROS                   TO DATE-CONVERSION-PARMS     48699103
486996        SET PARM-ADD                 TO TRUE                      48699203
486997        MOVE WS-EFF-DATE             TO PARM-PRI-DATE-GREG        48699303
486998        MOVE '000001'                TO PARM-SEC-DATE-GREG        48699403
486999        EXEC CICS LINK                                            48699503
487000                  PROGRAM(P903-PGM)                               48699603
487001                  COMMAREA(DATE-CONVERSION-PARMS)                 48699703
487002                  LENGTH(P903-LGTH)                               48699803
487003                  RESP(WS-RESPONSE)                               48699903
487004        END-EXEC                                                  48700003
487005        MOVE WS-RESPONSE             TO FILE-STATUS               48700103
487006        IF NOT SUCCESS                                            48700203
487007           MOVE 'P2605-1'            TO ERR-PARAGRAPH             48700303
487008           PERFORM P9999-GOT-PROBLEM                              48700403
487009        END-IF                                                    48700503
487010        MOVE PARM-RES-DATE-GREG      TO P970-EMSA-START-DATE      48700603
487011     ELSE                                                         48700703
487012        MOVE WS-EFF-DATE             TO P970-EMSA-START-DATE      48700803
487013     END-IF                                                       48700903
487014     IF VAC-DONT-BANK-STAT                                        48701003
487015        PERFORM P2607-CHECK-HOLIDAY                               48701103
487016     ELSE                                                         48701203
487017        PERFORM P2609-BUILD-TIMESLIP                              48701303
487018     END-IF.                                                      48701403
487019*                                                                 48701503
487020 P2606-GET-RATES.                                                 48701603
487030*                                                                 48701703
487040     MOVE SPACES                    TO WORK-JOB-TYPE              48701803
487050                                       WORK-RATE-CODE             48701903
487060                                       WORK-CNTLKEY               48702003
487070     IF P970-EMSA-ASGN-TYPE = 'U'                                 48703003
487080        MOVE P970-EMSA-ASGN-DIST    TO WK-CNTL-DIST               48704003
487090        MOVE P970-EMSA-ASGN-SDIST   TO WK-CNTL-SUB-DIST           48705003
487091        MOVE P970-EMSA-ASGN(1:2)    TO WK-CNTL-XB-CODE            48706003
487092        MOVE 'F'                    TO WK-CNTL-POOL-TYPE          48707003
487093        MOVE '03'                   TO WK-CNTL-REC-TYPE           48708003
487094        MOVE WORK-CNTLKEY           TO CNTLKEY                    48709003
487095        PERFORM P8400-READ-CNTLFILE                               48709103
487096        IF NOT SUCCESS                                            48709203
487097           IF NOT(NO-RECORD-FND OR END-OF-FILE)                   48709303
487098              MOVE 'P2606-1'            TO ERR-PARAGRAPH          48709403
487099              MOVE CNTLKEY              TO ERR-KEY                48709503
487100              PERFORM P9999-GOT-PROBLEM                           48709603
487101           END-IF                                                 48709703
487102        ELSE                                                      48709803
487103           MOVE CNTL-POOL-JOB-TYPE  TO WORK-JOB-TYPE              48709903
487104           MOVE CNTL-POOL-RATE-CODE TO WORK-RATE-CODE             48710003
487105        END-IF                                                    48710103
487106     ELSE                                                         48710203
487107        IF P970-EMSA-ASGN-TYPE = 'X'                              48710303
487108           MOVE P970-EMSA-ASGN-DIST TO WK-CNTL-DIST               48710403
487109           MOVE P970-EMSA-ASGN-SDIST                              48710503
487110                                    TO WK-CNTL-SUB-DIST           48710603
487120           MOVE P970-EMSA-ASGN-CC   TO WK-CNTL-XB-CC              48710703
487121           MOVE '08'                TO WK-CNTL-REC-TYPE           48710803
487122           MOVE WORK-CNTLKEY        TO CNTLKEY                    48710903
487123           PERFORM P8400-READ-CNTLFILE                            48711003
487124           IF NOT SUCCESS                                         48712003
487125              IF NOT(NO-RECORD-FND OR END-OF-FILE)                48712103
487126                 MOVE 'P2606-2'     TO ERR-PARAGRAPH              48712203
487127                 MOVE CNTLKEY       TO ERR-KEY                    48712303
487128                 PERFORM P9999-GOT-PROBLEM                        48712403
487129              END-IF                                              48712503
487130           ELSE                                                   48712603
487131              MOVE CNTL-XB-JOB-TYPE TO WORK-JOB-TYPE              48712703
487132              MOVE CNTL-XB-RATE-CODE                              48712803
487133                                    TO WORK-RATE-CODE             48712903
487134              MOVE CNTL-XB-ROSTER-CC TO P970-EMSA-CRAFT-CODE      48713003
487136           END-IF                                                 48713103
487137        ELSE                                                      48713203
487138           IF P970-EMSA-ASGN-TYPE = 'A'                           48713303
487139              MOVE SPACES           TO WS-TRCN-FILE               48713403
487140              MOVE P970-EMSA-ASGN-DIST                            48713503
487141                                    TO TRCN-DIST3                 48713603
487142              MOVE P970-EMSA-ASGN-SDIST                           48713703
487143                                    TO TRCN-SDIST3                48713803
487144              MOVE P970-EMSA-ASGN   TO TRCN-ASSIGNMENT            48713903
487145              MOVE TRCN-KEY3        TO TRCNKEY3                   48714003
487146              PERFORM P8530-READ-TRCN                             48714103
487147              IF SUCCESS                                          48714203
487148                 MOVE TRCN-JOB-TYPE TO WORK-JOB-TYPE              48714303
487149                 MOVE TRCN-RATE-CODE                              48714403
487150                                    TO WORK-RATE-CODE             48714503
487151              END-IF                                              48714603
487152           END-IF                                                 48714703
487153        END-IF                                                    48714803
487154     END-IF.                                                      48714903
487155*                                                                 48715003
487156 P2607-CHECK-HOLIDAY.                                             48715103
487157*                                                                 48715203
487158     MOVE ZEROES                        TO M                      48715303
487159     PERFORM VARYING L FROM 1 BY 1 UNTIL L > P917-PAID-DAYS-NUM   48715403
487160        SET WRITE-TIMESLIP              TO TRUE                   48715503
487161        IF L > 1                                                  48715603
487162           MOVE ZEROS                   TO DATE-CONVERSION-PARMS  48715703
487163           SET PARM-ADD                 TO TRUE                   48715803
487164           MOVE P970-EMSA-START-DATE    TO PARM-PRI-DATE-GREG     48715903
487165           MOVE WS-EFF-TIME             TO PARM-PRI-HRMN          48716003
487166           MOVE 1                       TO PARM-SEC-DATE-JULIAN   48716103
487167           EXEC CICS LINK                                         48716203
487168                     PROGRAM(P903-PGM)                            48716303
487169                     COMMAREA(DATE-CONVERSION-PARMS)              48716403
487170                     LENGTH(P903-LGTH)                            48716503
487171                     RESP(WS-RESPONSE)                            48716603
487172           END-EXEC                                               48716703
487173           MOVE WS-RESPONSE             TO FILE-STATUS            48716803
487174           IF NOT SUCCESS                                         48716903
487175              MOVE 'P2605-3'            TO ERR-PARAGRAPH          48717003
487176              PERFORM P9999-GOT-PROBLEM                           48717103
487177           END-IF                                                 48717203
487178           MOVE PARM-RES-DATE-GREG      TO P970-EMSA-START-DATE   48717303
487179        END-IF                                                    48717403
487180        PERFORM P2608-READ-HOLIDAY-CNTL                           48717503
487181        IF WRITE-TIMESLIP                                         48717603
487182           ADD 1                        TO M                      48717703
487183           MOVE P970-EMSA-START-DATE    TO WS-TS-EFF-DATE(M)      48717803
487184        ELSE                                                      48717903
487185           SUBTRACT WORK-BASIC-DAY-RATE FROM P917-BT-TOTAL-AMT-NUM48718003
487186        END-IF                                                    48718103
487187     END-PERFORM                                                  48718203
487188     PERFORM VARYING L FROM 1 BY 1 UNTIL L > M                    48718303
487189        IF WS-TS-EFF-DATE(L) > SPACES                             48718403
487190           MOVE WS-TS-EFF-DATE(L)       TO P970-EMSA-START-DATE   48718503
487191           ADD P970-EMSA-CLM-AMT-NUM    TO WORK-CURRENT-BANKTIME  48718603
487192           IF L = M                                               48718703
487193              MOVE P917-BT-TOTAL-AMT    TO WORK-TOTAL-BANKTIME-X  48718803
487194              PERFORM P2610-GET-TS-DIFF                           48718903
487195           END-IF                                                 48719003
487196           PERFORM P8650-WRITE-TMSLIP                             48719103
487197        END-IF                                                    48719203
487198     END-PERFORM                                                  48719303
487199     IF WORK-TOTAL-BANKTIME > ZEROES                              48719403
487200        PERFORM P9550-UPDATE-BT-TOTALS                            48719503
487201     END-IF.                                                      48719603
487202*                                                                 48719703
487203 P2608-READ-HOLIDAY-CNTL.                                         48719803
487204*                                                                 48719903
487205     MOVE SPACES                   TO WS-CNTL-FILE                48720003
487206     MOVE P970-EMSA-ASGN           TO JOB-DEF-CHECK               48720103
487207     IF JOB-DEF-YARD-ASGN                                         48720203
487208        MOVE 'Y'                   TO CNTL-YARD-ROAD              48720303
487209     ELSE                                                         48720403
487210        MOVE 'R'                   TO CNTL-YARD-ROAD              48720503
487211     END-IF                                                       48720603
487212     MOVE '20'                     TO CNTL-REC-TYPE               48720703
487213     MOVE P970-EMSA-START-DATE     TO WS-HOLIDAY-DATE             48720803
487214     MOVE WS-EFF-TIME              TO WS-HOLIDAY-TIME             48720903
487215     MOVE WS-HOLIDAY-MO            TO CNTL-HOLIDAY-MO             48721003
487216     MOVE WS-HOLIDAY-DY            TO CNTL-HOLIDAY-DY             48721103
487217     MOVE WS-HOLIDAY-YR            TO CNTL-HOLIDAY-YR             48721203
487218     MOVE P970-EMSA-ASGN-DIST      TO CNTL-DIST                   48721303
487219     MOVE P970-EMSA-ASGN-SDIST     TO CNTL-SUB-DIST               48721403
487220     MOVE CNTLKEY-AREA             TO WORK-CNTLKEY                48721503
487221     PERFORM P8400-READ-CNTLFILE                                  48721603
487222     IF SUCCESS                                                   48721703
487223        IF WS-HOLIDAY-TIME < CNTL-HOLIDAY-FROM                    48721803
487224         OR WS-HOLIDAY-TIME > CNTL-HOLIDAY-TO                     48721903
487225           CONTINUE                                               48722003
487226        ELSE                                                      48722103
487227           SET DONT-WRITE-TIMESLIP TO TRUE                        48722203
487228        END-IF                                                    48722303
487229     ELSE                                                         48722403
487230        IF NOT NO-RECORD-FND                                      48722503
487231           MOVE 'P2608-1' TO ERR-PARAGRAPH                        48722603
487232           MOVE CNTLKEY    TO ERR-KEY                             48722703
487233           PERFORM P9999-GOT-PROBLEM                              48722803
487234        END-IF                                                    48722903
487235     END-IF.                                                      48723003
487236*                                                                 48723103
487237 P2609-BUILD-TIMESLIP.                                            48723203
487238*                                                                 48723303
487239     PERFORM VARYING L FROM 1 BY 1 UNTIL L > P917-PAID-DAYS-NUM   48723403
487240        IF L > 1                                                  48723503
487241           MOVE ZEROS                   TO DATE-CONVERSION-PARMS  48723603
487242           SET PARM-ADD                 TO TRUE                   48723703
487243           MOVE P970-EMSA-START-DATE    TO PARM-PRI-DATE-GREG     48723803
487244           MOVE WS-EFF-TIME             TO PARM-PRI-HRMN          48723903
487245           MOVE 1                       TO PARM-SEC-DATE-JULIAN   48724003
487246           EXEC CICS LINK                                         48724103
487247                     PROGRAM(P903-PGM)                            48724203
487248                     COMMAREA(DATE-CONVERSION-PARMS)              48724303
487249                     LENGTH(P903-LGTH)                            48724403
487250                     RESP(WS-RESPONSE)                            48724503
487251           END-EXEC                                               48724603
487252           MOVE WS-RESPONSE             TO FILE-STATUS            48724703
487253           IF NOT SUCCESS                                         48724803
487254              MOVE 'P2605-3'            TO ERR-PARAGRAPH          48724903
487255              PERFORM P9999-GOT-PROBLEM                           48725003
487256           END-IF                                                 48725103
487257           MOVE PARM-RES-DATE-GREG      TO P970-EMSA-START-DATE   48725203
487258        END-IF                                                    48725303
487259        ADD P970-EMSA-CLM-AMT-NUM       TO WORK-CURRENT-BANKTIME  48725403
487260        IF L = P917-PAID-DAYS-NUM                                 48725503
487261           MOVE P917-BT-TOTAL-AMT       TO WORK-TOTAL-BANKTIME-X  48725603
487262           PERFORM P2610-GET-TS-DIFF                              48725703
487263        END-IF                                                    48725803
487264        PERFORM P8650-WRITE-TMSLIP                                48725903
487265     END-PERFORM                                                  48726003
487266     PERFORM P9550-UPDATE-BT-TOTALS.                              48726103
487267*                                                                 48726203
487268 P2610-GET-TS-DIFF.                                               48726303
487269*                                                                 48726403
487270     EVALUATE TRUE                                                48726503
487271        WHEN WORK-CURRENT-BANKTIME = WORK-TOTAL-BANKTIME          48726603
487272           CONTINUE                                               48726703
487273        WHEN WORK-CURRENT-BANKTIME < WORK-TOTAL-BANKTIME          48726803
487274           SUBTRACT WORK-CURRENT-BANKTIME                         48726903
487275               FROM WORK-TOTAL-BANKTIME                           48727003
487276             GIVING WORK-BANKTIME-DIFF                            48727103
487277                ADD WORK-BANKTIME-DIFF TO P970-EMSA-CLM-AMT-NUM   48727203
487278        WHEN WORK-CURRENT-BANKTIME > WORK-TOTAL-BANKTIME          48727303
487279           SUBTRACT WORK-TOTAL-BANKTIME                           48727403
487280               FROM WORK-CURRENT-BANKTIME                         48727503
487281             GIVING WORK-BANKTIME-DIFF                            48727603
487282           SUBTRACT WORK-BANKTIME-DIFF FROM P970-EMSA-CLM-AMT-NUM 48727703
487283     END-EVALUATE.                                                48727803
487284*                                                                 48727903
487285 P2620-GET-REGULAR-ASGN.                                          48728003
487286*                                                                 48728103
487287     MOVE SPACES                        TO WS-AHIST               48728203
487288     MOVE AHM-AHIST-KEY                 TO AH-KEY-1               48728303
487289                                           AH1KEY                 48728403
487290     MOVE AH-FILE-VIA-K1                TO AH-DATASET             48728503
487291     EXEC CICS READ                                               48728603
487292               DATASET(AH-DATASET)                                48728703
487293               INTO(WS-AHIST)                                     48728803
487294               LENGTH(AH1-RLGTH)                                  48728903
487295               RIDFLD(AH1KEY)                                     48729003
487296               KEYLENGTH(AH1-KLGTH)                               48729103
487297               RESP(WS-RESPONSE)                                  48729203
487298     END-EXEC                                                     48729303
487299     MOVE WS-RESPONSE                   TO FILE-STATUS            48729403
487300     IF NOT SUCCESS                                               48729503
487301        MOVE 'P2620-1'                  TO ERR-PARAGRAPH          48729603
487302        MOVE AH1KEY                     TO ERR-KEY                48729703
487303        PERFORM P9999-GOT-PROBLEM                                 48729803
487304     END-IF                                                       48729903
487305     .                                                            48730003
487306*                                                                 48730103
487307 P2625-GET-LAST-JOB-WORKED.                                       48730203
487308*                                                                 48730303
487309     MOVE SPACES                     TO WS-AHIST-MISC             48730403
487310     MOVE ZERO                       TO WS-AHM-DONE-CODE          48730503
487311     SET AHM-EMPLOYEE-RECORD         TO TRUE                      48730603
487312     MOVE AHM-FILE                   TO AHM-DATASET               48730703
487313     MOVE EMP-TRHIST-KEY             TO AHM-KEY                   48730803
487314     MOVE AHM-KEY                    TO AHMKEY                    48730903
487315     EXEC CICS STARTBR                                            48731003
487316               DATASET(AHM-DATASET)                               48731103
487317               RIDFLD(AHMKEY)                                     48731203
487318               GTEQ                                               48731303
487319               RESP(WS-RESPONSE)                                  48731403
487320     END-EXEC                                                     48731503
487321     MOVE WS-RESPONSE                TO FILE-STATUS               48731603
487322     IF SUCCESS                                                   48731703
487323        PERFORM UNTIL AHM-DONE OR AHM-FOUND                       48731803
487324           EXEC CICS READNEXT                                     48731903
487325                     DATASET(AHM-DATASET)                         48732003
487326                     INTO(WS-AHIST-MISC)                          48732103
487327                     LENGTH(AHM-RLGTH)                            48732203
487328                     RIDFLD(AHMKEY)                               48732303
487329                     KEYLENGTH(AHM-KLGTH)                         48732403
487330                     RESP(WS-RESPONSE)                            48732503
487331           END-EXEC                                               48732603
487332           MOVE WS-RESPONSE          TO FILE-STATUS               48732703
487340           IF SUCCESS                                             48732803
487350              IF AHM-DIST       = EMP-TRHIST-KEY(1:2)  AND        48732903
487360                 AHM-SUB-DIST   = EMP-TRHIST-KEY(3:2)  AND        48733003
487370                 AHM-POOL       = EMP-TRHIST-KEY(5:2)  AND        48734003
487380                 AHM-EFF-DATE-TIME = EMP-TRHIST-KEY(7:10) AND     48735003
487390                 AHM-TRAIN      = EMP-TRHIST-KEY(17:10)   AND     48736003
487400                 AHM-EMPLOYEE-NUMBER = EMP-NBR OF WS-MSTR         48737003
487500                 SET AHM-FOUND    TO TRUE                         48738003
487600              ELSE                                                48739003
487700                 IF AHM-DIST   NOT = EMP-TRHIST-KEY(1:2)  OR      48740003
487800                    AHM-SUB-DIST  NOT = EMP-TRHIST-KEY(3:2)  OR   48750003
487900                    AHM-POOL   NOT = EMP-TRHIST-KEY(5:2)          48760003
488000                    SET AHM-DONE        TO TRUE                   48770003
488100                 END-IF                                           48780003
488200              END-IF                                              48790003
488300           ELSE                                                   48800003
488400              SET AHM-DONE        TO TRUE                         48810003
488500           END-IF                                                 48820003
488600        END-PERFORM                                               48830003
488700     END-IF                                                       48840003
488800     EXEC CICS ENDBR                                              48850003
488900               DATASET(AHM-DATASET)                               48860003
489000               RESP(WS-RESPONSE)                                  48870003
489100     END-EXEC.                                                    48880003
489200*                                                                 48890003
489300 P2630-DETERMINE-WHEN-WORKED.                                     48900003
489400*                                                                 48910003
489500     MOVE ZEROES             TO DATE-CONVERSION-PARMS             48920003
489600                                WS-WHEN-WORKED-AREA               48930003
489700     SET PARM-DIFF           TO TRUE                              48940003
489800     MOVE LAYOFF-DATE        TO PARM-PRI-DATE-GREG                48950003
489900     MOVE '0001'             TO PARM-PRI-HRMN                     48960003
490000     MOVE AHM-EFF-DATE       TO PARM-SEC-DATE-GREG                48970003
490100     MOVE '0001'             TO PARM-SEC-HRMN                     48980003
490200     EXEC CICS LINK                                               48990003
490300               PROGRAM(P903-PGM)                                  49000003
490400               COMMAREA(DATE-CONVERSION-PARMS)                    49010003
490500               LENGTH(P903-LGTH)                                  49020003
490600               RESP(WS-RESPONSE)                                  49030003
490700     END-EXEC                                                     49040003
490800     MOVE WS-RESPONSE TO FILE-STATUS                              49050003
490900     IF NOT SUCCESS                                               49060003
491000        MOVE 'P2630-7' TO ERR-PARAGRAPH                           49070003
491100        PERFORM P9999-GOT-PROBLEM                                 49080003
491200     END-IF                                                       49090003
491300     MOVE PARM-RES-TOT-DAYS  TO WS-WHEN-WORKED-AREA.              49100003
491400*                                                                 49110003
491500*P2600-SYSTEM-AVAIL-LIST.                                         49120003
491600*    MOVE P917-EMP-NO TO MSTRNBRK                                 49130003
491700*    EXEC CICS READ                                               49140003
491800*              DATASET(MSTR-VIA-EMP-NBR)                          49150003
491900*              INTO(WS-MSTR)                                      49160003
492000*              LENGTH(MSTRENBR-RLGTH)                             49170003
492100*              RIDFLD(MSTRNBRK)                                   49180003
492200*              KEYLENGTH(MSTRENBR-KLGTH)                          49190003
492300*              RESP(WS-RESPONSE)                                  49200003
492400*    END-EXEC                                                     49210003
492500*    MOVE WS-RESPONSE TO FILE-STATUS                              49220003
492600*    IF NOT SUCCESS                                               49230003
492700*       MOVE 'P2600-1' TO ERR-PARAGRAPH                           49240003
492800*       MOVE MSTRNBRK  TO ERR-KEY                                 49250003
492900*       PERFORM P9999-GOT-PROBLEM                                 49260003
493000*    END-IF                                                       49270003
493100*                                                                 49280003
493200*    SET  SYSTEM-LIST-ID      TO TRUE                             49290003
493300*    MOVE SPACE               TO WS-AVAIL-LIST                    49300003
493400*    MOVE DIST     OF WS-MSTR TO AL-DIST                          49310003
493500*    MOVE SUB-DIST OF WS-MSTR TO AL-SUB-DIST                      49320003
493600*    MOVE WS-LIST-ID          TO AL-SVC                           49330003
493700*    MOVE ZERO                TO AL-DAY, AL-SHIFT                 49340003
493800*    MOVE P917-EMP-NO         TO AL-SSN                           49350003
493900*    MOVE P917-EFF-DATE-TIME  TO AL-DATE-TIME                     49360003
494000*    MOVE ZERO                TO AL-SC                            49370003
494100*                                AL-MS                            49380003
494200*                                AL-OLD-DATE-TIME                 49390003
494300*    MOVE AVAIL-LIST-KEY1     TO ALEMPK                           49400003
494400*                                                                 49410003
494500*    MOVE LAYOFF-CODE-1       TO WS-LAYOFF-CODE-CHECK             49420003
494600*    EVALUATE TRUE                                                49430003
494700*       WHEN LOC-AVAILABLE                                        49440003
494800*          PERFORM P2620-SYSTEM-AVAIL-LIST-DEL                    49450003
494900*       WHEN LOC-WORKING                                          49460003
495000*          PERFORM P2620-SYSTEM-AVAIL-LIST-DEL                    49470003
495100*       WHEN VALID-UNAVAIL-CODE                                   49480003
495200*          PERFORM P2610-SYSTEM-AVAIL-LIST-ADD                    49490003
495300*    END-EVALUATE.                                                49500003
495400*                                                                 49510003
495500*P2610-SYSTEM-AVAIL-LIST-ADD.                                     49520003
495600*                                                                 49530003
495700*    PERFORM WITH TEST AFTER VARYING LO-TABLE-SUB                 49540003
495800*       FROM 1 BY 1    UNTIL LO-TABLE-SUB > LO-ARRAY-MAX          49550003
495900*        IF WS-LO-CODE(LO-TABLE-SUB) = LAYOFF-CODE-1              49560003
496000*           IF LO-ADD-TO-SYSTEM-AVAIL-LIST(LO-TABLE-SUB)          49570003
496100*              PERFORM P2615-SYSTEM-AVAIL-LIST-ADD                49580003
496200*           ELSE                                                  49590003
496300*              PERFORM P2620-SYSTEM-AVAIL-LIST-DEL                49600003
496400*           END-IF                                                49610003
496500*           COMPUTE LO-TABLE-SUB = LO-ARRAY-MAX + 1               49620003
496600*        END-IF                                                   49630003
496700*    END-PERFORM.                                                 49640003
496800*                                                                 49650003
496900*P2615-SYSTEM-AVAIL-LIST-ADD.                                     49660003
497000*                                                                 49670003
497100*    EXEC CICS WRITE                                              49680003
497200*              DATASET(AL-FILE-VIA-ALEMPK)                        49690003
497300*              FROM(WS-AVAIL-LIST)                                49700003
497400*              LENGTH(AVLLIST1-RLGTH)                             49710003
497500*              KEYLENGTH(AVLLIST1-KLGTH)                          49720003
497600*              RIDFLD(ALEMPK)                                     49730003
497700*              RESP(WS-RESPONSE)                                  49740003
497800*    END-EXEC                                                     49750003
497900*    MOVE WS-RESPONSE TO FILE-STATUS                              49760003
498000*    IF NOT SUCCESS                                               49770003
498100*       IF DUP-KEY-ERR                                            49780003
498200*          PERFORM P2615-REWRITE-AVAIL-LIST                       49790003
498300*       ELSE                                                      49800003
498400*          MOVE 'P2610-1' TO ERR-PARAGRAPH                        49810003
498500*          MOVE ALEMPK    TO ERR-KEY                              49820003
498600*          PERFORM P9999-GOT-PROBLEM                              49830003
498700*       END-IF                                                    49840003
498800*    END-IF.                                                      49850003
498900*                                                                 49860003
499000*P2615-REWRITE-AVAIL-LIST.                                        49870003
499100*                                                                 49880003
499200*    MOVE AVAIL-LIST-KEY1 TO ALEMPK                               49890003
499300*    EXEC CICS READ                                               49900003
499400*              UPDATE                                             49910003
499500*              DATASET(AL-FILE-VIA-ALEMPK)                        49920003
499600*              INTO(WS-AVAIL-LIST)                                49930003
499700*              LENGTH(AVLLIST1-RLGTH)                             49940003
499800*              RIDFLD(ALEMPK)                                     49950003
499900*              KEYLENGTH(AVLLIST1-KLGTH)                          49960003
500000*              RESP(WS-RESPONSE)                                  49970003
500100*    END-EXEC                                                     49980003
500200*    MOVE WS-RESPONSE TO FILE-STATUS                              49990003
500300*    IF NOT SUCCESS                                               50000003
500400*       MOVE 'P2615-1' TO ERR-PARAGRAPH                           50010003
500500*       MOVE ALEMPK    TO ERR-KEY                                 50020003
500600*       PERFORM P9999-GOT-PROBLEM                                 50030003
500700*    END-IF                                                       50040003
500800*    MOVE AL-DATE-TIME         TO AL-OLD-DATE-TIME                50050003
500900*    MOVE P917-EFF-DATE-TIME   TO AL-DATE-TIME                    50060003
501000*    EXEC CICS REWRITE                                            50070003
501100*              DATASET(AL-FILE-VIA-ALEMPK)                        50080003
501200*              FROM(WS-AVAIL-LIST)                                50090003
501300*              LENGTH(AVLLIST1-RLGTH)                             50100003
501400*              RESP(WS-RESPONSE)                                  50110003
501500*    END-EXEC                                                     50120003
501600*    MOVE WS-RESPONSE TO FILE-STATUS                              50130003
501700*    IF NOT SUCCESS                                               50140003
501800*       MOVE 'P2615-2' TO ERR-PARAGRAPH                           50150003
501900*       MOVE ALEMPK    TO ERR-KEY                                 50160003
502000*       PERFORM P9999-GOT-PROBLEM                                 50170003
502100*    END-IF.                                                      50180003
502200*                                                                 50190003
502300*P2620-SYSTEM-AVAIL-LIST-DEL.                                     50200003
502400*                                                                 50210003
502500*    EXEC CICS DELETE                                             50220003
502600*              DATASET(AL-FILE-VIA-ALEMPK)                        50230003
502700*              RIDFLD(ALEMPK)                                     50240003
502800*              RESP(WS-RESPONSE)                                  50250003
502900*    END-EXEC                                                     50260003
503000*    MOVE WS-RESPONSE        TO FILE-STATUS                       50270003
503100*    IF NOT SUCCESS AND                                           50280003
503200*       NOT NO-RECORD-FND                                         50290003
503300*       MOVE 'P2620-1'       TO ERR-PARAGRAPH                     50300003
503400*       MOVE ALEMPK          TO ERR-KEY                           50310003
503500*       PERFORM P9999-GOT-PROBLEM                                 50320003
503600*    END-IF.                                                      50330003
503700*                                                                 50340003
503800 P2650-POSITION-TURN.                                             50350003
503900*                                                                 50360003
504000*    GET THE CURRENT OWNER OF THE TURN                            50370003
504100*                                                                 50380003
504200     MOVE SPACES                     TO XXXX-ASGNKEY1             50390003
504300     SET XX-ASGN-JOB-XB              TO TRUE                      50400003
504400     MOVE ASGN-DIST                  TO XX-ASGN-DIST              50410003
504500     MOVE ASGN-SUB-DIST              TO XX-ASGN-SUB-DIST          50420003
504600     MOVE ASGN-XB-JOB OF ASGN-ASSIGNMENT                          50430003
504700                                     TO XX-ASGN-JOB               50440003
504800     PERFORM PXXXX-LATEST-TEMP                                    50450003
504900     IF ASGN-EMP-NO NOT > ZERO                                    50460003
505000        PERFORM PXXXX-JOB-OWNER                                   50470003
505100     END-IF                                                       50480003
505200*                                                                 50490003
505300*    IF THE CURRENT OWNER IS A MORE RECENT TEMP THAN THE GUY      50500003
505400*    WE JUST REMOVED FROM THIS ASSIGNMENT, DON'T TOUCH THE TURN.  50510003
505500*                                                                 50520003
505600     MOVE SPACES                    TO DE-COMPARE3-DATE-TIME      50530003
505700     MOVE SPACES                    TO DE-COMPARE4-DATE-TIME      50540003
505800                                                                  50550003
505900     SET DE-YYMMDD-FORMAT           TO TRUE                       50560003
506000     MOVE ASGN-DATE-TIME(1:6)       TO DE-YYMMDD                  50570003
506100     PERFORM P8998-DATEEDIT                                       50580003
506200     MOVE DE-CCYYMMDD               TO DE-COMPARE3-DATE           50590003
506300     MOVE ASGN-DATE-TIME(7:4)       TO DE-COMPARE3-TIME           50600003
506400                                                                  50610003
506500     SET DE-YYMMDD-FORMAT           TO TRUE                       50620003
506600     MOVE REL-ASGN-DATE-TIME(1:6)   TO DE-YYMMDD                  50630003
506700     PERFORM P8998-DATEEDIT                                       50640003
506800     MOVE DE-CCYYMMDD               TO DE-COMPARE4-DATE           50650003
506900     MOVE REL-ASGN-DATE-TIME(7:4)   TO DE-COMPARE4-TIME           50660003
507000                                                                  50670003
507100     IF ASGN-EMP-NO > ZERO AND                                    50680003
507200        DE-COMPARE3-DATE-TIME > DE-COMPARE4-DATE-TIME             50690003
507300**      ASGN-DATE-TIME > REL-ASGN-DATE-TIME                       50700003
507400        CONTINUE                                                  50710003
507500     ELSE                                                         50720003
507600*                                                                 50730003
507700*    READ XB CNTL RECORD TO DETERMINE IF THE BOARD IS CONSISTED   50740003
507800*    OR NON-CONSISTED                                             50750003
507900*                                                                 50760003
508000        MOVE REL-ASGNKEY1            TO ASGNKEY1                  50770003
508100        MOVE SPACES                  TO WORK-CNTLKEY              50780003
508200        MOVE '08'                    TO WK-CNTL-REC-TYPE          50790003
508300        MOVE ASGN-DIST               TO WK-CNTL-DIST              50800003
508400        MOVE ASGN-SUB-DIST           TO WK-CNTL-SUB-DIST          50810003
508500        MOVE ASGN-XB-CC              TO WK-CNTL-XB-CODE           50820003
508600        MOVE WORK-CNTLKEY            TO CNTLKEY                   50830003
508700        PERFORM P8400-READ-CNTLFILE                               50840003
508800        IF NOT SUCCESS                                            50850003
508900           MOVE 'P2650-1'            TO ERR-PARAGRAPH             50860003
509000           MOVE CNTLKEY              TO ERR-KEY                   50870003
509100           PERFORM P9999-GOT-PROBLEM                              50880003
509200        END-IF                                                    50890003
509300*                                                                 50900003
509400*    READ THE XB TURN RECORD TO SEE IF THE TURN IS ON OR          50910003
509500*    OFF BOARD                                                    50920003
509600*                                                                 50930003
509700        MOVE SPACES                  TO EBTURN-AREA               50940003
509800        MOVE ASGN-DIST               TO DIST-REPEAT               50950003
509900        MOVE ASGN-SUB-DIST           TO SUBDIST-REPEAT            50960003
510000        MOVE ASGN-XB-TURN            TO TURN-NBR OF EBTURN-AREA   50970003
510100        MOVE ASGN-XB-CC              TO CRAFT-CODE-REPEAT         50980003
510200        MOVE EBTURN-AREA             TO EBTURN                    50990003
510300        EXEC CICS READ                                            51000003
510400                  DATASET(EB-VIA-TURN-NBR)                        51010003
510500                  INTO(WS-EXTRA-BOARD)                            51020003
510600                  LENGTH(EBTURNNO-RLGTH)                          51030003
510700                  RIDFLD(EBTURN)                                  51040003
510800                  KEYLENGTH(EBTURNNO-KLGTH)                       51050003
510900                  RESP(WS-RESPONSE)                               51060003
511000        END-EXEC                                                  51070003
511100        MOVE WS-RESPONSE             TO FILE-STATUS               51080003
511200        IF NOT SUCCESS                                            51090003
511300           MOVE 'P2650-2'            TO ERR-PARAGRAPH             51100003
511400           MOVE EBTURN               TO ERR-KEY                   51110003
511500           PERFORM P9999-GOT-PROBLEM                              51120003
511600        END-IF                                                    51130003
511700*                                                                 51140003
511800*    IF THE TURN IS NOW VACANT,                                   51150003
511900*    THE TURN SHOULD BE ON BOARD.  OTHERWISE, THE TURN            51160003
512000*    SHOULD BE OFF BOARD.                                         51170003
512100*                                                                 51180003
512200        IF ASGN-EMP-NO NOT > ZEROES                               51190003
512300         AND EB-OFF-BOARD                                         51200003
512400           PERFORM P2660-REALIGN-XB                               51210003
512500        ELSE                                                      51220003
512600           IF EB-ON-BOARD                                         51230003
512700              PERFORM P2670-SEN-MOVE-XB                           51240003
512800           END-IF                                                 51250003
512900        END-IF                                                    51260003
513000     END-IF                                                       51270003
513100                                                                  51280003
513200     MOVE REL-ASGNKEY1               TO ASGNKEY1                  51290003
513300     MOVE REL-ASGNKEY2               TO ASGNKEY2.                 51300003
513400*                                                                 51310003
513500 P2660-REALIGN-XB.                                                51320003
513600*                                                                 51330003
513700     MOVE SPACES                      TO P914-COMMAREA-PARMS      51340003
513800     SET P914-REALIGN-FUNCTION        TO TRUE                     51350003
513900     MOVE P956-ST-RSN-REL-FROM-ASGN   TO P914-MOVE-TYPE           51360003
514000     MOVE P917-EFF-DATE-TIME          TO P914-EFF-DATE-TIME       51370003
514100     MOVE XX-ASGN-DIST                TO P914-TURN-DIST           51380003
514200     MOVE XX-ASGN-SUB-DIST            TO P914-TURN-SUB-DIST       51390003
514300     MOVE XX-ASGN-JOB-CC              TO P914-TURN-CC             51400003
514400     MOVE XX-LAST-4-BYTES             TO P914-TURN                51410003
514500     MOVE P917-TIME-ZONE              TO P914-TIME-ZONE           51420003
514600                                                                  51430003
514700     EXEC CICS LINK                                               51440003
514800               PROGRAM(P914-PGM)                                  51450003
514900               COMMAREA(P914-COMMAREA-PARMS)                      51460003
515000               LENGTH(P914-LGTH)                                  51470003
515100               RESP(WS-RESPONSE)                                  51480003
515200     END-EXEC                                                     51490003
515300     MOVE WS-RESPONSE                TO FILE-STATUS               51500003
515400     IF NOT SUCCESS                                               51510003
515500        MOVE 'P2660-1'               TO ERR-PARAGRAPH             51520003
515600        PERFORM P9999-GOT-PROBLEM                                 51530003
515700     END-IF.                                                      51540003
515800*                                                                 51550003
515900 P2670-SEN-MOVE-XB.                                               51560003
516000*                                                                 51570003
516100     MOVE SPACES                     TO P913-COMMAREA-PARMS       51580003
516200     MOVE XX-ASGN-DIST               TO P913-TURN-DIST            51590003
516300     MOVE XX-ASGN-SUB-DIST           TO P913-TURN-SUB-DIST        51600003
516400     MOVE XX-ASGN-JOB-CC             TO P913-TURN-CC              51610003
516500     MOVE XX-LAST-4-BYTES            TO P913-TURN                 51620003
516600     SET P913-SEN-MOVE-FUNCTION      TO TRUE                      51630003
516700     MOVE P917-TIME-ZONE             TO P913-TIME-ZONE            51640003
516800     MOVE P917-EFF-DATE-TIME         TO P913-EFF-DATE-TIME        51650003
516900     EXEC CICS LINK                                               51660003
517000               PROGRAM(P913-PGM)                                  51670003
517100               COMMAREA(P913-COMMAREA-PARMS)                      51680003
517200               LENGTH(P913-LGTH)                                  51690003
517300               RESP(WS-RESPONSE)                                  51700003
517400     END-EXEC                                                     51710003
517500     MOVE WS-RESPONSE                TO FILE-STATUS               51720003
517600     IF NOT SUCCESS                                               51730003
517700        MOVE 'P2670-1'               TO ERR-PARAGRAPH             51740003
517800        PERFORM P9999-GOT-PROBLEM                                 51750003
517900     END-IF.                                                      51760003
518000*                                                                 51770003
518100 P2700-SPLIT-VAC-SCHEDULE.                                        51780003
518200*                                                                 51790003
518300* PLEASE NOTE - THIS CODE ALSO EXISTS IN 919, ANY CHANGES         51800003
518400* MADE HERE MUST ALSO BE MADE THERE                               51810003
518500     MOVE SPACES                    TO P977-COMMAREA-PARMS        51820003
518600     SET P977-SCHEDULE              TO TRUE                       51830003
518700     SET P977-SPLIT-FUNCTION        TO TRUE                       51840003
518800     MOVE EMP-NBR OF WS-MSTR        TO P977-EMP-NO                51850003
518900*    IF THE EMPLOYEE IS BEING MARKED UP AFTER 2000PM              51860003
519000*    CONSIDER IT TO BEGIN THE NEXT DAY                            51870003
519100*                                                                 51880003
519200     IF WS-EFF-TIME > 1959                                        51890003
519300        MOVE ZEROS                    TO DATE-CONVERSION-PARMS    51900003
519400        SET PARM-ADD                  TO TRUE                     51910003
519500        MOVE P917-EFF-DATE            TO PARM-PRI-DATE-GREG       51920003
519600        MOVE '000001'                 TO PARM-SEC-DATE-GREG       51930003
519700        EXEC CICS LINK                                            51940003
519800                  PROGRAM(P903-PGM)                               51950003
519900                  COMMAREA(DATE-CONVERSION-PARMS)                 51960003
520000                  LENGTH(P903-LGTH)                               51970003
520100                  RESP(WS-RESPONSE)                               51980003
520200        END-EXEC                                                  51990003
520300        MOVE WS-RESPONSE              TO FILE-STATUS              52000003
520400        IF NOT SUCCESS                                            52010003
520500           MOVE 'P2700-1'             TO ERR-PARAGRAPH            52020003
520600           MOVE 'P903LINK'            TO ERR-KEY                  52030003
520700           PERFORM P9999-GOT-PROBLEM                              52040003
520800        END-IF                                                    52050003
520900        MOVE PARM-RES-DATE-GREG       TO P977-MID-DATE            52060003
521000     ELSE                                                         52070003
521100        MOVE P917-EFF-DATE            TO P977-MID-DATE            52080003
521200     END-IF                                                       52090003
521300*                                                                 52100003
521400     IF P977-MID-YR > '90'                                        52110003
521500        MOVE '19'                   TO P977-MID-CENT              52120003
521600     ELSE                                                         52130003
521700        MOVE '20'                   TO P977-MID-CENT              52140003
521800     END-IF                                                       52150003
521900     PERFORM P8700-LINK-TO-P977                                   52160003
522000*                                                                 52170003
522100*    THE SPLIT FUNCTION WILL RETURN THE END DATE OF THE           52180003
522200*    ORIGINAL SCHEDULE IN P977-END-CENT-DATE. ALL WE              52190003
522300*    NEED TO DO IS RESET THE FLAG ON THE UNTAKEN LAST PART        52200003
522400*    OF THE SCHEDULE. BECAUSE ALL OF THE KEY INFORMATION IS       52210003
522500*    ALREADY IN THE COMMAREA, ALL WE NEED TO DO IS RESET          52220003
522600*    THE FUNCTION CODE AND RE-CALL 977.                           52230003
522700*                                                                 52240003
522800     IF P977-MID-CENT-DATE < P977-END-CENT-DATE                   52250003
522900        SET FULL-VAC-NOT-TAKEN      TO TRUE                       52260003
523000     END-IF                                                       52270003
523100     SET P977-UNTAKE-FUNCTION       TO TRUE                       52280003
523200     PERFORM P8700-LINK-TO-P977.                                  52290003
523300*                                                                 52300003
523400*                                                                 52310003
523500 P2800-CHECK-RELEASE-FROM-ASGN.                                   52320003
523600*                                                                 52330003
523700     MOVE SPACES                      TO ASGNKEY2                 52340003
523800     MOVE EMP-NBR OF WS-MSTR          TO ASGN-EMP-NO              52350003
523900     SET ASGN-EMP-OWNER-REC           TO TRUE                     52360003
524000     MOVE '0000000000'                TO ASGN-EMP-DATE-TIME       52370003
524100     MOVE 0                           TO WS-DONE-CODE             52380003
524200     PERFORM UNTIL DONE                                           52390003
524300        MOVE ASGNKEY2                 TO ASGNEMP                  52400003
524400        EXEC CICS READ                                            52410003
524500                  DATASET(ASGN-VIA-ASGNEMP)                       52420003
524600                  INTO(WS-ASGN-FILE)                              52430003
524700                  LENGTH(ASGNEMP-RLGTH)                           52440003
524800                  RIDFLD(ASGNEMP)                                 52450003
524900                  KEYLENGTH(ASGNEMP-KLGTH)                        52460003
525000                  GTEQ                                            52470003
525100                  RESP(WS-RESPONSE)                               52480003
525200        END-EXEC                                                  52490003
525300        MOVE WS-RESPONSE TO FILE-STATUS                           52500003
525400        IF SUCCESS                                                52510003
525500           IF ASGN-EMP-NO             = EMP-NBR OF WS-MSTR        52520003
525600              IF ASGN-EMP-OWNER-REC                               52530003
525700              OR ASGN-EMP-TEMP-REC                                52540003
525800                 MOVE ASGNKEY1        TO REL-ASGNKEY1             52550003
525900                 MOVE ASGNKEY2        TO REL-ASGNKEY2             52560003
526000                 MOVE SPACES          TO P956-COMMAREA-PARMS      52570003
526100                 SET P956-GET-CNTL-STATUS-REASON                  52580003
526200                                      TO TRUE                     52590003
526300                 MOVE P917-STATUS-CODE1                           52600003
526400                                      TO P956-STATUS-CODE         52610003
526500                 MOVE P917-EDB-CODE   TO P956-REASON-CODE         52620003
526600                 MOVE CRAFT OF WS-MSTR                            52630003
526700                                      TO P956-CC                  52640003
526800                 MOVE ASGN-DIST       TO P956-DIST                52650003
526900                 MOVE ASGN-SUB-DIST   TO P956-SDIST               52660003
527000                 MOVE ASGN-JOB-TYPE   TO P956-ASGN-TYPE           52670003
527100                 IF ASGN-XB-JOB OF ASGN-JOB-TYPE                  52680003
527200                    MOVE ASGN-XB-CC   TO P956-XB                  52690003
527300                 END-IF                                           52700003
527400                 MOVE ASGN-AJ-JOB-NO  TO P956-ASGN                52710003
527500                 PERFORM P9840-RETRIEVE-CNTL-INFO                 52720003
527600                 IF P956-ERROR-FOUND                              52730003
527700                    MOVE 'P2800-1'    TO ERR-PARAGRAPH            52740003
527800                    MOVE 'P965 LINK'  TO ERR-SENTENCE             52750003
527900                    MOVE P956-INPUT-PARMS                         52760003
528000                                      TO ERR-KEY                  52770003
528100                    PERFORM P9999-GOT-PROBLEM                     52780003
528200                 END-IF                                           52790003
528300              ELSE                                                52800003
528400                 SET DONE             TO TRUE                     52810003
528500              END-IF                                              52820003
528600              IF ASGN-EMP-OWNER-REC                               52830003
528700                 IF P956-ST-RSN-REL-ASGN-PERM                     52840003
528800                 OR P956-ST-RSN-REL-ASGN-BOTH                     52850003
528900                    PERFORM P2510-DELETE-ASGNJOB                  52860003
529000                    IF ASGN-XB-JOB OF ASGN-JOB-TYPE               52870003
529100                       PERFORM P2850-POSITION-TURN                52880003
529200                    END-IF                                        52890003
529300                 END-IF                                           52900003
529400              END-IF                                              52910003
529500              IF ASGN-EMP-TEMP-REC AND                            52920003
529600                 (P956-ST-RSN-REL-ASGN-TEMP OR                    52930003
529700                  P956-ST-RSN-REL-ASGN-BOTH)                      52940003
529800                 PERFORM P2510-DELETE-ASGNJOB                     52950003
529900                 IF ASGN-XB-JOB OF ASGN-JOB-TYPE                  52960003
530000                    PERFORM P2650-POSITION-TURN                   52970003
530100                 END-IF                                           52980003
530200              END-IF                                              52990003
530300              MOVE ASGN-EMP-DATE-TIME  TO WS-WORK-DATE-TIME-X     53000003
530400              ADD 1                    TO WS-WORK-DATE-TIME       53010003
530500              MOVE WS-WORK-DATE-TIME-X TO ASGN-EMP-DATE-TIME      53020003
530600           ELSE                                                   53030003
530700              SET DONE                TO TRUE                     53040003
530800           END-IF                                                 53050003
530900        ELSE                                                      53060003
531000           SET DONE                   TO TRUE                     53070003
531100           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  53080003
531200              MOVE 'P2800-2' TO ERR-PARAGRAPH                     53090003
531300              MOVE ASGNEMP   TO ERR-KEY                           53100003
531400              PERFORM P9999-GOT-PROBLEM                           53110003
531500           END-IF                                                 53120003
531600        END-IF                                                    53130003
531700     END-PERFORM.                                                 53140003
531800*                                                                 53150003
531900 P2850-POSITION-TURN.                                             53160003
532000*                                                                 53170003
532100*    GET THE CURRENT OWNER OF THE TURN                            53180003
532200*                                                                 53190003
532300     MOVE SPACES                     TO XXXX-ASGNKEY1             53200003
532400     SET XX-ASGN-JOB-XB              TO TRUE                      53210003
532500     MOVE ASGN-DIST                  TO XX-ASGN-DIST              53220003
532600     MOVE ASGN-SUB-DIST              TO XX-ASGN-SUB-DIST          53230003
532700     MOVE ASGN-XB-JOB OF ASGN-ASSIGNMENT                          53240003
532800                                     TO XX-ASGN-JOB               53250003
532900     PERFORM PXXXX-LATEST-TEMP                                    53260003
533000     IF ASGN-EMP-NO NOT > ZERO                                    53270003
533100        PERFORM PXXXX-JOB-OWNER                                   53280003
533200     END-IF                                                       53290003
533300*                                                                 53300003
533400*    IF THE CURRENT OWNER IS A MORE RECENT TEMP THAN THE GUY      53310003
533500*    WE JUST REMOVED FROM THIS ASSIGNMENT, DON'T TOUCH THE TURN.  53320003
533600*                                                                 53330003
533700     MOVE SPACES                    TO DE-COMPARE3-DATE-TIME      53340003
533800     MOVE SPACES                    TO DE-COMPARE4-DATE-TIME      53350003
533900                                                                  53360003
534000     SET DE-YYMMDD-FORMAT           TO TRUE                       53370003
534100     MOVE ASGN-DATE-TIME(1:6)       TO DE-YYMMDD                  53380003
534200     PERFORM P8998-DATEEDIT                                       53390003
534300     MOVE DE-CCYYMMDD               TO DE-COMPARE3-DATE           53400003
534400     MOVE ASGN-DATE-TIME(7:4)       TO DE-COMPARE3-TIME           53410003
534500                                                                  53420003
534600     SET DE-YYMMDD-FORMAT           TO TRUE                       53430003
534700     MOVE REL-ASGN-DATE-TIME(1:6)   TO DE-YYMMDD                  53440003
534800     PERFORM P8998-DATEEDIT                                       53450003
534900     MOVE DE-CCYYMMDD               TO DE-COMPARE4-DATE           53460003
535000     MOVE REL-ASGN-DATE-TIME(7:4)   TO DE-COMPARE4-TIME           53470003
535100                                                                  53480003
535200     IF ASGN-EMP-NO > ZERO AND                                    53490003
535300        DE-COMPARE3-DATE-TIME > DE-COMPARE4-DATE-TIME             53500003
535400**      ASGN-DATE-TIME > REL-ASGN-DATE-TIME                       53510003
535500        CONTINUE                                                  53520003
535600     ELSE                                                         53530003
535700*                                                                 53540003
535800*    READ XB CNTL RECORD TO DETERMINE IF THE BOARD IS CONSISTED   53550003
535900*    OR NON-CONSISTED                                             53560003
536000*                                                                 53570003
536100        MOVE REL-ASGNKEY1            TO ASGNKEY1                  53580003
536200        MOVE SPACES                  TO WORK-CNTLKEY              53590003
536300        MOVE '08'                    TO WK-CNTL-REC-TYPE          53600003
536400        MOVE ASGN-DIST               TO WK-CNTL-DIST              53610003
536500        MOVE ASGN-SUB-DIST           TO WK-CNTL-SUB-DIST          53620003
536600        MOVE ASGN-XB-CC              TO WK-CNTL-XB-CODE           53630003
536700        MOVE WORK-CNTLKEY            TO CNTLKEY                   53640003
536800        PERFORM P8400-READ-CNTLFILE                               53650003
536900        IF NOT SUCCESS                                            53660003
537000           MOVE 'P2850-1'            TO ERR-PARAGRAPH             53670003
537100           MOVE CNTLKEY              TO ERR-KEY                   53680003
537200           PERFORM P9999-GOT-PROBLEM                              53690003
537300        END-IF                                                    53700003
537400*                                                                 53710003
537500*    READ THE XB TURN RECORD TO SEE IF THE TURN IS ON OR          53720003
537600*    OFF BOARD                                                    53730003
537700*                                                                 53740003
537800        MOVE SPACES                  TO EBTURN-AREA               53750003
537900        MOVE ASGN-DIST               TO DIST-REPEAT               53760003
538000        MOVE ASGN-SUB-DIST           TO SUBDIST-REPEAT            53770003
538100        MOVE ASGN-XB-TURN            TO TURN-NBR OF EBTURN-AREA   53780003
538200        MOVE ASGN-XB-CC              TO CRAFT-CODE-REPEAT         53790003
538300        MOVE EBTURN-AREA             TO EBTURN                    53800003
538400        EXEC CICS READ                                            53810003
538500                  DATASET(EB-VIA-TURN-NBR)                        53820003
538600                  INTO(WS-EXTRA-BOARD)                            53830003
538700                  LENGTH(EBTURNNO-RLGTH)                          53840003
538800                  RIDFLD(EBTURN)                                  53850003
538900                  KEYLENGTH(EBTURNNO-KLGTH)                       53860003
539000                  RESP(WS-RESPONSE)                               53870003
539100        END-EXEC                                                  53880003
539200        MOVE WS-RESPONSE             TO FILE-STATUS               53890003
539300        IF NOT SUCCESS                                            53900003
539400           MOVE 'P2850-2'            TO ERR-PARAGRAPH             53910003
539500           MOVE EBTURN               TO ERR-KEY                   53920003
539600           PERFORM P9999-GOT-PROBLEM                              53930003
539700        END-IF                                                    53940003
539800*                                                                 53950003
539900*    IF THE TURN IS NOW VACANT,                                   53960003
540000*    THE TURN SHOULD BE ON BOARD.  OTHERWISE, THE TURN            53970003
540100*    SHOULD BE OFF BOARD.                                         53980003
540200*                                                                 53990003
540300        IF ASGN-EMP-NO NOT > ZEROES                               54000003
540400         AND EB-OFF-BOARD                                         54010003
540500           PERFORM P2860-REALIGN-XB                               54020003
540600        ELSE                                                      54030003
540700           IF EB-ON-BOARD                                         54040003
540800              PERFORM P2870-SEN-MOVE-XB                           54050003
540900           END-IF                                                 54060003
541000        END-IF                                                    54070003
541100     END-IF                                                       54080003
541200                                                                  54090003
541300     MOVE REL-ASGNKEY1               TO ASGNKEY1                  54100003
541400     MOVE REL-ASGNKEY2               TO ASGNKEY2.                 54110003
541500*                                                                 54120003
541600 P2860-REALIGN-XB.                                                54130003
541700*                                                                 54140003
541800     MOVE SPACES                      TO P914-COMMAREA-PARMS      54150003
541900     SET P914-REALIGN-FUNCTION        TO TRUE                     54160003
542000     MOVE P956-ST-RSN-REL-FROM-ASGN   TO P914-MOVE-TYPE           54170003
542100     MOVE P917-EFF-DATE-TIME          TO P914-EFF-DATE-TIME       54180003
542200     MOVE XX-ASGN-DIST                TO P914-TURN-DIST           54190003
542300     MOVE XX-ASGN-SUB-DIST            TO P914-TURN-SUB-DIST       54200003
542400     MOVE XX-ASGN-JOB-CC              TO P914-TURN-CC             54210003
542500     MOVE XX-LAST-4-BYTES             TO P914-TURN                54220003
542600     MOVE P917-TIME-ZONE              TO P914-TIME-ZONE           54230003
542700                                                                  54240003
542800     EXEC CICS LINK                                               54250003
542900               PROGRAM(P914-PGM)                                  54260003
543000               COMMAREA(P914-COMMAREA-PARMS)                      54270003
543100               LENGTH(P914-LGTH)                                  54280003
543200               RESP(WS-RESPONSE)                                  54290003
543300     END-EXEC                                                     54300003
543400     MOVE WS-RESPONSE                TO FILE-STATUS               54310003
543500     IF NOT SUCCESS                                               54320003
543600        MOVE 'P2860-1'               TO ERR-PARAGRAPH             54330003
543700        PERFORM P9999-GOT-PROBLEM                                 54340003
543800     END-IF.                                                      54350003
543900*                                                                 54360003
544000 P2870-SEN-MOVE-XB.                                               54370003
544100*                                                                 54380003
544200     MOVE SPACES                     TO P913-COMMAREA-PARMS       54390003
544300     MOVE XX-ASGN-DIST               TO P913-TURN-DIST            54400003
544400     MOVE XX-ASGN-SUB-DIST           TO P913-TURN-SUB-DIST        54410003
544500     MOVE XX-ASGN-JOB-CC             TO P913-TURN-CC              54420003
544600     MOVE XX-LAST-4-BYTES            TO P913-TURN                 54430003
544700     SET P913-SEN-MOVE-FUNCTION      TO TRUE                      54440003
544800     MOVE P917-TIME-ZONE             TO P913-TIME-ZONE            54450003
544900     MOVE P917-EFF-DATE-TIME         TO P913-EFF-DATE-TIME        54460003
545000     EXEC CICS LINK                                               54470003
545100               PROGRAM(P913-PGM)                                  54480003
545200               COMMAREA(P913-COMMAREA-PARMS)                      54490003
545300               LENGTH(P913-LGTH)                                  54500003
545400               RESP(WS-RESPONSE)                                  54510003
545500     END-EXEC                                                     54520003
545600     MOVE WS-RESPONSE                TO FILE-STATUS               54530003
545700     IF NOT SUCCESS                                               54540003
545800        MOVE 'P2870-1'               TO ERR-PARAGRAPH             54550003
545900        PERFORM P9999-GOT-PROBLEM                                 54560003
546000     END-IF.                                                      54570003
546100*                                                                 54580003
546200*                                                                 54590003
546300 P3000-SEND-STAFF-FORM.                                           54600003
546400*                                                                 54610003
546500     MOVE EMP-NAME    OF WS-MSTR TO SF-EMP-NAME                   54620003
546600     MOVE EMP-NBR     OF WS-MSTR TO SF-EMP-NBR                    54630003
546700     MOVE DIST        OF WS-MSTR TO SF-DIST                       54640003
546800     MOVE SUB-DIST    OF WS-MSTR TO SF-SUB-DIST                   54650003
546900     MOVE CRAFT       OF WS-MSTR TO SF-CRAFT                      54660003
547000     MOVE WS-AFTER-STATUS-2      TO SF-STATUS                     54670003
547100     MOVE WS-AFTER-ECC           TO SF-ECC                        54680003
547200     PERFORM VARYING LO-TABLE-SUB FROM 1 BY 1                     54690003
547300         UNTIL LO-TABLE-SUB > LO-ARRAY-MAX                        54700003
547400         OR WS-LO-CODE(LO-TABLE-SUB) = WS-AFTER-STATUS            54710003
547500     END-PERFORM                                                  54720003
547600     IF LO-TABLE-SUB > LO-ARRAY-MAX                               54730003
547700         MOVE 'NOT FOUND'        TO SF-STATUS-DESC                54740003
547800     ELSE                                                         54750003
547900         MOVE WS-LO-CODE-DESC (LO-TABLE-SUB, 1)                   54760003
548000                                 TO SF-STATUS-DESC                54770003
548100     END-IF                                                       54780003
548200     MOVE SPACES                 TO P956-REASON-LONG-DESC         54790003
548300     MOVE WS-AFTER-ECC           TO P956-REASON-CODE              54800003
548400     SET P956-VALIDATE-REASON-CODE TO TRUE                        54810003
548500     PERFORM P9840-RETRIEVE-CNTL-INFO                             54820003
548600     MOVE P956-REASON-LONG-DESC  TO SF-ECC-DESC                   54830003
548700     MOVE WS-BEFORE-STATUS-2     TO SF-LAST-STATUS                54840003
548800     MOVE WS-BEFORE-ECC          TO SF-LAST-ECC                   54850003
548900                                    P956-REASON-CODE              54860003
549000     PERFORM VARYING LO-TABLE-SUB FROM 1 BY 1                     54870003
549100         UNTIL LO-TABLE-SUB > LO-ARRAY-MAX                        54880003
549200         OR WS-LO-CODE(LO-TABLE-SUB) = WS-BEFORE-STATUS           54890003
549300     END-PERFORM                                                  54900003
549400     IF LO-TABLE-SUB > LO-ARRAY-MAX                               54910003
549500         MOVE 'NOT FOUND'        TO SF-LAST-STATUS-DESC           54920003
549600     ELSE                                                         54930003
549700         MOVE WS-LO-CODE-DESC (LO-TABLE-SUB, 1)                   54940003
549800                                 TO SF-LAST-STATUS-DESC           54950003
549900     END-IF                                                       54960003
550000     MOVE SPACES                 TO P956-REASON-LONG-DESC         54970003
550100     SET P956-VALIDATE-REASON-CODE TO TRUE                        54980003
550200     PERFORM P9840-RETRIEVE-CNTL-INFO                             54990003
550300     MOVE P956-REASON-LONG-DESC  TO SF-LAST-ECC-DESC              55000003
550400     MOVE LAYOFF-YEAR            TO SF-LAST-OFF-YR                55010003
550500     MOVE LAYOFF-MONTH           TO SF-LAST-OFF-MO                55020003
550600     MOVE LAYOFF-DAY             TO SF-LAST-OFF-DY                55030003
550700     MOVE LAYOFF-HOUR            TO SF-LAST-OFF-HR                55040003
550800     MOVE LAYOFF-MINUTE          TO SF-LAST-OFF-MN                55050003
550900     MOVE MARKUP-YEAR            TO SF-LAST-ON-YR                 55060003
551000     MOVE MARKUP-MONTH           TO SF-LAST-ON-MO                 55070003
551100     MOVE MARKUP-DAY             TO SF-LAST-ON-DY                 55080003
551200     MOVE MARKUP-HOUR            TO SF-LAST-ON-HR                 55090003
551300     MOVE MARKUP-MINUTE          TO SF-LAST-ON-MN                 55100003
551400     MOVE MSTR2-EMP-SIN          TO SF-EMP-SIN                    55110003
551500     MOVE MSTR2-EMP-SSN          TO SF-EMP-SSN                    55120003
551600     MOVE MSTR2-BD-YR            TO SF-EMP-BD-YR                  55130003
551700     MOVE MSTR2-BD-MO            TO SF-EMP-BD-MO                  55140003
551800     MOVE MSTR2-BD-DY            TO SF-EMP-BD-DY                  55150003
551900     MOVE MSTR2-DOE-YR           TO SF-EMP-DOE-YR                 55160003
552000     MOVE MSTR2-DOE-MO           TO SF-EMP-DOE-MO                 55170003
552100     MOVE MSTR2-DOE-DY           TO SF-EMP-DOE-DY                 55180003
552200     MOVE LAST-JOB-ASG           TO WS-LAST-ASSGN                 55190003
552300     MOVE WS-LAST-ASSGN1         TO SF-LAST-ASSGN1                55200003
552400     MOVE WS-LAST-ASSGN2         TO SF-LAST-ASSGN2                55210003
552500     MOVE ODT-YEAR               TO SF-ON-DUTY-YR                 55220003
552600     MOVE ODT-MONTH              TO SF-ON-DUTY-MO                 55230003
552700     MOVE ODT-DAY                TO SF-ON-DUTY-DY                 55240003
552800     MOVE ODT-HOUR               TO SF-ON-DUTY-HR                 55250003
552900     MOVE ODT-MINUTE             TO SF-ON-DUTY-MN                 55260003
553000     MOVE TUT-YEAR               TO SF-TIE-UP-YR                  55270003
553100     MOVE TUT-MONTH              TO SF-TIE-UP-MO                  55280003
553200     MOVE TUT-DAY                TO SF-TIE-UP-DY                  55290003
553300     MOVE TUT-HOUR               TO SF-TIE-UP-HR                  55300003
553400     MOVE TUT-MINUTE             TO SF-TIE-UP-MN                  55310003
553500*                                                                 55320003
553600     MOVE SPACES                 TO P930-COMMAREA                 55330003
553700     SET P930-STAFF-FORM         TO TRUE                          55340003
553800     MOVE DIST OF WS-MSTR        TO P930-DIST                     55350003
553900     MOVE SUB-DIST OF WS-MSTR    TO P930-SUB-DIST                 55360003
554000     PERFORM VARYING TALLY FROM 1 BY 1 UNTIL TALLY > 15           55370003
554100        MOVE WS-STAFF-FORM-LINE (TALLY)                           55380003
554200                                 TO P930-DATA-LINE (TALLY)        55390003
554300     END-PERFORM                                                  55400003
554400*                                                                 55410003
554500     EXEC CICS LINK                                               55420003
554600               PROGRAM(P930-PGM)                                  55430003
554700               COMMAREA(P930-COMMAREA)                            55440003
554800               LENGTH(P930-LGTH)                                  55450003
554900               RESP(WS-RESPONSE)                                  55460003
555000     END-EXEC                                                     55470003
555100     MOVE WS-RESPONSE TO FILE-STATUS                              55480003
555200     IF NOT SUCCESS                                               55490003
555300        MOVE 'P3000-1' TO ERR-PARAGRAPH                           55500003
555400        PERFORM P9999-GOT-PROBLEM                                 55510003
555500     END-IF.                                                      55520003
555600*                                                                 55530003
555700 P3500-FUTURE-MARKUP-DUEBACK.                                     55540003
555800*                                                                 55550003
555900     IF SAVE-ASGN-TYPE = 'X' OR 'U'                               55560003
556000        IF P956-ST-RSN-AUTO-MU                                    55570003
556100           IF P956-ST-RSN-MU-PLUS                                 55580003
556200              MOVE ZERO TO DATE-CONVERSION-PARMS                  55590003
556300              SET PARM-ADD            TO TRUE                     55600003
556400              MOVE P917-EXP-DATE      TO PARM-PRI-DATE-GREG       55610003
556500              MOVE P917-EXP-TIME      TO PARM-PRI-HRMN            55620003
556600              MOVE P956-ST-RSN-MU-TIME                            55630003
556700                                   TO PARM-SEC-HRMN               55640003
556800              EXEC CICS LINK                                      55650003
556900                        PROGRAM(P903-PGM)                         55660003
557000                        COMMAREA(DATE-CONVERSION-PARMS)           55670003
557100                        LENGTH(P903-LGTH)                         55680003
557200                        RESP(WS-RESPONSE)                         55690003
557300              END-EXEC                                            55700003
557400              MOVE WS-RESPONSE TO FILE-STATUS                     55710003
557500              IF NOT SUCCESS                                      55720003
557600                 MOVE 'P3500-1' TO ERR-PARAGRAPH                  55730003
557700                 PERFORM P9999-GOT-PROBLEM                        55740003
557800              END-IF                                              55750003
557900              MOVE PARM-RES-DATE-GREG TO P917-EXP-DATE            55760003
558000              MOVE PARM-RES-HRMN      TO P917-EXP-TIME            55770003
558100           ELSE                                                   55780003
558200              MOVE ZERO TO DATE-CONVERSION-PARMS                  55790003
558300              SET PARM-SUBTRACT       TO TRUE                     55800003
558400              MOVE P917-EXP-DATE      TO PARM-PRI-DATE-GREG       55810003
558500              MOVE P917-EXP-TIME      TO PARM-PRI-HRMN            55820003
558600              MOVE P956-ST-RSN-MU-TIME                            55830003
558700                                   TO PARM-SEC-HRMN               55840003
558800              EXEC CICS LINK                                      55850003
558900                        PROGRAM(P903-PGM)                         55860003
559000                        COMMAREA(DATE-CONVERSION-PARMS)           55870003
559100                        LENGTH(P903-LGTH)                         55880003
559200                        RESP(WS-RESPONSE)                         55890003
559300              END-EXEC                                            55900003
559400              MOVE WS-RESPONSE TO FILE-STATUS                     55910003
559500              IF NOT SUCCESS                                      55920003
559600                 MOVE 'P3500-2' TO ERR-PARAGRAPH                  55930003
559700                 PERFORM P9999-GOT-PROBLEM                        55940003
559800              END-IF                                              55950003
559900              MOVE PARM-RES-DATE-GREG    TO P917-EXP-DATE         55960003
560000              MOVE PARM-RES-HRMN         TO P917-EXP-TIME         55970003
560100           END-IF                                                 55980003
560200        END-IF                                                    55990003
560300     END-IF.                                                      56000003
560400*                                                                 56010003
560500 P4000-WRITE-REF-BKOFF-HIST.                                      56020003
560600*                                                                 56030003
560700     MOVE P917-EMP-NO         TO P943-EMP-NBR                     56040003
560800     MOVE WS-EFF-DATE-TIME    TO P943-EFF-DATE-TIME               56050003
560900     MOVE WS-TIME-ZONE        TO P943-EMP-TIME-ZONE               56060003
561000     MOVE DIST     OF WS-MSTR TO P943-DIST                        56070003
561100     MOVE SUB-DIST OF WS-MSTR TO P943-SDIST                       56080003
561200     MOVE CRAFT    OF WS-MSTR TO P943-CRAFT                       56090003
561300     SET P943-REFUSE-LAYOFF-FUN TO TRUE                           56100003
561400     MOVE P917-EDB-CODE         TO P943-ECC-CODE                  56110003
561500     MOVE P917-STATUS-CODE1     TO P943-LO-CODE-1                 56120003
561600     IF P917-ON-CALL                                              56130003
561700        MOVE P917-TRAIN-AREA  TO P943-TEMP-ASGN-TRAIN             56140003
561800        IF TEMPORARY-ASGNMT > SPACE                               56150003
561900           MOVE TEMPORARY-ASGNMT TO P943-NORM-ASGN                56160003
562000           IF TEMP-ASGN-UFP                                       56170003
562100              MOVE TA-POOL       TO P943-POOL-ASG                 56180003
562200           END-IF                                                 56190003
562300        ELSE                                                      56200003
562400           MOVE NORMAL-ASGNMT    TO P943-NORM-ASGN                56210003
562500           IF NORM-ASGN-UFP                                       56220003
562600              MOVE NA-POOL       TO P943-POOL-ASG                 56230003
562700           END-IF                                                 56240003
562800        END-IF                                                    56250003
562900     ELSE                                                         56260003
563000        IF TEMPORARY-ASGNMT > SPACE                               56270003
563100           MOVE TEMPORARY-ASGNMT TO P943-NORM-ASGN                56280003
563200           IF TEMP-ASGN-UFP                                       56290003
563300              MOVE TA-POOL       TO P943-POOL-ASG                 56300003
563400           END-IF                                                 56310003
563500        ELSE                                                      56320003
563600           MOVE NORMAL-ASGNMT    TO P943-NORM-ASGN                56330003
563700           IF NORM-ASGN-UFP                                       56340003
563800              MOVE NA-POOL       TO P943-POOL-ASG                 56350003
563900           END-IF                                                 56360003
564000        END-IF                                                    56370003
564100     END-IF                                                       56380003
564200                                                                  56390003
564300     PERFORM P8900-WRITE-HISTORY.                                 56400003
564400*                                                                 56410003
564500 P8000-READ-VACEMP-FILE.                                          56420003
564600*                                                                 56430003
564700     EXEC CICS READ                                               56440003
564800               DATASET(VACATION-VIA-EMP-DATE)                     56450003
564900               INTO(WS-VACATION-RECORD)                           56460003
565000               LENGTH(VACEMPD-RLGTH)                              56470003
565100               RIDFLD(VACKEY1)                                    56480003
565200               KEYLENGTH(VACEMPD-KLGTH)                           56490003
565300               RESP(WS-RESPONSE)                                  56500003
565400     END-EXEC                                                     56510003
565500     MOVE WS-RESPONSE TO FILE-STATUS                              56520003
565600     IF NOT SUCCESS                                               56530003
565700        IF NO-RECORD-FND OR END-OF-FILE                           56540003
565800           CONTINUE                                               56550003
565900        ELSE                                                      56560003
566000           MOVE 'P8000-1'  TO ERR-PARAGRAPH                       56570003
566100           MOVE VACKEY1     TO ERR-KEY                            56580003
566200           PERFORM P9999-GOT-PROBLEM                              56590003
566300        END-IF                                                    56600003
566400     END-IF.                                                      56610003
566500*                                                                 56620003
566600 P8050-READ-TKCRFILE-GTEQ.                                        56630003
566700     EXEC CICS READ                                               56640003
566800               GTEQ                                               56650003
566900               DATASET(TKCR-FILE-VIA-TKCRKEY)                     56660003
567000               INTO(WS-TKCR-FILE)                                 56670003
567100               LENGTH(TKCRFILE-RLGTH)                             56680003
567200               RIDFLD(TKCRKEY)                                    56690003
567300               KEYLENGTH(TKCRFILE-KLGTH)                          56700003
567400               RESP(WS-RESPONSE)                                  56710003
567500     END-EXEC                                                     56720003
567600     MOVE WS-RESPONSE        TO FILE-STATUS.                      56730003
567700*                                                                 56740003
567800 P8100-READ-TKCRFILE.                                             56750003
567900*                                                                 56760003
568000     EXEC CICS READ                                               56770003
568100               DATASET(TKCR-FILE-VIA-TKCRKEY)                     56780003
568200               INTO(WS-TKCR-FILE)                                 56790003
568300               LENGTH(TKCRFILE-RLGTH)                             56800003
568400               RIDFLD(TKCRKEY)                                    56810003
568500               KEYLENGTH(TKCRFILE-KLGTH)                          56820003
568600               RESP(WS-RESPONSE)                                  56830003
568700     END-EXEC                                                     56840003
568800     MOVE WS-RESPONSE        TO FILE-STATUS.                      56850003
568900                                                                  56860003
569000*                                                                 56870003
569100 P8200-READ-MSTR-UPDATE.                                          56880003
569200*                                                                 56890003
569300     EXEC CICS READ                                               56900003
569400               UPDATE                                             56910003
569500               DATASET(MSTR-VIA-EMP-NBR)                          56920003
569600               INTO(WS-MSTR)                                      56930003
569700               LENGTH(MSTRENBR-RLGTH)                             56940003
569800               RIDFLD(MSTRNBRK)                                   56950003
569900               KEYLENGTH(MSTRENBR-KLGTH)                          56960003
570000               RESP(WS-RESPONSE)                                  56970003
570100     END-EXEC                                                     56980003
570200     MOVE WS-RESPONSE TO FILE-STATUS.                             56990003
570300*                                                                 57000003
570400 P8210-REWRITE-MSTR.                                              57010003
570500*                                                                 57020003
570600     EXEC CICS REWRITE                                            57030003
570700               DATASET(MSTR-VIA-EMP-NBR)                          57040003
570800               FROM(WS-MSTR)                                      57050003
570900               LENGTH(MSTRENBR-RLGTH)                             57060003
571000               RESP(WS-RESPONSE)                                  57070003
571100     END-EXEC                                                     57080003
571200     MOVE WS-RESPONSE                TO FILE-STATUS.              57090003
571300*=================================================================57100003
571400 P8250-READ-JSKEY1-GTEQ.                                          57110003
571500*=================================================================57120003
571600     MOVE JS-KEY1                        TO WORK-JS-KEY1, JSKEY1  57130003
571700     EXEC CICS READ                                               57140003
571800               GTEQ                                               57150003
571900               DATASET(JS-VIA-JSKEY1)                             57160003
572000               INTO(WS-JOB-SCHEDULE)                              57170003
572100               LENGTH(JSKEY1-RLGTH)                               57180003
572200               RIDFLD(JSKEY1)                                     57190003
572300               KEYLENGTH(JSKEY1-KLGTH)                            57200003
572400               RESP(WS-RESPONSE)                                  57210003
572500     END-EXEC                                                     57220003
572600     MOVE WS-RESPONSE TO FILE-STATUS                              57230003
572700     IF SUCCESS                                                   57240003
572800        IF JSK1-ASGN-DIST        NOT = WK-JSK1-ASGN-DIST          57250003
572900           OR JSK1-ASGN-SUB-DIST NOT = WK-JSK1-ASGN-SUB-DIST      57260003
573000           OR JSK1-ASSIGNMENT    NOT = WK-JSK1-ASSIGNMENT         57270003
573100           OR NOT JSK1-ASGN-STATS                                 57280003
573200           SET NO-RECORD-FND             TO TRUE                  57290003
573300        END-IF                                                    57300003
573400     ELSE                                                         57310003
573500        IF NOT (END-OF-FILE OR NO-RECORD-FND)                     57320003
573600           MOVE 'P8250-1'                TO ERR-PARAGRAPH         57330003
573700           MOVE JSKEY1                   TO ERR-KEY               57340003
573800           PERFORM P9999-GOT-PROBLEM                              57350003
573900        END-IF                                                    57360003
574000     END-IF.                                                      57370003
574100*                                                                 57380003
574200 P8300-READ-MSTR2-UPDATE.                                         57390003
574300*                                                                 57400003
574400     EXEC CICS READ                                               57410003
574500               UPDATE                                             57420003
574600               DATASET(MSTR2-VIA-EMP-NBR)                         57430003
574700               INTO(WS-MSTR2)                                     57440003
574800               LENGTH(MSTR2ENBR-RLGTH)                            57450003
574900               RIDFLD(MSTR2NBRK)                                  57460003
575000               KEYLENGTH(MSTR2ENBR-KLGTH)                         57470003
575100               RESP(WS-RESPONSE)                                  57480003
575200     END-EXEC                                                     57490003
575300     MOVE WS-RESPONSE TO FILE-STATUS.                             57500003
575400*                                                                 57510003
575500 P8310-REWRITE-MSTR2.                                             57520003
575600*                                                                 57530003
575700     EXEC CICS REWRITE                                            57540003
575800               DATASET(MSTR2-VIA-EMP-NBR)                         57550003
575900               FROM(WS-MSTR2)                                     57560003
576000               LENGTH(MSTR2ENBR-RLGTH)                            57570003
576100               RESP(WS-RESPONSE)                                  57580003
576200     END-EXEC                                                     57590003
576300     MOVE WS-RESPONSE TO FILE-STATUS.                             57600003
576400*                                                                 57610003
576500 P8400-READ-CNTLFILE.                                             57620003
576600*                                                                 57630003
576700     MOVE WORK-CNTLKEY TO CNTLKEY                                 57640003
576800     EXEC CICS READ                                               57650003
576900               DATASET(CNTL-FILE-VIA-CNTLKEY)                     57660003
577000               INTO(WS-CNTL-FILE)                                 57670003
577100               LENGTH(CNTLFILE-RLGTH)                             57680003
577200               RIDFLD(CNTLKEY)                                    57690003
577300               KEYLENGTH(CNTLFILE-KLGTH)                          57700003
577400               RESP(WS-RESPONSE)                                  57710003
577500     END-EXEC                                                     57720003
577600     MOVE WS-RESPONSE TO FILE-STATUS.                             57730003
577700*                                                                 57740003
577800 P8510-MASTER-JOBS.                                               57750003
577900*                                                                 57760003
578000     MOVE SPACES             TO WS-ASGN-FILE                      57770003
578100     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       57780003
578200     PERFORM PXXXX-JOB-OWNED                                      57790003
578300     IF ASGN-ASSIGNMENT > SPACES                                  57800003
578400        MOVE ASGN-JOB-TYPE      TO NORMAL-ASGNMT-FLAG             57810003
578500        MOVE ASGN-ASSIGNMENT    TO NORMAL-ASGNMT                  57820003
578600     END-IF                                                       57830003
578700     MOVE SPACES             TO WS-ASGN-FILE                      57840003
578800     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       57850003
578900     PERFORM PXXXX-LATEST-TEMP-JOB                                57860003
579000     IF ASGN-ASSIGNMENT > SPACES                                  57870003
579100        MOVE ASGN-JOB-TYPE      TO TEMPORARY-ASGNMT-FLAG          57880003
579200        MOVE ASGN-ASSIGNMENT    TO TEMPORARY-ASGNMT               57890003
579300     END-IF                                                       57900003
579400     MOVE SPACES             TO WS-ASGN-FILE                      57910003
579500     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       57920003
579600     PERFORM PXXXX-JOB-ON-DUTY                                    57930003
579700     IF ASGN-ASSIGNMENT > SPACES                                  57940003
579800        MOVE ASGN-JOB-TYPE      TO ON-DUTY-ASGNMT-FLAG            57950003
579900        MOVE ASGN-ASSIGNMENT    TO ON-DUTY-ASGNMT                 57960003
580000        MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE      57970003
580100     END-IF                                                       57980003
580200     IF TEMPORARY-ASGNMT > SPACES                                 57990003
580300        MOVE TEMPORARY-ASGNMT-FLAG TO SAVE-ASGN-TYPE              58000003
580400     ELSE                                                         58010003
580500        IF NORMAL-ASGNMT > SPACES                                 58020003
580600           MOVE NORMAL-ASGNMT-FLAG TO SAVE-ASGN-TYPE              58030003
580700        ELSE                                                      58040003
580800           IF ON-DUTY-ASGNMT > SPACES                             58050003
580900              MOVE ON-DUTY-ASGNMT-FLAG TO SAVE-ASGN-TYPE          58060003
581000           END-IF                                                 58070003
581100        END-IF                                                    58080003
581200     END-IF.                                                      58090003
581300******************************************************************58100003
581400 P8530-READ-TRCN.                                                 58110003
581500******************************************************************58120003
581600     EXEC CICS READ                                               58130003
581700               DATASET  (TRAIN-CN-VIA-DSD-ASGN)                   58140003
581800               INTO     (WS-TRCN-FILE)                            58150003
581900               LENGTH   (TRAINCN-DSD-RLGTH)                       58160003
582000               RIDFLD   (TRCNKEY3)                                58170003
582100               KEYLENGTH(TRAINCN-DSD-KLGTH)                       58180003
582200               RESP     (WS-RESPONSE)                             58190003
582300     END-EXEC                                                     58200003
582400     MOVE WS-RESPONSE       TO FILE-STATUS                        58210003
582500     IF NOT SUCCESS                                               58220003
582600        IF NOT NO-RECORD-FND                                      58230003
582700           MOVE 'P8530'     TO ERR-PARAGRAPH                      58240003
582800           PERFORM P9999-GOT-PROBLEM                              58250003
582810        END-IF                                                    58260003
582820     END-IF.                                                      58270003
582830*                                                                 58280003
582840******************************************************************58281003
582850 P8600-STARTBR-PERS.                                              58282003
582860******************************************************************58283003
582870     EXEC CICS STARTBR                                            58284003
582880               DATASET(PLD-VIA-EMP-DATE)                          58285003
582890               RIDFLD(PLDKEY)                                     58286003
582900               GTEQ                                               58287003
583000               RESP(WS-RESPONSE)                                  58288003
583100     END-EXEC                                                     58289003
583200     MOVE WS-RESPONSE TO FILE-STATUS.                             58290003
583300*                                                                 58300003
583400******************************************************************58310003
583500 P8610-READNEXT-PERS.                                             58320003
583600******************************************************************58330003
583700     EXEC CICS READNEXT                                           58340003
583800               DATASET(PLD-VIA-EMP-DATE)                          58350003
583900               INTO(WS-PLD-RECORD)                                58360003
584000               RIDFLD(PLDKEY)                                     58370003
584100               LENGTH(PLDKEY-RLGTH)                               58380003
584200               KEYLENGTH(PLDKEY-KLGTH)                            58390003
584300               RESP(WS-RESPONSE)                                  58400003
584400     END-EXEC                                                     58410003
584500     MOVE WS-RESPONSE TO FILE-STATUS.                             58420003
584600*                                                                 58430003
584700******************************************************************58440003
584800 P8620-ENDBR-PERS.                                                58450003
584900******************************************************************58460003
585000     EXEC CICS ENDBR                                              58470003
585100               DATASET(PLD-VIA-EMP-DATE)                          58480003
585200               RESP(WS-RESPONSE)                                  58490003
585300     END-EXEC                                                     58500003
585400     MOVE WS-RESPONSE TO FILE-STATUS.                             58510003
585500*                                                                 58520003
585600 P8630-STARTBR-POINTER.                                           58530003
585700*                                                                 58540003
585800     EXEC CICS STARTBR                                            58550003
585900               DATASET(POINT-FILE-VIA-EMP)                        58560003
586000               RIDFLD(POINTKEY)                                   58570003
586100               GTEQ                                               58580003
586200               RESP(WS-RESPONSE)                                  58590003
586300     END-EXEC                                                     58600003
586400     MOVE WS-RESPONSE                   TO FILE-STATUS            58610003
586500     .                                                            58620003
586600*                                                                 58630003
586700 P8640-READPREV-POINTER.                                          58640003
586800*                                                                 58650003
586900     EXEC CICS READPREV                                           58660003
587000               DATASET(POINT-FILE-VIA-EMP)                        58670003
587100               INTO(WS-POINTER)                                   58680003
587200               LENGTH(POINT-RLGTH)                                58690003
587300               RIDFLD(POINTKEY)                                   58700003
587400               KEYLENGTH(POINT-KLGTH)                             58710003
587500               RESP(WS-RESPONSE)                                  58720003
587600     END-EXEC                                                     58730003
587700     MOVE WS-RESPONSE                   TO FILE-STATUS            58740003
587800     .                                                            58750003
587900*                                                                 58760003
588000 P8650-ENDBR-POINTER.                                             58770003
588100*                                                                 58780003
588200     EXEC CICS ENDBR                                              58790003
588300               DATASET(POINT-FILE-VIA-EMP)                        58800003
588400               RESP(WS-RESPONSE)                                  58810003
588500     END-EXEC                                                     58820003
588600     MOVE WS-RESPONSE                   TO FILE-STATUS            58830003
588700     .                                                            58840003
588800*                                                                 58850003
588810 PXXXX-JOB-OWNER.                                                 58860003
588820*                                                                 58870003
588830     SET ASGN-OWNER-REC TO TRUE                                   58880003
588840     MOVE ZEROES        TO ASGN-DATE-TIME                         58881003
588850     MOVE ASGNKEY1      TO ASGNJOB                                58882003
588860     EXEC CICS READ                                               58883003
588870               DATASET(ASGN-VIA-ASGNJOB)                          58884003
588880               INTO(WS-ASGN-FILE)                                 58885003
588890               LENGTH(ASGNJOB-RLGTH)                              58886003
588900               RIDFLD(ASGNJOB)                                    58887003
588901               KEYLENGTH(ASGNJOB-KLGTH)                           58888003
588902               RESP(WS-RESPONSE)                                  58889003
588903     END-EXEC                                                     58890003
588904     MOVE WS-RESPONSE TO FILE-STATUS                              58890103
588905     IF NOT SUCCESS                                               58890203
588906        IF NO-RECORD-FND OR END-OF-FILE                           58890303
588907           MOVE ZEROES  TO ASGN-EMP-NO                            58890403
588908        ELSE                                                      58890503
588909           MOVE 'PXXXX-JO' TO ERR-PARAGRAPH                       58890603
588910           MOVE ASGNJOB    TO ERR-KEY                             58890703
588920           PERFORM P9999-GOT-PROBLEM                              58890803
588930        END-IF                                                    58890903
588940     END-IF.                                                      58891003
588950*                                                                 58892003
588960 PXXXX-LATEST-TEMP.                                               58893003
588970*                                                                 58894003
588980     MOVE SPACES       TO SAVE-ASGN-AREA                          58895003
588990     SET ASGN-TEMP-REC TO TRUE                                    58896003
589000     MOVE ZEROES       TO ASGN-DATE-TIME                          58897003
589100     MOVE ASGNKEY1     TO ASGNJOB                                 58898003
589200                          XXXX-ASGNKEY1                           58899003
589300     EXEC CICS STARTBR                                            58900003
589400               DATASET(ASGN-VIA-ASGNJOB)                          58910003
589500               RIDFLD(ASGNJOB)                                    58920003
589600               GTEQ                                               58930003
589700               RESP(WS-RESPONSE)                                  58940003
589800     END-EXEC                                                     58950003
589900     MOVE WS-RESPONSE TO FILE-STATUS                              58960003
590000     IF SUCCESS                                                   58970003
590100        MOVE 0 TO ASGN-DONE-CODE                                  58980003
590200        PERFORM UNTIL ASGN-DONE                                   58990003
590300           EXEC CICS READNEXT                                     59000003
590400                     DATASET(ASGN-VIA-ASGNJOB)                    59010003
590500                     INTO(ASGN-AREA)                              59020003
590600                     LENGTH(ASGNJOB-RLGTH)                        59030003
590700                     RIDFLD(ASGNJOB)                              59040003
590800                     KEYLENGTH(ASGNJOB-KLGTH)                     59050003
590900                     RESP(WS-RESPONSE)                            59060003
591000           END-EXEC                                               59070003
591100           MOVE WS-RESPONSE TO FILE-STATUS                        59080003
591200           IF SUCCESS                                             59090003
591300              IF XX-ASGN-DIST = ASGN-DIST                         59100003
591400                 AND XX-ASGN-SUB-DIST = ASGN-SUB-DIST             59110003
591500                 AND XX-ASGN-JOB = ASGN-AJ-JOB OF ASGN-ASSIGNMENT 59120003
591600                 AND ASGN-TEMP-REC                                59130003
591700                 MOVE ASGN-AREA TO SAVE-ASGN-AREA                 59140003
591800              ELSE                                                59150003
591900                 SET ASGN-DONE TO TRUE                            59160003
592000              END-IF                                              59170003
592100           ELSE                                                   59180003
592200              IF NO-RECORD-FND OR END-OF-FILE                     59190003
592300                 SET ASGN-DONE TO TRUE                            59200003
592400              ELSE                                                59210003
592500                 SET ASGN-DONE TO TRUE                            59220003
592600                 MOVE 'PXXXX-LT' TO ERR-PARAGRAPH                 59230003
592700                 MOVE ASGNJOB    TO ERR-KEY                       59240003
592800                 PERFORM P9999-GOT-PROBLEM                        59250003
592900              END-IF                                              59260003
593000           END-IF                                                 59270003
593100        END-PERFORM                                               59280003
593200     ELSE                                                         59290003
593300        IF NO-RECORD-FND OR END-OF-FILE                           59300003
593400           MOVE ZERO TO ASGN-EMP-NO                               59310003
593500        ELSE                                                      59320003
593600           MOVE 'PXXXX' TO ERR-PARAGRAPH                          59330003
593700           MOVE ASGNJOB TO ERR-KEY                                59340003
593800           PERFORM P9999-GOT-PROBLEM                              59350003
593900        END-IF                                                    59360003
594000     END-IF                                                       59370003
594100     EXEC CICS ENDBR                                              59380003
594200               DATASET(ASGN-VIA-ASGNJOB)                          59390003
594300               RESP(WS-RESPONSE)                                  59400003
594400     END-EXEC                                                     59410003
594500     IF SAVE-ASGN-AREA > SPACE                                    59420003
594600        MOVE SAVE-ASGN-AREA TO ASGN-AREA                          59430003
594700     ELSE                                                         59440003
594800        MOVE ZERO TO ASGN-EMP-NO                                  59450003
594900     END-IF.                                                      59460003
595000*                                                                 59470003
595100 PXXXX-ON-DUTY-EMP.                                               59480003
595200*                                                                 59490003
595300     SET ASGN-ON-DUTY-REC TO TRUE                                 59500003
595400     MOVE ZEROES          TO ASGN-DATE-TIME                       59510003
595500     MOVE ASGNKEY1        TO ASGNJOB                              59520003
595600     EXEC CICS READ                                               59530003
595700               DATASET(ASGN-VIA-ASGNJOB)                          59540003
595800               INTO(ASGN-AREA)                                    59550003
595900               LENGTH(ASGNJOB-RLGTH)                              59560003
596000               RIDFLD(ASGNJOB)                                    59570003
596100               KEYLENGTH(ASGNJOB-KLGTH)                           59580003
596200               RESP(WS-RESPONSE)                                  59590003
596300     END-EXEC                                                     59600003
596400     MOVE WS-RESPONSE TO FILE-STATUS                              59610003
596500     IF NOT SUCCESS                                               59620003
596600        IF NO-RECORD-FND OR END-OF-FILE                           59630003
596700           MOVE ZERO TO ASGN-EMP-NO                               59640003
596800        ELSE                                                      59650003
596900           MOVE 'PXXXX-ODE' TO ERR-PARAGRAPH                      59660003
597000           MOVE ASGNJOB     TO ERR-KEY                            59670003
597100           PERFORM P9999-GOT-PROBLEM                              59680003
597200        END-IF                                                    59690003
597300     END-IF.                                                      59700003
597400*                                                                 59710003
597500 PXXXX-JOB-OWNED.                                                 59720003
597600*                                                                 59730003
597700     MOVE '1'      TO ASGN-EMP-NO-REC-TYPE                        59740003
597800     MOVE ZEROES   TO ASGN-EMP-DATE-TIME                          59750003
597900     MOVE ASGNKEY2 TO ASGNEMP                                     59760003
598000     MOVE SPACES   TO SAVE-NORM-ASGN-KEY                          59770003
598100     EXEC CICS READ                                               59780003
598200               DATASET(ASGN-VIA-ASGNEMP)                          59790003
598300               INTO(WS-ASGN-FILE)                                 59800003
598400               LENGTH(ASGNEMP-RLGTH)                              59810003
598500               RIDFLD(ASGNEMP)                                    59820003
598600               KEYLENGTH(ASGNEMP-KLGTH)                           59830003
598700               RESP(WS-RESPONSE)                                  59840003
598800     END-EXEC                                                     59850003
598900     MOVE WS-RESPONSE TO FILE-STATUS                              59860003
599000     IF SUCCESS                                                   59870003
599100        MOVE ASGNKEY1 TO SAVE-NORM-ASGN-KEY                       59880003
599200     ELSE                                                         59890003
599300        IF NO-RECORD-FND OR END-OF-FILE                           59900003
599400           MOVE SPACE TO ASGN-ASSIGNMENT                          59910003
599500        ELSE                                                      59920003
599600           MOVE 'PXXXX-JO' TO ERR-PARAGRAPH                       59930003
599700           MOVE ASGNEMP    TO ERR-KEY                             59940003
599800           PERFORM P9999-GOT-PROBLEM                              59950003
599900        END-IF                                                    59960003
600000     END-IF.                                                      59970003
600100*                                                                 59980003
600200 PXXXX-LATEST-TEMP-JOB.                                           59990003
600300*                                                                 60000003
600400     MOVE '2'           TO ASGN-EMP-NO-REC-TYPE                   60010003
600500     MOVE ZEROES        TO ASGN-EMP-DATE-TIME                     60020003
600600     MOVE ASGNKEY2      TO ASGNEMP                                60030003
600700                           XXXX-ASGNKEY2                          60040003
600800     MOVE SPACES        TO ASGN-AREA                              60050003
600900                           SAVE-TEMP-ASGN-KEY                     60060003
601000     MOVE XXXX-ASGNKEY2 TO ASGNKEY2                               60070003
601100     MOVE ASGN-AREA     TO SAVE-ASGN-AREA                         60080003
601200     EXEC CICS STARTBR                                            60090003
601300               DATASET(ASGN-VIA-ASGNEMP)                          60100003
601400               RIDFLD(ASGNEMP)                                    60110003
601500               GTEQ                                               60120003
601600               RESP(WS-RESPONSE)                                  60130003
601700     END-EXEC                                                     60140003
601800     MOVE WS-RESPONSE TO FILE-STATUS                              60150003
601900     IF SUCCESS                                                   60160003
602000        MOVE 0 TO ASGN-DONE-CODE                                  60170003
602100        PERFORM UNTIL ASGN-DONE                                   60180003
602200           EXEC CICS READNEXT                                     60190003
602300                     DATASET(ASGN-VIA-ASGNEMP)                    60200003
602400                     INTO(ASGN-AREA)                              60210003
602500                     LENGTH(ASGNEMP-RLGTH)                        60220003
602600                     RIDFLD(ASGNEMP)                              60230003
602700                     KEYLENGTH(ASGNEMP-KLGTH)                     60240003
602800                     RESP(WS-RESPONSE)                            60250003
602900           END-EXEC                                               60260003
603000           MOVE WS-RESPONSE TO FILE-STATUS                        60270003
603100           IF SUCCESS                                             60280003
603200              IF ASGN-EMP-NO = XX-ASGN-EMP                        60290003
603300                 AND ASGN-EMP-NO-REC-TYPE = '2'                   60300003
603400                 MOVE ASGN-AREA TO SAVE-ASGN-AREA                 60310003
603500                 MOVE ASGNKEY1  TO SAVE-TEMP-ASGN-KEY             60320003
603600              ELSE                                                60330003
603700                 SET ASGN-DONE  TO TRUE                           60340003
603800              END-IF                                              60350003
603900           ELSE                                                   60360003
604000              IF NO-RECORD-FND OR END-OF-FILE                     60370003
604100                 MOVE SPACE TO ASGN-ASSIGNMENT                    60380003
604200                 SET ASGN-DONE TO TRUE                            60390003
604300              ELSE                                                60400003
604400                 SET ASGN-DONE TO TRUE                            60410003
604500                 MOVE 'PXXXX-LTJ' TO ERR-PARAGRAPH                60420003
604600                 MOVE ASGNEMP     TO ERR-KEY                      60430003
604700                 PERFORM P9999-GOT-PROBLEM                        60440003
604800              END-IF                                              60450003
604900           END-IF                                                 60460003
605000        END-PERFORM                                               60470003
605100        MOVE SAVE-ASGN-AREA TO ASGN-AREA                          60480003
605200     ELSE                                                         60490003
605300        IF NO-RECORD-FND OR END-OF-FILE                           60500003
605400           MOVE SPACE TO ASGN-ASSIGNMENT                          60510003
605500        ELSE                                                      60520003
605600           MOVE 'PXXXX' TO ERR-PARAGRAPH                          60530003
605700           MOVE ASGNEMP TO ERR-KEY                                60540003
605800           PERFORM P9999-GOT-PROBLEM                              60550003
605900        END-IF                                                    60560003
606000     END-IF                                                       60570003
606100     EXEC CICS ENDBR                                              60580003
606200               DATASET(ASGN-VIA-ASGNEMP)                          60590003
606300               RESP(WS-RESPONSE)                                  60600003
606400     END-EXEC.                                                    60610003
606500*                                                                 60620003
606600 PXXXX-JOB-ON-DUTY.                                               60630003
606700*                                                                 60640003
606800     MOVE '3'      TO ASGN-EMP-NO-REC-TYPE                        60650003
606900     MOVE ZEROES   TO ASGN-EMP-DATE-TIME                          60660003
607000     MOVE ASGNKEY2 TO ASGNEMP                                     60670003
607100     EXEC CICS READ                                               60680003
607200               DATASET(ASGN-VIA-ASGNEMP)                          60690003
607300               INTO(ASGN-AREA)                                    60700003
607400               LENGTH(ASGNEMP-RLGTH)                              60710003
607500               RIDFLD(ASGNEMP)                                    60720003
607600               KEYLENGTH(ASGNEMP-KLGTH)                           60730003
607700               RESP(WS-RESPONSE)                                  60740003
607800     END-EXEC                                                     60750003
607900     MOVE WS-RESPONSE TO FILE-STATUS                              60760003
608000     IF NOT SUCCESS                                               60770003
608100        IF NO-RECORD-FND OR END-OF-FILE                           60780003
608200           MOVE SPACE TO ASGN-ASSIGNMENT                          60790003
608300        ELSE                                                      60800003
608400           MOVE 'PXXXX-JOD' TO ERR-PARAGRAPH                      60810003
608500           MOVE ASGNEMP TO ERR-KEY                                60820003
608600           PERFORM P9999-GOT-PROBLEM                              60830003
608700        END-IF                                                    60840003
608800     END-IF.                                                      60850003
608900*                                                                 60860003
609000 P8550-READ-MSTR2.                                                60870003
609100*                                                                 60880003
609200     EXEC CICS READ                                               60890003
609300               UPDATE                                             60900003
609400               DATASET(MSTR2-VIA-EMP-NBR)                         60910003
609500               INTO(WS-MSTR2)                                     60920003
609600               LENGTH(MSTR2ENBR-RLGTH)                            60930003
609700               RIDFLD(MSTR2NBRK)                                  60940003
609800               KEYLENGTH(MSTR2ENBR-KLGTH)                         60950003
609900               RESP(WS-RESPONSE)                                  60960003
610000     END-EXEC                                                     60970003
610100     MOVE WS-RESPONSE TO FILE-STATUS.                             60980003
610200*                                                                 60990003
610300 P8600-LINK-TO-P937.                                              61000003
610400*                                                                 61010003
610500     EXEC CICS LINK                                               61020003
610600               PROGRAM(P937-PGM)                                  61030003
610700               COMMAREA(P937-COMMAREA-PARMS)                      61040003
610800               LENGTH(P937-LGTH)                                  61050003
610900               RESP(WS-RESPONSE)                                  61060003
611000     END-EXEC                                                     61070003
611100     MOVE WS-RESPONSE                   TO FILE-STATUS            61080003
611200     IF NOT SUCCESS                                               61090003
611300        MOVE 'P8600-1'                  TO ERR-PARAGRAPH          61100003
611400        MOVE P937-AVL-EMP-KEY           TO ERR-KEY                61110003
611500        MOVE 'P937LINK'                 TO ERR-KEY                61120003
611600        PERFORM P9999-GOT-PROBLEM                                 61130003
611700     END-IF.                                                      61140003
611800*                                                                 61150003
611900 P8650-WRITE-TMSLIP.                                              61160003
612000*                                                                 61170003
612100     EXEC CICS LINK                                               61180003
612200               PROGRAM(P970-PGM)                                  61190003
612300               COMMAREA(P970-COMMAREA-PARMS)                      61200003
612400               LENGTH(P970-LGTH)                                  61210003
612500               RESP(WS-RESPONSE)                                  61220003
612600     END-EXEC                                                     61230003
612700     MOVE WS-RESPONSE                TO FILE-STATUS               61240003
612800     IF NOT SUCCESS                                               61250003
612900        MOVE 'P8650-1'               TO ERR-PARAGRAPH             61260003
613000        MOVE 'P970LINK'              TO ERR-KEY                   61270003
613100        PERFORM P9999-GOT-PROBLEM                                 61280003
613200     END-IF.                                                      61290003
613300*                                                                 61300003
613400 P8700-LINK-TO-P977.                                              61310003
613500*                                                                 61320003
613600     EXEC CICS LINK                                               61330003
613700               PROGRAM(P977-PGM)                                  61340003
613800               COMMAREA(P977-COMMAREA-PARMS)                      61350003
613900               LENGTH(P977-LGTH)                                  61360003
614000               RESP(WS-RESPONSE)                                  61370003
614100     END-EXEC                                                     61380003
614200     MOVE WS-RESPONSE        TO FILE-STATUS                       61390003
614300     IF NOT SUCCESS                                               61400003
614400        MOVE 'P8700-1'       TO ERR-PARAGRAPH                     61410003
614500        MOVE 'P977-LINK'     TO ERR-KEY                           61420003
614600        PERFORM P9999-GOT-PROBLEM                                 61430003
614700     END-IF                                                       61440003
614800     IF P977-ERROR-FOUND                                          61450003
614900        MOVE P977-MSGLOG-CODE       TO MSGLOG-CODE                61460003
615000        SET P917-DATE-ERROR         TO TRUE                       61470003
615100     END-IF.                                                      61480003
615200*                                                                 61490003
615300 P8750-LINK-942.                                                  61500003
615400*                                                                 61510003
615500     EXEC CICS LINK                                               61520003
615600               PROGRAM(P942-PGM)                                  61530003
615700               COMMAREA(P942-COMMAREA-PARMS)                      61540003
615800               LENGTH(P942-LGTH)                                  61550003
615900               RESP(WS-RESPONSE)                                  61560003
616000     END-EXEC                                                     61570003
616100     MOVE WS-RESPONSE    TO FILE-STATUS                           61580003
616200     IF NOT SUCCESS                                               61590003
616300        MOVE 'P8700-1'   TO ERR-PARAGRAPH                         61600003
616400        MOVE 'P942-LINK' TO ERR-KEY                               61610003
616500        PERFORM P9999-GOT-PROBLEM                                 61620003
616600     END-IF.                                                      61630003
616700                                                                  61640003
616800*                                                                 61650003
616900 P8800-WRITE-TASK.                                                61660003
617000*                                                                 61670003
617100     EXEC CICS LINK                                               61680003
617200               PROGRAM(P927-PGM)                                  61690003
617300               COMMAREA(P927-COMMAREA-PARMS)                      61700003
617400               LENGTH(P927-LGTH)                                  61710003
617500               RESP(WS-RESPONSE)                                  61720003
617600     END-EXEC                                                     61730003
617700     MOVE WS-RESPONSE TO FILE-STATUS                              61740003
617800     IF NOT SUCCESS                                               61750003
617900        MOVE 'P8800-1' TO ERR-PARAGRAPH                           61760003
618000        PERFORM P9999-GOT-PROBLEM                                 61770003
618100     END-IF.                                                      61780003
618200*                                                                 61790003
618300 P8900-WRITE-HISTORY.                                             61800003
618400*                                                                 61810003
618500     SET P943-EMPLOYEE-FUNCTION      TO TRUE                      61820003
618600     IF P917-FROM-00I                                             61830003
618700        MOVE 'AUTOEXEC' TO P943-USERID                            61840003
618800     ELSE                                                         61850003
618900        EXEC CICS ASSIGN                                          61860003
619000                  USERID(P943-USERID)                             61870003
619100        END-EXEC                                                  61880003
619200     END-IF                                                       61890003
619300                                                                  61900003
619400     IF  P917-SUPV-INIT          > SPACE                          61910003
619500         IF P943-LAYOFF-FUN                                       61920003
619600            MOVE P917-SUPV-INIT         TO P943-SUPV-AUTH         61930003
619700         ELSE                                                     61940003
619800            IF  P943-PENDING-LAYOFF-FUN                           61950003
619900                MOVE P917-SUPV-INIT     TO P943-FUN36-SUPV-AUTH   61960003
620000            END-IF                                                61970003
620100         END-IF                                                   61980003
620200     END-IF                                                       61990003
620300                                                                  62000003
620400     IF P917-CORRECTION-RECORD                                    62010003
620500        SET P943-CORRECTION-RECORD TO TRUE                        62020003
620600     END-IF                                                       62030003
620700                                                                  62040003
620800     EXEC CICS LINK                                               62050003
620900               PROGRAM(P943-PGM)                                  62060003
621000               COMMAREA(P943-COMMAREA-PARMS)                      62070003
621100               LENGTH(P943-LGTH)                                  62080003
621200               RESP(WS-RESPONSE)                                  62090003
621300     END-EXEC                                                     62100003
621400     MOVE WS-RESPONSE                TO FILE-STATUS               62110003
621500     IF NOT SUCCESS                                               62120003
621600        MOVE 'P8900-1'               TO ERR-PARAGRAPH             62130003
621700        MOVE 'P943'                  TO ERR-KEY                   62140003
621800        PERFORM P9999-GOT-PROBLEM                                 62150003
621900     END-IF                                                       62160003
622000     IF P943-RETURN-STATUS > ZEROES                               62170003
622100        MOVE 'P8900-2'               TO ERR-PARAGRAPH             62180003
622200        MOVE P943-RETURN-STATUS      TO FILE-STATUS               62190003
622300        MOVE 'P943-RETURN'           TO ERR-KEY                   62200003
622400        PERFORM P9999-GOT-PROBLEM                                 62210003
622500     END-IF.                                                      62220003
622600*                                                                 62230003
622700     EXEC SQL INCLUDE TIMEZONE END-EXEC.                          62240003
622800*                                                                 62250003
622900     EXEC SQL INCLUDE TZERROR END-EXEC.                           62260003
623000*                                                                 62270003
623100     EXEC SQL INCLUDE TIMEEDIT END-EXEC.                          62280003
623200*                                                                 62290003
623300     EXEC SQL INCLUDE DATEEDIT END-EXEC.                          62300003
623400*                                                                 62310003
623500     EXEC SQL INCLUDE BIFEDIT END-EXEC.                           62320003
623600*                                                                 62330003
623700 P8990-SEND-TO-EM.                                                62340003
623800*                                                                 62350003
623900     IF EMP-NBR      OF WS-MSTR NUMERIC                           62360003
624000        MOVE EMP-NBR OF WS-MSTR TO WS-EMP-NUM                     62370003
624100        MOVE SPACES             TO P929-COMMAREA-PARMS            62380003
624200        MOVE WS-EMP-NUM         TO P929-TO-EMP-NUM                62390003
624300        SET P929-TO-EM          TO TRUE                           62400003
624400        EXEC CICS LINK                                            62410003
624500                  PROGRAM(P929-PGM)                               62420003
624600                  COMMAREA(P929-COMMAREA-PARMS)                   62430003
624700                  LENGTH(P929-LGTH)                               62440003
624800                  RESP(WS-RESPONSE)                               62450003
624900        END-EXEC                                                  62460003
625000        MOVE WS-RESPONSE TO FILE-STATUS                           62470003
625100        IF NOT SUCCESS                                            62480003
625200           MOVE 'P8990-1' TO ERR-PARAGRAPH                        62490003
625300           PERFORM P9999-GOT-PROBLEM                              62500003
625400        END-IF                                                    62510003
625500     END-IF.                                                      62520003
625600*                                                                 62530003
625700 P9000-RETURN.                                                    62540003
625800*                                                                 62550003
625900     IF MSGLOG-CODE > SPACES                                      62560003
626000         MOVE MSGLOG-CODE         TO P917-MSGLOG-CODE             62570003
626100         PERFORM P9030-GET-MESSAGE                                62580003
626200         MOVE MSGLOG-MESSAGE-AREA TO P917-ERRORMSG                62590003
626300     END-IF                                                       62600003
626400                                                                  62610003
626500     MOVE P917-COMMAREA-PARMS TO DFHCOMMAREA                      62620003
626600     EXEC CICS RETURN END-EXEC.                                   62630003
626700*                                                                 62640003
626800 P9030-GET-MESSAGE.                                               62650003
626900*                                                                 62660003
627000     MOVE P917-SUB-CODE TO MSGLOG-SUB-CODE                        62670003
627100     EXEC CICS READ                                               62680003
627200               DATASET(MSGLOG-VIA-CODE)                           62690003
627300               INTO(MSGLOG-AREA)                                  62700003
627400               LENGTH(MSGLOG-RLGTH)                               62710003
627500               RIDFLD(MSGLOG-KEY)                                 62720003
627600               KEYLENGTH(MSGLOG-KLGTH)                            62730003
627700               RESP(WS-RESPONSE)                                  62740003
627800     END-EXEC                                                     62750003
627900     MOVE WS-RESPONSE TO FILE-STATUS                              62760003
628000     IF NOT SUCCESS                                               62770003
628100        IF P917-SUB-CODE = 1                                      62780003
628200           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            62790003
628300        ELSE                                                      62800003
628400           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            62810003
628500        END-IF                                                    62820003
628600     END-IF                                                       62830003
628700     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE                      62840003
628800     MOVE '-'             TO MSGLOG-MSG-SEP                       62850003
628900     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.                 62860003
629000*                                                                 62870003
629100 P9550-UPDATE-BT-TOTALS.                                          62880003
629200*                                                                 62890003
629300     MOVE SPACES                  TO PS45-COMMAREA-PARMS          62900003
629400     SET PS45-UPDATE-BT-TOTALS    TO TRUE                         62910003
629500     MOVE P917-EMP-NO             TO PS45-EMP-NBR                 62920003
629510     MOVE WORK-CURRENT-BANKTIME   TO PS45-AMOUNT                  62930003
629520     MOVE 'D'                     TO PS45-PAY-TYPE                62940003
629530     EXEC CICS LINK                                               62950003
629540               PROGRAM(PS45-PGM)                                  62951003
629550               COMMAREA(PS45-COMMAREA-PARMS)                      62952003
629560               LENGTH(LENGTH OF PS45-COMMAREA-PARMS)              62953003
629570               RESP(WS-RESPONSE)                                  62954003
629580     END-EXEC                                                     62955003
629590     MOVE WS-RESPONSE             TO FILE-STATUS                  62956003
629591     IF NOT SUCCESS                                               62957003
629592        MOVE 'P9550-1'            TO ERR-PARAGRAPH                62958003
629593        MOVE 'PS45LINK'           TO ERR-KEY                      62959003
629594        PERFORM P9999-GOT-PROBLEM                                 62959103
629595     END-IF.                                                      62959203
629596*                                                                 62959303
629597 P9800-GET-TIME-OFFSET.                                           62959403
629598*                                                                 62959503
629599     MOVE SPACES                TO CNTLKEY-AREA                   62959603
629600     SET DATE-TIME-OFFSET-REC   TO TRUE                           62959703
629610     EXEC CICS ASSIGN                                             62959803
629620               USERID(CNTL-DT-USERID)                             62959903
629630     END-EXEC                                                     62960003
629640     MOVE CNTLKEY-AREA          TO CNTLKEY                        62961003
629650     EXEC CICS READ                                               62962003
629660               DATASET(CNTL-FILE-VIA-CNTLKEY)                     62963003
629670               INTO(WS-CNTL-FILE)                                 62964003
629680               LENGTH(CNTLFILE-RLGTH)                             62965003
629690               RIDFLD(CNTLKEY)                                    62966003
629700               KEYLENGTH(CNTLFILE-KLGTH)                          62967003
629800               RESP(WS-RESPONSE)                                  62968003
629900     END-EXEC                                                     62969003
630000     MOVE WS-RESPONSE TO FILE-STATUS                              62970003
630100     IF NOT SUCCESS                                               62980003
630200        IF (NO-RECORD-FND OR END-OF-FILE)                         62990003
630300           MOVE '********'      TO CNTL-DT-USERID                 63000003
630400           MOVE CNTLKEY-AREA    TO CNTLKEY                        63010003
630500           EXEC CICS READ                                         63020003
630600                     DATASET(CNTL-FILE-VIA-CNTLKEY)               63030003
630700                     INTO(WS-CNTL-FILE)                           63040003
630800                     LENGTH(CNTLFILE-RLGTH)                       63050003
630900                     RIDFLD(CNTLKEY)                              63060003
631000                     KEYLENGTH(CNTLFILE-KLGTH)                    63070003
631100                     RESP(WS-RESPONSE)                            63080003
631200           END-EXEC                                               63090003
631300           MOVE WS-RESPONSE TO FILE-STATUS                        63100003
631400        ELSE                                                      63110003
631500           MOVE 'P9800-1'       TO ERR-PARAGRAPH                  63120003
631600           MOVE CNTLKEY         TO ERR-KEY                        63130003
631700           PERFORM P9999-GOT-PROBLEM                              63140003
631800        END-IF                                                    63150003
631900     END-IF                                                       63160003
632000     IF SUCCESS                                                   63170003
632100        MOVE CNTL-DT-ADD-SUBTRACT                                 63180003
632200                                TO WS-DT-OS-FUN                   63190003
632300        MOVE CNTL-DT-DAYS-OFFSET                                  63200003
632400                                TO WS-DT-OS-DAYS                  63210003
632500        MOVE CNTL-DT-HRMN-OFFSET                                  63220003
632600                                TO WS-DT-OS-HRMN                  63230003
632700     ELSE                                                         63240003
632800        MOVE SPACES             TO WS-DATE-TIME-OFFSET            63250003
632900        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     63260003
633000           MOVE 'P9800-2'       TO ERR-PARAGRAPH                  63270003
633100           MOVE CNTLKEY         TO ERR-KEY                        63280003
633200           PERFORM P9999-GOT-PROBLEM                              63290003
633300        END-IF                                                    63300003
633400     END-IF.                                                      63310003
633500*                                                                 63320003
633600 P9810-PROCESS-OFFSET.                                            63330003
633700*                                                                 63340003
633800     MOVE WS-DT-OS-FUN          TO PARM-CONV-TYPE                 63350003
633900     MOVE WS-DT-OS-DAYS         TO PARM-SEC-JULIAN-DAY            63360003
634000     MOVE WS-DT-OS-HRMN         TO PARM-SEC-HRMN                  63370003
634100     EXEC CICS LINK                                               63380003
634200               PROGRAM(P903-PGM)                                  63390003
634300               COMMAREA(DATE-CONVERSION-PARMS)                    63400003
634400               LENGTH(P903-LGTH)                                  63410003
634500               RESP(WS-RESPONSE)                                  63420003
634600     END-EXEC                                                     63430003
634700     MOVE WS-RESPONSE           TO FILE-STATUS                    63440003
634800     IF NOT SUCCESS                                               63450003
634900        MOVE 'P9810-1'          TO ERR-PARAGRAPH                  63460003
635000        MOVE 'P903'             TO ERR-KEY                        63470003
635100        PERFORM P9999-GOT-PROBLEM                                 63480003
635200     END-IF.                                                      63490003
635300*                                                                 63500003
635400 P9820-SNAPSHOT-XB.                                               63510003
635500*                                                                 63520003
635600     IF TEMP-ASGN-XB                                              63530003
635700        OR (NORM-ASGN-XB AND TEMPORARY-ASGNMT NOT > SPACE)        63540003
635800        MOVE SPACES             TO P913-COMMAREA-PARMS            63550003
635900        SET P913-SNAPSHOT-FUNCTION                                63560003
636000                                TO TRUE                           63570003
636100        IF TEMP-ASGN-XB                                           63580003
636200           MOVE TA-DIST         TO P913-TURN-DIST                 63590003
636300           MOVE TA-SUB-DIST     TO P913-TURN-SUB-DIST             63600003
636400           MOVE TA-CC           TO P913-TURN-CC                   63610003
636500        ELSE                                                      63620003
636600           MOVE NA-DIST         TO P913-TURN-DIST                 63630003
636700           MOVE NA-SUB-DIST     TO P913-TURN-SUB-DIST             63640003
636800           MOVE NA-CC           TO P913-TURN-CC                   63650003
636900        END-IF                                                    63660003
637000        EXEC CICS LINK                                            63670003
637100                  PROGRAM(P913-PGM)                               63680003
637200                  COMMAREA(P913-COMMAREA-PARMS)                   63690003
637300                  LENGTH(P913-LGTH)                               63700003
637400                  RESP(WS-RESPONSE)                               63710003
637500        END-EXEC                                                  63720003
637600        MOVE WS-RESPONSE        TO FILE-STATUS                    63730003
637700        IF NOT SUCCESS                                            63740003
637800           MOVE 'P9820-1'       TO ERR-PARAGRAPH                  63750003
637900           MOVE 'P913LINK'      TO ERR-KEY                        63760003
638000           PERFORM P9999-GOT-PROBLEM                              63770003
638100        END-IF                                                    63780003
638200     END-IF.                                                      63790003
638300*                                                                 63800003
638400 P9830-SNAPSHOT-UFP.                                              63810003
638500*                                                                 63820003
638600     IF TEMP-ASGN-UFP                                             63830003
638700        OR (NORM-ASGN-UFP AND TEMPORARY-ASGNMT NOT > SPACE)       63840003
638800        MOVE SPACES             TO P915-COMMAREA-PARMS            63850003
638900        SET P915-SNAPSHOT-FUNCTION                                63860003
639000                                TO TRUE                           63870003
639100        IF TEMP-ASGN-UFP                                          63880003
639200           MOVE TA-DIST         TO P915-TURN-DIST                 63890003
639300           MOVE TA-SUB-DIST     TO P915-TURN-SUB-DIST             63900003
639400           MOVE TA-POOL         TO P915-TURN-POOL                 63910003
639500        ELSE                                                      63920003
639600           MOVE NA-DIST         TO P915-TURN-DIST                 63930003
639700           MOVE NA-SUB-DIST     TO P915-TURN-SUB-DIST             63940003
639800           MOVE NA-POOL         TO P915-TURN-POOL                 63950003
639900        END-IF                                                    63960003
640000        EXEC CICS LINK                                            63970003
640100                  PROGRAM(P915-PGM)                               63980003
640200                  COMMAREA(P915-COMMAREA-PARMS)                   63990003
640300                  LENGTH(P915-LGTH)                               64000003
640400                  RESP(WS-RESPONSE)                               64010003
640500        END-EXEC                                                  64020003
640600        MOVE WS-RESPONSE        TO FILE-STATUS                    64030003
640700        IF NOT SUCCESS                                            64040003
640800           MOVE 'P9830-1'       TO ERR-PARAGRAPH                  64050003
640900           MOVE 'P915LINK'      TO ERR-KEY                        64060003
641000           PERFORM P9999-GOT-PROBLEM                              64070003
641100        END-IF                                                    64080003
641200     END-IF.                                                      64090003
641300*                                                                 64100003
641400 P9840-RETRIEVE-CNTL-INFO.                                        64110003
641500*                                                                 64120003
641600     EXEC CICS LINK                                               64130003
641700               PROGRAM (P956-PGM)                                 64140003
641800               COMMAREA(P956-COMMAREA-PARMS)                      64150003
641900               LENGTH  (P956-LGTH)                                64160003
642000               RESP    (WS-RESPONSE)                              64170003
642100     END-EXEC                                                     64180003
642200     MOVE WS-RESPONSE           TO FILE-STATUS                    64190003
642300     IF NOT SUCCESS                                               64200003
642400        MOVE 'P9840-1'          TO ERR-PARAGRAPH                  64210003
642500        MOVE P956-INPUT-PARMS   TO ERR-KEY                        64220003
642600        PERFORM P9999-GOT-PROBLEM                                 64230003
642700     END-IF.                                                      64240003
642800*                                                                 64250003
642900 P9850-LINK-TO-P931.                                              64260003
643000*                                                                 64270003
643100     EXEC CICS LINK                                               64280003
643200               PROGRAM(P931-PGM)                                  64290003
643300               COMMAREA(P931-COMMAREA-PARMS)                      64300003
643400               LENGTH(P931-LGTH)                                  64310003
643500               RESP(WS-RESPONSE)                                  64320003
643600     END-EXEC                                                     64330003
643700     MOVE WS-RESPONSE                   TO FILE-STATUS            64340003
643800     IF NOT SUCCESS                                               64350003
643900        MOVE 'P9850-1'                  TO ERR-PARAGRAPH          64360003
644000        MOVE 'P931LINK'                 TO ERR-KEY                64370003
644100        PERFORM P9999-GOT-PROBLEM                                 64380003
644200     END-IF                                                       64390003
644300     .                                                            64400003
644400*                                                                 64410003
644500 X9999-GOBACK.                                                    64420003
644600     GOBACK.                                                      64430003
644700*                                                                 64440003
644800     EXEC SQL INCLUDE EBCCUR02 END-EXEC.                          64450003
644900     EXEC SQL INCLUDE EBCINS   END-EXEC.                          64460003
645000     EXEC SQL INCLUDE EBCSHARE END-EXEC.                          64470003
645100*                                                                 64480003
645200     EXEC SQL INCLUDE GOTPROB  END-EXEC.                          64490003
645300*                                                                 64500003
