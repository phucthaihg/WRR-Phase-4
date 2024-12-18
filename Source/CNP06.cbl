000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.  CNP06.                                              00020000
000300***************************************************************** 00030000
000400*    THIS PROGRAM HANDLES MAINTENANCE OF THE EXTRABOARDS          00040000
000500***************************************************************** 00050000
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060000
000700*-------- ------- ------  --------------------------------------- 00070000
000800*11/13/90   ERW           CICS CONVERSION.                        00080000
000900*04/23/94   GER           TIMEZONE UPGRADES.                      00090000
001000*08/08/94   MMM           ADDED VARIABLE BROWSE (PF10).           00100000
001100*04/26/95   PLS           MODIFIED TO MAINTAIN FAST/SLOW          00110000
001200*                         EXTRABOARD.                             00120000
001300*05/16/95   LMB           DON'T ALLOW ADD EMPLOYEE TO SPAREBOARD  00130000
001400*                         IF THE PERSON IS WAITING TURN OR PEND-  00140000
001500*                         ING WAITING TURN.                       00150000
001600*09/18/95   MMM           NEW RESTRICTION CONTROL PROCESSING      00160000
001700*                         ADDED.  CNC0009.                        00170000
001800*05/03/96   FLW  CNC0006  DEFINITION OF WORK WEEK.                00180000
001900*09/18/96   RCW  CNC0006  USE P931-MASTER-INQUIRY ON CALLING 931  00190000
002000*09/27/96   RCW  CNC0077  NORTHERN QUEBEC SPAREBOARD - PHASE 2    00200000
002100*                         ADD VIEW TIME, XCTL TO CNP06S AND       00210000
002200*                         SCHEDULED TURN FIELDS.                  00220000
002300*01/16/97   WLW  CNC0132  SCREEN SECURITY                         00230000
002400*03/30/98   AMF  CNC0155  ENHANCED TRACKING CHANGES.              00240000
002500*04/03/98   AMF           ADDED HELP LOGIC.                       00250000
002600*04/03/98   WLW  CNC0155  REVERSED ENHANCED TRACKING LOGIC        00260000
002700*04/09/98   GBJ  Y2K      REMOVE WRITE TO HISTORY AND             00270000
002800*                         REPLACE WITH A CALL TO P943.            00280000
002900*05/07/98   LPS  Y2K      Y2K MODIFICATIONS FOR THE TASK LIST.    00290000
003000*08/20/98   LPS  Y2K      YEAR 2000 SWEEP.                        00300000
003100*12/23/98   KMW           ALWAYS SEND DAY AND TIME TO CNP912.     00310000
003200*02/24/99   NJB           ZAP LOGIC AND DISPLAY ALL ON            00320000
003300*                         BOARD ON 12/31/99 AND 01/01/00          00330000
003400*04/19/99   AJK  CNC0227  RECOMPILE FOR NEW P997COMM              00340000
003500*04/23/99   AJK  CNC0228  ADDED SNAPSHOT LOGIC.                   00350000
003600*05/14/99   SAM  CNC0216  STATUS/REASON CODE ENHANCEMENT.         00360000
003700*05/21/99   NJB           ASGN SEN                                00370000
003800*12/11/99   MOO  CNC0183  ADD CHECK FOR MTOY & MTOR.              00380000
003900*05/03/00   AJK  CNC0183  REMOVE CHECK FOR MTOR & MTOY.           00390000
004000*07/07/00   PLS  CNC0297  ADD FUNCTION TO DISPLAY SENIORITY AND   00400000
004100*                         QUALIFICATION CODES.                    00410000
004200*                                                                 00420000
004300*02/05/01   TJR        A) WHEN DISPLAYING THE FAST/SLOW           00430000
004400*                         SPAREBOARD USING THE BOARD TYPE OF 'Q', 00440000
004500*                         WE WERE ONLY GETTING THE 1ST REC. THIS  00450000
004600*                         HAPPENED BECAUSE WE WERE USING A CONTROL00460000
004700*                         REC FIELD, AND THE ENTIRE CONTROL REC   00470000
004800*                         CHANGES TO READ FOR QUALIFICATION TYPE. 00480000
004900*                         SO, FOR ALL OCCURANCES IS PGM, NOT JUST 00490000
005000*                         WHEN PROCESSING BOARD TYPE OF 'Q',      00500000
005100*                         SWITCH FRM FASTSLOW-XB TO WS-FASTSLOW-XB00510000
005200*                         WHICH IS CREATED AFTER THE CONTROL FILE 00520000
005300*                         IS READ FOR THE 1ST TIME.               00530000
005400*                                                                 00540000
005500*                      B) WHEN DISPLAYING THE SPAREBOARD USING THE00550000
005600*                         BOARD TYPE 'S' OPTION, WE WERE LOOSING  00560000
005700*                         1 REC (THE 21'ST) FROM PAGE TO PAGE.    00570000
005800*                         IN RTN P1230-CHECK-EMPLOYEE-SETUP, CHG  00580000
005900*                         CODE TO NOT SAVE THE 21'S REC'S EMP NUM 00590000
006000*                         FOR LATER PROCESSING.                   00600000
006100*                                                                 00610000
006110*02/24/03   JAJ           DON'T ALLOW TURN TO BE CUT IF EXTRABOARD00611000
006120*                         JOB SCHEDULES EXIST.                    00612000
006130*04/15/03   SAM  C477     CLEARED OUT EB-AWARDS-TIE-BREAKER       00613000
006131*09/09/03   AJK  CNC0378  SCHEDULED SPAREBOARD ENHANCEMENT - ADDED00613100
006132*                         LINK TO NEW EXTENDED SCHEDULE SCREEN -  00613200
006133*                         PSTS06E.                                00613300
006134*10/04/04   KJS  CNC0386A ADD DAY ASSOCIATED WITH PERSONAL REST   00613405
006140***************************************************************** 00614000
006150 ENVIRONMENT DIVISION.                                            00615000
006160 CONFIGURATION SECTION.                                           00616000
006170 SOURCE-COMPUTER.  IBM-9370.                                      00617000
006180 OBJECT-COMPUTER.  IBM-9370.                                      00618000
006190 DATA DIVISION.                                                   00619000
006200 WORKING-STORAGE SECTION.                                         00620000
006300 01  FILLER                      PIC X(10)  VALUE 'PGM06 W/S'.    00630000
006400 01  P06-COMM-LGTH               PIC S9(4)  COMP VALUE +1570.     00640000
006500                                                                  00650000
006600 01  WS-SUBSCRIPTS.                                               00660000
006700     02  NAME-SUB                PIC 99    VALUE ZERO.            00670000
006800     02  FUNC-SUB                PIC 99    VALUE ZERO.            00680000
006900     02  TURN-SUB                PIC 99    VALUE ZERO.            00690000
007000     02  QUAL-SUB                PIC 99    VALUE ZERO.            00700000
007100     02  I                       PIC 99    VALUE ZERO.            00710000
007200     02  SUB1                    PIC 99    VALUE ZERO.            00720000
007300     02  X1                      PIC 99    VALUE ZERO.            00730000
007400     02  ARRAY-MAX               PIC 99    VALUE 20.              00740000
007500     02  SEN-SUB                 PIC 99    VALUE ZERO.            00750000
007600     02  SEN-MAX                 PIC 99    VALUE 20.              00760000
007700     02  DUP-SUB                 PIC 9(3)  VALUE ZERO.            00770000
007800     02  DUP-MAX                 PIC 9(3)  VALUE 100.             00780000
007900     02  WS-REST-SUB             PIC S9(4) COMP VALUE ZERO.       00790000
008000                                                                  00800000
008100 01  WS-FLAGS.                                                    00810000
008200     02  WS-PROTECTION-FLAG      PIC X     VALUE '0'.             00820000
008300         88  PROTECTION-IS-IN-EFFECT       VALUE '1'.             00830000
008400     02  WS-DAILY-MARK-FLAG      PIC X     VALUE '0'.             00840000
008500         88  DAILY-MARK-YARD               VALUE '1'.             00850000
008600     02  WS-TAG-FOUND-FLAG       PIC X     VALUE '0'.             00860000
008700         88  TAG-FOUND                     VALUE '1'.             00870000
008800     02  SCREEN-FLAG             PIC X     VALUE '0'.             00880000
008900         88  CONTINUE-SCREEN               VALUE '0'.             00890000
009000         88  CREATE-SCREEN                 VALUE '1'.             00900000
009100         88  SEND-BUFFER                   VALUE '2'.             00910000
009200     02  WS-ERROR-FLAG           PIC 9     VALUE ZERO.            00920000
009300         88  ERRORS-FOUND                  VALUE 1.               00930000
009400     02  DONE-CODE               PIC 9     VALUE 0.               00940000
009500         88  DONE                          VALUE 1.               00950000
009600     02  SEN-DONE-CODE           PIC 9     VALUE 0.               00960000
009700         88  SEN-DONE                      VALUE 1.               00970000
009800     02  QUAL-DONE-CODE          PIC 9     VALUE 0.               00980000
009900         88  QUAL-DONE                     VALUE 1.               00990000
010000     02  TAG-DONE-CODE           PIC 9     VALUE 0.               01000000
010100         88  TAG-DONE                      VALUE 1.               01010000
010200     02  FUNCTION-FOUND-FLAG     PIC X     VALUE '0'.             01020000
010300         88  FUNCTION-FOUND                VALUE '1'.             01030000
010400     02  GOT-EMPLOYEE-FLAG       PIC X     VALUE '0'.             01040000
010500         88  GOT-EMPLOYEE                  VALUE '1'.             01050000
010600     02  DUP-EMP-FLAG            PIC X     VALUE '0'.             01060000
010700         88  NOT-DUP-EMP                   VALUE '0'.             01070000
010800         88  DUP-EMP                       VALUE '1'.             01080000
010900     02  GOT-TURN-FLAG           PIC X     VALUE '0'.             01090000
011000         88  GOT-TURN                      VALUE '1'.             01100000
011100     02  SEN-ROSTER-DONE-CODE    PIC X     VALUE '0'.             01110000
011200         88  SEN-ROSTER-DONE               VALUE '1'.             01120000
011300     02  WS-BOARD                PIC X     VALUE SPACE.           01130000
011400         88  TURN-BOARD                    VALUE 'T'.             01140000
011500         88  POSITION-BOARD                VALUE 'P' 'A'.         01150000
011600         88  ALL-POSITIONS                 VALUE 'A'.             01160000
011700         88  SENIORITY-BOARD               VALUE 'S'.             01170000
011800         88  QUAL-BOARD                    VALUE 'Q'.             01180000
011900         88  BOARD-OK                VALUES 'S' 'P' 'T' 'A' 'Q'.  01190000
012000     02  WS-ASGN-DONE-CODE       PIC X.                           01200000
012100         88  ASGN-DONE                     VALUE 'Y'.             01210000
012200     02  WS-TEMPS-DONE-CODE      PIC X.                           01220000
012300         88  TEMPS-DONE                    VALUE 'Y'.             01230000
012400     02  DONE-WITH-SEN-CODE      PIC 9     VALUE ZERO.            01240000
012500         88  DONE-WITH-SEN                 VALUE 1.               01250000
012600     02  ON-DUTY-OUT-TOWN-CODE   PIC X(10) VALUE '9999999999'.    01260000
012700         88  OUT-TOWN                      VALUE '0000000000'.    01270000
012800     02  TEST-QUAL.                                               01280000
012900         04  FILLER              PIC X(3)  VALUE SPACES.          01290000
013000         04  FILLER              PIC X(1)  VALUE SPACE.           01300000
013100             88  STEP-RATE-QUAL            VALUE '%'.             01310000
013200     02  TEMP-ASGN-XB-AUG-FLAG   PIC X     VALUE SPACE.           01320000
013300         88  TEMP-ASGN-XB-AUG              VALUE 'Y'.             01330000
013400     02  WS-EMP-ADDED-PROT-STATUS  PIC X.                         01340000
013500         88  EMP-ADDED-IS-PROTECTED        VALUE '1'.             01350000
013600         88  EMP-ADDED-HAS-LOST-PROTECTION VALUE '2'.             01360000
013700         88  EMP-ADDED-IS-NOT-PROTECTED    VALUE '3'.             01370000
013800     02  WS-EMP-CUT-PROT-STATUS   PIC X    VALUE SPACE.           01380000
013900         88  EMP-CUT-IS-PROTECTED          VALUE '1'.             01390000
014000         88  EMP-CUT-HAS-LOST-PROTECTION   VALUE '2'.             01400000
014100         88  EMP-CUT-IS-NOT-PROTECTED      VALUE '3'.             01410000
014200     02  WS-FUNCTION                      PIC X VALUE SPACE.      01420000
014300         88  INQUIRY-REQ                        VALUE 'I'.        01430000
014400         88  ADD-REQ                            VALUE 'A'.        01440000
014500         88  DELETE-REQ                         VALUES 'D', 'C'.  01450000
014600         88  REPOSITION-REQ                     VALUE 'R'.        01460000
014700         88  MOVE-REQ                           VALUE 'M'.        01470000
014800         88  TAG-REQ                            VALUE 'T'.        01480000
014900         88  SCHEDULE-REQ                       VALUE 'S'.        01490000
015000         88  FUNCTION-OK VALUES 'I' 'A' 'D' 'R' 'C' 'M' 'S' 'T'.  01500000
015100     02  WS-DISPLAY-EMP-FLAG              PIC X VALUE 'N'.        01510000
015200         88  DONT-DISPLAY-EMP                   VALUE 'N'.        01520000
015300         88  DISPLAY-EMP                        VALUE 'Y'.        01530000
015400                                                                  01540000
015500 01  WS-MISC.                                                     01550000
015600     02  WS-NBR-ASSIGNED              PIC 9(003) VALUE ZEROS.     01560000
015700     02  WS-EMP-TO-REGAIN-PROTECTION  PIC X(9) VALUE SPACE.       01570000
015800     02  WS-EMP-TO-LOSE-PROTECTION    PIC X(9) VALUE SPACE.       01580000
015900     02  WS-HOLD-EMP-TO-BE-CUT        PIC X(9)  VALUE SPACE.      01590000
016000     02  AD-NBR                       PIC 99    VALUE ZERO.       01600000
016100     02  WS-EMP-ADDED-SEN             PIC X(8)  VALUE SPACE.      01610000
016200     02  WS-TURN-WITH-TAG             PIC X(10) VALUE SPACE.      01620000
016300     02  WS-VIEW-DATE-TIME-CENT.                                  01630000
016400         04  WS-VIEW-CENT             PIC X(02) VALUE SPACE.      01640000
016500         04  WS-VIEW-DATE-TIME.                                   01650000
016600             06  WS-VIEW-DATE         PIC X(06) VALUE SPACE.      01660000
016700             06  WS-VIEW-TIME         PIC X(04) VALUE SPACES.     01670000
016800             06  FILLER REDEFINES WS-VIEW-TIME.                   01680000
016900                 08 WS-VIEW-HOUR      PIC 99.                     01690000
017000                 08 WS-VIEW-MINUTE    PIC 99.                     01700000
017100*    02  WS-POS-DATE-TIME-TZ          PIC X(10) VALUE SPACES.     01710000
017200     02  WS-POS-CENT-DATE-TIME-TZ     PIC X(12) VALUE SPACES.     01720000
017300     02  WS-XB-TYPE                   PIC X(01) VALUE SPACES.     01730000
017400         88  WS-FASTSLOW-XB                     VALUE '7'.        01740000
017420     02  WS-NUM                       PIC 9(01) VALUE ZEROES.     01742003
017430     02  HOLD-RES-DATE-GREG           PIC 9(6).                   01743003
017500                                                                  01750000
017600 01  WS-TABLES.                                                   01760000
017700     02  WS-ASGN-SEN-AREA.                                        01770000
017800         03  WS-ASGN-SEN-ARRAY OCCURS 20.                         01780000
017900             05  WS-ASGN-SEN-RSTR     PIC X(004).                 01790000
018000             05  WS-ASGN-SEN-CC       PIC X(002).                 01800000
018100             05  FILLER               PIC X(004).                 01810000
018200                                                                  01820000
018300*    CNC0006 - FLW, 5/8/96, START                                 01830000
018400 01  WS-VARIABLE-LINE-1-HDR-FRENCH.                               01840000
018500     02  FILLER                  PIC X(3) VALUE 'POS'.            01850000
018600     02  FILLER                  PIC X    VALUE SPACE.            01860000
018700     02  FILLER                  PIC X(2) VALUE 'ST'.             01870000
018800     02  FILLER                  PIC X    VALUE SPACE.            01880000
018900     02  FILLER                  PIC X(2) VALUE 'ET'.             01890000
019000     02  FILLER                  PIC X    VALUE SPACE.            01900000
019100     02  FILLER                  PIC X(4) VALUE 'THSO'.           01910000
019200     02  FILLER                  PIC X    VALUE SPACE.            01920000
019300     02  FILLER                  PIC X(4) VALUE 'TSP '.           01930000
019400     02  FILLER                  PIC X    VALUE SPACE.            01940000
019500     02  FILLER                  PIC X(4) VALUE 'HMEU'.           01950000
019600     02  FILLER                  PIC X    VALUE SPACE.            01960000
019700     02  FILLER                  PIC X(4) VALUE ' SP '.           01970000
019800     02  FILLER                  PIC X(2) VALUE SPACE.            01980005
019900     02  FILLER                  PIC X(4) VALUE 'PERS'.           01990000
020000     02  FILLER                  PIC X(2) VALUE SPACE.            02000006
020100     02  FILLER                  PIC X(3) VALUE 'CNV'.            02010000
020200     02  FILLER                  PIC X(1) VALUE SPACE.            02020000
020300     02  FILLER                  PIC X(2) VALUE 'S#'.             02030000
020400     02  FILLER                  PIC X(1) VALUE SPACE.            02040000
020500     02  FILLER                  PIC X(2) VALUE 'O#'.             02050000
020700 01  WS-VARIABLE-LINE-2-HDR-FRENCH.                               02070000
020800     02  FILLER                  PIC X(3)  VALUE 'POS'.           02080000
020900     02  FILLER                  PIC X     VALUE SPACE.           02090000
021000     02  FILLER                  PIC X(2)  VALUE 'ST'.            02100000
021100     02  FILLER                  PIC X     VALUE SPACE.           02110000
021200     02  FILLER                  PIC X(8)  VALUE 'DAT-ANCE'.      02120000
021300     02  FILLER                  PIC X     VALUE SPACE.           02130000
021400     02  FILLER                  PIC X(8)  VALUE '  AFF   '.      02140000
021500     02  FILLER                  PIC X     VALUE SPACE.           02150000
021600     02  WS-VL2-HDR-BOARD-FRENCH PIC X(2)  VALUE SPACE.           02160000
021700     02  FILLER                  PIC X(19) VALUE SPACES.          02170000
021800 01  WS-VARIABLE-LINE-3-HDR-FRENCH.                               02180000
021900     02  FILLER                  PIC X(10) VALUE 'TMP SUR TB'.    02190000
022000     02  FILLER                  PIC X(10) VALUE '-CD FIN SV'.    02200000
022100     02  FILLER                  PIC X(4)  VALUE ' EQT'.          02210000
022200     02  FILLER                  PIC X(22) VALUE SPACES.          02220000
022300 01  WS-VARIABLE-LINE-4-HDR-FRENCH.                               02230000
022400     02  FILLER                  PIC X(3) VALUE 'POS'.            02240000
022500     02  FILLER                  PIC X    VALUE SPACE.            02250000
022600     02  FILLER                  PIC X(2) VALUE 'EN'.             02260000
022700     02  FILLER                  PIC X    VALUE SPACE.            02270000
022800     02  FILLER                  PIC X(2) VALUE 'CO'.             02280000
022900     02  FILLER                  PIC X    VALUE SPACE.            02290000
023000     02  FILLER                  PIC X(2) VALUE 'FO'.             02300000
023100     02  FILLER                  PIC X    VALUE SPACE.            02310000
023200     02  FILLER                  PIC X(2) VALUE 'SW'.             02320000
023300     02  FILLER                  PIC X    VALUE SPACE.            02330000
023400     02  FILLER                  PIC X(2) VALUE 'BK'.             02340000
023500     02  FILLER                  PIC X    VALUE SPACE.            02350000
023600     02  FILLER                  PIC X(2) VALUE 'YA'.             02360000
023700     02  FILLER                  PIC X(2) VALUE SPACES.           02370000
023800     02  FILLER                  PIC X(5) VALUE '-----'.          02380000
023900     02  FILLER                  PIC X(14) VALUE 'QUALIFICATIONS'.02390000
024000     02  FILLER                  PIC X(5) VALUE '-----'.          02400000
024100     02  FILLER                  PIC X(2) VALUE SPACES.           02410000
024200 01  WS-VARIABLE-LINE-1-HDR.                                      02420000
024300     02  FILLER                  PIC X(3) VALUE 'POS'.            02430000
024400     02  FILLER                  PIC X    VALUE SPACE.            02440000
024500     02  FILLER                  PIC X(2) VALUE 'ST'.             02450000
024600     02  FILLER                  PIC X    VALUE SPACE.            02460000
024700     02  FILLER                  PIC X(2) VALUE 'TG'.             02470000
024800     02  FILLER                  PIC X    VALUE SPACE.            02480000
024900     02  FILLER                  PIC X(4) VALUE 'MTOD'.           02490000
025000     02  FILLER                  PIC X    VALUE SPACE.            02500000
025100     02  FILLER                  PIC X(4) VALUE 'MTPD'.           02510000
025200     02  FILLER                  PIC X    VALUE SPACE.            02520000
025300     02  FILLER                  PIC X(4) VALUE 'USHR'.           02530000
025400     02  FILLER                  PIC X    VALUE SPACE.            02540000
025500     02  FILLER                  PIC X(4) VALUE ' PD '.           02550000
025600     02  FILLER                  PIC X(2) VALUE SPACE.            02560005
025700     02  FILLER                  PIC X(4) VALUE 'PERS'.           02570000
025800     02  FILLER                  PIC X(2) VALUE SPACE.            02580006
025900     02  FILLER                  PIC X(3) VALUE 'SHR'.            02590000
026000     02  FILLER                  PIC X(1) VALUE SPACE.            02600000
026100     02  FILLER                  PIC X(2) VALUE '#S'.             02610000
026200     02  FILLER                  PIC X(1) VALUE SPACE.            02620000
026300     02  FILLER                  PIC X(2) VALUE '#O'.             02630000
026500 01  WS-VARIABLE-LINE-2-HDR.                                      02650000
026600     02  FILLER                  PIC X(3)  VALUE 'POS'.           02660000
026700     02  FILLER                  PIC X     VALUE SPACE.           02670000
026800     02  FILLER                  PIC X(2)  VALUE 'ST'.            02680000
026900     02  FILLER                  PIC X     VALUE SPACE.           02690000
027000     02  FILLER                  PIC X(8)  VALUE 'SENDATE'.       02700000
027100     02  FILLER                  PIC X     VALUE SPACE.           02710000
027200     02  FILLER                  PIC X(8)  VALUE '  ASGN  '.      02720000
027300     02  FILLER                  PIC X     VALUE SPACE.           02730000
027400     02  WS-VL2-HDR-BOARD        PIC X(2)  VALUE SPACE.           02740000
027500     02  FILLER                  PIC X(9)  VALUE '   OFF   '.     02750000
027600     02  FILLER                  PIC X(8)  VALUE ' AV TIME'.      02760000
027700     02  FILLER                  PIC X(2)  VALUE SPACES.          02770000
027800 01  WS-VARIABLE-LINE-3-HDR.                                      02780000
027900     02  FILLER                  PIC X(10) VALUE 'BOARD TIME'.    02790000
028000     02  FILLER                  PIC X(10) VALUE '-TIE CODE '.    02800000
028100     02  FILLER                  PIC X(4)  VALUE ' TAG'.          02810000
028200     02  FILLER                  PIC X(22) VALUE SPACES.          02820000
028300 01  WS-VARIABLE-LINE-4-HDR.                                      02830000
028400     02  FILLER                  PIC X(3) VALUE 'POS'.            02840000
028500     02  FILLER                  PIC X    VALUE SPACE.            02850000
028600     02  FILLER                  PIC X(2) VALUE 'EN'.             02860000
028700     02  FILLER                  PIC X    VALUE SPACE.            02870000
028800     02  FILLER                  PIC X(2) VALUE 'CO'.             02880000
028900     02  FILLER                  PIC X    VALUE SPACE.            02890000
029000     02  FILLER                  PIC X(2) VALUE 'FO'.             02900000
029100     02  FILLER                  PIC X    VALUE SPACE.            02910000
029200     02  FILLER                  PIC X(2) VALUE 'SW'.             02920000
029300     02  FILLER                  PIC X    VALUE SPACE.            02930000
029400     02  FILLER                  PIC X(2) VALUE 'BK'.             02940000
029500     02  FILLER                  PIC X    VALUE SPACE.            02950000
029600     02  FILLER                  PIC X(2) VALUE 'YA'.             02960000
029700     02  FILLER                  PIC X(2) VALUE SPACES.           02970000
029800     02  FILLER                  PIC X(5) VALUE '-----'.          02980000
029900     02  FILLER                  PIC X(14) VALUE 'QUALIFICATIONS'.02990000
030000     02  FILLER                  PIC X(5) VALUE '-----'.          03000000
030100     02  FILLER                  PIC X(2) VALUE SPACES.           03010000
030200                                                                  03020000
030300*    POSITION DISPLAY VARIABLE PORTION                            03030000
030400 01  WS-VARIABLE-LINE-1.                                          03040000
030500     02  WS-VL1-POS              PIC X(3).                        03050000
030600     02  FILLER                  PIC X.                           03060000
030700     02  WS-VL1-LO               PIC X(2).                        03070000
030800     02  FILLER                  PIC X.                           03080000
030900     02  WS-VL1-TAG              PIC X.                           03090000
031000     02  FILLER                  PIC X(2).                        03100000
031100     02  WS-VL1-MTOD             PIC X(4).                        03110000
031200     02  FILLER                  PIC X.                           03120000
031300     02  WS-VL1-MTPD             PIC X(4).                        03130000
031400     02  FILLER                  PIC X.                           03140000
031500     02  WS-VL1-USHR             PIC X(4).                        03150000
031600     02  FILLER                  PIC X.                           03160000
031700     02  WS-VL1-PD               PIC X(4).                        03170000
031800     02  FILLER                  PIC X.                           03180000
031900     02  WS-VL1-PERS             PIC X(4).                        03190000
032000     02  FILLER                  PIC X.                           03200000
032010     02  WS-VL1-PERS-DY          PIC X(2).                        03201005
032020     02  FILLER                  PIC X.                           03202005
032100     02  WS-VL1-SHRT             PIC XX.                          03210000
032200     02  FILLER                  PIC X.                           03220005
032300     02  WS-VL1-STT-STARTS       PIC XX.                          03230000
032400     02  FILLER                  PIC X.                           03240000
032500     02  WS-VL1-OVT-STARTS       PIC XX.                          03250000
032700                                                                  03270000
032800*    TURN DISPLAY VARIABLE PORTION                                03280000
032900 01  WS-VARIABLE-LINE-2 REDEFINES WS-VARIABLE-LINE-1.             03290000
033000     02  WS-VL2-POS              PIC X(3).                        03300000
033100     02  FILLER                  PIC X.                           03310000
033200     02  WS-VL2-LO               PIC X(2).                        03320000
033300     02  FILLER                  PIC X.                           03330000
033400     02  WS-VL2-SEN-LVL          PIC X(2).                        03340000
033500     02  WS-VL2-SEN-DATE         PIC X(6).                        03350000
033600     02  FILLER                  PIC X.                           03360000
033700     02  WS-VL2-ASGN             PIC X(8).                        03370000
033800     02  FILLER                  PIC X.                           03380000
033900     02  WS-VL2-BOARD            PIC X(2).                        03390000
034000     02  FILLER                  PIC X.                           03400000
034100     02  WS-VL2-REST-DAYS        PIC X(7).                        03410000
034200     02  FILLER REDEFINES WS-VL2-REST-DAYS.                       03420000
034300         04  WS-VL2-REST-DAY     PIC X OCCURS 7 TIMES.            03430000
034400     02  FILLER                  PIC X.                           03440000
034500     02  WS-VL2-AVAIL-START      PIC X(4).                        03450000
034600     02  WS-VL2-HYPHEN           PIC X.                           03460000
034700     02  WS-VL2-AVAIL-END        PIC X(4).                        03470000
034800     02  FILLER                  PIC X.                           03480000
034900 01  WS-VARIABLE-LINE-3 REDEFINES WS-VARIABLE-LINE-1.             03490000
035000     02  WS-VL3-DATE-TIME.                                        03500000
035100         03  WS-VL3-DATE.                                         03510000
035200             04  WS-VL3-YR       PIC XX.                          03520000
035300             04  WS-VL3-MO       PIC XX.                          03530000
035400             04  WS-VL3-DY       PIC XX.                          03540000
035500         03  WS-VL3-TIME.                                         03550000
035600             04  WS-VL3-HR       PIC XX.                          03560000
035700             04  WS-VL3-MN       PIC XX.                          03570000
035800     02  WS-VL3-DASH             PIC X.                           03580000
035900     02  WS-VL3-TIE-CODE         PIC X(4).                        03590000
036000     02  FILLER                  PIC X(7).                        03600000
036100     02  WS-VL3-TAG              PIC X.                           03610000
036200     02  FILLER                  PIC X(23).                       03620000
036300                                                                  03630000
036400*    QUAL/SEN DISPLAY VARIABLE PORTION                            03640000
036500 01  WS-VARIABLE-LINE-4.                                          03650000
036600     02  WS-VL4-POS              PIC X(3).                        03660000
036700     02  FILLER                  PIC X.                           03670000
036800     02  WS-VL4-EN               PIC X.                           03680000
036900     02  FILLER                  PIC X(2).                        03690000
037000     02  WS-VL4-CO               PIC X.                           03700000
037100     02  FILLER                  PIC X(2).                        03710000
037200     02  WS-VL4-FO               PIC X.                           03720000
037300     02  FILLER                  PIC X(2).                        03730000
037400     02  WS-VL4-SW               PIC X.                           03740000
037500     02  FILLER                  PIC X(2).                        03750000
037600     02  WS-VL4-BK               PIC X.                           03760000
037700     02  FILLER                  PIC X(2).                        03770000
037800     02  WS-VL4-YA               PIC X.                           03780000
037900     02  FILLER                  PIC X(3).                        03790000
038000     02  WS-VL4-QUAL-ENT OCCURS 5 TIMES.                          03800000
038100         04  WS-VL4-QUAL         PIC X(4).                        03810000
038200         04  FILLER              PIC X.                           03820000
038300     02  FILLER                  PIC X.                           03830000
038400                                                                  03840000
038500 01  WORK-HIST-TIME              PIC 9(14).                       03850000
038600 01  WS-VL3-LOCAL-DATE-TIME      PIC X(10) VALUE SPACE.           03860000
038700 01  DAY1                        PIC 99   VALUE ZERO.             03870000
038800                                                                  03880000
038900 01  CONFIRM-MSG                 PIC X(49)                        03890000
039000     VALUE 'PLACE A (Y) OR (N) NEXT TO NAME TO CONFIRM UPDATE'.   03900000
039100                                                                  03910000
039200 01  XB-WORK-KEY.                                                 03920000
039300     02  XB-WORK-DIST                  PIC XX    VALUE SPACE.     03930000
039400     02  XB-WORK-SUB-DIST              PIC XX    VALUE SPACE.     03940000
039500     02  XB-WORK-CC                    PIC XX    VALUE SPACE.     03950000
039600     02  XB-WORK-NBR                   PIC X(4)  VALUE SPACE.     03960000
039700     02  FILLER                        PIC X(12) VALUE SPACES.    03970000
039800 01  XB-WORK-KEY-X REDEFINES XB-WORK-KEY.                         03980000
039900     02  XB-WORK-KEY1.                                            03990000
040000         04  FILLER                    PIC X(6).                  04000000
040100         04  XB-WORK-KEY1-POSITION.                               04010000
040200             05  XB-WK-KEY1-ON-OFF     PIC 9(1).                  04020000
040300             05  XB-WK-KEY1-BOARD-TIME.                           04030000
040400                 06  XB-WK-KEY1-BOARD  PIC 9(1).                  04040000
040500                 06  XB-WK-KEY1-TIME.                             04050000
040600                     08  XB-WK-KEY1-DATE-TIME  PIC X(10).         04060000
040700                     08  XB-WK-KEY1-TIE-CODE   PIC X(4).          04070000
040800 01  XB-WORK-KEY2  REDEFINES XB-WORK-KEY.                         04080000
040900     02  FILLER                        PIC X(6).                  04090000
041000     02  XB-WORK-KEY2-TURN             PIC X(4).                  04100000
041100     02  FILLER                        PIC X(12).                 04110000
041200                                                                  04120000
041300 01  XB-WK-KEY1-WORK-AREA.                                        04130000
041400     02  XB-WK-KEY1-TIME-CENT.                                    04140000
041500         03  XB-WK-KEY1-CENT           PIC X(02).                 04150000
041600         03  XB-WK-KEY1-DT             PIC X(10).                 04160000
041700         03  XB-WK-KEY1-TC             PIC X(4).                  04170000
041800                                                                  04180000
041900 01  EB-POS-DATE-TIME-TIE-WA.                                     04190000
042000     02  EB-POS-DATE-TIME-CENT.                                   04200000
042100         03  EB-POS-CENT               PIC X(02).                 04210000
042200         03  EB-POS-DT                 PIC X(10).                 04220000
042300         03  EB-POS-TB                 PIC X(4).                  04230000
042400                                                                  04240000
042500 01  WORK-ASGNKEY1.                                               04250000
042600     02  WK-ASGN-JOB-TYPE              PIC X    VALUE 'X'.        04260000
042700     02  WK-ASGN-DIST                  PIC XX   VALUE SPACE.      04270000
042800     02  WK-ASGN-SUB-DIST              PIC XX   VALUE SPACE.      04280000
042900     02  WK-SWASSGN-ASGN.                                         04290000
043000       04  FILLER                      PIC XX   VALUE 'EX'.       04300000
043100       04  WK-ASGN-XB-TURN             PIC X(4) VALUE SPACE.      04310000
043200       04  WK-ASGN-CC                  PIC XX   VALUE SPACE.      04320000
043300     02  WK-ASGN-REC-TYPE              PIC X    VALUE '1'.        04330000
043400     02  WK-ASGN-DATE-TIME             PIC 9(10) VALUE 0.         04340000
043500                                                                  04350000
043600 01  WORK-ASGNKEY2.                                               04360000
043700     02  WK-ASGN-EMP-NO                PIC 9(9) VALUE 0.          04370000
043800     02  WK-ASGN-EMP-REC-TYPE          PIC X    VALUE SPACE.      04380000
043900     02  WK-ASGN-EMP-DATE-TIME         PIC 9(10) VALUE 0.         04390000
044000                                                                  04400000
044100 01  WS-SAVE-ASGN-FILE                 PIC X(128).                04410000
044200 01  WS-HOLD-ASGN-FILE                 PIC X(128).                04420000
044300 01  WS-HOLD-EB                        PIC X(128).                04430000
044400                                                                  04440000
044500 01  WORK-JOB-CODE.                                               04450000
044600     02  WK-JOB-CODE-POS12             PIC X(002) VALUE SPACES.   04460000
044700     02  WK-JOB-CODE-POS3456           PIC X(004) VALUE SPACES.   04470000
044800     02  WK-JOB-CODE-POS78             PIC X(002) VALUE SPACES.   04480000
044900                                                                  04490000
045000 01  NORMAL-ASGNMT-FLAG                PIC X     VALUE SPACE.     04500000
045100     88  NORM-ASGN-UFP                           VALUE 'U'.       04510000
045200     88  NORM-ASGN-XB                            VALUE 'X'.       04520000
045300     88  NORM-ASGN-AJ                            VALUE 'A'.       04530000
045400 01  NORMAL-ASGNMT.                                               04540000
045500     02  NA-DIST                       PIC XX   VALUE SPACE.      04550000
045600     02  NA-SUB-DIST                   PIC XX   VALUE SPACE.      04560000
045700     02  NA-AREA.                                                 04570000
045800       03  NA-1                        PIC X(6).                  04580000
045900       03  NA-2 REDEFINES NA-1.                                   04590000
046000         04  NA-POOL                   PIC XX.                    04600000
046100         04  NA-TURN                   PIC X(4).                  04610000
046200       03  NA-3 REDEFINES NA-1.                                   04620000
046300         04  NA-FILLER                 PIC XX.                    04630000
046400         04  NA-XB-TURN                PIC X(4).                  04640000
046500       03  NA-CC                       PIC XX.                    04650000
046600                                                                  04660000
046700 01  TEMPORARY-ASGNMT-FLAG             PIC X     VALUE SPACE.     04670000
046800     88  TEMP-ASGN-UFP                           VALUE 'U'.       04680000
046900     88  TEMP-ASGN-XB                            VALUE 'X'.       04690000
047000     88  TEMP-ASGN-AJ                            VALUE 'A'.       04700000
047100 01  TEMPORARY-ASGNMT.                                            04710000
047200     02  TA-DIST                        PIC XX   VALUE SPACE.     04720000
047300     02  TA-SUB-DIST                    PIC XX   VALUE SPACE.     04730000
047400     02  TA-AREA.                                                 04740000
047500       03  TA-1                         PIC X(6).                 04750000
047600       03  TA-2 REDEFINES TA-1.                                   04760000
047700         04  TA-POOL                    PIC XX.                   04770000
047800         04  TA-TURN                    PIC X(4).                 04780000
047900       03  TA-3 REDEFINES TA-1.                                   04790000
048000         04  TA-FILLER                  PIC XX.                   04800000
048100         04  TA-XB-TURN                 PIC X(4).                 04810000
048200       03  TA-CC                        PIC XX   VALUE SPACE.     04820000
048300                                                                  04830000
048400 01  ON-DUTY-ASGNMT-FLAG                PIC X    VALUE SPACE.     04840000
048500     88  ON-DUTY-UFP                             VALUE 'U'.       04850000
048600     88  ON-DUTY-AJ                              VALUE 'A'.       04860000
048700 01  ON-DUTY-ASGNMT.                                              04870000
048800     02  OD-DIST                        PIC XX   VALUE SPACE.     04880000
048900     02  OD-SUB-DIST                    PIC XX   VALUE SPACE.     04890000
049000     02  OD-AREA.                                                 04900000
049100       03  OD-1                         PIC X(6).                 04910000
049200       03  OD-2 REDEFINES OD-1.                                   04920000
049300         04  OD-POOL                    PIC XX.                   04930000
049400         04  OD-TURN                    PIC X(4).                 04940000
049500       03  OD-CC                        PIC XX   VALUE SPACE.     04950000
049600                                                                  04960000
049700 01  WS-FORMAT-NAME-AUG.                                          04970000
049800     02  FILLER                        PIC X(21).                 04980000
049900     02  WS-FORMAT-NAME-AUG-FIELD      PIC X(05).                 04990000
050000                                                                  05000000
050100 01  WS-FORMAT-NAME-PROT.                                         05010000
050200     02  FILLER                        PIC X(20).                 05020000
050300     02  WS-FORMAT-NAME-PROT-FIELD     PIC X(06).                 05030000
050400                                                                  05040000
050500 01  WORK-CNTLKEY.                                                05050000
050600     02  WK-CNTL-REC-TYPE              PIC XX    VALUE '08'.      05060000
050700     02  WK-CNTL-DIST                  PIC XX    VALUE SPACE.     05070000
050800     02  WK-CNTL-SUB-DIST              PIC XX    VALUE SPACE.     05080000
050900     02  WK-CNTL-XB                    PIC XX    VALUE SPACE.     05090000
051000     02  FILLER                        PIC X(12) VALUE SPACE.     05100000
051100                                                                  05110000
051200******************************************************************05120000
051300***                  TEMPORARY STORAGE QUEUE                   ***05130000
051400******************************************************************05140000
051500 01  P06TSQ-QUEUE-ITEM          PIC S9(4)  COMP VALUE +1.         05150000
051600 01  P06TSQ-MAP-QUEUE-ID.                                         05160000
051700     05  P06TSQ-MAP-QUEUE       PIC X(4)   VALUE '06M'.           05170000
051800     05  P06TSQ-MAP-TERM-ID     PIC X(4)   VALUE SPACES.          05180000
051900 01  P06TSQ-CA-QUEUE-ID.                                          05190000
052000     05  P06TSQ-CA-QUEUE        PIC X(4)   VALUE '06C'.           05200000
052100     05  P06TSQ-CA-TERM-ID      PIC X(4)   VALUE SPACES.          05210000
052200 01  P06TSQ-QLGTH               PIC S9(4)  COMP VALUE +1.         05220000
052300***************************************************************** 05230000
052400***                 I/O STATUS CHECK FIELDS                       05240000
052500***************************************************************** 05250000
052600 01  WS-RESPONSE             PIC S9(8) COMP VALUE ZEROES.         05260000
052700 01  FILE-STATUS             PIC 9(4)  VALUE ZEROES.              05270000
052800     COPY IOCODES.                                                05280000
052900***************************************************************** 05290000
053000***                    COMMAREA COPYBOOKS                         05300000
053100***************************************************************** 05310000
053200     COPY PSTCOMM.                                                05320000
053300     COPY P998COMM.                                               05330000
053400     COPY P997COMM.                                               05340000
053500     COPY P06ECOMM.                                               05350000
053510     COPY P06SCOMM.                                               05351000
053600     COPY P06COMM.                                                05360000
053700     COPY PT48COMM.                                               05370000
053800***************************************************************** 05380000
053900***                     MAP AREA COPYBOOK                         05390000
054000***************************************************************** 05400000
054100     COPY PSM06RE.                                                05410000
054200***************************************************************** 05420000
054300***                   PROGRAM NAMES COPYBOOKS                     05430000
054400***************************************************************** 05440000
054500     COPY PSTCB03.                                                05450000
054600     COPY PSTCB06.                                                05460000
054700     COPY PSTCB06E.                                               05470000
054710     COPY PSTCB06S.                                               05471000
054800     COPY PSTCB997.                                               05480000
054900     COPY PSTCB998.                                               05490000
055000***************************************************************** 05500000
055100***                 CALLED ROUTINES COPYBOOKS.                    05510000
055200***************************************************************** 05520000
055300     COPY PSTERAR.                                                05530000
055400     COPY P903COMM.                                               05540000
055500     COPY P907COMM.                                               05550000
055600     COPY P911COMM.                                               05560000
055700     COPY P912COMM.                                               05570000
055800     COPY P913COMM.                                               05580000
055900     COPY P914COMM.                                               05590000
056000     COPY P931COMM.                                               05600000
056100     COPY P942COMM.                                               05610000
056200     COPY P943COMM.                                               05620000
056300     COPY P947COMM.                                               05630000
056400     COPY P956COMM.                                               05640000
056410     COPY PS42COMM.                                               05641002
056500***************************************************************** 05650000
056600***                     FILE COPYBOOKS                            05660000
056700***************************************************************** 05670000
056800     COPY WSMSTR.                                                 05680000
056900     COPY WSSEN.                                                  05690000
057000     COPY WSEB.                                                   05700000
057100     COPY WSTASK.                                                 05710000
057200     COPY WSCNTL.                                                 05720000
057300     COPY WSASGN.                                                 05730000
057400     COPY WSSWASGN.                                               05740000
057500     COPY WSQUAL.                                                 05750000
057600     COPY WSJS.                                                   05760000
057700***************************************************************** 05770000
057800***                     MISC. COPYBOOKS                           05780000
057900***************************************************************** 05790000
058000     COPY PSTKEYS.                                                05800000
058100     COPY PSTATTR.                                                05810000
058200     COPY WSEDDATE.                                               05820000
058300     COPY WSSYDTTM.                                               05830000
058400     COPY WSDAYWK.                                                05840000
058500     COPY WSBIF.                                                  05850000
058600     COPY WSMSG.                                                  05860000
058700     COPY WSZONE.                                                 05870000
058800     COPY WSBUFFER.                                               05880000
058900     COPY WSZAP.                                                  05890000
059000                                                                  05900000
059100 LINKAGE SECTION.                                                 05910000
059200 01  DFHCOMMAREA.                                                 05920000
059300     05  LINK-COMM-A           PIC X(170).                        05930000
059400     05  LINK-COMM-B           PIC X(1400).                       05940000
059500                                                                  05950000
059600 PROCEDURE DIVISION.                                              05960000
059700*                                                                 05970000
059800 P0000-MAINLINE.                                                  05980000
059900*                                                                 05990000
060000     EXEC CICS IGNORE                                             06000000
060100               CONDITION                                          06010000
060200               ERROR                                              06020000
060300     END-EXEC                                                     06030000
060400     EXEC CICS HANDLE                                             06040000
060500               ABEND                                              06050000
060600               LABEL(P9999-GOT-PROBLEM)                           06060000
060700     END-EXEC                                                     06070000
060800*                                                                 06080000
060900     COPY ABSTIME.                                                06090000
061000*                                                                 06100000
061100     IF EIBCALEN = ZERO                                           06110000
061200        PERFORM P9990-CLEAR-SCREEN                                06120000
061300     END-IF                                                       06130000
061400     IF EIBCALEN > PSTCOMM-LGTH                                   06140000
061500        MOVE LINK-COMM-A  TO PSTCOMM-AREA                         06150000
061600        MOVE LINK-COMM-B  TO P06CA-SCREEN-COMM                    06160000
061700     ELSE                                                         06170000
061800        MOVE LINK-COMM-A  TO PSTCOMM-AREA                         06180000
061900     END-IF                                                       06190000
062000     IF EIBTRNID NOT = P06-TRAN                                   06200000
062100        SET CREATE-SCREEN          TO TRUE                        06210000
062200        MOVE LOW-VALUES            TO PSTS06                      06220000
062300        IF EIBTRNID = P998-TRAN                                   06230000
062400           MOVE P998CA-CURSOR-POS TO EIBCPOSN                     06240000
062500           PERFORM P7010-READ-TSQUEUE                             06250000
062600           PERFORM P9000-SEND-MAP-AND-RETURN                      06260000
062700        END-IF                                                    06270000
062800        MOVE ZEROES           TO P06CA-LAST-SS                    06280000
062900        PERFORM PXXXX-SCREEN-SECURITY                             06290000
063000        EVALUATE TRUE                                             06300000
063100           WHEN EIBTRNID = P997-TRAN                              06310000
063200              SET ENTER-KEY           TO TRUE                     06320000
063300              MOVE P997CA-PASS-KEY(3:2) TO SCR06-DIST             06330000
063400              MOVE P997CA-PASS-KEY(5:2) TO SCR06-SUB-DIST         06340000
063500              MOVE P997CA-PASS-KEY(7:2) TO SCR06-CC               06350000
063600              MOVE 'P'                TO SCR06-BOARD              06360000
063700              MOVE 'I'                TO SCR06-FUNCTION           06370000
063800           WHEN EIBTRNID = P06E-TRAN                              06380000
063900              SET ENTER-KEY           TO TRUE                     06390000
064000              MOVE PSTCA-DIST         TO SCR06-DIST               06400000
064100              MOVE PSTCA-SUB-DIST     TO SCR06-SUB-DIST           06410000
064200              MOVE 'I'                TO SCR06-FUNCTION           06420000
064300              MOVE P06ECA-CC          TO SCR06-CC                 06430000
064400              MOVE P06ECA-BOARD       TO SCR06-BOARD              06440000
064500              MOVE P06ECA-YARD-ROAD   TO SCR06-YARD-ROAD          06450000
064600              MOVE P06ECA-VIEW-TIME   TO SCR06-VIEW-TIME          06460000
064610           WHEN EIBTRNID = P06S-TRAN                              06461000
064620              SET ENTER-KEY           TO TRUE                     06462000
064630              MOVE PSTCA-DIST         TO SCR06-DIST               06463000
064640              MOVE PSTCA-SUB-DIST     TO SCR06-SUB-DIST           06464000
064650              MOVE 'I'                TO SCR06-FUNCTION           06465000
064660              MOVE P06SCA-CC          TO SCR06-CC                 06466000
064670              MOVE P06SCA-BOARD       TO SCR06-BOARD              06467000
064680              MOVE P06SCA-YARD-ROAD   TO SCR06-YARD-ROAD          06468000
064690              MOVE P06SCA-VIEW-TIME   TO SCR06-VIEW-TIME          06469000
064700           WHEN OTHER                                             06470000
064800              MOVE PSTCA-DIST         TO SCR06-DIST               06480000
064900              MOVE PSTCA-SUB-DIST     TO SCR06-SUB-DIST           06490000
065000              MOVE 'P'                TO SCR06-BOARD              06500000
065100              MOVE 'I'                TO SCR06-FUNCTION           06510000
065200              MOVE -1                 TO SCR06-CC-CURSOR          06520000
065300              PERFORM P9000-SEND-MAP-AND-RETURN                   06530000
065400        END-EVALUATE                                              06540000
065500     ELSE                                                         06550000
065600        MOVE EIBAID   TO PF-CHECK                                 06560000
065700        IF EXIT-KEY                                               06570000
065800           PERFORM P9100-SETUP-SCR03                              06580000
065900        END-IF                                                    06590000
066000     END-IF                                                       06600000
066100     PERFORM P0100-PROCESS-INPUT                                  06610000
066200     PERFORM PXXXX-SCREEN-SECURITY                                06620000
066300     PERFORM P9000-SEND-MAP-AND-RETURN.                           06630000
066400*                                                                 06640000
066500 P0100-PROCESS-INPUT.                                             06650000
066600*                                                                 06660000
066700     IF EIBTRNID NOT = P997-TRAN                                  06670000
066800        AND NOT = P06S-TRAN                                       06680000
066810        AND NOT = P06E-TRAN                                       06681000
066900        MOVE P06-MAP-VERSION(PSTCA-SUB) TO P06-MAP                06690000
067000        EXEC CICS RECEIVE MAP(P06-MAP)                            06700000
067100                          MAPSET(P06-SET)                         06710000
067200                          INTO(PSTS06)                            06720000
067300                          RESP(WS-RESPONSE)                       06730000
067400        END-EXEC                                                  06740000
067500        MOVE WS-RESPONSE TO FILE-STATUS                           06750000
067600        IF NOT SUCCESS                                            06760000
067700           MOVE 'P0100-1' TO ERR-PARAGRAPH                        06770000
067800           PERFORM P9999-GOT-PROBLEM                              06780000
067900        END-IF                                                    06790000
068000     END-IF                                                       06800000
068100     IF PFKEY1                                                    06810000
068200        PERFORM P7005-WRITE-TSQUEUE                               06820000
068300        PERFORM P9500-SETUP-SCR998                                06830000
068400     END-IF                                                       06840000
068500     IF PFKEY10                                                   06850000
068600        PERFORM P9600-SETUP-SCR997                                06860000
068700     END-IF                                                       06870000
068800     IF NOT ENTER-KEY AND NOT PFKEY8 AND NOT PFKEY10              06880000
068900*            INVALID-FUNC-MSG                                     06890000
069000        MOVE 'I006' TO MSGLOG-CODE                                06900000
069100        MOVE -1 TO SCR06-FUNCTION-CURSOR                          06910000
069200        PERFORM P9000-SEND-MAP-AND-RETURN                         06920000
069300     END-IF                                                       06930000
069400     PERFORM P0200-RESET-ATTRIBUTES                               06940000
069500     PERFORM P8800-GET-CURRENT-TIME                               06950000
069600*                                                                 06960000
069700     MOVE WS-SYSTEM-DATE-TIME TO WORK-HIST-TIME                   06970000
069800*                                                                 06980000
069900     MOVE ZEROS TO DATE-CONVERSION-PARMS                          06990000
070000     SET PARM-CONV TO TRUE                                        07000000
070100     MOVE WS-LOCAL-DATE TO PARM-PRI-DATE-GREG                     07010000
070200     PERFORM P8700-CALL-DATE-ROUTINE                              07020000
070300     MOVE PARM-PRI-DAY-OF-WEEK TO DAY1                            07030000
070400     MOVE SPACES         TO SCR06-ERRORMSG                        07040000
070500     IF SCR06-CC NOT = P06CA-LAST-CC OR                           07050000
070600        SCR06-BOARD NOT = P06CA-LAST-BOARD OR                     07060000
070700        SCR06-YARD-ROAD NOT = P06CA-LAST-YARD-ROAD                07070000
070800          MOVE SPACE TO P06CA-LAST-FUNCTION                       07080000
070900     END-IF                                                       07090000
071000*                                                                 07100000
071100*    RETRIEVE SUB-DISTRICT CONTROL RECORD                         07110000
071200*                                                                 07120000
071300     MOVE SCR06-DIST     TO XB-WORK-DIST                          07130000
071400                            WK-ASGN-DIST                          07140000
071500                            WK-CNTL-DIST                          07150000
071600     MOVE SCR06-SUB-DIST TO XB-WORK-SUB-DIST                      07160000
071700                            WK-ASGN-SUB-DIST                      07170000
071800                            WK-CNTL-SUB-DIST                      07180000
071900     MOVE '02'           TO WK-CNTL-REC-TYPE                      07190000
072000     MOVE WORK-CNTLKEY   TO CNTLKEY                               07200000
072100     EXEC CICS READ                                               07210000
072200               DATASET(CNTL-FILE-VIA-CNTLKEY)                     07220000
072300               INTO(WS-CNTL-FILE)                                 07230000
072400               LENGTH(CNTLFILE-RLGTH)                             07240000
072500               RIDFLD(CNTLKEY)                                    07250000
072600               KEYLENGTH(CNTLFILE-KLGTH)                          07260000
072700               RESP(WS-RESPONSE)                                  07270000
072800     END-EXEC                                                     07280000
072900     MOVE WS-RESPONSE TO FILE-STATUS                              07290000
073000     IF NOT SUCCESS                                               07300000
073100        MOVE 'P0100-3' TO ERR-PARAGRAPH                           07310000
073200        MOVE CNTLKEY   TO ERR-KEY                                 07320000
073300        PERFORM P9999-GOT-PROBLEM                                 07330000
073400     END-IF                                                       07340000
073500     IF DAILY-MARKUP-ALLOWED                                      07350000
073600        SET DAILY-MARK-YARD TO TRUE                               07360000
073700     END-IF                                                       07370000
073800*                                                                 07380000
073900*    CHECK THE FUNCTION CODE FOR VALIDITY                         07390000
074000*                                                                 07400000
074100     MOVE SCR06-FUNCTION TO WS-FUNCTION                           07410000
074200     IF NOT FUNCTION-OK                                           07420000
074300        MOVE -1        TO SCR06-FUNCTION-CURSOR                   07430000
074400        MOVE REV-VIDEO TO SCR06-FUNCTION-HI                       07440000
074500*            INVALID-CODE-MSG                                     07450000
074600        MOVE 'I041' TO MSGLOG-CODE                                07460000
074700        PERFORM P9000-SEND-MAP-AND-RETURN                         07470000
074800     END-IF                                                       07480000
074900*                                                                 07490000
075000*    CHECK THE EXTRABOARD ID FOR VALIDITY                         07500000
075100*                                                                 07510000
075200     MOVE SCR06-CC     TO WK-ASGN-CC                              07520000
075300                          WK-CNTL-XB                              07530000
075400                          XB-WORK-CC                              07540000
075500     MOVE '08'         TO WK-CNTL-REC-TYPE                        07550000
075600     MOVE WORK-CNTLKEY TO CNTLKEY                                 07560000
075700     EXEC CICS READ                                               07570000
075800               DATASET(CNTL-FILE-VIA-CNTLKEY)                     07580000
075900               INTO(WS-CNTL-FILE)                                 07590000
076000               LENGTH(CNTLFILE-RLGTH)                             07600000
076100               RIDFLD(CNTLKEY)                                    07610000
076200               KEYLENGTH(CNTLFILE-KLGTH)                          07620000
076300               RESP(WS-RESPONSE)                                  07630000
076400     END-EXEC                                                     07640000
076500     MOVE WS-RESPONSE TO FILE-STATUS                              07650000
076600     IF NOT SUCCESS                                               07660000
076700        IF NO-RECORD-FND                                          07670000
076800*               'NO CONTROL RECORD EXISTS FOR THIS SPAREBOARD'    07680000
076900           MOVE 'N025' TO MSGLOG-CODE                             07690000
077000           MOVE -1 TO SCR06-CC-CURSOR                             07700000
077100           MOVE REV-VIDEO TO SCR06-CC-HI                          07710000
077200           PERFORM P9000-SEND-MAP-AND-RETURN                      07720000
077300        ELSE                                                      07730000
077400           MOVE 'P0100-4' TO ERR-PARAGRAPH                        07740000
077500           MOVE CNTLKEY   TO ERR-KEY                              07750000
077600           PERFORM P9999-GOT-PROBLEM                              07760000
077700        END-IF                                                    07770000
077800     END-IF                                                       07780000
077900     MOVE CNTL-POOL-ZAP-FLAG   TO WS-ZAP-FLAG                     07790000
078000     MOVE CNTL-XB-TYPE         TO WS-XB-TYPE                      07800000
078100*                                                                 07810000
078200*    CHECK THE BOARD FOR VALIDITY                                 07820000
078300*                                                                 07830000
078400     MOVE SCR06-BOARD    TO WS-BOARD                              07840000
078500     IF (ALL-POSITIONS AND                                        07850000
078600         NOT CNTL-XB-TRACK) OR                                    07860000
078700        NOT BOARD-OK                                              07870000
078800        MOVE -1               TO SCR06-BOARD-CURSOR               07880000
078900        MOVE REV-VIDEO        TO SCR06-BOARD-HI                   07890000
079000*            INVALID-CODE-MSG                                     07900000
079100        MOVE 'I041' TO MSGLOG-CODE                                07910000
079200        PERFORM P9000-SEND-MAP-AND-RETURN                         07920000
079300     END-IF                                                       07930000
079400*                                                                 07940000
079500*    CHECK THE YARD/ROAD FLAG FOR VALIDITY                        07950000
079600*    IT MUST BE USED FOR 'DUAL' AND FASTSLOW BOARDS. IT           07960000
079700*    CANNOT BE USED FOR ANY OTHER BOARD.                   **PLS  07970000
079800*                                                                 07980000
079900     IF DUAL-XB                                                   07990000
080000        IF SCR06-YARD-ROAD NOT = ('Y' AND 'R')                    08000000
080100           IF POSITION-BOARD                                      08010000
080200              OR ADD-REQ                                          08020000
080300              OR REPOSITION-REQ                                   08030000
080400              MOVE -1 TO SCR06-YARD-ROAD-CURSOR                   08040000
080500              MOVE REV-VIDEO TO SCR06-YARD-ROAD-HI                08050000
080600*                  INVALID-CODE-MSG                               08060000
080700              MOVE 'I041' TO MSGLOG-CODE                          08070000
080800              PERFORM P9000-SEND-MAP-AND-RETURN                   08080000
080900           END-IF                                                 08090000
081000        END-IF                                                    08100000
081100     ELSE                                                         08110000
081200        IF WS-FASTSLOW-XB                                         08120000
081300           IF SCR06-YARD-ROAD NOT = ('Y' AND 'R')                 08130000
081400              MOVE -1 TO SCR06-YARD-ROAD-CURSOR                   08140000
081500              MOVE REV-VIDEO TO SCR06-YARD-ROAD-HI                08150000
081600*                  INVALID-CODE-MSG                               08160000
081700              MOVE 'I041' TO MSGLOG-CODE                          08170000
081800              PERFORM P9000-SEND-MAP-AND-RETURN                   08180000
081900           END-IF                                                 08190000
082000        ELSE                                                      08200000
082100           IF SCR06-YARD-ROAD > SPACE                             08210000
082200              MOVE -1 TO SCR06-YARD-ROAD-CURSOR                   08220000
082300              MOVE REV-VIDEO TO SCR06-YARD-ROAD-HI                08230000
082400*                  INVALID-CODE-MSG                               08240000
082500              MOVE 'I041' TO MSGLOG-CODE                          08250000
082600              PERFORM P9000-SEND-MAP-AND-RETURN                   08260000
082700           END-IF                                                 08270000
082800        END-IF                                                    08280000
082900     END-IF                                                       08290000
083000                                                                  08300000
083100     IF SCR06-CC                    NOT  = P06CA-LAST-CC          08310000
083200        OR SCR06-BOARD              NOT  = P06CA-LAST-BOARD       08320000
083300        OR SCR06-YARD-ROAD          NOT  = P06CA-LAST-YARD-ROAD   08330000
083400        PERFORM P9820-SNAPSHOT-XB                                 08340000
083500        PERFORM P0300-GET-NBR-ASSIGNED                            08350000
083600     END-IF                                                       08360000
083700                                                                  08370000
083800     PERFORM P0250-EDIT-VIEW-TIME                                 08380000
083900                                                                  08390000
084000     MOVE '0' TO WS-ERROR-FLAG                                    08400000
084100     EVALUATE TRUE                                                08410000
084200         WHEN INQUIRY-REQ                                         08420000
084300            PERFORM P1000-LIST-XB                                 08430000
084400         WHEN ADD-REQ                                             08440000
084500            PERFORM P3000-ADD-XB                                  08450000
084600         WHEN REPOSITION-REQ                                      08460000
084700            IF POSITION-BOARD                                     08470000
084800               IF P06CA-LAST-FUNCTION = 'R'                       08480000
084900                  AND P06CA-LAST-CC = SCR06-CC                    08490000
085000                  AND P06CA-LAST-BOARD = SCR06-BOARD              08500000
085100                  AND P06CA-LAST-YARD-ROAD = SCR06-YARD-ROAD      08510000
085200                  AND ENTER-KEY                                   08520000
085300                  PERFORM P4000-REPOSITION                        08530000
085400               ELSE                                               08540000
085500                  PERFORM P1000-LIST-XB                           08550000
085600               END-IF                                             08560000
085700            ELSE                                                  08570000
085800               MOVE -1 TO SCR06-FUNCTION-CURSOR                   08580000
085900*                   'MUST INQUIRE ON POSITION BOARD FIRST'        08590000
086000            MOVE 'M022' TO MSGLOG-CODE                            08600000
086100            END-IF                                                08610000
086200         WHEN DELETE-REQ                                          08620000
086300            PERFORM P5000-DELETE-XB                               08630000
086400         WHEN MOVE-REQ                                            08640000
086500            IF DUAL-XB                                            08650000
086600               PERFORM P6000-MOVE-YARD-ROAD                       08660000
086700            ELSE                                                  08670000
086800               MOVE -1 TO SCR06-FUNCTION-CURSOR                   08680000
086900               MOVE REV-VIDEO TO SCR06-FUNCTION-HI                08690000
087000*                   INVALID-CODE-MSG                              08700000
087100               MOVE 'I041' TO MSGLOG-CODE                         08710000
087200            END-IF                                                08720000
087300         WHEN TAG-REQ                                             08730000
087400            IF TAG-XB                                             08740000
087500               PERFORM P7000-SET-TAG                              08750000
087600            ELSE                                                  08760000
087700               MOVE -1 TO SCR06-FUNCTION-CURSOR                   08770000
087800               MOVE REV-VIDEO TO SCR06-FUNCTION-HI                08780000
087900*                   INVALID-CODE-MSG                              08790000
088000               MOVE 'I041' TO MSGLOG-CODE                         08800000
088100            END-IF                                                08810000
088200         WHEN SCHEDULE-REQ                                        08820000
088300            IF CNTL-XB-SCHEDULED                                  08830000
088400               PERFORM P7500-SCHEDULE-REQUEST                     08840000
088500            ELSE                                                  08850000
088510               IF CNTL-XB-EXTENDED-SCHED                          08851000
088520                  PERFORM P7600-EXTENDED-SCHED-REQUEST            08852001
088530               ELSE                                               08853000
088600*     SCHEDULE FUNCTION IS NOT ALLOWED UNLESS BOARD IS SCHEDULED  08860000
088700                  MOVE 'B076' TO MSGLOG-CODE                      08870000
088800                  MOVE -1 TO SCR06-FUNCTION-CURSOR                08880000
088900                  MOVE REV-VIDEO TO SCR06-FUNCTION-HI             08890000
088910               END-IF                                             08891000
089000            END-IF                                                08900000
089100         WHEN OTHER                                               08910000
089200*                INVALID-CODE-MSG                                 08920000
089300            MOVE 'I041' TO MSGLOG-CODE                            08930000
089400            MOVE -1 TO SCR06-FUNCTION-CURSOR                      08940000
089500            MOVE REV-VIDEO TO SCR06-FUNCTION-HI                   08950000
089600     END-EVALUATE.                                                08960000
089700*                                                                 08970000
089800 P0200-RESET-ATTRIBUTES.                                          08980000
089900*                                                                 08990000
090000     MOVE DEFAULT-ATTR TO SCR06-DIST-HI                           09000000
090100                          SCR06-SUB-DIST-HI                       09010000
090200                          SCR06-FUNCTION-HI                       09020000
090300                          SCR06-CC-HI                             09030000
090400                          SCR06-BOARD-HI                          09040000
090500                          SCR06-YARD-ROAD-HI                      09050000
090600                          SCR06-VIEW-TIME-HI                      09060000
090700     PERFORM VARYING I FROM 1 BY 1                                09070000
090800             UNTIL I > ARRAY-MAX                                  09080000
090900        MOVE DEFAULT-ATTR TO SCR06-FUNC-CODE-HI(I)                09090000
091000                             SCR06-NAME-HI(I)                     09100000
091100                             SCR06-TURN-HI(I)                     09110000
091200                             SCR06-VARIABLE-HI(I)                 09120000
091300     END-PERFORM.                                                 09130000
091400*************************************************************     09140000
091500 P0250-EDIT-VIEW-TIME.                                            09150000
091600*   ADD VIEW TIME WHICH ALLOWS USERS TO HAVE A SNAPSHOT           09160000
091700*   OF AVAILABILITY OVER A 24 HOUR PERIOD.  VIEW TIME IS          09170000
091800*   ONLY ALLOWED ON POSITION AND TURN BOARD VIEWS.                09180000
091900*                        NORTHERN QUEBEC SPAREBOARD - PHASE 2     09190000
092000*************************************************************     09200000
092100*    MOVE WS-LOCAL-DATE-TIME TO WS-VIEW-DATE-TIME                 09210000
092200     MOVE WS-LOCAL-DATE-TIME-CENT TO WS-VIEW-DATE-TIME-CENT.      09220000
092300     IF SCR06-VIEW-TIME > SPACES                                  09230000
092400        IF SCR06-BOARD = 'P' OR 'T' OR 'Q'                        09240000
092500           CONTINUE                                               09250000
092600        ELSE                                                      09260000
092700           MOVE SPACES TO SCR06-VIEW-TIME                         09270000
092800        END-IF                                                    09280000
092900     END-IF                                                       09290000
093000                                                                  09300000
093100     IF SCR06-VIEW-TIME > SPACES                                  09310000
093200        MOVE SCR06-VIEW-TIME TO BIF-FIELD                         09320000
093300        MOVE LENGTH OF SCR06-VIEW-TIME TO BIF-LEN                 09330000
093400        PERFORM P8999-BIFEDIT                                     09340000
093500        IF BIF-ERROR OR                                           09350000
093600           BIF-EDITED-FIELD = ZEROES                              09360000
093700**         'INVALID VIEW TIME'                                    09370000
093800           MOVE 'B059' TO MSGLOG-CODE                             09380000
093900           MOVE -1 TO SCR06-VIEW-TIME-CURSOR                      09390000
094000           MOVE REV-VIDEO TO SCR06-VIEW-TIME-HI                   09400000
094100           PERFORM P9000-SEND-MAP-AND-RETURN                      09410000
094200        END-IF                                                    09420000
094300        MOVE BIF-EDITED-FIELD TO WS-VIEW-TIME                     09430000
094400                                                                  09440000
094500        IF WS-VIEW-HOUR > 23 OR                                   09450000
094600           WS-VIEW-MINUTE > 59                                    09460000
094700**         'INVALID VIEW TIME'                                    09470000
094800           MOVE 'B059' TO MSGLOG-CODE                             09480000
094900           MOVE -1 TO SCR06-VIEW-TIME-CURSOR                      09490000
095000           MOVE REV-VIDEO TO SCR06-VIEW-TIME-HI                   09500000
095100           PERFORM P9000-SEND-MAP-AND-RETURN                      09510000
095200        END-IF                                                    09520000
095300     END-IF                                                       09530000
095400                                                                  09540000
095500     IF WS-VIEW-TIME < WS-LOCAL-TIME                              09550000
095600        MOVE ZEROS TO DATE-CONVERSION-PARMS                       09560000
095700        MOVE WS-VIEW-DATE TO PARM-PRI-DATE-GREG                   09570000
095800        SET PARM-ADD TO TRUE                                      09580000
095900        MOVE 1 TO PARM-SEC-GREG-DAY                               09590000
096000        PERFORM P8700-CALL-DATE-ROUTINE                           09600000
096100        MOVE PARM-RES-GREG-CENT TO WS-VIEW-CENT                   09610000
096200        MOVE PARM-RES-DATE-GREG TO WS-VIEW-DATE                   09620000
096300     END-IF.                                                      09630000
096400*=================================================================09640000
096500 P0300-GET-NBR-ASSIGNED.                                          09650000
096600*=================================================================09660000
096700     MOVE SPACES                        TO P913-COMMAREA-PARMS    09670000
096800     SET P913-GET-NBR-ASSIGN-FUNCTION   TO TRUE                   09680000
096900     MOVE WS-LOCAL-DATE-TIME            TO P913-EFF-DATE-TIME     09690000
097000     MOVE PSTCA-TIME-ZONE               TO P913-TIME-ZONE         09700000
097100     MOVE PSTCA-DIST                    TO P913-TURN-DIST         09710000
097200     MOVE PSTCA-SUB-DIST                TO P913-TURN-SUB-DIST     09720000
097300     MOVE SCR06-CC                      TO P913-TURN-CC           09730000
097400     EXEC CICS LINK                                               09740000
097500               PROGRAM(P913-PGM)                                  09750000
097600               COMMAREA(P913-COMMAREA-PARMS)                      09760000
097700               LENGTH(P913-LGTH)                                  09770000
097800               RESP(WS-RESPONSE)                                  09780000
097900     END-EXEC                                                     09790000
098000     MOVE WS-RESPONSE                   TO FILE-STATUS            09800000
098100     IF NOT SUCCESS                                               09810000
098200        MOVE 'P0300-01'                 TO ERR-PARAGRAPH          09820000
098300        MOVE 'P913'                     TO ERR-KEY                09830000
098400        PERFORM P9999-GOT-PROBLEM                                 09840000
098500     END-IF                                                       09850000
098600     IF P913-NBR-ASSIGNED                > SPACES                 09860000
098700        MOVE P913-NBR-ASSIGNED          TO SCR06-NBR-ASSIGNED     09870000
098800                                           P06CA-NBR-ASSIGNED     09880000
098900     ELSE                                                         09890000
099000        MOVE ZEROS                      TO P06CA-NBR-ASSIGNED-NUM 09900000
099100     END-IF                                                       09910000
099200     IF P913-NBR-ELIGIBLE                > SPACES                 09920000
099300        MOVE P913-NBR-ELIGIBLE          TO SCR06-NBR-ELIGIBLE     09930000
099400                                           P06CA-NBR-ELIGIBLE     09940000
099500     ELSE                                                         09950000
099600        MOVE ZEROS                      TO P06CA-NBR-ELIGIBLE-NUM 09960000
099700     END-IF.                                                      09970000
099800*=================================================================09980000
099900 P1000-LIST-XB.                                                   09990000
100000*                                                                 10000000
100100*****PERFORM P8800-GET-CURRENT-TIME                               10010000
100200*                                                                 10020000
100300*    MOVE THE APPROPRIATE HEADER FOR THE TYPE OF BOARD            10030000
100400*                                                                 10040000
100500     IF POSITION-BOARD                                            10050000
100600        IF REPOSITION-REQ                                         10060000
100700           IF PSTCA-SUB = 1                                       10070000
100800              MOVE WS-VARIABLE-LINE-3-HDR TO SCR06-HEADER         10080000
100900           ELSE                                                   10090000
101000              MOVE WS-VARIABLE-LINE-3-HDR-FRENCH TO SCR06-HEADER  10100000
101100           END-IF                                                 10110000
101200        ELSE                                                      10120000
101300           IF PSTCA-SUB = 1                                       10130000
101400              MOVE WS-VARIABLE-LINE-1-HDR TO SCR06-HEADER         10140000
101500           ELSE                                                   10150000
101600              MOVE WS-VARIABLE-LINE-1-HDR-FRENCH TO SCR06-HEADER  10160000
101700           END-IF                                                 10170000
101800        END-IF                                                    10180000
101900     ELSE                                                         10190000
102000        IF QUAL-BOARD                                             10200000
102100          IF PSTCA-SUB = 1                                        10210000
102200             MOVE WS-VARIABLE-LINE-4-HDR TO SCR06-HEADER          10220000
102300          ELSE                                                    10230000
102400             MOVE WS-VARIABLE-LINE-4-HDR-FRENCH TO SCR06-HEADER   10240000
102500          END-IF                                                  10250000
102600        ELSE                                                      10260000
102700          IF DUAL-XB                                              10270000
102800             IF PSTCA-SUB = 1                                     10280000
102900                MOVE 'BD' TO WS-VL2-HDR-BOARD                     10290000
103000             ELSE                                                 10300000
103100                MOVE 'TB' TO WS-VL2-HDR-BOARD-FRENCH              10310000
103200             END-IF                                               10320000
103300          ELSE                                                    10330000
103400             IF PSTCA-SUB = 1                                     10340000
103500                MOVE '  ' TO WS-VL2-HDR-BOARD                     10350000
103600             ELSE                                                 10360000
103700                MOVE '  ' TO WS-VL2-HDR-BOARD-FRENCH              10370000
103800             END-IF                                               10380000
103900          END-IF                                                  10390000
104000          IF PSTCA-SUB = 1                                        10400000
104100             MOVE WS-VARIABLE-LINE-2-HDR TO SCR06-HEADER          10410000
104200          ELSE                                                    10420000
104300             MOVE WS-VARIABLE-LINE-2-HDR-FRENCH TO SCR06-HEADER   10430000
104400          END-IF                                                  10440000
104500        END-IF                                                    10450000
104600     END-IF                                                       10460000
104700*                                                                 10470000
104800*    CLEAR ARRAY - RESET ATTRIBUTES AND COLORS                    10480000
104900*                                                                 10490000
105000     PERFORM VARYING TURN-SUB FROM 1 BY 1                         10500000
105100        UNTIL TURN-SUB > ARRAY-MAX                                10510000
105200        MOVE SPACES         TO SCR06-FUNC-CODE(TURN-SUB)          10520000
105300                               SCR06-NAME(TURN-SUB)               10530000
105400                               SCR06-TURN(TURN-SUB)               10540000
105500                               SCR06-VARIABLE(TURN-SUB)           10550000
105600                               P06CA-ARRAY(TURN-SUB)              10560000
105700        IF REPOSITION-REQ                                         10570000
105800           MOVE AUTOSKIP-MDT TO SCR06-NAME-ATTR(TURN-SUB)         10580000
105900                                SCR06-TURN-ATTR(TURN-SUB)         10590000
106000           MOVE MDT          TO SCR06-VARIABLE-ATTR(TURN-SUB)     10600000
106100           MOVE WHITE        TO SCR06-VARIABLE-COLOR(TURN-SUB)    10610000
106200           MOVE CYAN         TO SCR06-NAME-COLOR(TURN-SUB)        10620000
106300                                SCR06-TURN-COLOR(TURN-SUB)        10630000
106400        ELSE                                                      10640000
106500           MOVE MDT          TO SCR06-NAME-ATTR(TURN-SUB)         10650000
106600                                SCR06-TURN-ATTR(TURN-SUB)         10660000
106700           MOVE AUTOSKIP-MDT TO SCR06-VARIABLE-ATTR(TURN-SUB)     10670000
106800           MOVE CYAN         TO SCR06-VARIABLE-COLOR(TURN-SUB)    10680000
106900           MOVE WHITE        TO SCR06-NAME-COLOR(TURN-SUB)        10690000
107000                                SCR06-TURN-COLOR(TURN-SUB)        10700000
107100        END-IF                                                    10710000
107200     END-PERFORM                                                  10720000
107300     IF SCR06-FUNCTION = P06CA-LAST-FUNCTION                      10730000
107400        AND SCR06-CC = P06CA-LAST-CC                              10740000
107500        AND SCR06-BOARD = P06CA-LAST-BOARD                        10750000
107600        AND SCR06-YARD-ROAD = P06CA-LAST-YARD-ROAD                10760000
107700        IF P06CA-HOLD-POS NOT NUMERIC                             10770000
107800           MOVE ZEROS TO P06CA-HOLD-POS                           10780000
107900        END-IF                                                    10790000
108000     ELSE                                                         10800000
108100        SET ENTER-KEY TO TRUE                                     10810000
108200        MOVE SPACES TO P06CA-SCROLL-KEY                           10820000
108300        MOVE ZEROS TO P06CA-HOLD-POS                              10830000
108400     END-IF                                                       10840000
108500     MOVE ZEROS TO TURN-SUB                                       10850000
108600                                                                  10860000
108700     IF TURN-BOARD                                                10870000
108800        PERFORM P1100-BUILD-TURN-BOARD                            10880000
108900     ELSE                                                         10890000
109000        IF SENIORITY-BOARD                                        10900000
109010           PERFORM P1200-BUILD-SENIORITY-BOARD                    10901000
109020        ELSE                                                      10902000
109030           IF QUAL-BOARD                                          10903000
109040              IF WS-FASTSLOW-XB                                   10904000
109050                 AND SCR06-YARD-ROAD = 'R'                        10905000
109060                 PERFORM P1450-BUILD-SLOW-QUAL-BOARD              10906000
109070              ELSE                                                10907000
109080                 PERFORM P1400-BUILD-QUAL-BOARD                   10908000
109090              END-IF                                              10909000
109100           ELSE                                                   10910000
109200*        USE SLOW SIDE OF THE BOARD IF 'R' IS REQUESTED ON        10920000
109300*        A SLOW BOARD.  OTHERWISE USE NORMAL POSITION.     **PLS  10930000
109400              IF WS-FASTSLOW-XB                                   10940000
109500                 AND SCR06-YARD-ROAD = 'R'                        10950000
109600                 PERFORM P1350-BUILD-SLOW-POS-BOARD               10960000
109700              ELSE                                                10970000
109800                 PERFORM P1300-BUILD-POSITION-BOARD               10980000
109900              END-IF                                              10990000
110000           END-IF                                                 11000000
110100        END-IF                                                    11010000
110200     END-IF                                                       11020000
110300     IF REPOSITION-REQ                                            11030000
110400        MOVE SCR06-FUNCTION TO P06CA-LAST-FUNCTION                11040000
110500     ELSE                                                         11050000
110600        MOVE 'I' TO SCR06-FUNCTION                                11060000
110700                    P06CA-LAST-FUNCTION                           11070000
110800     END-IF                                                       11080000
110900     MOVE SCR06-CC        TO P06CA-LAST-CC                        11090000
111000     MOVE SCR06-BOARD     TO P06CA-LAST-BOARD                     11100000
111100     MOVE SCR06-YARD-ROAD TO P06CA-LAST-YARD-ROAD                 11110000
111200     MOVE -1 TO SCR06-FUNCTION-CURSOR                             11120000
111300     IF TURN-SUB NOT > ARRAY-MAX                                  11130000
111400*            'END OF SPAREBOARD'                                  11140000
111500        MOVE 'E010' TO MSGLOG-CODE                                11150000
111600     END-IF.                                                      11160000
111700*                                                                 11170000
111800 P1100-BUILD-TURN-BOARD.                                          11180000
111900*                                                                 11190000
112000     MOVE SPACES TO P06CA-HOLD-POS                                11200000
112100     IF PFKEY8                                                    11210000
112200        AND P06CA-SCROLL-KEY > SPACES                             11220000
112300        MOVE P06CA-SCROLL-KEY TO EBTURN                           11230000
112400     ELSE                                                         11240000
112500        SET ENTER-KEY TO TRUE                                     11250000
112600        MOVE SPACES TO EBTURN                                     11260000
112700        MOVE SCR06-DIST TO DIST OF EBTURN                         11270000
112800        MOVE SCR06-SUB-DIST TO SUBDIST OF EBTURN                  11280000
112900        MOVE SCR06-CC TO CRAFT-CODE OF EBTURN                     11290000
113000     END-IF                                                       11300000
113100     MOVE SPACES TO P06CA-SCROLL-KEY                              11310000
113200     EXEC CICS STARTBR                                            11320000
113300               DATASET(EB-VIA-TURN-NBR)                           11330000
113400               RIDFLD(EBTURN)                                     11340000
113500               GTEQ                                               11350000
113600               RESP(WS-RESPONSE)                                  11360000
113700     END-EXEC                                                     11370000
113800     MOVE WS-RESPONSE TO FILE-STATUS                              11380000
113900     IF SUCCESS                                                   11390000
114000        IF PFKEY8                                                 11400000
114100           EXEC CICS READNEXT                                     11410000
114200                     DATASET(EB-VIA-TURN-NBR)                     11420000
114300                     INTO(WS-EXTRA-BOARD)                         11430000
114400                     LENGTH(EBTURNNO-RLGTH)                       11440000
114500                     RIDFLD(EBTURN)                               11450000
114600                     KEYLENGTH(EBTURNNO-KLGTH)                    11460000
114700                     RESP(WS-RESPONSE)                            11470000
114800           END-EXEC                                               11480000
114900           MOVE WS-RESPONSE TO FILE-STATUS                        11490000
115000        END-IF                                                    11500000
115100     END-IF                                                       11510000
115200     IF SUCCESS                                                   11520000
115300        MOVE '0' TO DONE-CODE                                     11530000
115400        PERFORM UNTIL DONE                                        11540000
115500           EXEC CICS READNEXT                                     11550000
115600                     DATASET(EB-VIA-TURN-NBR)                     11560000
115700                     INTO(WS-EXTRA-BOARD)                         11570000
115800                     LENGTH(EBTURNNO-RLGTH)                       11580000
115900                     RIDFLD(EBTURN)                               11590000
116000                     KEYLENGTH(EBTURNNO-KLGTH)                    11600000
116100                     RESP(WS-RESPONSE)                            11610000
116200           END-EXEC                                               11620000
116300           MOVE WS-RESPONSE TO FILE-STATUS                        11630000
116400           IF SUCCESS                                             11640000
116500              IF DIST-REPEAT = SCR06-DIST                         11650000
116600                 AND SUBDIST-REPEAT = SCR06-SUB-DIST              11660000
116700                 AND CRAFT-CODE-REPEAT = SCR06-CC                 11670000
116800*               ONLY DISPLAY REQUESTED BOARD IF DUAL BOARD.       11680000
116900*               OTHERWISE DISPLAY ALL VALID TURNS.         **PLS  11690000
117000                 IF ((SCR06-YARD-ROAD = 'Y' AND EB-YARD-BOARD)    11700000
117100                    OR (SCR06-YARD-ROAD = 'R' AND EB-ROAD-BOARD)  11710000
117200                  OR (WS-FASTSLOW-XB AND SCR06-YARD-ROAD > SPACE) 11720000
117300                    OR SCR06-YARD-ROAD NOT > SPACE)               11730000
117400                    ADD 1 TO TURN-SUB                             11740000
117500                    IF TURN-SUB NOT > ARRAY-MAX                   11750000
117600                       IF TURN-SUB = ARRAY-MAX                    11760000
117700                          MOVE EBTURN-AREA TO P06CA-SCROLL-KEY    11770000
117800                       END-IF                                     11780000
117900                       MOVE ZERO     TO ASGN-EMP-NO               11790000
118000                                        GOT-EMPLOYEE-FLAG         11800000
118100                       MOVE SPACES   TO WS-MSTR                   11810000
118200                       MOVE TURN-NBR TO WK-ASGN-XB-TURN           11820000
118300                       PERFORM PXXXX-LATEST-TEMP                  11830000
118400                       IF ASGN-EMP-NO NOT > ZERO                  11840000
118500                          PERFORM PXXXX-JOB-OWNER                 11850000
118600                       END-IF                                     11860000
118700                       IF ASGN-EMP-NO > ZERO                      11870000
118800                          MOVE ASGN-EMP-NO TO MSTRNBRK            11880000
118900                          PERFORM P8500-READ-MASTER               11890000
119000                          SET GOT-EMPLOYEE TO TRUE                11900000
119100                       END-IF                                     11910000
119200                       MOVE TURN-SUB TO NAME-SUB                  11920000
119300                       PERFORM P1500-SETUP-NAME-LINE              11930000
119400                    ELSE                                          11940000
119500                       SET DONE TO TRUE                           11950000
119600                    END-IF                                        11960000
119700                 END-IF                                           11970000
119800              ELSE                                                11980000
119900                 SET DONE TO TRUE                                 11990000
120000              END-IF                                              12000000
120100           ELSE                                                   12010000
120200              SET DONE TO TRUE                                    12020000
120300              IF NOT (NO-RECORD-FND OR END-OF-FILE)               12030000
120400                 MOVE 'P1000-4' TO ERR-PARAGRAPH                  12040000
120500                 MOVE EBTURN    TO ERR-KEY                        12050000
120600                 PERFORM P9999-GOT-PROBLEM                        12060000
120700              END-IF                                              12070000
120800           END-IF                                                 12080000
120900        END-PERFORM                                               12090000
121000        EXEC CICS ENDBR                                           12100000
121100                  DATASET(EB-VIA-TURN-NBR)                        12110000
121200                  RESP(WS-RESPONSE)                               12120000
121300        END-EXEC                                                  12130000
121400     ELSE                                                         12140000
121500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     12150000
121600           MOVE 'P1000-5' TO ERR-PARAGRAPH                        12160000
121700           MOVE EBTURN    TO ERR-KEY                              12170000
121800           PERFORM P9999-GOT-PROBLEM                              12180000
121900        END-IF                                                    12190000
122000     END-IF.                                                      12200000
122100*                                                                 12210000
122200 P1200-BUILD-SENIORITY-BOARD.                                     12220000
122300*                                                                 12230000
122400     MOVE SPACES                TO P942-COMMAREA-PARMS            12240000
122500     SET P942-ASGN-SEN-FUNCTION TO TRUE                           12250000
122600     SET P942-ASGN-XB           TO TRUE                           12260000
122700     MOVE SCR06-DIST            TO P942-ASGN-DIST                 12270000
122800     MOVE SCR06-SUB-DIST        TO P942-ASGN-SUB-DIST             12280000
122900     MOVE SCR06-CC              TO P942-ASGN-CC                   12290000
123000     MOVE '******'              TO P942-ASGN-ASGN                 12300000
123100                                                                  12310000
123200     PERFORM P8080-LINK-942                                       12320000
123300     MOVE P942-ASGN-SEN-AREA    TO WS-ASGN-SEN-AREA               12330000
123400                                                                  12340000
123500     IF WS-ASGN-SEN-AREA > SPACES                                 12350000
123600        IF PFKEY8                                                 12360000
123700           AND P06CA-SCROLL-KEY > SPACES                          12370000
123800           MOVE P06CA-LAST-SS   TO SEN-SUB                        12380000
123900        ELSE                                                      12390000
124000           MOVE SPACES          TO P06CA-DUP-EMPLOYEE-TABLE       12400000
124100           MOVE ZEROS           TO P06CA-HOLD-POS                 12410000
124200           MOVE 1               TO SEN-SUB                        12420000
124300        END-IF                                                    12430000
124400        MOVE '0'                TO SEN-ROSTER-DONE-CODE           12440000
124500        PERFORM UNTIL SEN-ROSTER-DONE                             12450000
124600           IF WS-ASGN-SEN-RSTR(SEN-SUB) > SPACES                  12460000
124700              MOVE SPACES       TO WS-CNTL-FILE                   12470000
124800              SET ROSTER-TYPE-REC TO TRUE                         12480000
124900              MOVE WS-ASGN-SEN-RSTR(SEN-SUB)                      12490000
125000                                TO CNTL-P-ROSTER-CODE             12500000
125100              MOVE WS-ASGN-SEN-CC(SEN-SUB)                        12510000
125200                                TO CNTL-ROSTER-CC                 12520000
125300              MOVE CNTLKEY-AREA TO CNTLKEY                        12530000
125400              EXEC CICS READ                                      12540000
125500                        DATASET(CNTL-FILE-VIA-CNTLKEY)            12550000
125600                        INTO(WS-CNTL-FILE)                        12560000
125700                        LENGTH(CNTLFILE-RLGTH)                    12570000
125800                        RIDFLD(CNTLKEY)                           12580000
125900                        KEYLENGTH(CNTLFILE-KLGTH)                 12590000
126000                        RESP(WS-RESPONSE)                         12600000
126100              END-EXEC                                            12610000
126200              MOVE WS-RESPONSE  TO FILE-STATUS                    12620000
126300              IF NOT SUCCESS                                      12630000
126400                 MOVE 'P1200-1' TO ERR-PARAGRAPH                  12640000
126500                 MOVE CNTLKEY   TO ERR-KEY                        12650000
126600                 PERFORM P9999-GOT-PROBLEM                        12660000
126700              END-IF                                              12670000
126800              PERFORM P1220-BUILD-SENIORITY-ROSTER                12680000
126900           END-IF                                                 12690000
127000           ADD 1                TO SEN-SUB                        12700000
127100           IF SEN-SUB > SEN-MAX                                   12710000
127200              SET SEN-ROSTER-DONE                                 12720000
127300                                TO TRUE                           12730000
127400           END-IF                                                 12740000
127500        END-PERFORM                                               12750000
127600     END-IF.                                                      12760000
127700*                                                                 12770000
127800 P1220-BUILD-SENIORITY-ROSTER.                                    12780000
127900*                                                                 12790000
128000     IF PFKEY8                                                    12800000
128100        AND P06CA-SCROLL-KEY > SPACES                             12810000
128200        MOVE P06CA-SCROLL-KEY TO SENKEY1                          12820000
128300     ELSE                                                         12830000
128400        SET ENTER-KEY                 TO TRUE                     12840000
128500        MOVE ZEROS                    TO P06CA-HOLD-POS           12850000
128600        MOVE SPACES                   TO SF-SENKEY1               12860000
128700        MOVE WS-ASGN-SEN-RSTR(SEN-SUB) TO SF-ROSTER               12870000
128800        MOVE WS-ASGN-SEN-CC(SEN-SUB)  TO SF-CRAFT                 12880000
128900        MOVE SF-SENKEY1               TO SENKEY1                  12890000
129000     END-IF                                                       12900000
129100                                                                  12910000
129200     IF TURN-SUB < ARRAY-MAX                                      12920000
129300        MOVE SPACES TO P06CA-SCROLL-KEY                           12930000
129400     END-IF                                                       12940000
129500                                                                  12950000
129600     EXEC CICS STARTBR                                            12960000
129700               DATASET(SENFILE-VIA-DIST)                          12970000
129800               RIDFLD(SENKEY1)                                    12980000
129900               GTEQ                                               12990000
130000               RESP(WS-RESPONSE)                                  13000000
130100     END-EXEC                                                     13010000
130200                                                                  13020000
130300     MOVE WS-RESPONSE TO FILE-STATUS                              13030000
130400                                                                  13040000
130500     IF SUCCESS                                                   13050000
130600        IF PFKEY8                                                 13060000
130700           EXEC CICS READNEXT                                     13070000
130800                     DATASET(SENFILE-VIA-DIST)                    13080000
130900                     INTO (WS-SENIORITY)                          13090000
131000                     LENGTH(SENDIST-RLGTH)                        13100000
131100                     RIDFLD(SENKEY1)                              13110000
131200                     KEYLENGTH(SENDIST-KLGTH)                     13120000
131300                     RESP(WS-RESPONSE)                            13130000
131400           END-EXEC                                               13140000
131500           MOVE WS-RESPONSE TO FILE-STATUS                        13150000
131600        END-IF                                                    13160000
131700     END-IF                                                       13170000
131800                                                                  13180000
131900     IF SUCCESS                                                   13190000
132000        MOVE '0' TO DONE-CODE                                     13200000
132100        PERFORM UNTIL DONE                                        13210000
132200           EXEC CICS READNEXT                                     13220000
132300                     DATASET(SENFILE-VIA-DIST)                    13230000
132400                     INTO (WS-SENIORITY)                          13240000
132500                     LENGTH(SENDIST-RLGTH)                        13250000
132600                     RIDFLD(SENKEY1)                              13260000
132700                     KEYLENGTH(SENDIST-KLGTH)                     13270000
132800                     RESP(WS-RESPONSE)                            13280000
132900           END-EXEC                                               13290000
133000           MOVE WS-RESPONSE TO FILE-STATUS                        13300000
133100           IF SUCCESS                                             13310000
133200              IF SF-ROSTER = WS-ASGN-SEN-RSTR(SEN-SUB)            13320000
133300                 AND SF-CRAFT = WS-ASGN-SEN-CC(SEN-SUB)           13330000
133400                 PERFORM P1230-CHECK-EMPLOYEE-SETUP               13340000
133500              ELSE                                                13350000
133600                 SET DONE TO TRUE                                 13360000
133700              END-IF                                              13370000
133800           ELSE                                                   13380000
133900              SET DONE TO TRUE                                    13390000
134000              IF NOT (NO-RECORD-FND OR END-OF-FILE)               13400000
134100                 MOVE 'P1220-1' TO ERR-PARAGRAPH                  13410007
134200                 MOVE SENKEY1   TO ERR-KEY                        13420000
134300                 PERFORM P9999-GOT-PROBLEM                        13430000
134400              END-IF                                              13440000
134500           END-IF                                                 13450000
134600        END-PERFORM                                               13460000
134700        EXEC CICS ENDBR                                           13470000
134800                  DATASET(SENFILE-VIA-DIST)                       13480000
134900                  RESP(WS-RESPONSE)                               13490000
135000        END-EXEC                                                  13500000
135100     ELSE                                                         13510000
135200        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     13520000
135300           MOVE 'P1220-2' TO ERR-PARAGRAPH                        13530007
135400           MOVE SENKEY3   TO ERR-KEY                              13540000
135500           PERFORM P9999-GOT-PROBLEM                              13550000
135600        END-IF                                                    13560000
135700     END-IF.                                                      13570000
135800*                                                                 13580000
135900 P1230-CHECK-EMPLOYEE-SETUP.                                      13590000
136000*                                                                 13600000
136100     MOVE SF-EMP-NO TO MSTRNBRK                                   13610000
136200     PERFORM P8500-READ-MASTER                                    13620000
136300     MOVE ZERO TO GOT-EMPLOYEE-FLAG                               13630000
136400     MOVE SPACES TO EBTURN                                        13640000
136500                                                                  13650000
136600     IF (TEMPORARY-ASGNMT > SPACES                                13660000
136700        AND TEMP-ASGN-XB                                          13670000
136800        AND TA-DIST = SCR06-DIST                                  13680000
136900        AND TA-SUB-DIST = SCR06-SUB-DIST                          13690000
137000        AND TA-CC = SCR06-CC)                                     13700000
137100        MOVE TA-DIST TO DIST OF EBTURN                            13710000
137200        MOVE TA-SUB-DIST TO SUBDIST OF EBTURN                     13720000
137300        MOVE TA-CC TO CRAFT-CODE OF EBTURN                        13730000
137400        MOVE TA-XB-TURN TO EB-TURN-NBR OF EBTURN                  13740000
137500     ELSE                                                         13750000
137600        IF (NORMAL-ASGNMT > SPACE                                 13760000
137700           AND NORM-ASGN-XB                                       13770000
137800           AND NA-DIST = SCR06-DIST                               13780000
137900           AND NA-SUB-DIST = SCR06-SUB-DIST                       13790000
138000           AND NA-CC = SCR06-CC)                                  13800000
138100           MOVE NA-DIST TO DIST OF EBTURN                         13810000
138200           MOVE NA-SUB-DIST TO SUBDIST OF EBTURN                  13820000
138300           MOVE NA-CC TO CRAFT-CODE OF EBTURN                     13830000
138400           MOVE NA-XB-TURN TO EB-TURN-NBR OF EBTURN               13840000
138500        END-IF                                                    13850000
138600     END-IF                                                       13860000
138700                                                                  13870000
138800     IF EBTURN > SPACES                                           13880000
138900        EXEC CICS READ                                            13890000
139000                  DATASET(EB-VIA-TURN-NBR)                        13900000
139100                  INTO(WS-EXTRA-BOARD)                            13910000
139200                  LENGTH(EBTURNNO-RLGTH)                          13920000
139300                  RIDFLD(EBTURN)                                  13930000
139400                  KEYLENGTH(EBTURNNO-KLGTH)                       13940000
139500                  RESP(WS-RESPONSE)                               13950000
139600        END-EXEC                                                  13960000
139700        MOVE WS-RESPONSE TO FILE-STATUS                           13970000
139800        IF NOT SUCCESS                                            13980000
139900           MOVE 'P1230-1' TO ERR-PARAGRAPH                        13990007
140000           MOVE EBTURN  TO ERR-KEY                                14000000
140100           PERFORM P9999-GOT-PROBLEM                              14010000
140200        END-IF                                                    14020000
140300        SET NOT-DUP-EMP     TO TRUE                               14030000
140400                                                                  14040000
140500*TJR                                                              14050000
140600*    IF  TURN-SUB = ARRAY-MAX, THE SCREEN IS FULL,                14060000
140700*    AND THE READ WAS DONE SO WE COULD TELL IF ANY MORE RECS      14070000
140800*    TO DISPLAY. WE DO NOT WANT TO WRITE THE 21ST RECORD READ     14080000
140900*    EMP NO TO THE DUP TABLE. WE WERE DOING THAT AND THE          14090000
141000*    RESULT WAS SKIPPING THIS REC ON THE NEXT PAGE. TURN-SUB      14100000
141100*    IS INCREMENTED BELOW, AND CHECKED LATER FOR END OF SPAREBOARD14110000
141200*                                                                 14120000
141300        IF TURN-SUB     < ARRAY-MAX                               14130000
141400*TJR                                                              14140000
141500           PERFORM VARYING DUP-SUB FROM 1 BY 1                    14150000
141600             UNTIL DUP-SUB > DUP-MAX                              14160000
141700              IF P06CA-DUP-EMP-NBR(DUP-SUB) NOT > SPACES          14170000
141800                 MOVE SF-EMP-NO  TO P06CA-DUP-EMP-NBR(DUP-SUB)    14180000
141900                 MOVE DUP-MAX    TO DUP-SUB                       14190000
142000              ELSE                                                14200000
142100                 IF SF-EMP-NO = P06CA-DUP-EMP-NBR(DUP-SUB)        14210000
142200                    SET DUP-EMP  TO TRUE                          14220000
142300                    MOVE DUP-MAX TO DUP-SUB                       14230000
142400                 END-IF                                           14240000
142500              END-IF                                              14250000
142600           END-PERFORM                                            14260000
142700                                                                  14270000
142800           IF NOT DUP-EMP                                         14280000
142900*          DISPLAY ONLY REQUESTED BOARD, IF DUAL BOARD,           14290000
143000*          OTHERWISE, DISPLAY ALL VALID TURNS.                    14300000
143100              IF ((SCR06-YARD-ROAD = 'R' AND EB-ROAD-BOARD)       14310000
143200              OR  (SCR06-YARD-ROAD = 'Y' AND EB-YARD-BOARD)       14320000
143210              OR  (SCR06-YARD-ROAD > SPACE AND WS-FASTSLOW-XB)    14321000
143220              OR SCR06-YARD-ROAD NOT > SPACE)                     14322000
143230                 ADD  1               TO TURN-SUB                 14323000
143240                 IF TURN-SUB NOT > ARRAY-MAX                      14324000
143250                    IF TURN-SUB = ARRAY-MAX                       14325000
143260                       MOVE SENKEY1   TO P06CA-SCROLL-KEY         14326000
143270                       MOVE SEN-SUB   TO P06CA-LAST-SS            14327000
143280                    END-IF                                        14328000
143290                    SET  GOT-EMPLOYEE TO TRUE                     14329000
143300                    MOVE TURN-SUB     TO NAME-SUB                 14330000
143400                    PERFORM P1500-SETUP-NAME-LINE                 14340000
143500                 ELSE                                             14350000
143600                    SET  DONE         TO TRUE                     14360000
143700                 END-IF                                           14370000
143800              END-IF                                              14380000
143900           END-IF                                                 14390000
144000        ELSE                                                      14400000
144100           PERFORM VARYING DUP-SUB FROM 1 BY 1                    14410000
144200              UNTIL DUP-SUB > DUP-MAX                             14420000
144300              IF SF-EMP-NO = P06CA-DUP-EMP-NBR(DUP-SUB)           14430000
144400                 SET  DUP-EMP  TO TRUE                            14440000
144410                 MOVE DUP-MAX  TO DUP-SUB                         14441000
144420              END-IF                                              14442000
144430           END-PERFORM                                            14443000
144431           IF NOT DUP-EMP                                         14443100
144432              ADD  1           TO TURN-SUB                        14443200
144433           END-IF                                                 14443300
144434        END-IF                                                    14443400
144435     END-IF.                                                      14443500
144436*                                                                 14443600
144437 P1300-BUILD-POSITION-BOARD.                                      14443700
144438*                                                                 14443800
144439     IF DAILY-MARK-YARD                                           14443900
144440        AND SWITCHMAN-GEL-BOARD                                   14444000
144450        IF WS-LOCAL-HR > 20 AND SCR06-CC = 'S1'                   14445000
144460          ADD 1 TO DAY1                                           14446000
144470        END-IF                                                    14447000
144480        IF WS-LOCAL-HR < 07 AND SCR06-CC = 'S3'                   14448000
144490          SUBTRACT 1 FROM DAY1                                    14449000
144500        END-IF                                                    14450000
144600        IF DAY1 = ZERO                                            14460000
144700           MOVE 7 TO DAY1                                         14470000
144800        END-IF                                                    14480000
144900        IF DAY1 = 08                                              14490000
145000           MOVE 1 TO DAY1                                         14500000
145100        END-IF                                                    14510000
145200     END-IF                                                       14520000
145300     IF PFKEY8                                                    14530000
145400        AND P06CA-SCROLL-KEY > SPACES                             14540000
145500        MOVE P06CA-SCROLL-KEY TO EBPOS                            14550000
145600     ELSE                                                         14560000
145700        SET ENTER-KEY TO TRUE                                     14570000
145800        MOVE ZEROS TO P06CA-HOLD-POS                              14580000
145900        MOVE SPACES TO EBPOS-AREA                                 14590000
146000        MOVE SCR06-DIST TO DIST OF WS-EXTRA-BOARD                 14600000
146100        MOVE SCR06-SUB-DIST TO SUB-DIST OF WS-EXTRA-BOARD         14610000
146200        MOVE SCR06-CC TO CRAFT-CODE OF WS-EXTRA-BOARD             14620000
146300        MOVE ZEROS TO EB-POSITION                                 14630000
146400        IF DUAL-XB                                                14640000
146500           IF SCR06-YARD-ROAD = 'Y'                               14650000
146600              MOVE '1' TO EB-POS-BOARD                            14660000
146700           ELSE                                                   14670000
146800              IF SCR06-YARD-ROAD = 'R'                            14680000
146900                 MOVE '2' TO EB-POS-BOARD                         14690000
147000              END-IF                                              14700000
147100           END-IF                                                 14710000
147200        END-IF                                                    14720000
147300        MOVE EBPOS-AREA TO EBPOS                                  14730000
147400     END-IF                                                       14740000
147500     MOVE SPACES TO P06CA-SCROLL-KEY                              14750000
147600     EXEC CICS STARTBR                                            14760000
147700               DATASET(EB-VIA-CRAFT-POSITION)                     14770000
147800               RIDFLD(EBPOS)                                      14780000
147900               GTEQ                                               14790000
148000               RESP(WS-RESPONSE)                                  14800000
148100     END-EXEC                                                     14810000
148200     MOVE WS-RESPONSE TO FILE-STATUS                              14820000
148300     IF SUCCESS                                                   14830000
148400        IF PFKEY8                                                 14840000
148500           EXEC CICS READNEXT                                     14850000
148600                     DATASET(EB-VIA-CRAFT-POSITION)               14860000
148700                     INTO(WS-EXTRA-BOARD)                         14870000
148800                     LENGTH(EBCRPOS-RLGTH)                        14880000
148900                     RIDFLD(EBPOS)                                14890000
149000                     KEYLENGTH(EBCRPOS-KLGTH)                     14900000
149100                     RESP(WS-RESPONSE)                            14910000
149200           END-EXEC                                               14920000
149300           MOVE WS-RESPONSE TO FILE-STATUS                        14930000
149400        END-IF                                                    14940000
149500     END-IF                                                       14950000
149600     IF SUCCESS                                                   14960000
149700        MOVE '0' TO DONE-CODE                                     14970000
149800        PERFORM UNTIL DONE                                        14980000
149900           EXEC CICS READNEXT                                     14990000
150000                     DATASET(EB-VIA-CRAFT-POSITION)               15000000
150100                     INTO(WS-EXTRA-BOARD)                         15010000
150200                     LENGTH(EBCRPOS-RLGTH)                        15020000
150300                     RIDFLD(EBPOS)                                15030000
150400                     KEYLENGTH(EBCRPOS-KLGTH)                     15040000
150500                     RESP(WS-RESPONSE)                            15050000
150600           END-EXEC                                               15060000
150700           MOVE WS-RESPONSE TO FILE-STATUS                        15070000
150800*                                                          **PLS  15080000
150900           IF SUCCESS                                             15090000
151000              IF DIST OF WS-EXTRA-BOARD = SCR06-DIST              15100000
151100                 AND SUB-DIST OF WS-EXTRA-BOARD = SCR06-SUB-DIST  15110000
151200                 AND CRAFT-CODE OF WS-EXTRA-BOARD = SCR06-CC      15120000
151300                 AND ((SCR06-YARD-ROAD = 'Y' AND EB-YARD-BOARD)   15130000
151400                 OR (SCR06-YARD-ROAD = 'R' AND EB-ROAD-BOARD)     15140000
151500                 OR (SCR06-YARD-ROAD > SPACE AND WS-FASTSLOW-XB)  15150000
151600                 OR SCR06-YARD-ROAD NOT > SPACE)                  15160000
151700                 MOVE SPACES       TO TZ-PARAMETERS               15170000
151800                 SET TZ-IN-SYSTEM-ZONE TO TRUE                    15180000
151900                 MOVE EB-POS-DATE-TIME TO TZ-IN-DATE-TIME         15190000
152000                 MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE              15200000
152100                 PERFORM P8996-TIMEZONE                           15210000
152200*                MOVE TZ-OUT-DATE-TIME TO WS-POS-DATE-TIME-TZ     15220000
152300                 MOVE TZ-OUT-DATE-TIME-CENT TO                    15230000
152400                      WS-POS-CENT-DATE-TIME-TZ                    15240000
152500*                IF WS-POS-DATE-TIME-TZ <= WS-VIEW-DATE-TIME      15250000
152600                 IF (WS-POS-CENT-DATE-TIME-TZ <=                  15260000
152700                    WS-VIEW-DATE-TIME-CENT                        15270000
152800                    OR WS-SYSTEM-DATE = '991231' OR '000101')     15280000
152900                    AND ((NOT WS-FASTSLOW-XB AND EB-ON-BOARD) OR  15290000
153000                         (WS-FASTSLOW-XB AND EB-ON-BOARD AND      15300000
153100                                     EB-SLOW-ON-BOARD))           15310000
153200                    ADD 1 TO TURN-SUB                             15320000
153300                    IF TURN-SUB NOT > ARRAY-MAX                   15330000
153400                      IF TURN-SUB = ARRAY-MAX                     15340000
153500                          MOVE EBPOS-AREA TO P06CA-SCROLL-KEY     15350000
153600                       END-IF                                     15360000
153700                       MOVE ZERO     TO ASGN-EMP-NO               15370000
153800                                        GOT-EMPLOYEE-FLAG         15380000
153900                       MOVE SPACES   TO WS-MSTR                   15390000
154000                       MOVE TURN-NBR TO WK-ASGN-XB-TURN           15400000
154100                       PERFORM PXXXX-LATEST-TEMP                  15410000
154200                       IF ASGN-EMP-NO NOT > ZERO                  15420000
154300                          PERFORM PXXXX-JOB-OWNER                 15430000
154400                       END-IF                                     15440000
154500                       IF ASGN-EMP-NO > ZERO                      15450000
154600                          MOVE ASGN-EMP-NO TO MSTRNBRK            15460000
154700                          PERFORM P8500-READ-MASTER               15470000
154800                          SET GOT-EMPLOYEE TO TRUE                15480000
154900                       END-IF                                     15490000
155000                       IF GOT-EMPLOYEE                            15500000
155100                          PERFORM P1505-CALL-XB-POS-PARMS         15510000
155200*ET                                                               15520000
155300*ET ENHANCED TRACKING REQUIRES THAT ALL EMPLOYEES RETAIN THEIR    15530000
155400*ET POSITIONS. HOWEVER, IF THE DISPLAY W/O TRACKING FLAGS DO      15540000
155500*ET NOT CONTAIN THEIR STATUS CODE, THEY WILL NOT DISPLAY ON       15550000
155600*ET THE P BOARD, BUT WILL ON THE A BOARD                          15560000
155700*ET                                                               15570000
155800*ET NOTE: CN DOES WANT TO SEE EMPLOYEES ON THEIR REST DAYS ON     15580000
155900*ET       BOTH BOARDS, REGARDLESS OF WHAT PETE SAYS. NJB          15590000
156000*ET                                                               15600000
156100                          SET DISPLAY-EMP TO TRUE                 15610000
156200                          IF AVAILABLE OR WORKING                 15620000
156300                             CONTINUE                             15630000
156400                          ELSE                                    15640000
156500                             IF NOT ALL-POSITIONS                 15650000
156600                                SET DONT-DISPLAY-EMP TO TRUE      15660000
156700                                PERFORM P9830-RETRIEVE-CNTL-INFO  15670000
156800*                               PERFORM VARYING SUB1 FROM 1 BY 1  15680000
156900*                                  UNTIL SUB1 > 5                 15690000
157000*                                  IF CNTL-XB-RETAIN-POS(SUB1) =  15700000
157100*                                     (LAYOFF-CODE-1 OR '*')      15710000
157200*                                     SET DISPLAY-EMP TO TRUE     15720000
157300*                                  END-IF                         15730000
157400*                               END-PERFORM                       15740000
157500                             END-IF                               15750000
157600                          END-IF                                  15760000
157700                          IF DISPLAY-EMP                          15770000
157800                             MOVE TURN-SUB TO NAME-SUB            15780000
157900                             PERFORM P1500-SETUP-NAME-LINE        15790000
158000                          ELSE                                    15800000
158100                             SUBTRACT 1 FROM TURN-SUB             15810000
158200                          END-IF                                  15820000
158300                       ELSE                                       15830000
158400                          SUBTRACT 1 FROM TURN-SUB                15840000
158500                       END-IF                                     15850000
158600                    ELSE                                          15860000
158700                       SET DONE TO TRUE                           15870000
158800                    END-IF                                        15880000
158900                 END-IF                                           15890000
159000              ELSE                                                15900000
159100                 SET DONE TO TRUE                                 15910000
159200              END-IF                                              15920000
159300           ELSE                                                   15930000
159400              SET DONE TO TRUE                                    15940000
159500              IF NOT (NO-RECORD-FND OR END-OF-FILE)               15950000
159600                 MOVE 'P1300-1' TO ERR-PARAGRAPH                  15960000
159700                 MOVE EBPOS     TO ERR-KEY                        15970000
159800                 PERFORM P9999-GOT-PROBLEM                        15980000
159900              END-IF                                              15990000
160000           END-IF                                                 16000000
160100        END-PERFORM                                               16010000
160200        EXEC CICS ENDBR                                           16020000
160300                  DATASET(EB-VIA-CRAFT-POSITION)                  16030000
160400                  RESP(WS-RESPONSE)                               16040000
160500        END-EXEC                                                  16050000
160600     ELSE                                                         16060000
160700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     16070000
160800           MOVE 'P1300-2' TO ERR-PARAGRAPH                        16080000
160900           MOVE EBTURN    TO ERR-KEY                              16090000
161000           PERFORM P9999-GOT-PROBLEM                              16100000
161100        END-IF                                                    16110000
161200     END-IF.                                                      16120000
161300*                                                                 16130000
161400 P1350-BUILD-SLOW-POS-BOARD.                                      16140000
161500*                                                                 16150000
161600*  USE THE SLOW-POSITION KEY TO SET UP THE POSITION BOARD,        16160000
161700*  IF IT'S A FASTSLOW BOARD, AND "R" (ROAD) WAS REQUESTED.        16170000
161800*                                                          **PLS  16180000
161900     IF DAILY-MARK-YARD                                           16190000
162000        AND SWITCHMAN-GEL-BOARD                                   16200000
162100        IF WS-LOCAL-HR > 20 AND SCR06-CC = 'S1'                   16210000
162200          ADD 1 TO DAY1                                           16220000
162300        END-IF                                                    16230000
162400        IF WS-LOCAL-HR < 07 AND SCR06-CC = 'S3'                   16240000
162500          SUBTRACT 1 FROM DAY1                                    16250000
162600        END-IF                                                    16260000
162700        IF DAY1 = ZERO                                            16270000
162800           MOVE 7 TO DAY1                                         16280000
162900        END-IF                                                    16290000
163000        IF DAY1 = 08                                              16300000
163100           MOVE 1 TO DAY1                                         16310000
163200        END-IF                                                    16320000
163300     END-IF                                                       16330000
163400     IF PFKEY8                                                    16340000
163500        AND P06CA-SCROLL-KEY > SPACES                             16350000
163600        MOVE P06CA-SCROLL-KEY TO EBSLOW-POS                       16360000
163700     ELSE                                                         16370000
163800        SET ENTER-KEY TO TRUE                                     16380000
163900        MOVE ZEROS TO P06CA-HOLD-POS                              16390000
164000        MOVE SPACES TO EBSLOW-POS                                 16400000
164100        MOVE SCR06-DIST TO DIST OF EBSLOW-POS                     16410000
164200        MOVE SCR06-SUB-DIST TO SUBDIST OF EBSLOW-POS              16420000
164300        MOVE SCR06-CC TO CRAFT-CODE OF EBSLOW-POS                 16430000
164400        MOVE ZEROS TO EBSLOWPOS                                   16440000
164500     END-IF                                                       16450000
164600     MOVE SPACES TO P06CA-SCROLL-KEY                              16460000
164700     EXEC CICS STARTBR                                            16470000
164800               DATASET(EB-VIA-SLOW-POSITION)                      16480000
164900               RIDFLD(EBSLOW-POS)                                 16490000
165000               GTEQ                                               16500000
165100               RESP(WS-RESPONSE)                                  16510000
165200     END-EXEC                                                     16520000
165300     MOVE WS-RESPONSE TO FILE-STATUS                              16530000
165400     IF SUCCESS                                                   16540000
165500        IF PFKEY8                                                 16550000
165600           EXEC CICS READNEXT                                     16560000
165700                     DATASET(EB-VIA-SLOW-POSITION)                16570000
165800                     INTO(WS-EXTRA-BOARD)                         16580000
165900                     LENGTH(EBSLPOS-RLGTH)                        16590000
166000                     RIDFLD(EBSLOW-POS)                           16600000
166100                     KEYLENGTH(EBSLPOS-KLGTH)                     16610000
166200                     RESP(WS-RESPONSE)                            16620000
166300           END-EXEC                                               16630000
166400           MOVE WS-RESPONSE TO FILE-STATUS                        16640000
166500        END-IF                                                    16650000
166600     END-IF                                                       16660000
166700     IF SUCCESS                                                   16670000
166800        MOVE '0' TO DONE-CODE                                     16680000
166900        PERFORM UNTIL DONE                                        16690000
167000           EXEC CICS READNEXT                                     16700000
167100                     DATASET(EB-VIA-SLOW-POSITION)                16710000
167200                     INTO(WS-EXTRA-BOARD)                         16720000
167300                     LENGTH(EBSLPOS-RLGTH)                        16730000
167400                     RIDFLD(EBSLOW-POS)                           16740000
167500                     KEYLENGTH(EBSLPOS-KLGTH)                     16750000
167600                     RESP(WS-RESPONSE)                            16760000
167700           END-EXEC                                               16770000
167800           MOVE WS-RESPONSE TO FILE-STATUS                        16780000
167900           IF SUCCESS                                             16790000
168000              IF DIST OF WS-EXTRA-BOARD = SCR06-DIST              16800000
168100                 AND SUB-DIST OF WS-EXTRA-BOARD = SCR06-SUB-DIST  16810000
168200                 AND CRAFT-CODE OF WS-EXTRA-BOARD = SCR06-CC      16820000
168300                 AND ((SCR06-YARD-ROAD = 'Y' AND EB-YARD-BOARD)   16830000
168400                 OR (SCR06-YARD-ROAD = 'R' AND EB-ROAD-BOARD)     16840000
168500                 OR (SCR06-YARD-ROAD > SPACE AND WS-FASTSLOW-XB)  16850000
168600                 OR SCR06-YARD-ROAD NOT > SPACE)                  16860000
168700                 MOVE SPACES       TO TZ-PARAMETERS               16870000
168800                 SET TZ-IN-SYSTEM-ZONE TO TRUE                    16880000
168900                 MOVE EB-SLOW-POS-DATE-TIME TO TZ-IN-DATE-TIME    16890000
169000                 MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE              16900000
169100                 PERFORM P8996-TIMEZONE                           16910000
169200*                MOVE TZ-OUT-DATE-TIME TO WS-POS-DATE-TIME-TZ     16920000
169300                 MOVE TZ-OUT-DATE-TIME-CENT TO                    16930000
169400                      WS-POS-CENT-DATE-TIME-TZ                    16940000
169500*-------------------------------------------------------------    16950000
169600*       WHEN DISPLAYING THE SLOW SIDE OF A FASTSLOW BOARD,        16960000
169700*       VERIFY THE ON-BOARD STATUS OF THE SLOW BOARD ONLY.        16970000
169800*       IF 'OFF' ON THE FAST(YARD) SIDE, CONSIDER THE TURN        16980000
169900*       AS ON THE BOARD FOR THE SLOW SIDE.                        16990000
170000*----------------------------------------------------------PLS    17000000
170100*                IF WS-POS-DATE-TIME-TZ <= WS-VIEW-DATE-TIME      17010000
170200                 IF (WS-POS-CENT-DATE-TIME-TZ <=                  17020000
170300                    WS-VIEW-DATE-TIME-CENT                        17030000
170400                    OR WS-SYSTEM-DATE = '991231' OR '000101')     17040000
170500                    AND EB-SLOW-ON-BOARD                          17050000
170600                    ADD 1 TO TURN-SUB                             17060000
170700                    IF TURN-SUB NOT > ARRAY-MAX                   17070000
170800                      IF TURN-SUB = ARRAY-MAX                     17080000
170900                        MOVE EB-SLOW-POS-AREA TO P06CA-SCROLL-KEY 17090000
171000                      END-IF                                      17100000
171100                      MOVE ZERO     TO ASGN-EMP-NO                17110000
171200                                       GOT-EMPLOYEE-FLAG          17120000
171300                      MOVE SPACES   TO WS-MSTR                    17130000
171400                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            17140000
171500                      PERFORM PXXXX-LATEST-TEMP                   17150000
171600                      IF ASGN-EMP-NO NOT > ZERO                   17160000
171700                         PERFORM PXXXX-JOB-OWNER                  17170000
171800                      END-IF                                      17180000
171900                      IF ASGN-EMP-NO > ZERO                       17190000
172000                         MOVE ASGN-EMP-NO TO MSTRNBRK             17200000
172100                         PERFORM P8500-READ-MASTER                17210000
172200                         SET GOT-EMPLOYEE TO TRUE                 17220000
172300                      END-IF                                      17230000
172400                      IF GOT-EMPLOYEE                             17240000
172500                         PERFORM P1505-CALL-XB-POS-PARMS          17250000
172600*ET                                                               17260000
172700*ET ENHANCED TRACKING REQUIRES THAT ALL EMPLOYEES RETAIN THEIR    17270000
172800*ET POSITIONS. HOWEVER, IT THE DISPLAY W/O TRACKING FLAGS DO      17280000
172900*ET NOT CONTAIN THEIR STATUS CODE, THEY WILL NOT DISPLAY ON       17290000
173000*ET THE P BOARD, BUT WILL ON THE A BOARD                          17300000
173100*ET                                                               17310000
173200*ET NOTE: CN DOES WANT TO SEE EMPLOYEES ON THEIR REST DAYS ON     17320000
173300*ET       BOTH BOARDS, REGARDLESS OF WHAT PETE SAYS. NJB          17330000
173400*ET                                                               17340000
173500                         SET DISPLAY-EMP TO TRUE                  17350000
173600                         IF AVAILABLE OR WORKING                  17360000
173700                            CONTINUE                              17370000
173800                         ELSE                                     17380000
173900                            IF NOT ALL-POSITIONS                  17390000
174000                               SET DONT-DISPLAY-EMP TO TRUE       17400000
174100                               PERFORM P9830-RETRIEVE-CNTL-INFO   17410000
174200*                              PERFORM VARYING SUB1 FROM 1 BY 1   17420000
174300*                                 UNTIL SUB1 > 5                  17430000
174400*                                 IF CNTL-XB-RETAIN-POS(SUB1) =   17440000
174500*                                    (LAYOFF-CODE-1 OR '*')       17450000
174600*                                    SET DISPLAY-EMP TO TRUE      17460000
174700*                                 END-IF                          17470000
174800*                              END-PERFORM                        17480000
174900                            END-IF                                17490000
175000                         END-IF                                   17500000
175100                         IF DISPLAY-EMP                           17510000
175200                            MOVE TURN-SUB TO NAME-SUB             17520000
175300                            PERFORM P1500-SETUP-NAME-LINE         17530000
175400                         ELSE                                     17540000
175500                            SUBTRACT 1 FROM TURN-SUB              17550000
175600                         END-IF                                   17560000
175700                      ELSE                                        17570000
175800                         SUBTRACT 1 FROM TURN-SUB                 17580000
175900                      END-IF                                      17590000
176000                   ELSE                                           17600000
176100                      SET DONE TO TRUE                            17610000
176200                   END-IF                                         17620000
176300                END-IF                                            17630000
176400              ELSE                                                17640000
176500                 SET DONE TO TRUE                                 17650000
176600              END-IF                                              17660000
176700           ELSE                                                   17670000
176800              SET DONE TO TRUE                                    17680000
176900              IF NOT (NO-RECORD-FND OR END-OF-FILE)               17690000
177000                 MOVE 'P1300-1' TO ERR-PARAGRAPH                  17700000
177100                 MOVE EBPOS     TO ERR-KEY                        17710000
177200                 PERFORM P9999-GOT-PROBLEM                        17720000
177300              END-IF                                              17730000
177400           END-IF                                                 17740000
177500        END-PERFORM                                               17750000
177600        EXEC CICS ENDBR                                           17760000
177700                  DATASET(EB-VIA-SLOW-POSITION)                   17770000
177800                  RESP(WS-RESPONSE)                               17780000
177900        END-EXEC                                                  17790000
178000     ELSE                                                         17800000
178100        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     17810000
178200           MOVE 'P1300-2' TO ERR-PARAGRAPH                        17820000
178300           MOVE EBTURN    TO ERR-KEY                              17830000
178400           PERFORM P9999-GOT-PROBLEM                              17840000
178500        END-IF                                                    17850000
178600     END-IF.                                                      17860000
178700                                                                  17870000
178800                                                                  17880000
178900*                                                                 17890000
179000 P1400-BUILD-QUAL-BOARD.                                          17900000
179100*                                                                 17910000
179200     IF DAILY-MARK-YARD                                           17920000
179300        AND SWITCHMAN-GEL-BOARD                                   17930000
179400        IF WS-LOCAL-HR > 20 AND SCR06-CC = 'S1'                   17940000
179500          ADD 1 TO DAY1                                           17950000
179600        END-IF                                                    17960000
179700        IF WS-LOCAL-HR < 07 AND SCR06-CC = 'S3'                   17970000
179800          SUBTRACT 1 FROM DAY1                                    17980000
179900        END-IF                                                    17990000
180000        IF DAY1 = ZERO                                            18000000
180100           MOVE 7 TO DAY1                                         18010000
180200        END-IF                                                    18020000
180300        IF DAY1 = 08                                              18030000
180400           MOVE 1 TO DAY1                                         18040000
180500        END-IF                                                    18050000
180600     END-IF                                                       18060000
180700     IF PFKEY8                                                    18070000
180800        AND P06CA-SCROLL-KEY > SPACES                             18080000
180900        MOVE P06CA-SCROLL-KEY TO EBPOS                            18090000
181000     ELSE                                                         18100000
181100        SET ENTER-KEY TO TRUE                                     18110000
181200        MOVE ZEROS TO P06CA-HOLD-POS                              18120000
181300        MOVE SPACES TO EBPOS-AREA                                 18130000
181400        MOVE SCR06-DIST TO DIST OF WS-EXTRA-BOARD                 18140000
181500        MOVE SCR06-SUB-DIST TO SUB-DIST OF WS-EXTRA-BOARD         18150000
181600        MOVE SCR06-CC TO CRAFT-CODE OF WS-EXTRA-BOARD             18160000
181700        MOVE ZEROS TO EB-POSITION                                 18170000
181800        IF DUAL-XB                                                18180000
181900           IF SCR06-YARD-ROAD = 'Y'                               18190000
182000              MOVE '1' TO EB-POS-BOARD                            18200000
182100           ELSE                                                   18210000
182200              IF SCR06-YARD-ROAD = 'R'                            18220000
182300                 MOVE '2' TO EB-POS-BOARD                         18230000
182400              END-IF                                              18240000
182500           END-IF                                                 18250000
182600        END-IF                                                    18260000
182700        MOVE EBPOS-AREA TO EBPOS                                  18270000
182800     END-IF                                                       18280000
182900     MOVE SPACES TO P06CA-SCROLL-KEY                              18290000
183000     EXEC CICS STARTBR                                            18300000
183100               DATASET(EB-VIA-CRAFT-POSITION)                     18310000
183200               RIDFLD(EBPOS)                                      18320000
183300               GTEQ                                               18330000
183400               RESP(WS-RESPONSE)                                  18340000
183500     END-EXEC                                                     18350000
183600     MOVE WS-RESPONSE TO FILE-STATUS                              18360000
183700     IF SUCCESS                                                   18370000
183800        IF PFKEY8                                                 18380000
183900           EXEC CICS READNEXT                                     18390000
184000                     DATASET(EB-VIA-CRAFT-POSITION)               18400000
184100                     INTO(WS-EXTRA-BOARD)                         18410000
184200                     LENGTH(EBCRPOS-RLGTH)                        18420000
184300                     RIDFLD(EBPOS)                                18430000
184400                     KEYLENGTH(EBCRPOS-KLGTH)                     18440000
184500                     RESP(WS-RESPONSE)                            18450000
184600           END-EXEC                                               18460000
184700           MOVE WS-RESPONSE TO FILE-STATUS                        18470000
184800        END-IF                                                    18480000
184900     END-IF                                                       18490000
185000     IF SUCCESS                                                   18500000
185100        MOVE '0' TO DONE-CODE                                     18510000
185200        PERFORM UNTIL DONE                                        18520000
185300           EXEC CICS READNEXT                                     18530000
185400                     DATASET(EB-VIA-CRAFT-POSITION)               18540000
185500                     INTO(WS-EXTRA-BOARD)                         18550000
185600                     LENGTH(EBCRPOS-RLGTH)                        18560000
185700                     RIDFLD(EBPOS)                                18570000
185800                     KEYLENGTH(EBCRPOS-KLGTH)                     18580000
185900                     RESP(WS-RESPONSE)                            18590000
186000           END-EXEC                                               18600000
186100           MOVE WS-RESPONSE TO FILE-STATUS                        18610000
186200*                                                          **PLS  18620000
186300           IF SUCCESS                                             18630000
186400              IF DIST OF WS-EXTRA-BOARD = SCR06-DIST              18640000
186500                 AND SUB-DIST OF WS-EXTRA-BOARD = SCR06-SUB-DIST  18650000
186600                 AND CRAFT-CODE OF WS-EXTRA-BOARD = SCR06-CC      18660000
186700                 AND ((SCR06-YARD-ROAD = 'Y' AND EB-YARD-BOARD)   18670000
186800                 OR (SCR06-YARD-ROAD = 'R' AND EB-ROAD-BOARD)     18680000
186900                 OR (SCR06-YARD-ROAD > SPACE AND WS-FASTSLOW-XB)  18690000
187000                 OR SCR06-YARD-ROAD NOT > SPACE)                  18700000
187100                 MOVE SPACES       TO TZ-PARAMETERS               18710000
187200                 SET TZ-IN-SYSTEM-ZONE TO TRUE                    18720000
187300                 MOVE EB-POS-DATE-TIME TO TZ-IN-DATE-TIME         18730000
187400                 MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE              18740000
187500                 PERFORM P8996-TIMEZONE                           18750000
187600                 MOVE TZ-OUT-DATE-TIME-CENT TO                    18760000
187700                      WS-POS-CENT-DATE-TIME-TZ                    18770000
187800                                                                  18780000
187900                 IF (WS-POS-CENT-DATE-TIME-TZ <=                  18790000
188000                    WS-VIEW-DATE-TIME-CENT                        18800000
188100                    OR WS-SYSTEM-DATE = '991231' OR '000101')     18810000
188200                    AND ((NOT WS-FASTSLOW-XB AND EB-ON-BOARD) OR  18820000
188300                         (WS-FASTSLOW-XB AND EB-ON-BOARD AND      18830000
188400                                     EB-SLOW-ON-BOARD))           18840000
188500                    ADD 1 TO TURN-SUB                             18850000
188600                    IF TURN-SUB NOT > ARRAY-MAX                   18860000
188700                      IF TURN-SUB = ARRAY-MAX                     18870000
188800                          MOVE EBPOS-AREA TO P06CA-SCROLL-KEY     18880000
188900                       END-IF                                     18890000
189000                       MOVE ZERO     TO ASGN-EMP-NO               18900000
189100                                        GOT-EMPLOYEE-FLAG         18910000
189200                       MOVE SPACES   TO WS-MSTR                   18920000
189300                       MOVE TURN-NBR TO WK-ASGN-XB-TURN           18930000
189400                       PERFORM PXXXX-LATEST-TEMP                  18940000
189500                       IF ASGN-EMP-NO NOT > ZERO                  18950000
189600                          PERFORM PXXXX-JOB-OWNER                 18960000
189700                       END-IF                                     18970000
189800                       IF ASGN-EMP-NO > ZERO                      18980000
189900                          MOVE ASGN-EMP-NO TO MSTRNBRK            18990000
190000                          PERFORM P8500-READ-MASTER               19000000
190100                          SET GOT-EMPLOYEE TO TRUE                19010000
190200                       END-IF                                     19020000
190300                       IF GOT-EMPLOYEE                            19030000
190400                          PERFORM P1505-CALL-XB-POS-PARMS         19040000
190500*ET                                                               19050000
190600*ET ENHANCED TRACKING REQUIRES THAT ALL EMPLOYEES RETAIN THEIR    19060000
190700*ET POSITIONS. HOWEVER, IF THE DISPLAY W/O TRACKING FLAGS DO      19070000
190800*ET NOT CONTAIN THEIR STATUS CODE, THEY WILL NOT DISPLAY ON       19080000
190900*ET THE P BOARD, BUT WILL ON THE A BOARD                          19090000
191000*ET                                                               19100000
191100*ET NOTE: CN DOES WANT TO SEE EMPLOYEES ON THEIR REST DAYS ON     19110000
191200*ET       BOTH BOARDS, REGARDLESS OF WHAT PETE SAYS. NJB          19120000
191300*ET                                                               19130000
191400                          SET DISPLAY-EMP TO TRUE                 19140000
191500                          IF AVAILABLE OR WORKING                 19150000
191600                             CONTINUE                             19160000
191700                          ELSE                                    19170000
191800                             IF NOT ALL-POSITIONS                 19180000
191900                                SET DONT-DISPLAY-EMP TO TRUE      19190000
192000                                PERFORM P9830-RETRIEVE-CNTL-INFO  19200000
192100                             END-IF                               19210000
192200                          END-IF                                  19220000
192300                          IF DISPLAY-EMP                          19230000
192400                             MOVE TURN-SUB TO NAME-SUB            19240000
192500                             PERFORM P1500-SETUP-NAME-LINE        19250000
192600                          ELSE                                    19260000
192700                             SUBTRACT 1 FROM TURN-SUB             19270000
192800                          END-IF                                  19280000
192900                       ELSE                                       19290000
193000                          SUBTRACT 1 FROM TURN-SUB                19300000
193100                       END-IF                                     19310000
193200                    ELSE                                          19320000
193300                       SET DONE TO TRUE                           19330000
193400                    END-IF                                        19340000
193500                 END-IF                                           19350000
193600              ELSE                                                19360000
193700                 SET DONE TO TRUE                                 19370000
193800              END-IF                                              19380000
193900           ELSE                                                   19390000
194000              SET DONE TO TRUE                                    19400000
194100              IF NOT (NO-RECORD-FND OR END-OF-FILE)               19410000
194200                 MOVE 'P1300-1' TO ERR-PARAGRAPH                  19420000
194300                 MOVE EBPOS     TO ERR-KEY                        19430000
194400                 PERFORM P9999-GOT-PROBLEM                        19440000
194500              END-IF                                              19450000
194600           END-IF                                                 19460000
194700        END-PERFORM                                               19470000
194800        EXEC CICS ENDBR                                           19480000
194900                  DATASET(EB-VIA-CRAFT-POSITION)                  19490000
195000                  RESP(WS-RESPONSE)                               19500000
195100        END-EXEC                                                  19510000
195200     ELSE                                                         19520000
195300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     19530000
195400           MOVE 'P1300-2' TO ERR-PARAGRAPH                        19540000
195500           MOVE EBTURN    TO ERR-KEY                              19550000
195600           PERFORM P9999-GOT-PROBLEM                              19560000
195700        END-IF                                                    19570000
195800     END-IF.                                                      19580000
195900*                                                                 19590000
196000 P1450-BUILD-SLOW-QUAL-BOARD.                                     19600000
196100*                                                                 19610000
196200*  USE THE SLOW-POSITION KEY TO SET UP THE POSITION BOARD,        19620000
196300*  IF IT'S A FASTSLOW BOARD, AND "R" (ROAD) WAS REQUESTED.        19630000
196400*                                                          **PLS  19640000
196500     IF DAILY-MARK-YARD                                           19650000
196600        AND SWITCHMAN-GEL-BOARD                                   19660000
196700        IF WS-LOCAL-HR > 20 AND SCR06-CC = 'S1'                   19670000
196800          ADD 1 TO DAY1                                           19680000
196900        END-IF                                                    19690000
197000        IF WS-LOCAL-HR < 07 AND SCR06-CC = 'S3'                   19700000
197100          SUBTRACT 1 FROM DAY1                                    19710000
197200        END-IF                                                    19720000
197300        IF DAY1 = ZERO                                            19730000
197400           MOVE 7 TO DAY1                                         19740000
197500        END-IF                                                    19750000
197600        IF DAY1 = 08                                              19760000
197700           MOVE 1 TO DAY1                                         19770000
197800        END-IF                                                    19780000
197900     END-IF                                                       19790000
198000     IF PFKEY8                                                    19800000
198100        AND P06CA-SCROLL-KEY > SPACES                             19810000
198200        MOVE P06CA-SCROLL-KEY TO EBSLOW-POS                       19820000
198300     ELSE                                                         19830000
198400        SET ENTER-KEY TO TRUE                                     19840000
198500        MOVE ZEROS TO P06CA-HOLD-POS                              19850000
198600        MOVE SPACES TO EBSLOW-POS                                 19860000
198700        MOVE SCR06-DIST TO DIST OF EBSLOW-POS                     19870000
198800        MOVE SCR06-SUB-DIST TO SUBDIST OF EBSLOW-POS              19880000
198900        MOVE SCR06-CC TO CRAFT-CODE OF EBSLOW-POS                 19890000
199000        MOVE ZEROS TO EBSLOWPOS                                   19900000
199100     END-IF                                                       19910000
199200     MOVE SPACES TO P06CA-SCROLL-KEY                              19920000
199300     EXEC CICS STARTBR                                            19930000
199400               DATASET(EB-VIA-SLOW-POSITION)                      19940000
199500               RIDFLD(EBSLOW-POS)                                 19950000
199600               GTEQ                                               19960000
199700               RESP(WS-RESPONSE)                                  19970000
199800     END-EXEC                                                     19980000
199900     MOVE WS-RESPONSE TO FILE-STATUS                              19990000
200000     IF SUCCESS                                                   20000000
200100        IF PFKEY8                                                 20010000
200200           EXEC CICS READNEXT                                     20020000
200300                     DATASET(EB-VIA-SLOW-POSITION)                20030000
200400                     INTO(WS-EXTRA-BOARD)                         20040000
200500                     LENGTH(EBSLPOS-RLGTH)                        20050000
200600                     RIDFLD(EBSLOW-POS)                           20060000
200700                     KEYLENGTH(EBSLPOS-KLGTH)                     20070000
200800                     RESP(WS-RESPONSE)                            20080000
200900           END-EXEC                                               20090000
201000           MOVE WS-RESPONSE TO FILE-STATUS                        20100000
201100        END-IF                                                    20110000
201200     END-IF                                                       20120000
201300     IF SUCCESS                                                   20130000
201400        MOVE '0' TO DONE-CODE                                     20140000
201500        PERFORM UNTIL DONE                                        20150000
201600           EXEC CICS READNEXT                                     20160000
201700                     DATASET(EB-VIA-SLOW-POSITION)                20170000
201800                     INTO(WS-EXTRA-BOARD)                         20180000
201900                     LENGTH(EBSLPOS-RLGTH)                        20190000
202000                     RIDFLD(EBSLOW-POS)                           20200000
202100                     KEYLENGTH(EBSLPOS-KLGTH)                     20210000
202200                     RESP(WS-RESPONSE)                            20220000
202300           END-EXEC                                               20230000
202400           MOVE WS-RESPONSE TO FILE-STATUS                        20240000
202500           IF SUCCESS                                             20250000
202600              IF DIST OF WS-EXTRA-BOARD = SCR06-DIST              20260000
202700                 AND SUB-DIST OF WS-EXTRA-BOARD = SCR06-SUB-DIST  20270000
202800                 AND CRAFT-CODE OF WS-EXTRA-BOARD = SCR06-CC      20280000
202900                 AND ((SCR06-YARD-ROAD = 'Y' AND EB-YARD-BOARD)   20290000
203000                 OR (SCR06-YARD-ROAD = 'R' AND EB-ROAD-BOARD)     20300000
203100                 OR (SCR06-YARD-ROAD > SPACE AND WS-FASTSLOW-XB)  20310000
203200                 OR SCR06-YARD-ROAD NOT > SPACE)                  20320000
203300                 MOVE SPACES       TO TZ-PARAMETERS               20330000
203400                 SET TZ-IN-SYSTEM-ZONE TO TRUE                    20340000
203500                 MOVE EB-SLOW-POS-DATE-TIME TO TZ-IN-DATE-TIME    20350000
203600                 MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE              20360000
203700                 PERFORM P8996-TIMEZONE                           20370000
203800*                MOVE TZ-OUT-DATE-TIME TO WS-POS-DATE-TIME-TZ     20380000
203900                 MOVE TZ-OUT-DATE-TIME-CENT TO                    20390000
204000                      WS-POS-CENT-DATE-TIME-TZ                    20400000
204100*-------------------------------------------------------------    20410000
204200*       WHEN DISPLAYING THE SLOW SIDE OF A FASTSLOW BOARD,        20420000
204300*       VERIFY THE ON-BOARD STATUS OF THE SLOW BOARD ONLY.        20430000
204400*       IF 'OFF' ON THE FAST(YARD) SIDE, CONSIDER THE TURN        20440000
204500*       AS ON THE BOARD FOR THE SLOW SIDE.                        20450000
204600*----------------------------------------------------------PLS    20460000
204700*                IF WS-POS-DATE-TIME-TZ <= WS-VIEW-DATE-TIME      20470000
204800                 IF (WS-POS-CENT-DATE-TIME-TZ <=                  20480000
204900                    WS-VIEW-DATE-TIME-CENT                        20490000
205000                    OR WS-SYSTEM-DATE = '991231' OR '000101')     20500000
205100                    AND EB-SLOW-ON-BOARD                          20510000
205200                    ADD 1 TO TURN-SUB                             20520000
205300                    IF TURN-SUB NOT > ARRAY-MAX                   20530000
205400                      IF TURN-SUB = ARRAY-MAX                     20540000
205500                        MOVE EB-SLOW-POS-AREA TO P06CA-SCROLL-KEY 20550000
205600                      END-IF                                      20560000
205700                      MOVE ZERO     TO ASGN-EMP-NO                20570000
205800                                       GOT-EMPLOYEE-FLAG          20580000
205900                      MOVE SPACES   TO WS-MSTR                    20590000
206000                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            20600000
206100                      PERFORM PXXXX-LATEST-TEMP                   20610000
206200                      IF ASGN-EMP-NO NOT > ZERO                   20620000
206300                         PERFORM PXXXX-JOB-OWNER                  20630000
206400                      END-IF                                      20640000
206500                      IF ASGN-EMP-NO > ZERO                       20650000
206600                         MOVE ASGN-EMP-NO TO MSTRNBRK             20660000
206700                         PERFORM P8500-READ-MASTER                20670000
206800                         SET GOT-EMPLOYEE TO TRUE                 20680000
206900                      END-IF                                      20690000
207000                      IF GOT-EMPLOYEE                             20700000
207100                         PERFORM P1505-CALL-XB-POS-PARMS          20710000
207200*ET                                                               20720000
207300*ET ENHANCED TRACKING REQUIRES THAT ALL EMPLOYEES RETAIN THEIR    20730000
207400*ET POSITIONS. HOWEVER, IT THE DISPLAY W/O TRACKING FLAGS DO      20740000
207500*ET NOT CONTAIN THEIR STATUS CODE, THEY WILL NOT DISPLAY ON       20750000
207600*ET THE P BOARD, BUT WILL ON THE A BOARD                          20760000
207700*ET                                                               20770000
207800*ET NOTE: CN DOES WANT TO SEE EMPLOYEES ON THEIR REST DAYS ON     20780000
207900*ET       BOTH BOARDS, REGARDLESS OF WHAT PETE SAYS. NJB          20790000
208000*ET                                                               20800000
208100                         SET DISPLAY-EMP TO TRUE                  20810000
208200                         IF AVAILABLE OR WORKING                  20820000
208300                            CONTINUE                              20830000
208400                         ELSE                                     20840000
208500                            IF NOT ALL-POSITIONS                  20850000
208600                               SET DONT-DISPLAY-EMP TO TRUE       20860000
208700                               PERFORM P9830-RETRIEVE-CNTL-INFO   20870000
208800*                              PERFORM VARYING SUB1 FROM 1 BY 1   20880000
208900*                                 UNTIL SUB1 > 5                  20890000
209000*                                 IF CNTL-XB-RETAIN-POS(SUB1) =   20900000
209100*                                    (LAYOFF-CODE-1 OR '*')       20910000
209200*                                    SET DISPLAY-EMP TO TRUE      20920000
209300*                                 END-IF                          20930000
209400*                              END-PERFORM                        20940000
209500                            END-IF                                20950000
209600                         END-IF                                   20960000
209700                         IF DISPLAY-EMP                           20970000
209800                            MOVE TURN-SUB TO NAME-SUB             20980000
209900                            PERFORM P1500-SETUP-NAME-LINE         20990000
210000                         ELSE                                     21000000
210100                            SUBTRACT 1 FROM TURN-SUB              21010000
210200                         END-IF                                   21020000
210300                      ELSE                                        21030000
210400                         SUBTRACT 1 FROM TURN-SUB                 21040000
210500                      END-IF                                      21050000
210600                   ELSE                                           21060000
210700                      SET DONE TO TRUE                            21070000
210800                   END-IF                                         21080000
210900                END-IF                                            21090000
211000              ELSE                                                21100000
211100                 SET DONE TO TRUE                                 21110000
211200              END-IF                                              21120000
211300           ELSE                                                   21130000
211400              SET DONE TO TRUE                                    21140000
211500              IF NOT (NO-RECORD-FND OR END-OF-FILE)               21150000
211600                 MOVE 'P1300-1' TO ERR-PARAGRAPH                  21160000
211700                 MOVE EBPOS     TO ERR-KEY                        21170000
211800                 PERFORM P9999-GOT-PROBLEM                        21180000
211900              END-IF                                              21190000
212000           END-IF                                                 21200000
212100        END-PERFORM                                               21210000
212200        EXEC CICS ENDBR                                           21220000
212300                  DATASET(EB-VIA-SLOW-POSITION)                   21230000
212400                  RESP(WS-RESPONSE)                               21240000
212500        END-EXEC                                                  21250000
212600     ELSE                                                         21260000
212700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     21270000
212800           MOVE 'P1300-2' TO ERR-PARAGRAPH                        21280000
212900           MOVE EBTURN    TO ERR-KEY                              21290000
213000           PERFORM P9999-GOT-PROBLEM                              21300000
213100        END-IF                                                    21310000
213200     END-IF.                                                      21320000
213300                                                                  21330000
213400                                                                  21340000
213500*                                                                 21350000
213600 P1500-SETUP-NAME-LINE.                                           21360000
213700*                                                                 21370000
213800     IF GOT-EMPLOYEE                                              21380000
213900        MOVE EMP-NBR OF WS-MSTR TO P06CA-EMP-NO(NAME-SUB)         21390000
214000        IF TEMPORARY-ASGNMT > SPACE                               21400000
214100            AND TEMP-ASGN-XB-AUG                                  21410000
214200            AND TA-DIST = DIST-REPEAT                             21420000
214300            AND TA-SUB-DIST = SUBDIST-REPEAT                      21430000
214400            AND TA-XB-TURN = TURN-NBR OF WS-EXTRA-BOARD           21440000
214500            AND TA-CC = CRAFT-CODE-REPEAT                         21450000
214600            MOVE EMP-NAME OF WS-MSTR TO WS-FORMAT-NAME-AUG        21460000
214700            MOVE ' /AUG' TO WS-FORMAT-NAME-AUG-FIELD              21470000
214800            MOVE WS-FORMAT-NAME-AUG TO SCR06-NAME(NAME-SUB)       21480000
214900         ELSE                                                     21490000
215000            IF PROTECTION-IS-IN-EFFECT                            21500000
215100               MOVE SPACES TO PROTECTED-EMPLOYEE-PARMS            21510000
215200               MOVE EMP-NBR OF WS-MSTR TO PROT-EMP-NUMBER-PARM    21520000
215300               EXEC CICS LINK                                     21530000
215400                         PROGRAM(P907-PGM)                        21540000
215500                         COMMAREA(PROTECTED-EMPLOYEE-PARMS)       21550000
215600                         LENGTH(P907-LGTH)                        21560000
215700                         RESP(WS-RESPONSE)                        21570000
215800               END-EXEC                                           21580000
215900               MOVE WS-RESPONSE TO FILE-STATUS                    21590000
216000               IF NOT SUCCESS                                     21600000
216100                  MOVE 'P1500-1' TO ERR-PARAGRAPH                 21610000
216200                  PERFORM P9999-GOT-PROBLEM                       21620000
216300               END-IF                                             21630000
216400            END-IF                                                21640000
216500            IF EMPLOYEE-IS-PROTECTED                              21650000
216600               MOVE EMP-NAME OF WS-MSTR                           21660000
216700                             TO WS-FORMAT-NAME-PROT               21670000
216800               MOVE ' /PROT' TO WS-FORMAT-NAME-PROT-FIELD         21680000
216900               MOVE WS-FORMAT-NAME-PROT                           21690000
217000                             TO SCR06-NAME(NAME-SUB)              21700000
217100            ELSE                                                  21710000
217200               MOVE EMP-NAME OF WS-MSTR                           21720000
217300                             TO SCR06-NAME(NAME-SUB)              21730000
217400            END-IF                                                21740000
217500         END-IF                                                   21750000
217600     ELSE                                                         21760000
217700        IF PSTCA-SUB = 2                                          21770000
217800           MOVE '   TOUR VACANT        ' TO SCR06-NAME(NAME-SUB)  21780000
217900        ELSE                                                      21790000
218000           MOVE '   OPEN TURN          ' TO SCR06-NAME(NAME-SUB)  21800000
218100        END-IF                                                    21810000
218200        MOVE '000000000'              TO P06CA-EMP-NO(NAME-SUB)   21820000
218300     END-IF                                                       21830000
218400     MOVE TURN-NBR OF WS-EXTRA-BOARD TO SCR06-TURN(NAME-SUB)      21840000
218500     MOVE EBTURN-AREA TO P06CA-TURN-KEY(NAME-SUB)                 21850000
218600     PERFORM P1505-CALL-XB-POS-PARMS                              21860000
218700                                                                  21870000
218800     IF POSITION-BOARD                                            21880000
218900        IF REPOSITION-REQ                                         21890000
219000           MOVE SPACES TO WS-VARIABLE-LINE-3                      21900000
219100*                                                                 21910000
219200*          CONVERT SYSTEM TIME     TO LOCAL-TIME                  21920000
219300*                                                                 21930000
219400           MOVE SPACES             TO TZ-PARAMETERS               21940000
219500           SET TZ-IN-EASTERN-ZONE  TO TRUE                        21950000
219600*                                                                 21960000
219700*        IF SLOW SIDE OF FASTSLOW BOARD REQUESTED, USE THE SLOW   21970000
219800*        POSITION DATA.  OTHERWISE, USE THE NORMAL POSITION.      21980000
219900*                                                          **PLS  21990000
220000           IF WS-FASTSLOW-XB                                      22000000
220100              AND SCR06-YARD-ROAD = 'R'                           22010000
220200              MOVE EB-SLOW-POS-DATE-TIME   TO TZ-IN-DATE-TIME     22020000
220300              MOVE EB-SLOW-POS-TIE-BREAKER TO WS-VL3-TIE-CODE     22030000
220400           ELSE                                                   22040000
220500              MOVE EB-POS-DATE-TIME        TO TZ-IN-DATE-TIME     22050000
220600              MOVE EB-POS-TIE-BREAKER      TO WS-VL3-TIE-CODE     22060000
220700           END-IF                                                 22070000
220800           MOVE PSTCA-TIME-ZONE    TO TZ-OUT-ZONE                 22080000
220900           PERFORM P8996-TIMEZONE                                 22090000
221000           MOVE TZ-OUT-DATE-TIME   TO WS-VL3-DATE-TIME            22100000
221100           MOVE '-'                TO WS-VL3-DASH                 22110000
221200*                                                                 22120000
221300           IF TAG-XB                                              22130000
221400              AND TAG-YOUR-IT                                     22140000
221500              MOVE EB-TAG TO WS-VL3-TAG                           22150000
221600           END-IF                                                 22160000
221700           MOVE WS-VARIABLE-LINE-3 TO SCR06-VARIABLE(NAME-SUB)    22170000
221800        ELSE                                                      22180000
221900           MOVE SPACES TO WS-VARIABLE-LINE-1                      22190000
222000           ADD 1 TO P06CA-HOLD-POS-NUM                            22200000
222100           MOVE P06CA-HOLD-POS-NUM TO WS-VL1-POS                  22210000
222200           IF TAG-XB                                              22220000
222300              AND TAG-YOUR-IT                                     22230000
222400              MOVE EB-TAG TO WS-VL1-TAG                           22240000
222500           END-IF                                                 22250000
222600           IF GOT-EMPLOYEE                                        22260000
222700              IF TEMPORARY-ASGNMT > SPACE AND                     22270000
222800                 NOT (TA-DIST = DIST-REPEAT AND                   22280000
222900                 TA-SUB-DIST = SUBDIST-REPEAT AND                 22290000
223000                 TA-CC = CRAFT-CODE-REPEAT AND                    22300000
223100                 TA-TURN = TURN-NBR)                              22310000
223200                 MOVE 'TV'    TO WS-VL1-LO                        22320000
223300              ELSE                                                22330000
223400                 IF LAYOFF-CODE NOT = 'A0'                        22340000
223500                    MOVE LAYOFF-CODE TO WS-VL1-LO                 22350000
223600                 ELSE                                             22360000
223700                    IF OUT-TOWN                                   22370000
223800                       MOVE 'OT' TO WS-VL1-LO                     22380000
223900                    END-IF                                        22390000
224000                 END-IF                                           22400000
224100              END-IF                                              22410000
224200*                                                                 22420000
224300*             CNC0183 - MATT - 12/11/99                           22430000
224400*                                                                 22440000
224500              MOVE ZEROS                  TO DE-COMPARE1-DATE-TIME22450000
224600              IF EMP-MTOD > '0000000000'                          22460000
224700                 SET DE-YYMMDD-FORMAT     TO TRUE                 22470000
224800                 MOVE EMP-MTOD(1:6)       TO DE-YYMMDD            22480000
224900                 PERFORM P8998-DATEEDIT                           22490000
225000                 MOVE DE-CCYYMMDD         TO DE-COMPARE1-DATE     22500000
225100                 MOVE EMP-MTOD(7:4)       TO DE-COMPARE1-TIME     22510000
225200              END-IF                                              22520000
225300              IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT  22530000
225400                 MOVE DE-COMPARE1-TIME    TO WS-VL1-MTOD          22540000
225500                 IF EMP-PREV-DUTY-MTOD IS NUMERIC                 22550000
225600                    AND EMP-PREV-DUTY-MTOD > '0000'               22560000
225700                    MOVE EMP-PREV-DUTY-MTOD TO WS-VL1-MTPD        22570000
225800                 END-IF                                           22580000
225900              END-IF                                              22590000
226000*                                                                 22600000
226100              IF EMP-US-RSTD IS NUMERIC                           22610000
226200                 SET DE-YYMMDD-FORMAT     TO TRUE                 22620000
226300                 MOVE EMP-US-RSTD(1:6)    TO DE-YYMMDD            22630000
226400                 PERFORM P8998-DATEEDIT                           22640000
226500                 MOVE DE-CCYYMMDD         TO DE-COMPARE1-DATE     22650000
226600                 MOVE EMP-US-RSTD(7:4)    TO DE-COMPARE1-TIME     22660000
226700*                AND EMP-US-RSTD > WS-LOCAL-DATE-TIME             22670000
226800                 IF DE-COMPARE1-DATE-TIME >                       22680000
226900                          WS-LOCAL-DATE-TIME-CENT                 22690000
227000                    MOVE EMP-US-RSTD-TIME TO WS-VL1-USHR          22700000
227100                    IF EMP-PREV-DUTY IS NUMERIC                   22710000
227200                       AND EMP-PREV-DUTY > '0000'                 22720000
227300                       MOVE EMP-PREV-DUTY TO WS-VL1-PD            22730000
227400                    END-IF                                        22740000
227500                 END-IF                                           22750000
227600              END-IF                                              22760000
227700              IF EMP-PERS-REST IS NUMERIC                         22770000
227800                 SET DE-YYMMDD-FORMAT        TO TRUE              22780000
227900                 MOVE EMP-PERS-REST(1:6)     TO DE-YYMMDD         22790000
228000                 PERFORM P8998-DATEEDIT                           22800000
228100                 MOVE DE-CCYYMMDD            TO DE-COMPARE1-DATE  22810000
228200                 MOVE EMP-PERS-REST(7:4)     TO DE-COMPARE1-TIME  22820000
228300*                AND EMP-PERS-REST > WS-LOCAL-DATE-TIME           22830000
228400                 IF DE-COMPARE1-DATE-TIME >                       22840000
228500                            WS-LOCAL-DATE-TIME-CENT               22850000
228600                    MOVE EMP-PERS-REST-TIME TO WS-VL1-PERS        22860000
228610                    MOVE EMP-PERS-REST-DATE(5:2) TO WS-VL1-PERS-DY22861005
228700                 END-IF                                           22870000
228800              END-IF                                              22880000
228900*-------------------------------------------------------------    22890000
229000*           FOR A FASTSLOW BOARD, CHECK BOTH ON-BOARD FLAGS       22900000
229100*           FOR THE FAST(YARD) SIDE, BUT ONLY CHECK THE SLOW      22910000
229200*           ON BOARD FLAG FOR THE SLOW(ROAD) SIDE.                22920000
229300*----------------------------------------------------------PLS    22930000
229400              IF WS-FASTSLOW-XB                                   22940000
229500                 IF ((SCR06-YARD-ROAD = 'R' AND EB-SLOW-ON-BOARD) 22950000
229600                   OR (SCR06-YARD-ROAD = 'Y' AND EB-ON-BOARD      22960000
229700                                           AND EB-SLOW-ON-BOARD)) 22970000
229800                   AND EB-SHORT-TURN                              22980000
229900                   AND TEMPORARY-ASGNMT NOT > SPACE               22990000
230000                   AND ON-DUTY-ASGNMT NOT > SPACE                 23000000
230100                    MOVE 'ST' TO WS-VL1-SHRT                      23010000
230200                 END-IF                                           23020000
230300              ELSE                                                23030000
230400                 IF EB-ON-BOARD                                   23040000
230500                   AND EB-SHORT-TURN                              23050000
230600                   AND TEMPORARY-ASGNMT NOT > SPACE               23060000
230700                   AND ON-DUTY-ASGNMT NOT > SPACE                 23070000
230800                    MOVE 'ST' TO WS-VL1-SHRT                      23080000
230900                 END-IF                                           23090000
231000              END-IF                                              23100000
231100              IF SCR06-YARD-ROAD = 'R'                            23110000
231200*    CNC0006 - FLW, 5/8/96, START                                 23120000
231300                 MOVE EB-NBR-YARD-STT-STARTS-PICX                 23130000
231400                                   TO WS-VL1-STT-STARTS           23140000
231500                 MOVE EB-NBR-YARD-OVT-STARTS-PICX                 23150000
231600                                   TO WS-VL1-OVT-STARTS           23160000
231700              ELSE                                                23170000
231800                 PERFORM P1510-INQUIRE-STARTS                     23180000
231900*    CNC0006 - FLW, 5/8/96, END                                   23190000
232000              END-IF                                              23200000
232100           END-IF                                                 23210000
232200           IF CNTL-XB-SCHEDULED                                   23220000
232210              OR CNTL-XB-EXTENDED-SCHED                           23221002
232300              PERFORM P1550-CHECK-SCHEDULE                        23230000
232400           END-IF                                                 23240000
232500           MOVE WS-VARIABLE-LINE-1 TO SCR06-VARIABLE(NAME-SUB)    23250000
232600        END-IF                                                    23260000
232700     ELSE                                                         23270000
232800        IF QUAL-BOARD                                             23280000
232900           MOVE SPACES             TO WS-VARIABLE-LINE-4          23290000
233000           ADD 1                   TO P06CA-HOLD-POS-NUM          23300000
233100           MOVE P06CA-HOLD-POS-NUM TO WS-VL4-POS                  23310000
233200           IF GOT-EMPLOYEE                                        23320000
233300              PERFORM P1520-SET-SENIORITY-ARRAY                   23330000
233400              PERFORM P1540-SET-QUAL-ARRAY                        23340000
233500           END-IF                                                 23350000
233600           MOVE WS-VARIABLE-LINE-4 TO SCR06-VARIABLE(NAME-SUB)    23360000
233700        ELSE                                                      23370000
233800           EXEC CICS ENDBR                                        23380000
233900                     DATASET(EB-VIA-TURN-NBR)                     23390000
234000                     RESP(WS-RESPONSE)                            23400000
234100           END-EXEC                                               23410000
234200           MOVE SPACES TO WS-VARIABLE-LINE-2                      23420000
234300           EXEC CICS STARTBR                                      23430000
234400                     DATASET(EB-VIA-TURN-NBR)                     23440000
234500                     RIDFLD(EBTURN)                               23450000
234600                     GTEQ                                         23460000
234700                     RESP(WS-RESPONSE)                            23470000
234800           END-EXEC                                               23480000
234900           MOVE WS-RESPONSE TO FILE-STATUS                        23490000
235000           IF NOT SUCCESS                                         23500000
235100              MOVE 'P1500-2' TO ERR-PARAGRAPH                     23510000
235200              MOVE EBTURN TO ERR-KEY                              23520000
235300              PERFORM P9999-GOT-PROBLEM                           23530000
235400           END-IF                                                 23540000
235500           EXEC CICS READNEXT                                     23550000
235600                     DATASET(EB-VIA-TURN-NBR)                     23560000
235700                     INTO(WS-EXTRA-BOARD)                         23570000
235800                     LENGTH(EBTURNNO-RLGTH)                       23580000
235900                     RIDFLD(EBTURN)                               23590000
236000                     KEYLENGTH(EBTURNNO-KLGTH)                    23600000
236100                     RESP(WS-RESPONSE)                            23610000
236200           END-EXEC                                               23620000
236300           MOVE WS-RESPONSE TO FILE-STATUS                        23630000
236400           IF NOT SUCCESS                                         23640000
236500              MOVE 'P1500-3' TO ERR-PARAGRAPH                     23650000
236600              MOVE EBTURN TO ERR-KEY                              23660000
236700              PERFORM P9999-GOT-PROBLEM                           23670000
236800           END-IF                                                 23680000
236900           MOVE P912-POS-PARM-NUM TO WS-VL2-POS                   23690000
237000           IF DUAL-XB                                             23700000
237100              IF EB-ON-BOARD                                      23710000
237200                 IF EB-YARD-BOARD                                 23720000
237300                    MOVE 'YD' TO WS-VL2-BOARD                     23730000
237400                 END-IF                                           23740000
237500                 IF EB-ROAD-BOARD                                 23750000
237600                    MOVE 'RD' TO WS-VL2-BOARD                     23760000
237700                 END-IF                                           23770000
237800              END-IF                                              23780000
237900           END-IF                                                 23790000
238000           IF GOT-EMPLOYEE                                        23800000
238100              IF TEMPORARY-ASGNMT > SPACE AND                     23810000
238200                 NOT (TA-DIST = DIST-REPEAT AND                   23820000
238300                 TA-SUB-DIST = SUBDIST-REPEAT AND                 23830000
238400                 TA-CC = CRAFT-CODE-REPEAT AND                    23840000
238500                 TA-TURN = TURN-NBR)                              23850000
238600                  MOVE 'TV' TO WS-VL2-LO                          23860000
238700                  MOVE TA-AREA TO WS-VL2-ASGN                     23870000
238800              ELSE                                                23880000
238900                 IF LAYOFF-CODE NOT = 'A0'                        23890000
239000                    MOVE LAYOFF-CODE TO WS-VL2-LO                 23900000
239100                    IF OD-AREA > SPACES                           23910000
239200                       MOVE OD-AREA TO WS-VL2-ASGN                23920000
239300                    END-IF                                        23930000
239400                 ELSE                                             23940000
239500                    IF OUT-TOWN                                   23950000
239600                       MOVE 'OT' TO WS-VL2-LO                     23960000
239700                       MOVE OD-AREA TO WS-VL2-ASGN                23970000
239800                    END-IF                                        23980000
239900                 END-IF                                           23990000
240000              END-IF                                              24000000
240100              IF SENIORITY-BOARD                                  24010000
240200                 SUBTRACT 1      FROM SEN-SUB                     24020000
240300                 MOVE SEN-SUB      TO WS-VL2-SEN-LVL              24030000
240400                 MOVE SF-DATE      TO WS-VL2-SEN-DATE             24040000
240500                 ADD 1             TO SEN-SUB                     24050000
240600              ELSE                                                24060000
240700                 PERFORM P3050-SET-UP-P942LINK                    24070000
240800                 MOVE P942-EMP-SEN-LEVEL TO WS-VL2-SEN-LVL        24080000
240900                 MOVE P942-EMP-SEN-DATE TO WS-VL2-SEN-DATE        24090000
241000              END-IF                                              24100000
241100           END-IF                                                 24110000
241200           MOVE SPACES TO WS-VL2-REST-DAYS                        24120000
241300                          WS-VL2-AVAIL-START                      24130000
241400                          WS-VL2-HYPHEN                           24140000
241500                          WS-VL2-AVAIL-END                        24150000
241600           IF CNTL-XB-SCHEDULED                                   24160000
241610              OR CNTL-XB-EXTENDED-SCHED                           24161002
241700              PERFORM P1550-CHECK-SCHEDULE                        24170000
241800              PERFORM VARYING WS-REST-SUB FROM +1 BY +1           24180000
241900                 UNTIL WS-REST-SUB > 7                            24190000
242000                 IF P912-JOB-ON-REST-DAY(WS-REST-SUB)             24200000
242010                    IF CNTL-XB-SCHEDULED                          24201002
242100                       MOVE DAYOFWK-2CHAR(WS-REST-SUB, PSTCA-SUB) 24210002
242200                                  TO WS-VL2-REST-DAY(WS-REST-SUB) 24220002
242210                    ELSE                                          24221002
242211                       PERFORM P1560-GET-REST-DAY-DESC            24221102
242240                    END-IF                                        24224002
242300                 END-IF                                           24230000
242400              END-PERFORM                                         24240000
242500              IF P912-AVAILABLE-START-TIME > SPACES               24250000
242600              MOVE P912-AVAILABLE-START-TIME TO WS-VL2-AVAIL-START24260000
242700                 MOVE '-'               TO WS-VL2-HYPHEN          24270000
242800                 MOVE P912-AVAILABLE-END-TIME TO WS-VL2-AVAIL-END 24280000
242900              END-IF                                              24290000
243000           END-IF                                                 24300000
243100           MOVE WS-VARIABLE-LINE-2   TO SCR06-VARIABLE(NAME-SUB)  24310002
243200        END-IF                                                    24320000
243300     END-IF.                                                      24330000
243400*                                                                 24340000
243500 P1505-CALL-XB-POS-PARMS.                                         24350000
243600*                                                                 24360000
243700*                                                                 24370000
243800*    IF USING THE SLOW SIDE OF THE FAST/SLOW BOARD, SET           24380000
243900*    THE PARM TO GET THE RELATIVE POSITION OF THE SLOW            24390000
244000*    POSITION.                                             **PLS  24400000
244100*                                                                 24410000
244200*    ALSO USING P912 TO GET SCHEDULED SPAREBOARD INFORMATION      24420000
244300*    SO IT NEEDS TO BE CALLED FOR BOTH POSITION AND TURN          24430000
244400*    REQUESTS.                      NORTHERN QUEBEC SPAREBOARDS   24440000
244500*                                                                 24450000
244600     MOVE SPACES TO P912-XB-POS-PARMS                             24460000
244700     MOVE EBTURN-AREA  TO P912-TURN-PARM                          24470000
244800     IF WS-FASTSLOW-XB                                            24480000
244900        AND SCR06-YARD-ROAD = 'R'                                 24490000
245000        SET P912-SLOW-BOARD       TO TRUE                         24500000
245100     ELSE                                                         24510000
245200        MOVE SPACE        TO P912-FASTSLOW-PARM                   24520000
245300     END-IF                                                       24530000
246400     IF      WS-VIEW-TIME NOT = WS-LOCAL-TIME AND                 24640000
246500             WS-VIEW-TIME     > SPACES                            24650000
246600        MOVE WS-VIEW-TIME    TO P912-VIEW-TIME                    24660000
246700     ELSE                                                         24670000
246800        MOVE WS-LOCAL-TIME   TO P912-VIEW-TIME                    24680000
246900     END-IF                                                       24690000
247000     IF      P912-VIEW-TIME   < '0001'                            24700000
247100        MOVE '0001'          TO P912-VIEW-TIME                    24710000
247200     END-IF                                                       24720000
247300     MOVE    DAY1            TO P912-VIEW-DAY-N                   24730000
247400     IF      P912-VIEW-TIME   < WS-LOCAL-TIME                     24740000
247500        ADD  1               TO P912-VIEW-DAY-N                   24750000
247600        IF   P912-VIEW-DAY-N  > 7                                 24760000
247700           MOVE 1            TO P912-VIEW-DAY-N                   24770000
247800        END-IF                                                    24780000
247900     END-IF                                                       24790000
248000     EXEC CICS LINK                                               24800000
248100               PROGRAM(P912-PGM)                                  24810000
248200               COMMAREA(P912-XB-POS-PARMS)                        24820000
248300               LENGTH(P912-LGTH)                                  24830000
248400               RESP(WS-RESPONSE)                                  24840000
248500     END-EXEC                                                     24850000
248600     MOVE WS-RESPONSE TO FILE-STATUS                              24860000
248700     IF NOT SUCCESS                                               24870000
248800        MOVE 'P1505-1' TO ERR-PARAGRAPH                           24880000
248900        PERFORM P9999-GOT-PROBLEM                                 24890000
249000     END-IF.                                                      24900000
249100*                                                                 24910000
249200 P1510-INQUIRE-STARTS.                                            24920000
249300*                                                                 24930000
249400     MOVE SPACES                    TO P931-COMMAREA-PARMS        24940000
249500     SET P931-INQUIRY-FUN           TO TRUE                       24950000
249600     MOVE EMP-NBR IN WS-MSTR        TO P931-INQ-EMP-NO            24960000
249700     SET P931-INQ-STARTS            TO TRUE                       24970000
249800     SET P931-INQ-MILES             TO TRUE                       24980000
249900     SET P931-CURRENT-PERIOD        TO TRUE                       24990000
250000     SET P931-MASTER-INQUIRY        TO TRUE                       25000000
250100     EXEC CICS LINK                                               25010000
250200               PROGRAM(P931-PGM)                                  25020000
250300               COMMAREA(P931-COMMAREA-PARMS)                      25030000
250400               LENGTH(P931-LGTH)                                  25040000
250500               RESP(WS-RESPONSE)                                  25050000
250600     END-EXEC                                                     25060000
250700     MOVE WS-RESPONSE               TO FILE-STATUS                25070000
250800     IF NOT SUCCESS                                               25080000
250900        MOVE 'P1510-1'              TO ERR-PARAGRAPH              25090000
251000        MOVE 'P931LINK'             TO ERR-KEY                    25100000
251100        PERFORM P9999-GOT-PROBLEM                                 25110000
251200     END-IF                                                       25120000
251300*    CNC0006 - FLW, 5/8/96, START                                 25130000
251400     MOVE P931-RET-CRAFT-STT            TO WS-VL1-STT-STARTS      25140000
251500     MOVE P931-RET-CRAFT-OVT            TO WS-VL1-OVT-STARTS.     25150000
251600*    CNC0006 - FLW, 5/8/96, END                                   25160000
251700*                                                                 25170000
251800*                                                                 25180000
251900 P1520-SET-SENIORITY-ARRAY.                                       25190000
252000*                                                                 25200000
252100     MOVE SPACES             TO WS-SENIORITY                      25210000
252200     MOVE EMP-NBR OF WS-MSTR TO SF-EMP-NO                         25220000
252300     MOVE SF-SENKEY2         TO SENKEY2                           25230000
252400     EXEC CICS STARTBR                                            25240000
252500               DATASET(SENFILE-VIA-EMP-NO)                        25250000
252600               RIDFLD(SENKEY2)                                    25260000
252700               GTEQ                                               25270000
252800               RESP(WS-RESPONSE)                                  25280000
252900     END-EXEC                                                     25290000
253000     MOVE WS-RESPONSE TO FILE-STATUS                              25300000
253100     IF SUCCESS                                                   25310000
253200        MOVE '0' TO SEN-DONE-CODE                                 25320000
253300        PERFORM UNTIL SEN-DONE                                    25330000
253400           EXEC CICS READNEXT                                     25340000
253500                     DATASET(SENFILE-VIA-EMP-NO)                  25350000
253600                     INTO (WS-SENIORITY)                          25360000
253700                     LENGTH(SENENBR-RLGTH)                        25370000
253800                     RIDFLD(SENKEY2)                              25380000
253900                     KEYLENGTH(SENENBR-KLGTH)                     25390000
254000                     RESP(WS-RESPONSE)                            25400000
254100           END-EXEC                                               25410000
254200           MOVE WS-RESPONSE TO FILE-STATUS                        25420000
254300           IF SUCCESS                                             25430000
254400              IF SF-EMP-NO = EMP-NBR OF WS-MSTR                   25440000
254500                 IF SF-CRAFT = 'EN'                               25450000
254600                    MOVE 'Y' TO WS-VL4-EN                         25460000
254700                 ELSE                                             25470000
254800                    IF SF-CRAFT = 'CO'                            25480000
254900                       MOVE 'Y' TO WS-VL4-CO                      25490000
255000                    ELSE                                          25500000
255100                       IF SF-CRAFT = 'FO'                         25510000
255200                          MOVE 'Y' TO WS-VL4-FO                   25520000
255300                       ELSE                                       25530000
255400                          IF SF-CRAFT = 'SW'                      25540000
255500                             MOVE 'Y' TO WS-VL4-SW                25550000
255600                          ELSE                                    25560000
255700                             IF SF-CRAFT = 'BK'                   25570000
255800                                MOVE 'Y' TO WS-VL4-BK             25580000
255900                             ELSE                                 25590000
256000                                IF SF-CRAFT = 'YA'                25600000
256100                                   MOVE 'Y' TO WS-VL4-YA          25610000
256200                                END-IF                            25620000
256300                             END-IF                               25630000
256400                          END-IF                                  25640000
256500                       END-IF                                     25650000
256600                    END-IF                                        25660000
256700                 END-IF                                           25670000
256800              ELSE                                                25680000
256900                 SET SEN-DONE TO TRUE                             25690000
257000              END-IF                                              25700000
257100           ELSE                                                   25710000
257200              SET SEN-DONE TO TRUE                                25720000
257300              IF NOT (NO-RECORD-FND OR END-OF-FILE)               25730000
257400                 MOVE 'P1520-1' TO ERR-PARAGRAPH                  25740000
257500                 MOVE SENKEY2   TO ERR-KEY                        25750000
257600                 PERFORM P9999-GOT-PROBLEM                        25760000
257700              END-IF                                              25770000
257800           END-IF                                                 25780000
257900        END-PERFORM                                               25790000
258000        EXEC CICS ENDBR                                           25800000
258100                  DATASET(SENFILE-VIA-EMP-NO)                     25810000
258200                  RESP(WS-RESPONSE)                               25820000
258300        END-EXEC                                                  25830000
258400     ELSE                                                         25840000
258500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     25850000
258600           MOVE 'P1520-2' TO ERR-PARAGRAPH                        25860000
258700           MOVE SENKEY2   TO ERR-KEY                              25870000
258800           PERFORM P9999-GOT-PROBLEM                              25880000
258900        END-IF                                                    25890000
259000     END-IF.                                                      25900000
259100*                                                                 25910000
259200 P1540-SET-QUAL-ARRAY.                                            25920000
259300*                                                                 25930000
259400     MOVE SPACES             TO WS-QUAL-FILE                      25940000
259500     MOVE EMP-NBR OF WS-MSTR TO QUAL-EMP-NO                       25950000
259600     MOVE QUALEMP-KEY        TO QUALEMP                           25960000
259700     EXEC CICS STARTBR                                            25970000
259800               DATASET(QUAL-FILE-VIA-QUALEMP)                     25980000
259900               RIDFLD(QUALEMP)                                    25990000
260000               GTEQ                                               26000000
260100               RESP(WS-RESPONSE)                                  26010000
260200     END-EXEC                                                     26020000
260300     MOVE WS-RESPONSE TO FILE-STATUS                              26030000
260400     IF SUCCESS                                                   26040000
260500        MOVE '0' TO QUAL-DONE-CODE                                26050000
260600        MOVE 1 TO QUAL-SUB                                        26060000
260700        PERFORM UNTIL QUAL-DONE OR QUAL-SUB > 5                   26070000
260800           EXEC CICS READNEXT                                     26080000
260900                     DATASET(QUAL-FILE-VIA-QUALEMP)               26090000
261000                     INTO (WS-QUAL-FILE)                          26100000
261100                     LENGTH(QUALEMP-RLGTH)                        26110000
261200                     RIDFLD(QUALEMP)                              26120000
261300                     KEYLENGTH(QUALEMP-KLGTH)                     26130000
261400                     RESP(WS-RESPONSE)                            26140000
261500           END-EXEC                                               26150000
261600           MOVE WS-RESPONSE TO FILE-STATUS                        26160000
261700           IF SUCCESS                                             26170000
261800              IF QUAL-EMP-NO = EMP-NBR OF WS-MSTR                 26180000
261900                 MOVE QUALIFICATION TO TEST-QUAL                  26190000
262000                 IF NOT STEP-RATE-QUAL                            26200000
262100                    MOVE SPACES         TO WS-CNTL-FILE           26210000
262200                    SET QUAL-CODE-TYPE-REC TO TRUE                26220000
262300                    MOVE QUALIFICATION  TO CNTL-QUAL-CODE         26230000
262400                    MOVE CNTLKEY-AREA   TO CNTLKEY                26240000
262500                    EXEC CICS READ                                26250000
262600                              DATASET(CNTL-FILE-VIA-CNTLKEY)      26260000
262700                              INTO(WS-CNTL-FILE)                  26270000
262800                              LENGTH(CNTLFILE-RLGTH)              26280000
262900                              RIDFLD(CNTLKEY)                     26290000
263000                              KEYLENGTH(CNTLFILE-KLGTH)           26300000
263100                              RESP(WS-RESPONSE)                   26310000
263200                    END-EXEC                                      26320000
263300                    MOVE WS-RESPONSE TO FILE-STATUS               26330000
263400                    IF SUCCESS AND CNTL-QUAL-DISP-SB-MAINT = 'Y'  26340000
263500                       MOVE QUALIFICATION TO WS-VL4-QUAL(QUAL-SUB)26350000
263600                       ADD 1 TO QUAL-SUB                          26360000
263700                    END-IF                                        26370000
263800                 END-IF                                           26380000
263900              ELSE                                                26390000
264000                 SET QUAL-DONE TO TRUE                            26400000
264100              END-IF                                              26410000
264200           ELSE                                                   26420000
264300              SET QUAL-DONE TO TRUE                               26430000
264400              IF NOT (NO-RECORD-FND OR END-OF-FILE)               26440000
264500                 MOVE 'P1540-1' TO ERR-PARAGRAPH                  26450000
264600                 MOVE QUALEMP   TO ERR-KEY                        26460000
264700                 PERFORM P9999-GOT-PROBLEM                        26470000
264800              END-IF                                              26480000
264900           END-IF                                                 26490000
265000        END-PERFORM                                               26500000
265100        EXEC CICS ENDBR                                           26510000
265200                  DATASET(QUAL-FILE-VIA-QUALEMP)                  26520000
265300                  RESP(WS-RESPONSE)                               26530000
265400        END-EXEC                                                  26540000
265500     ELSE                                                         26550000
265600        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     26560000
265700           MOVE 'P1540-2' TO ERR-PARAGRAPH                        26570000
265800           MOVE QUALEMP   TO ERR-KEY                              26580000
265900           PERFORM P9999-GOT-PROBLEM                              26590000
266000        END-IF                                                    26600000
266100     END-IF.                                                      26610000
266200*************************************************************     26620000
266300 P1550-CHECK-SCHEDULE.                                            26630000
266400*   ALTER PRINT LINES BASED ON SCHEDULED SPAREBOARD               26640000
266500*   INFORMATION FROM P912.                                        26650000
266600*                        NORTHERN QUEBEC SPAREBOARD - PHASE 2     26660000
266700*************************************************************     26670000
266800      IF WS-VL1-LO <= SPACES                                      26680000
266900         IF P912-SCHEDULED-REST-DAY                               26690000
267000            MOVE 'RD' TO WS-VL1-LO                                26700000
267100         END-IF                                                   26710000
267200         IF P912-SCHEDULED-REST-PERIOD                            26720000
267300            MOVE 'RP' TO WS-VL1-LO                                26730000
267400         END-IF                                                   26740000
267500      END-IF                                                      26750000
267600      IF P912-SCHEDULED-OFF-BOARD                                 26760000
267700         MOVE RED TO SCR06-NAME-COLOR(TURN-SUB)                   26770000
267800                     SCR06-TURN-COLOR(TURN-SUB)                   26780000
267900                     SCR06-VARIABLE-COLOR(TURN-SUB)               26790000
268000      END-IF                                                      26800000
268100      .                                                           26810000
268101*                                                                 26810102
268102 P1560-GET-REST-DAY-DESC.                                         26810202
268103*                                                                 26810302
268104*    CNPS42 RETURNS THE NEXT 7 DAYS WORTH OF REST DAYS, IF ANY,   26810402
268105*    STARTING WITH THE CURRENT DAY.  DETERMINE THE DAYS OF THE    26810502
268106*    WEEK FOR THE REST DAYS IN QUESTION.                          26810602
268107*                                                                 26810702
268110     IF WS-REST-SUB NOT = 1                                       26811003
268130        MOVE ZEROES                     TO DATE-CONVERSION-PARMS  26813002
268140        SET PARM-ADD                    TO TRUE                   26814002
268150        MOVE WS-LOCAL-DATE              TO PARM-PRI-DATE-GREG     26815003
268170        MOVE WS-LOCAL-TIME              TO PARM-PRI-TIME          26817003
268192        COMPUTE WS-NUM = WS-REST-SUB - 1                          26819203
268193        MOVE WS-NUM                     TO PARM-SEC-DATE-GREG     26819303
268194        EXEC CICS LINK                                            26819402
268195                  PROGRAM(P903-PGM)                               26819502
268196                  COMMAREA(DATE-CONVERSION-PARMS)                 26819602
268197                  LENGTH(P903-LGTH)                               26819702
268198                  RESP(WS-RESPONSE)                               26819802
268199        END-EXEC                                                  26819902
268200        MOVE WS-RESPONSE                TO FILE-STATUS            26820002
268201        IF NOT SUCCESS                                            26820102
268202           MOVE 'P1560-1'               TO ERR-PARAGRAPH          26820202
268203           PERFORM P9999-GOT-PROBLEM                              26820302
268204        END-IF                                                    26820402
268205        MOVE PARM-RES-DATE-GREG         TO HOLD-RES-DATE-GREG     26820502
268206     ELSE                                                         26820602
268207        MOVE WS-LOCAL-DATE              TO HOLD-RES-DATE-GREG     26820703
268208     END-IF                                                       26820802
268209                                                                  26820902
268210     MOVE ZEROS                         TO DATE-CONVERSION-PARMS  26821002
268211     SET PARM-CONV                      TO TRUE                   26821102
268212     MOVE HOLD-RES-DATE-GREG            TO PARM-PRI-DATE-GREG     26821202
268213     EXEC CICS LINK                                               26821302
268214               PROGRAM(P903-PGM)                                  26821402
268215               COMMAREA(DATE-CONVERSION-PARMS)                    26821502
268216               LENGTH(P903-LGTH)                                  26821602
268217               RESP(WS-RESPONSE)                                  26821702
268218     END-EXEC                                                     26821802
268219     MOVE WS-RESPONSE                   TO FILE-STATUS            26821902
268220     IF NOT SUCCESS                                               26822002
268221        MOVE 'P1560-2'                  TO ERR-PARAGRAPH          26822102
268222        PERFORM P9999-GOT-PROBLEM                                 26822202
268223     END-IF                                                       26822302
268226     MOVE DAYOFWK-2CHAR(PARM-PRI-DAY-OF-WEEK, PSTCA-SUB)          26822602
268227                                  TO WS-VL2-REST-DAY(WS-REST-SUB) 26822702
268228     .                                                            26822802
268230*                                                                 26823002
268300 P3000-ADD-XB.                                                    26830000
268400*                                                                 26840000
268500     PERFORM VARYING FUNC-SUB FROM 1 BY 1                         26850000
268600        UNTIL FUNC-SUB > ARRAY-MAX                                26860000
268700              OR ERRORS-FOUND                                     26870000
268800        MOVE SPACES TO NORMAL-ASGNMT                              26880000
268900                       TEMPORARY-ASGNMT                           26890000
269000                       ON-DUTY-ASGNMT                             26900000
269100                       WS-MSTR                                    26910000
269200        MOVE '0'    TO GOT-EMPLOYEE-FLAG                          26920000
269300        MOVE FUNC-SUB TO NAME-SUB                                 26930000
269400        IF SCR06-FUNC-CODE(FUNC-SUB) = ('Y' OR 'N' OR 'X' OR 'O') 26940000
269500           SET FUNCTION-FOUND TO TRUE                             26950000
269600           MOVE SPACES TO SCR06-VARIABLE(NAME-SUB)                26960000
269700           PERFORM P3010-EDIT-TURN                                26970000
269800           IF NOT ERRORS-FOUND                                    26980000
269900              IF SCR06-NAME(NAME-SUB) > SPACES                    26990000
270000                 AND (SCR06-NAME(NAME-SUB) NOT = '   OPEN TURN'   27000000
270100                      AND '   TOUR VACANT')                       27010000
270200                 AND NOT SLOT-BOARD                               27020000
270300                 MOVE SCR06-NAME(NAME-SUB) TO EMP-NAME OF WS-MSTR 27030000
270400                 MOVE P06CA-EMP-NO(NAME-SUB) TO EMP-NBR OF WS-MSTR27040000
270500                 MOVE EMP-NAME-NBR-KEY TO MSTREMPK                27050000
270600                 EXEC CICS READ                                   27060000
270700                           DATASET(MSTR-VIA-EMP-NAME)             27070000
270800                           INTO(WS-MSTR)                          27080000
270900                           LENGTH(MSTRENAM-RLGTH)                 27090000
271000                           RIDFLD(MSTREMPK)                       27100000
271100                           KEYLENGTH(MSTRENAM-KLGTH)              27110000
271200                           RESP(WS-RESPONSE)                      27120000
271300                 END-EXEC                                         27130000
271400                 MOVE WS-RESPONSE TO FILE-STATUS                  27140000
271500                 IF SUCCESS                                       27150000
271600                    IF SCR06-FUNC-CODE(NAME-SUB) = 'N'            27160000
271700                       PERFORM P3020-GET-NEXT-EMPLOYEE            27170000
271800                    ELSE                                          27180000
271900                       SET GOT-EMPLOYEE TO TRUE                   27190000
272000                    END-IF                                        27200000
272100                 ELSE                                             27210000
272200                    IF (NO-RECORD-FND OR END-OF-FILE)             27220000
272300                       MOVE SPACES TO MSTREMPK                    27230000
272400                                      P06CA-EMP-NO(NAME-SUB)      27240000
272500                       MOVE SCR06-NAME(NAME-SUB) TO MSTREMPK      27250000
272600                       PERFORM P3020-GET-NEXT-EMPLOYEE            27260000
272700                    ELSE                                          27270000
272800                       MOVE 'P3000-1'  TO ERR-PARAGRAPH           27280000
272900                       MOVE MSTREMPK   TO ERR-KEY                 27290000
273000                       PERFORM P9999-GOT-PROBLEM                  27300000
273100                    END-IF                                        27310000
273200                 END-IF                                           27320000
273300                 IF GOT-EMPLOYEE                                  27330000
273400                    MOVE EMP-NAME OF WS-MSTR                      27340000
273500                       TO SCR06-NAME(NAME-SUB)                    27350000
273600                    MOVE EMP-NBR OF WS-MSTR                       27360000
273700                       TO P06CA-EMP-NO(NAME-SUB)                  27370000
273800                    PERFORM P3030-EDIT-EMPLOYEE                   27380000
273900                    IF NOT ERRORS-FOUND                           27390000
274000                       IF SCR06-FUNC-CODE(NAME-SUB)               27400000
274100                          NOT = ('Y' AND 'O')                     27410000
274200                          SET ERRORS-FOUND TO TRUE                27420000
274300*                              CONFIRM-MSG                        27430000
274400                          MOVE 'P020' TO MSGLOG-CODE              27440000
274500                          MOVE -1                                 27450000
274600                             TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)  27460000
274700                       END-IF                                     27470000
274800                    END-IF                                        27480000
274900                 ELSE                                             27490000
275000                    SET ERRORS-FOUND TO TRUE                      27500000
275100                    MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)   27510000
275200*                          'NO MORE EMPLOYEE RECORDS FOUND '      27520000
275300*                          'IN ' SCR06-DIST '/' SCR06-SUB-DIST    27530000
275400                    MOVE 'N011' TO MSGLOG-CODE                    27540000
275500                 END-IF                                           27550000
275600              ELSE                                                27560000
275700                 MOVE SPACES TO P06CA-EMP-NO(NAME-SUB)            27570000
275800              END-IF                                              27580000
275900           END-IF                                                 27590000
276000                                                                  27600000
276100           IF NOT ERRORS-FOUND                                    27610000
276200              COMPUTE WS-NBR-ASSIGNED = P06CA-NBR-ASSIGNED-NUM + 127620000
276300              IF WS-NBR-ASSIGNED > P06CA-NBR-ELIGIBLE             27630000
276400                 SET ERRORS-FOUND TO TRUE                         27640000
276500                 MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)      27650000
276600                 STRING 'ADD ERROR, EXCEEDED ELIGIBLE POSITIONS ' 27660000
276700                        'ON EXTRABOARD'                           27670000
276800                         DELIMITED BY SIZE                        27680000
276900                         INTO SCR06-ERRORMSG                      27690000
277000              ELSE                                                27700000
277100                 MOVE WS-NBR-ASSIGNED TO P06CA-NBR-ASSIGNED-NUM   27710000
277200                                         SCR06-NBR-ASSIGNED       27720000
277300              END-IF                                              27730000
277400           END-IF                                                 27740000
277500                                                                  27750000
277600           IF NOT ERRORS-FOUND                                    27760000
277700              PERFORM P3040-PROCESS-ADD                           27770000
277800           END-IF                                                 27780000
277900        ELSE                                                      27790000
278000           IF SCR06-FUNC-CODE(NAME-SUB) > SPACE                   27800000
278100              SET ERRORS-FOUND TO TRUE                            27810000
278200              MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)         27820000
278300              MOVE REV-VIDEO TO SCR06-FUNC-CODE-HI(NAME-SUB)      27830000
278400*                  INVALID-CODE-MSG                               27840000
278500              MOVE 'I041' TO MSGLOG-CODE                          27850000
278600           END-IF                                                 27860000
278700        END-IF                                                    27870000
278800     END-PERFORM                                                  27880000
278900     IF NOT FUNCTION-FOUND                                        27890000
279000        SET ERRORS-FOUND TO TRUE                                  27900000
279100*            'PLACE AN "X" NEXT TO THE POSITION TO BE UPDATED'    27910000
279200        MOVE 'P021' TO MSGLOG-CODE                                27920000
279300        MOVE -1 TO SCR06-FUNC-CODE-CURSOR(1)                      27930000
279400     END-IF                                                       27940000
279500     IF NOT ERRORS-FOUND                                          27950000
279600        PERFORM P1000-LIST-XB                                     27960000
279700     END-IF.                                                      27970000
279800*                                                                 27980000
279900 P3010-EDIT-TURN.                                                 27990000
280000*                                                                 28000000
280100     MOVE '0' TO GOT-TURN-FLAG                                    28010000
280200     IF SCR06-TURN(NAME-SUB) NOT > SPACES                         28020000
280300        SET ERRORS-FOUND TO TRUE                                  28030000
280400        MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)                    28040000
280500        MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)                 28050000
280600*            'MUST ENTER A TURN NBR'                              28060000
280700     MOVE 'M023' TO MSGLOG-CODE                                   28070000
280800     END-IF.                                                      28080000
280900     MOVE SCR06-DIST TO DIST OF EBTURN                            28090000
281000     MOVE SCR06-SUB-DIST TO SUBDIST OF EBTURN                     28100000
281100     MOVE SCR06-CC TO CRAFT-CODE OF EBTURN                        28110000
281200     MOVE SCR06-TURN(NAME-SUB) TO EB-TURN-NBR OF EBTURN           28120000
281300     EXEC CICS READ                                               28130000
281400               DATASET(EB-VIA-TURN-NBR)                           28140000
281500               INTO(WS-EXTRA-BOARD)                               28150000
281600               LENGTH(EBTURNNO-RLGTH)                             28160000
281700               RIDFLD(EBTURN)                                     28170000
281800               KEYLENGTH(EBTURNNO-KLGTH)                          28180000
281900               RESP(WS-RESPONSE)                                  28190000
282000     END-EXEC                                                     28200000
282100     MOVE WS-RESPONSE TO FILE-STATUS                              28210000
282200     IF SUCCESS                                                   28220000
282300        SET GOT-TURN TO TRUE                                      28230000
282400        MOVE 'X' TO WK-ASGN-JOB-TYPE                              28240000
282500        MOVE SCR06-DIST TO WK-ASGN-DIST                           28250000
282600        MOVE SCR06-SUB-DIST TO WK-ASGN-SUB-DIST                   28260000
282700        MOVE 'EX      ' TO WK-SWASSGN-ASGN                        28270000
282800        MOVE SCR06-TURN(NAME-SUB) TO WK-ASGN-XB-TURN              28280000
282900        MOVE SCR06-CC TO WK-ASGN-CC                               28290000
283000        PERFORM PXXXX-JOB-OWNER                                   28300000
283100        IF ASGN-EMP-NO NOT > ZERO                                 28310000
283200           PERFORM PXXXX-LATEST-TEMP                              28320000
283300        END-IF                                                    28330000
283400        IF ASGN-EMP-NO > ZEROS                                    28340000
283500           SET ERRORS-FOUND TO TRUE                               28350000
283600           MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)                 28360000
283700           MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)              28370000
283800*               'TURN ALREADY ASSIGNED'                           28380000
283900           MOVE 'T007' TO MSGLOG-CODE                             28390000
284000        ELSE                                                      28400000
284100           IF (SCR06-NAME(NAME-SUB) = '   OPEN TURN'              28410000
284200               OR '   TOUR VACANT')                               28420000
284300              OR SCR06-NAME(NAME-SUB) NOT > SPACE                 28430000
284400              SET ERRORS-FOUND TO TRUE                            28440000
284500              MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)              28450000
284600              MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)           28460000
284700*                  'DUPLICATE TURN NBR'                           28470000
284800              MOVE 'D009' TO MSGLOG-CODE                          28480000
284900           END-IF                                                 28490000
285000        END-IF                                                    28500000
285100     ELSE                                                         28510000
285200        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     28520000
285300           MOVE 'P3010-2'   TO ERR-PARAGRAPH                      28530000
285400           MOVE EBTURN      TO ERR-KEY                            28540000
285500           PERFORM P9999-GOT-PROBLEM                              28550000
285600        END-IF                                                    28560000
285700     END-IF.                                                      28570000
285800*                                                                 28580000
285900 P3020-GET-NEXT-EMPLOYEE.                                         28590000
286000*                                                                 28600000
286100     EXEC CICS STARTBR                                            28610000
286200               DATASET(MSTR-VIA-EMP-NAME)                         28620000
286300               RIDFLD(MSTREMPK)                                   28630000
286400               GTEQ                                               28640000
286500               RESP(WS-RESPONSE)                                  28650000
286600     END-EXEC                                                     28660000
286700     MOVE WS-RESPONSE TO FILE-STATUS                              28670000
286800     IF SUCCESS                                                   28680000
286900        MOVE '0' TO DONE-CODE                                     28690000
287000        PERFORM UNTIL DONE                                        28700000
287100           EXEC CICS READNEXT                                     28710000
287200                     DATASET(MSTR-VIA-EMP-NAME)                   28720000
287300                     INTO (WS-MSTR)                               28730000
287400                     LENGTH(MSTRENAM-RLGTH)                       28740000
287500                     RIDFLD(MSTREMPK)                             28750000
287600                     KEYLENGTH(MSTRENAM-KLGTH)                    28760000
287700                     RESP(WS-RESPONSE)                            28770000
287800           END-EXEC                                               28780000
287900           MOVE WS-RESPONSE TO FILE-STATUS                        28790000
288000           IF SUCCESS                                             28800000
288100              IF DIST OF WS-MSTR = SCR06-DIST                     28810000
288200                 AND SUB-DIST OF WS-MSTR = SCR06-SUB-DIST         28820000
288300                 AND EMP-NBR OF WS-MSTR                           28830000
288400                     NOT = P06CA-EMP-NO(NAME-SUB)                 28840000
288500                 SET GOT-EMPLOYEE TO TRUE                         28850000
288600                 SET DONE         TO TRUE                         28860000
288700                 MOVE EMP-NBR OF WS-MSTR TO P06CA-EMP-NO(NAME-SUB)28870000
288800              END-IF                                              28880000
288900           ELSE                                                   28890000
289000              SET DONE TO TRUE                                    28900000
289100              IF NOT (NO-RECORD-FND OR END-OF-FILE)               28910000
289200                 MOVE 'P3020-1'   TO ERR-PARAGRAPH                28920000
289300                 MOVE MSTREMPK  TO ERR-KEY                        28930000
289400                 PERFORM P9999-GOT-PROBLEM                        28940000
289500              END-IF                                              28950000
289600           END-IF                                                 28960000
289700        END-PERFORM                                               28970000
289800        EXEC CICS ENDBR                                           28980000
289900                  DATASET(MSTR-VIA-EMP-NAME)                      28990000
290000                  RESP(WS-RESPONSE)                               29000000
290100        END-EXEC                                                  29010000
290200     ELSE                                                         29020000
290300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     29030000
290400           MOVE 'P3020-2'   TO ERR-PARAGRAPH                      29040000
290500           MOVE MSTREMPK  TO ERR-KEY                              29050000
290600           PERFORM P9999-GOT-PROBLEM                              29060000
290700        END-IF                                                    29070000
290800     END-IF.                                                      29080000
290900*                                                                 29090000
291000 P3030-EDIT-EMPLOYEE.                                             29100000
291100*                                                                 29110000
291200*                                                                 29120000
291300*    SEE IF EMPLOYEE HAS THE NECESSARY SENIORITY                  29130000
291400*    FOR THIS EXTRABOARD.                                         29140000
291500*                                                                 29150000
291600     IF NOT ERRORS-FOUND                                          29160000
291700        PERFORM P3050-SET-UP-P942LINK                             29170000
291800        IF P942-EMP-SEN-DATE > ZEROES                             29180000
291900           MOVE SF-SENIORITY-DATE TO WS-EMP-ADDED-SEN             29190000
292000        ELSE                                                      29200000
292100           SET ERRORS-FOUND TO TRUE                               29210000
292200           MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)            29220000
292300           MOVE REV-VIDEO                                         29230000
292400              TO SCR06-NAME-HI(NAME-SUB)                          29240000
292500*             'EMPLOYEE NOT QUALIFIED FOR THIS SPAREBOARD'        29250000
292600              MOVE 'E027' TO MSGLOG-CODE                          29260000
292700        END-IF                                                    29270000
292800     END-IF                                                       29280000
292900*                                                                 29290000
293000*    SEE IF EMPLOYEE HAS A NORMAL ASSIGNMENT                      29300000
293100*    IF SO HE MUST BE REMOVED FROM THAT ASSIGNMENT                29310000
293200*    BEFORE HE CAN BE ADDED TO THE EXTRABOARD                     29320000
293300*                                                                 29330000
293400     IF NOT ERRORS-FOUND                                          29340000
293500        PERFORM P8510-READ-MASTER-JOBS                            29350000
293600        IF NORMAL-ASGNMT > SPACES                                 29360000
293700           SET ERRORS-FOUND TO TRUE                               29370000
293800           MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)            29380000
293900           MOVE REV-VIDEO TO SCR06-NAME-HI(NAME-SUB)              29390000
294000*               'EMPLOYEE ALREADY HAS A REGULAR ASSIGNMENT'       29400000
294100           MOVE 'E049' TO MSGLOG-CODE                             29410000
294200        END-IF                                                    29420000
294300     END-IF                                                       29430000
294400*                                                                 29440000
294500*    SEE IF EMPLOYEE IS PENDING NOTIFICATION                      29450000
294600*    IF SO HE MUST BE NOTIFIED BEFORE HE CAN BE ADDED             29460000
294700*    TO THE EXTRABOARD                                            29470000
294800*                                                                 29480000
294900     IF NOT ERRORS-FOUND                                          29490000
295000        MOVE SPACES                TO TASK-EMPLOYEE-KEY           29500000
295100        MOVE EMP-NBR OF WS-MSTR    TO EMP-NBR OF WS-TASK          29510000
295200        SET TASK-NOTIFY OF WS-TASK TO TRUE                        29520000
295300        MOVE TASK-EMPLOYEE-KEY     TO TASKEMPK                    29530000
295400        EXEC CICS READ                                            29540000
295500                  DATASET(TASK-VIA-EMP-NBR)                       29550000
295600                  INTO(WS-TASK)                                   29560000
295700                  LENGTH(TASKENBR-RLGTH)                          29570000
295800                  RIDFLD(TASKEMPK)                                29580000
295900                  KEYLENGTH(TASKENBR-KLGTH)                       29590000
296000                  RESP(WS-RESPONSE)                               29600000
296100        END-EXEC                                                  29610000
296200        MOVE WS-RESPONSE TO FILE-STATUS                           29620000
296300        IF SUCCESS                                                29630000
296400           SET ERRORS-FOUND TO TRUE                               29640000
296500           MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)            29650000
296600           MOVE REV-VIDEO TO SCR06-NAME-HI(NAME-SUB)              29660000
296700*                 'EMPLOYEE HAS NOTIFICATION PENDING,'            29670000
296800*                 ' NOTIFY FIRST, THEN ADD TO SPAREBOARD'         29680000
296900           MOVE 'E050' TO MSGLOG-CODE                             29690000
297000        ELSE                                                      29700000
297100           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  29710000
297200              MOVE 'P3030-2'   TO ERR-PARAGRAPH                   29720000
297300              MOVE TASKEMPK  TO ERR-KEY                           29730000
297400              PERFORM P9999-GOT-PROBLEM                           29740000
297500           END-IF                                                 29750000
297600        END-IF                                                    29760000
297700     END-IF                                                       29770000
297800*                                                                 29780000
297900*    SEE IF EMPLOYEE IS WAITING TURN OR PENDING WAITING TURN.     29790000
298000*    IF SO HE MUST BE MARKED UP BEFORE HE CAN BE ADDED TO THE     29800000
298100*    EXTRABOARD.                                                  29810000
298200*                                                                 29820000
298300     IF NOT ERRORS-FOUND                                          29830000
298400        IF WAIT-TURN OR MSTR-PENDED-WAIT-TURN                     29840000
298500           SET ERRORS-FOUND  TO TRUE                              29850000
298600           MOVE -1           TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)  29860000
298700           MOVE REV-VIDEO    TO SCR06-NAME-HI(NAME-SUB)           29870000
298800*                 'EMPLOYEE IS WAITING TURN, MARK UP FIRST,       29880000
298900*                 ' THEN ADD TO SPAREBOARD'                       29890000
299000           MOVE 'E177'       TO MSGLOG-CODE                       29900000
299100        END-IF                                                    29910000
299200     END-IF                                                       29920000
299300*                                                                 29930000
299400*    SEE IF EMPLOYEE IS RESTRICTED FROM OWNING THIS EXTRABOARD    29940000
299500*    POSITION.                                                    29950000
299600*                                                                 29960000
299700     IF NOT ERRORS-FOUND                                          29970000
299800        MOVE SPACES                  TO P947-COMMAREA-PARMS       29980000
299900        SET P947-EMP-RESTR-FUNCTION  TO TRUE                      29990000
300000        MOVE EMP-NBR OF WS-MSTR      TO P947-EMP-NO               30000000
300100        SET P947-ASGN-XB             TO TRUE                      30010000
300200        MOVE SCR06-DIST              TO P947-ASGN-DIST            30020000
300300        MOVE SCR06-SUB-DIST          TO P947-ASGN-SUB-DIST        30030000
300400        MOVE 'EX'                    TO WK-JOB-CODE-POS12         30040000
300500        MOVE SCR06-TURN(NAME-SUB)    TO WK-JOB-CODE-POS3456       30050000
300600        MOVE SCR06-CC                TO WK-JOB-CODE-POS78         30060000
300700        MOVE WORK-JOB-CODE           TO P947-ASGN-ASSIGNMENT      30070000
300800        SET P947-FROM-OWN            TO TRUE                      30080000
300900        EXEC CICS LINK                                            30090000
301000                  PROGRAM(P947-PGM)                               30100000
301100                  COMMAREA(P947-COMMAREA-PARMS)                   30110000
301200                  LENGTH(P947-LGTH)                               30120000
301300                  RESP(WS-RESPONSE)                               30130000
301400        END-EXEC                                                  30140000
301500        MOVE WS-RESPONSE            TO FILE-STATUS                30150000
301600        IF NOT SUCCESS                                            30160000
301700           MOVE 'P3030-3'           TO ERR-PARAGRAPH              30170000
301800           MOVE 'LINK947'           TO ERR-KEY                    30180000
301900           PERFORM P9999-GOT-PROBLEM                              30190000
302000        END-IF                                                    30200000
302100        IF P947-RESTRICTED                                        30210000
302200           SET ERRORS-FOUND   TO TRUE                             30220000
302300           MOVE -1            TO SCR06-FUNC-CODE-CURSOR(NAME-SUB) 30230000
302400           MOVE REV-VIDEO     TO SCR06-NAME-HI(NAME-SUB)          30240000
302500*               'EMPLOYEE NOT QUALIFIED - HAS RESTRICTION'        30250000
302600           MOVE 'E186'        TO MSGLOG-CODE                      30260000
302700        END-IF                                                    30270000
302800     END-IF.                                                      30280000
302900                                                                  30290000
303000 P3040-PROCESS-ADD.                                               30300000
303100*                                                                 30310000
303200     MOVE SPACES TO P914-COMMAREA-PARMS                           30320000
303300     SET P914-ADD-FUNCTION TO TRUE                                30330000
303400     MOVE SCR06-DIST                TO P914-TURN-DIST             30340000
303500     MOVE SCR06-SUB-DIST            TO P914-TURN-SUB-DIST         30350000
303600     MOVE SCR06-CC                  TO P914-TURN-CC               30360000
303700     MOVE SCR06-TURN(NAME-SUB)      TO P914-TURN                  30370000
303800     IF SCR06-FUNC-CODE(NAME-SUB) = 'O'                           30380000
303900        AND LAST-ARRIVAL-TIME IS NUMERIC                          30390000
304000        AND LAST-ARRIVAL-TIME > '0000000000'                      30400000
304100        MOVE LAST-ARRIVAL-TIME      TO P914-EFF-DATE-TIME         30410000
304200     ELSE                                                         30420000
304300        MOVE WS-LOCAL-DATE-TIME     TO P914-EFF-DATE-TIME         30430000
304400     END-IF                                                       30440000
304500     MOVE PSTCA-TIME-ZONE           TO P914-TIME-ZONE             30450000
304600*                                                                 30460000
304700*   FOR AN ADD TO THE EXTRABOARD, ONLY SET THE FLAG FOR           30470000
304800*   A DUAL BOARD.  FOR EXTRABOARDS, THE SUBROUTINE WILL           30480000
304900*   UPDATE BOTH SIDES OF THE EXTRABOARD.                          30490000
305000*                                                          **PLS  30500000
305100     IF DUAL-XB AND                                               30510000
305200        SCR06-YARD-ROAD > SPACE                                   30520000
305300        MOVE SCR06-YARD-ROAD        TO P914-YARD-ROAD             30530000
305400     END-IF                                                       30540000
305500     IF GOT-EMPLOYEE                                              30550000
305600        MOVE P06CA-EMP-NO(NAME-SUB) TO P914-EMP-NO                30560000
305700     END-IF                                                       30570000
305800     EXEC CICS LINK                                               30580000
305900               PROGRAM(P914-PGM)                                  30590000
306000               COMMAREA(P914-COMMAREA-PARMS)                      30600000
306100               LENGTH(P914-LGTH)                                  30610000
306200               RESP(WS-RESPONSE)                                  30620000
306300     END-EXEC                                                     30630000
306400     MOVE WS-RESPONSE TO FILE-STATUS                              30640000
306500     IF NOT SUCCESS                                               30650000
306600        MOVE 'P3040-1' TO ERR-PARAGRAPH                           30660000
306700        PERFORM P9999-GOT-PROBLEM                                 30670000
306800     END-IF                                                       30680000
306900     MOVE ' ' TO SCR06-FUNC-CODE(NAME-SUB).                       30690000
307000*                                                                 30700000
307100 P3050-SET-UP-P942LINK.                                           30710000
307200*                                                                 30720000
307300     MOVE SPACES                TO P942-COMMAREA-PARMS            30730000
307400     SET P942-EMP-SEN-FUNCTION  TO TRUE                           30740000
307500     SET P942-ASGN-XB           TO TRUE                           30750000
307600     MOVE EMP-NBR IN WS-MSTR    TO P942-EMP-NO                    30760000
307700     MOVE SCR06-DIST            TO P942-ASGN-DIST                 30770000
307800     MOVE SCR06-SUB-DIST        TO P942-ASGN-SUB-DIST             30780000
307900     MOVE 'EX'                  TO P942-ASGN-POOL                 30790000
308000     MOVE SCR06-TURN(NAME-SUB)  TO P942-ASGN-TURN                 30800000
308100     MOVE SCR06-CC              TO P942-ASGN-CC                   30810000
308200     PERFORM P8080-LINK-942.                                      30820000
308300*                                                                 30830000
308400 P4000-REPOSITION.                                                30840000
308500*                                                                 30850000
308600     MOVE SPACES TO P943-COMMAREA-PARMS                           30860000
308700     SET P943-REPOSITION-FUN TO TRUE                              30870000
308800*                                                                 30880000
308900**** PERFORM P8800-GET-CURRENT-TIME                               30890000
309000*                                                                 30900000
309100     PERFORM VARYING FUNC-SUB FROM 1 BY 1                         30910000
309200        UNTIL FUNC-SUB > ARRAY-MAX                                30920000
309300        IF SCR06-FUNC-CODE(FUNC-SUB) = 'X'                        30930000
309400           SET FUNCTION-FOUND TO TRUE                             30940000
309500           MOVE FUNC-SUB TO NAME-SUB                              30950000
309600           ADD ARRAY-MAX TO FUNC-SUB                              30960000
309700        END-IF                                                    30970000
309800     END-PERFORM                                                  30980000
309900     IF FUNCTION-FOUND                                            30990000
310000        IF P06CA-TURN-KEY(NAME-SUB) NOT > SPACES                  31000000
310100           SET ERRORS-FOUND TO TRUE                               31010000
310200           MOVE -1          TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)   31020000
310300*               'NO RECORD AVAILABLE FOR REPOSITION'              31030000
310400           MOVE 'N010' TO MSGLOG-CODE                             31040000
310500        ELSE                                                      31050000
310600           PERFORM P4100-EDIT-EFFECTIVE-TIME                      31060000
310700           IF NOT ERRORS-FOUND                                    31070000
310800              PERFORM P4200-PROCESS-REPOSITION                    31080000
310900           END-IF                                                 31090000
311000        END-IF                                                    31100000
311100     ELSE                                                         31110000
311200        SET ERRORS-FOUND TO TRUE                                  31120000
311300        MOVE -1  TO SCR06-FUNC-CODE-CURSOR(01)                    31130000
311400*            'NO TURN MARKED FOR REPOSITION'                      31140000
311500        MOVE 'N008' TO MSGLOG-CODE                                31150000
311600     END-IF                                                       31160000
311700     IF NOT ERRORS-FOUND                                          31170000
311800        PERFORM P1000-LIST-XB                                     31180000
311900     END-IF.                                                      31190000
312000*                                                                 31200000
312100 P4100-EDIT-EFFECTIVE-TIME.                                       31210000
312200*                                                                 31220000
312300     MOVE SCR06-VARIABLE(NAME-SUB) TO WS-VARIABLE-LINE-3          31230000
312400     SET DE-YYMMDD-FORMAT          TO TRUE                        31240000
312500     MOVE WS-VL3-DATE              TO DE-YYMMDD                   31250000
312600     PERFORM P8998-DATEEDIT                                       31260000
312700     IF DE-INVALID-DATE OR                                        31270000
312800        WS-VL3-TIME NOT NUMERIC OR                                31280000
312900        WS-VL3-HR > '23' OR                                       31290000
313000        WS-VL3-MN > '59'                                          31300000
313100         SET ERRORS-FOUND TO TRUE                                 31310000
313200         MOVE -1        TO SCR06-VARIABLE-CURSOR(NAME-SUB)        31320000
313300         MOVE REV-VIDEO TO SCR06-VARIABLE-HI(NAME-SUB)            31330000
313400*             INVALID-DATE-TIME-MSG                               31340000
313500         MOVE 'I042' TO MSGLOG-CODE                               31350000
313600     ELSE                                                         31360000
313700        MOVE WS-VL3-DATE-TIME   TO WS-VL3-LOCAL-DATE-TIME         31370000
313800*                                                                 31380000
313900*       CONVERT LOCAL TIME TO SYSTEM TIME                         31390000
314000*                                                                 31400000
314100        MOVE SPACES             TO TZ-PARAMETERS                  31410000
314200        MOVE PSTCA-TIME-ZONE    TO TZ-IN-ZONE                     31420000
314300        MOVE WS-VL3-DATE-TIME   TO TZ-IN-DATE-TIME                31430000
314400        SET TZ-OUT-EASTERN-ZONE TO TRUE                           31440000
314500        PERFORM P8996-TIMEZONE                                    31450000
314600        MOVE TZ-OUT-DATE-TIME  TO WS-VL3-DATE-TIME                31460000
314700*                                                                 31470000
314800        SET DE-YYMMDD-FORMAT        TO TRUE                       31480000
314900        MOVE WS-VL3-DATE-TIME(1:6)  TO DE-YYMMDD                  31490000
315000        PERFORM P8998-DATEEDIT                                    31500000
315100        MOVE DE-CCYYMMDD            TO DE-COMPARE1-DATE           31510000
315200        MOVE WS-VL3-DATE-TIME(7:4)  TO DE-COMPARE1-TIME           31520000
315300*       IF WS-VL3-DATE-TIME > PRESENT-TIME                        31530000
315400        IF DE-COMPARE1-DATE-TIME > WS-PRESENT-TIME-CENT           31540000
315500           SET ERRORS-FOUND TO TRUE                               31550000
315600           MOVE -1        TO SCR06-VARIABLE-CURSOR(NAME-SUB)      31560000
315700           MOVE REV-VIDEO TO SCR06-VARIABLE-HI(NAME-SUB)          31570000
315800*               'CANNOT REPOSITION TO FUTURE DATE'                31580000
315900           MOVE 'C013' TO MSGLOG-CODE                             31590000
316000        ELSE                                                      31600000
316100           IF WS-VL3-TIE-CODE NOT NUMERIC                         31610000
316200              OR WS-VL3-TIE-CODE > '9000'                         31620000
316300              SET ERRORS-FOUND TO TRUE                            31630000
316400              MOVE -1        TO SCR06-VARIABLE-CURSOR(NAME-SUB)   31640000
316500              MOVE REV-VIDEO TO SCR06-VARIABLE-HI(NAME-SUB)       31650000
316600*                  'INVALID TIE CODE'                             31660000
316700              MOVE 'I031' TO MSGLOG-CODE                          31670000
316800           END-IF                                                 31680000
316900        END-IF                                                    31690000
317000     END-IF.                                                      31700000
317100*                                                                 31710000
317200 P4200-PROCESS-REPOSITION.                                        31720000
317300*                                                                 31730000
317400     MOVE SCR06-DIST       TO XB-WORK-DIST                        31740000
317500     MOVE SCR06-SUB-DIST   TO XB-WORK-SUB-DIST                    31750000
317600     MOVE SCR06-CC         TO XB-WORK-CC                          31760000
317700     MOVE ZERO             TO XB-WK-KEY1-ON-OFF                   31770000
317800                              XB-WK-KEY1-BOARD                    31780000
317900*                                                                 31790000
318000*   ONLY USE THE SCREEN INDICATOR TO SET THE BOARD FLAG ON        31800000
318100*   A DUAL BOARD.                                                 31810000
318200*                                                          **PLS  31820000
318300     IF DUAL-XB                                                   31830000
318400        IF SCR06-YARD-ROAD = 'Y'                                  31840000
318500           MOVE '1'           TO XB-WK-KEY1-BOARD                 31850000
318600        END-IF                                                    31860000
318700        IF SCR06-YARD-ROAD = 'R'                                  31870000
318800           MOVE '2'           TO XB-WK-KEY1-BOARD                 31880000
318900        END-IF                                                    31890000
319000     END-IF                                                       31900000
319100     MOVE WS-VL3-DATE-TIME TO XB-WK-KEY1-DATE-TIME                31910000
319200     MOVE WS-VL3-TIE-CODE  TO XB-WK-KEY1-TIE-CODE                 31920000
319300*                                                                 31930000
319400*   IF REPOSITIONING THE SLOW SIDE OF A FASTSLOW BOARD,           31940000
319500*   VALIDATE THE SLOW POSITION KEY.  OTHERWISE, VALIDATE          31950000
319600*   THE NORMAL POSITION KEY.                                      31960000
319700*                                                          **PLS  31970000
319800     IF WS-FASTSLOW-XB                                            31980000
319900        AND SCR06-YARD-ROAD = 'R'                                 31990000
320000        MOVE XB-WORK-KEY1     TO EBSLOW-POS                       32000000
320100        EXEC CICS READ                                            32010000
320200                  DATASET(EB-VIA-SLOW-POSITION)                   32020000
320300                  INTO(WS-EXTRA-BOARD)                            32030000
320400                  LENGTH(EBSLPOS-RLGTH)                           32040000
320500                  RIDFLD(EBSLOW-POS)                              32050000
320600                  KEYLENGTH(EBSLPOS-KLGTH)                        32060000
320700                  RESP(WS-RESPONSE)                               32070000
320800        END-EXEC                                                  32080000
320900     ELSE                                                         32090000
321000        MOVE XB-WORK-KEY1     TO EBPOS                            32100000
321100        EXEC CICS READ                                            32110000
321200                  DATASET(EB-VIA-CRAFT-POSITION)                  32120000
321300                  INTO(WS-EXTRA-BOARD)                            32130000
321400                  LENGTH(EBCRPOS-RLGTH)                           32140000
321500                  RIDFLD(EBPOS)                                   32150000
321600                  KEYLENGTH(EBCRPOS-KLGTH)                        32160000
321700                  RESP(WS-RESPONSE)                               32170000
321800        END-EXEC                                                  32180000
321900     END-IF                                                       32190000
322000        MOVE WS-RESPONSE TO FILE-STATUS                           32200000
322100     IF SUCCESS                                                   32210000
322200        SET ERRORS-FOUND TO TRUE                                  32220000
322300        MOVE -1 TO SCR06-VARIABLE-CURSOR(NAME-SUB)                32230000
322400        MOVE REV-VIDEO TO SCR06-VARIABLE-HI(NAME-SUB)             32240000
322500*            'RECORD ALREADY EXISTS WITH THIS EFFECTIVE DATE/TIME'32250000
322600        MOVE 'R008' TO MSGLOG-CODE                                32260000
322700     ELSE                                                         32270000
322800        IF NO-RECORD-FND                                          32280000
322900           MOVE P06CA-TURN-KEY(NAME-SUB) TO EBTURN                32290000
323000           IF TAG-XB                                              32300000
323100              EXEC CICS READ                                      32310000
323200                        DATASET(EB-VIA-TURN-NBR)                  32320000
323300                        INTO(WS-EXTRA-BOARD)                      32330000
323400                        LENGTH(EBTURNNO-RLGTH)                    32340000
323500                        RIDFLD(EBTURN)                            32350000
323600                        KEYLENGTH(EBTURNNO-KLGTH)                 32360000
323700                        RESP(WS-RESPONSE)                         32370000
323800              END-EXEC                                            32380000
323900              MOVE WS-RESPONSE TO FILE-STATUS                     32390000
324000              IF NOT SUCCESS                                      32400000
324100                 MOVE 'P4200-1' TO ERR-PARAGRAPH                  32410000
324200                 MOVE EBTURN    TO ERR-KEY                        32420000
324300                 PERFORM P9999-GOT-PROBLEM                        32430000
324400              END-IF                                              32440000
324500              IF TAG-YOUR-IT                                      32450000
324600                 MOVE WS-VL3-DATE-TIME TO XB-WK-KEY1-DATE-TIME    32460000
324700                                          XB-WK-KEY1-DT           32470000
324800                 MOVE WS-VL3-TIE-CODE  TO XB-WK-KEY1-TIE-CODE     32480000
324900                                          XB-WK-KEY1-TC           32490000
325000                 MOVE EB-POS-DATE-TIME TO EB-POS-DT               32500000
325100                 MOVE EB-POS-TIE-BREAKER TO EB-POS-TB             32510000
325200*                                                                 32520000
325300                 SET DE-YYMMDD-FORMAT       TO TRUE               32530000
325400                 MOVE XB-WK-KEY1-DT(1:6)    TO DE-YYMMDD          32540000
325500                 PERFORM P8998-DATEEDIT                           32550000
325600                 MOVE DE-YYMMDD-CE          TO XB-WK-KEY1-CENT    32560000
325700*                                                                 32570000
325800                 SET DE-YYMMDD-FORMAT       TO TRUE               32580000
325900                 MOVE EB-POS-DT(1:6)        TO DE-YYMMDD          32590000
326000                 PERFORM P8998-DATEEDIT                           32600000
326100                 MOVE DE-YYMMDD-CE          TO EB-POS-CENT        32610000
326200*                                                                 32620000
326300*                IF XB-WK-KEY1-TIME > EB-POS-DATE-TIME-TIE        32630000
326400                 IF XB-WK-KEY1-TIME-CENT > EB-POS-DATE-TIME-CENT  32640000
326500                    SET TAG-B-XB TO TRUE                          32650000
326600                 ELSE                                             32660000
326700                    SET TAG-A-XB TO TRUE                          32670000
326800                 END-IF                                           32680000
326900                 PERFORM P4300-ROTATE-TAG                         32690000
327000                 MOVE EB-POS-DATE-TIME TO P943-FUN05-OLD-DATE-TIME32700000
327100              END-IF                                              32710000
327200           END-IF                                                 32720000
327300           MOVE P06CA-TURN-KEY(NAME-SUB) TO EBTURN                32730000
327400           EXEC CICS READ                                         32740000
327500                     UPDATE                                       32750000
327600                     DATASET(EB-VIA-TURN-NBR)                     32760000
327700                     INTO(WS-EXTRA-BOARD)                         32770000
327800                     LENGTH(EBTURNNO-RLGTH)                       32780000
327900                     RIDFLD(EBTURN)                               32790000
328000                     KEYLENGTH(EBTURNNO-KLGTH)                    32800000
328100                     RESP(WS-RESPONSE)                            32810000
328200           END-EXEC                                               32820000
328300           MOVE WS-RESPONSE TO FILE-STATUS                        32830000
328400           IF NOT SUCCESS                                         32840000
328500              MOVE 'P4200-2' TO ERR-PARAGRAPH                     32850000
328600              MOVE EBTURN    TO ERR-KEY                           32860000
328700              PERFORM P9999-GOT-PROBLEM                           32870000
328800           END-IF                                                 32880000
328900           IF TAG-XB AND                                          32890000
329000              TAG-YOUR-IT                                         32900000
329100                MOVE SPACES TO EB-TAG                             32910000
329200           END-IF                                                 32920000
329300*                                                                 32930000
329400*    IF NOT A FASTSLOW BOARD, UPDATE BOTHE THE FAST AND SLOW      32940000
329500*    POSITIONS.  OTHERWISE, UPDATE THE APPROPRIATE SIDE.          32950000
329600*                                                          **PLS  32960000
329700           MOVE SPACES TO EB-AWARDS-TIE-BREAKER                   32970000
329800           IF WS-FASTSLOW-XB                                      32980000
329900              IF SCR06-YARD-ROAD = 'Y'                            32990000
330000                 MOVE WS-VL3-DATE-TIME TO EB-POS-DATE-TIME        33000000
330100                 MOVE WS-VL3-TIE-CODE  TO EB-POS-TIE-BREAKER      33010000
330200                 MOVE EB-POSITION-TIME TO EB-LAST-POSITION        33020000
330300              ELSE                                                33030000
330400                 MOVE WS-VL3-DATE-TIME TO EB-SLOW-POS-DATE-TIME   33040000
330500                 MOVE WS-VL3-TIE-CODE  TO EB-SLOW-POS-TIE-BREAKER 33050000
330600                 MOVE EB-SLOW-POSITION-TIME                       33060000
330700                                       TO EB-SLOW-LAST-POSITION   33070000
330800              END-IF                                              33080000
330900           ELSE                                                   33090000
331000              MOVE WS-VL3-DATE-TIME TO EB-POS-DATE-TIME           33100000
331100              MOVE WS-VL3-TIE-CODE  TO EB-POS-TIE-BREAKER         33110000
331200              MOVE WS-VL3-DATE-TIME TO EB-SLOW-POS-DATE-TIME      33120000
331300              MOVE WS-VL3-TIE-CODE  TO EB-SLOW-POS-TIE-BREAKER    33130000
331400              MOVE EB-POSITION-TIME TO EB-LAST-POSITION           33140000
331500              MOVE EB-SLOW-POSITION-TIME                          33150000
331600                                    TO EB-SLOW-LAST-POSITION      33160000
331700           END-IF                                                 33170000
331800           MOVE EB-POS-DATE-TIME    TO P943-FUN05-OLD-DATE-TIME   33180000
331900                                                                  33190000
332000           PERFORM P8200-REWRITE-EB                               33200000
332100           IF NOT SUCCESS                                         33210000
332200              MOVE 'P4200-3' TO ERR-PARAGRAPH                     33220000
332300              MOVE EBTURN    TO ERR-KEY                           33230000
332400              PERFORM P9999-GOT-PROBLEM                           33240000
332500           END-IF                                                 33250000
332600           MOVE 'X'            TO WK-ASGN-JOB-TYPE                33260000
332700           MOVE SCR06-DIST     TO WK-ASGN-DIST                    33270000
332800           MOVE SCR06-SUB-DIST TO WK-ASGN-SUB-DIST                33280000
332900           MOVE 'EX      '     TO WK-SWASSGN-ASGN                 33290000
333000           MOVE SCR06-TURN(NAME-SUB) TO WK-ASGN-XB-TURN           33300000
333100           MOVE SCR06-CC       TO WK-ASGN-CC                      33310000
333200           IF P06CA-EMP-NO(NAME-SUB) > ZEROES                     33320000
333300              MOVE P06CA-EMP-NO(NAME-SUB) TO MSTRNBRK             33330000
333400              PERFORM P8500-READ-MASTER                           33340000
333500              MOVE CRAFT OF WS-MSTR       TO P943-CRAFT           33350000
333600              MOVE EMP-NBR OF WS-MSTR     TO P943-EMP-NBR         33360000
333700              MOVE LAYOFF-CODE OF WS-MSTR TO P943-LO              33370000
333800           ELSE                                                   33380000
333900              MOVE '999999998' TO P943-EMP-NBR                    33390000
334000           END-IF                                                 33400000
334100           MOVE SCR06-DIST          TO P943-DIST                  33410000
334200                                       P943-NA-DIST               33420000
334300           MOVE SCR06-SUB-DIST      TO P943-SDIST                 33430000
334400                                       P943-NA-SUB-DIST           33440000
334500           MOVE WK-SWASSGN-ASGN     TO P943-NORM-ASGNMT           33450000
334600           MOVE WS-VL3-LOCAL-DATE-TIME                            33460000
334700                                    TO P943-EFF-DATE-TIME         33470000
334800           PERFORM P8900-WRITE-HISTORY                            33480000
334900        ELSE                                                      33490000
335000           MOVE 'P4200-4' TO ERR-PARAGRAPH                        33500000
335100           MOVE EBTURN    TO ERR-KEY                              33510000
335200           PERFORM P9999-GOT-PROBLEM                              33520000
335300        END-IF                                                    33530000
335400     END-IF.                                                      33540000
335500*                                                                 33550000
335600 P4300-ROTATE-TAG.                                                33560000
335700*                                                                 33570000
335800     MOVE EBPOS-AREA TO EBPOS                                     33580000
335900     EXEC CICS STARTBR                                            33590000
336000               DATASET(EB-VIA-CRAFT-POSITION)                     33600000
336100               RIDFLD(EBPOS)                                      33610000
336200               EQUAL                                              33620000
336300               RESP(WS-RESPONSE)                                  33630000
336400     END-EXEC                                                     33640000
336500     MOVE WS-RESPONSE TO FILE-STATUS                              33650000
336600     IF SUCCESS                                                   33660000
336700        IF TAG-A-XB                                               33670000
336800           EXEC CICS READPREV                                     33680000
336900                     DATASET(EB-VIA-CRAFT-POSITION)               33690000
337000                     INTO(WS-EXTRA-BOARD)                         33700000
337100                     LENGTH(EBCRPOS-RLGTH)                        33710000
337200                     RIDFLD(EBPOS)                                33720000
337300                     KEYLENGTH(EBCRPOS-KLGTH)                     33730000
337400                     RESP(WS-RESPONSE)                            33740000
337500           END-EXEC                                               33750000
337600           MOVE WS-RESPONSE TO FILE-STATUS                        33760000
337700           IF SUCCESS                                             33770000
337800              EXEC CICS READPREV                                  33780000
337900                        DATASET(EB-VIA-CRAFT-POSITION)            33790000
338000                        INTO(WS-EXTRA-BOARD)                      33800000
338100                        LENGTH(EBCRPOS-RLGTH)                     33810000
338200                        RIDFLD(EBPOS)                             33820000
338300                        KEYLENGTH(EBCRPOS-KLGTH)                  33830000
338400                        RESP(WS-RESPONSE)                         33840000
338500              END-EXEC                                            33850000
338600              MOVE WS-RESPONSE TO FILE-STATUS                     33860000
338700           END-IF                                                 33870000
338800        ELSE                                                      33880000
338900           EXEC CICS READNEXT                                     33890000
339000                     DATASET(EB-VIA-CRAFT-POSITION)               33900000
339100                     INTO(WS-EXTRA-BOARD)                         33910000
339200                     LENGTH(EBCRPOS-RLGTH)                        33920000
339300                     RIDFLD(EBPOS)                                33930000
339400                     KEYLENGTH(EBCRPOS-KLGTH)                     33940000
339500                     RESP(WS-RESPONSE)                            33950000
339600           END-EXEC                                               33960000
339700           MOVE WS-RESPONSE TO FILE-STATUS                        33970000
339800           IF SUCCESS                                             33980000
339900              EXEC CICS READNEXT                                  33990000
340000                        DATASET(EB-VIA-CRAFT-POSITION)            34000000
340100                        INTO(WS-EXTRA-BOARD)                      34010000
340200                        LENGTH(EBCRPOS-RLGTH)                     34020000
340300                        RIDFLD(EBPOS)                             34030000
340400                        KEYLENGTH(EBCRPOS-KLGTH)                  34040000
340500                        RESP(WS-RESPONSE)                         34050000
340600              END-EXEC                                            34060000
340700              MOVE WS-RESPONSE TO FILE-STATUS                     34070000
340800           ELSE                                                   34080000
340900              MOVE 'P4300-1' TO ERR-PARAGRAPH                     34090000
341000              MOVE EBPOS     TO ERR-KEY                           34100000
341100              PERFORM P9999-GOT-PROBLEM                           34110000
341200           END-IF                                                 34120000
341300        END-IF                                                    34130000
341400        MOVE WS-RESPONSE TO FILE-STATUS                           34140000
341500*                                                          **PLS  34150000
341600        IF SUCCESS                                                34160000
341700           EXEC CICS ENDBR                                        34170000
341800                     DATASET(EB-VIA-CRAFT-POSITION)               34180000
341900                     RESP(WS-RESPONSE)                            34190000
342000           END-EXEC                                               34200000
342100           IF DIST OF WS-EXTRA-BOARD = SCR06-DIST                 34210000
342200              AND SUB-DIST OF WS-EXTRA-BOARD = SCR06-SUB-DIST     34220000
342300              AND CRAFT-CODE OF WS-EXTRA-BOARD = SCR06-CC         34230000
342400              AND EB-ON-BOARD                                     34240000
342500              MOVE EBTURN-AREA TO EBTURN                          34250000
342600              EXEC CICS READ                                      34260000
342700                        UPDATE                                    34270000
342800                        DATASET(EB-VIA-TURN-NBR)                  34280000
342900                        INTO(WS-EXTRA-BOARD)                      34290000
343000                        LENGTH(EBTURNNO-RLGTH)                    34300000
343100                        RIDFLD(EBTURN)                            34310000
343200                        KEYLENGTH(EBTURNNO-KLGTH)                 34320000
343300                        RESP(WS-RESPONSE)                         34330000
343400              END-EXEC                                            34340000
343500              MOVE WS-RESPONSE TO FILE-STATUS                     34350000
343600              IF SUCCESS                                          34360000
343700                 SET TAG-YOUR-IT TO TRUE                          34370000
343800                 PERFORM P8200-REWRITE-EB                         34380000
343900              ELSE                                                34390000
344000                 MOVE 'P4300-2' TO ERR-PARAGRAPH                  34400000
344100                 MOVE EBPOS     TO ERR-KEY                        34410000
344200                 PERFORM P9999-GOT-PROBLEM                        34420000
344300              END-IF                                              34430000
344400           END-IF                                                 34440000
344500        ELSE                                                      34450000
344600           EXEC CICS ENDBR                                        34460000
344700                     DATASET(EB-VIA-CRAFT-POSITION)               34470000
344800                     RESP(WS-RESPONSE)                            34480000
344900           END-EXEC                                               34490000
345000           IF NOT (NO-RECORD-FND OR END-OF-FILE)                  34500000
345100              MOVE 'P4300-3' TO ERR-PARAGRAPH                     34510000
345200              MOVE EBPOS     TO ERR-KEY                           34520000
345300              PERFORM P9999-GOT-PROBLEM                           34530000
345400           END-IF                                                 34540000
345500        END-IF                                                    34550000
345600     ELSE                                                         34560000
345700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     34570000
345800           MOVE 'P4300-4' TO ERR-PARAGRAPH                        34580000
345900           MOVE EBTURN    TO ERR-KEY                              34590000
346000           PERFORM P9999-GOT-PROBLEM                              34600000
346100        END-IF                                                    34610000
346200     END-IF.                                                      34620000
346300*                                                                 34630000
346400 P5000-DELETE-XB.                                                 34640000
346500*                                                                 34650000
346600     PERFORM VARYING FUNC-SUB FROM 1 BY 1                         34660000
346700        UNTIL FUNC-SUB > ARRAY-MAX                                34670000
346800        OR ERRORS-FOUND                                           34680000
346900        MOVE FUNC-SUB TO NAME-SUB                                 34690000
347000        IF SCR06-FUNC-CODE(FUNC-SUB) = 'X'                        34700000
347100           SET FUNCTION-FOUND TO TRUE                             34710000
347200           MOVE SCR06-TURN(NAME-SUB) TO XB-WORK-KEY2-TURN         34720000
347300           MOVE XB-WORK-KEY2 TO EBTURN                            34730000
347400           EXEC CICS READ                                         34740000
347500                     DATASET(EB-VIA-TURN-NBR)                     34750000
347600                     INTO(WS-EXTRA-BOARD)                         34760000
347700                     LENGTH(EBTURNNO-RLGTH)                       34770000
347800                     RIDFLD(EBTURN)                               34780000
347900                     KEYLENGTH(EBTURNNO-KLGTH)                    34790000
348000                     RESP(WS-RESPONSE)                            34800000
348100           END-EXEC                                               34810000
348200           MOVE WS-RESPONSE TO FILE-STATUS                        34820000
348300           IF SUCCESS                                             34830000
348400              MOVE 'X' TO WK-ASGN-JOB-TYPE                        34840000
348500              MOVE SCR06-DIST TO WK-ASGN-DIST                     34850000
348600              MOVE SCR06-SUB-DIST TO WK-ASGN-SUB-DIST             34860000
348700              MOVE 'EX      ' TO WK-SWASSGN-ASGN                  34870000
348800              MOVE TURN-NBR OF WS-EXTRA-BOARD TO WK-ASGN-XB-TURN  34880000
348900              MOVE CRAFT-CODE OF WS-EXTRA-BOARD TO WK-ASGN-CC     34890000
349000              IF SLOT-BOARD                                       34900000
349100                 PERFORM PXXXX-LATEST-TEMP                        34910000
349200                 IF ASGN-EMP-NO NOT > ZERO                        34920000
349300                    PERFORM PXXXX-JOB-OWNER                       34930000
349400                 END-IF                                           34940000
349500                 IF ASGN-EMP-NO > ZERO                            34950000
349600                    SET ERRORS-FOUND TO TRUE                      34960000
349700                    MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)   34970000
349800                    MOVE REV-VIDEO TO SCR06-NAME-HI(NAME-SUB)     34980000
349900*                          'MUST USE DISPLACEMENT FOR EMPLOYEE'   34990000
350000*                          ' ON SLOT BOARD'                       35000000
350100                    MOVE 'M024' TO MSGLOG-CODE                    35010000
350200                 END-IF                                           35020000
350300              ELSE                                                35030000
350400                 PERFORM PXXXX-LATEST-TEMP                        35040000
350500                 IF ASGN-EMP-NO > ZERO                            35050000
350600                    IF AUGMENTED-TO-EXTRA-BOARD                   35060000
350700                       SET ERRORS-FOUND TO TRUE                   35070000
350800                       MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)35080000
350900                       MOVE REV-VIDEO TO SCR06-NAME-HI(NAME-SUB)  35090000
351000                       IF WS-FUNCTION = 'C'                       35100000
351100*                                'CANNOT CUT TURN WITH AUG'       35110000
351200*                                'MENTED EMPLOYEE - MUST '        35120000
351300*                                'DEADHEAD HOME FIRST'            35130000
351400                          MOVE 'C014' TO MSGLOG-CODE              35140000
351500                       ELSE                                       35150000
351600*                                'CANNOT DELETE TURN WITH '       35160000
351700*                                'AUGMENTED EMPLOYEE - MUST '     35170000
351800*                                'DEADHEAD HOME FIRST'            35180000
351900                          MOVE 'C015' TO MSGLOG-CODE              35190000
352000                       END-IF                                     35200000
352100                    END-IF                                        35210000
352200                 END-IF                                           35220000
352300              END-IF                                              35230000
352310              IF CNTL-XB-SCHEDULED                                35231002
352400                 PERFORM P5010-CHECK-XB-SCHEDULES                 35240002
352401              ELSE                                                35240102
352410                 IF CNTL-XB-EXTENDED-SCHED                        35241002
352420                    PERFORM P5020-CHECK-EXT-XB-SCHED              35242002
352430                 END-IF                                           35243002
352440              END-IF                                              35244002
352500              IF NOT ERRORS-FOUND                                 35250000
352600                 PERFORM P5050-PROCESS-DELETE                     35260002
352700              END-IF                                              35270000
352800           ELSE                                                   35280000
352900              SET ERRORS-FOUND TO TRUE                            35290000
353000              IF NO-RECORD-FND                                    35300000
353100*                     'NO RECORD FOUND TO DELETE'                 35310000
353200                 MOVE 'N007' TO MSGLOG-CODE                       35320000
353300                 MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)           35330000
353400                 MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)        35340000
353500              ELSE                                                35350000
353600                 MOVE 'P5000-1' TO ERR-PARAGRAPH                  35360000
353700                 MOVE EBTURN TO ERR-KEY                           35370000
353800                 PERFORM P9999-GOT-PROBLEM                        35380000
353900              END-IF                                              35390000
354000           END-IF                                                 35400000
354100        ELSE                                                      35410000
354200           IF SCR06-FUNC-CODE(NAME-SUB) > SPACE                   35420000
354300              SET ERRORS-FOUND TO TRUE                            35430000
354400              MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)         35440000
354500              MOVE REV-VIDEO TO SCR06-FUNC-CODE-HI(NAME-SUB)      35450000
354600*                  INVALID-CODE-MSG                               35460000
354700              MOVE 'I041' TO MSGLOG-CODE                          35470000
354800           END-IF                                                 35480000
354900        END-IF                                                    35490000
355000     END-PERFORM                                                  35500000
355100     IF NOT FUNCTION-FOUND                                        35510000
355200        SET ERRORS-FOUND TO TRUE                                  35520000
355300        MOVE -1 TO SCR06-FUNC-CODE-CURSOR(1)                      35530000
355400*            'PLACE AN X NEXT TO POSITION TO BE DELETED'          35540000
355500        MOVE 'P022' TO MSGLOG-CODE                                35550000
355600     END-IF                                                       35560000
355700     IF NOT ERRORS-FOUND                                          35570000
355800        PERFORM P1000-LIST-XB                                     35580000
355900     END-IF.                                                      35590000
356000*=================================================================35600000
356100 P5010-CHECK-XB-SCHEDULES.                                        35610000
356200*=================================================================35620000
356300     MOVE SPACES                   TO WORK-JS-KEY1                35630000
356400     MOVE SCR06-DIST               TO WK-JSK1-ASGN-DIST           35640000
356500     MOVE SCR06-SUB-DIST           TO WK-JSK1-ASGN-SUB-DIST       35650000
356600     MOVE 'EX'                     TO WK-JSK1-ASGN(1:2)           35660000
356700     MOVE SCR06-TURN(NAME-SUB)     TO WK-JSK1-ASGN(3:4)           35670000
356800     MOVE SCR06-CC                 TO WK-JSK1-ASGN-CC             35680000
356900     MOVE WORK-JS-KEY1             TO JSKEY1                      35690000
356910*                                                                 35691000
356920     EXEC CICS READ                                               35692000
356930               GTEQ                                               35693000
356940               DATASET(JS-VIA-JSKEY1)                             35694000
356950               INTO(WS-JOB-SCHEDULE)                              35695000
356960               LENGTH(JSKEY1-RLGTH)                               35696000
356970               RIDFLD(JSKEY1)                                     35697000
356980               KEYLENGTH(JSKEY1-KLGTH)                            35698000
356990               RESP(WS-RESPONSE)                                  35699000
357000     END-EXEC                                                     35700000
357001     MOVE WS-RESPONSE              TO FILE-STATUS                 35700100
357002     IF SUCCESS                                                   35700200
357003        IF  JSK1-ASGN-DIST     = WK-JSK1-ASGN-DIST                35700300
357004        AND JSK1-ASGN-SUB-DIST = WK-JSK1-ASGN-SUB-DIST            35700400
357005        AND JSK1-ASSIGNMENT    = WK-JSK1-ASSIGNMENT               35700500
357006           SET ERRORS-FOUND        TO TRUE                        35700600
357007           MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)            35700700
357008           MOVE REV-VIDEO TO SCR06-NAME-HI(NAME-SUB)              35700800
357009*               'CANNOT CUT/DELETE TURN - DELETE SPAREBOARD '     35700900
357010*               'SCHEDULES FIRST '                                35701000
357011           MOVE 'C184'             TO MSGLOG-CODE                 35701100
357012        END-IF                                                    35701200
357013     ELSE                                                         35701300
357014        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     35701400
357015           MOVE 'P5010-1'          TO ERR-PARAGRAPH               35701500
357016           MOVE JSKEY1             TO ERR-KEY                     35701600
357017           PERFORM P9999-GOT-PROBLEM                              35701700
357018        END-IF                                                    35701800
357019     END-IF.                                                      35701900
357020*=================================================================35702002
357021 P5020-CHECK-EXT-XB-SCHED.                                        35702102
357022*=================================================================35702202
357023     INITIALIZE PS42-COMMAREA-PARMS                               35702302
357024     SET PS42-GET-CURRENT-SCHED         TO TRUE                   35702402
357025     MOVE SCR06-DIST                    TO PS42-DIST-CD           35702502
357026     MOVE SCR06-SUB-DIST                TO PS42-SUB-DIST-CD       35702602
357027     MOVE SCR06-CC                      TO PS42-BOARD-ID          35702704
357028     MOVE SCR06-TURN(NAME-SUB)          TO PS42-TURN-CD           35702802
357029     MOVE WS-LOCAL-DATE                 TO PS42-START-DATE        35702903
357030     MOVE WS-LOCAL-TIME                 TO PS42-FROM-TIME         35703003
357031*                                                                 35703102
357032     EXEC CICS LINK                                               35703202
357033               PROGRAM(PS42-PGM)                                  35703302
357034               COMMAREA(PS42-COMMAREA-PARMS)                      35703402
357036               LENGTH(PS42-LGTH)                                  35703602
357039               RESP(WS-RESPONSE)                                  35703902
357040     END-EXEC                                                     35704002
357041     MOVE WS-RESPONSE              TO FILE-STATUS                 35704102
357042     IF SUCCESS                                                   35704202
357043        IF PS42-SCHEDULES-EXIST                                   35704302
357044           OR PS42-CURR-SCHED-FOUND                               35704402
357046           SET ERRORS-FOUND             TO TRUE                   35704602
357047           MOVE -1             TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)35704702
357048           MOVE REV-VIDEO               TO SCR06-NAME-HI(NAME-SUB)35704802
357049*               'CANNOT CUT/DELETE TURN - DELETE SPAREBOARD '     35704902
357050*               'SCHEDULES FIRST '                                35705002
357051           MOVE 'C184'                  TO MSGLOG-CODE            35705102
357052        END-IF                                                    35705202
357053     ELSE                                                         35705302
357055        MOVE 'P5020-1'                  TO ERR-PARAGRAPH          35705502
357056        MOVE 'PS42-LINK'                TO ERR-KEY                35705602
357057        PERFORM P9999-GOT-PROBLEM                                 35705702
357059     END-IF.                                                      35705902
357060*                                                                 35706002
357061 P5050-PROCESS-DELETE.                                            35706102
357062*                                                                 35706202
357063     MOVE SPACES TO P913-COMMAREA-PARMS                           35706302
357064     IF WS-FUNCTION = 'C'                                         35706402
357070        SET P913-CUT-FUNCTION TO TRUE                             35707000
357080     ELSE                                                         35708000
357090        SET P913-DELETE-FUNCTION TO TRUE                          35709000
357100     END-IF                                                       35710000
357200     MOVE EBTURN-AREA     TO P913-TURN-PARM                       35720000
357300     MOVE WS-LOCAL-DATE-TIME TO P913-EFF-DATE-TIME                35730000
357400     MOVE PSTCA-TIME-ZONE TO P913-TIME-ZONE                       35740000
357500     EXEC CICS LINK                                               35750000
357600               PROGRAM(P913-PGM)                                  35760000
357700               COMMAREA(P913-COMMAREA-PARMS)                      35770000
357800               LENGTH(P913-LGTH)                                  35780000
357900               RESP(WS-RESPONSE)                                  35790000
358000     END-EXEC                                                     35800000
358100     MOVE WS-RESPONSE TO FILE-STATUS                              35810000
358200     IF NOT SUCCESS                                               35820000
358300        MOVE 'P5050-1' TO ERR-PARAGRAPH                           35830000
358400        PERFORM P9999-GOT-PROBLEM                                 35840000
358500     END-IF                                                       35850000
358600     MOVE ' ' TO SCR06-FUNC-CODE(NAME-SUB).                       35860000
358700*                                                                 35870000
358800 P6000-MOVE-YARD-ROAD.                                            35880000
358900*                                                                 35890000
359000     PERFORM VARYING FUNC-SUB FROM 1 BY 1                         35900000
359100        UNTIL FUNC-SUB > ARRAY-MAX                                35910000
359200        OR ERRORS-FOUND                                           35920000
359300        MOVE FUNC-SUB TO NAME-SUB                                 35930000
359400        IF SCR06-FUNC-CODE(FUNC-SUB) = 'X'                        35940000
359500           SET FUNCTION-FOUND TO TRUE                             35950000
359600           MOVE SCR06-TURN(NAME-SUB) TO XB-WORK-KEY2-TURN         35960000
359700           MOVE XB-WORK-KEY2 TO EBTURN                            35970000
359800           EXEC CICS READ                                         35980000
359900                     DATASET(EB-VIA-TURN-NBR)                     35990000
360000                     INTO(WS-EXTRA-BOARD)                         36000000
360100                     LENGTH(EBTURNNO-RLGTH)                       36010000
360200                     RIDFLD(EBTURN)                               36020000
360300                     KEYLENGTH(EBTURNNO-KLGTH)                    36030000
360400                     RESP(WS-RESPONSE)                            36040000
360500           END-EXEC                                               36050000
360600           MOVE WS-RESPONSE TO FILE-STATUS                        36060000
360700           IF SUCCESS                                             36070000
360800              IF EB-ON-BOARD                                      36080000
360900                 IF EB-YARD-BOARD                                 36090000
361000                    SET EB-ROAD-BOARD TO TRUE                     36100000
361100                 ELSE                                             36110000
361200                    SET EB-YARD-BOARD TO TRUE                     36120000
361300                 END-IF                                           36130000
361400                 MOVE WS-PRESENT-TIME TO EB-POS-DATE-TIME         36140000
361500                 MOVE SPACES         TO EB-POS-TIE-BREAKER        36150000
361600                 MOVE SPACES         TO P911-POSITION-PARMS       36160000
361700                 MOVE EBPOS-AREA     TO P911-POS-KEY              36170000
361800                 EXEC CICS LINK                                   36180000
361900                           PROGRAM(P911-PGM)                      36190000
362000                           COMMAREA(P911-POSITION-PARMS)          36200000
362100                           LENGTH(P911-LGTH)                      36210000
362200                           RESP(WS-RESPONSE)                      36220000
362300                 END-EXEC                                         36230000
362400                 MOVE WS-RESPONSE TO FILE-STATUS                  36240000
362500                 IF NOT SUCCESS                                   36250000
362600                    MOVE 'P6000-1' TO ERR-PARAGRAPH               36260000
362700                    PERFORM P9999-GOT-PROBLEM                     36270000
362800                 END-IF                                           36280000
362900                 MOVE XB-WORK-KEY2 TO EBTURN                      36290000
363000                 EXEC CICS READ                                   36300000
363100                           UPDATE                                 36310000
363200                           DATASET(EB-VIA-TURN-NBR)               36320000
363300                           INTO(WS-EXTRA-BOARD)                   36330000
363400                           LENGTH(EBTURNNO-RLGTH)                 36340000
363500                           RIDFLD(EBTURN)                         36350000
363600                           KEYLENGTH(EBTURNNO-KLGTH)              36360000
363700                           RESP(WS-RESPONSE)                      36370000
363800                 END-EXEC                                         36380000
363900                 MOVE WS-RESPONSE TO FILE-STATUS                  36390000
364000                 IF NOT SUCCESS                                   36400000
364100                    MOVE 'P6000-2' TO ERR-PARAGRAPH               36410000
364200                    MOVE EBTURN    TO ERR-KEY                     36420000
364300                    PERFORM P9999-GOT-PROBLEM                     36430000
364400                 END-IF                                           36440000
364500*                                                                 36450000
364600*             SET BOTH POSITION KEYS.                      **PLS  36460000
364700                 MOVE P911-POS-KEY         TO EBPOS-AREA          36470000
364800                                              EB-SLOW-POS-AREA    36480000
364900                 IF EB-ROAD-BOARD                                 36490000
365000*    CNC0006 - FLW, 5/8/96, START                                 36500000
365100                    MOVE ZEROS TO EB-NBR-YARD-STT-STARTS          36510000
365200                                  EB-NBR-YARD-OVT-STARTS          36520000
365300*    CNC0006 - FLW, 5/8/96, END                                   36530000
365400                 END-IF                                           36540000
365500                 MOVE SPACES TO EB-AWARDS-TIE-BREAKER             36550000
365600                 PERFORM P8200-REWRITE-EB                         36560000
365700                 IF NOT SUCCESS                                   36570000
365800                    MOVE 'P6000-3' TO ERR-PARAGRAPH               36580000
365900                    MOVE EBTURN    TO ERR-KEY                     36590000
366000                    PERFORM P9999-GOT-PROBLEM                     36600000
366100                 END-IF                                           36610000
366200*>>> ADDED 6/8/92 TO WRITE HISTORY <<<*                           36620000
366300                 MOVE SPACES         TO P943-COMMAREA-PARMS       36630000
366400                 SET P943-YARD-ROAD-MOVE-FUN TO TRUE              36640000
366500                 MOVE 'X'            TO WK-ASGN-JOB-TYPE          36650000
366600                 MOVE SCR06-DIST     TO WK-ASGN-DIST              36660000
366700                 MOVE SCR06-SUB-DIST TO WK-ASGN-SUB-DIST          36670000
366800                 MOVE 'EX      '     TO WK-SWASSGN-ASGN           36680000
366900                 MOVE SCR06-TURN(NAME-SUB) TO WK-ASGN-XB-TURN     36690000
367000                 MOVE SCR06-CC       TO WK-ASGN-CC                36700000
367100                 IF P06CA-EMP-NO(NAME-SUB) > ZEROES               36710000
367200                    MOVE P06CA-EMP-NO(NAME-SUB) TO MSTRNBRK       36720000
367300                    PERFORM P8500-READ-MASTER                     36730000
367400                    MOVE CRAFT OF WS-MSTR       TO P943-CRAFT     36740000
367500                    MOVE EMP-NBR OF WS-MSTR     TO P943-EMP-NBR   36750000
367600                    MOVE LAYOFF-CODE OF WS-MSTR TO P943-LO        36760000
367700                 ELSE                                             36770000
367800                    MOVE '999999998' TO P943-EMP-NBR              36780000
367900                 END-IF                                           36790000
368000                 MOVE SCR06-DIST          TO P943-DIST            36800000
368100                                             P943-NA-DIST         36810000
368200                 MOVE SCR06-SUB-DIST      TO P943-SDIST           36820000
368300                                             P943-NA-SUB-DIST     36830000
368400                 MOVE WK-SWASSGN-ASGN     TO P943-NORM-ASGNMT     36840000
368500                 MOVE WS-VL3-LOCAL-DATE-TIME                      36850000
368600                                          TO P943-EFF-DATE-TIME   36860000
368700                 PERFORM P8900-WRITE-HISTORY                      36870000
368800*>>> 6/8/92  END OF ADDED CODE TO WRITE HISTORY <<<*              36880000
368900              ELSE                                                36890000
369000                 SET ERRORS-FOUND TO TRUE                         36900000
369100                 MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)           36910000
369200                 MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)        36920000
369300*                     'TURN IS NOT ON "ACTIVE" BOARD'             36930000
369400                 MOVE 'T008' TO MSGLOG-CODE                       36940000
369500              END-IF                                              36950000
369600           ELSE                                                   36960000
369700              SET ERRORS-FOUND TO TRUE                            36970000
369800              IF NO-RECORD-FND                                    36980000
369900*                     'NO RECORD FOUND TO "MOVE"'                 36990000
370000                 MOVE 'N010' TO MSGLOG-CODE                       37000000
370100                 MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)           37010000
370200                 MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)        37020000
370300              ELSE                                                37030000
370400                 MOVE 'P6000-4' TO ERR-PARAGRAPH                  37040000
370500                 MOVE EBTURN TO ERR-KEY                           37050000
370600                 PERFORM P9999-GOT-PROBLEM                        37060000
370700              END-IF                                              37070000
370800           END-IF                                                 37080000
370900        ELSE                                                      37090000
371000           IF SCR06-FUNC-CODE(NAME-SUB) > SPACE                   37100000
371100              SET ERRORS-FOUND TO TRUE                            37110000
371200              MOVE -1 TO SCR06-FUNC-CODE-CURSOR(NAME-SUB)         37120000
371300              MOVE REV-VIDEO TO SCR06-FUNC-CODE-HI(NAME-SUB)      37130000
371400*                  INVALID-CODE-MSG                               37140000
371500              MOVE 'I041' TO MSGLOG-CODE                          37150000
371600           END-IF                                                 37160000
371700        END-IF                                                    37170000
371800     END-PERFORM                                                  37180000
371900     IF NOT FUNCTION-FOUND                                        37190000
372000        SET ERRORS-FOUND TO TRUE                                  37200000
372100        MOVE -1 TO SCR06-FUNC-CODE-CURSOR(1)                      37210000
372200*            'PLACE AN X NEXT TO POSITION TO BE "MOVED"'          37220000
372300        MOVE 'P023' TO MSGLOG-CODE                                37230000
372400     END-IF                                                       37240000
372500     IF NOT ERRORS-FOUND                                          37250000
372600        PERFORM P1000-LIST-XB                                     37260000
372700     END-IF.                                                      37270000
372800*                                                                 37280000
372900 P7000-SET-TAG.                                                   37290000
373000*                                                                 37300000
373100     PERFORM VARYING FUNC-SUB FROM 1 BY 1                         37310000
373200        UNTIL FUNC-SUB > ARRAY-MAX                                37320000
373300        IF SCR06-FUNC-CODE(FUNC-SUB) = 'X'                        37330000
373400           SET FUNCTION-FOUND TO TRUE                             37340000
373500           MOVE FUNC-SUB TO NAME-SUB                              37350000
373600           ADD ARRAY-MAX TO FUNC-SUB                              37360000
373700        END-IF                                                    37370000
373800     END-PERFORM                                                  37380000
373900     IF FUNCTION-FOUND                                            37390000
374000        MOVE SCR06-TURN(NAME-SUB) TO XB-WORK-KEY2-TURN            37400000
374100        MOVE XB-WORK-KEY2 TO EBTURN                               37410000
374200        EXEC CICS READ                                            37420000
374300                  DATASET(EB-VIA-TURN-NBR)                        37430000
374400                  INTO(WS-EXTRA-BOARD)                            37440000
374500                  LENGTH(EBTURNNO-RLGTH)                          37450000
374600                  RIDFLD(EBTURN)                                  37460000
374700                  KEYLENGTH(EBTURNNO-KLGTH)                       37470000
374800                  RESP(WS-RESPONSE)                               37480000
374900        END-EXEC                                                  37490000
375000        MOVE WS-RESPONSE TO FILE-STATUS                           37500000
375100        IF SUCCESS                                                37510000
375200           PERFORM P8000-LOCATE-TAG                               37520000
375300           IF WS-TURN-WITH-TAG > SPACES                           37530000
375400              MOVE WS-TURN-WITH-TAG TO EBTURN                     37540000
375500              EXEC CICS READ                                      37550000
375600                        UPDATE                                    37560000
375700                        DATASET(EB-VIA-TURN-NBR)                  37570000
375800                        INTO(WS-EXTRA-BOARD)                      37580000
375900                        LENGTH(EBTURNNO-RLGTH)                    37590000
376000                        RIDFLD(EBTURN)                            37600000
376100                        KEYLENGTH(EBTURNNO-KLGTH)                 37610000
376200                        RESP(WS-RESPONSE)                         37620000
376300              END-EXEC                                            37630000
376400              MOVE WS-RESPONSE TO FILE-STATUS                     37640000
376500              IF NOT SUCCESS                                      37650000
376600                 MOVE 'P7000-1' TO ERR-PARAGRAPH                  37660000
376700                 MOVE EBTURN    TO ERR-KEY                        37670000
376800                 PERFORM P9999-GOT-PROBLEM                        37680000
376900              END-IF                                              37690000
377000              MOVE ' ' TO EB-TAG                                  37700000
377100              PERFORM P8200-REWRITE-EB                            37710000
377200              IF NOT SUCCESS                                      37720000
377300                 MOVE 'P7000-2' TO ERR-PARAGRAPH                  37730000
377400                 MOVE EBTURN    TO ERR-KEY                        37740000
377500                 PERFORM P9999-GOT-PROBLEM                        37750000
377600              END-IF                                              37760000
377700           END-IF                                                 37770000
377800           MOVE XB-WORK-KEY2 TO EBTURN                            37780000
377900           EXEC CICS READ                                         37790000
378000                     UPDATE                                       37800000
378100                     DATASET(EB-VIA-TURN-NBR)                     37810000
378200                     INTO(WS-EXTRA-BOARD)                         37820000
378300                     LENGTH(EBTURNNO-RLGTH)                       37830000
378400                     RIDFLD(EBTURN)                               37840000
378500                     KEYLENGTH(EBTURNNO-KLGTH)                    37850000
378600                     RESP(WS-RESPONSE)                            37860000
378700           END-EXEC                                               37870000
378800           MOVE WS-RESPONSE TO FILE-STATUS                        37880000
378900           IF NOT SUCCESS                                         37890000
379000              MOVE 'P7000-3' TO ERR-PARAGRAPH                     37900000
379100              MOVE EBTURN    TO ERR-KEY                           37910000
379200              PERFORM P9999-GOT-PROBLEM                           37920000
379300           END-IF                                                 37930000
379400           SET TAG-YOUR-IT TO TRUE                                37940000
379500           PERFORM P8200-REWRITE-EB                               37950000
379600           IF NOT SUCCESS                                         37960000
379700              MOVE 'P7000-4' TO ERR-PARAGRAPH                     37970000
379800              MOVE EBTURN    TO ERR-KEY                           37980000
379900              PERFORM P9999-GOT-PROBLEM                           37990000
380000           END-IF                                                 38000000
380100        ELSE                                                      38010000
380200           SET ERRORS-FOUND TO TRUE                               38020000
380300           IF NO-RECORD-FND                                       38030000
380400*                  'NO RECORD FOUND TO "TAG"'                     38040000
380500              MOVE 'N005' TO MSGLOG-CODE                          38050000
380600              MOVE -1 TO SCR06-TURN-CURSOR(NAME-SUB)              38060000
380700              MOVE REV-VIDEO TO SCR06-TURN-HI(NAME-SUB)           38070000
380800           ELSE                                                   38080000
380900              MOVE 'P7000-5' TO ERR-PARAGRAPH                     38090000
381000              MOVE EBTURN TO ERR-KEY                              38100000
381100              PERFORM P9999-GOT-PROBLEM                           38110000
381200           END-IF                                                 38120000
381300        END-IF                                                    38130000
381400     ELSE                                                         38140000
381500        SET ERRORS-FOUND TO TRUE                                  38150000
381600        MOVE -1 TO SCR06-FUNC-CODE-CURSOR(1)                      38160000
381700*            'PLACE AN X NEXT TO POSITION TO BE "TAGGED"'         38170000
381800        MOVE 'P024' TO MSGLOG-CODE                                38180000
381900     END-IF                                                       38190000
382000     IF NOT ERRORS-FOUND                                          38200000
382100        PERFORM P1000-LIST-XB                                     38210000
382200     END-IF.                                                      38220000
382300*                                                                 38230000
382400 P7005-WRITE-TSQUEUE.                                             38240000
382500*                                                                 38250000
382600*                                                                 38260000
382700*      WRITE MAP TSQUEUE                                          38270000
382800*                                                                 38280000
382900     EXEC CICS ASSIGN                                             38290000
383000          EXTDS(WS-CICS-EXTDS-CODE)                               38300000
383100     END-EXEC                                                     38310000
383200*                                                                 38320000
383300     IF SCREEN-HAS-EXT-ATTR                                       38330000
383400        EXEC CICS SEND STRFIELD                                   38340000
383500                  FROM(WS-STRFIELD)                               38350000
383600                  LENGTH(WS-STRFIELD-LGTH)                        38360000
383700                  RESP(WS-RESPONSE)                               38370000
383800        END-EXEC                                                  38380000
383900        MOVE WS-RESPONSE           TO FILE-STATUS                 38390000
384000        IF NOT SUCCESS                                            38400000
384100           MOVE 'P7005-1'          TO ERR-PARAGRAPH               38410000
384200           MOVE 'SEND STRFIELD'    TO ERR-KEY                     38420000
384300           PERFORM P9999-GOT-PROBLEM                              38430000
384400        END-IF                                                    38440000
384500     END-IF                                                       38450000
384600*                                                                 38460000
384700     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH              38470000
384800     EXEC CICS RECEIVE BUFFER                                     38480000
384900               INTO(WS-BUFFER-DATA)                               38490000
385000               LENGTH(WS-BUFFER-LGTH)                             38500000
385100               RESP(WS-RESPONSE)                                  38510000
385200     END-EXEC                                                     38520000
385300     MOVE WS-RESPONSE              TO FILE-STATUS                 38530000
385400     IF NOT SUCCESS AND NOT EOC                                   38540000
385500        MOVE 'P7005-2'             TO ERR-PARAGRAPH               38550000
385600        MOVE 'RECEIVE BUFFER'      TO ERR-KEY                     38560000
385700        PERFORM P9999-GOT-PROBLEM                                 38570000
385800     END-IF                                                       38580000
385900     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR            38590000
386000                                                                  38600000
386100                                                                  38610000
386200     MOVE LENGTH OF WS-BUFFER-AREA TO P06TSQ-QLGTH                38620000
386300     MOVE EIBTRMID                 TO P06TSQ-MAP-TERM-ID          38630000
386400     EXEC CICS WRITEQ TS                                          38640000
386500               QUEUE(P06TSQ-MAP-QUEUE-ID)                         38650000
386600               FROM(WS-BUFFER-AREA)                               38660000
386700               LENGTH(P06TSQ-QLGTH)                               38670000
386800               RESP(WS-RESPONSE)                                  38680000
386900     END-EXEC                                                     38690000
387000     MOVE WS-RESPONSE              TO FILE-STATUS                 38700000
387100     IF NOT SUCCESS                                               38710000
387200        MOVE 'P7005-3'             TO ERR-PARAGRAPH               38720000
387300        PERFORM P9999-GOT-PROBLEM                                 38730000
387400     END-IF                                                       38740000
387500     MOVE EIBTRMID TO P06TSQ-CA-TERM-ID                           38750000
387600     EXEC CICS WRITEQ TS                                          38760000
387700               QUEUE(P06TSQ-CA-QUEUE-ID)                          38770000
387800               FROM(PSTCOMM-AREA)                                 38780000
387900               LENTGH(PSTCOMM-LGTH)                               38790000
388000               ITEM(P06TSQ-QUEUE-ITEM)                            38800000
388100               RESP(WS-RESPONSE)                                  38810000
388200     END-EXEC                                                     38820000
388300     MOVE WS-RESPONSE              TO FILE-STATUS                 38830000
388400     IF NOT SUCCESS                                               38840000
388500        MOVE 'P7005-4'             TO ERR-PARAGRAPH               38850000
388600        PERFORM P9999-GOT-PROBLEM                                 38860000
388700     END-IF.                                                      38870000
388800*                                                                 38880000
388900 P7010-READ-TSQUEUE.                                              38890000
389000*                                                                 38900000
389100*              READ THE MAPS TSQUEUE                              38910000
389200*                                                                 38920000
389300     MOVE LENGTH OF WS-BUFFER-AREA TO P06TSQ-QLGTH                38930000
389400     MOVE EIBTRMID                 TO P06TSQ-MAP-TERM-ID          38940000
389500     EXEC CICS READQ TS                                           38950000
389600               QUEUE(P06TSQ-MAP-QUEUE-ID)                         38960000
389700               INTO(WS-BUFFER-AREA)                               38970000
389800               LENGTH(P06TSQ-QLGTH)                               38980000
389900               ITEM(P06TSQ-QUEUE-ITEM)                            38990000
390000               RESP(WS-RESPONSE)                                  39000000
390100     END-EXEC                                                     39010000
390200     MOVE WS-RESPONSE              TO FILE-STATUS                 39020000
390300     IF SUCCESS                                                   39030000
390400        SET SEND-BUFFER            TO TRUE                        39040000
390500     ELSE                                                         39050000
390600        SET CREATE-SCREEN          TO TRUE                        39060000
390700        MOVE LOW-VALUES            TO PSTS06                      39070000
390800     END-IF                                                       39080000
390900     MOVE EIBTRMID TO P06TSQ-CA-TERM-ID                           39090000
391000     EXEC CICS READQ TS                                           39100000
391100               QUEUE(P06TSQ-CA-QUEUE-ID)                          39110000
391200               INTO(PSTCOMM-AREA)                                 39120000
391300               LENGTH(PSTCOMM-LGTH)                               39130000
391400               ITEM(P06TSQ-QUEUE-ITEM)                            39140000
391500               RESP(WS-RESPONSE)                                  39150000
391600     END-EXEC                                                     39160000
391700     MOVE WS-RESPONSE TO FILE-STATUS                              39170000
391800     IF NOT SUCCESS                                               39180000
391900        MOVE SPACES TO PSTCOMM-AREA                               39190000
392000     END-IF                                                       39200000
392100     PERFORM P7020-DELETE-TSQUEUE.                                39210000
392200*                                                                 39220000
392300 P7020-DELETE-TSQUEUE.                                            39230000
392400*                                                                 39240000
392500     MOVE EIBTRMID TO P06TSQ-MAP-TERM-ID                          39250000
392600     EXEC CICS DELETEQ TS                                         39260000
392700               QUEUE(P06TSQ-MAP-QUEUE-ID)                         39270000
392800               RESP(WS-RESPONSE)                                  39280000
392900     END-EXEC                                                     39290000
393000     MOVE EIBTRMID TO P06TSQ-CA-TERM-ID                           39300000
393100     EXEC CICS DELETEQ TS                                         39310000
393200               QUEUE(P06TSQ-CA-QUEUE-ID)                          39320000
393300               RESP(WS-RESPONSE)                                  39330000
393400     END-EXEC.                                                    39340000
393500*************************************************************     39350000
393600 P7500-SCHEDULE-REQUEST.                                          39360000
393700*                                                                 39370000
393800*   FIND THE SELECTED TURN BEFORE TRANSFERRING TO CNP06S.         39380000
393900*                        NORTHERN QUEBEC SPAREBOARD - PHASE 2     39390000
394000*************************************************************     39400000
394100     PERFORM VARYING FUNC-SUB FROM +1 BY +1                       39410000
394200        UNTIL FUNC-SUB > ARRAY-MAX                                39420000
394300              OR SCR06-FUNC-CODE(FUNC-SUB) = 'X'                  39430000
394400     END-PERFORM                                                  39440000
394500     IF FUNC-SUB > ARRAY-MAX                                      39450000
394600        OR SCR06-TURN(FUNC-SUB) <= SPACES                         39460000
394700*            'TURN MUST BE SELECTED FOR SCHEDULE DISPLAY'         39470000
394800        MOVE 'B062' TO MSGLOG-CODE                                39480000
394900        MOVE -1 TO SCR06-FUNC-CODE-CURSOR(1)                      39490000
395000        MOVE REV-VIDEO TO SCR06-FUNC-CODE-HI(1)                   39500000
395100        PERFORM P9000-SEND-MAP-AND-RETURN                         39510000
395200     END-IF                                                       39520000
395300     INITIALIZE P06SCOMM-AREA                                     39530000
395400     MOVE SCR06-CC    TO P06SCA-CC                                39540000
395500     MOVE SCR06-BOARD TO P06SCA-BOARD                             39550000
395600     MOVE SCR06-YARD-ROAD TO P06SCA-YARD-ROAD                     39560000
395700     MOVE SCR06-VIEW-TIME TO P06SCA-VIEW-TIME                     39570000
395800     MOVE SCR06-TURN(FUNC-SUB) TO P06SCA-TURN                     39580000
395900     MOVE P06CA-EMP-NO(FUNC-SUB) TO P06SCA-EMP-NO                 39590000
396000     PERFORM P9150-TRANSFER-TO-SCHEDULE                           39600000
396100     .                                                            39610000
396110*************************************************************     39611000
396120 P7600-EXTENDED-SCHED-REQUEST.                                    39612001
396130*                                                                 39613000
396140*   FIND THE SELECTED TURN BEFORE TRANSFERRING TO CNP06E.         39614000
396160*************************************************************     39616000
396170     PERFORM VARYING FUNC-SUB FROM +1 BY +1                       39617000
396180        UNTIL FUNC-SUB > ARRAY-MAX                                39618000
396190              OR SCR06-FUNC-CODE(FUNC-SUB) = 'X'                  39619000
396191     END-PERFORM                                                  39619100
396192     IF FUNC-SUB > ARRAY-MAX                                      39619200
396193        OR SCR06-TURN(FUNC-SUB) <= SPACES                         39619300
396194*            'TURN MUST BE SELECTED FOR SCHEDULE DISPLAY'         39619400
396195        MOVE 'B062'                  TO MSGLOG-CODE               39619500
396196        MOVE -1                      TO SCR06-FUNC-CODE-CURSOR(1) 39619600
396197        MOVE REV-VIDEO               TO SCR06-FUNC-CODE-HI(1)     39619700
396198        PERFORM P9000-SEND-MAP-AND-RETURN                         39619800
396199     END-IF                                                       39619900
396200     INITIALIZE P06ECOMM-AREA                                     39620000
396201     MOVE SCR06-CC                   TO P06ECA-CC                 39620100
396202     MOVE SCR06-BOARD                TO P06ECA-BOARD              39620200
396203     MOVE SCR06-YARD-ROAD            TO P06ECA-YARD-ROAD          39620300
396204     MOVE SCR06-VIEW-TIME            TO P06ECA-VIEW-TIME          39620400
396205     MOVE SCR06-TURN(FUNC-SUB)       TO P06ECA-TURN               39620500
396206     MOVE P06CA-EMP-NO(FUNC-SUB)     TO P06ECA-EMP-NO             39620600
396207     PERFORM P9150-TRANSFER-TO-SCHEDULE                           39620700
396208     .                                                            39620800
396210                                                                  39621000
396300*                                                                 39630000
396400 P8000-LOCATE-TAG.                                                39640000
396500*                                                                 39650000
396600     MOVE '0' TO WS-TAG-FOUND-FLAG                                39660000
396700     MOVE SPACES TO EBTURN                                        39670000
396800                    WS-TURN-WITH-TAG                              39680000
396900     MOVE SCR06-DIST TO DIST OF EBTURN                            39690000
397000     MOVE SCR06-SUB-DIST TO SUBDIST OF EBTURN                     39700000
397100     MOVE SCR06-CC TO CRAFT-CODE OF EBTURN                        39710000
397200     EXEC CICS STARTBR                                            39720000
397300               DATASET(EB-VIA-TURN-NBR)                           39730000
397400               RIDFLD(EBTURN)                                     39740000
397500               GTEQ                                               39750000
397600               RESP(WS-RESPONSE)                                  39760000
397700     END-EXEC                                                     39770000
397800     MOVE WS-RESPONSE TO FILE-STATUS                              39780000
397900     IF SUCCESS                                                   39790000
398000        MOVE '0' TO TAG-DONE-CODE                                 39800000
398100        PERFORM UNTIL TAG-DONE                                    39810000
398200           EXEC CICS READNEXT                                     39820000
398300                     DATASET(EB-VIA-TURN-NBR)                     39830000
398400                     INTO(WS-EXTRA-BOARD)                         39840000
398500                     LENGTH(EBTURNNO-RLGTH)                       39850000
398600                     RIDFLD(EBTURN)                               39860000
398700                     KEYLENGTH(EBTURNNO-KLGTH)                    39870000
398800                     RESP(WS-RESPONSE)                            39880000
398900           END-EXEC                                               39890000
399000           MOVE WS-RESPONSE TO FILE-STATUS                        39900000
399100           IF SUCCESS                                             39910000
399200              IF DIST-REPEAT = SCR06-DIST                         39920000
399300                 AND SUBDIST-REPEAT = SCR06-SUB-DIST              39930000
399400                 AND CRAFT-CODE-REPEAT = SCR06-CC                 39940000
399500                 IF TAG-YOUR-IT                                   39950000
399600                    SET TAG-DONE TO TRUE                          39960000
399700                    SET TAG-FOUND TO TRUE                         39970000
399800                    MOVE EBTURN-AREA TO WS-TURN-WITH-TAG          39980000
399900                 END-IF                                           39990000
400000              ELSE                                                40000000
400100                 SET TAG-DONE TO TRUE                             40010000
400200              END-IF                                              40020000
400300           ELSE                                                   40030000
400400              SET TAG-DONE TO TRUE                                40040000
400500              IF NOT (NO-RECORD-FND OR END-OF-FILE)               40050000
400600                 MOVE 'P8000-1' TO ERR-PARAGRAPH                  40060000
400700                 MOVE EBTURN    TO ERR-KEY                        40070000
400800                 PERFORM P9999-GOT-PROBLEM                        40080000
400900              END-IF                                              40090000
401000           END-IF                                                 40100000
401100        END-PERFORM                                               40110000
401200        EXEC CICS ENDBR                                           40120000
401300                  DATASET(EB-VIA-TURN-NBR)                        40130000
401400                  RESP(WS-RESPONSE)                               40140000
401500        END-EXEC                                                  40150000
401600     ELSE                                                         40160000
401700        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     40170000
401800           MOVE 'P8000-2' TO ERR-PARAGRAPH                        40180000
401900           MOVE EBTURN    TO ERR-KEY                              40190000
402000           PERFORM P9999-GOT-PROBLEM                              40200000
402100        END-IF                                                    40210000
402200     END-IF.                                                      40220000
402300*                                                                 40230000
402400 P8080-LINK-942.                                                  40240000
402500*                                                                 40250000
402600     EXEC CICS LINK                                               40260000
402700               PROGRAM(P942-PGM)                                  40270000
402800               COMMAREA(P942-COMMAREA-PARMS)                      40280000
402900               LENGTH(P942-LGTH)                                  40290000
403000               RESP(WS-RESPONSE)                                  40300000
403100     END-EXEC                                                     40310000
403200     MOVE WS-RESPONSE     TO FILE-STATUS                          40320000
403300     IF NOT SUCCESS                                               40330000
403400        MOVE 'P8080-1'    TO ERR-PARAGRAPH                        40340000
403500        MOVE 'P942LINK'   TO ERR-KEY                              40350000
403600        PERFORM P9999-GOT-PROBLEM                                 40360000
403700     END-IF.                                                      40370000
403800*                                                                 40380000
403900 P8200-REWRITE-EB.                                                40390000
404000*                                                                 40400000
404100     EXEC CICS REWRITE                                            40410000
404200               DATASET(EB-VIA-TURN-NBR)                           40420000
404300               FROM(WS-EXTRA-BOARD)                               40430000
404400               LENGTH(EBTURNNO-RLGTH)                             40440000
404500               RESP(WS-RESPONSE)                                  40450000
404600     END-EXEC                                                     40460000
404700     MOVE WS-RESPONSE TO FILE-STATUS.                             40470000
404800     IF SUCCESS                                                   40480000
404900        IF EB-POS-DATE-TIME(1:2) < '90' AND                       40490000
405000           EB-POS-DATE-TIME(3:2) > '00' AND                       40500000
405100           OK-TO-ZAP                                              40510000
405200           MOVE SPACES                    TO P914-COMMAREA-PARMS  40520000
405300           SET P914-ZAP-FUNCTION          TO TRUE                 40530000
405400           MOVE DIST OF EBPOS-AREA        TO P914-TURN-DIST       40540000
405500           MOVE SUB-DIST OF EBPOS-AREA    TO P914-TURN-SUB-DIST   40550000
405600           MOVE CRAFT-CODE OF EBPOS-AREA  TO P914-TURN-CC         40560000
405700           EXEC CICS LINK                                         40570000
405800                PROGRAM(P914-PGM)                                 40580000
405900                COMMAREA(P914-COMMAREA-PARMS)                     40590000
406000                LENGTH(P914-LGTH)                                 40600000
406100                RESP(WS-RESPONSE)                                 40610000
406200           END-EXEC                                               40620000
406300           MOVE WS-RESPONSE TO FILE-STATUS                        40630000
406400           IF NOT SUCCESS                                         40640000
406500              MOVE 'P8200-2' TO ERR-PARAGRAPH                     40650000
406600              PERFORM P9999-GOT-PROBLEM                           40660000
406700           END-IF                                                 40670000
406800           MOVE SPACE        TO WS-ZAP-FLAG                       40680000
406900        END-IF                                                    40690000
407000     END-IF.                                                      40700000
407100*                                                                 40710000
407200*                                                                 40720000
407300 P8500-READ-MASTER.                                               40730000
407400*                                                                 40740000
407500     EXEC CICS READ                                               40750000
407600               DATASET(MSTR-VIA-EMP-NBR)                          40760000
407700               INTO(WS-MSTR)                                      40770000
407800               LENGTH(MSTRENBR-RLGTH)                             40780000
407900               RIDFLD(MSTRNBRK)                                   40790000
408000               KEYLENGTH(MSTRENBR-KLGTH)                          40800000
408100               RESP(WS-RESPONSE)                                  40810000
408200     END-EXEC                                                     40820000
408300     MOVE WS-RESPONSE TO FILE-STATUS                              40830000
408400     IF SUCCESS                                                   40840000
408500        PERFORM P8510-READ-MASTER-JOBS                            40850000
408600     ELSE                                                         40860000
408700        MOVE 'P8500' TO ERR-PARAGRAPH                             40870000
408800        MOVE MSTRNBRK TO ERR-KEY                                  40880000
408900        PERFORM P9999-GOT-PROBLEM                                 40890000
409000     END-IF.                                                      40900000
409100*                                                                 40910000
409200 P8510-READ-MASTER-JOBS.                                          40920000
409300*                                                                 40930000
409400     MOVE SPACES                 TO WS-ASGN-FILE                  40940000
409500     MOVE EMP-NBR OF WS-MSTR     TO WK-ASGN-EMP-NO                40950000
409600     PERFORM PXXXX-JOB-OWNED                                      40960000
409700     MOVE ASGN-JOB-TYPE          TO NORMAL-ASGNMT-FLAG            40970000
409800     MOVE ASGN-ASSIGNMENT        TO NORMAL-ASGNMT                 40980000
409900     MOVE SPACES                 TO WS-ASGN-FILE                  40990000
410000     PERFORM PXXXX-LATEST-TEMP-JOB                                41000000
410100     MOVE ASGN-JOB-TYPE          TO TEMPORARY-ASGNMT-FLAG         41010000
410200     MOVE SPACES                 TO TEMP-ASGN-XB-AUG-FLAG         41020000
410300     IF ASGN-JOB-TYPE = 'X'                                       41030000
410400        AND AUGMENTED-TO-EXTRA-BOARD                              41040000
410500        SET TEMP-ASGN-XB-AUG    TO TRUE                           41050000
410600     END-IF                                                       41060000
410700     MOVE ASGN-ASSIGNMENT        TO TEMPORARY-ASGNMT              41070000
410800     MOVE SPACE                  TO WS-ASGN-FILE                  41080000
410900     PERFORM PXXXX-JOB-ON-DUTY                                    41090000
411000     MOVE ASGN-JOB-TYPE          TO ON-DUTY-ASGNMT-FLAG           41100000
411100     MOVE ASGN-ASSIGNMENT        TO ON-DUTY-ASGNMT                41110000
411200     MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE         41120000
411300     IF ASGN-ASSIGNMENT    > SPACE              AND               41130000
411400        ASGN-XB-PREFIX     = 'EX'               AND               41140000
411500        ASGN-AJ-JOB-NO NOT = ASGN-TRAIN-NO(1:6)                   41150000
411600        MOVE ASGN-TRAIN-NO(1:6)  TO OD-1                          41160000
411700        MOVE ASGN-TRAIN-CC       TO OD-CC                         41170000
411800     END-IF.                                                      41180000
411900                                                                  41190000
412000*************************************************************     41200000
412100 P8700-CALL-DATE-ROUTINE.                                         41210000
412200*                       NORTHERN QUEBEC SPAREBOARD - PHASE 2      41220000
412300*************************************************************     41230000
412400     EXEC CICS LINK                                               41240000
412500               PROGRAM(P903-PGM)                                  41250000
412600               COMMAREA(DATE-CONVERSION-PARMS)                    41260000
412700               LENGTH(P903-LGTH)                                  41270000
412800               RESP(WS-RESPONSE)                                  41280000
412900     END-EXEC                                                     41290000
413000     MOVE WS-RESPONSE     TO FILE-STATUS                          41300000
413100     IF NOT SUCCESS                                               41310000
413200        MOVE 'P8700-0'    TO ERR-PARAGRAPH                        41320000
413300        MOVE 'P903'       TO ERR-KEY                              41330000
413400        PERFORM P9999-GOT-PROBLEM                                 41340000
413500     END-IF.                                                      41350000
413600                                                                  41360000
413700                                                                  41370000
413800*                                                                 41380000
413900 P8800-GET-CURRENT-TIME.                                          41390000
414000*                                                                 41400000
414100     EXEC CICS ASKTIME                                            41410000
414200               ABSTIME(WS-ABSTIME)                                41420000
414300     END-EXEC                                                     41430000
414400*                                                                 41440000
414500     ADD WS-ABSTIME-OFFSET TO WS-ABSTIME                          41450000
414600*                                                                 41460000
414700     EXEC CICS FORMATTIME                                         41470000
414800               ABSTIME(WS-ABSTIME)                                41480000
414900               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      41490000
415000               TIME(WS-SYSTEM-TIME-AREA)                          41500000
415100     END-EXEC                                                     41510000
415200*                                                                 41520000
415300*    INSTALL APPLICATION DATE/TIME                                41530000
415400*                                                                 41540000
415500     IF PSTCA-DATE-TIME-OFFSET > SPACES                           41550000
415600        MOVE ZEROS          TO DATE-CONVERSION-PARMS              41560000
415700        MOVE WS-SYSTEM-DATE TO PARM-PRI-DATE-GREG                 41570000
415800        MOVE WS-SYSTEM-TIME TO PARM-PRI-HRMN                      41580000
415900        PERFORM P9810-PROCESS-OFFSET                              41590000
416000        MOVE PARM-RES-GREG-CENT  TO WS-SYSTEM-CENT                41600000
416100        MOVE PARM-RES-DATE-GREG  TO WS-SYSTEM-DATE                41610000
416200        MOVE PARM-RES-HRMN       TO WS-SYSTEM-TIME                41620000
416300     END-IF                                                       41630000
416400*    SET DE-YYMMDD-FORMAT   TO TRUE                               41640000
416500*    MOVE WS-SYSTEM-DATE    TO DE-YYMMDD                          41650000
416600*    PERFORM P8998-DATEEDIT                                       41660000
416700*    MOVE DE-YYMMDD-CE      TO WS-SYSTEM-CENT                     41670000
416800*                                                                 41680000
416900*    CONVERT SYSTEM TIME TO LOCAL TIME                            41690000
417000*                                                                 41700000
417100     MOVE SPACES            TO TZ-PARAMETERS                      41710000
417200     SET TZ-IN-EASTERN-ZONE TO TRUE                               41720000
417300*    MOVE PRESENT-TIME      TO TZ-IN-DATE-TIME                    41730000
417400     MOVE WS-PRESENT-TIME   TO TZ-IN-DATE-TIME                    41740000
417500     MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                        41750000
417600     PERFORM P8996-TIMEZONE                                       41760000
417700     MOVE TZ-OUT-DATE-TIME-CENT  TO WS-LOCAL-DATE-TIME-CENT.      41770000
417800*    MOVE TZ-OUT-CE         TO WS-LOCAL-CENT.                     41780000
417900*                                                                 41790000
418000 P8900-WRITE-HISTORY.                                             41800000
418100*                                                                 41810000
418200     MOVE WORK-HIST-TIME        TO P943-CLOCK-TIME                41820000
418300     SET P943-EMPLOYEE-FUNCTION TO TRUE                           41830000
418400     MOVE PSTCA-TIME-ZONE       TO P943-EMP-TIME-ZONE             41840000
418500     IF P943-EFF-DATE-TIME NOT > SPACES                           41850000
418600        MOVE WS-LOCAL-DATE-TIME TO P943-EFF-DATE-TIME             41860000
418700     END-IF                                                       41870000
418800     EXEC CICS ASSIGN                                             41880000
418900               USERID(P943-USERID)                                41890000
419000     END-EXEC                                                     41900000
419100                                                                  41910000
419200     EXEC CICS LINK                                               41920000
419300               PROGRAM(P943-PGM)                                  41930000
419400               COMMAREA(P943-COMMAREA-PARMS)                      41940000
419500               LENGTH(P943-LGTH)                                  41950000
419600               RESP(WS-RESPONSE)                                  41960000
419700     END-EXEC                                                     41970000
419800     MOVE WS-RESPONSE           TO FILE-STATUS                    41980000
419900                                                                  41990000
420000     IF NOT SUCCESS                                               42000000
420100        MOVE 'P8900'            TO ERR-PARAGRAPH                  42010000
420200        MOVE 'P943'             TO ERR-KEY                        42020000
420300        PERFORM P9999-GOT-PROBLEM                                 42030000
420400     END-IF                                                       42040000
420500     ADD 1                      TO WORK-HIST-TIME.                42050000
420600*                                                                 42060000
420700 PXXXX-JOB-OWNER.                                                 42070000
420800*                                                                 42080000
420900     MOVE WORK-ASGNKEY1 TO ASGNKEY1                               42090000
421000     SET ASGN-OWNER-REC TO TRUE                                   42100000
421100     MOVE ZERO          TO ASGN-DATE-TIME                         42110000
421200     MOVE ASGNKEY1      TO ASGNJOB                                42120000
421300     EXEC CICS READ                                               42130000
421400               DATASET(ASGN-VIA-ASGNJOB)                          42140000
421500               INTO(ASGN-AREA)                                    42150000
421600               LENGTH(ASGNJOB-RLGTH)                              42160000
421700               RIDFLD(ASGNJOB)                                    42170000
421800               KEYLENGTH(ASGNJOB-KLGTH)                           42180000
421900               RESP(WS-RESPONSE)                                  42190000
422000     END-EXEC                                                     42200000
422100     MOVE WS-RESPONSE TO FILE-STATUS                              42210000
422200     IF NOT SUCCESS                                               42220000
422300        MOVE ZEROS TO ASGN-EMP-NO                                 42230000
422400     END-IF.                                                      42240000
422500*                                                                 42250000
422600 PXXXX-LATEST-TEMP.                                               42260000
422700*                                                                 42270000
422800     MOVE SPACES        TO WS-SAVE-ASGN-FILE                      42280000
422900     MOVE WORK-ASGNKEY1 TO ASGNKEY1                               42290000
423000     SET ASGN-TEMP-REC  TO TRUE                                   42300000
423100     MOVE ZERO          TO ASGN-DATE-TIME                         42310000
423200     MOVE ASGNKEY1      TO ASGNJOB                                42320000
423300     EXEC CICS STARTBR                                            42330000
423400               DATASET(ASGN-VIA-ASGNJOB)                          42340000
423500               RIDFLD(ASGNJOB)                                    42350000
423600               GTEQ                                               42360000
423700               RESP(WS-RESPONSE)                                  42370000
423800     END-EXEC                                                     42380000
423900     MOVE WS-RESPONSE TO FILE-STATUS                              42390000
424000     IF SUCCESS                                                   42400000
424100        MOVE 'N' TO WS-ASGN-DONE-CODE                             42410000
424200        PERFORM UNTIL ASGN-DONE                                   42420000
424300           EXEC CICS READNEXT                                     42430000
424400                     DATASET(ASGN-VIA-ASGNJOB)                    42440000
424500                     INTO(ASGN-AREA)                              42450000
424600                     LENGTH(ASGNJOB-RLGTH)                        42460000
424700                     RIDFLD(ASGNJOB)                              42470000
424800                     KEYLENGTH(ASGNJOB-KLGTH)                     42480000
424900                     RESP(WS-RESPONSE)                            42490000
425000           END-EXEC                                               42500000
425100           MOVE WS-RESPONSE TO FILE-STATUS                        42510000
425200           IF SUCCESS                                             42520000
425300              IF WK-ASGN-DIST = ASGN-DIST                         42530000
425400                 AND WK-ASGN-SUB-DIST = ASGN-SUB-DIST             42540000
425500                 AND WK-SWASSGN-ASGN = ASGN-AJ-JOB                42550000
425600                     OF ASGN-ASSIGNMENT                           42560000
425700                 AND ASGN-TEMP-REC                                42570000
425800                 MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE              42580000
425900              ELSE                                                42590000
426000                 SET ASGN-DONE TO TRUE                            42600000
426100              END-IF                                              42610000
426200           ELSE                                                   42620000
426300              SET ASGN-DONE TO TRUE                               42630000
426400           END-IF                                                 42640000
426500        END-PERFORM                                               42650000
426600        EXEC CICS ENDBR                                           42660000
426700                  DATASET(ASGN-VIA-ASGNJOB)                       42670000
426800                  RESP(WS-RESPONSE)                               42680000
426900        END-EXEC                                                  42690000
427000     END-IF                                                       42700000
427100     IF WS-SAVE-ASGN-FILE > SPACE                                 42710000
427200        MOVE WS-SAVE-ASGN-FILE TO ASGN-AREA                       42720000
427300     ELSE                                                         42730000
427400        MOVE ZEROS TO ASGN-EMP-NO                                 42740000
427500     END-IF.                                                      42750000
427600*                                                                 42760000
427700 PXXXX-ON-DUTY-EMP.                                               42770000
427800*                                                                 42780000
427900     MOVE WORK-ASGNKEY1   TO ASGNKEY1                             42790000
428000     SET ASGN-ON-DUTY-REC TO TRUE                                 42800000
428100     MOVE ZERO            TO ASGN-DATE-TIME                       42810000
428200     MOVE ASGNKEY1        TO ASGNJOB                              42820000
428300     EXEC CICS READ                                               42830000
428400               DATASET(ASGN-VIA-ASGNJOB)                          42840000
428500               INTO(ASGN-AREA)                                    42850000
428600               LENGTH(ASGNJOB-RLGTH)                              42860000
428700               RIDFLD(ASGNJOB)                                    42870000
428800               KEYLENGTH(ASGNJOB-KLGTH)                           42880000
428900               RESP(WS-RESPONSE)                                  42890000
429000     END-EXEC                                                     42900000
429100     MOVE WS-RESPONSE TO FILE-STATUS                              42910000
429200     IF NOT SUCCESS                                               42920000
429300        MOVE ZEROS TO ASGN-EMP-NO                                 42930000
429400     END-IF.                                                      42940000
429500*                                                                 42950000
429600 PXXXX-JOB-OWNED.                                                 42960000
429700*                                                                 42970000
429800                                                                  42980000
429900     MOVE WORK-ASGNKEY2 TO ASGNKEY2                               42990000
430000     MOVE '1'           TO ASGN-EMP-NO-REC-TYPE                   43000000
430100     MOVE ZERO          TO ASGN-EMP-DATE-TIME                     43010000
430200     MOVE ASGNKEY2      TO ASGNEMP                                43020000
430300     EXEC CICS READ                                               43030000
430400               DATASET(ASGN-VIA-ASGNEMP)                          43040000
430500               INTO(WS-ASGN-FILE)                                 43050000
430600               LENGTH(ASGNEMP-RLGTH)                              43060000
430700               RIDFLD(ASGNEMP)                                    43070000
430800               KEYLENGTH(ASGNEMP-KLGTH)                           43080000
430900               RESP(WS-RESPONSE)                                  43090000
431000     END-EXEC                                                     43100000
431100     MOVE WS-RESPONSE TO FILE-STATUS                              43110000
431200     IF NOT SUCCESS                                               43120000
431300        MOVE SPACES TO WS-ASGN-FILE                               43130000
431400     END-IF.                                                      43140000
431500*                                                                 43150000
431600 PXXXX-LATEST-TEMP-JOB.                                           43160000
431700*                                                                 43170000
431800     MOVE WORK-ASGNKEY2 TO ASGNKEY2                               43180000
431900     MOVE '2'           TO ASGN-EMP-NO-REC-TYPE                   43190000
432000     MOVE ZERO          TO ASGN-EMP-DATE-TIME                     43200000
432100     MOVE ASGNKEY2      TO ASGNEMP                                43210000
432200     MOVE SPACES        TO WS-ASGN-FILE                           43220000
432300                           WS-SAVE-ASGN-FILE                      43230000
432400     EXEC CICS STARTBR                                            43240000
432500               DATASET(ASGN-VIA-ASGNEMP)                          43250000
432600               RIDFLD(ASGNEMP)                                    43260000
432700               GTEQ                                               43270000
432800               RESP(WS-RESPONSE)                                  43280000
432900     END-EXEC                                                     43290000
433000     MOVE WS-RESPONSE TO FILE-STATUS                              43300000
433100     IF SUCCESS                                                   43310000
433200        MOVE 'N' TO WS-ASGN-DONE-CODE                             43320000
433300        PERFORM UNTIL ASGN-DONE                                   43330000
433400           EXEC CICS READNEXT                                     43340000
433500                     DATASET(ASGN-VIA-ASGNEMP)                    43350000
433600                     INTO(ASGN-AREA)                              43360000
433700                     LENGTH(ASGNEMP-RLGTH)                        43370000
433800                     RIDFLD(ASGNEMP)                              43380000
433900                     KEYLENGTH(ASGNEMP-KLGTH)                     43390000
434000                     RESP(WS-RESPONSE)                            43400000
434100           END-EXEC                                               43410000
434200           MOVE WS-RESPONSE TO FILE-STATUS                        43420000
434300           IF SUCCESS                                             43430000
434400              IF ASGN-EMP-NO = WK-ASGN-EMP-NO                     43440000
434500                 AND ASGN-EMP-NO-REC-TYPE = '2'                   43450000
434600                 MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE              43460000
434700              ELSE                                                43470000
434800                 SET ASGN-DONE TO TRUE                            43480000
434900              END-IF                                              43490000
435000           ELSE                                                   43500000
435100              SET ASGN-DONE TO TRUE                               43510000
435200           END-IF                                                 43520000
435300        END-PERFORM                                               43530000
435400        EXEC CICS ENDBR                                           43540000
435500                  DATASET(ASGN-VIA-ASGNEMP)                       43550000
435600                  RESP(WS-RESPONSE)                               43560000
435700        END-EXEC                                                  43570000
435800     END-IF                                                       43580000
435900     IF WS-SAVE-ASGN-FILE > SPACES                                43590000
436000        MOVE WS-SAVE-ASGN-FILE TO WS-ASGN-FILE                    43600000
436100     ELSE                                                         43610000
436200        MOVE SPACES TO WS-ASGN-FILE                               43620000
436300     END-IF.                                                      43630000
436400*                                                                 43640000
436500 PXXXX-JOB-ON-DUTY.                                               43650000
436600*                                                                 43660000
436700     MOVE WORK-ASGNKEY2 TO ASGNKEY2                               43670000
436800     MOVE '3' TO ASGN-EMP-NO-REC-TYPE                             43680000
436900     MOVE ZERO TO ASGN-EMP-DATE-TIME                              43690000
437000     MOVE ASGNKEY2 TO ASGNEMP                                     43700000
437100     EXEC CICS READ                                               43710000
437200               DATASET(ASGN-VIA-ASGNEMP)                          43720000
437300               INTO(ASGN-AREA)                                    43730000
437400               LENGTH(ASGNEMP-RLGTH)                              43740000
437500               RIDFLD(ASGNEMP)                                    43750000
437600               KEYLENGTH(ASGNEMP-KLGTH)                           43760000
437700               RESP(WS-RESPONSE)                                  43770000
437800     END-EXEC                                                     43780000
437900     MOVE WS-RESPONSE TO FILE-STATUS                              43790000
438000     IF NOT SUCCESS                                               43800000
438100        MOVE SPACES TO WS-ASGN-FILE                               43810000
438200     END-IF.                                                      43820000
438300*                                                                 43830000
438400 COPY TIMEZONE.                                                   43840000
438500*                                                                 43850000
438600 COPY DATEEDIT.                                                   43860000
438700*                                                                 43870000
438800 COPY BIFEDIT.                                                    43880000
438900*                                                                 43890000
439000*                                                                 43900000
439100 PXXXX-SCREEN-SECURITY.                                           43910000
439200*                                                                 43920000
439300     EXEC CICS ASSIGN                                             43930000
439400               USERID(PT48-USERID)                                43940000
439500     END-EXEC                                                     43950000
439600     MOVE 'PSTS06  '  TO PT48-SCREEN                              43960000
439700     MOVE PF-CHECK    TO PT48-KEY-USED                            43970000
439800     MOVE 'N'         TO PT48-UPDATE-FLAG                         43980000
439900     EXEC CICS LINK                                               43990000
440000               PROGRAM(PT48-PGM)                                  44000000
440100               COMMAREA(PT48-COMMAREA)                            44010000
440200               LENGTH(PT48-LGTH)                                  44020000
440300               RESP(WS-RESPONSE)                                  44030000
440400     END-EXEC                                                     44040000
440500 COPY PRCD06.                                                     44050000
440600*                                                                 44060000
440700 P9000-SEND-MAP-AND-RETURN.                                       44070000
440800*                                                                 44080000
440900     IF MSGLOG-CODE > SPACES                                      44090000
441000         PERFORM P9030-GET-MESSAGE                                44100000
441100         MOVE MSGLOG-MESSAGE-AREA TO SCR06-ERRORMSG               44110000
441200     END-IF                                                       44120000
441300                                                                  44130000
441400     MOVE P06-MAP-VERSION(PSTCA-SUB) TO P06-MAP                   44140000
441500     IF CREATE-SCREEN                                             44150000
441600        PERFORM P9010-SEND-PHYSICAL-MAP                           44160000
441700     ELSE                                                         44170000
441800        IF CONTINUE-SCREEN                                        44180000
441900           PERFORM P9020-SEND-DATAONLY-MAP                        44190000
442000        ELSE                                                      44200000
442100           PERFORM P9035-SEND-BUFFER                              44210000
442200        END-IF                                                    44220000
442300     END-IF                                                       44230000
442400     EXEC CICS RETURN                                             44240000
442500               TRANSID(P06-TRAN)                                  44250000
442600               COMMAREA(PSTCOMM-AREA)                             44260000
442700               LENGTH(P06-COMM-LGTH)                              44270000
442800     END-EXEC.                                                    44280000
442900*                                                                 44290000
443000 P9010-SEND-PHYSICAL-MAP.                                         44300000
443100*                                                                 44310000
443200     EXEC CICS SEND MAP(P06-MAP)                                  44320000
443300                    MAPSET(P06-SET)                               44330000
443400                    FROM(PSTS06)                                  44340000
443500                    CURSOR                                        44350000
443600                    ERASE                                         44360000
443700                    RESP(WS-RESPONSE)                             44370000
443800     END-EXEC                                                     44380000
443900     MOVE WS-RESPONSE TO FILE-STATUS                              44390000
444000     IF NOT SUCCESS                                               44400000
444100        MOVE 'P9010'   TO ERR-PARAGRAPH                           44410000
444200        PERFORM P9999-GOT-PROBLEM                                 44420000
444300     END-IF.                                                      44430000
444400*                                                                 44440000
444500 P9020-SEND-DATAONLY-MAP.                                         44450000
444600*                                                                 44460000
444700     EXEC CICS SEND MAP(P06-MAP)                                  44470000
444800                    MAPSET(P06-SET)                               44480000
444900                    FROM(PSTS06)                                  44490000
445000                    DATAONLY                                      44500000
445100                    CURSOR                                        44510000
445200                    RESP(WS-RESPONSE)                             44520000
445300     END-EXEC                                                     44530000
445400     MOVE WS-RESPONSE TO FILE-STATUS                              44540000
445500     IF NOT SUCCESS                                               44550000
445600        MOVE 'P9020' TO ERR-PARAGRAPH                             44560000
445700        PERFORM P9999-GOT-PROBLEM                                 44570000
445800     END-IF.                                                      44580000
445900*                                                                 44590000
446000 P9030-GET-MESSAGE.                                               44600000
446100*                                                                 44610000
446200     MOVE PSTCA-SUB TO MSGLOG-SUB-CODE                            44620000
446300     EXEC CICS READ                                               44630000
446400               DATASET(MSGLOG-VIA-CODE)                           44640000
446500               INTO(MSGLOG-AREA)                                  44650000
446600               LENGTH(MSGLOG-RLGTH)                               44660000
446700               RIDFLD(MSGLOG-KEY)                                 44670000
446800               KEYLENGTH(MSGLOG-KLGTH)                            44680000
446900               RESP(WS-RESPONSE)                                  44690000
447000     END-EXEC                                                     44700000
447100     MOVE WS-RESPONSE TO FILE-STATUS                              44710000
447200     IF NOT SUCCESS                                               44720000
447300        IF PSTCA-SUB = 1                                          44730000
447400           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            44740000
447500        ELSE                                                      44750000
447600           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            44760000
447700        END-IF                                                    44770000
447800     END-IF                                                       44780000
447900     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE                      44790000
448000     MOVE '-'             TO MSGLOG-MSG-SEP                       44800000
448100     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.                 44810000
448200*                                                                 44820000
448300 P9035-SEND-BUFFER.                                               44830000
448400*                                                                 44840000
448500     EXEC CICS SEND                                               44850000
448600               FROM(WS-BUFFER-DATA)                               44860000
448700               LENGTH(WS-BUFFER-LGTH)                             44870000
448800               ERASE                                              44880000
448900               RESP(WS-RESPONSE)                                  44890000
449000     END-EXEC                                                     44900000
449100     MOVE WS-RESPONSE       TO FILE-STATUS                        44910000
449200     IF NOT SUCCESS                                               44920000
449300        MOVE 'P9035-1'      TO ERR-PARAGRAPH                      44930000
449400        MOVE 'SEND BUFFER'  TO ERR-KEY                            44940000
449500        PERFORM P9999-GOT-PROBLEM                                 44950000
449600     END-IF                                                       44960000
449700     EXEC CICS SEND                                               44970000
449800               CONTROL                                            44980000
449900               CURSOR(WS-BUFFER-CURSOR)                           44990000
450000               RESP(WS-RESPONSE)                                  45000000
450100     END-EXEC                                                     45010000
450200     MOVE WS-RESPONSE       TO FILE-STATUS                        45020000
450300     IF NOT SUCCESS                                               45030000
450400        MOVE 'P9035-2'      TO ERR-PARAGRAPH                      45040000
450500        MOVE 'SEND CURSOR'  TO ERR-KEY                            45050000
450600        PERFORM P9999-GOT-PROBLEM                                 45060000
450700     END-IF.                                                      45070000
450800*                                                                 45080000
450900 P9100-SETUP-SCR03.                                               45090000
451000*                                                                 45100000
451100     MOVE SPACES TO PSTCA-VARIABLE-AREA                           45110000
451200     EXEC CICS XCTL                                               45120000
451300               PROGRAM(P03-PGM)                                   45130000
451400               COMMAREA(PSTCOMM-AREA)                             45140000
451500               LENGTH(PSTCOMM-LGTH)                               45150000
451600               RESP(WS-RESPONSE)                                  45160000
451700     END-EXEC                                                     45170000
451800     MOVE WS-RESPONSE TO FILE-STATUS                              45180000
451900     IF NOT SUCCESS                                               45190000
452000        MOVE 'P9100' TO ERR-PARAGRAPH                             45200000
452100        PERFORM P9999-GOT-PROBLEM                                 45210000
452200     END-IF.                                                      45220000
452300*                                                                 45230000
452400 P9150-TRANSFER-TO-SCHEDULE.                                      45240000
452500*                                                                 45250000
452510     IF CNTL-XB-SCHEDULED                                         45251000
452520*                                                                 45252000
452600*       TRANSFER TO CNP06S TO ALLOW MAINTENANCE OF TURN SCHEDULE. 45260000
452700*                                                          CW0996 45270000
452800        EXEC CICS XCTL                                            45280000
452900                  PROGRAM(P06S-PGM)                               45290000
453000                  COMMAREA(PSTCOMM-AREA)                          45300000
453100                  LENGTH(PSTCOMM-LGTH)                            45310000
453200                  RESP(WS-RESPONSE)                               45320000
453300        END-EXEC                                                  45330000
453400        MOVE WS-RESPONSE                TO FILE-STATUS            45340000
453500        IF NOT SUCCESS                                            45350000
453600           MOVE 'P9150-1'               TO ERR-PARAGRAPH          45360000
453700           PERFORM P9999-GOT-PROBLEM                              45370000
453800        END-IF                                                    45380000
453801     ELSE                                                         45380100
453810*                                                                 45381000
453820*       TRANSFER TO CNP06E TO ALLOW MAINTENANCE OF EXTENDED TURN  45382000
453821*       SCHEDULE.                                                 45382100
453830*                                                                 45383000
453840        EXEC CICS XCTL                                            45384000
453850                  PROGRAM(P06E-PGM)                               45385000
453860                  COMMAREA(PSTCOMM-AREA)                          45386000
453870                  LENGTH(PSTCOMM-LGTH)                            45387000
453880                  RESP(WS-RESPONSE)                               45388000
453890        END-EXEC                                                  45389000
453891        MOVE WS-RESPONSE                TO FILE-STATUS            45389100
453892        IF NOT SUCCESS                                            45389200
453893           MOVE 'P9150-2'               TO ERR-PARAGRAPH          45389300
453894           PERFORM P9999-GOT-PROBLEM                              45389400
453895        END-IF                                                    45389500
453896     END-IF                                                       45389600
453900     .                                                            45390000
454000*                                                                 45400000
454100 P9500-SETUP-SCR998.                                              45410000
454200*                                                                 45420000
454300     MOVE SPACES            TO P998COMM-AREA                      45430000
454400     MOVE P06-PGM           TO P998CA-FROM-PROGRAM                45440000
454500     MOVE P06-MAP           TO P998CA-SCREEN-ID                   45450000
454600     MOVE EIBCPOSN          TO P998CA-CURSOR-POS                  45460000
454700     EXEC CICS XCTL                                               45470000
454800               PROGRAM(P998-PGM)                                  45480000
454900               COMMAREA(PSTCOMM-AREA)                             45490000
455000               LENGTH(PSTCOMM-LGTH)                               45500000
455100               RESP(WS-RESPONSE)                                  45510000
455200     END-EXEC                                                     45520000
455300     MOVE WS-RESPONSE       TO FILE-STATUS                        45530000
455400     IF NOT SUCCESS                                               45540000
455500        MOVE 'P9500'        TO ERR-PARAGRAPH                      45550000
455600        PERFORM P9999-GOT-PROBLEM                                 45560000
455700     END-IF.                                                      45570000
455800*                                                                 45580000
455900 P9600-SETUP-SCR997.                                              45590000
456000*                                                                 45600000
456100     MOVE SPACES                 TO PSTCA-VARIABLE-AREA           45610000
456200     MOVE P06-PGM                TO P997CA-FROM-PROGRAM           45620000
456300     SET P997CA-EXTRABOARD       TO TRUE                          45630000
456400     MOVE CNTL-FILE-VIA-CNTLKEY  TO P997CA-FILE                   45640000
456500     MOVE CNTLFILE-RLGTH         TO P997CA-RLGTH                  45650000
456600     MOVE CNTLFILE-KLGTH         TO P997CA-KLGTH                  45660000
456700     MOVE '08'                   TO P997CA-PASS-KEY(1:2)          45670000
456800     MOVE SCR06-DIST             TO P997CA-PASS-KEY(3:2)          45680000
456900     MOVE SCR06-SUB-DIST         TO P997CA-PASS-KEY(5:2)          45690000
457000     MOVE SCR06-CC               TO P997CA-PASS-KEY(7:2)          45700000
457100     EXEC CICS XCTL                                               45710000
457200               PROGRAM(P997-PGM)                                  45720000
457300               COMMAREA(PSTCOMM-AREA)                             45730000
457400               LENGTH(PSTCOMM-LGTH)                               45740000
457500               RESP(WS-RESPONSE)                                  45750000
457600     END-EXEC                                                     45760000
457700     MOVE WS-RESPONSE          TO FILE-STATUS                     45770000
457800     IF NOT SUCCESS                                               45780000
457900         MOVE 'P9600'          TO ERR-PARAGRAPH                   45790000
458000         PERFORM P9999-GOT-PROBLEM                                45800000
458100     END-IF.                                                      45810000
458200*                                                                 45820000
458300 P9810-PROCESS-OFFSET.                                            45830000
458400*                                                                 45840000
458500     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE                 45850000
458600     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY            45860000
458700     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN                  45870000
458800     PERFORM P8700-CALL-DATE-ROUTINE                              45880000
458900     .                                                            45890000
459000*                                                                 45900000
459100 P9820-SNAPSHOT-XB.                                               45910000
459200*                                                                 45920000
459300     MOVE SPACES                      TO P913-COMMAREA-PARMS      45930000
459400     SET P913-SNAPSHOT-FUNCTION       TO TRUE                     45940000
459500     MOVE SCR06-DIST                  TO P913-TURN-DIST           45950000
459600     MOVE SCR06-SUB-DIST              TO P913-TURN-SUB-DIST       45960000
459700     MOVE WS-LOCAL-DATE-TIME          TO P913-EFF-DATE-TIME       45970000
459800     MOVE PSTCA-TIME-ZONE             TO P913-TIME-ZONE           45980000
459900     MOVE SCR06-CC                    TO P913-TURN-CC             45990000
460000     EXEC CICS LINK                                               46000000
460100               PROGRAM(P913-PGM)                                  46010000
460200               COMMAREA(P913-COMMAREA-PARMS)                      46020000
460300               LENGTH(P913-LGTH)                                  46030000
460400               RESP(WS-RESPONSE)                                  46040000
460500     END-EXEC                                                     46050000
460600     MOVE WS-RESPONSE                 TO FILE-STATUS              46060000
460700     IF NOT SUCCESS                                               46070000
460800        MOVE 'P9820-1'                TO ERR-PARAGRAPH            46080000
460900        MOVE 'P913LINK'               TO ERR-KEY                  46090000
461000        PERFORM P9999-GOT-PROBLEM                                 46100000
461100     END-IF.                                                      46110000
461200*                                                                 46120000
461300 P9830-RETRIEVE-CNTL-INFO.                                        46130000
461400*                                                                 46140000
461500     MOVE SPACES                     TO P956-COMMAREA-PARMS       46150000
461600     MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE          46160000
461700     SET P956-GET-CNTL-STATUS-REASON TO TRUE                      46170000
461800     MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE          46180000
461900     MOVE DIST     OF WS-MSTR        TO P956-DIST                 46190000
462000     MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST                46200000
462100     MOVE CRAFT OF WS-MSTR           TO P956-CC                   46210000
462200     IF TEMPORARY-ASGNMT > SPACE                                  46220000
462300        MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE            46230000
462400        MOVE TA-1                    TO P956-ASGN                 46240000
462500        MOVE TA-DIST                 TO P956-DIST                 46250000
462600        MOVE TA-SUB-DIST             TO P956-SDIST                46260000
462700        IF TEMP-ASGN-XB                                           46270000
462800           MOVE TA-CC                TO P956-XB                   46280000
462900        END-IF                                                    46290000
463000     ELSE                                                         46300000
463100        IF NORMAL-ASGNMT > SPACES                                 46310000
463200           MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE            46320000
463300           MOVE NA-1                 TO P956-ASGN                 46330000
463400           MOVE NA-DIST              TO P956-DIST                 46340000
463500           MOVE NA-SUB-DIST          TO P956-SDIST                46350000
463600           IF NORM-ASGN-XB                                        46360000
463700              MOVE NA-CC             TO P956-XB                   46370000
463800           END-IF                                                 46380000
463900        END-IF                                                    46390000
464000     END-IF                                                       46400000
464100     EXEC CICS LINK                                               46410000
464200               PROGRAM (P956-PGM)                                 46420000
464300               COMMAREA(P956-COMMAREA-PARMS)                      46430000
464400               LENGTH  (P956-LGTH)                                46440000
464500               RESP    (WS-RESPONSE)                              46450000
464600     END-EXEC                                                     46460000
464700     MOVE WS-RESPONSE           TO FILE-STATUS                    46470000
464800     IF NOT SUCCESS                                               46480000
464900        MOVE 'P9830-1'          TO ERR-PARAGRAPH                  46490000
465000        MOVE P956-INPUT-PARMS   TO ERR-KEY                        46500000
465100        PERFORM P9999-GOT-PROBLEM                                 46510000
465200     END-IF                                                       46520000
465300     IF P956-ST-RSN-DISP-WO-TRACKING                              46530000
465400        SET DISPLAY-EMP         TO TRUE                           46540000
465500     END-IF.                                                      46550000
465600*                                                                 46560000
465700 P9990-CLEAR-SCREEN.                                              46570000
465800*                                                                 46580000
465900     EXEC CICS SEND CONTROL                                       46590000
466000                    ERASE                                         46600000
466100                    FREEKB                                        46610000
466200     END-EXEC                                                     46620000
466300     EXEC CICS RETURN END-EXEC.                                   46630000
466400*                                                                 46640000
466500 P9999-GOT-PROBLEM.                                               46650000
466600*                                                                 46660000
466700     MOVE P06-PGM  TO ERR-PROGRAM                                 46670000
466800     MOVE DFHEIBLK TO ERR-EIBLK                                   46680000
466900     EXEC CICS XCTL                                               46690000
467000               PROGRAM(PSTERR-PGM)                                46700000
467100               COMMAREA(PSTERAR-AREA)                             46710000
467200               LENGTH(PSTERAR-LGTH)                               46720000
467300               RESP(WS-RESPONSE)                                  46730000
467400     END-EXEC                                                     46740000
467500     EXEC CICS ABEND                                              46750000
467600               ABCODE(PSTERR-ABCODE)                              46760000
467700               CANCEL                                             46770000
467800     END-EXEC.                                                    46780000
467900*                                                                 46790000
468000 X9999-GOBACK.                                                    46800000
468100     GOBACK.                                                      46810000
468200*                                                                 46820000
