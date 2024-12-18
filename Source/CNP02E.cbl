000100 IDENTIFICATION DIVISION.                                         00010071
000200 PROGRAM-ID.  CNP02E.                                             00020071
000300***************************************************************** 00030071
000400*                  FIELD INQURY UFP MAINTENANCE                   00040071
000500***************************************************************** 00050071
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060071
000700*-------- ------- ------  --------------------------------------- 00070071
000800*11/15/91   DBP           COPIED FROM CNP15X                      00080071
000900*09/23/92   ERW           CHANGED BROWSE BY POSITION TO GTEQ READ.00090071
001000*06/10/94   DLO           DISPLAY NAME OF OWNER OF TURN           00100071
001100*01/19/95   GJK           CONVERT EMP-PERS-REST TO CORRECT TIME   00110071
001200*06/17/96   TGJ   C-294   ADDED MOVE OF 0 TO ASSOCIATED-TURN-FLAG 00120071
001300*                         IN P1060 BEFORE PROCESSING TURNS.       00130071
001400*07/24/96   FLW  CNC0023  ADDED OPTION TO DISPLAY BY CRAFT CODE   00140071
001500*08/08/96   RCW  CNC0021  DISPLAY RETURN DATE ON OFF MILES AND    00150071
001600*                CNC0022  VACATIONS.                              00160071
001700*04/03/98   AMF           ADDED HELP LOGIC.                       00170071
001800*05/06/98   LPS           Y2K MODIFICATIONS FOR THE TASK LIST.    00180071
001900*07/28/98   GBJ           YEAR 2000 SWEEP.                        00190071
002000*09/21/98   RDH  C442     ENLARGE SELECTION TO 3 DIGITS.          00200071
002100*02/24/99   NJB           DISPLAY ALL ON BOARD 12/31/99 & 01/01/0000210071
002200*04/23/99   AJK  CNC0228  ADDED SNAPSHOT LOGIC.                   00220071
002300*12/11/99   MOO  CNC0183  ADDED CHECK FOR MTOY & MTOR.            00230071
002400*03/23/00   AJK  CNC0275  ADD POOL TYPE LOGIC.                    00240071
002500*05/03/00   AJK  CNC0183  REMOVE CHECK FOR MTOR & MTOY.           00250071
002600*05/15/01   AJK  CNC252B  ADD CALL ORDER LOGIC FOR SCHEDULED POOLS00260071
002700*10/15/01   AJK           ADD 'SCHEDULE OVERLAP' CHECK.  ONLY     00270071
002800*                         DISPLAY WINDOWS FROM CURRENT CYCLE.     00280071
002900*10/06/04   KJS  CNC0386A ADD DAY ASSOCIATED WITH PERSONAL REST   00290071
003000*07/29/05   AJK  C616     CORRECT CALL ORDER TIE BREAK LOGIC TO   00300071
003100*                         SORT BY BOARD DATE/TIME INSTEAD OF TURN 00310071
003200*                         ID.                                     00320071
003300*04/12/06   MET  CNC0398  TIME OFF                                00330071
003400*02/10/09   JES  CNC0436B RECOMPILE ONLY FOR WSTASK EXPANSION.    00340071
003500*03/12/09   JES           CHECK STATUS-REASON WHEN OFF-MILES      00350071
003600*                         TO DETERMINE RETURN DATE DISPLAY.       00360071
003700*04/27/10   AXK  CNC0492  ALWAYS CHECK FOR A DUEBACK/BOOK-ON TASK 00370071
003800*                         AND IF EXISTS, DISPLAY RETURN DATE/TIME.00380071
003900*                         PERFORMING P4190-SET-RETURN-DATE.       00390071
004000*                         THIS IS ONLY FOR OTHER LAYOFF CODES.    00400071
004100*02/27/12   SXJ  CNC0510  DO NOT SET NOT-FROM-CURRENT-CYCLE WHEN  00410071
004200*                         SCHEDULE WINDOW STARTS ON THE FIRST DAY 00420071
004300*                         OF THE NEXT BIDPACK SCHEDULE CYCLE.     00430071
004400*07/14/14   MFO  CNC0556  ADD REASON CODE                         00440071
004500*03/27/15   MFO  CNC0564A ADD STARTING AT FIELD FOR BIDPK TIEBRK  00450071
004600*07/22/15   NJB  CNC0568  CHANGES FOR NON-VACANCY DISPLAY         00460085
004700*10/20/15   MFO  C1104    EDIT BIDPACK DISPLAY TO FACTOR LEAD TIME00470087
004800*                         DON'T PRESENT DUPLICATE TURNS IF DAILY  00480089
004900*01/11/16   MFO  CNC0573  MASK STATUS/REASON FIELDS               00490090
005000*02/23/16   MFO  CNC0576  MASK STATUS/REASON FIELDS - HOLD TURN.  00500091
005100*06/29/16   RXB  C1129    ADD BOOK-ON RECORD FROM TASK LIST TO    00510092
005200*                         DISPLAY CORRECT RETURN DATE FOR VACATION00520092
005300*04/02/23   RPV CNC0600   WRR PHASE 2 CHANGES                     00521094
009000*08/16/24   RJA  CNLD-309 MTOY AND MTOR ARE OBSOLETE.
005400***************************************************************** 00530071
005500 ENVIRONMENT DIVISION.                                            00540071
005600 CONFIGURATION SECTION.                                           00550071
005700 SOURCE-COMPUTER.  IBM-9370.                                      00560071
005800 OBJECT-COMPUTER.  IBM-9370.                                      00570071
005900 DATA DIVISION.                                                   00580071
006000 WORKING-STORAGE SECTION.                                         00590071
006100 01  FILLER                      PIC X(10)   VALUE 'PGM02E W/S'.  00600071
006200*01  WS-ABSTIME                  PIC S9(15)  COMP-3 VALUE +0.     00610071
006300 01  P02E-COMM-LGTH              PIC S9(4)   COMP   VALUE +870.   00620071
006400     COPY P02ETSQ.                                                00630071
006500 01  WS-SUBSCRIPTS.                                               00640071
006600     05  I                             PIC 999  VALUE ZERO.       00650071
006700     05  J                             PIC 999  VALUE ZERO.       00660071
006800     05  K                             PIC 999  VALUE ZERO.       00670071
006900     05  S1                            PIC 999  VALUE ZERO.       00680071
007000     05  S2                            PIC 999  VALUE ZERO.       00690071
007100     05  S3                            PIC 999  VALUE ZERO.       00700071
007200     05  S4                            PIC 999  VALUE ZERO.       00710071
007300     05  S5                            PIC 999  VALUE ZERO.       00720071
007400*C1104 - BEG                                                      00730089
007500     05  S6                            PIC 999  VALUE ZERO.       00740089
007600*C1104 - END                                                      00750089
007700     05  SUB1                          PIC S999 VALUE ZERO.       00760071
007800     05  SUB2                          PIC S999 VALUE ZERO.       00770071
007900     05  BEG-SUB                       PIC S999 VALUE ZERO.       00780071
008000     05  END-SUB                       PIC S999 VALUE ZERO.       00790071
008100     05  WS-SAVE-SUB                   PIC 999  VALUE ZEROS.      00800071
008200     05  WS-COMPUTE-SUB                PIC 999  VALUE ZEROS.      00810071
008300     05  CRAFT-SUB-INCREMENT           PIC 999  VALUE ZERO.       00820071
008400     05  CRAFT-ARRAY-SUB               PIC 999  VALUE ZERO.       00830071
008500     05  CC-ARRAY-SUB                  PIC 999  VALUE ZERO.       00840071
008600     05  CRAFT-ARRAY-MAX               PIC 999  VALUE 8.          00850071
008700     05  ARRAY-SUB                     PIC 999  VALUE ZERO.       00860071
008800     05  ARRAY-MAX                     PIC 999  VALUE 16.         00870071
008900*CNC0510     05  COT-ARRAY-MAX                 PIC 999  VALUE 28. 00880071
009000     05  COT-ARRAY-MAX                 PIC 999  VALUE 98.         00890071
009100     05  GROUP-MAX                     PIC 999  VALUE ZERO.       00900071
009200                                                                  00910071
009300 01  WS-CRAFT-AREA.                                               00920071
009400     05  WS-COMPARE-CRAFT              PIC XX.                    00930071
009500     05  WS-CRAFT-ARRAY OCCURS 8 TIMES.                           00940071
009600         10  WS-CRAFT-CODE             PIC XX.                    00950071
009700         10  WS-ASSOC-CRAFT            PIC XX.                    00960071
009800         10  WS-CRAFT-STATUS           PIC X.                     00970071
009900             88  OPTIONAL-CRAFT            VALUE '1'.             00980071
010000         10  WS-CRAFT-SUB              PIC 999.                   00990071
010100         10  WS-CRAFT-MAX              PIC 999.                   01000071
010200         10  WS-CRAFT-SCROLL-KEY       PIC X(12).                 01010071
010300             88  WS-CRAFT-DONE             VALUE 'DONE        '.  01020071
010400                                                                  01030071
010500 01  WS-CALL-ORDER-CRAFT-AREA.                                    01040071
010600     05  WS-CO-CRAFT-ARRAY OCCURS 8 TIMES.                        01050071
010700*CNC0510 10  WS-CO-TURN-ARRAY OCCURS 28 TIMES.                    01060071
010800         10  WS-CO-TURN-ARRAY OCCURS 98 TIMES.                    01070071
010900             15  WS-COT-KEY.                                      01080071
011000                 20  WS-COT-START-DATE-TIME.                      01090071
011100                     25  WS-COT-START-DATE     PIC X(8) VALUE ' '.01100071
011200                     25  WS-COT-START-TIME     PIC X(4) VALUE ' '.01110071
011300                 20  WS-COT-CALL-ORDER         PIC X(1) VALUE ' '.01120071
011400                 20  WS-COT-BOARD-DATE-TIME-TIE.                  01130071
011500                     25  WS-COT-BOARD-CENT     PIC X(2) VALUE ' '.01140071
011600                     25  WS-COT-BOARD-DATE-TIME.                  01150071
011700                         30  WS-COT-BOARD-DATE PIC X(6) VALUE ' '.01160071
011800                         30  WS-COT-BOARD-TIME PIC X(4) VALUE ' '.01170071
011900                     25  WS-COT-BOARD-TIE      PIC X(4) VALUE ' '.01180071
012000             15  WS-COT-SCHEDKEY1             PIC X(36) VALUE ' '.01190071
012100*CNC0510                                                          01200071
012200             15  WS-COT-ASGN                  PIC X(08) VALUE ' '.01210071
012300             15  WS-COT-END-DATE-TIME.                            01220071
012400                 20  WS-COT-END-DATE          PIC X(8) VALUE ' '. 01230071
012500                 20  WS-COT-END-TIME          PIC X(4) VALUE ' '. 01240071
012600*CNC0564A - BEG                                                   01250071
012700             15  WS-TURN-PROTECTED-FL         PIC X    VALUE ' '. 01260071
012800                 88  WS-TURN-PROTECTED                 VALUE ' '. 01270071
012900                 88  WS-TURN-PROTECTED-NO              VALUE '1'. 01280071
013000*01  WS-CALL-ORDER-SAVE-AREA           PIC X(65) VALUE SPACES.    01290071
013100 01  WS-CALL-ORDER-SAVE-AREA           PIC X(86) VALUE SPACES.    01300071
013200*CNC0564A - END                                                   01310071
013300*C1104 - BEG                                                      01320087
013400 01  WS-POOL-HOME-LEAD-TIME            PIC 9999    VALUE ZEROS.   01330086
013500 01  WS-SCHED-START-ML-DATE-TIME.                                 01340086
013600     02 WS-SCHED-START-ML-CE           PIC X(02)   VALUE SPACES.  01350086
013700     02 WS-SCHED-START-ML-DATE         PIC X(06)   VALUE SPACES.  01360086
013800     02 WS-SCHED-START-ML-TIME         PIC X(04)   VALUE SPACES.  01370086
013900 01  WS-SCHED-END-ML-DATE-TIME.                                   01380086
014000     02 WS-SCHED-END-ML-CE             PIC X(02)   VALUE SPACES.  01390086
014100     02 WS-SCHED-END-ML-DATE           PIC X(06)   VALUE SPACES.  01400086
014200     02 WS-SCHED-END-ML-TIME           PIC X(04)   VALUE SPACES.  01410086
014300*C1104 - END                                                      01420087
014400                                                                  01430071
014500*CNC0510                                                          01440071
014600 01  WS-PREV-SCHED-END-DTTM            PIC 9(12)   VALUE ZEROS.   01450071
014700 01  WS-PREV-SCHED-ASGN                PIC X(08)   VALUE SPACES.  01460071
014800 01  WS-COMPARE-SCHED-START-DTTM       PIC 9(12)   VALUE ZEROS.   01470071
014900 01  WS-COMPARE-SCHED-END-DTTM         PIC 9(12)   VALUE ZEROS.   01480071
015000*                                                                 01490071
015100                                                                  01500071
015200 01  WS-TURN-POS-CRAFT-AREA.                                      01510071
015300     05  WS-TURN-POS-ARRAY OCCURS 8 TIMES.                        01520071
015400         10  WS-TURN-POS-CRAFT         PIC X(02).                 01530071
015500         10  WS-TURN-POS-SORT-KEY.                                01540071
015600             15  WS-TURN-POS-ON-DUTY   PIC X(10).                 01550071
015700             15  WS-TURN-POS-TRAIN     PIC X(10).                 01560071
015800             15  WS-TURN-POS-CRAFT-SEQ PIC X(03).                 01570071
015900         10  WS-TURN-POS-UFP-KEY       PIC X(12).                 01580071
016000                                                                  01590071
016100 01  WS-CALL-ORDER-AREA.                                          01600071
016200     02 WS-CALL-ORDER-KEY-ARRAY OCCURS 8.                         01610071
016300         04 WS-CALL-ORDER-KEY.                                    01620071
016400             06 WS-CO-START-DATE       PIC X(08) VALUE SPACES.    01630071
016500             06 WS-CO-START-TIME       PIC X(04) VALUE SPACES.    01640071
016600             06 WS-CO-CALL-ORDER       PIC X(01) VALUE SPACES.    01650071
016700             06 WS-CO-POSITION         PIC X(16) VALUE SPACES.    01660071
016800                                                                  01670071
016900 01  WS-FLAGS.                                                    01680071
017000     05  DO-INCREMENT-FLAG             PIC X   VALUE '0'.         01690071
017100         88  DO-INCREMENT                      VALUE '1'.         01700071
017200     05  WS-BOARD                      PIC X  VALUE SPACE.        01710071
017300         88  TURN-BOARD                       VALUE 'T'.          01720071
017400         88  POSITION-BOARD                   VALUE '0' THRU '5'. 01730071
017500     05  WS-TERMINAL                   PIC X  VALUE SPACE.        01740071
017600         88  HOME-TERMINAL-INQ                VALUE '0'.          01750071
017700     05  WS-TURN-POS-FLAG              PIC X  VALUE '0'.          01760071
017800         88  WS-TURN-POS                      VALUE '1'.          01770071
017900     05  PROTECTION-IS-IN-EFFECT-FLAG  PIC X   VALUE '0'.         01780071
018000         88  PROTECTION-IS-IN-EFFECT           VALUE '1'.         01790071
018100     05  SCREEN-FLAG                   PIC X   VALUE '0'.         01800071
018200         88  CONTINUE-SCREEN                   VALUE '0'.         01810071
018300         88  CREATE-SCREEN                     VALUE '1'.         01820071
018400         88  SEND-BUFFER                       VALUE '2'.         01830071
018500     05  DONE-CODE                     PIC X   VALUE '0'.         01840071
018600         88 NOT-DONE                           VALUE '0'.         01850071
018700         88 DONE                               VALUE '1'.         01860071
018800     05  DONE-CODE1                    PIC X   VALUE '0'.         01870071
018900         88 NOT-DONE1                          VALUE '0'.         01880071
019000         88 DONE1                              VALUE '1'.         01890071
019100     05  SORT-DONE-FLAG                PIC X   VALUE 'N'.         01900071
019200         88 SORT-NOT-DONE                      VALUE 'N'.         01910071
019300         88 SORT-DONE                          VALUE 'Y'.         01920071
019400     05  COT-DONE-FLAG                 PIC X   VALUE 'N'.         01930071
019500         88 COT-NOT-DONE                       VALUE 'N'.         01940071
019600         88 COT-DONE                           VALUE 'Y'.         01950071
019700     05  CONVERT-TURN-CODE             PIC X   VALUE '0'.         01960071
019800         88  CONVERT-TURN-TO-MADE-UP           VALUE '1'.         01970071
019900     05  ASGN-DONE-CODE                PIC X   VALUE '0'.         01980071
020000         88 ASGN-DONE                          VALUE '1'.         01990071
020100     05  WS-TASK-DONE-CONDE            PIC X   VALUE 'N'.         02000071
020200         88 TASK-NOT-DONE                      VALUE 'N'.         02010071
020300         88 TASK-DONE                          VALUE 'Y'.         02020071
020400     05  SCHED-DONE-CODE               PIC X   VALUE '0'.         02030071
020500         88 NOT-SCHED-DONE                     VALUE '0'.         02040071
020600         88 SCHED-DONE                         VALUE '1'.         02050071
020700     05  ASSOCIATED-TURN-FLAG          PIC 9   VALUE 0.           02060071
020800         88  ASSOCIATED-TURN                   VALUE 1.           02070071
020900     05  EN-FI-MARRIED-FLAG            PIC X   VALUE SPACE.       02080071
021000         88  EN-FI-MARRIED                     VALUE 'Y', '1'.    02090071
021100         88  EN-SE-MARRIED                     VALUE '1'.         02100071
021200     05  CO-BK-MARRIED-FLAG            PIC X   VALUE SPACE.       02110071
021300         88  CO-BK-MARRIED                     VALUE 'Y'.         02120071
021400     05  CO-B1-MARRIED-FLAG            PIC X   VALUE SPACE.       02130071
021500         88  CO-B1-MARRIED                     VALUE '1'.         02140071
021600     05  CO-B2-MARRIED-FLAG            PIC X   VALUE SPACE.       02150071
021700         88  CO-B2-MARRIED                     VALUE '1'.         02160071
021800     05  CO-BG-MARRIED-FLAG            PIC X   VALUE SPACE.       02170071
021900         88  CO-BG-MARRIED                     VALUE '1'.         02180071
022000     05  B1-B2-MARRIED-FLAG            PIC X   VALUE SPACE.       02190071
022100         88  B1-B2-MARRIED                     VALUE 'Y'.         02200071
022200     05  EN-ET-MARRIED-FLAG            PIC X   VALUE SPACE.       02210071
022300         88  EN-ET-MARRIED                     VALUE 'Y'.         02220071
022400     05  CO-TT-MARRIED-FLAG            PIC X   VALUE SPACE.       02230071
022500         88  CO-TT-MARRIED                     VALUE 'Y'.         02240071
022600     05  DONE-WITH-ASGN-CODE           PIC X   VALUE '0'.         02250071
022700         88 DONE-WITH-ASGN                     VALUE '1'.         02260071
022800     05  EN-ID-POOL-FLAG               PIC X   VALUE '0'.         02270071
022900         88  EN-ID-POOL                        VALUE '1'.         02280071
023000     05  TR-ID-POOL-FLAG               PIC X   VALUE '0'.         02290071
023100         88  TR-ID-POOL                        VALUE '1'.         02300071
023200     05  POOL-SERVICE-CODE             PIC X   VALUE ' '.         02310071
023300         88  MINE-TURN-SVC                     VALUE 'M'.         02320071
023400     05  READ-NEXT-REC-FLAG            PIC X   VALUE SPACE.       02330071
023500         88  DONT-READ-NEXT-REC                VALUE ' '.         02340071
023600         88  READ-NEXT-REC                     VALUE 'Y'.         02350071
023700     05  BID-PACK-FLAG                 PIC X   VALUE SPACE.       02360071
023800         88  BID-PACK-POOL                     VALUE 'B'.         02370071
023900         88  SCHEDULE-POOL                     VALUE 'S'.         02380071
024000     05  DISPLAY-TURN-FLAG             PIC X   VALUE SPACE.       02390071
024100         88  DISPLAY-TURN                      VALUE ' '.         02400071
024200         88  DONT-DISPLAY-TURN                 VALUE 'N'.         02410071
024300         88  DONT-DISPLAY-STOP                 VALUE 'S'.         02420071
024400*CNC0564A - BEG                                                   02430071
024500         88  DISPLAY-TURN-RD-RED               VALUE 'R'.         02440071
024600*CNC0564A - END                                                   02450071
024700     05  CALL-ORDER-FLAG               PIC X   VALUE 'N'.         02460071
024800         88  CALL-ORDER-REQ                    VALUE 'C'.         02470071
024900     05  FOUND-CYCLE-FLAG              PIC X   VALUE '0'.         02480071
025000         88  NOT-FOUND-CYCLE                   VALUE '0'.         02490071
025100         88  FOUND-CYCLE                       VALUE '1'.         02500071
025200     05  CURRENT-CYCLE-FLAG            PIC X   VALUE '0'.         02510071
025300         88  FROM-CURRENT-CYCLE                VALUE '0'.         02520071
025400         88  NOT-FROM-CURRENT-CYCLE            VALUE '1'.         02530071
025500     05  WS-DUEBACK-FOUND-FLAG         PIC X   VALUE '0'.         02540071
025600         88  WS-DUEBACK-FOUND-N                VALUE '0'.         02550071
025700         88  WS-DUEBACK-FOUND-Y                VALUE '1'.         02560071
025800*CNC0564A - BEG                                                   02570071
025900     05  WS-3A-BIDPK-TIEBRK-FL         PIC X   VALUE SPACE.       02580071
026000         88  WS-3A-BIDPK-TIEBRK-PW-BRD         VALUE  'B'.        02590071
026100         88  WS-3A-BIDPK-TIEBRK-PROT-W         VALUES ' ' 'P'.    02600071
026200     05  TURN-IS-SCHEDULED-FL          PIC X   VALUE '0'.         02610071
026300         88  TURN-IS-SCHEDULED                 VALUE '0'.         02620071
026400         88  TURN-IS-NOT-SCHEDULED             VALUE '1'.         02630071
026500*CNC0564A - END                                                   02640071
026600*C1104 - BEG                                                      02650089
026700     05  DUP-TURN-FLAG                 PIC X   VALUE 'N'.         02660089
026800         88 DUP-TURN-NOT-DONE                  VALUE 'N'.         02670089
026900         88 DUP-TURN-DONE                      VALUE 'Y'.         02680089
027000*C1104 - ENG                                                      02690089
027100*CNC0600-B                                                        02691096
027200     05  WS-CAN-WRR-FLAG             PIC X(001) VALUE ' '.        02692096
027300         88  WS-CAN-WRR-NEW                     VALUE 'N'.        02693096
027400         88  WS-CAN-WRR-OLD                     VALUE ' ' 'O'.    02694096
027500     05  WS-EMP-STATUS-FLAG          PIC X(001) VALUE ' '.        02694199
027600         88  WS-EMP-STATUS-RED                  VALUE 'R'.        02694299
027700         88  WS-EMP-STATUS-YELLOW               VALUE 'Y'.        02694399
027800     02  AT-HOME-TERM-FLAG             PIC X(001) VALUE SPACES.   02694499
027900         88  NOT-AT-HOME-TERM          VALUE '1' '2' '3' '4' '5'. 02694599
028000         88  AT-HOME-TERM                       VALUE '0'.        02694699
027800     02  WS-CHECK-60-192-FLAG        PIC X(001) VALUE 'N'.
027900         88  WS-CHECK-60-192-STATUS             VALUE 'Y'.
027900         88  WS-DO-NOT-CHECK-60-192-STATUS      VALUE 'N'.
028100*CNC0600-E                                                        02695096
028200                                                                  02700071
028300 01  WS-SAVE-ASGN-FILE.                                           02710071
028400     02  WORK-ASGNKEY1.                                           02720071
028500         04  WK-ASGN-JOB-TYPE          PIC X     VALUE 'X'.       02730071
028600         04  WK-ASGN-DIST              PIC XX    VALUE SPACE.     02740071
028700         04  WK-ASGN-SUB-DIST          PIC XX    VALUE SPACE.     02750071
028800         04  WK-ASGN-ASGN.                                        02760071
028900             06  WK-ASGN-POOL          PIC X(2)  VALUE SPACE.     02770071
029000             06  WK-ASGN-TURN          PIC X(4)  VALUE SPACE.     02780071
029100             06  WK-ASGN-CC            PIC X(2)  VALUE SPACE.     02790071
029200         04  WK-ASGN-REC-TYPE          PIC X(1)  VALUE SPACE.     02800071
029300         04  WK-ASGN-DATE-TIME         PIC 9(10) VALUE ZEROS.     02810071
029400     02  WORK-ASGNKEY2.                                           02820071
029500         04  WK-ASGN-EMP-NO            PIC X(9)  VALUE SPACE.     02830071
029600         04  FILLER                    PIC X(11) VALUE SPACE.     02840071
029700     02  FILLER                        PIC X(84) VALUE SPACE.     02850071
029800                                                                  02860071
029900 01  WS-HOLD-SCHED                     PIC X(158) VALUE SPACES.   02870071
030000 01  SAVE-SCHED-KEY2.                                             02880071
030100     02  FILLER                        PIC X(06) VALUE SPACES.    02890071
030200     02  SAVE-SK2-CC                   PIC X(02) VALUE SPACES.    02900071
030300     02  FILLER                        PIC X(13) VALUE SPACES.    02910071
030400     02  SAVE-SK2-TURN-ID              PIC X(04) VALUE SPACES.    02920071
030500     02  FILLER                        PIC X(08) VALUE SPACES.    02930071
030600                                                                  02940071
030700 01  WS-PROFILE.                                                  02950071
030800     02  WS-PROF-DIST                  PIC X(02) VALUE SPACES.    02960071
030900     02  WS-PROF-SUB-DIST              PIC X(02) VALUE SPACES.    02970071
031000     02  WS-PROF-BOARD                 PIC X(02) VALUE SPACES.    02980071
031100     02  WS-PROF-TERM                  PIC X(01) VALUE SPACES.    02990071
031200     02  WS-PROF-TYPE                  PIC X(01) VALUE SPACES.    03000071
031300 01  WS-CREW-UNIT                      PIC X(01) VALUE SPACES.    03010071
031400                                                                  03020071
031500 01  WS-DATES.                                                    03030071
031600     02  WS-EVAL-DATE-TIME.                                       03040071
031700         04  WS-EVAL-DATE            PIC X(06) VALUE SPACES.      03050071
031800         04  WS-EVAL-TIME            PIC X(04) VALUE SPACES.      03060071
031900     02  WS-END-CYCLE-DATE-TIME.                                  03070071
032000         04  WS-END-CYCLE-DATE.                                   03080071
032100             06  WS-END-CYCLE-CENT   PIC X(02) VALUE SPACES.      03090071
032200             06  WS-END-CYCLE-YYMMDD PIC X(06) VALUE SPACES.      03100071
032300         04  WS-END-CYCLE-TIME       PIC X(04) VALUE SPACES.      03110071
032400                                                                  03120071
032500     02  WS-SCHED-END-DATE-TIME.                                  03130071
032600         04  WS-SCHED-END-DATE.                                   03140071
032700             06  WS-SCHED-END-CE     PIC X(02) VALUE SPACES.      03150071
032800             06  WS-SCHED-END-YYMMDD PIC X(06) VALUE SPACES.      03160071
032900         04  WS-SCHED-END-TIME       PIC X(04) VALUE SPACES.      03170071
033000     02  WS-CYCLE-START-DATE-TIME.                                03180071
033100         04  WS-CYCLE-START-DATE-CENT.                            03190071
033200             06  WS-CYCLE-START-CE     PIC X(02) VALUE SPACES.    03200071
033300             06  WS-CYCLE-START-DATE.                             03210071
033400                 08  WS-CYCLE-START-YY PIC X(02) VALUE SPACES.    03220071
033500                 08  WS-CYCLE-START-MM PIC X(02) VALUE SPACES.    03230071
033600                 08  WS-CYCLE-START-DD PIC X(02) VALUE SPACES.    03240071
033700         04  WS-CYCLE-START-TIME       PIC X(04).                 03250071
033800     02  WS-CYCLE-START-DATE-CALC REDEFINES                       03260071
033900         WS-CYCLE-START-DATE-TIME.                                03270071
034000         04  WS-CYCLE-START-CCYY     PIC 9(04).                   03280071
034100         04  WS-CYCLE-START-CCYR  REDEFINES                       03290071
034200             WS-CYCLE-START-CCYY.                                 03300071
034300             06  WS-CYCLE-START-CC   PIC X(02).                   03310071
034400             06  WS-CYCLS-START-YR   PIC X(02).                   03320071
034500         04  FILLER                  PIC X(08).                   03330071
034600*CNC0600-B                                                        03331099
034700 01  WS-WRR-CALC-FIELDS.
035800     02  WS-LOCAL-CURRENT-DTTM-CENT.
035900         04 WS-LOCAL-CURRENT-DATE.
036500             05 WS-LOCAL-CURRENT-DATE-CE      PIC X(02).
036500             05 WS-LOCAL-CURRENT-DATE-YYMMDD  PIC X(06).
036000         04 WS-LOCAL-CURRENT-TIME     PIC X(04) VALUE SPACES.
035800     02  WS-LOCAL-CUR-DTTM-YYMMDDHHSS.
036500         04 WS-LOCAL-CUR-DATE-YYMMDD  PIC X(06) VALUE SPACES.
036000         04 WS-LOCAL-CUR-TIME-HHMM    PIC X(04) VALUE SPACES.
036100     02  WS-WRR-6HRS-AFTER-DTTM.
036200         04  WS-WRR-6HRS-AFTER-NUM            PIC 9(10).
036300         04  WS-WRR-6HRS-AFTER-CH
036400             REDEFINES WS-WRR-6HRS-AFTER-NUM.
036500             05 WS-WRR-6HRS-AFTER-DATE        PIC X(06).
036600             05 WS-WRR-6HRS-AFTER-TIME        PIC X(04).
036700* WS-RB-LEAD-TIME IS USED TO GET THE LEAD TIME BEFORE TO SHOW
036800* EMPLOYEE STATUS IN COLORS
036900     02  WS-RESET-BK-LEAD-TIME         PIC X(04) VALUE '0600'.
037000*CNC0600-E                                                        03340699
037100*                                                                 03341071
037200 01  NORMAL-ASGNMT-FLAG                PIC X     VALUE SPACE.     03350071
037300     88  NORM-ASGN-UFP                           VALUE 'U'.       03360071
037400     88  NORM-ASGN-XB                            VALUE 'X'.       03370071
037500     88  NORM-ASGN-AJ                            VALUE 'A'.       03380071
037600 01  NORMAL-ASGNMT.                                               03390071
037700     02  NA-DIST                       PIC XX   VALUE SPACE.      03400071
037800     02  NA-SUB-DIST                   PIC XX   VALUE SPACE.      03410071
037900     02  NA-AREA.                                                 03420071
038000       03  NA-1                        PIC X(6) VALUE SPACE.      03430071
038100       03  NA-2 REDEFINES NA-1.                                   03440071
038200         04  NA-POOL                   PIC XX.                    03450071
038300         04  NA-TURN                   PIC X(4).                  03460071
038400       03  NA-3 REDEFINES NA-1.                                   03470071
038500         04  NA-FILLER                 PIC XX.                    03480071
038600         04  NA-XB-TURN                PIC X(4).                  03490071
038700       03  NA-CC                       PIC XX   VALUE SPACE.      03500071
038800                                                                  03510071
038900 01  TEMPORARY-ASGNMT-FLAG             PIC X     VALUE SPACE.     03520071
039000     88  TEMP-ASGN-UFP                           VALUE 'U'.       03530071
039100     88  TEMP-ASGN-XB                            VALUE 'X'.       03540071
039200     88  TEMP-ASGN-AJ                            VALUE 'A'.       03550071
039300 01  TEMPORARY-ASGNMT.                                            03560071
039400     02  TA-DIST                        PIC XX   VALUE SPACE.     03570071
039500     02  TA-SUB-DIST                    PIC XX   VALUE SPACE.     03580071
039600     02  TA-AREA.                                                 03590071
039700       03  TA-1                         PIC X(6) VALUE SPACE.     03600071
039800       03  TA-2 REDEFINES TA-1.                                   03610071
039900         04  TA-POOL                    PIC XX.                   03620071
040000         04  TA-TURN                    PIC X(4).                 03630071
040100       03  TA-3 REDEFINES TA-1.                                   03640071
040200         04  TA-FILLER                  PIC XX.                   03650071
040300         04  TA-XB-TURN                 PIC X(4).                 03660071
040400       03  TA-CC                        PIC XX   VALUE SPACE.     03670071
040500                                                                  03680071
040600 01  ON-DUTY-ASGNMT-FLAG                PIC X    VALUE SPACE.     03690071
040700     88  ON-DUTY-UFP                             VALUE 'U'.       03700071
040800     88  ON-DUTY-AJ                              VALUE 'A'.       03710071
040900 01  ON-DUTY-ASGNMT.                                              03720071
041000     02  OD-DIST                        PIC XX   VALUE SPACE.     03730071
041100     02  OD-SUB-DIST                    PIC XX   VALUE SPACE.     03740071
041200     02  OD-AREA.                                                 03750071
041300       03  OD-1                         PIC X(6) VALUE SPACE.     03760071
041400       03  OD-2 REDEFINES OD-1.                                   03770071
041500         04  OD-POOL                    PIC XX.                   03780071
041600         04  OD-TURN                    PIC X(4).                 03790071
041700       03  OD-CC                        PIC XX   VALUE SPACE.     03800071
041800 01  ON-DUTY-OUT-TOWN-CODE              PIC X(10)                 03810071
041900                                        VALUE '9999999999'.       03820071
042000     88  OUT-TOWN                       VALUE '0000000000'.       03830071
042100                                                                  03840071
042200 01  WS-MISC-NUM.                                                 03850071
042300     02  FIRST-EMP-NBR                 PIC 9(9) VALUE 0.          03860071
042400         88  HAVE-PERMANENT-EMPLOYEE   VALUES 000000001           03870071
042500                                         THRU 999999995.          03880071
042600     02  ON-DUTY-EMP                   PIC 9(9) VALUE 0.          03890071
042700         88  HAVE-ON-DUTY-EMPLOYEE     VALUES 000000001           03900071
042800                                         THRU 999999995.          03910071
042900     02  TEMP-EMP-ONE                  PIC 9(9) VALUE 0.          03920071
043000         88  HAVE-TEMPORARY-EMPLOYEE   VALUES 000000001           03930071
043100                                         THRU 999999995.          03940071
043200                                                                  03950071
043300 01  WS-MISC.                                                     03960071
043400     02  WS-COMPARE-TURN               PIC X(12) VALUE SPACES.    03970071
043500     02  WS-HT-ON-DUTY-TIME            PIC X(10) VALUE SPACES.    03980071
043600     02  WS-FIRST-TIME                 PIC X(01) VALUE SPACES.    03990071
043700     02  WS-TURN-POS-COUNT             PIC 9(03) VALUE ZEROS.     04000071
043800     02  WS-SORT-KEY                   PIC X(23) VALUE SPACES.    04010071
043900     02  WS-RETURN-DATE                PIC X(4).                  04020071
044000     02  FILLER REDEFINES WS-RETURN-DATE.                         04030071
044100         05  WS-RETURN-DATE-MM         PIC 99.                    04040071
044200         05  WS-RETURN-DATE-DD         PIC 99.                    04050071
044300     02  WS-RETURN-DY                  PIC X(2).                  04060071
044400     02  WS-REST-DATE-TIME             PIC 9(10).                 04070071
044500     02  WS-REST-DATE-TIME-C REDEFINES                            04080071
044600         WS-REST-DATE-TIME             PIC X(10).                 04090071
044700     02  HOLD-START-DATE-TIME.                                    04100071
044800         04  HOLD-START-DATE           PIC X(08) VALUE SPACES.    04110071
044900         04  HOLD-START-TIME           PIC X(04) VALUE SPACES.    04120071
045000     02  WS-CRAFT-SEL                  PIC X(02) VALUE SPACES.    04130083
045100                                                                  04140071
045200 01  WORK-CNTLKEY.                                                04150071
045300     02  WK-CNTL-REC-TYPE              PIC XX    VALUE '03'.      04160071
045400     02  WK-CNTL-DIST                  PIC XX    VALUE SPACE.     04170071
045500     02  WK-CNTL-SUB-DIST              PIC XX    VALUE SPACE.     04180071
045600     02  WK-CNTL-POOL                  PIC XX    VALUE SPACE.     04190071
045700     02  WK-CNTL-POOL-TYPE             PIC X     VALUE SPACE.     04200071
045800     02  FILLER                        PIC X(11) VALUE SPACE.     04210071
045900                                                                  04220071
046000 01  WORK-FIELD.                                                  04230071
046100     02  FILLER                        PIC XXX   VALUE 'ON '.     04240071
046200     02  TR-FLD                        PIC X(10) VALUE SPACE.     04250071
046300     02  FILLER                        PIC XXX   VALUE ' / '.     04260071
046400     02  NM-FLD                        PIC X(10) VALUE SPACE.     04270071
046500                                                                  04280071
046600 01  TRAIN-FIELD.                                                 04290071
046700     02  FILLER                        PIC XXX   VALUE 'ON '.     04300071
046800     02  TRAIN-FLD                     PIC X(10) VALUE SPACE.     04310071
046900                                                                  04320071
047000 01  TRAIN-FIELD-3.                                               04330071
047100     02  FILLER                        PIC X     VALUE ' '.       04340071
047200     02  TRAIN-FLD-3                   PIC X(10) VALUE SPACE.     04350071
047300                                                                  04360071
047400 01  LOCATION-AREA.                                               04370071
047500     02  FILLER                        PIC X     VALUE SPACE.     04380071
047600     02  LOC-DIST                      PIC XX    VALUE SPACE.     04390071
047700     02  FILLER                        PIC X     VALUE '/'.       04400071
047800     02  LOC-SUB-DIST                  PIC XX    VALUE SPACE.     04410071
047900     02  FILLER                        PIC X     VALUE '/'.       04420071
048000     02  LOC-POOL                      PIC XX    VALUE SPACE.     04430071
048100     02  FILLER                        PIC X     VALUE '-'.       04440071
048200     02  LOC-I-O                       PIC X     VALUE SPACE.     04450071
048300                                                                  04460071
048400*01  TIME-AREA.                                                   04470071
048500*    02  SYSTEM-DATE.                                             04480071
048600*        03  SYS-YR                    PIC 9(2).                  04490071
048700*        03  SYS-MO                    PIC 9(2).                  04500071
048800*        03  SYS-DY                    PIC 9(2).                  04510071
048900*    02  SYSTEM-TIME.                                             04520071
049000*        03  SYS-HRMN.                                            04530071
049100*            04  SYS-HR                PIC 9(2).                  04540071
049200*            04  SYS-MN                PIC 9(2).                  04550071
049300*        03  SYS-SCMS.                                            04560071
049400*            04  SYS-SC                PIC 9(2).                  04570071
049500*            04  SYS-MS                PIC 9(2).                  04580071
049600*01  TIME-AREA-A REDEFINES TIME-AREA.                             04590071
049700*    02  PRESENT-TIME                  PIC 9(10).                 04600071
049800*    02  FILLER                        PIC X(4).                  04610071
049900*01  WS-LOCAL-TIME-AREA.                                          04620071
050000*    02  WS-LOCAL-DATE-TIME            PIC 9(10).                 04630071
050100                                                                  04640071
050200 01  WS-UFP-TURN-KEY.                                             04650071
050300     02  WS-UFP-DIST                   PIC XX    VALUE SPACE.     04660071
050400     02  WS-UFP-SUB-DIST               PIC XX    VALUE SPACE.     04670071
050500     02  WS-UFP-POOL                   PIC XX    VALUE SPACE.     04680071
050600     02  WS-UFP-TURN                   PIC X(4)  VALUE SPACE.     04690071
050700     02  WS-UFP-CC                     PIC XX    VALUE SPACE.     04700071
050800                                                                  04710071
050900 01  WS-UFP-POS-KEY.                                              04720071
051000     02  WS-UFP-POS-DIST               PIC XX    VALUE SPACE.     04730071
051100     02  WS-UFP-POS-SUB-DIST           PIC XX    VALUE SPACE.     04740071
051200     02  WS-UFP-POS-POOL               PIC XX    VALUE SPACE.     04750071
051300     02  WS-UFP-POS-CC                 PIC XX    VALUE SPACE.     04760071
051400     02  WS-UFP-POS-IN-OUT             PIC 99    VALUE ZEROES.    04770071
051500     02  FILLER REDEFINES WS-UFP-POS-IN-OUT.                      04780071
051600         04  FILLER                    PIC X.                     04790071
051700         04  WS-UFP-POS-IN-OUT-TERM    PIC X.                     04800071
051800     02  WS-KEY-POS.                                              04810071
051900         04  WS-KEY-POS-BOARD          PIC 9     VALUE ZEROES.    04820071
052000         04  WS-KEY-POS-AREA.                                     04830071
052100             06  WS-KEY-POS-DATE-TIME  PIC X(10) VALUE ZEROES.    04840071
052200             06  WS-KEY-POS-TIE-BREAK  PIC 9(4)  VALUE ZEROES.    04850071
052300                                                                  04860071
052400 01  HOLD-POOL-REST-CODES.                                        04870071
052500     02  HOLD-POOL-REST-CODE           PIC X(01) VALUE SPACES.    04880071
052600 01  HOLD-MSTR.                                                   04890071
052700     02  FILLER                       PIC X(260) VALUE SPACES.    04900071
052800     02  HOLD-MSTR-TRHIST             PIC X(10)  VALUE SPACES.    04910071
052900     02  FILLER                       PIC X(114) VALUE SPACES.    04920071
053000 01  HOLD-TRHIST.                                                 04930071
053100     02  FILLER                       PIC X(16).                  04940071
053200     02  HOLD-TRAIN-SYMBOL            PIC X(10).                  04950071
053300                                                                  04960071
053400 01  WS-VARIABLE-LINE-1-HDR.                                      04970071
053500     02  FILLER           PIC X(6)  VALUE SPACES.                 04980071
053600     02  FILLER           PIC X(16) VALUE 'TURN INFORMATION'.     04990071
053700     02  FILLER           PIC X(5)  VALUE SPACE.                  05000071
053800     02  FILLER           PIC X(7)  VALUE 'N S B  '.              05010071
053900     02  FILLER           PIC X(3)  VALUE SPACE.                  05020071
054000 01  WS-VARIABLE-LINE-1-HDR-FRENCH.                               05030071
054100     02  FILLER           PIC X(6)  VALUE SPACES.                 05040071
054200     02  FILLER           PIC X(16) VALUE 'INFORMATION TOUR'.     05050071
054300     02  FILLER           PIC X(5)  VALUE SPACE.                  05060071
054400     02  FILLER           PIC X(7)  VALUE 'N S B  '.              05070071
054500     02  FILLER           PIC X(3)  VALUE SPACE.                  05080071
054600 01  WS-VARIABLE-LINE-3-HDR.                                      05090071
054700***  02  FILLER           PIC X(20) VALUE 'RETN    SC HM REST  '. 05100071
054800***  02  FILLER           PIC X(17) VALUE ' USHR TURN INFO  '.    05110071
054900     02  FILLER           PIC X(20) VALUE 'RN  RETN   SC  REST '. 05120071
055000     02  FILLER           PIC X(18) VALUE '  USHR TURN INFO  '.   05130071
055100 01  WS-VARIABLE-LINE-3-HDR-FRENCH.                               05140071
055200***  02  FILLER           PIC X(20) VALUE 'RETN    SE HM REST  '. 05150071
055300***  02  FILLER           PIC X(17) VALUE ' HMEU INFO TOUR  '.    05160071
055400     02  FILLER           PIC X(20) VALUE 'RN  RETN   SE  REST '. 05170071
055500     02  FILLER           PIC X(18) VALUE '  HMEU INFO TOUR  '.   05180071
055600 01  WS-VARIABLE-LINE-4-HDR.                                      05190071
055700***  02  FILLER           PIC X(20) VALUE 'RETN    SC HM REST  '. 05200071
055800***  02  FILLER           PIC X(17) VALUE ' USHR STRT END CO'.    05210071
055900     02  FILLER           PIC X(20) VALUE 'RN RETN    SC  REST '. 05220071
056000     02  FILLER           PIC X(18) VALUE '  USHR STRT END CO'.   05230071
056100 01  WS-VARIABLE-LINE-4-HDR-FRENCH.                               05240071
056200***  02  FILLER           PIC X(20) VALUE 'RETN    SE HM REST  '. 05250071
056300***  02  FILLER           PIC X(17) VALUE ' HMEU STRT END CO'.    05260071
056400     02  FILLER           PIC X(20) VALUE 'RN  RETN   SE  REST '. 05270071
056500     02  FILLER           PIC X(18) VALUE '  HMEU STRT END CO'.   05280071
056600*CNC0564A - 04/29/15 - BEG                                        05290071
056700 01  WS-VARIABLE-LINE-5-HDR.                                      05300071
056800     02  FILLER           PIC X(18) VALUE 'RN RETN    REST   '.   05310071
056900     02  FILLER           PIC X(20) VALUE ' USHR  START   END  '. 05320071
057000 01  WS-VARIABLE-LINE-5-HDR-FRENCH.                               05330071
057100     02  FILLER           PIC X(18) VALUE 'RN  RETN   REST   '.   05340071
057200     02  FILLER           PIC X(20) VALUE ' HMEU  START   END  '. 05350071
057300*CNC0564A - 04/29/15 - END                                        05360071
057400                                                                  05370071
057500 01  WS-VARIABLE-LINE-1.                                          05380071
057600     02  WS-VL1-VARIABLE.                                         05390071
057700         04  WS-VL1-VARIABLE-A  PIC X(15) VALUE SPACES.           05400071
057800         04  WS-VL1-VARIABLE-B  PIC X(11) VALUE SPACES.           05410071
057900     02  WS-VL1-RETURN-INFO REDEFINES WS-VL1-VARIABLE.            05420071
058000         04  WS-VL1-RETURN-TEXT PIC X(09).                        05430071
058100         04  WS-VL1-RETURN-DATE PIC X(04).                        05440071
058200         04  FILLER             PIC X(01).                        05450071
058300         04  WS-VL1-RETURN-DY   PIC X(02).                        05460071
058400         04  FILLER             PIC X(10).                        05470071
058500     02  FILLER                 PIC X     VALUE SPACE.            05480071
058600     02  WS-VL1-INFO-1          PIC X     VALUE SPACE.            05490071
058700     02  FILLER                 PIC X     VALUE SPACE.            05500071
058800     02  WS-VL1-INFO-2          PIC X     VALUE SPACE.            05510071
058900     02  FILLER                 PIC X     VALUE SPACE.            05520071
059000     02  WS-VL1-INFO-3          PIC X     VALUE SPACE.            05530071
059100     02  FILLER                 PIC X(5)  VALUE SPACE.            05540071
059200                                                                  05550071
059300 01  WS-VARIABLE-LINE-3.                                          05560071
059400     02  WS-VL3-RSN              PIC X(2).                        05570071
059500     02  FILLER                  PIC X.                           05580071
059600     02  WS-VL3-RETURN-DATE      PIC X(4).                        05590071
059700     02  FILLER                  PIC X.                           05600071
059800     02  WS-VL3-RETURN-DY        PIC X(2).                        05610071
059900     02  FILLER                  PIC X.                           05620071
060000     02  WS-VL3-SHORT-TURN       PIC XX.                          05630071
060100     02  FILLER                  PIC X.                           05640071
060200***  02  WS-VL3-HOME             PIC X.                           05650071
060300***  02  FILLER                  PIC X.                           05660071
060400     02  WS-VL3-REST-TIME        PIC X(4).                        05670071
060500     02  FILLER                  PIC X.                           05680071
060600     02  WS-VL3-REST-DY          PIC X(2).                        05690071
060700     02  FILLER                  PIC X.                           05700071
060800     02  WS-VL3-USHR             PIC X(4).                        05710071
060900     02  FILLER                  PIC X.                           05720071
061000     02  WS-VL3-TURN-INFO        PIC X(11).                       05730071
061100                                                                  05740071
061200 01  WS-VARIABLE-LINE-4.                                          05750071
061300     02  WS-VL4-RSN              PIC X(2).                        05760071
061400     02  FILLER                  PIC X.                           05770071
061500     02  WS-VL4-RETURN-DATE      PIC X(4).                        05780071
061600     02  FILLER                  PIC X.                           05790071
061700     02  WS-VL4-RETURN-DY        PIC X(2).                        05800071
061800     02  FILLER                  PIC X.                           05810071
061900     02  WS-VL4-SHORT-TURN       PIC XX.                          05820071
062000     02  FILLER                  PIC X.                           05830071
062100***  02  WS-VL4-HOME             PIC X.                           05840071
062200***  02  FILLER                  PIC X.                           05850071
062300     02  WS-VL4-REST-TIME        PIC X(4).                        05860071
062400     02  FILLER                  PIC X.                           05870071
062500     02  WS-VL4-REST-DY          PIC X(2).                        05880071
062600     02  FILLER                  PIC X.                           05890071
062700     02  WS-VL4-USHR             PIC X(4).                        05900071
062800     02  FILLER                  PIC X.                           05910071
062900     02  WS-VL4-START            PIC X(04).                       05920071
063000     02  FILLER                  PIC X.                           05930071
063100     02  WS-VL4-END              PIC X(04).                       05940071
063200     02  FILLER                  PIC X.                           05950071
063300     02  WS-VL4-CO               PIC X(01).                       05960071
063400*CNC0564A - 04/29/15 - BEG                                        05970071
063500 01  WS-VARIABLE-LINE-5.                                          05980071
063600     02  WS-VL5-RSN              PIC X(2).                        05990071
063700     02  FILLER                  PIC X.                           06000071
063800     02  WS-VL5-RETURN-DATE      PIC X(4).                        06010071
063900     02  FILLER                  PIC X.                           06020071
064000     02  WS-VL5-RETURN-DY        PIC X(2).                        06030071
064100     02  FILLER                  PIC X.                           06040071
064200     02  WS-VL5-REST-TIME        PIC X(4).                        06050071
064300     02  FILLER                  PIC X.                           06060071
064400     02  WS-VL5-REST-DY          PIC X(2).                        06070071
064500     02  FILLER                  PIC X.                           06080071
064600     02  WS-VL5-USHR             PIC X(4).                        06090071
064700     02  FILLER                  PIC XX.                          06100071
064800     02  WS-VL5-DAY              PIC X(02).                       06110071
064900     02  FILLER                  PIC X.                           06120071
065000     02  WS-VL5-START            PIC X(04).                       06130071
065100     02  FILLER                  PIC X.                           06140071
065200     02  WS-VL5-END              PIC X(04).                       06150071
065300     02  FILLER                  PIC X.                           06160071
065400*CNC0564A - 04/29/15 - END                                        06170071
065500                                                                  06180071
065600******************************************************************06190071
065700***                  TEMPORARY STORAGE QUEUE                   ***06200071
065800******************************************************************06210071
065900 01  P02ETSQ2-QUEUE-ITEM        PIC S9(4)  COMP VALUE +1.         06220071
066000 01  P02ETSQ-MAP-QUEUE-ID.                                        06230071
066100     05  P02ETSQ-MAP-QUEUE      PIC X(4)   VALUE '02EM'.          06240071
066200     05  P02ETSQ-MAP-TERM-ID    PIC X(4)   VALUE SPACES.          06250071
066300 01  P02ETSQ-CA-QUEUE-ID.                                         06260071
066400     05  P02ETSQ-CA-QUEUE       PIC X(4)   VALUE '02EC'.          06270071
066500     05  P02ETSQ-CA-TERM-ID     PIC X(4)   VALUE SPACES.          06280071
066600 01  P02ETSQ2-QLGTH             PIC S9(4)  COMP VALUE +1.         06290071
066700***************************************************************** 06300071
066800***                 I/O STATUS CHECK FIELDS                       06310071
066900***************************************************************** 06320071
067000 01  WS-RESPONSE                 PIC S9(8) COMP VALUE ZEROES.     06330071
067100 01  FILE-STATUS                 PIC 9(4)  VALUE ZEROES.          06340071
067200     COPY IOCODES.                                                06350071
067300***************************************************************** 06360071
067400***                      COMMAREA COPYBOOKS                       06370071
067500***************************************************************** 06380071
067600     COPY PSTCOMM.                                                06390071
067700     COPY P27PCOMM.                                               06400074
067800     COPY P998COMM.                                               06410071
067900     COPY FICOMM.                                                 06420071
068000     05  P02ECA-SCREEN-COMM.                                      06430071
068100         10  P02ECA-DATA-OUT-SPACES      PIC X(696)  VALUE SPACES.06440071
068200         10  P02ECA-DATA-OUT REDEFINES P02ECA-DATA-OUT-SPACES.    06450071
068300             15  P02ECA-ARRAY OCCURS 16 TIMES.                    06460071
068400                 18  P02ECA-TURN-KEY     PIC X(12).               06470071
068500                 18  P02ECA-CALL-ORDER-KEY PIC X(29).             06480071
068600             15  P02ECA-HOLD-POS-ARRAY   OCCURS 08 TIMES.         06490071
068700                 18  P02ECA-HOLD-POS     PIC X(03).               06500071
068800                 18  P02ECA-HOLD-POS-NUM REDEFINES                06510071
068900                     P02ECA-HOLD-POS     PIC 9(03).               06520071
069000             15  FILLER                  PIC X(16).               06530071
069100         10  P02ECA-POS-DONE-FLAG        PIC X(01).               06540071
069200             88  P02ECA-POS-DONE                     VALUE '1'.   06550071
069300         10  P02ECA-SUB                  PIC 9(03).               06560071
069400***************************************************************** 06570071
069500***                     MAP AREA COPYBOOK                         06580071
069600***************************************************************** 06590071
069700     COPY PSM02ERE.                                               06600071
069800***************************************************************** 06610071
069900***                   PROGRAM NAMES COPYBOOKS                     06620071
070000***************************************************************** 06630071
070100     COPY PSTCB02E.                                               06640071
070200     COPY PSTCB02.                                                06650071
070300     COPY PSTCB27P.                                               06660074
070400     COPY PSTCB998.                                               06670071
070500***************************************************************** 06680071
070600***                 CALLED ROUTINES COPYBOOKS.                    06690071
070700***************************************************************** 06700071
070800     COPY PSTERAR.                                                06710071
070900     COPY P903COMM.                                               06720071
071000     COPY P910COMM.                                               06730071
071100     COPY P915COMM.                                               06740071
071200     COPY P956COMM.                                               06750071
071300*CNC0600-B                                                        06750199
071400     COPY PS08COMM.                                               06751099
071500*CNC0600-E                                                        06752099
071600***************************************************************** 06760071
071700***                     FILE COPYBOOKS                            06770071
071800***************************************************************** 06780071
071900     COPY WSMSTR.                                                 06790071
072000*CNC000-B                                                         06790197
072100     COPY WSMSTR3.                                                06791097
072200*CNC000-E                                                         06792097
072300     COPY WSASGN.                                                 06800071
072400     COPY WSCNTL.                                                 06810071
072500     COPY WSFICT.                                                 06820071
072600     COPY WSUFP.                                                  06830071
072700     COPY WSTASK.                                                 06840071
072800     COPY WSSCHED.                                                06850071
072900***************************************************************** 06860071
073000***                     MISC. COPYBOOKS                           06870071
073100***************************************************************** 06880071
073200     COPY PSTKEYS.                                                06890071
073300     COPY PSTATTR.                                                06900071
073400     COPY WSMSG.                                                  06910071
073500     COPY PSTCCRFT.                                               06920071
073600     COPY WSZONE.                                                 06930071
073700     COPY WSSYDTTM.                                               06940071
073800     COPY WSEDDATE.                                               06950071
073900     COPY WSEDTIME.                                               06960071
074000     COPY WSBUFFER.                                               06970071
074100*    COPY P02ETSQ.                                                06980071
074200                                                                  06990071
074300 LINKAGE SECTION.                                                 07000071
074400 01  DFHCOMMAREA.                                                 07010071
074500     05  LK-PST-CA              PIC X(170).                       07020071
074600     05  LK-P02E-CA             PIC X(700).                       07030071
074700                                                                  07040071
074800 PROCEDURE DIVISION.                                              07050071
074900*                                                                 07060071
075000 P0000-MAINLINE.                                                  07070071
075100*                                                                 07080071
075200     EXEC CICS IGNORE                                             07090071
075300               CONDITION                                          07100071
075400               ERROR                                              07110071
075500     END-EXEC                                                     07120071
075600     EXEC CICS HANDLE                                             07130071
075700               ABEND                                              07140071
075800               LABEL(P9999-GOT-PROBLEM)                           07150071
075900     END-EXEC                                                     07160071
076000     COPY ABSTIME.                                                07170071
076100     IF EIBCALEN = ZERO                                           07180071
076200        PERFORM P9990-CLEAR-SCREEN                                07190071
076300     END-IF                                                       07200071
076400     MOVE LK-PST-CA  TO PSTCOMM-AREA                              07210071
076500     IF EIBCALEN > PSTCOMM-LGTH                                   07220071
076600        MOVE LK-P02E-CA TO P02ECA-SCREEN-COMM                     07230071
076700     END-IF                                                       07240071
076800     MOVE EIBAID TO PF-CHECK                                      07250071
076900     IF EIBTRNID = P998-TRAN                                      07260071
077000        MOVE LOW-VALUES TO PSTS02E                                07270071
077100        SET CREATE-SCREEN TO TRUE                                 07280071
077200        MOVE P998CA-CURSOR-POS TO EIBCPOSN                        07290071
077300        PERFORM P7010-READ-TSQUEUE                                07300071
077400        PERFORM P9000-SEND-MAP-AND-RETURN                         07310071
077500     END-IF                                                       07320071
077600     IF PFKEY11                                                   07330071
077700        SET PSTCA-FLD-RET-PFKEY11 TO TRUE                         07340071
077800     END-IF                                                       07350071
077900     IF EXIT-KEY OR PFKEY11                                       07360071
078000        PERFORM P9400-DELETE-TSQUEUE                              07370071
078100        PERFORM P9100-SETUP-SCR02                                 07380071
078200     END-IF                                                       07390071
078300     PERFORM P0010-GET-FICT-RECORD                                07400073
078400     IF EIBTRNID NOT = P02E-TRAN                                  07410071
078500        PERFORM P9400-DELETE-TSQUEUE                              07420071
078600        MOVE ZEROS           TO P02ECA-SUB                        07430071
078700        SET CREATE-SCREEN    TO TRUE                              07440071
078800        MOVE LOW-VALUES      TO PSTS02E                           07450071
078900        SET ENTER-KEY        TO TRUE                              07460071
079000        IF PSTCA-FROM-PROGRAM = P27P-PGM                          07470080
079100           MOVE P27PCA-CRAFT-CD TO SCR02E-CRAFT-SEL               07480080
079200        ELSE                                                      07490080
079300           MOVE WS-CRAFT-SEL    TO SCR02E-CRAFT-SEL               07500083
079400        END-IF                                                    07510080
079500     ELSE                                                         07520071
079600        MOVE P02E-MAP-VERSION(PSTCA-SUB) TO P02E-MAP              07530071
079700        EXEC CICS RECEIVE MAP(P02E-MAP)                           07540071
079800                          MAPSET(P02E-SET)                        07550071
079900                          INTO(PSTS02E)                           07560071
080000                          RESP(WS-RESPONSE)                       07570071
080100        END-EXEC                                                  07580071
080200        MOVE WS-RESPONSE     TO FILE-STATUS                       07590071
080300        IF NOT SUCCESS                                            07600071
080400           MOVE 'P0000-1'    TO ERR-PARAGRAPH                     07610071
080500           PERFORM P9999-GOT-PROBLEM                              07620071
080600        END-IF                                                    07630071
080700     END-IF                                                       07640071
080800     IF PFKEY1                                                    07650071
080900        PERFORM P7000-WRITE-TSQUEUE                               07660071
081000        PERFORM P9750-SETUP-SCR998                                07670071
081100     END-IF                                                       07680071
081200                                                                  07690071
081300     IF PFKEY10                                                   07700076
081400        IF PSTCA-FROM-PROGRAM NOT = P27P-PGM                      07710077
081500           MOVE SPACES           TO P27P-COMMAREA-PARMS           07720076
081600           MOVE WS-FICT-KEY      TO P27PCA-FICT-KEY               07730082
081700        END-IF                                                    07740076
081800        MOVE SCR02E-DIST         TO P27PCA-DIST                   07750076
081900        MOVE SCR02E-SUB-DIST     TO P27PCA-SUB-DIST               07760076
082000        MOVE SCR02E-CRAFT-SEL    TO P27PCA-CRAFT-CD               07770080
082100        MOVE 'N'                 TO P27PCA-VCNY-ONLY              07780076
082200        MOVE SCR02E-POOL         TO P27PCA-POOL                   07790076
082300        PERFORM P9200-SETUP-SCR27P                                07800078
082400     END-IF                                                       07810076
082500     PERFORM P0100-PROCESS-INPUT                                  07820071
082600     PERFORM P9000-SEND-MAP-AND-RETURN.                           07830071
082700*                                                                 07840071
082800 P0010-GET-FICT-RECORD.                                           07850074
082900*                                                                 07860073
083000     IF PSTCA-FROM-PROGRAM = P27P-PGM AND                         07870081
083100        P27PCA-FICT-KEY > SPACES                                  07880081
083200        MOVE P27PCA-FICT-KEY     TO WS-FICT-KEY                   07890081
083300        MOVE P27PCA-CRAFT-CD     TO WS-CRAFT-SEL                  07900083
083400        MOVE SPACES              TO FICA-AREA                     07910081
083500        MOVE WS-FICT-KEY         TO FICA-FICT-RECORD-KEY          07920081
083600        MOVE P02-PGM             TO PSTCA-FROM-PROGRAM            07930081
083700     END-IF                                                       07940081
083800     IF PSTCA-FROM-PROGRAM = P27P-PGM                             07950073
083900        MOVE SPACES              TO WS-FICT                       07960073
084000        MOVE SPACES              TO WS-CNTL-FILE                  07970074
084100        MOVE P27PCA-DIST         TO CNTL-DIST                     07980073
084200        MOVE P27PCA-SUB-DIST     TO CNTL-SUB-DIST                 07990073
084300        MOVE P27PCA-POOL         TO CNTL-POOL-CODE                08000073
084400        MOVE 'F'                 TO CNTL-POOL-TYPE                08010073
084500        SET POOL-TYPE-REC        TO TRUE                          08020073
084600        MOVE CNTLKEY-AREA        TO CNTLKEY                       08030073
084700        PERFORM P8000-READ-CNTLFILE                               08040073
084800        IF NOT SUCCESS                                            08050073
084900           IF NO-RECORD-FND                                       08060073
085000*             'POOL SELECTED FOR INQUIRY NOT FOUND'               08070073
085100              MOVE 'P037' TO MSGLOG-CODE                          08080073
085200              PERFORM P9000-SEND-MAP-AND-RETURN                   08090073
085300           ELSE                                                   08100073
085400              MOVE 'P0010-1' TO ERR-PARAGRAPH                     08110073
085500              MOVE CNTLKEY TO ERR-KEY                             08120073
085600              PERFORM P9999-GOT-PROBLEM                           08130073
085700           END-IF                                                 08140073
085800        END-IF                                                    08150073
085900        MOVE P27PCA-DIST         TO FICT-JOB-DIST                 08160073
086000        MOVE P27PCA-SUB-DIST     TO FICT-JOB-SUB-DIST             08170073
086100        SET FICT-POOL-SERVICE    TO TRUE                          08180073
086200        SET FICT-EN-SERVICE      TO TRUE                          08190073
086300        SET FICT-SUPERVISOR-INQ  TO TRUE                          08200073
086400        MOVE 'P'                 TO FICT-JOB-TYPE                 08210073
086500        MOVE P27PCA-POOL         TO FICT-JOB-GRP-PL-XB            08220073
086600        IF CNTL-POOL-CYCLE-CODE = 'B' OR 'S'                      08230073
086700           SET FICT-CALL-ORDER   TO TRUE                          08240075
086800        ELSE                                                      08250073
086900           SET FICT-POSITION-ORDER TO TRUE                        08260073
087000        END-IF                                                    08270073
087100        MOVE '0'                 TO FICT-BOARD                    08280075
087200        MOVE CNTL-POOL-NAME      TO FICT-DESC(1)                  08290073
087300        MOVE CNTL-POOL-NAME      TO FICT-DESC(2)                  08300073
087400     ELSE                                                         08310073
087500        MOVE FICA-FICT-RECORD-KEY TO FICTKEY                      08320073
087600        EXEC CICS READ                                            08330073
087700                  DATASET(FICT-VIA-LOC-SEQ)                       08340073
087800                  INTO(WS-FICT)                                   08350073
087900                  LENGTH(FICTLOSQ-RLGTH)                          08360073
088000                  RIDFLD(FICTKEY)                                 08370073
088100                  KEYLENGTH(FICTLOSQ-KLGTH)                       08380073
088200                  RESP(WS-RESPONSE)                               08390073
088300        END-EXEC                                                  08400073
088400        MOVE WS-RESPONSE TO FILE-STATUS                           08410073
088500        IF NOT SUCCESS                                            08420073
088600           MOVE 'P0000-1'         TO ERR-PARAGRAPH                08430073
088700           MOVE FICTKEY           TO ERR-KEY                      08440073
088800           PERFORM P9999-GOT-PROBLEM                              08450073
088900        END-IF                                                    08460073
089000     END-IF.                                                      08470073
089100*                                                                 08480073
089200 P0100-PROCESS-INPUT.                                             08490071
089300*                                                                 08500071
089400     MOVE SPACES             TO SCR02E-ERRORMSG                   08510071
089500*CNC0564A - BEG                                                   08520071
089600                                WS-3A-BIDPK-TIEBRK-FL             08530071
089700*CNC0564A - END                                                   08540071
089800     IF NOT ENTER-KEY AND NOT PFKEY8                              08550071
089900*            INVALID-FUNC-MSG                                     08560071
090000        MOVE 'I006'          TO MSGLOG-CODE                       08570071
090100        PERFORM P9000-SEND-MAP-AND-RETURN                         08580071
090200     END-IF                                                       08590071
090300     IF FICT-CALL-ORDER                                           08600073
090400        OR FICT-POSITION-ORDER                                    08610073
090500        MOVE FICT-BOARD      TO WS-BOARD                          08620073
090600                                SCR02E-BOARD                      08630071
090700     ELSE                                                         08640071
090800        MOVE FICT-ORDER-BY   TO WS-BOARD                          08650073
090900                                SCR02E-BOARD                      08660071
091000     END-IF                                                       08670071
091100     IF SCR02E-BOARD          = '0'                               08680071
091200        SET HOME-TERMINAL-INQ TO TRUE                             08690071
091300     END-IF                                                       08700071
091400     MOVE FICT-JOB-DIST      TO SCR02E-DIST                       08710073
091500     MOVE FICT-JOB-SUB-DIST  TO SCR02E-SUB-DIST                   08720073
091600     MOVE FICT-DESC(PSTCA-SUB) TO SCR02E-BOARD-DESC               08730073
091700     MOVE FICT-JOB-GRP-PL-XB TO SCR02E-POOL                       08740073
091800*    CNC0006 - FLW, 7/24/96, START                                08750071
091900     IF SCR02E-CRAFT-SEL NOT = (SPACES AND '**')                  08760071
092000        MOVE SCR02E-CRAFT-SEL TO WS-CRAFT-CODE-CHECK              08770071
092100        IF NOT VALID-ASGN-CRAFT                                   08780071
092200*               'INVALID CRAFT CODE'                              08790071
092300           MOVE 'I011' TO MSGLOG-CODE                             08800071
092400           PERFORM P9000-SEND-MAP-AND-RETURN                      08810071
092500        END-IF                                                    08820071
092600     END-IF                                                       08830071
092700     MOVE -1                 TO SCR02E-CRAFT-SEL-CURSOR           08840071
092800*    CNC0006 - FLW, 7/24/96, END                                  08850071
092900*                                                                 08860071
093000     EXEC CICS ASKTIME                                            08870071
093100               ABSTIME(WS-ABSTIME)                                08880071
093200     END-EXEC                                                     08890071
093300     ADD  WS-ABSTIME-OFFSET  TO WS-ABSTIME                        08900071
093400     EXEC CICS FORMATTIME                                         08910071
093500               ABSTIME(WS-ABSTIME)                                08920071
093600               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      08930071
093700               TIME(WS-SYSTEM-TIME-AREA)                          08940071
093800     END-EXEC                                                     08950071
093900*                                                                 08960071
094000*    INSTALL APPLICATION DATE/TIME                                08970071
094100*                                                                 08980071
094200     IF PSTCA-DATE-TIME-OFFSET > SPACES                           08990071
094300        MOVE ZEROS          TO DATE-CONVERSION-PARMS              09000071
094400        MOVE WS-SYSTEM-DATE TO PARM-PRI-DATE-GREG                 09010071
094500        MOVE WS-SYSTEM-TIME TO PARM-PRI-HRMN                      09020071
094600        PERFORM P9810-PROCESS-OFFSET                              09030071
094700        MOVE PARM-RES-DATE-GREG                                   09040071
094800                            TO WS-SYSTEM-DATE                     09050071
094900        MOVE PARM-RES-GREG-CENT                                   09060071
095000                            TO WS-SYSTEM-CENT                     09070071
095100        MOVE PARM-RES-HRMN                                        09080071
095200                            TO WS-SYSTEM-TIME                     09090071
095300     END-IF                                                       09100071
095400*    SET DE-YYMMDD-FORMAT   TO TRUE                               09110071
095500*    MOVE WS-SYSTEM-DATE    TO DE-YYMMDD                          09120071
095600*    PERFORM P8998-DATEEDIT                                       09130071
095700*    MOVE DE-YYMMDD-CE      TO WS-SYSTEM-CENT                     09140071
095800                                                                  09150071
095900     MOVE SPACES            TO TZ-PARAMETERS                      09160071
096000     SET TZ-IN-EASTERN-ZONE TO TRUE                               09170071
096100     MOVE WS-PRESENT-TIME   TO TZ-IN-DATE-TIME                    09180071
096200     MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                        09190071
096300     PERFORM P8996-TIMEZONE                                       09200071
096400     MOVE TZ-OUT-DATE-TIME  TO WS-LOCAL-DATE-TIME                 09210071
096500     MOVE TZ-OUT-CE         TO WS-LOCAL-CENT                      09220071
096600*CNC0564A - BEG                                                   09230071
096700*CNC0600-B                                                        09231099
096800     MOVE WS-LOCAL-DATE-TIME-CENT TO WS-LOCAL-CURRENT-DTTM-CENT   09231199
           MOVE WS-LOCAL-CURRENT-DATE-YYMMDD TO WS-LOCAL-CUR-DATE-YYMMDD
           MOVE WS-LOCAL-CURRENT-TIME        TO WS-LOCAL-CUR-TIME-HHMM
097000*CNC0600-E                                                        09232099
097100*                                                                 09240071
097200*    GET CREW PROFILE CONTROL RECORD                              09250071
097300*                                                                 09260071
097400     IF FICT-CALL-ORDER                                           09270073
097500        MOVE SPACES          TO WORK-CNTLKEY                      09280071
097600        MOVE '3A'            TO WK-CNTL-REC-TYPE                  09290071
097700        MOVE SCR02E-DIST     TO WK-CNTL-DIST                      09300071
097800        MOVE SCR02E-SUB-DIST TO WK-CNTL-SUB-DIST                  09310071
097900        MOVE SCR02E-POOL     TO WK-CNTL-POOL                      09320071
098000        MOVE 'F'             TO WK-CNTL-POOL-TYPE                 09330071
098100        MOVE WORK-CNTLKEY    TO CNTLKEY                           09340071
098200        EXEC CICS READ                                            09350071
098300                  DATASET(CNTL-FILE-VIA-CNTLKEY)                  09360071
098400                  INTO(WS-CNTL-FILE)                              09370071
098500                  LENGTH(CNTLFILE-RLGTH)                          09380071
098600                  RIDFLD(CNTLKEY)                                 09390071
098700                  KEYLENGTH(CNTLFILE-KLGTH)                       09400071
098800                  RESP(WS-RESPONSE)                               09410071
098900        END-EXEC                                                  09420071
099000        MOVE WS-RESPONSE TO FILE-STATUS                           09430071
099100        IF NOT SUCCESS                                            09440071
099200           IF NO-RECORD-FND                                       09450071
099300*                  'POOL SELECTED FOR INQUIRY NOT FOUND'          09460071
099400              MOVE 'P037' TO MSGLOG-CODE                          09470071
099500              PERFORM P9000-SEND-MAP-AND-RETURN                   09480071
099600           ELSE                                                   09490071
099700              MOVE 'P0100-3A' TO ERR-PARAGRAPH                    09500071
099800              MOVE CNTLKEY TO ERR-KEY                             09510071
099900              PERFORM P9999-GOT-PROBLEM                           09520071
100000           END-IF                                                 09530071
100100        ELSE                                                      09540071
100200           MOVE CNTL-3A-BIDPK-TIEBRK-FL TO WS-3A-BIDPK-TIEBRK-FL  09550071
100300           IF WS-3A-BIDPK-TIEBRK-PW-BRD                           09560071
100400              CONTINUE                                            09570071
100500           ELSE                                                   09580071
100600              MOVE SPACES            TO SCR02E-START-DATE         09590071
100700                                        SCR02E-START-TIME         09600071
100800              MOVE AUTOSKIP-DARK     TO SCR02E-START-DATE-ATTR    09610071
100900                                        SCR02E-START-TIME-ATTR    09620071
101000           END-IF                                                 09630071
101100        END-IF                                                    09640071
101200     ELSE                                                         09650071
101300        MOVE SPACES                  TO SCR02E-START-DATE         09660071
101400                                        SCR02E-START-TIME         09670071
101500        MOVE AUTOSKIP-DARK           TO SCR02E-START-DATE-ATTR    09680071
101600                                        SCR02E-START-TIME-ATTR    09690071
101700     END-IF                                                       09700071
101800*CNC0564A - END                                                   09710071
101900*CNC0600-B - GET THE NEW CAN WRR FLAG                             09710197
102000     MOVE SPACES                 TO WORK-CNTLKEY                  09711097
102100     MOVE '02'                   TO WK-CNTL-REC-TYPE              09712097
102200     MOVE SCR02E-DIST            TO WK-CNTL-DIST                  09713097
102300     MOVE SCR02E-SUB-DIST        TO WK-CNTL-SUB-DIST              09714097
102400     MOVE WORK-CNTLKEY           TO CNTLKEY                       09715097
102500     EXEC CICS READ                                               09716097
102600               DATASET(CNTL-FILE-VIA-CNTLKEY)                     09717097
102700               INTO(WS-CNTL-FILE)                                 09718097
102800               LENGTH(CNTLFILE-RLGTH)                             09719097
102900               RIDFLD(CNTLKEY)                                    09719197
103000               KEYLENGTH(CNTLFILE-KLGTH)                          09719297
103100               RESP(WS-RESPONSE)                                  09719397
103200     END-EXEC                                                     09719497
103300     MOVE WS-RESPONSE TO FILE-STATUS                              09719597
103400     IF NOT SUCCESS                                               09719697
103500        MOVE 'P0100-2' TO ERR-PARAGRAPH                           09719797
103600        MOVE CNTLKEY      TO ERR-KEY                              09719897
103700        PERFORM P9999-GOT-PROBLEM                                 09719997
103800     END-IF                                                       09720097
103900     MOVE CNTL-CAN-WRR-FLAG TO WS-CAN-WRR-FLAG                    09720197
104000*CNC0600-E                                                        09720297
104100*                                                                 09721071
104200*                                                                 09730071
104300*     GET POOL CONTROL REC FOR PROCESSING                         09740071
104400*                                                                 09750071
104500     MOVE SPACES             TO WORK-CNTLKEY                      09760071
104600     MOVE '03'               TO WK-CNTL-REC-TYPE                  09770071
104700     MOVE SCR02E-DIST        TO WK-CNTL-DIST                      09780071
104800     MOVE SCR02E-SUB-DIST    TO WK-CNTL-SUB-DIST                  09790071
104900     MOVE SCR02E-POOL        TO WK-CNTL-POOL                      09800071
105000     MOVE 'F'                TO WK-CNTL-POOL-TYPE                 09810071
105100     MOVE WORK-CNTLKEY       TO CNTLKEY                           09820071
105200     EXEC CICS READ                                               09830071
105300               DATASET(CNTL-FILE-VIA-CNTLKEY)                     09840071
105400               INTO(WS-CNTL-FILE)                                 09850071
105500               LENGTH(CNTLFILE-RLGTH)                             09860071
105600               RIDFLD(CNTLKEY)                                    09870071
105700               KEYLENGTH(CNTLFILE-KLGTH)                          09880071
105800               RESP(WS-RESPONSE)                                  09890071
105900     END-EXEC                                                     09900071
106000     MOVE WS-RESPONSE TO FILE-STATUS                              09910071
106100     IF NOT SUCCESS                                               09920071
106200        IF NO-RECORD-FND                                          09930071
106300*               'POOL SELECTED FOR INQUIRY NOT FOUND'             09940071
106400           MOVE 'P037' TO MSGLOG-CODE                             09950071
106500           PERFORM P9000-SEND-MAP-AND-RETURN                      09960071
106600        ELSE                                                      09970071
106700           MOVE 'P0100-1' TO ERR-PARAGRAPH                        09980071
106800           MOVE CNTLKEY   TO ERR-KEY                              09990071
106900           PERFORM P9999-GOT-PROBLEM                              10000071
107000        END-IF                                                    10010071
107100     END-IF                                                       10020071
107200                                                                  10030071
107300     PERFORM P9830-SNAPSHOT-UFP                                   10040071
107400                                                                  10050071
107500     MOVE CNTL-EN-FI    TO EN-FI-MARRIED-FLAG                     10060071
107600     IF EN-FI-MARRIED                                             10070071
107700        IF CNTL-POOL-FI-CRAFT NOT = ('Y' AND 'O')                 10080071
107800           SET EN-SE-MARRIED TO TRUE                              10090071
107900        END-IF                                                    10100071
108000     END-IF                                                       10110071
108100     MOVE CNTL-CO-B1-B2 TO CO-BK-MARRIED-FLAG                     10120071
108200     IF CO-BK-MARRIED                                             10130071
108300        IF CNTL-POOL-B1-CRAFT = ('Y' OR 'O')                      10140071
108400           SET CO-B1-MARRIED TO TRUE                              10150071
108500        END-IF                                                    10160071
108600        IF CNTL-POOL-B2-CRAFT = ('Y' OR 'O')                      10170071
108700           SET CO-B2-MARRIED TO TRUE                              10180071
108800        END-IF                                                    10190071
108900        IF CNTL-POOL-BG-CRAFT = ('Y' OR 'O')                      10200071
109000           SET CO-BG-MARRIED TO TRUE                              10210071
109100        END-IF                                                    10220071
109200     END-IF                                                       10230071
109300     MOVE CNTL-B1-B2         TO B1-B2-MARRIED-FLAG                10240071
109400     MOVE CNTL-EN-ET         TO EN-ET-MARRIED-FLAG                10250071
109500     MOVE CNTL-CO-TT         TO CO-TT-MARRIED-FLAG                10260071
109600     IF CNTL-ALT-DIST > SPACE                                     10270071
109700        AND CNTL-ALT-SUB-DIST > SPACE                             10280071
109800        IF CNTL-ALT-EN-POOL > SPACE                               10290071
109900           SET EN-ID-POOL    TO TRUE                              10300071
110000        END-IF                                                    10310071
110100        IF CNTL-ALT-TR-POOL > SPACE                               10320071
110200           SET TR-ID-POOL    TO TRUE                              10330071
110300        END-IF                                                    10340071
110400     END-IF                                                       10350071
110500     MOVE CNTL-POOL-REST-CODE TO HOLD-POOL-REST-CODE              10360071
110600*                                                                 10370071
110700*    'LOAD' THE CRAFT ARRAY IN WORKING STORAGE                    10380071
110800*                                                                 10390071
110900     MOVE SPACES             TO WS-CRAFT-AREA                     10400071
111000     MOVE ZEROS              TO CRAFT-ARRAY-SUB                   10410071
111100*    CNC0006 - FLW, 7/24/96, START                                10420071
111200     IF (FICT-SUPERVISOR-INQ OR                                   10430073
111300         FICT-EN-SERVICE) AND                                     10440073
111400        (SCR02E-CRAFT-SEL = SPACES OR '**')                       10450071
111500*    CNC0006 - FLW, 7/24/96, END                                  10460071
111600        IF CNTL-POOL-EN-CRAFT = 'Y'                               10470071
111700           ADD 1             TO CRAFT-ARRAY-SUB                   10480071
111800           MOVE 'EN'         TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    10490071
111900           IF EN-FI-MARRIED                                       10500071
112000              ADD 1          TO CRAFT-ARRAY-SUB                   10510071
112100              MOVE 'EN'      TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   10520071
112200              IF EN-SE-MARRIED                                    10530071
112300                 MOVE 'SE'   TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    10540071
112400                 IF CNTL-POOL-SE-CRAFT = 'O'                      10550071
112500                    SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)           10560071
112600                             TO TRUE                              10570071
112700                 END-IF                                           10580071
112800              ELSE                                                10590071
112900                 MOVE 'FI'   TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    10600071
113000                 IF CNTL-POOL-FI-CRAFT = 'O'                      10610071
113100                    SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)           10620071
113200                             TO TRUE                              10630071
113300                 END-IF                                           10640071
113400              END-IF                                              10650071
113500           END-IF                                                 10660071
113600*                                                                 10670071
113700*          DO NOT DISPLAY STUDENTS ON FIELD INQUIRIES             10680071
113800*                                                                 10690071
113900*          IF EN-ET-MARRIED                                       10700071
114000*             ADD 1          TO CRAFT-ARRAY-SUB                   10710071
114100*             MOVE 'ET'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    10720071
114200*             MOVE 'EN'      TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   10730071
114300*             SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)                 10740071
114400*                            TO TRUE                              10750071
114500*          END-IF                                                 10760071
114600*                                                                 10770071
114700        END-IF                                                    10780071
114800        IF CNTL-POOL-FI-CRAFT = ('Y' OR 'O')                      10790071
114900           IF NOT EN-FI-MARRIED                                   10800071
115000              ADD 1          TO CRAFT-ARRAY-SUB                   10810071
115100              MOVE 'FI'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    10820071
115200              IF CNTL-POOL-FI-CRAFT = 'O'                         10830071
115300                 SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)              10840071
115400                             TO TRUE                              10850071
115500              END-IF                                              10860071
115600           END-IF                                                 10870071
115700        END-IF                                                    10880071
115800        IF CNTL-POOL-SE-CRAFT = ('Y' OR 'O')                      10890071
115900           IF NOT EN-FI-MARRIED                                   10900071
116000              ADD 1          TO CRAFT-ARRAY-SUB                   10910071
116100              MOVE 'SE'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    10920071
116200              IF CNTL-POOL-SE-CRAFT = 'O'                         10930071
116300                 SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)              10940071
116400                             TO TRUE                              10950071
116500              END-IF                                              10960071
116600           END-IF                                                 10970071
116700        END-IF                                                    10980071
116800*                                                                 10990071
116900*       DO NOT DISPLAY STUDENTS ON FIELD INQUIRIES                11000071
117000*                                                                 11010071
117100*       IF NOT EN-ET-MARRIED                                      11020071
117200*          ADD 1             TO CRAFT-ARRAY-SUB                   11030071
117300*          MOVE 'ET'         TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11040071
117400*          SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)                    11050071
117500*                            TO TRUE                              11060071
117600*       END-IF                                                    11070071
117700*                                                                 11080071
117800     END-IF                                                       11090071
117900*    CNC0006 - FLW, 7/24/96, START                                11100071
118000     IF (FICT-SUPERVISOR-INQ OR                                   11110073
118100         FICT-POOL-SERVICE) AND                                   11120073
118200        (SCR02E-CRAFT-SEL = SPACES OR '**')                       11130071
118300*    CNC0006 - FLW, 7/24/96, END                                  11140071
118400        IF CNTL-POOL-CO-CRAFT = 'Y'                               11150071
118500           ADD 1             TO CRAFT-ARRAY-SUB                   11160071
118600           MOVE 'CO'         TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11170071
118700           IF CO-BK-MARRIED                                       11180071
118800              IF CO-B1-MARRIED                                    11190071
118900                 ADD 1       TO CRAFT-ARRAY-SUB                   11200071
119000                 MOVE 'B1'   TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11210071
119100                 MOVE 'CO'   TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   11220071
119200                 IF CNTL-POOL-B1-CRAFT = 'O'                      11230071
119300                    SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)           11240071
119400                             TO TRUE                              11250071
119500                 END-IF                                           11260071
119600              END-IF                                              11270071
119700              IF CO-B2-MARRIED                                    11280071
119800                 ADD 1       TO CRAFT-ARRAY-SUB                   11290071
119900                 MOVE 'B2'   TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11300071
120000                 MOVE 'CO'   TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   11310071
120100                 IF CNTL-POOL-B2-CRAFT = 'O'                      11320071
120200                    SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)           11330071
120300                             TO TRUE                              11340071
120400                 END-IF                                           11350071
120500              END-IF                                              11360071
120600              IF CO-BG-MARRIED                                    11370071
120700                 ADD 1       TO CRAFT-ARRAY-SUB                   11380071
120800                 MOVE 'BG'   TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11390071
120900                 MOVE 'CO'   TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   11400071
121000                 IF CNTL-POOL-BG-CRAFT = 'O'                      11410071
121100                    SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)           11420071
121200                             TO TRUE                              11430071
121300                 END-IF                                           11440071
121400              END-IF                                              11450071
121500           END-IF                                                 11460071
121600*                                                                 11470071
121700*          DO NOT DISPLAY STUDENTS ON FIELD INQUIRIES             11480071
121800*                                                                 11490071
121900*          IF CO-TT-MARRIED                                       11500071
122000*             ADD 1          TO CRAFT-ARRAY-SUB                   11510071
122100*             MOVE 'TT'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11520071
122200*             MOVE 'CO'      TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   11530071
122300*             SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)                 11540071
122400*                            TO TRUE                              11550071
122500*          END-IF                                                 11560071
122600*                                                                 11570071
122700        END-IF                                                    11580071
122800        IF CNTL-POOL-B1-CRAFT = ('Y' OR 'O')                      11590071
122900           IF NOT CO-BK-MARRIED                                   11600071
123000              ADD 1          TO CRAFT-ARRAY-SUB                   11610071
123100              MOVE 'B1'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11620071
123200              IF CNTL-POOL-B1-CRAFT = 'O'                         11630071
123300                 SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)              11640071
123400                             TO TRUE                              11650071
123500              END-IF                                              11660071
123600              IF B1-B2-MARRIED                                    11670071
123700                 ADD 1       TO CRAFT-ARRAY-SUB                   11680071
123800                 MOVE 'B2'   TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11690071
123900                 MOVE 'B1'   TO WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)   11700071
124000                 IF CNTL-POOL-B2-CRAFT = 'O'                      11710071
124100                    SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)           11720071
124200                             TO TRUE                              11730071
124300                 END-IF                                           11740071
124400              END-IF                                              11750071
124500           END-IF                                                 11760071
124600        END-IF                                                    11770071
124700        IF CNTL-POOL-B2-CRAFT = ('Y' OR 'O')                      11780071
124800           IF NOT (CO-BK-MARRIED OR B1-B2-MARRIED)                11790071
124900              ADD 1          TO CRAFT-ARRAY-SUB                   11800071
125000              MOVE 'B2'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11810071
125100              IF CNTL-POOL-B2-CRAFT = 'O'                         11820071
125200                 SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)              11830071
125300                             TO TRUE                              11840071
125400              END-IF                                              11850071
125500           END-IF                                                 11860071
125600        END-IF                                                    11870071
125700        IF CNTL-POOL-BG-CRAFT = ('Y' OR 'O')                      11880071
125800           IF NOT CO-BG-MARRIED                                   11890071
125900              ADD 1          TO CRAFT-ARRAY-SUB                   11900071
126000              MOVE 'BG'      TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    11910071
126100              IF CNTL-POOL-BG-CRAFT = 'O'                         11920071
126200                 SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)              11930071
126300                             TO TRUE                              11940071
126400              END-IF                                              11950071
126500           END-IF                                                 11960071
126600        END-IF                                                    11970071
126700        IF CNTL-POOL-AC-CRAFT = ('Y' OR 'O')                      11980071
126800           ADD 1             TO CRAFT-ARRAY-SUB                   11990071
126900           MOVE 'AC'         TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    12000071
127000           IF CNTL-POOL-AC-CRAFT = 'O'                            12010071
127100              SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)                 12020071
127200                             TO TRUE                              12030071
127300           END-IF                                                 12040071
127400        END-IF                                                    12050071
127500*                                                                 12060071
127600*       DO NOT DISPLAY STUDENTS ON FIELD INQUIRIES                12070071
127700*                                                                 12080071
127800*       IF NOT CO-TT-MARRIED                                      12090071
127900*          ADD 1             TO CRAFT-ARRAY-SUB                   12100071
128000*          MOVE 'TT'         TO WS-CRAFT-CODE(CRAFT-ARRAY-SUB)    12110071
128100*          SET OPTIONAL-CRAFT(CRAFT-ARRAY-SUB)                    12120071
128200*                            TO TRUE                              12130071
128300*       END-IF                                                    12140071
128400*                                                                 12150071
128500     END-IF                                                       12160071
128600*                                                                 12170071
128700*    CNC0006 - FLW, 7/24/96, START                                12180071
128800*                                                                 12190071
128900     IF SCR02E-CRAFT-SEL NOT = (SPACES AND '**')                  12200071
129000        IF (SCR02E-CRAFT-SEL = 'EN'                               12210071
129100              AND CNTL-POOL-EN-CRAFT = 'Y') OR                    12220071
129200           (SCR02E-CRAFT-SEL = 'FI'                               12230071
129300              AND CNTL-POOL-FI-CRAFT = ('Y' OR 'O')) OR           12240071
129400           (SCR02E-CRAFT-SEL = 'CO'                               12250071
129500              AND CNTL-POOL-CO-CRAFT = 'Y') OR                    12260071
129600           (SCR02E-CRAFT-SEL = 'B1'                               12270071
129700              AND CNTL-POOL-B1-CRAFT = ('Y' OR 'O')) OR           12280071
129800           (SCR02E-CRAFT-SEL = 'B2'                               12290071
129900              AND CNTL-POOL-B2-CRAFT = ('Y' OR 'O')) OR           12300071
130000           (SCR02E-CRAFT-SEL = 'SE'                               12310071
130100              AND CNTL-POOL-SE-CRAFT = ('Y' OR 'O')) OR           12320071
130200           (SCR02E-CRAFT-SEL = 'BG'                               12330071
130300              AND CNTL-POOL-BG-CRAFT = ('Y' OR 'O')) OR           12340071
130400           (SCR02E-CRAFT-SEL = 'AC'                               12350071
130500              AND CNTL-POOL-AC-CRAFT = ('Y' OR 'O')) OR           12360071
130600           (SCR02E-CRAFT-SEL = ('ET' OR 'TT'))                    12370071
130700             ADD 1                 TO CRAFT-ARRAY-SUB             12380071
130800             MOVE SCR02E-CRAFT-SEL TO                             12390071
130900                                    WS-CRAFT-CODE(CRAFT-ARRAY-SUB)12400071
131000             CONTINUE                                             12410071
131100        ELSE                                                      12420071
131200           MOVE -1        TO SCR02E-CRAFT-SEL-CURSOR              12430071
131300           MOVE REV-VIDEO TO SCR02E-CRAFT-SEL-HI                  12440071
131400*               INVALID-CRAFT-MSG                                 12450071
131500           MOVE 'I011' TO MSGLOG-CODE                             12460071
131600           PERFORM P9000-SEND-MAP-AND-RETURN                      12470071
131700        END-IF                                                    12480071
131800     END-IF                                                       12490071
131900*                                                                 12500071
132000*    CNC0006 - FLW, 7/24/96, END                                  12510071
132100*                                                                 12520071
132200     IF FICT-CALL-ORDER                                           12530073
132300        SET CALL-ORDER-REQ TO TRUE                                12540071
132400        PERFORM P1005-CALL-ORDER-INQ                              12550071
132500     ELSE                                                         12560071
132600        MOVE SPACES        TO CALL-ORDER-FLAG                     12570071
132700        PERFORM P1000-INQUIRY                                     12580071
132800     END-IF.                                                      12590071
132900*                                                                 12600071
133000 P1000-INQUIRY.                                                   12610071
133100*                                                                 12620071
133200     IF POSITION-BOARD                                            12630071
133300        IF PSTCA-SUB = 1                                          12640071
133400           MOVE WS-VARIABLE-LINE-3-HDR TO SCR02E-HEADER           12650071
133500        ELSE                                                      12660071
133600           MOVE WS-VARIABLE-LINE-3-HDR-FRENCH TO SCR02E-HEADER    12670071
133700        END-IF                                                    12680071
133800     ELSE                                                         12690071
133900        IF PSTCA-SUB = 1                                          12700071
134000           MOVE WS-VARIABLE-LINE-1-HDR TO SCR02E-HEADER           12710071
134100        ELSE                                                      12720071
134200           MOVE WS-VARIABLE-LINE-1-HDR-FRENCH TO SCR02E-HEADER    12730071
134300        END-IF                                                    12740071
134400     END-IF                                                       12750071
134500*                                                                 12760071
134600     PERFORM P1010-INITIALIZE-CRAFT-ARRAY                         12770071
134700*                                                                 12780071
134800*    DETERMINE IF THIS IS A VALID SCROLL, AND IF SO LOAD          12790071
134900*    THE UFP TURN KEY WE SAVED OFF IN THE COMMAREA SO WE          12800071
135000*    CAN SETUP FOR NEXT RECORD.                                   12810071
135100*                                                                 12820071
135200     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  12830071
135300        UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX                   12840071
135400        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 12850071
135500           MOVE WS-CRAFT-MAX(CRAFT-ARRAY-SUB) TO K                12860071
135600           IF PFKEY8                                              12870071
135700              AND P02ECA-TURN-KEY(K) > SPACE                      12880071
135800              MOVE P02ECA-TURN-KEY(K)                             12890071
135900                 TO WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)          12900071
136000           ELSE                                                   12910071
136100              MOVE ZEROS TO P02ECA-HOLD-POS(CRAFT-ARRAY-SUB)      12920071
136200              MOVE SPACES TO WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB) 12930071
136300           END-IF                                                 12940071
136400        ELSE                                                      12950071
136500           MOVE ZEROS TO P02ECA-HOLD-POS(CRAFT-ARRAY-SUB)         12960071
136600           MOVE SPACES TO WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)    12970071
136700        END-IF                                                    12980071
136800     END-PERFORM                                                  12990071
136900*                                                                 13000071
137000*    CLEAR OUT THE SCREEN ARRAY AND THE UFPTURN KEY ARRAY IN THE  13010071
137100*    COMMAREA                                                     13020071
137200*                                                                 13030071
137300     PERFORM VARYING ARRAY-SUB FROM 1 BY 1                        13040071
137400             UNTIL ARRAY-SUB > ARRAY-MAX                          13050071
137500        MOVE SPACES TO SCR02E-CC(ARRAY-SUB)                       13060071
137600                       SCR02E-TURN(ARRAY-SUB)                     13070071
137700                       SCR02E-I-O(ARRAY-SUB)                      13080071
137800                       SCR02E-POS(ARRAY-SUB)                      13090071
137900                       SCR02E-NAME(ARRAY-SUB)                     13100071
138000                       SCR02E-LO(ARRAY-SUB)                       13110071
138100                       SCR02E-VARIABLE(ARRAY-SUB)                 13120071
138200        IF ENTER-KEY                                              13130071
138300           MOVE SPACES TO P02ECA-TURN-KEY(ARRAY-SUB)              13140071
138400        END-IF                                                    13150071
138500     END-PERFORM                                                  13160071
138600*                                                                 13170071
138700*    LOAD NEW CRAFT ARRAY ON SCREEN                               13180071
138800*                                                                 13190071
138900     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  13200071
139000             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              13210071
139100        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACES                13220071
139200           MOVE WS-CRAFT-SUB(CRAFT-ARRAY-SUB) TO ARRAY-SUB        13230071
139300           MOVE '0' TO DONE-CODE                                  13240071
139400           PERFORM UNTIL DONE                                     13250071
139500              MOVE WS-CRAFT-CODE(CRAFT-ARRAY-SUB)                 13260071
139600                 TO SCR02E-CC(ARRAY-SUB)                          13270071
139700*CNC0564A - BEG                                                   13280071
139800              MOVE CYAN  TO SCR02E-VARIABLE-COLOR(ARRAY-SUB)      13290071
139900*CNC0564A - END                                                   13300071
140000              PERFORM VARYING CT-IND FROM 1 BY 1                  13310071
140100                 UNTIL CT-IND > WS-CRAFT-TABLE-MAX                13320071
140200                 IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB)                13330071
140300                    = CT-CRAFT-CODE(CT-IND)                       13340071
140400                    MOVE CT-CRAFT-COLOR(CT-IND)                   13350071
140500                       TO SCR02E-CC-COLOR(ARRAY-SUB)              13360071
140600                          SCR02E-I-O-COLOR(ARRAY-SUB)             13370071
140700                          SCR02E-POS-COLOR(ARRAY-SUB)             13380071
140800                          SCR02E-NAME-COLOR(ARRAY-SUB)            13390071
140900*CNC0564A - BEG                                                   13400071
141000                          SCR02E-LO-COLOR(ARRAY-SUB)              13410071
141100                    MOVE WHITE TO SCR02E-TURN-COLOR(ARRAY-SUB)    13420071
141200*CNC0564A - END                                                   13430071
141300                    SET CT-IND TO WS-CRAFT-TABLE-MAX              13440071
141400                 END-IF                                           13450071
141500              END-PERFORM                                         13460071
141600              ADD CRAFT-SUB-INCREMENT TO ARRAY-SUB                13470071
141700              IF ARRAY-SUB > WS-CRAFT-MAX(CRAFT-ARRAY-SUB)        13480071
141800                 SET DONE TO TRUE                                 13490071
141900              END-IF                                              13500071
142000           END-PERFORM                                            13510071
142100        END-IF                                                    13520071
142200     END-PERFORM                                                  13530071
142300                                                                  13540071
142400     MOVE CNTL-POOL-SVC     TO POOL-SERVICE-CODE                  13550071
142500*                                                                 13560071
142600*    BEGIN PROCESSING                                             13570071
142700*                                                                 13580071
142800     MOVE ZEROS TO ARRAY-SUB                                      13590071
142900     IF POSITION-BOARD                                            13600071
143000        PERFORM P1020-INQUIRE-BY-POSITION                         13610071
143100     ELSE                                                         13620071
143200        PERFORM P1090-INQUIRE-BY-TURN                             13630071
143300     END-IF                                                       13640071
143400*         'END OF POOL LISTING'                                   13650071
143500     MOVE 'E007' TO MSGLOG-CODE                                   13660071
143600     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  13670071
143700             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              13680071
143800        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 13690071
143900           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 13700071
144000           IF WS-CRAFT-SUB(CRAFT-ARRAY-SUB)                       13710071
144100              > WS-CRAFT-MAX(CRAFT-ARRAY-SUB)                     13720071
144200              MOVE SPACES TO SCR02E-ERRORMSG                      13730071
144300              MOVE SPACES TO MSGLOG-CODE                          13740071
144400           ELSE                                                   13750071
144500              MOVE WS-CRAFT-MAX(CRAFT-ARRAY-SUB) TO K             13760071
144600              MOVE 'DONE' TO P02ECA-TURN-KEY(K)                   13770071
144700           END-IF                                                 13780071
144800        END-IF                                                    13790071
144900     END-PERFORM.                                                 13800071
145000*                                                                 13810071
145100 P1005-CALL-ORDER-INQ.                                            13820071
145200*                                                                 13830071
145300*C1104 - BEG                                                      13840088
145400     MOVE CNTL-POOL-HOME-LEAD-TIME   TO WS-POOL-HOME-LEAD-TIME    13850088
145500*C1104 - END                                                      13860088
145600*CNC0564A - BEG                                                   13870071
145700***  MOVE WS-LOCAL-DATE-TIME-CENT          TO HOLD-START-DATE-TIME13880071
145800     IF WS-3A-BIDPK-TIEBRK-PW-BRD                                 13890071
145900        IF SCR02E-START-DATE > SPACES                             13900071
146000           SET DE-YYMMDD-FORMAT      TO TRUE                      13910071
146100           MOVE SCR02E-START-DATE    TO DE-YYMMDD                 13920071
146200           PERFORM P8998-DATEEDIT                                 13930071
146300           IF DE-INVALID-DATE                                     13940071
146400              MOVE -1                TO SCR02E-START-DATE-CURSOR  13950071
146500              MOVE REV-VIDEO         TO SCR02E-START-DATE-HI      13960071
146600*                'INVALID DATE'                                   13970071
146700              MOVE 'I042'            TO MSGLOG-CODE               13980071
146800              PERFORM P9000-SEND-MAP-AND-RETURN                   13990071
146900           END-IF                                                 14000071
147000           MOVE DE-CCYYMMDD          TO HOLD-START-DATE           14010071
147100                                                                  14020071
147200           IF SCR02E-START-TIME NOT > SPACES                      14030071
147300              MOVE '0001'            TO SCR02E-START-TIME         14040071
147400                                           HOLD-START-TIME        14050071
147500           ELSE                                                   14060071
147600              MOVE SCR02E-START-TIME TO TE-MILITARY-TIME          14070071
147700              SET TE-MILITARY-FORMAT TO TRUE                      14080071
147800              PERFORM P8997-TIMEEDIT                              14090071
147900              IF TE-INVALID-TIME                                  14100071
148000                 MOVE -1             TO SCR02E-START-TIME-CURSOR  14110071
148100                 MOVE REV-VIDEO      TO SCR02E-START-TIME-HI      14120071
148200*                     INVALID-TIME-MSG                            14130071
148300                 MOVE 'I022'         TO MSGLOG-CODE               14140071
148400                 PERFORM P9000-SEND-MAP-AND-RETURN                14150071
148500              END-IF                                              14160071
148600              MOVE SCR02E-START-TIME TO HOLD-START-TIME           14170071
148700           END-IF                                                 14180071
148800        ELSE                                                      14190071
148900           MOVE WS-LOCAL-DATE-TIME-CENT TO HOLD-START-DATE-TIME   14200071
149000           MOVE WS-LOCAL-DATE        TO SCR02E-START-DATE         14210071
149100           MOVE WS-LOCAL-TIME        TO SCR02E-START-TIME         14220071
149200        END-IF                                                    14230071
149300***                                                               14240071
149400     ELSE                                                         14250071
149500        MOVE WS-LOCAL-DATE-TIME-CENT       TO HOLD-START-DATE-TIME14260071
149600     END-IF                                                       14270071
149700*CNC0564A - END                                                   14280071
149800*CNC0564A - 04/29/15 - BEG                                        14290071
149900     IF WS-3A-BIDPK-TIEBRK-PW-BRD                                 14300071
150000        IF PSTCA-SUB = 1                                          14310071
150100           MOVE WS-VARIABLE-LINE-5-HDR     TO SCR02E-HEADER       14320071
150200        ELSE                                                      14330071
150300           MOVE WS-VARIABLE-LINE-5-HDR-FRENCH                     14340071
150400                                           TO SCR02E-HEADER       14350071
150500        END-IF                                                    14360071
150600     ELSE                                                         14370071
150700        IF PSTCA-SUB = 1                                          14380071
150800           MOVE WS-VARIABLE-LINE-4-HDR     TO SCR02E-HEADER       14390071
150900        ELSE                                                      14400071
151000           MOVE WS-VARIABLE-LINE-4-HDR-FRENCH                     14410071
151100                                           TO SCR02E-HEADER       14420071
151200        END-IF                                                    14430071
151300     END-IF                                                       14440071
151400*CNC0564A - 04/29/15 - END                                        14450071
151500                                                                  14460071
151600     PERFORM P1010-INITIALIZE-CRAFT-ARRAY                         14470071
151700*                                                                 14480071
151800*    DETERMINE IF THIS IS A VALID SCROLL, AND IF SO LOAD          14490071
151900*    THE UFP TURN KEY WE SAVED OFF IN THE COMMAREA SO WE          14500071
152000*    CAN SETUP FOR NEXT RECORD.                                   14510071
152100*                                                                 14520071
152200     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  14530071
152300        UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX                   14540071
152400        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 14550071
152500           MOVE WS-CRAFT-MAX(CRAFT-ARRAY-SUB) TO K                14560071
152600           IF PFKEY8                                              14570071
152700              AND P02ECA-TURN-KEY(K) > SPACE                      14580071
152800              MOVE P02ECA-TURN-KEY(K)                             14590071
152900                          TO WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB) 14600071
153000              IF P02ECA-CALL-ORDER-KEY(K) > SPACES                14610071
153100                 MOVE P02ECA-CALL-ORDER-KEY(K)                    14620071
153200                          TO WS-CALL-ORDER-KEY(CRAFT-ARRAY-SUB)   14630071
153300              END-IF                                              14640071
153400           ELSE                                                   14650071
153500              MOVE ZEROS TO P02ECA-HOLD-POS(CRAFT-ARRAY-SUB)      14660071
153600              MOVE SPACES TO WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB) 14670071
153700                             WS-CALL-ORDER-KEY(CRAFT-ARRAY-SUB)   14680071
153800           END-IF                                                 14690071
153900        ELSE                                                      14700071
154000           MOVE ZEROS TO P02ECA-HOLD-POS(CRAFT-ARRAY-SUB)         14710071
154100           MOVE SPACES TO WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)    14720071
154200                          WS-CALL-ORDER-KEY(CRAFT-ARRAY-SUB)      14730071
154300        END-IF                                                    14740071
154400     END-PERFORM                                                  14750071
154500*                                                                 14760071
154600*    CLEAR OUT THE SCREEN ARRAY AND THE UFPTURN KEY ARRAY IN THE  14770071
154700*    COMMAREA                                                     14780071
154800*                                                                 14790071
154900     PERFORM VARYING ARRAY-SUB FROM 1 BY 1                        14800071
155000             UNTIL ARRAY-SUB > ARRAY-MAX                          14810071
155100        MOVE SPACES TO SCR02E-CC(ARRAY-SUB)                       14820071
155200                       SCR02E-TURN(ARRAY-SUB)                     14830071
155300                       SCR02E-I-O(ARRAY-SUB)                      14840071
155400                       SCR02E-POS(ARRAY-SUB)                      14850071
155500                       SCR02E-NAME(ARRAY-SUB)                     14860071
155600                       SCR02E-LO(ARRAY-SUB)                       14870071
155700                       SCR02E-VARIABLE(ARRAY-SUB)                 14880071
155800        IF ENTER-KEY                                              14890071
155900           MOVE SPACES TO P02ECA-TURN-KEY(ARRAY-SUB)              14900071
156000                          P02ECA-CALL-ORDER-KEY(ARRAY-SUB)        14910071
156100        END-IF                                                    14920071
156200     END-PERFORM                                                  14930071
156300*                                                                 14940071
156400*    LOAD NEW CRAFT ARRAY ON SCREEN                               14950071
156500*                                                                 14960071
156600     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  14970071
156700             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              14980071
156800        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACES                14990071
156900           MOVE WS-CRAFT-SUB(CRAFT-ARRAY-SUB) TO ARRAY-SUB        15000071
157000           MOVE '0' TO DONE-CODE                                  15010071
157100           PERFORM UNTIL DONE                                     15020071
157200              MOVE WS-CRAFT-CODE(CRAFT-ARRAY-SUB)                 15030071
157300                 TO SCR02E-CC(ARRAY-SUB)                          15040071
157400*CNC0564A - BEG                                                   15050071
157500              MOVE CYAN  TO SCR02E-VARIABLE-COLOR(ARRAY-SUB)      15060071
157600*CNC0564A - END                                                   15070071
157700              PERFORM VARYING CT-IND FROM 1 BY 1                  15080071
157800                 UNTIL CT-IND > WS-CRAFT-TABLE-MAX                15090071
157900                 IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB)                15100071
158000                    = CT-CRAFT-CODE(CT-IND)                       15110071
158100                    MOVE CT-CRAFT-COLOR(CT-IND)                   15120071
158200                       TO SCR02E-CC-COLOR(ARRAY-SUB)              15130071
158300                          SCR02E-I-O-COLOR(ARRAY-SUB)             15140071
158400                          SCR02E-POS-COLOR(ARRAY-SUB)             15150071
158500                          SCR02E-NAME-COLOR(ARRAY-SUB)            15160071
158600*CNC0564A - BEG                                                   15170071
158700                          SCR02E-LO-COLOR(ARRAY-SUB)              15180071
158800                    MOVE WHITE TO SCR02E-TURN-COLOR(ARRAY-SUB)    15190071
158900*CNC0564A - END                                                   15200071
159000                    SET CT-IND TO WS-CRAFT-TABLE-MAX              15210071
159100                 END-IF                                           15220071
159200              END-PERFORM                                         15230071
159300              ADD CRAFT-SUB-INCREMENT TO ARRAY-SUB                15240071
159400              IF ARRAY-SUB > WS-CRAFT-MAX(CRAFT-ARRAY-SUB)        15250071
159500                 SET DONE TO TRUE                                 15260071
159600              END-IF                                              15270071
159700           END-PERFORM                                            15280071
159800        END-IF                                                    15290071
159900     END-PERFORM                                                  15300071
160000                                                                  15310071
160100     MOVE CNTL-POOL-SVC        TO POOL-SERVICE-CODE               15320071
160200     MOVE CNTL-POOL-CYCLE-CODE TO BID-PACK-FLAG                   15330071
160300*                                                                 15340071
160400*    BEGIN PROCESSING                                             15350071
160500*                                                                 15360071
160600     MOVE ZEROS TO ARRAY-SUB                                      15370071
160700     PERFORM P1095-INQUIRE-BY-CALL-ORDER                          15380071
160800*         'NO SCHEDULES FOUND FOR 'STARTING AT' DATE/TIME'        15390071
160900     MOVE 'N171'                TO MSGLOG-CODE                    15400071
161000     PERFORM VARYING ARRAY-SUB FROM 1 BY 1                        15410071
161100        UNTIL ARRAY-SUB > ARRAY-MAX                               15420071
161200        IF SCR02E-TURN(ARRAY-SUB) > SPACES                        15430071
161300*               'END OF POOL LISTING'                             15440071
161400           MOVE 'E007'          TO MSGLOG-CODE                    15450071
161500           MOVE ARRAY-MAX       TO ARRAY-SUB                      15460071
161600        END-IF                                                    15470071
161700     END-PERFORM                                                  15480071
161800     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  15490071
161900             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              15500071
162000        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 15510071
162100           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 15520071
162200           IF WS-CRAFT-SUB(CRAFT-ARRAY-SUB)                       15530071
162300              > WS-CRAFT-MAX(CRAFT-ARRAY-SUB)                     15540071
162400              MOVE SPACES       TO SCR02E-ERRORMSG                15550071
162500                                   MSGLOG-CODE                    15560071
162600           ELSE                                                   15570071
162700              MOVE WS-CRAFT-MAX(CRAFT-ARRAY-SUB) TO K             15580071
162800              MOVE 'DONE'       TO P02ECA-TURN-KEY(K)             15590071
162900           END-IF                                                 15600071
163000        END-IF                                                    15610071
163100     END-PERFORM.                                                 15620071
163200*                                                                 15630071
163300 P1010-INITIALIZE-CRAFT-ARRAY.                                    15640071
163400*                                                                 15650071
163500*                                                                 15660071
163600*    FIND OUT HOW MANY CRAFT CODES WE ARE DISPLAYING              15670071
163700*    AND INITIALIZE THE ARRAY                                     15680071
163800*                                                                 15690071
163900     MOVE ZEROS TO CRAFT-SUB-INCREMENT                            15700071
164000     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  15710071
164100             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              15720071
164200        MOVE ZERO TO WS-CRAFT-SUB(CRAFT-ARRAY-SUB)                15730071
164300                     WS-CRAFT-MAX(CRAFT-ARRAY-SUB)                15740071
164400        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 15750071
164500           MOVE CRAFT-ARRAY-SUB TO WS-CRAFT-SUB(CRAFT-ARRAY-SUB)  15760071
164600                                   CRAFT-SUB-INCREMENT            15770071
164700        END-IF                                                    15780071
164800     END-PERFORM                                                  15790071
164900*                                                                 15800071
165000*    IF WE ARE DISPLAYING MORE THAN 1, ADD 1 TO THE INCREMENT     15810071
165100*    TO 'SKIP' A LINE BETWEEN EACH 'GROUP'.                       15820071
165200*                                                                 15830071
165300     IF CRAFT-SUB-INCREMENT > 1                                   15840071
165400        ADD 1 TO CRAFT-SUB-INCREMENT                              15850071
165500     END-IF                                                       15860071
165600*                                                                 15870071
165700*    COMPUTE THE LAST POSSIBLE SCREEN POSITION FOR ANY            15880071
165800*    'WHOLE' GROUP                                                15890071
165900*                                                                 15900071
166000     COMPUTE GROUP-MAX = (ARRAY-MAX + 1) / CRAFT-SUB-INCREMENT    15910071
166100                         * CRAFT-SUB-INCREMENT - 1                15920071
166200*                                                                 15930071
166300     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  15940071
166400             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              15950071
166500*                                                                 15960071
166600*       COMPUTE THE MAXIMUM SCREEN POSITION FOR EACH INDIVIDUAL   15970071
166700*       CRAFT CODE TO ENABLE TESTING FOR 'DONE'.                  15980071
166800*                                                                 15990071
166900        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 16000071
167000           MOVE WS-CRAFT-SUB(CRAFT-ARRAY-SUB)                     16010071
167100              TO WS-CRAFT-MAX(CRAFT-ARRAY-SUB)                    16020071
167200           MOVE '0' TO DONE-CODE                                  16030071
167300           PERFORM UNTIL DONE                                     16040071
167400              MOVE WS-CRAFT-MAX(CRAFT-ARRAY-SUB) TO K             16050071
167500              ADD CRAFT-SUB-INCREMENT TO K                        16060071
167600              IF K NOT > ARRAY-MAX                                16070071
167700                 AND K NOT > GROUP-MAX                            16080071
167800                 MOVE K TO WS-CRAFT-MAX(CRAFT-ARRAY-SUB)          16090071
167900              ELSE                                                16100071
168000                 SET DONE TO TRUE                                 16110071
168100              END-IF                                              16120071
168200           END-PERFORM                                            16130071
168300        END-IF                                                    16140071
168400     END-PERFORM.                                                 16150071
168500*                                                                 16160071
168600 P1020-INQUIRE-BY-POSITION.                                       16170071
168700*                                                                 16180071
168800     IF ENTER-KEY                                                 16190071
168900        IF HOME-TERMINAL-INQ                                      16200071
169000           PERFORM P1030-BUILD-TURN-POSITION                      16210071
169100        END-IF                                                    16220071
169200        PERFORM P1070-DISPLAY-POSITION                            16230071
169300        IF P02ECA-POS-DONE                                        16240071
169400           MOVE ZEROS TO P02ECA-SUB                               16250071
169500           PERFORM P1060-DISPLAY-TURN-POSITION                    16260071
169600        END-IF                                                    16270071
169700     ELSE                                                         16280071
169800        IF PFKEY8                                                 16290071
169900           IF P02ECA-POS-DONE                                     16300071
170000              PERFORM P1060-DISPLAY-TURN-POSITION                 16310071
170100           ELSE                                                   16320071
170200              PERFORM P1070-DISPLAY-POSITION                      16330071
170300              IF P02ECA-POS-DONE                                  16340071
170400                 MOVE ZEROS TO P02ECA-SUB                         16350071
170500                 PERFORM P1060-DISPLAY-TURN-POSITION              16360071
170600              END-IF                                              16370071
170700           END-IF                                                 16380071
170800        END-IF                                                    16390071
170900     END-IF.                                                      16400071
171000*                                                                 16410071
171100 P1030-BUILD-TURN-POSITION.                                       16420071
171200*                                                                 16430071
171300     MOVE ZEROS                 TO P02ECA-SUB                     16440071
171400                                                                  16450071
171500     MOVE SPACES                TO WS-HT-ON-DUTY-TIME             16460071
171600*                                                                 16470071
171700*  DELETE AND RECREATE THE TSQUEUE, PLACE ALL '9' IN SORT-KEY     16480071
171800*  TO ALWAYS FORCE THE INSERT LOGIC.                              16490071
171900*                                                                 16500071
172000                                                                  16510071
172100     PERFORM P9400-DELETE-TSQUEUE                                 16520071
172200     MOVE SPACES                TO P02ETSQ-AREA                   16530071
172300     MOVE ALL '9'               TO P02ETSQ-SORT-KEY(1)            16540071
172400     PERFORM P9500-WRITE-TSQUEUE                                  16550071
172500     PERFORM P9700-READ-TSQUEUE                                   16560071
172600     IF NOT SUCCESS                                               16570071
172700        MOVE 'P1030-1'          TO ERR-PARAGRAPH                  16580071
172800        MOVE P02ETSQ-QUEUE-ID   TO ERR-KEY                        16590071
172900        PERFORM P9999-GOT-PROBLEM                                 16600071
173000     END-IF                                                       16610071
173100                                                                  16620071
173200*                                                                 16630071
173300*  INIT THE WS-TURN-POS-ARRAY                                     16640071
173400*                                                                 16650071
173500     PERFORM P1052-INIT-TURN-POS-ARRAY                            16660071
173600                                                                  16670071
173700*                                                                 16680071
173800*  LOOP THRU THE UFP FILE BY TURN EXTRACTING OFF-BOARD OR ON-BOARD16690071
173900*  OUT-OF-TOWN RECORDS. ONCE ALL THE CRAFTS HAVE BEEN SELECTED AND16700071
174000*  MOVED TO THE WS-TURN-POS-ARRAY TABLE THEN MOVE THE GROUP TO    16710071
174100*  THE TSQUEUE IN HT-ON-DUTY-TIME/CRAFT/TRAIN SEQUENCE.           16720071
174200*                                                                 16730071
174300     MOVE SPACES                TO WS-UFP-TURN-KEY                16740071
174400     MOVE SCR02E-DIST           TO WS-UFP-DIST                    16750071
174500     MOVE SCR02E-SUB-DIST       TO WS-UFP-SUB-DIST                16760071
174600     MOVE SCR02E-POOL           TO WS-UFP-POOL                    16770071
174700     PERFORM P1200-START-UFP-TURN                                 16780071
174800     IF SUCCESS                                                   16790071
174900        MOVE ZERO               TO DONE-CODE                      16800071
175000        PERFORM UNTIL DONE                                        16810071
175100           MOVE '0'             TO ASSOCIATED-TURN-FLAG           16820071
175200           PERFORM P1210-READ-NEXT-TURN                           16830071
175300           IF SUCCESS                                             16840071
175400              IF DIST2      = SCR02E-DIST     AND                 16850071
175500                 SUB-DIST2  = SCR02E-SUB-DIST AND                 16860071
175600                 POOL-NAME2 = SCR02E-POOL                         16870071
175700                 PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1      16880071
175800                         UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX  16890071
175900                    IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) =           16900071
176000                       POOL-CRAFT-CODE2                           16910071
176100                       MOVE CRAFT-ARRAY-SUB TO CC-ARRAY-SUB       16920071
176200                       IF WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB) > SPACE 16930071
176300                          SET ASSOCIATED-TURN  TO TRUE            16940071
176400                       END-IF                                     16950071
176500                       IF NOT ASSOCIATED-TURN                     16960071
176600                          IF UFP-OFF-BOARD                        16970071
176700                             MOVE SPACES TO WS-ASGN-FILE          16980071
176800                             SET ASGN-UFP-JOB    TO TRUE          16990071
176900                             MOVE DIST OF WS-UFP TO ASGN-DIST     17000071
177000                             MOVE SUB-DIST OF WS-UFP  TO          17010071
177100                                  ASGN-SUB-DIST                   17020071
177200                             MOVE POOL-NAME OF WS-UFP TO          17030071
177300                                  ASGN-UFP-POOL                   17040071
177400                             MOVE TURN-NBR OF WS-UFP  TO          17050071
177500                                  ASGN-UFP-TURN                   17060071
177600                             MOVE POOL-CRAFT-CODE OF WS-UFP TO    17070071
177700                                  ASGN-UFP-CC                     17080071
177800                             PERFORM PXXXX-ON-DUTY-EMP            17090071
177900                             IF ASGN-EMP-NO > ZERO                17100071
178000                                PERFORM P1040-LOAD-UFP-TURN-TABLE 17110071
178100                             END-IF                               17120071
178200                          ELSE                                    17130071
178300                             IF IN-OUT-TOWN-FLAG NOT =            17140071
178400                                WS-UFP-POS-IN-OUT                 17150071
178500                                PERFORM P1035-GET-LAST-TRAIN      17160071
178600                                PERFORM P1040-LOAD-UFP-TURN-TABLE 17170071
178700                             END-IF                               17180071
178800                          END-IF                                  17190071
178900                       END-IF                                     17200071
179000                       MOVE CRAFT-ARRAY-MAX TO CRAFT-ARRAY-SUB    17210071
179100                    END-IF                                        17220071
179200                 END-PERFORM                                      17230071
179300              ELSE                                                17240071
179400                 SET DONE TO TRUE                                 17250071
179500              END-IF                                              17260071
179600           ELSE                                                   17270071
179700              SET DONE TO TRUE                                    17280071
179800              IF NOT (NO-RECORD-FND OR END-OF-FILE)               17290071
179900                 MOVE 'P1030-2' TO ERR-PARAGRAPH                  17300071
180000                 MOVE UFPTURN TO ERR-KEY                          17310071
180100                 PERFORM P9999-GOT-PROBLEM                        17320071
180200              END-IF                                              17330071
180300           END-IF                                                 17340071
180400        END-PERFORM                                               17350071
180500     ELSE                                                         17360071
180600        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     17370071
180700           MOVE 'P1030-3' TO ERR-PARAGRAPH                        17380071
180800           MOVE WS-UFP-TURN-KEY TO ERR-KEY                        17390071
180900           PERFORM P9999-GOT-PROBLEM                              17400071
181000        END-IF                                                    17410071
181100     END-IF                                                       17420071
181200     PERFORM P1220-ENDBR-UFP-TURN.                                17430071
181300*                                                                 17440071
181400*  PROCESS THE LAST WS-TURN-POS-ARRAY CRAFT GROUP                 17450071
181500*                                                                 17460071
181600     PERFORM P1050-SORT-TURN-POSITION                             17470071
181700     PERFORM P9600-REWRITE-TSQUEUE.                               17480071
181800*                                                                 17490071
181900 P1035-GET-LAST-TRAIN.                                            17500071
182000*                                                                 17510071
182100      MOVE SPACES                    TO WS-ASGN-FILE              17520071
182200      SET ASGN-UFP-JOB               TO TRUE                      17530071
182300      MOVE DIST OF WS-UFP            TO ASGN-DIST                 17540071
182400      MOVE SUB-DIST OF WS-UFP        TO ASGN-SUB-DIST             17550071
182500      MOVE POOL-NAME OF WS-UFP       TO ASGN-UFP-POOL             17560071
182600      MOVE TURN-NBR OF WS-UFP        TO ASGN-UFP-TURN             17570071
182700      MOVE POOL-CRAFT-CODE OF WS-UFP TO ASGN-UFP-CC               17580071
182800      PERFORM PXXXX-ON-DUTY-EMP                                   17590071
182900      IF ASGN-EMP-NO > ZERO                                       17600071
183000         MOVE ASGN-EMP-NO            TO MSTRNBRK                  17610071
183100         EXEC CICS READ                                           17620071
183200                   DATASET(MSTR-VIA-EMP-NBR)                      17630071
183300                   INTO(WS-MSTR)                                  17640071
183400                   LENGTH(MSTRENBR-RLGTH)                         17650071
183500                   RIDFLD(MSTRNBRK)                               17660071
183600                   KEYLENGTH(MSTRENBR-KLGTH)                      17670071
183700                   RESP(WS-RESPONSE)                              17680071
183800         END-EXEC                                                 17690071
183900         MOVE WS-RESPONSE            TO FILE-STATUS               17700071
184000         IF SUCCESS                                               17710071
184100            IF EMP-TRHIST-KEY > SPACES                            17720071
184200               MOVE EMP-TRHIST-KEY   TO HOLD-TRHIST               17730071
184300            ELSE                                                  17740071
184400               MOVE SPACES           TO HOLD-TRHIST               17750071
184500            END-IF                                                17760071
184600         ELSE                                                     17770071
184700            MOVE 'P1035-1'           TO ERR-PARAGRAPH             17780071
184800            MOVE MSTRNBRK            TO ERR-KEY                   17790071
184900            PERFORM P9999-GOT-PROBLEM                             17800071
185000         END-IF                                                   17810071
185100      ELSE                                                        17820071
185200         MOVE SPACES                 TO HOLD-TRHIST               17830071
185300      END-IF.                                                     17840071
185400*                                                                 17850071
185500 P1040-LOAD-UFP-TURN-TABLE.                                       17860071
185600*                                                                 17870071
185700*    IF TURNS HAVE CHANGED PASS THE WS-TURN-POS-ARRAY TO          17880071
185800*    THE TSQUEUE. LOAD THE UFP TURN KEY IN THE WS-TURN-POS-ARRAY. 17890071
185900*                                                                 17900071
186000     IF WS-FIRST-TIME = SPACE                                     17910071
186100        MOVE 'Y' TO WS-FIRST-TIME                                 17920071
186200     ELSE                                                         17930071
186300        MOVE 'N' TO WS-FIRST-TIME                                 17940071
186400     END-IF                                                       17950071
186500                                                                  17960071
186600     IF WS-FIRST-TIME = 'N'                                       17970071
186700        PERFORM P1050-SORT-TURN-POSITION                          17980071
186800     END-IF                                                       17990071
186900*                                                                 18000071
187000*  ALL CRAFTS WITHIN THE GROUP WILL HAVE THE SAME HT-ON-DUTY-TIME 18010071
187100*  FOR SORTING PURPOSES                                           18020071
187200*                                                                 18030071
187300*    IF WS-HT-ON-DUTY-TIME NOT > SPACES                           18040071
187400        MOVE HT-ON-DUTY-TIME TO WS-HT-ON-DUTY-TIME                18050071
187500*    END-IF                                                       18060071
187600                                                                  18070071
187700     MOVE UFPTURN-AREA TO WS-UFP-TURN-KEY                         18080071
187800                          WS-TURN-POS-UFP-KEY(CRAFT-ARRAY-SUB)    18090071
187900     IF TRAIN-SYMBOL > SPACES                                     18100071
188000        MOVE TRAIN-SYMBOL                                         18110071
188100                       TO WS-TURN-POS-TRAIN(CRAFT-ARRAY-SUB)      18120071
188200     ELSE                                                         18130071
188300        MOVE HOLD-TRAIN-SYMBOL                                    18140071
188400                       TO WS-TURN-POS-TRAIN(CRAFT-ARRAY-SUB)      18150071
188500        MOVE SPACES    TO HOLD-TRHIST                             18160071
188600     END-IF.                                                      18170071
188700*                                                                 18180071
188800 P1050-SORT-TURN-POSITION.                                        18190071
188900*                                                                 18200071
189000*                                                                 18210071
189100*    MOVE THE COMMON HT-ON-DUTY-TIME TO EACH CRAFT IN THE         18220071
189200*    WS-TURN-POS-ARRAY GROUP FOR SORTING PURPOSES, COUNT THE      18230071
189300*    NUMBER OF CRAFTS IN THE WS-TURN-POS-ARRAY THAT HAVE          18240071
189400*    UFP KEYS                                                     18250071
189500*                                                                 18260071
189600     MOVE ZEROS     TO WS-TURN-POS-COUNT                          18270071
189700     PERFORM VARYING SUB1 FROM 1 BY 1                             18280071
189800       UNTIL SUB1 > CRAFT-ARRAY-MAX                               18290071
189900       IF WS-TURN-POS-UFP-KEY(SUB1) > SPACES                      18300071
190000          MOVE WS-HT-ON-DUTY-TIME TO WS-TURN-POS-ON-DUTY(SUB1)    18310071
190100          ADD 1                   TO WS-TURN-POS-COUNT            18320071
190200       END-IF                                                     18330071
190300     END-PERFORM                                                  18340071
190400                                                                  18350071
190500     MOVE SPACES TO WS-HT-ON-DUTY-TIME                            18360071
190600                                                                  18370071
190700*                                                                 18380071
190800*  PLACE THE CRAFT GROUP INTO THE TSQUEUE IN SORTED ORDER BY      18390071
190900*  P02ETSQ-SORT-KEY(HT-ON-DUTY-TIME, CRAFT, TRAIN SEQUENCE)       18400071
191000*                                                                 18410071
191100*  SOONER OR LATER THE WS-SORT-KEY WILL BE LESS THAN THE          18420071
191200*  P02ECA-SORT-KEY IN THE TSQUEUE SINCE THE FIRST OCCURS          18430071
191300*  WAS INITLIZED TO ALL '9'.                                      18440071
191400*                                                                 18450071
191500     MOVE SPACES TO WS-SORT-KEY                                   18460071
191600     PERFORM VARYING SUB1 FROM 1 BY 1                             18470071
191700       UNTIL SUB1 > CRAFT-ARRAY-MAX                               18480071
191800       IF WS-TURN-POS-UFP-KEY(SUB1) > SPACES                      18490071
191900          MOVE WS-TURN-POS-SORT-KEY(SUB1) TO WS-SORT-KEY          18500071
192000          MOVE CRAFT-ARRAY-MAX            TO SUB1                 18510071
192100       END-IF                                                     18520071
192200     END-PERFORM                                                  18530071
192300                                                                  18540071
192400     PERFORM VARYING P02ECA-SUB FROM 1 BY 1                       18550071
192500       UNTIL P02ECA-SUB > P02ETSQ-ARRAY-MAX                       18560071
192600       IF WS-SORT-KEY > SPACES                                    18570071
192700          IF WS-SORT-KEY < P02ETSQ-SORT-KEY(P02ECA-SUB)           18580071
192800             PERFORM P1051-INSERT-TURN-POSITION                   18590071
192900             MOVE P02ETSQ-ARRAY-MAX TO P02ECA-SUB                 18600071
193000          ELSE                                                    18610071
193100*                                                                 18620071
193200* ABEND WHEN TSQUEUE ARRAY LIMIT HAS BEEN REACHED                 18630071
193300*                                                                 18640071
193400             IF P02ECA-SUB = P02ETSQ-ARRAY-MAX                    18650071
193500                MOVE 'P1050-2'        TO ERR-PARAGRAPH            18660071
193600                MOVE P02ETSQ-QUEUE-ID TO ERR-KEY                  18670071
193700                MOVE 'ARRAY MAX REACHED'                          18680071
193800                                      TO ERR-SENTENCE             18690071
193900                PERFORM P9999-GOT-PROBLEM                         18700071
194000             END-IF                                               18710071
194100          END-IF                                                  18720071
194200       END-IF                                                     18730071
194300     END-PERFORM                                                  18740071
194400*                                                                 18750071
194500*  RE-INIT THE WS-TURN-POS-ARRAY FOR NEXT GROUP OF CRAFTS         18760071
194600*                                                                 18770071
194700     PERFORM P1052-INIT-TURN-POS-ARRAY.                           18780071
194800*                                                                 18790071
194900 P1051-INSERT-TURN-POSITION.                                      18800071
195000*                                                                 18810071
195100*                                                                 18820071
195200*  FIRST BUMP THE EXISTING RECORDS IN THE TSQUEUE BACK AS MANY    18830071
195300*  POSITIONS AS THE WS-TURN-POS-ARRAY CRAFT GROUP NEEDS. THEN     18840071
195400*  INSERT THE WS-TURN-POS-ARRAY CRAFT GROUP IN THE OPEN SLOTS.    18850071
195500*                                                                 18860071
195600*  BEG-SUB IS BEGINING POSITION OF WHERE GROUP TO TO BE BUMPED    18870071
195700*  END-SUB IS ENDING POSITION OF WHERE GROUP TO TO BE BUMPED      18880071
195800*                                                                 18890071
195900     MOVE P02ECA-SUB   TO BEG-SUB                                 18900071
196000     PERFORM VARYING SUB2 FROM P02ETSQ-ARRAY-MAX BY -1            18910071
196100       UNTIL SUB2 < 1                                             18920071
196200         IF P02ETSQ-ARRAY(SUB2) > SPACES                          18930071
196300            MOVE SUB2  TO END-SUB                                 18940071
196400            MOVE ZEROS TO SUB2                                    18950071
196500         END-IF                                                   18960071
196600     END-PERFORM                                                  18970071
196700                                                                  18980071
196800     COMPUTE SUB2 = END-SUB + WS-TURN-POS-COUNT                   18990071
196900                                                                  19000071
197000     IF SUB2 NOT > P02ETSQ-ARRAY-MAX                              19010071
197100        PERFORM VARYING END-SUB FROM END-SUB BY -1                19020071
197200          UNTIL END-SUB < BEG-SUB                                 19030071
197300            MOVE P02ETSQ-ARRAY(END-SUB) TO                        19040071
197400                 P02ETSQ-ARRAY(SUB2)                              19050071
197500            MOVE SPACES TO P02ETSQ-ARRAY(END-SUB)                 19060071
197600            SUBTRACT 1 FROM SUB2                                  19070071
197700        END-PERFORM                                               19080071
197800*                                                                 19090071
197900*  ONLY PASS CRAFTS THAT HAVE UFP KEYS TO THE TSQUEUE             19100071
198000*                                                                 19110071
198100        PERFORM VARYING SUB1 FROM 1 BY 1                          19120071
198200          UNTIL SUB1 > CRAFT-ARRAY-MAX                            19130071
198300            IF WS-TURN-POS-UFP-KEY(SUB1) > SPACES                 19140071
198400               MOVE WS-TURN-POS-SORT-KEY(SUB1) TO                 19150071
198500                    P02ETSQ-SORT-KEY(BEG-SUB)                     19160071
198600               MOVE WS-TURN-POS-UFP-KEY(SUB1)   TO                19170071
198700                    P02ETSQ-UFP-TURN-NBR-KEY(BEG-SUB)             19180071
198800               ADD 1 TO BEG-SUB                                   19190071
198900            END-IF                                                19200071
199000        END-PERFORM                                               19210071
199100     ELSE                                                         19220071
199200        MOVE 'P1051-1'        TO ERR-PARAGRAPH                    19230071
199300        MOVE SUB2             TO ERR-KEY                          19240071
199400        MOVE 'ARRAY MAX REACHED'                                  19250071
199500                              TO ERR-SENTENCE                     19260071
199600        PERFORM P9999-GOT-PROBLEM                                 19270071
199700     END-IF.                                                      19280071
199800*                                                                 19290071
199900 P1052-INIT-TURN-POS-ARRAY.                                       19300071
200000*                                                                 19310071
200100     MOVE SPACES    TO WS-TURN-POS-CRAFT-AREA                     19320071
200200                                                                  19330071
200300     PERFORM VARYING SUB1 FROM 1 BY 1                             19340071
200400       UNTIL SUB1 > CRAFT-ARRAY-MAX                               19350071
200500       IF WS-CRAFT-CODE(SUB1) > SPACES                            19360071
200600          MOVE WS-CRAFT-CODE(SUB1)                                19370071
200700                    TO WS-TURN-POS-CRAFT(SUB1)                    19380071
200800          MOVE SUB1 TO WS-TURN-POS-CRAFT-SEQ(SUB1)                19390071
200900       END-IF                                                     19400071
201000     END-PERFORM.                                                 19410071
201100*                                                                 19420071
201200 P1060-DISPLAY-TURN-POSITION.                                     19430071
201300*                                                                 19440071
201400     SET WS-TURN-POS TO TRUE                                      19450071
201500*                                                                 19460071
201600*    DISPLAY THE TURNS FROM THE TSQUEUE                           19470071
201700*                                                                 19480071
201800     ADD 1 TO P02ECA-SUB                                          19490071
201900     IF P02ECA-SUB NOT > P02ETSQ-ARRAY-MAX                        19500071
202000        PERFORM P9700-READ-TSQUEUE                                19510071
202100        IF SUCCESS                                                19520071
202200           MOVE ZERO   TO DONE-CODE                               19530071
202300           PERFORM UNTIL DONE                                     19540071
202400              MOVE P02ETSQ-UFP-TURN-NBR-KEY(P02ECA-SUB)           19550071
202500                       TO UFPTURN                                 19560071
202600              PERFORM P8600-READ-UFPTURN                          19570071
202700              IF SUCCESS                                          19580071
202800                 MOVE 0 TO ASSOCIATED-TURN-FLAG                   19590071
202900                 PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1      19600071
203000                         UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX  19610071
203100                      MOVE CRAFT-ARRAY-SUB TO CC-ARRAY-SUB        19620071
203200                   IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) =            19630071
203300                      POOL-CRAFT-CODE2                            19640071
203400                      IF WS-CRAFT-SUB(CRAFT-ARRAY-SUB) >          19650071
203500                         WS-CRAFT-MAX(CRAFT-ARRAY-SUB)            19660071
203600                         SET DONE TO TRUE                         19670071
203700                         SUBTRACT 1 FROM P02ECA-SUB               19680071
203800                      ELSE                                        19690071
203900                         PERFORM P1500-SETUP-NAME-LINE            19700071
204000                         MOVE UFPTURN-AREA TO WS-UFP-TURN-KEY     19710071
204100                         PERFORM P1400-CHECK-ASSOCIATED-TURNS     19720071
204200                         MOVE CRAFT-ARRAY-MAX TO CRAFT-ARRAY-SUB  19730071
204300                      END-IF                                      19740071
204400                   END-IF                                         19750071
204500                 END-PERFORM                                      19760071
204600              ELSE                                                19770071
204700                 SET DONE TO TRUE                                 19780071
204800                 IF NOT (NO-RECORD-FND OR END-OF-FILE)            19790071
204900                    MOVE 'P1060-1' TO ERR-PARAGRAPH               19800071
205000                    MOVE UFPTURN   TO ERR-KEY                     19810071
205100                    PERFORM P9999-GOT-PROBLEM                     19820071
205200                 END-IF                                           19830071
205300              END-IF                                              19840071
205400              IF NOT DONE                                         19850071
205500                 ADD 1          TO P02ECA-SUB                     19860071
205600                 IF P02ECA-SUB > P02ETSQ-ARRAY-MAX                19870071
205700                    SET DONE    TO TRUE                           19880071
205800                 END-IF                                           19890071
205900              END-IF                                              19900071
206000           END-PERFORM                                            19910071
206100        END-IF                                                    19920071
206200     END-IF.                                                      19930071
206300*                                                                 19940071
206400 P1070-DISPLAY-POSITION.                                          19950071
206500*                                                                 19960071
206600     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  19970071
206700             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              19980071
206800        MOVE CRAFT-ARRAY-SUB TO CC-ARRAY-SUB                      19990071
206900        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACES                20000071
207000           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 20010071
207100           AND WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB) NOT > SPACES       20020071
207200           MOVE SPACES          TO WS-UFP-POS-KEY                 20030071
207300           MOVE SCR02E-DIST     TO WS-UFP-POS-DIST                20040071
207400           MOVE SCR02E-SUB-DIST TO WS-UFP-POS-SUB-DIST            20050071
207500           MOVE SCR02E-POOL     TO WS-UFP-POS-POOL                20060071
207600           MOVE WS-CRAFT-CODE(CRAFT-ARRAY-SUB) TO WS-UFP-POS-CC   20070071
207700           MOVE SCR02E-BOARD    TO WS-UFP-POS-IN-OUT              20080071
207800           IF WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB) > SPACES       20090071
207900              MOVE WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)           20100071
208000                 TO UFPTURN                                       20110071
208100              PERFORM P8600-READ-UFPTURN                          20120071
208200              IF NOT SUCCESS                                      20130071
208300                 MOVE 'P1070-1' TO ERR-PARAGRAPH                  20140071
208400                 MOVE UFPTURN   TO ERR-KEY                        20150071
208500                 PERFORM P9999-GOT-PROBLEM                        20160071
208600              END-IF                                              20170071
208700              MOVE UFP-POS TO WS-KEY-POS                          20180071
208800           ELSE                                                   20190071
208900              MOVE ZEROS TO WS-KEY-POS                            20200071
209000              IF MINE-TURN-SVC                                    20210071
209100                 MOVE SPACES    TO WS-KEY-POS-DATE-TIME           20220071
209200              END-IF                                              20230071
209300           END-IF                                                 20240071
209400           MOVE ZERO TO DONE-CODE                                 20250071
209500           PERFORM UNTIL DONE                                     20260071
209600              MOVE 0 TO ASSOCIATED-TURN-FLAG                      20270071
209700              ADD  1 TO WS-KEY-POS-TIE-BREAK                      20280071
209800              MOVE WS-UFP-POS-KEY TO UFPPOS                       20290071
209900              PERFORM P1110-READ-NEXT-POS                         20300071
210000              IF SUCCESS                                          20310071
210100                 SET DE-YYMMDD-FORMAT        TO TRUE              20320071
210200                 MOVE UFP-POS-DATE-TIME(1:6) TO DE-YYMMDD         20330071
210300                 PERFORM P8998-DATEEDIT                           20340071
210400                 MOVE DE-CCYYMMDD            TO DE-COMPARE1-DATE  20350071
210500                 MOVE UFP-POS-DATE-TIME(7:4) TO DE-COMPARE1-TIME  20360071
210600                 IF DIST OF WS-UFP = SCR02E-DIST                  20370071
210700                    AND SUB-DIST OF WS-UFP = SCR02E-SUB-DIST      20380071
210800                    AND POOL-NAME OF WS-UFP = SCR02E-POOL         20390071
210900                    AND POOL-CRAFT-CODE                           20400071
211000                        = WS-CRAFT-CODE(CRAFT-ARRAY-SUB)          20410071
211100                    AND IN-OUT-TOWN-FLAG = WS-UFP-POS-IN-OUT      20420071
211200                    AND UFP-ON-BOARD                              20430071
211300                    AND ((ENGINE-POOL-CRAFT AND EN-ID-POOL)       20440071
211400                       OR (TRAIN-POOL-CRAFT AND TR-ID-POOL)       20450071
211500                       OR MINE-TURN-SVC                           20460071
211600*                      OR UFP-POS-DATE-TIME                       20470071
211700*                         NOT > PRESENT-TIME)                     20480071
211800                       OR WS-SYSTEM-DATE = '991231' OR '000101'   20490071
211900                       OR DE-COMPARE1-DATE                        20500071
212000                          NOT > WS-PRESENT-TIME-CENT)             20510071
212100                    MOVE UFPPOS-AREA TO WS-UFP-POS-KEY            20520071
212200                    PERFORM P1500-SETUP-NAME-LINE                 20530071
212300                    PERFORM P1400-CHECK-ASSOCIATED-TURNS          20540071
212400                 ELSE                                             20550071
212500                    SET DONE            TO TRUE                   20560071
212600                 END-IF                                           20570071
212700              ELSE                                                20580071
212800                 SET DONE            TO TRUE                      20590071
212900                 IF NOT (NO-RECORD-FND OR END-OF-FILE)            20600071
213000                    MOVE 'P1070-1' TO ERR-PARAGRAPH               20610071
213100                    MOVE UFPPOS TO ERR-KEY                        20620071
213200                    PERFORM P9999-GOT-PROBLEM                     20630071
213300                 END-IF                                           20640071
213400              END-IF                                              20650071
213500           END-PERFORM                                            20660071
213600        END-IF                                                    20670071
213700     END-PERFORM                                                  20680071
213800                                                                  20690071
213900     SET P02ECA-POS-DONE TO TRUE                                  20700071
214000     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  20710071
214100           UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX                20720071
214200        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 20730071
214300           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 20740071
214400           IF WS-CRAFT-SUB(CRAFT-ARRAY-SUB)                       20750071
214500              > WS-CRAFT-MAX(CRAFT-ARRAY-SUB)                     20760071
214600              MOVE '0' TO P02ECA-POS-DONE-FLAG                    20770071
214700           END-IF                                                 20780071
214800        END-IF                                                    20790071
214900     END-PERFORM                                                  20800071
215000*                                                                 20810071
215100*    IF THE POSITION INQUIRY IS COMPLETE WE ARE NOW READY FOR     20820071
215200*    THE POS TURN INQUIRY. MUST RESET THE WS-CRAFT-SUB'S          20830071
215300*    VALUES TO A SEQUENTIAL ORDER BASED UPON THE HIGHEST VALUE    20840071
215400*    IN THE GROUP. ONCE THE HIGHEST VALUE IN THE GROUP IS FOUND   20850071
215500*    ADJUST OF THE WS-CRAFTS-SUB'S VALUES. THIS WILL              20860071
215600*    INSURE THAT THE FIRST POS TURN INQUIRY CRAFT GROUP WILL      20870071
215700*    START ON THE PROPER LINE TOGETHER.                           20880071
215800*                                                                 20890071
215900     IF P02ECA-POS-DONE                                           20900071
216000        MOVE ZEROS TO WS-COMPUTE-SUB                              20910071
216100                      WS-SAVE-SUB                                 20920071
216200        PERFORM VARYING SUB1 FROM 1 BY 1                          20930071
216300           UNTIL SUB1 > CRAFT-ARRAY-MAX                           20940071
216400           IF WS-CRAFT-CODE(SUB1) > SPACES                        20950071
216500              IF WS-CRAFT-SUB(SUB1) > WS-COMPUTE-SUB              20960071
216600                 MOVE WS-CRAFT-SUB(SUB1) TO WS-COMPUTE-SUB        20970071
216700                 MOVE SUB1               TO WS-SAVE-SUB           20980071
216800              END-IF                                              20990071
216900           END-IF                                                 21000071
217000        END-PERFORM                                               21010071
217100        IF WS-SAVE-SUB > ZEROS                                    21020071
217200           PERFORM VARYING SUB1 FROM WS-SAVE-SUB                  21030071
217300             BY -1 UNTIL SUB1 < 1                                 21040071
217400             IF WS-CRAFT-CODE(SUB1) > SPACES                      21050071
217500                IF SUB1 NOT = WS-SAVE-SUB                         21060071
217600                   SUBTRACT 1 FROM WS-COMPUTE-SUB                 21070071
217700                END-IF                                            21080071
217800             END-IF                                               21090071
217900           END-PERFORM                                            21100071
218000           PERFORM VARYING SUB1 FROM 1 BY 1                       21110071
218100             UNTIL SUB1 > CRAFT-ARRAY-MAX                         21120071
218200             IF WS-CRAFT-CODE(SUB1) > SPACES                      21130071
218300                MOVE WS-COMPUTE-SUB TO WS-CRAFT-SUB(SUB1)         21140071
218400                ADD 1            TO WS-COMPUTE-SUB                21150071
218500             END-IF                                               21160071
218600           END-PERFORM                                            21170071
218700        END-IF                                                    21180071
218800     END-IF.                                                      21190071
218900*                                                                 21200071
219000 P1090-INQUIRE-BY-TURN.                                           21210071
219100*                                                                 21220071
219200*    FIND THE 'LOWEST' KEY VALUE OF A TURN IN THE 'LAST' GROUP    21230071
219300*    THAT WAS PREVIOUSLY DISPLAYED                                21240071
219400*                                                                 21250071
219500     MOVE SPACES TO WS-COMPARE-TURN                               21260071
219600     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  21270071
219700             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              21280071
219800        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACE                 21290071
219900           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 21300071
220000           IF WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB) > SPACES       21310071
220100              IF WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)             21320071
220200                 < WS-COMPARE-TURN OR WS-COMPARE-TURN = SPACES    21330071
220300                 MOVE WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)        21340071
220400                    TO WS-COMPARE-TURN                            21350071
220500              END-IF                                              21360071
220600           END-IF                                                 21370071
220700        END-IF                                                    21380071
220800     END-PERFORM                                                  21390071
220900     IF WS-COMPARE-TURN > SPACE                                   21400071
221000        MOVE WS-COMPARE-TURN TO WS-UFP-TURN-KEY                   21410071
221100     ELSE                                                         21420071
221200        MOVE SPACES          TO WS-UFP-TURN-KEY                   21430071
221300        MOVE SCR02E-DIST     TO WS-UFP-DIST                       21440071
221400        MOVE SCR02E-SUB-DIST TO WS-UFP-SUB-DIST                   21450071
221500        MOVE SCR02E-POOL     TO WS-UFP-POOL                       21460071
221600     END-IF                                                       21470071
221700     PERFORM P1200-START-UFP-TURN                                 21480071
221800     IF SUCCESS                                                   21490071
221900        IF WS-COMPARE-TURN > SPACE                                21500071
222000           PERFORM P1210-READ-NEXT-TURN                           21510071
222100        END-IF                                                    21520071
222200     END-IF                                                       21530071
222300     IF SUCCESS                                                   21540071
222400        MOVE ZERO TO DONE-CODE                                    21550071
222500        PERFORM UNTIL DONE                                        21560071
222600           MOVE '0' TO ASSOCIATED-TURN-FLAG                       21570071
222700           PERFORM P1210-READ-NEXT-TURN                           21580071
222800           IF SUCCESS                                             21590071
222900              IF DIST2 = SCR02E-DIST AND                          21600071
223000                 SUB-DIST2 = SCR02E-SUB-DIST AND                  21610071
223100                 POOL-NAME2 = SCR02E-POOL                         21620071
223200                 PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1      21630071
223300                         UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX  21640071
223400                    IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB)             21650071
223500                       = POOL-CRAFT-CODE2                         21660071
223600                       IF UFPTURN-AREA                            21670071
223700                          > WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB)  21680071
223800                          MOVE SPACES TO                          21690071
223900                             WS-CRAFT-SCROLL-KEY(CRAFT-ARRAY-SUB) 21700071
224000                          MOVE CRAFT-ARRAY-SUB TO CC-ARRAY-SUB    21710071
224100                          IF WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB)      21720071
224200                             GREATER SPACE                        21730071
224300                             SET ASSOCIATED-TURN  TO TRUE         21740071
224400                          END-IF                                  21750071
224500                          IF NOT ASSOCIATED-TURN                  21760071
224600                             PERFORM P1500-SETUP-NAME-LINE        21770071
224700                             MOVE UFPTURN-AREA TO WS-UFP-TURN-KEY 21780071
224800                             PERFORM P1220-ENDBR-UFP-TURN         21790071
224900                             PERFORM P1400-CHECK-ASSOCIATED-TURNS 21800071
225000                             PERFORM P1200-START-UFP-TURN         21810071
225100                             IF NOT SUCCESS                       21820071
225200                                MOVE 'P1090-1' TO ERR-PARAGRAPH   21830071
225300                                MOVE UFPTURN TO ERR-KEY           21840071
225400                                PERFORM P9999-GOT-PROBLEM         21850071
225500                             END-IF                               21860071
225600                             PERFORM P1210-READ-NEXT-TURN         21870071
225700                             IF NOT SUCCESS                       21880071
225800                                MOVE 'P1090-2' TO ERR-PARAGRAPH   21890071
225900                                MOVE UFPTURN TO ERR-KEY           21900071
226000                                PERFORM P9999-GOT-PROBLEM         21910071
226100                             END-IF                               21920071
226200                          END-IF                                  21930071
226300                       END-IF                                     21940071
226400                       MOVE CRAFT-ARRAY-MAX TO CRAFT-ARRAY-SUB    21950071
226500                    END-IF                                        21960071
226600                 END-PERFORM                                      21970071
226700              ELSE                                                21980071
226800                 SET DONE TO TRUE                                 21990071
226900              END-IF                                              22000071
227000           ELSE                                                   22010071
227100              SET DONE TO TRUE                                    22020071
227200              IF NOT (NO-RECORD-FND OR END-OF-FILE)               22030071
227300                 MOVE 'P1090-3' TO ERR-PARAGRAPH                  22040071
227400                 MOVE UFPTURN TO ERR-KEY                          22050071
227500                 PERFORM P9999-GOT-PROBLEM                        22060071
227600              END-IF                                              22070071
227700           END-IF                                                 22080071
227800        END-PERFORM                                               22090071
227900     ELSE                                                         22100071
228000        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     22110071
228100           MOVE 'P1090-4' TO ERR-PARAGRAPH                        22120071
228200           MOVE WS-UFP-TURN-KEY TO ERR-KEY                        22130071
228300           PERFORM P9999-GOT-PROBLEM                              22140071
228400        END-IF                                                    22150071
228500     END-IF                                                       22160071
228600     PERFORM P1220-ENDBR-UFP-TURN.                                22170071
228700*                                                                 22180071
228800 P1095-INQUIRE-BY-CALL-ORDER.                                     22190071
228900*                                                                 22200071
229000*    THE FOLLOWING PERFORM STATEMENT WILL LOAD UP ALL OF THE      22210071
229100*    TURNS FOR ALL OF THE PERTINENT CRAFTS INTO A WORKING         22220071
229200*    STORAGE ARRAY.  THE TURNS FOR EACH CRAFT WILL BE SORTED      22230071
229300*    BY 'SCHEDULE START DTTM/CALL ORDER/BOARD DTTM'.              22240071
229400*                                                                 22250071
229500     INITIALIZE WS-CALL-ORDER-CRAFT-AREA                          22260071
229600     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  22270071
229700             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              22280071
229800        MOVE CRAFT-ARRAY-SUB            TO CC-ARRAY-SUB           22290071
229900        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACES                22300071
230000           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 22310071
230100           AND WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB) NOT > SPACES       22320071
230200           MOVE SPACES                  TO WS-SCHED               22330071
230300           MOVE SCR02E-DIST             TO SCHED2-DIST            22340071
230400           MOVE SCR02E-SUB-DIST         TO SCHED2-SUB-DIST        22350071
230500           MOVE SCR02E-POOL             TO SCHED2-BOARD-ID        22360071
230600           MOVE WS-CRAFT-CODE(CRAFT-ARRAY-SUB)                    22370071
230700                                        TO SCHED2-CC              22380071
230800           MOVE ZEROES                  TO DATE-CONVERSION-PARMS  22390071
230900           SET PARM-SUBTRACT            TO TRUE                   22400071
231000           MOVE HOLD-START-DATE(3:6)    TO PARM-PRI-DATE-GREG     22410071
231100           MOVE '000001'                TO PARM-SEC-DATE-GREG     22420071
231200           EXEC CICS LINK                                         22430071
231300                     PROGRAM(P903-PGM)                            22440071
231400                     COMMAREA(DATE-CONVERSION-PARMS)              22450071
231500                     LENGTH(P903-LGTH)                            22460071
231600                     RESP(WS-RESPONSE)                            22470071
231700           END-EXEC                                               22480071
231800           MOVE WS-RESPONSE             TO FILE-STATUS            22490071
231900           IF NOT SUCCESS                                         22500071
232000              MOVE 'P1095-2'            TO ERR-PARAGRAPH          22510071
232100              MOVE 'P903'               TO ERR-KEY                22520071
232200              PERFORM P9999-GOT-PROBLEM                           22530071
232300           END-IF                                                 22540071
232400           MOVE PARM-RES-DATE-GREG      TO SCHED2-START-YYMMDD    22550071
232500           MOVE PARM-RES-GREG-CENT      TO SCHED2-START-CE        22560071
232600           MOVE HOLD-START-TIME         TO SCHED2-START-TIME      22570071
232700           MOVE SCHED-KEY2              TO SCHEDKEY2              22580071
232800           SET NOT-DONE                 TO TRUE                   22590071
232900           PERFORM P8710-STARTBR-SCHEDKEY2                        22600071
233000           IF NOT SUCCESS                                         22610071
233100              SET DONE                  TO TRUE                   22620071
233200           END-IF                                                 22630071
233300           MOVE 0                       TO S1                     22640071
233400           PERFORM UNTIL DONE                                     22650071
233500              MOVE 0                    TO ASSOCIATED-TURN-FLAG   22660071
233600              PERFORM P8720-READNEXT-SCHEDKEY2                    22670071
233700              IF SUCCESS                                          22680071
233800                 IF SCHED1-DIST          = SCR02E-DIST            22690071
233900                    AND SCHED1-SUB-DIST  = SCR02E-SUB-DIST        22700071
234000                    AND SCHED1-BOARD-ID  = SCR02E-POOL            22710071
234100                    AND SCHED1-CC = WS-CRAFT-CODE(CRAFT-ARRAY-SUB)22720071
234200                    PERFORM P1096-VALIDATE-TURN                   22730071
234300                    IF DISPLAY-TURN                               22740071
234400*CNC0564A - BEG                                                   22750071
234500                    OR DISPLAY-TURN-RD-RED                        22760071
234600*CNC0564A - END                                                   22770071
234700                       PERFORM P8400-LOAD-CALL-ORDER-ARRAY        22780071
234800                    ELSE                                          22790071
234900                       IF DONT-DISPLAY-STOP                       22800071
235000                          SET DONE TO TRUE                        22810071
235100                       END-IF                                     22820071
235200                    END-IF                                        22830071
235300                 ELSE                                             22840071
235400                    SET DONE TO TRUE                              22850071
235500                 END-IF                                           22860071
235600              ELSE                                                22870071
235700                 SET DONE TO TRUE                                 22880071
235800              END-IF                                              22890071
235900           END-PERFORM                                            22900071
236000           PERFORM P8730-ENDBR-SCHEDKEY2                          22910071
236100        END-IF                                                    22920071
236200     END-PERFORM                                                  22930071
236300*                                                                 22940071
236400*    NOW WE'VE GOT ALL OF THE TURNS SORTED IN THE RIGHT ORDER,    22950071
236500*    SO LOAD THEM ONTO THE SCREEN BY READING THROUGH OUR          22960071
236600*    ARRAY, BY CRAFT.                                             22970071
236700*                                                                 22980071
236800     PERFORM VARYING CRAFT-ARRAY-SUB FROM 1 BY 1                  22990071
236900             UNTIL CRAFT-ARRAY-SUB > CRAFT-ARRAY-MAX              23000071
237000        MOVE CRAFT-ARRAY-SUB            TO CC-ARRAY-SUB           23010071
237100        IF WS-CRAFT-CODE(CRAFT-ARRAY-SUB) > SPACES                23020071
237200           AND NOT WS-CRAFT-DONE(CRAFT-ARRAY-SUB)                 23030071
237300           AND WS-ASSOC-CRAFT(CRAFT-ARRAY-SUB) NOT > SPACES       23040071
237400*                                                                 23050071
237500*          'S1' INDEXES THE CRAFT ARRAY                           23060071
237600*          'S2' INDEXES THE TURNS ARRAY WITHIN THE CRAFT ARRAY    23070071
237700*                                                                 23080071
237800           MOVE CRAFT-ARRAY-SUB         TO S1                     23090071
237900           IF WS-CALL-ORDER-KEY(S1) > SPACES                      23100071
238000*                                                                 23110071
238100*             WHEN PAGING, FIND THE LAST ENTRY WE DISPLAYED       23120071
238200*             FROM OUR ARRAY, BASED ON THE CALL-ORDER-KEY.        23130071
238300*             IF THAT ENTRY NO LONGER EXISTS, LOOK FOR THE        23140071
238400*             NEXT GREATEST KEY VALUE AND START DISPLAYING        23150071
238500*             FROM THAT POINT.                                    23160071
238600*                                                                 23170071
238700              SET COT-NOT-DONE          TO TRUE                   23180071
238800              PERFORM VARYING S2 FROM 1 BY 1                      23190071
238900                      UNTIL COT-DONE                              23200071
239000                         OR S2 = COT-ARRAY-MAX                    23210071
239100                 IF WS-CO-TURN-ARRAY(S1, S2) > SPACES             23220071
239200*CNC0564A - BEG                                                   23230071
239300                    IF WS-3A-BIDPK-TIEBRK-PW-BRD                  23240071
239400                       IF WS-COT-BOARD-DATE-TIME-TIE(S1, S2) >=   23250071
239500                          WS-CO-POSITION(S1)                      23260071
239600                          SET COT-DONE     TO TRUE                23270071
239700                          IF WS-COT-BOARD-DATE-TIME-TIE(S1, S2) > 23280071
239800                             WS-CO-POSITION(S1)                   23290071
239900                             SUBTRACT 1  FROM S2                  23300071
240000                          END-IF                                  23310071
240100                       END-IF                                     23320071
240200                    ELSE                                          23330071
240300                       IF WS-COT-KEY(S1, S2) >=                   23340071
240400                          WS-CALL-ORDER-KEY(S1)                   23350071
240500                          SET COT-DONE     TO TRUE                23360071
240600                          IF WS-COT-KEY(S1, S2) >                 23370071
240700                             WS-CALL-ORDER-KEY(S1)                23380071
240800                             SUBTRACT 1  FROM S2                  23390071
240900                          END-IF                                  23400071
241000                       END-IF                                     23410071
241100                    END-IF                                        23420071
241200*CNC0564A - END                                                   23430071
241300                 ELSE                                             23440071
241400*                                                                 23450071
241500*                   WE DIDN'T FIND A STARTING POINT, SO START     23460071
241600*                   DISPLAYING FROM THE BEGINNING OF THE ARRAY.   23470071
241700*                   SETTING 'S2' TO 0 WILL ACCOMPLISH THIS, AS    23480071
241800*                   IT WILL BE INCREMENTED BY 1 UPON EXITING      23490071
241900*                   THIS PERFORM LOOP.                            23500071
242000*                                                                 23510071
242100                    SET COT-DONE        TO TRUE                   23520071
242200                    MOVE 0              TO S2                     23530071
242300                 END-IF                                           23540071
242400              END-PERFORM                                         23550071
242500           ELSE                                                   23560071
242600              MOVE 1                    TO S2                     23570071
242700           END-IF                                                 23580071
242800           SET NOT-DONE                 TO TRUE                   23590071
242900           PERFORM VARYING S2 FROM S2 BY 1                        23600071
243000                   UNTIL DONE                                     23610071
243100              IF WS-CO-TURN-ARRAY(S1, S2) > SPACES                23620071
243200*CNC0564A - BEG                                                   23630071
243300                 SET TURN-IS-SCHEDULED  TO TRUE                   23640071
243400*CNC0564A - END                                                   23650071
243500                 MOVE SPACES            TO WS-SCHED               23660071
243600                 MOVE WS-COT-SCHEDKEY1(S1, S2)                    23670071
243700                                        TO SCHEDKEY1              23680071
243800                 PERFORM P8025-READ-SCHED                         23690071
243900                 MOVE SPACES            TO UFPTURN-AREA           23700071
244000                 MOVE SCHED1-DIST       TO DIST2                  23710071
244100                 MOVE SCHED1-SUB-DIST   TO SUB-DIST2              23720071
244200                 MOVE SCHED1-BOARD-ID   TO POOL-NAME2             23730071
244300                 MOVE SCHED1-TURN-ID    TO TURN-NBR               23740071
244400                 MOVE SCHED1-CC         TO POOL-CRAFT-CODE2       23750071
244500                 MOVE UFPTURN-AREA      TO UFPTURN                23760071
244600                 PERFORM P8600-READ-UFPTURN                       23770071
244700                 IF NOT SUCCESS                                   23780071
244800                    MOVE 'P1095-2'      TO ERR-PARAGRAPH          23790071
244900                    MOVE UFPTURN        TO ERR-KEY                23800071
245000                    PERFORM P9999-GOT-PROBLEM                     23810071
245100                 END-IF                                           23820071
245200*CNC0564A - BEG                                                   23830071
245300                 IF WS-TURN-PROTECTED-NO(S1, S2)                  23840071
245400                    SET TURN-IS-NOT-SCHEDULED TO TRUE             23850071
245500                 END-IF                                           23860071
245600*CNC0564A - END                                                   23870071
245700                 PERFORM P1500-SETUP-NAME-LINE                    23880071
245800                 PERFORM P1400-CHECK-ASSOCIATED-TURNS             23890071
245900              ELSE                                                23900071
246000                 SET DONE               TO TRUE                   23910071
246100              END-IF                                              23920071
246200           END-PERFORM                                            23930071
246300        END-IF                                                    23940071
246400     END-PERFORM                                                  23950071
246500     .                                                            23960071
246600*                                                                 23970071
246700 P1096-VALIDATE-TURN.                                             23980071
246800*                                                                 23990071
246900     SET DISPLAY-TURN          TO TRUE                            24000071
247000     IF SCHED-REJECT-MARKED                                       24010071
247100        SET DONT-DISPLAY-TURN   TO TRUE                           24020071
247200     END-IF                                                       24030071
247300                                                                  24040071
247400     IF SCHED1-STATS                                              24050071
247500        SET DONT-DISPLAY-TURN   TO TRUE                           24060071
247600     END-IF                                                       24070071
247700*                                                                 24080071
247800*    MAKE SURE THE SCHEDULE STARTS WITHIN 24 HOURS                24090071
247900*                                                                 24100071
248000     IF DISPLAY-TURN                                              24110071
248100        MOVE ZEROES               TO DATE-CONVERSION-PARMS        24120071
248200        SET PARM-DIFF             TO TRUE                         24130071
248300        MOVE SCHED1-START-YYMMDD  TO PARM-PRI-DATE-GREG           24140071
248400        MOVE SCHED1-START-TIME    TO PARM-PRI-TIME                24150071
248500        MOVE HOLD-START-DATE(3:6) TO PARM-SEC-DATE-GREG           24160071
248600        MOVE HOLD-START-TIME      TO PARM-SEC-TIME                24170071
248700        EXEC CICS LINK                                            24180071
248800                  PROGRAM(P903-PGM)                               24190071
248900                  COMMAREA(DATE-CONVERSION-PARMS)                 24200071
249000                  LENGTH(P903-LGTH)                               24210071
249100                  RESP(WS-RESPONSE)                               24220071
249200        END-EXEC                                                  24230071
249300        MOVE WS-RESPONSE          TO FILE-STATUS                  24240071
249400        IF NOT SUCCESS                                            24250071
249500           MOVE 'P1096-1'         TO ERR-PARAGRAPH                24260071
249600           MOVE 'P903'            TO ERR-KEY                      24270071
249700           PERFORM P9999-GOT-PROBLEM                              24280071
249800        END-IF                                                    24290071
249900        IF PARM-RES-TOT-DAYS > 0                                  24300071
250000*CNC0564A - BEG                                                   24310071
250100           IF WS-3A-BIDPK-TIEBRK-PW-BRD                           24320071
250200              SET DE-YYMMDD-FORMAT  TO TRUE                       24330071
250300              MOVE PARM-PRI-DATE-GREG TO DE-YYMMDD                24340071
250400              PERFORM P8998-DATEEDIT                              24350071
250500              IF DE-INVALID-DATE                                  24360071
250600                 MOVE 'P1096-2'     TO ERR-PARAGRAPH              24370071
250700                 MOVE DE-YYMMDD     TO ERR-KEY                    24380071
250800                 PERFORM P9999-GOT-PROBLEM                        24390071
250900              END-IF                                              24400071
251000              MOVE DE-CCYYMMDD      TO DE-COMPARE1-DATE           24410071
251100                                                                  24420071
251200              SET DE-YYMMDD-FORMAT  TO TRUE                       24430071
251300              MOVE SCR02E-START-DATE TO DE-YYMMDD                 24440071
251400              PERFORM P8998-DATEEDIT                              24450071
251500              IF DE-INVALID-DATE                                  24460071
251600                 MOVE 'P1096-3'     TO ERR-PARAGRAPH              24470071
251700                 MOVE DE-YYMMDD     TO ERR-KEY                    24480071
251800                 PERFORM P9999-GOT-PROBLEM                        24490071
251900              END-IF                                              24500071
252000              MOVE DE-CCYYMMDD      TO DE-COMPARE2-DATE           24510071
252100              IF DE-COMPARE1-DATE >= DE-COMPARE2-DATE             24520071
252200                 SET DONT-DISPLAY-STOP TO TRUE                    24530071
252300              END-IF                                              24540071
252400           ELSE                                                   24550071
252500              SET DONT-DISPLAY-STOP  TO TRUE                      24560071
252600           END-IF                                                 24570071
252700*CNC0564A - END                                                   24580071
252800        END-IF                                                    24590071
252900     END-IF                                                       24600071
253000*C1104 - BEG                                                      24610087
253100     MOVE SPACES                    TO WS-SCHED-START-ML-DATE-TIME24620087
253200                                       WS-SCHED-END-ML-DATE-TIME  24630087
253300     IF  DISPLAY-TURN                                             24640087
253400     AND WS-POOL-HOME-LEAD-TIME > 0                               24650087
253500        MOVE SCHED1-START-CE        TO WS-SCHED-START-ML-CE       24660087
253600        MOVE SCHED-END-CE           TO WS-SCHED-END-ML-CE         24670087
253700*                                                                 24680087
253800        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      24690087
253900        SET PARM-SUBTRACT           TO TRUE                       24700087
254000        MOVE SCHED1-START-YYMMDD    TO PARM-PRI-DATE-GREG         24710087
254100        MOVE SCHED1-START-TIME      TO PARM-PRI-HRMN              24720087
254200        MOVE WS-POOL-HOME-LEAD-TIME TO PARM-SEC-HRMN              24730087
254300        EXEC CICS LINK                                            24740087
254400                  PROGRAM(P903-PGM)                               24750087
254500                  COMMAREA(DATE-CONVERSION-PARMS)                 24760087
254600                  LENGTH(P903-LGTH)                               24770087
254700                  RESP(WS-RESPONSE)                               24780087
254800        END-EXEC                                                  24790087
254900        MOVE WS-RESPONSE           TO FILE-STATUS                 24800087
255000        IF NOT SUCCESS                                            24810087
255100           MOVE 'P1096-4'          TO ERR-PARAGRAPH               24820087
255200           MOVE 'P903LINK'         TO ERR-KEY                     24830087
255300           PERFORM P9999-GOT-PROBLEM                              24840087
255400        END-IF                                                    24850087
255500        MOVE PARM-RES-DATE-GREG    TO WS-SCHED-START-ML-DATE      24860087
255600        MOVE PARM-RES-HRMN         TO WS-SCHED-START-ML-TIME      24870087
255700*                                                                 24880087
255800        MOVE ZEROS                  TO DATE-CONVERSION-PARMS      24890087
255900        SET PARM-SUBTRACT           TO TRUE                       24900087
256000        MOVE SCHED-END-YYMMDD       TO PARM-PRI-DATE-GREG         24910087
256100        MOVE SCHED-END-TIME         TO PARM-PRI-HRMN              24920087
256200        MOVE WS-POOL-HOME-LEAD-TIME TO PARM-SEC-HRMN              24930087
256300        EXEC CICS LINK                                            24940087
256400                  PROGRAM(P903-PGM)                               24950087
256500                  COMMAREA(DATE-CONVERSION-PARMS)                 24960087
256600                  LENGTH(P903-LGTH)                               24970087
256700                  RESP(WS-RESPONSE)                               24980087
256800        END-EXEC                                                  24990087
256900        MOVE WS-RESPONSE           TO FILE-STATUS                 25000087
257000        IF NOT SUCCESS                                            25010087
257100           MOVE 'P1096-5'          TO ERR-PARAGRAPH               25020087
257200           MOVE 'P903LINK'         TO ERR-KEY                     25030087
257300           PERFORM P9999-GOT-PROBLEM                              25040087
257400        END-IF                                                    25050087
257500        MOVE PARM-RES-DATE-GREG    TO WS-SCHED-END-ML-DATE        25060087
257600        MOVE PARM-RES-HRMN         TO WS-SCHED-END-ML-TIME        25070087
257700     ELSE                                                         25080087
257800        MOVE SCHED1-START-DATE-TIME TO WS-SCHED-START-ML-DATE-TIME25090087
257900        MOVE SCHED-END-DATE-TIME    TO WS-SCHED-END-ML-DATE-TIME  25100087
258000     END-IF                                                       25110087
258100*C1104 - END                                                      25120087
258200*                                                                 25130071
258300*    IF THE TURN'S START TIME PRECEDES THE FILTER TIME, MAKE SURE 25140071
258400*    THE FILTER TIME FALLS WITHIN THE CALL WINDOW.                25150071
258500*                                                                 25160071
258600     IF DISPLAY-TURN                                              25170071
258700        IF SCHED1-START-DATE-TIME <= HOLD-START-DATE-TIME         25180071
258800           IF SCHED-END-DATE-TIME >= HOLD-START-DATE-TIME         25190071
258900              CONTINUE                                            25200071
259000           ELSE                                                   25210071
259100              SET DONT-DISPLAY-TURN TO TRUE                       25220071
259200           END-IF                                                 25230071
259300        END-IF                                                    25240071
259400     END-IF                                                       25250071
259500*CNC0564A - BEG                                                   25260071
259600*    WHEN WS-3A-BIDPK-TIEBRK-PW-BRD, TURNS NOT CURRENTLY SCHEDULED25270071
259700*    TO PROTECT DISPLAY IN RED, STATUS 'RP' FOR REST PERIOD.      25280071
259800*                                                                 25290071
259900     IF  DISPLAY-TURN                                             25300071
260000     AND WS-3A-BIDPK-TIEBRK-PW-BRD                                25310071
260100*C1104 - BEG                                                      25320087
260200***     IF SCHED1-START-DATE-TIME > HOLD-START-DATE-TIME          25330087
260300        IF WS-SCHED-START-ML-DATE-TIME > HOLD-START-DATE-TIME     25340087
260400           SET DISPLAY-TURN-RD-RED TO TRUE                        25350071
260500        END-IF                                                    25360071
260600        IF WS-SCHED-END-ML-DATE-TIME  < HOLD-START-DATE-TIME      25370087
260700           SET DONT-DISPLAY-TURN   TO TRUE                        25380087
260800        END-IF                                                    25390087
260900*C1104 - END                                                      25400087
261000     END-IF                                                       25410071
261100*CNC0564A- END                                                    25420071
261200*                                                                 25430071
261300*    MAKE SURE THE TURN IS ON THE BOARD AT THE HOME TERMINAL      25440071
261400*                                                                 25450071
261500     IF DISPLAY-TURN                                              25460071
261600*CNC0564A - BEG                                                   25470071
261700     OR DISPLAY-TURN-RD-RED                                       25480071
261800*CNC0564A - END                                                   25490071
261900        MOVE SPACES             TO UFPTURN-AREA                   25500071
262000        MOVE SCHED1-DIST        TO DIST2                          25510071
262100        MOVE SCHED1-SUB-DIST    TO SUB-DIST2                      25520071
262200        MOVE SCHED1-BOARD-ID    TO POOL-NAME2                     25530071
262300        MOVE SCHED1-TURN-ID     TO TURN-NBR                       25540071
262400        MOVE SCHED1-CC          TO POOL-CRAFT-CODE2               25550071
262500        MOVE UFPTURN-AREA       TO UFPTURN                        25560071
262600        PERFORM P8600-READ-UFPTURN                                25570071
262700        IF NOT SUCCESS                                            25580071
262800           IF NO-RECORD-FND                                       25590071
262900              SET DONT-DISPLAY-TURN TO TRUE                       25600071
263000           ELSE                                                   25610071
263100              MOVE 'P1096-6'    TO ERR-PARAGRAPH                  25620087
263200              MOVE UFPTURN      TO ERR-KEY                        25630071
263300              PERFORM P9999-GOT-PROBLEM                           25640071
263400           END-IF                                                 25650071
263500        ELSE                                                      25660071
263600           IF NOT (IN-TOWN AND UFP-ON-BOARD)                      25670071
263700              OR (DIST OF WS-UFP     NOT = DIST2                  25680071
263800              OR SUB-DIST OF WS-UFP  NOT = SUB-DIST2              25690071
263900              OR POOL-NAME OF WS-UFP NOT = POOL-NAME2)            25700071
264000              SET DONT-DISPLAY-TURN TO TRUE                       25710071
264100           END-IF                                                 25720071
264200        END-IF                                                    25730071
264300     END-IF                                                       25740071
264400*                                                                 25750071
264500*    MAKE SURE WE'RE NOT DEALING WITH A SCHEDULE FROM A CYCLE THAT25760071
264600*    WAS OVERLAID WITH A MORE RECENT CYCLE FOR THE SAME TIME      25770071
264700*    PERIOD.                                                      25780071
264800*                                                                 25790071
264900     IF DISPLAY-TURN                                              25800071
265000*CNC0564A - BEG                                                   25810071
265100     OR DISPLAY-TURN-RD-RED                                       25820071
265200*CNC0564A - END                                                   25830071
265300        SET FROM-CURRENT-CYCLE     TO TRUE                        25840071
265400        PERFORM P1097-CHECK-FOR-OVERLAY                           25850071
265500        IF NOT-FROM-CURRENT-CYCLE                                 25860071
265600           SET DONT-DISPLAY-TURN   TO TRUE                        25870071
265700        END-IF                                                    25880071
265800     END-IF.                                                      25890071
265900*                                                                 25900071
266000 P1097-CHECK-FOR-OVERLAY.                                         25910071
266100*                                                                 25920071
266200     PERFORM P8730-ENDBR-SCHEDKEY2                                25930071
266300     MOVE SCHED-KEY2               TO SAVE-SCHED-KEY2             25940071
266400                                                                  25950071
266500*                                                                 25960071
266600*    GET MOST CURRENT STATS RECORD FOR FILTER DATE                25970071
266700*                                                                 25980071
266800     SET SCHED1-STATS                TO TRUE                      25990071
266900     MOVE SPACES                     TO SCHED1-EFF-DATE           26000071
267000     MOVE SCHED-KEY1                 TO SCHEDKEY1                 26010071
267100     MOVE HOLD-START-DATE            TO DE-COMPARE1-DATE          26020071
267200     MOVE HOLD-START-TIME            TO DE-COMPARE1-TIME          26030071
267300                                                                  26040071
267400     SET NOT-SCHED-DONE              TO TRUE                      26050071
267500     SET NOT-FOUND-CYCLE             TO TRUE                      26060071
267600     MOVE HIGH-VALUES                TO WS-END-CYCLE-DATE-TIME    26070071
267700     PERFORM P8010-STARTBR-SCHED                                  26080071
267800     IF NOT SUCCESS                                               26090071
267900        SET SCHED-DONE                 TO TRUE                    26100071
268000     END-IF                                                       26110071
268100     PERFORM UNTIL SCHED-DONE                                     26120071
268200        PERFORM P8020-READNEXT-SCHED                              26130071
268300        IF SUCCESS                                                26140071
268400           IF SCHED1-DIST         = SCR02E-DIST                   26150071
268500              AND SCHED1-SUB-DIST = SCR02E-SUB-DIST               26160071
268600              AND SCHED1-BOARD-ID = SCR02E-POOL                   26170071
268700              AND SCHED1-TURN-ID  = SAVE-SK2-TURN-ID              26180071
268800              AND SCHED1-CC       = SAVE-SK2-CC                   26190071
268900              AND SCHED1-STATS                                    26200071
269000              PERFORM P1098-EVALUATE-STATS-REC                    26210071
269100              IF FOUND-CYCLE                                      26220071
269200                 SET SCHED-DONE        TO TRUE                    26230071
269300                 MOVE SCHED-CYCLE-START-DATE                      26240071
269400                                       TO WS-CYCLE-START-DATE-CENT26250071
269500                 MOVE SCHED-CYCLE-START-TIME                      26260071
269600                                       TO WS-CYCLE-START-TIME     26270071
269700              END-IF                                              26280071
269800           ELSE                                                   26290071
269900              SET SCHED-DONE           TO TRUE                    26300071
270000              IF WS-HOLD-SCHED > SPACES                           26310071
270100                 MOVE WS-HOLD-SCHED    TO WS-SCHED                26320071
270200                 MOVE SPACES           TO WS-HOLD-SCHED           26330071
270300                 MOVE SCHED-CYCLE-START-DATE                      26340071
270400                                       TO WS-CYCLE-START-DATE-CENT26350071
270500                 MOVE SCHED-CYCLE-START-TIME                      26360071
270600                                       TO WS-CYCLE-START-TIME     26370071
270700                 SET FOUND-CYCLE       TO TRUE                    26380071
270800              END-IF                                              26390071
270900           END-IF                                                 26400071
271000        ELSE                                                      26410071
271100           IF NO-RECORD-FND OR END-OF-FILE                        26420071
271200              SET SCHED-DONE         TO TRUE                      26430071
271300              IF WS-HOLD-SCHED > SPACES                           26440071
271400                 MOVE WS-HOLD-SCHED  TO WS-SCHED                  26450071
271500                 MOVE SPACES         TO WS-HOLD-SCHED             26460071
271600                 MOVE SCHED-CYCLE-START-DATE                      26470071
271700                                       TO WS-CYCLE-START-DATE-CENT26480071
271800                 MOVE SCHED-CYCLE-START-TIME                      26490071
271900                                       TO WS-CYCLE-START-TIME     26500071
272000                 SET FOUND-CYCLE     TO TRUE                      26510071
272100              END-IF                                              26520071
272200           ELSE                                                   26530071
272300              MOVE 'P1097-1'         TO ERR-PARAGRAPH             26540071
272400              MOVE SCHEDKEY1         TO ERR-KEY                   26550071
272500              PERFORM P9999-GOT-PROBLEM                           26560071
272600           END-IF                                                 26570071
272700        END-IF                                                    26580071
272800     END-PERFORM                                                  26590071
272900     PERFORM P8030-ENDBR-SCHED                                    26600071
273000     IF NOT-FOUND-CYCLE                                           26610071
273100        MOVE 'P1097-2'               TO ERR-PARAGRAPH             26620071
273200        MOVE SCHEDKEY1               TO ERR-KEY                   26630071
273300        PERFORM P9999-GOT-PROBLEM                                 26640071
273400     END-IF                                                       26650071
273500     MOVE SAVE-SCHED-KEY2            TO SCHEDKEY2                 26660071
273600     PERFORM P8710-STARTBR-SCHEDKEY2                              26670071
273700     PERFORM P8720-READNEXT-SCHEDKEY2                             26680071
273800*                                                                 26690071
273900*    SEE IF SCHEDULE SEQUENCE # IS IN SYNCH WITH THE              26700071
274000*    CYCLE START DATE.  IF IT ISN'T, WE'RE DEALING                26710071
274100*    WITH A RECORD FROM THE PREVIOUS CYCLE WHICH                  26720071
274200*    HAPPENS TO OVERLAY THE CURRENT CYCLE. IGNORE THIS            26730071
274300*    RECORD!                                                      26740071
274400*                                                                 26750071
274500     MOVE ZEROES                          TO DATE-CONVERSION-PARMS26760071
274600     SET PARM-ADD                         TO TRUE                 26770071
274700     MOVE WS-CYCLE-START-DATE             TO PARM-PRI-DATE-GREG   26780071
274800     SUBTRACT 1 FROM SCHED1-SEQ-NUM                               26790071
274900                          GIVING PARM-SEC-DATE-JULIAN             26800071
275000     EXEC CICS LINK                                               26810071
275100               PROGRAM(P903-PGM)                                  26820071
275200               COMMAREA(DATE-CONVERSION-PARMS)                    26830071
275300               LENGTH(P903-LGTH)                                  26840071
275400               RESP(WS-RESPONSE)                                  26850071
275500     END-EXEC                                                     26860071
275600     MOVE WS-RESPONSE                     TO FILE-STATUS          26870071
275700     IF NOT SUCCESS                                               26880071
275800        MOVE 'P1097-4'                    TO ERR-PARAGRAPH        26890071
275900        MOVE 'P903'                       TO ERR-KEY              26900071
276000        PERFORM P9999-GOT-PROBLEM                                 26910071
276100     END-IF                                                       26920071
276200     MOVE SCHED1-START-DATE-TIME          TO                      26930071
276300                                      WS-COMPARE-SCHED-START-DTTM 26940071
276400     IF PARM-RES-DATE-GREG NOT = SCHED1-START-YYMMDD              26950071
276500*CNC0510                                                          26960071
276600        IF SCHED1-SEQ-NUM = 001 AND                               26970071
276700           WS-PREV-SCHED-ASGN = SCHED1-ASSIGNMENT                 26980071
276800*          PREVIOUS ASSIGNMENT MATCH COMPARE DATE AND TIME        26990071
276900           IF WS-COMPARE-SCHED-START-DTTM < WS-PREV-SCHED-END-DTTM27000071
277000              SET NOT-FROM-CURRENT-CYCLE  TO TRUE                 27010071
277100           END-IF                                                 27020071
277200        ELSE                                                      27030071
277300           IF SCHED1-SEQ-NUM NOT = 001                            27040071
277400              SET NOT-FROM-CURRENT-CYCLE  TO TRUE                 27050071
277500           ELSE                                                   27060071
277600*             COMPARE FOR OVERLAPPING TURN FROM ALL PREV ENTRIES. 27070071
277700              IF S1 > 1                                           27080071
277800*                                                                 27090071
277900*                'S3' INDEXES THE PREVIOUS ARRAY ENTRY THAT WE'RE 27100071
278000*                     COMPARING AGAINST.                          27110071
278100                                                                  27120071
278200                 SET NOT-DONE1      TO TRUE                       27130071
278300                 MOVE S1            TO S3                         27140071
278400                 MOVE CRAFT-ARRAY-SUB TO S5                       27150071
278500                 PERFORM UNTIL S3 = 0 OR DONE1                    27160071
278600                    IF SCHED1-ASSIGNMENT = WS-COT-ASGN(S5, S3)    27170071
278700                       MOVE WS-COT-END-DATE-TIME(S5, S3)          27180071
278800                                    TO WS-COMPARE-SCHED-END-DTTM  27190071
278900                       IF WS-COMPARE-SCHED-START-DTTM             27200071
279000                        < WS-COMPARE-SCHED-END-DTTM               27210071
279100                          SET NOT-FROM-CURRENT-CYCLE TO TRUE      27220071
279200                          SET DONE1 TO TRUE                       27230071
279300                       END-IF                                     27240071
279400                    END-IF                                        27250071
279500                    SUBTRACT 1    FROM S3                         27260071
279600                 END-PERFORM                                      27270071
279700              END-IF                                              27280071
279800           END-IF                                                 27290071
279900        END-IF                                                    27300071
280000*CNC0510-END                                                      27310071
280100     END-IF.                                                      27320071
280200*                                                                 27330071
280300 P1098-EVALUATE-STATS-REC.                                        27340071
280400*                                                                 27350071
280500     MOVE SCHED-CYCLE-START-DATE       TO DE-COMPARE2-DATE        27360071
280600     MOVE SCHED-CYCLE-START-TIME       TO DE-COMPARE2-TIME        27370071
280700                                                                  27380071
280800     IF DE-COMPARE1-DATE-TIME >= DE-COMPARE2-DATE-TIME            27390071
280900        IF SCHEDULE-POOL                                          27400071
281000           PERFORM P1099-GET-PROFILE-REC                          27410071
281100           MOVE SPACES                 TO DATE-CONVERSION-PARMS   27420071
281200           MOVE DE-COMPARE2-YYMMDD     TO PARM-PRI-DATE-GREG      27430071
281300           MOVE CNTL-75-SCH-CYCLE-DAYS TO PARM-SEC-DATE-JULIAN    27440071
281400           SET PARM-ADD                TO TRUE                    27450071
281500           EXEC CICS LINK                                         27460071
281600                     PROGRAM(P903-PGM)                            27470071
281700                     COMMAREA(DATE-CONVERSION-PARMS)              27480071
281800                     LENGTH(P903-LGTH)                            27490071
281900                     RESP(WS-RESPONSE)                            27500071
282000           END-EXEC                                               27510071
282100           MOVE WS-RESPONSE            TO FILE-STATUS             27520071
282200           IF NOT SUCCESS                                         27530071
282300              MOVE 'P1098-1'           TO ERR-PARAGRAPH           27540071
282400              MOVE 'P903'              TO ERR-KEY                 27550071
282500              PERFORM P9999-GOT-PROBLEM                           27560071
282600           END-IF                                                 27570071
282700           MOVE PARM-RES-DATE-GREG     TO WS-SCHED-END-YYMMDD     27580071
282800           MOVE PARM-RES-GREG-CENT     TO WS-SCHED-END-CE         27590071
282900           MOVE CNTL-75-SCH-START-TIME TO WS-SCHED-END-TIME       27600071
283000           IF DE-COMPARE1-DATE-TIME <= WS-SCHED-END-DATE-TIME     27610071
283100              MOVE WS-SCHED        TO WS-HOLD-SCHED               27620071
283200           ELSE                                                   27630071
283300              MOVE SPACES          TO WS-HOLD-SCHED               27640071
283400           END-IF                                                 27650071
283500        ELSE                                                      27660071
283600           MOVE WS-SCHED           TO WS-HOLD-SCHED               27670071
283700        END-IF                                                    27680071
283800     ELSE                                                         27690071
283900        IF WS-HOLD-SCHED > SPACES                                 27700071
284000           SET FOUND-CYCLE              TO TRUE                   27710071
284100           MOVE SCHED-CYCLE-START-DATE  TO WS-END-CYCLE-DATE      27720071
284200           MOVE SCHED-CYCLE-START-TIME  TO WS-END-CYCLE-TIME      27730071
284300           MOVE WS-HOLD-SCHED           TO WS-SCHED               27740071
284400           MOVE SPACES                  TO WS-HOLD-SCHED          27750071
284500        ELSE                                                      27760071
284600           MOVE 'P1098-2'           TO ERR-PARAGRAPH              27770071
284700           MOVE SCHEDKEY1           TO ERR-KEY                    27780071
284800           PERFORM P9999-GOT-PROBLEM                              27790071
284900        END-IF                                                    27800071
285000     END-IF.                                                      27810071
285100*                                                                 27820071
285200 P1099-GET-PROFILE-REC.                                           27830071
285300*                                                                 27840071
285400     MOVE SPACES                 TO WS-CNTL-FILE                  27850071
285500     SET SCH-POOL-PROFILE-REC    TO TRUE                          27860071
285600     MOVE SCR02E-DIST            TO WS-PROF-DIST                  27870071
285700     MOVE SCR02E-SUB-DIST        TO WS-PROF-SUB-DIST              27880071
285800     MOVE SCR02E-POOL            TO WS-PROF-BOARD                 27890071
285900     MOVE '0'                    TO WS-PROF-TERM                  27900071
286000     MOVE 'D'                    TO WS-PROF-TYPE                  27910071
286100     MOVE WS-PROFILE             TO CNTL-75-PROFILE               27920071
286200     MOVE WS-CRAFT-CODE(CRAFT-ARRAY-SUB)                          27930071
286300                                 TO WS-CRAFT-CODE-CHECK           27940071
286400     IF ENGINE-CRAFT                                              27950071
286500        MOVE 'E'                 TO WS-CREW-UNIT                  27960071
286600     ELSE                                                         27970071
286700        MOVE 'T'                 TO WS-CREW-UNIT                  27980071
286800     END-IF                                                       27990071
286900     MOVE WS-CREW-UNIT           TO CNTL-75-CREW-UNIT             28000071
287000     MOVE CNTLKEY-AREA           TO CNTLKEY                       28010071
287100     PERFORM P8000-READ-CNTLFILE                                  28020071
287200     IF NOT SUCCESS                                               28030071
287300        IF NOT NO-RECORD-FND                                      28040071
287400           MOVE 'P1099-1'        TO ERR-PARAGRAPH                 28050071
287500           MOVE CNTLKEY          TO ERR-KEY                       28060071
287600           PERFORM P9999-GOT-PROBLEM                              28070071
287700        ELSE                                                      28080071
287800           MOVE '*'              TO CNTL-75-CREW-UNIT             28090071
287900           MOVE CNTLKEY-AREA     TO CNTLKEY                       28100071
288000           PERFORM P8000-READ-CNTLFILE                            28110071
288100           IF NOT SUCCESS                                         28120071
288200              MOVE 'P1099-2'     TO ERR-PARAGRAPH                 28130071
288300              MOVE CNTLKEY       TO ERR-KEY                       28140071
288400              PERFORM P9999-GOT-PROBLEM                           28150071
288500           END-IF                                                 28160071
288600        END-IF                                                    28170071
288700     END-IF.                                                      28180071
288800*                                                                 28190071
288900 P1110-READ-NEXT-POS.                                             28200071
289000*                                                                 28210071
289100     EXEC CICS READ                                               28220071
289200               GTEQ                                               28230071
289300               DATASET(UFP-VIA-POSITION)                          28240071
289400               INTO(WS-UFP)                                       28250071
289500               LENGTH(UFPPOS-RLGTH)                               28260071
289600               RIDFLD(UFPPOS)                                     28270071
289700               KEYLENGTH(UFPPOS-KLGTH)                            28280071
289800               RESP(WS-RESPONSE)                                  28290071
289900     END-EXEC                                                     28300071
290000     MOVE WS-RESPONSE TO FILE-STATUS.                             28310071
290100*                                                                 28320071
290200 P1200-START-UFP-TURN.                                            28330071
290300*                                                                 28340071
290400     MOVE WS-UFP-TURN-KEY TO UFPTURN                              28350071
290500     EXEC CICS STARTBR                                            28360071
290600               DATASET(UFP-VIA-TURN-NBR)                          28370071
290700               RIDFLD(UFPTURN)                                    28380071
290800               GTEQ                                               28390071
290900               RESP(WS-RESPONSE)                                  28400071
291000     END-EXEC                                                     28410071
291100     MOVE WS-RESPONSE TO FILE-STATUS.                             28420071
291200*                                                                 28430071
291300 P1210-READ-NEXT-TURN.                                            28440071
291400*                                                                 28450071
291500     EXEC CICS READNEXT                                           28460071
291600               DATASET(UFP-VIA-TURN-NBR)                          28470071
291700               INTO(WS-UFP)                                       28480071
291800               LENGTH(UFPTURN-RLGTH)                              28490071
291900               RIDFLD(UFPTURN)                                    28500071
292000               KEYLENGTH(UFPTURN-KLGTH)                           28510071
292100               RESP(WS-RESPONSE)                                  28520071
292200     END-EXEC                                                     28530071
292300     MOVE WS-RESPONSE TO FILE-STATUS.                             28540071
292400*                                                                 28550071
292500 P1220-ENDBR-UFP-TURN.                                            28560071
292600*                                                                 28570071
292700     EXEC CICS ENDBR                                              28580071
292800               DATASET(UFP-VIA-TURN-NBR)                          28590071
292900               RESP(WS-RESPONSE)                                  28600071
293000     END-EXEC.                                                    28610071
293100*                                                                 28620071
293200 P1400-CHECK-ASSOCIATED-TURNS.                                    28630071
293300*                                                                 28640071
293400     MOVE WS-CRAFT-CODE(CC-ARRAY-SUB)  TO WS-COMPARE-CRAFT        28650071
293500     PERFORM VARYING CC-ARRAY-SUB FROM 1 BY 1                     28660071
293600        UNTIL CC-ARRAY-SUB > CRAFT-ARRAY-MAX                      28670071
293700        IF WS-ASSOC-CRAFT(CC-ARRAY-SUB) = WS-COMPARE-CRAFT        28680071
293800           SET ASSOCIATED-TURN         TO TRUE                    28690071
293900           MOVE WS-CRAFT-CODE(CC-ARRAY-SUB)                       28700071
294000                                       TO POOL-CRAFT-CODE2        28710071
294100           MOVE UFPTURN-AREA           TO UFPTURN                 28720071
294200           PERFORM P8600-READ-UFPTURN                             28730071
294300           IF NOT SUCCESS                                         28740071
294400              IF NO-RECORD-FND                                    28750071
294500                 IF OPTIONAL-CRAFT(CC-ARRAY-SUB)                  28760071
294600                    ADD CRAFT-SUB-INCREMENT                       28770071
294700                                     TO WS-CRAFT-SUB(CC-ARRAY-SUB)28780071
294800                 ELSE                                             28790071
294900                    MOVE 'P1400-1'   TO ERR-PARAGRAPH             28800071
295000                    MOVE UFPTURN     TO ERR-KEY                   28810071
295100                    PERFORM P9999-GOT-PROBLEM                     28820071
295200                 END-IF                                           28830071
295300              ELSE                                                28840071
295400                 MOVE 'P1400-2'      TO ERR-PARAGRAPH             28850071
295500                 MOVE UFPTURN        TO ERR-KEY                   28860071
295600                 PERFORM P9999-GOT-PROBLEM                        28870071
295700              END-IF                                              28880071
295800           ELSE                                                   28890071
295900              PERFORM P1500-SETUP-NAME-LINE                       28900071
296000           END-IF                                                 28910071
296100        END-IF                                                    28920071
296200     END-PERFORM                                                  28930071
296300     MOVE CRAFT-ARRAY-SUB            TO CC-ARRAY-SUB.             28940071
296400*                                                                 28950071
296500 P1500-SETUP-NAME-LINE.                                           28960071
296600*                                                                 28970071
296700     MOVE SPACES TO WS-VARIABLE-LINE-1                            28980071
296800                    WS-VARIABLE-LINE-3                            28990071
296900                    WS-VARIABLE-LINE-4                            29000071
297000*CNC0564A - 04/29/15 - BEG                                        29010071
297100                    WS-VARIABLE-LINE-5                            29020071
297200*CNC0564A - 04/29/15 - END                                        29030071
297300                    LOC-DIST                                      29040071
297400                    LOC-SUB-DIST                                  29050071
297500                    LOC-POOL                                      29060071
297600                    LOC-I-O                                       29070071
297700     MOVE WS-CRAFT-SUB(CC-ARRAY-SUB) TO ARRAY-SUB                 29080071
297800     ADD CRAFT-SUB-INCREMENT TO WS-CRAFT-SUB(CC-ARRAY-SUB)        29090071
297900     IF ARRAY-SUB NOT > WS-CRAFT-MAX(CC-ARRAY-SUB)                29100071
298000        MOVE UFPTURN-AREA TO P02ECA-TURN-KEY(ARRAY-SUB)           29110071
298100        IF CALL-ORDER-REQ                                         29120071
298200           SET DE-YYMMDD-FORMAT         TO TRUE                   29130071
298300           SET DE-REFORMAT-ONLY         TO TRUE                   29140071
298400           MOVE UFP-POS-DATE-TIME(1:6)  TO DE-YYMMDD              29150071
298500           PERFORM P8998-DATEEDIT                                 29160071
298600           IF DE-INVALID-DATE                                     29170071
298700              MOVE 'P1500-A'            TO ERR-PARAGRAPH          29180071
298800              MOVE UFP-POS-DATE-TIME    TO ERR-KEY                29190071
298900              MOVE SCHED-KEY1           TO ERR-SENTENCE           29200071
299000              PERFORM P9999-GOT-PROBLEM                           29210071
299100           END-IF                                                 29220071
299200           MOVE SCHED-KEY2(9:13)                                  29230071
299300                       TO P02ECA-CALL-ORDER-KEY(ARRAY-SUB)(1:13)  29240071
299400           MOVE DE-YYMMDD-CE                                      29250071
299500                       TO P02ECA-CALL-ORDER-KEY(ARRAY-SUB)(14:2)  29260071
299600           MOVE UFP-POSITION-TIME                                 29270071
299700                       TO P02ECA-CALL-ORDER-KEY(ARRAY-SUB)(16:14) 29280071
299800        END-IF                                                    29290071
299900        PERFORM PXXXX-GET-UFP-EMPS                                29300071
300000        MOVE SPACES TO WS-MSTR                                    29310071
300100        IF HAVE-PERMANENT-EMPLOYEE                                29320071
300200           MOVE FIRST-EMP-NBR TO MSTRNBRK                         29330071
300300           PERFORM P8500-READ-MASTER                              29340071
300400           MOVE EMP-NAME OF WS-MSTR TO SCR02E-NAME(ARRAY-SUB)     29350071
300500           IF POSITION-BOARD                                      29360071
300600***           MOVE EMP-HOME-ONLY-FLAG   TO WS-VL3-HOME            29370071
300700*                                                                 29380071
300800*             CNC0183 - MATT - 12/11/99                           29390071
300900*                                                                 29400071
301000              PERFORM P1510-CALC-PERS-MTOD-REST                   29410071
301100*                                                                 29420071
301200              IF (HOLD-POOL-REST-CODE = '1' OR '3')               29430071
301300                 SET DE-YYMMDD-FORMAT         TO TRUE             29440071
301400                 MOVE EMP-US-RSTD-NUM(1:6)    TO DE-YYMMDD        29450071
301500                 PERFORM P8998-DATEEDIT                           29460071
301600                 MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE 29470071
301700                 MOVE EMP-US-RSTD-NUM(7:4)    TO DE-COMPARE1-TIME 29480071
301800*             AND EMP-US-RSTD-NUM  > WS-LOCAL-DATE-TIME           29490071
301900                IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT29500071
302000                    MOVE EMP-US-RSTD-TIME     TO WS-VL3-USHR      29510071
302100                END-IF                                            29520071
302200              END-IF                                              29530071
302300           END-IF                                                 29540071
302400           IF TEMPORARY-ASGNMT > SPACE                            29550071
302500              MOVE 'TV' TO SCR02E-LO(ARRAY-SUB)                   29560071
302600           ELSE                                                   29570071
302700              IF NOT AVAILABLE                                    29580071
302800*CNC0573 - BEG                                                    29590090
302900                 IF  PSTCA-FROM-FLD-MENU                          29600090
303000                 AND PSTCA-FLD-MENU-OPT NOT = '004'               29610090
303100                    PERFORM P9856-RETRIEVE-CNTL-INFO              29620090
303200                    IF P956-MASK-FLD-SCR-YES                      29630090
303300*CNC0576 - BEG                                                    29640091
303400                    OR P956-MASK-HOLD-TURN                        29650091
303500                       MOVE '** '       TO SCR02E-LO(ARRAY-SUB)   29660091
303600                       IF P956-MASK-HOLD-TURN                     29670091
303700                          MOVE 'HT'     TO WS-VL3-RSN             29680091
303800                       ELSE                                       29690091
303900                          MOVE '**'     TO WS-VL3-RSN             29700091
304000                       END-IF                                     29710091
304100*CNC0576 - END                                                    29720091
304200                    ELSE                                          29730090
304300                       MOVE LAYOFF-CODE TO SCR02E-LO(ARRAY-SUB)   29740090
304400                       IF LAYOFF-EM-CODE > SPACES                 29750090
304500                          IF P956-SHOW-RSN-ON-SCR-YES             29760090
304600                             IF POSITION-BOARD OR                 29770090
304700                                CALL-ORDER-REQ                    29780090
304800                                MOVE LAYOFF-EM-CODE  TO WS-VL3-RSN29790090
304900                             ELSE                                 29800090
305000                                MOVE SPACES          TO WS-VL3-RSN29810090
305100                             END-IF                               29820090
305200                          ELSE                                    29830090
305300                             MOVE SPACES             TO WS-VL3-RSN29840090
305400                          END-IF                                  29850090
305500                       END-IF                                     29860090
305600                    END-IF                                        29870090
305700                 ELSE                                             29880090
305800                    MOVE LAYOFF-CODE TO SCR02E-LO(ARRAY-SUB)      29890090
305900                    IF LAYOFF-EM-CODE > SPACES                    29900090
306000                       PERFORM P9856-RETRIEVE-CNTL-INFO           29910090
306100                       IF P956-SHOW-RSN-ON-SCR-YES                29920090
306200                          IF POSITION-BOARD OR                    29930090
306300                             CALL-ORDER-REQ                       29940090
306400                             MOVE LAYOFF-EM-CODE TO WS-VL3-RSN    29950090
306500                          ELSE                                    29960090
306600                             MOVE SPACES       TO WS-VL3-RSN      29970090
306700                          END-IF                                  29980090
306800                       ELSE                                       29990090
306900                          MOVE SPACES          TO WS-VL3-RSN      30000090
307000                       END-IF                                     30010090
307100                    END-IF                                        30020090
307200                 END-IF                                           30030090
307300*CNC0573 - END                                                    30040090
307400              ELSE                                                30050071
307500                 IF ON-DUTY-ASGNMT > SPACE                        30060071
307600                    AND ON-DUTY-ASGNMT NOT = UFPTURN-AREA         30070071
307700                    MOVE 'OT' TO SCR02E-LO(ARRAY-SUB)             30080071
307800                 END-IF                                           30090071
307900              END-IF                                              30100071
308000           END-IF                                                 30110071
308100*CNC0600-B                                                        30111099
308300           IF  WS-CAN-WRR-NEW                                     30113099
308400                  AND EMP-NBR OF WS-MSTR > ZEROES                 30114099
308500              PERFORM P6100-SET-EMP-STATUS-FLAGS                  30114199
308600           END-IF                                                 30124099
308700*CNC0600-E                                                        30124199
308800           INITIALIZE WS-RETURN-DATE                              30125071
308900                      WS-RETURN-DY                                30130071
309000           EVALUATE TRUE                                          30140071
309100              WHEN OFF-MILES-DAYS                                 30150071
309200                 PERFORM P9856-RETRIEVE-CNTL-INFO                 30160071
309300                 IF P956-ST-RSN-NBR-DAYS-REQ                      30170071
309400                 OR P956-ST-RSN-EXP-DATE-REQ                      30180071
309500                    PERFORM P1520-GET-DUEBACK-DATE                30190071
309600                    IF WS-DUEBACK-FOUND-Y                         30200071
309700                       MOVE TASK-LO-EXP-TIME                      30210071
309800                                        TO WS-RETURN-DATE         30220071
309900                       MOVE TASK-LO-EXP-DATE(5:2)                 30230071
310000                                        TO WS-RETURN-DY           30240071
310100                       PERFORM P4190-SET-RETURN-DATE              30250071
310200                    END-IF                                        30260071
310300                 ELSE                                             30270071
310400                    PERFORM P4150-OFF-MILES-RETURN-DATE           30280071
310500                    PERFORM P4190-SET-RETURN-DATE                 30290071
310600                 END-IF                                           30300071
310700              WHEN VACATION                                       30310071
310800                 PERFORM P4170-VACATION-RETURN-DATE               30320071
310900                 PERFORM P4190-SET-RETURN-DATE                    30330071
313200              WHEN OTHER                                          30341071
313300*CNC0492 - FOR OTHER LAYOFF CODES, GET DUEBACK/BOOK-ON DATE/TIME  30350071
313400                 IF NOT AVAILABLE  AND                            30360071
313500                    NOT WORKING    AND                            30370071
313600                    NOT TO-PLACE                                  30380071
313700                    AND LAYOFF-TIME NUMERIC                       30390071
313800                    AND LAYOFF-TIME > ZERO                        30400071
313900                    PERFORM P1520-GET-DUEBACK-DATE                30410071
314000                    IF WS-DUEBACK-FOUND-Y                         30420071
314100*CNC0556<<                                                        30430071
314200                       IF TASK-DUE-BACK                           30440071
314300                          CONTINUE                                30450071
314400                       ELSE                                       30460071
314500                          IF TASK-LO-EXP-DATE-TIME > SPACES       30470071
314600                             MOVE TASK-LO-EXP-TIME                30480071
314700                                        TO WS-RETURN-DATE         30490071
314800                             MOVE TASK-LO-EXP-DATE(5:2)           30500071
314900                                        TO WS-RETURN-DY           30510071
315000                          ELSE                                    30520071
315100                             IF EFFECTIVE-DATE-TIME-CENT > ZEROES 30530071
315200                                SET TZ-IN-EASTERN-ZONE   TO TRUE  30540071
315300                                MOVE EFFECTIVE-DATE-TIME          30550071
315400                                               TO TZ-IN-DATE-TIME 30560071
315500                                MOVE TASK-TIME-ZONE               30570071
315600                                               TO TZ-OUT-ZONE     30580071
315700                                PERFORM P8996-TIMEZONE            30590071
315800                                MOVE TZ-OUT-TIME                  30600071
315900                                        TO WS-RETURN-DATE         30610071
316000                                MOVE TZ-OUT-DD                    30620071
316100                                        TO WS-RETURN-DY           30630071
316200                             END-IF                               30640071
316300                          END-IF                                  30650071
316400                       END-IF                                     30660071
316500*CNC0556>>                                                        30670071
316600                       PERFORM P4190-SET-RETURN-DATE              30680071
316700                    END-IF                                        30690071
316800                 ELSE                                             30700071
316900                    CONTINUE                                      30710071
317000                 END-IF                                           30720071
317100           END-EVALUATE                                           30730071
317200        ELSE                                                      30740071
317300           IF UFP-BLANK-TURN                                      30750071
317400              IF PSTCA-SUB = 2                                    30760071
317500                 MOVE 'DISCONTINUATION' TO SCR02E-NAME(ARRAY-SUB) 30770071
317600              ELSE                                                30780071
317700                 MOVE 'DISCONTINUED' TO SCR02E-NAME(ARRAY-SUB)    30790071
317800              END-IF                                              30800071
317900           ELSE                                                   30810071
318000              IF PSTCA-SUB = 2                                    30820071
318100               MOVE '<< TOUR VACANT >>' TO SCR02E-NAME(ARRAY-SUB) 30830071
318200              ELSE                                                30840071
318300               MOVE '<< OPEN TURN >>' TO SCR02E-NAME(ARRAY-SUB)   30850071
318400              END-IF                                              30860071
318500           END-IF                                                 30870071
318600        END-IF                                                    30880071
318700        MOVE SPACES TO WS-MSTR                                    30890071
318800        IF HAVE-TEMPORARY-EMPLOYEE                                30900071
318900           AND FIRST-EMP-NBR NOT = TEMP-EMP-ONE                   30910071
319000           MOVE SPACES TO SCR02E-LO(ARRAY-SUB)                    30920071
319100           MOVE TEMP-EMP-ONE TO MSTRNBRK                          30930071
319200           PERFORM P8500-READ-MASTER                              30940071
319300           IF POSITION-BOARD                                      30950071
319400              MOVE SPACES               TO WS-VARIABLE-LINE-3     30960071
319500***           MOVE EMP-HOME-ONLY-FLAG   TO WS-VL3-HOME            30970071
319600*                                                                 30980071
319700*             CNC0183 - MATT - 12/11/99                           30990071
319800*                                                                 31000071
319900              PERFORM P1510-CALC-PERS-MTOD-REST                   31010071
320000*                                                                 31020071
320100              IF (HOLD-POOL-REST-CODE = '1' OR '3')               31030071
320200                 SET DE-YYMMDD-FORMAT        TO TRUE              31040071
320300                 MOVE EMP-US-RSTD-NUM(1:6)   TO DE-YYMMDD         31050071
320400                 PERFORM P8998-DATEEDIT                           31060071
320500                 MOVE DE-CCYYMMDD            TO DE-COMPARE1-DATE  31070071
320600                 MOVE EMP-US-RSTD-NUM(7:4)   TO DE-COMPARE1-TIME  31080071
320700*             AND EMP-US-RSTD-NUM  > WS-LOCAL-DATE-TIME           31090071
320800                IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT31100071
320900                 MOVE EMP-US-RSTD-TIME     TO WS-VL3-USHR         31110071
321000                END-IF                                            31120071
321100              END-IF                                              31130071
321200           ELSE                                                   31140071
321300              MOVE SCR02E-NAME(ARRAY-SUB)                         31150071
321400                                 TO WS-VL1-VARIABLE               31160071
321500           END-IF                                                 31170071
321600           MOVE EMP-NAME OF WS-MSTR TO SCR02E-NAME(ARRAY-SUB)     31180071
321700           IF TEMPORARY-ASGNMT > SPACE                            31190071
321800              AND TEMPORARY-ASGNMT NOT = UFPTURN-AREA             31200071
321900              MOVE 'TV' TO SCR02E-LO(ARRAY-SUB)                   31210071
322000           ELSE                                                   31220071
322100              IF NOT AVAILABLE                                    31230071
322200*CNC0573 - BEG                                                    31240090
322300                 IF  PSTCA-FROM-FLD-MENU                          31250090
322400                 AND PSTCA-FLD-MENU-OPT NOT = '004'               31260090
322500                    PERFORM P9856-RETRIEVE-CNTL-INFO              31270090
322600                    IF P956-MASK-FLD-SCR-YES                      31280090
322700*CNC0576 - BEG                                                    31290091
322800                    OR P956-MASK-HOLD-TURN                        31300091
322900                       MOVE '** '       TO SCR02E-LO(ARRAY-SUB)   31310091
323000                       IF P956-MASK-HOLD-TURN                     31320091
323100                          MOVE 'HT'     TO WS-VL3-RSN             31330091
323200                       ELSE                                       31340091
323300                          MOVE '**'     TO WS-VL3-RSN             31350091
323400                       END-IF                                     31360091
323500*CNC0576 - END                                                    31370091
323600                    ELSE                                          31380090
323700                       MOVE LAYOFF-CODE TO SCR02E-LO(ARRAY-SUB)   31390090
323800                       IF LAYOFF-EM-CODE > SPACES                 31400090
323900                          IF P956-SHOW-RSN-ON-SCR-YES             31410090
324000                             IF POSITION-BOARD OR                 31420090
324100                                CALL-ORDER-REQ                    31430090
324200                                MOVE LAYOFF-EM-CODE TO WS-VL3-RSN 31440090
324300                             ELSE                                 31450090
324400                                MOVE SPACES         TO WS-VL3-RSN 31460090
324500                             END-IF                               31470090
324600                          ELSE                                    31480090
324700                             MOVE SPACES            TO WS-VL3-RSN 31490090
324800                       END-IF                                     31500090
324900                    END-IF                                        31510090
325000                 ELSE                                             31520090
325100                    MOVE LAYOFF-CODE TO SCR02E-LO(ARRAY-SUB)      31530090
325200                    IF LAYOFF-EM-CODE > SPACES                    31540090
325300                       PERFORM P9856-RETRIEVE-CNTL-INFO           31550090
325400                       IF P956-SHOW-RSN-ON-SCR-YES                31560090
325500                          IF POSITION-BOARD OR                    31570090
325600                             CALL-ORDER-REQ                       31580090
325700                             MOVE LAYOFF-EM-CODE TO WS-VL3-RSN    31590090
325800                          ELSE                                    31600090
325900                             MOVE SPACES       TO WS-VL3-RSN      31610090
326000                          END-IF                                  31620090
326100                       ELSE                                       31630090
326200                          MOVE SPACES          TO WS-VL3-RSN      31640090
326300                       END-IF                                     31650090
326400                    END-IF                                        31660090
326500                 END-IF                                           31670090
326600*CNC0573 - END                                                    31680090
326700              ELSE                                                31690071
326800                 IF ON-DUTY-ASGNMT > SPACE                        31700071
326900                    AND ON-DUTY-ASGNMT NOT = UFPTURN-AREA         31710071
327000                    MOVE 'OT' TO SCR02E-LO(ARRAY-SUB)             31720071
327100                 END-IF                                           31730071
327200              END-IF                                              31740071
327300           END-IF                                                 31750071
327400*CNC0600-B                                                        31751099
327500           IF  WS-CAN-WRR-NEW                                     31751399
327600                  AND EMP-NBR OF WS-MSTR > ZEROES                 31751499
327700              PERFORM P6100-SET-EMP-STATUS-FLAGS                  31751599
327800           END-IF                                                 31751699
327900*CNC0600-E                                                        31759099
328000           INITIALIZE WS-RETURN-DATE                              31760071
328100                      WS-RETURN-DY                                31770071
328200           EVALUATE TRUE                                          31780071
328300              WHEN OFF-MILES-DAYS                                 31790071
328400                 PERFORM P9856-RETRIEVE-CNTL-INFO                 31800071
328500                 IF P956-ST-RSN-NBR-DAYS-REQ                      31810071
328600                 OR P956-ST-RSN-EXP-DATE-REQ                      31820071
328700                    PERFORM P1520-GET-DUEBACK-DATE                31830071
328800                    IF WS-DUEBACK-FOUND-Y                         31840071
328900                       MOVE TASK-LO-EXP-TIME                      31850071
329000                                        TO WS-RETURN-DATE         31860071
329100                       MOVE TASK-LO-EXP-DATE(5:2)                 31870071
329200                                        TO WS-RETURN-DY           31880071
329300                       PERFORM P4190-SET-RETURN-DATE              31890071
329400                    END-IF                                        31900071
329500                 ELSE                                             31910071
329600                    PERFORM P4150-OFF-MILES-RETURN-DATE           31920071
329700                    PERFORM P4190-SET-RETURN-DATE                 31930071
329800                 END-IF                                           31940071
329900              WHEN VACATION                                       31950071
330000                 PERFORM P4170-VACATION-RETURN-DATE               31960071
330100                 PERFORM P4190-SET-RETURN-DATE                    31970071
332400              WHEN OTHER                                          31981071
332500*CNC0492 - FOR OTHER LAYOFF CODES, GET DUEBACK/BOOK-ON DATE/TIME  31990071
332600                 IF NOT AVAILABLE  AND                            32000071
332700                    NOT WORKING    AND                            32010071
332800                    NOT TO-PLACE                                  32020071
332900                    AND LAYOFF-TIME NUMERIC                       32030071
333000                    AND LAYOFF-TIME > ZERO                        32040071
333100                    PERFORM P1520-GET-DUEBACK-DATE                32050071
333200                    IF WS-DUEBACK-FOUND-Y                         32060071
333300*CNC0556<<                                                        32070071
333400                       IF TASK-DUE-BACK                           32080071
333500                          CONTINUE                                32090071
333600                       ELSE                                       32100071
333700                          IF TASK-LO-EXP-DATE-TIME > SPACES       32110071
333800                             MOVE TASK-LO-EXP-TIME                32120071
333900                                        TO WS-RETURN-DATE         32130071
334000                             MOVE TASK-LO-EXP-DATE(5:2)           32140071
334100                                        TO WS-RETURN-DY           32150071
334200                          ELSE                                    32160071
334300                             IF EFFECTIVE-DATE-TIME-CENT > ZEROES 32170071
334400                                SET TZ-IN-EASTERN-ZONE   TO TRUE  32180071
334500                                MOVE EFFECTIVE-DATE-TIME          32190071
334600                                               TO TZ-IN-DATE-TIME 32200071
334700                                MOVE TASK-TIME-ZONE               32210071
334800                                               TO TZ-OUT-ZONE     32220071
334900                                PERFORM P8996-TIMEZONE            32230071
335000                                MOVE TZ-OUT-TIME                  32240071
335100                                        TO WS-RETURN-DATE         32250071
335200                                MOVE TZ-OUT-DD                    32260071
335300                                        TO WS-RETURN-DY           32270071
335400                             END-IF                               32280071
335500                          END-IF                                  32290071
335600                       END-IF                                     32300071
335700*CNC0556>>                                                        32310071
335800                       PERFORM P4190-SET-RETURN-DATE              32320071
335900                    END-IF                                        32330071
336000                 ELSE                                             32340071
336100                    CONTINUE                                      32350071
336200                 END-IF                                           32360071
336300           END-EVALUATE                                           32370071
336400        END-IF                                                    32380071
336500        IF IN-TOWN                                                32390071
336600           AND DIST OF WS-UFP = DIST2                             32400071
336700           AND SUB-DIST OF WS-UFP = SUB-DIST2                     32410071
336800           AND POOL-NAME OF WS-UFP = POOL-NAME2                   32420071
336900           AND NOT HAVE-ON-DUTY-EMPLOYEE                          32430071
337000           CONTINUE                                               32440071
337100        ELSE                                                      32450071
337200           MOVE SPACES TO WS-MSTR                                 32460071
337300           IF HAVE-ON-DUTY-EMPLOYEE                               32470071
337400              MOVE ON-DUTY-EMP TO MSTRNBRK                        32480071
337500              PERFORM P8500-READ-MASTER                           32490071
337600              IF POSITION-BOARD                                   32500071
337700                 MOVE SPACES               TO WS-VARIABLE-LINE-3  32510071
337800***              MOVE EMP-HOME-ONLY-FLAG   TO WS-VL3-HOME         32520071
337900*                                                                 32530071
338000*                CNC0183 - MATT - 12/11/99                        32540071
338100*                                                                 32550071
338200                 PERFORM P1510-CALC-PERS-MTOD-REST                32560071
338300*                                                                 32570071
338400                 IF (HOLD-POOL-REST-CODE = '1' OR '3')            32580071
338500                   SET DE-YYMMDD-FORMAT       TO TRUE             32590071
338600                   MOVE EMP-US-RSTD-NUM(1:6)  TO DE-YYMMDD        32600071
338700                   PERFORM P8998-DATEEDIT                         32610071
338800                   MOVE DE-CCYYMMDD           TO DE-COMPARE1-DATE 32620071
338900                   MOVE EMP-US-RSTD-NUM(7:4)  TO DE-COMPARE1-TIME 32630071
339000*                AND EMP-US-RSTD-NUM  > WS-LOCAL-DATE-TIME        32640071
339100                   IF DE-COMPARE1-DATE-TIME >                     32650071
339200                              WS-LOCAL-DATE-TIME-CENT             32660071
339300                      MOVE EMP-US-RSTD-TIME   TO WS-VL3-USHR      32670071
339400                   END-IF                                         32680071
339500                 END-IF                                           32690071
339600              ELSE                                                32700071
339700                 IF EMP-NAME OF WS-MSTR NOT =                     32710071
339800                                   SCR02E-NAME(ARRAY-SUB)         32720071
339900                    MOVE SCR02E-NAME(ARRAY-SUB) TO WS-VL1-VARIABLE32730071
340000                 END-IF                                           32740071
340100              END-IF                                              32750071
340200              MOVE SPACES TO SCR02E-LO(ARRAY-SUB)                 32760071
340300              MOVE EMP-NAME OF WS-MSTR TO SCR02E-NAME(ARRAY-SUB)  32770071
308100*CNC0600-B                                                        30111099
308300              IF WS-CAN-WRR-NEW                                   30113099
308400                     AND EMP-NBR OF WS-MSTR > ZEROES              30114099
308500                 PERFORM P6100-SET-EMP-STATUS-FLAGS               30114199
308600              END-IF                                              30124099
308700*CNC0600-E                                                        30124199
340400              IF NOT AVAILABLE                                    32780071
340500*CNC0573 - BEG                                                    32790090
340600                 IF  PSTCA-FROM-FLD-MENU                          32800090
340700                 AND PSTCA-FLD-MENU-OPT NOT = '004'               32810090
340800                    PERFORM P9856-RETRIEVE-CNTL-INFO              32820090
340900                    IF P956-MASK-FLD-SCR-YES                      32830090
341000*CNC0576 - BEG                                                    32840091
341100                    OR P956-MASK-HOLD-TURN                        32850091
341200                       MOVE '** '       TO SCR02E-LO(ARRAY-SUB)   32860091
341300                       IF P956-MASK-HOLD-TURN                     32870091
341400                          MOVE 'HT'     TO WS-VL3-RSN             32880091
341500                       ELSE                                       32890091
341600                          MOVE '**'     TO WS-VL3-RSN             32900091
341700                       END-IF                                     32910091
341800*CNC0576 - END                                                    32920091
341900                    ELSE                                          32930090
342000                       MOVE LAYOFF-CODE TO SCR02E-LO(ARRAY-SUB)   32940090
342100                       IF LAYOFF-EM-CODE > SPACES                 32950090
342200                          IF P956-SHOW-RSN-ON-SCR-YES             32960090
342300                             IF POSITION-BOARD OR                 32970090
342400                                CALL-ORDER-REQ                    32980090
342500                                MOVE LAYOFF-EM-CODE TO WS-VL3-RSN 32990090
342600                             ELSE                                 33000090
342700                                MOVE SPACES         TO WS-VL3-RSN 33010090
342800                             END-IF                               33020090
342900                          ELSE                                    33030090
343000                             MOVE SPACES            TO WS-VL3-RSN 33040090
343100                          END-IF                                  33050090
343200                       END-IF                                     33060090
343300                    END-IF                                        33070090
343400                 ELSE                                             33080090
343500                    MOVE LAYOFF-CODE TO SCR02E-LO(ARRAY-SUB)      33090090
343600                    IF LAYOFF-EM-CODE > SPACES                    33100090
343700                       PERFORM P9856-RETRIEVE-CNTL-INFO           33110090
343800                       IF P956-SHOW-RSN-ON-SCR-YES                33120090
343900                          IF POSITION-BOARD OR                    33130090
344000                             CALL-ORDER-REQ                       33140090
344100                             MOVE LAYOFF-EM-CODE TO WS-VL3-RSN    33150090
344200                          ELSE                                    33160090
344300                             MOVE SPACES       TO WS-VL3-RSN      33170090
344400                          END-IF                                  33180090
344500                       ELSE                                       33190090
344600                          MOVE SPACES          TO WS-VL3-RSN      33200090
344700                       END-IF                                     33210090
344800                    END-IF                                        33220090
344900                 END-IF                                           33230090
345000*CNC0573 - END                                                    33240090
345100              END-IF                                              33250071
345200           ELSE                                                   33260071
345300              MOVE SCR02E-NAME(ARRAY-SUB) TO WS-VL1-VARIABLE      33270071
345400              MOVE SPACES TO SCR02E-LO(ARRAY-SUB)                 33280071
345500              IF UFP-BLANK-TURN                                   33290071
345600                IF PSTCA-SUB = 2                                  33300071
345700                  MOVE 'DISCONTINU-VACANT'                        33310071
345800                         TO SCR02E-NAME(ARRAY-SUB)                33320071
345900                ELSE                                              33330071
346000                  MOVE 'DISCONTINUED-VACANT'                      33340071
346100                         TO SCR02E-NAME(ARRAY-SUB)                33350071
346200                END-IF                                            33360071
      *CNC0600-B
                      MOVE SPACES       TO WS-EMP-STATUS-FLAG
      *CNC0600-E
346300              ELSE                                                33370071
346400                IF PSTCA-SUB = 2                                  33380071
346500                  MOVE ' TOUR VACANT' TO SCR02E-NAME(ARRAY-SUB)   33390071
346600                ELSE                                              33400071
346700                  MOVE ' VACANT TURN' TO SCR02E-NAME(ARRAY-SUB)   33410071
346800                END-IF                                            33420071
      *CNC0600-B
                      MOVE SPACES       TO WS-EMP-STATUS-FLAG
      *CNC0600-E
346900              END-IF                                              33430071
347000           END-IF                                                 33440071
347100        END-IF                                                    33450071
347200*CNC0600-B                                                        33450199
347300        IF WS-CAN-WRR-NEW AND WS-EMP-STATUS-RED                   33450299
347400           MOVE RED     TO SCR02E-NAME-COLOR(ARRAY-SUB)           33451099
347500        ELSE                                                      33451299
347600           IF WS-CAN-WRR-NEW AND WS-EMP-STATUS-YELLOW             33451399
347700              MOVE YELLOW  TO  SCR02E-NAME-COLOR(ARRAY-SUB)       33451499
347800           END-IF                                                 33451699
347900        END-IF                                                    33451799
              MOVE SPACES        TO WS-EMP-STATUS-FLAG
348000*CNC0600-E                                                        33452099
348100        IF CALL-ORDER-REQ                                         33460071
348200*CNC0564A - 04/29/15 - BEG                                        33470071
348300           IF WS-3A-BIDPK-TIEBRK-PW-BRD                           33480071
348400              MOVE WS-VL3-RSN          TO WS-VL5-RSN              33490071
348500              IF TURN-IS-NOT-SCHEDULED                            33500071
348600                 IF SCR02E-LO(ARRAY-SUB) NOT > SPACES             33510071
348700                    MOVE 'RP'     TO SCR02E-LO(ARRAY-SUB)         33520071
348800                 END-IF                                           33530071
348900                 MOVE RED     TO SCR02E-CC-COLOR(ARRAY-SUB)       33540071
349000                                    SCR02E-TURN-COLOR(ARRAY-SUB)  33550071
349100                                    SCR02E-I-O-COLOR(ARRAY-SUB)   33560071
349200                                    SCR02E-POS-COLOR(ARRAY-SUB)   33570071
349300                                    SCR02E-NAME-COLOR(ARRAY-SUB)  33580071
349400                                    SCR02E-LO-COLOR(ARRAY-SUB)    33590071
349500                                  SCR02E-VARIABLE-COLOR(ARRAY-SUB)33600071
349600              END-IF                                              33610071
349700              MOVE WS-VL3-RETURN-DATE  TO WS-VL5-RETURN-DATE      33620071
349800              MOVE WS-VL3-RETURN-DY    TO WS-VL5-RETURN-DY        33630071
349900              MOVE WS-VL3-REST-TIME    TO WS-VL5-REST-TIME        33640071
350000              MOVE WS-VL3-REST-DY      TO WS-VL5-REST-DY          33650071
350100              MOVE WS-VL3-USHR         TO WS-VL5-USHR             33660071
350200              MOVE SCHED1-START-DD     TO WS-VL5-DAY              33670071
350300              MOVE SCHED1-START-TIME   TO WS-VL5-START            33680071
350400              MOVE SCHED-END-TIME      TO WS-VL5-END              33690071
350500           ELSE                                                   33700071
350600              MOVE WS-VL3-RSN          TO WS-VL4-RSN              33710071
350700              MOVE WS-VL3-RETURN-DATE  TO WS-VL4-RETURN-DATE      33720071
350800              MOVE WS-VL3-RETURN-DY    TO WS-VL4-RETURN-DY        33730071
350900              MOVE WS-VL3-SHORT-TURN   TO WS-VL4-SHORT-TURN       33740071
351000              MOVE WS-VL3-REST-TIME    TO WS-VL4-REST-TIME        33750071
351100              MOVE WS-VL3-REST-DY      TO WS-VL4-REST-DY          33760071
351200              MOVE WS-VL3-USHR         TO WS-VL4-USHR             33770071
351300              MOVE SCHED1-START-TIME   TO WS-VL4-START            33780071
351400              MOVE SCHED-END-TIME      TO WS-VL4-END              33790071
351500              MOVE SCHED1-CALL-ORDER   TO WS-VL4-CO               33800071
351600           END-IF                                                 33810071
351700*CNC0564A - 04/29/15 - END                                        33820071
351800        END-IF                                                    33830071
351900        MOVE TURN-NBR OF WS-UFP TO SCR02E-TURN(ARRAY-SUB)         33840071
352000        IF ASSOCIATED-TURN                                        33850071
352100           MOVE SPACES TO SCR02E-POS(ARRAY-SUB)                   33860071
352200        ELSE                                                      33870071
352300           IF TURN-BOARD                                          33880071
352400              OR WS-TURN-POS                                      33890071
352500              EXEC CICS ENDBR                                     33900071
352600                        DATASET(UFP-VIA-TURN-NBR)                 33910071
352700                        RESP(WS-RESPONSE)                         33920071
352800              END-EXEC                                            33930071
352900              MOVE UFPTURN-AREA TO P910-TURN-PARM                 33940071
353000              EXEC CICS LINK                                      33950071
353100                        PROGRAM(P910-PGM)                         33960071
353200                        COMMAREA(P910-UFP-POS-PARMS)              33970071
353300                        LENGTH(P910-LGTH)                         33980071
353400                        RESP(WS-RESPONSE)                         33990071
353500              END-EXEC                                            34000071
353600              MOVE WS-RESPONSE TO FILE-STATUS                     34010071
353700              IF NOT SUCCESS                                      34020071
353800                 MOVE 'P1500-1' TO ERR-PARAGRAPH                  34030071
353900                 PERFORM P9999-GOT-PROBLEM                        34040071
354000              END-IF                                              34050071
354100              MOVE UFPTURN      TO WS-UFP-TURN-KEY                34060071
354200              PERFORM P1200-START-UFP-TURN                        34070071
354300              IF NOT SUCCESS                                      34080071
354400                 MOVE 'P1500-2' TO ERR-PARAGRAPH                  34090071
354500                 MOVE UFPTURN   TO ERR-KEY                        34100071
354600                 PERFORM P9999-GOT-PROBLEM                        34110071
354700              END-IF                                              34120071
354800              PERFORM P1210-READ-NEXT-TURN                        34130071
354900              IF NOT SUCCESS                                      34140071
355000                 MOVE 'P1500-3' TO ERR-PARAGRAPH                  34150071
355100                 MOVE UFPTURN   TO ERR-KEY                        34160071
355200                 PERFORM P9999-GOT-PROBLEM                        34170071
355300              END-IF                                              34180071
355400              MOVE P910-POS-PARM-NUM TO SCR02E-POS(ARRAY-SUB)     34190071
355500           ELSE                                                   34200071
355600              ADD 1 TO P02ECA-HOLD-POS-NUM(CC-ARRAY-SUB)          34210071
355700              MOVE P02ECA-HOLD-POS(CC-ARRAY-SUB)                  34220071
355800                 TO SCR02E-POS(ARRAY-SUB)                         34230071
355900           END-IF                                                 34240071
356000        END-IF                                                    34250071
356100        IF TURN-BOARD                                             34260071
356200           IF NOT HAVE-PERMANENT-EMPLOYEE                         34270071
356300              AND NOT HAVE-TEMPORARY-EMPLOYEE                     34280071
356400              CONTINUE                                            34290071
356500           ELSE                                                   34300071
356600              IF NOT-REP-TURN                                     34310071
356700                 MOVE 'N' TO WS-VL1-INFO-1                        34320071
356800              END-IF                                              34330071
356900              IF SHORT-TURN-LAST                                  34340071
357000                 MOVE 'S' TO WS-VL1-INFO-2                        34350071
357100              END-IF                                              34360071
357200           END-IF                                                 34370071
357300           IF UFP-BLANK-TURN                                      34380071
357400              MOVE 'B' TO WS-VL1-INFO-3                           34390071
357500           ELSE                                                   34400071
357600              IF CO-POOL AND CONDUCTOR-GETS-2-BRAKEMEN            34410071
357700                 MOVE '2' TO WS-VL1-INFO-3                        34420071
357800              END-IF                                              34430071
357900           END-IF                                                 34440071
358000        END-IF                                                    34450071
358100        IF NOT ASSOCIATED-TURN                                    34460071
358200           IF (ENGINE-POOL-CRAFT AND EN-ID-POOL)                  34470071
358300              OR (TRAIN-POOL-CRAFT AND TR-ID-POOL)                34480071
358400              SET DE-YYMMDD-FORMAT         TO TRUE                34490071
358500              MOVE UFP-POS-DATE-TIME(1:6)  TO DE-YYMMDD           34500071
358600              PERFORM P8998-DATEEDIT                              34510071
358700              MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE    34520071
358800              MOVE UFP-POS-DATE-TIME(7:4)  TO DE-COMPARE1-TIME    34530071
358900              IF UFP-ID-INACTIVE-BOARD                            34540071
359000*                OR UFP-POS-DATE-TIME > PRESENT-TIME              34550071
359100                 OR DE-COMPARE1-DATE-TIME > WS-PRESENT-TIME-CENT  34560071
359200                 MOVE 'I'             TO SCR02E-I-O(ARRAY-SUB)    34570071
359300              ELSE                                                34580071
359400                 MOVE IN-OUT-TERMINAL TO SCR02E-I-O(ARRAY-SUB)    34590071
359500              END-IF                                              34600071
359600           ELSE                                                   34610071
359700              MOVE IN-OUT-TERMINAL    TO SCR02E-I-O(ARRAY-SUB)    34620071
359800           END-IF                                                 34630071
359900        END-IF                                                    34640071
360000        IF TURN-BOARD                                             34650071
360100           IF TRAIN-SYMBOL NOT = SPACES                           34660071
360200* DLO 940610  IF SCR02E-VARIABLE(ARRAY-SUB) > SPACES              34670071
360300* DLO            MOVE SCR02E-VARIABLE(ARRAY-SUB) TO NM-FLD        34680071
360400              IF WS-VL1-VARIABLE            > SPACES              34690071
360500                 MOVE WS-VL1-VARIABLE       TO NM-FLD             34700071
360600                 MOVE TRAIN-SYMBOL          TO TR-FLD             34710071
360700                 MOVE WORK-FIELD            TO WS-VL1-VARIABLE    34720071
360800              ELSE                                                34730071
360900                 MOVE TRAIN-SYMBOL          TO TRAIN-FLD          34740071
361000                 MOVE TRAIN-FIELD           TO WS-VL1-VARIABLE    34750071
361100              END-IF                                              34760071
361200           END-IF                                                 34770071
361300        END-IF                                                    34780071
361400        IF TURN-BOARD                                             34790071
361500           IF DIST OF WS-UFP NOT = DIST2 OR                       34800071
361600              SUB-DIST OF WS-UFP NOT = SUB-DIST2 OR               34810071
361700              POOL-NAME OF WS-UFP NOT = POOL-NAME2                34820071
361800               MOVE DIST OF WS-UFP      TO LOC-DIST               34830071
361900               MOVE SUB-DIST OF WS-UFP  TO LOC-SUB-DIST           34840071
362000               MOVE POOL-NAME           TO LOC-POOL               34850071
362100               MOVE IN-OUT-TERMINAL     TO LOC-I-O                34860071
362200           END-IF                                                 34870071
362300        ELSE                                                      34880071
362400           IF SCR02E-DIST NOT = DIST2 OR                          34890071
362500              SCR02E-SUB-DIST NOT = SUB-DIST2 OR                  34900071
362600              SCR02E-POOL NOT = POOL-NAME2                        34910071
362700               MOVE DIST2 OF WS-UFP     TO LOC-DIST               34920071
362800               MOVE SUB-DIST2 OF WS-UFP TO LOC-SUB-DIST           34930071
362900               MOVE POOL-NAME2          TO LOC-POOL               34940071
363000               MOVE IN-OUT-TERMINAL     TO LOC-I-O                34950071
363100           END-IF                                                 34960071
363200        END-IF                                                    34970071
363300        IF POSITION-BOARD                                         34980071
363400           IF UFP-SHORT-TURN-COUNT > ZEROES                       34990071
363500              MOVE UFP-SHORT-TURN-COUNT TO WS-VL3-SHORT-TURN      35000071
363600           END-IF                                                 35010071
363700***        IF WS-VL3-HOME NOT = 'Y'                               35020071
363800***           MOVE SPACES TO WS-VL3-HOME                          35030071
363900***        END-IF                                                 35040071
364000           IF WS-VL3-USHR = ZEROES                                35050071
364100              MOVE SPACES TO WS-VL3-USHR                          35060071
364200           END-IF                                                 35070071
364300           IF LOC-DIST > SPACES                                   35080071
364400              MOVE LOCATION-AREA    TO WS-VL3-TURN-INFO           35090071
364500           END-IF                                                 35100071
364600           IF WS-TURN-POS                                         35110071
364700              IF TRAIN-SYMBOL NOT = SPACES                        35120071
364800                  MOVE TRAIN-SYMBOL          TO TRAIN-FLD-3       35130071
364900                  MOVE TRAIN-FIELD-3         TO WS-VL3-TURN-INFO  35140071
365000              END-IF                                              35150071
365100           END-IF                                                 35160071
365200           IF CALL-ORDER-REQ                                      35170071
365300*CNC0564A - 04/29/15 - BEG                                        35180071
365400              IF WS-3A-BIDPK-TIEBRK-PW-BRD                        35190071
365500                 MOVE WS-VARIABLE-LINE-5                          35200071
365600                                     TO SCR02E-VARIABLE(ARRAY-SUB)35210071
365700              ELSE                                                35220071
365800                 MOVE WS-VARIABLE-LINE-4                          35230071
365900                                     TO SCR02E-VARIABLE(ARRAY-SUB)35240071
366000              END-IF                                              35250071
366100*CNC0564A - 04/29/15 - END                                        35260071
366200           ELSE                                                   35270071
366300             MOVE WS-VARIABLE-LINE-3 TO SCR02E-VARIABLE(ARRAY-SUB)35280071
366400           END-IF                                                 35290071
366500        ELSE                                                      35300071
366600           IF LOC-DIST > SPACES                                   35310071
366700              MOVE LOCATION-AREA    TO WS-VL1-VARIABLE-B          35320071
366800           END-IF                                                 35330071
366900           MOVE WS-VARIABLE-LINE-1  TO SCR02E-VARIABLE(ARRAY-SUB) 35340071
367000        END-IF                                                    35350071
367100     END-IF                                                       35360071
367200*                                                                 35370071
367300*    HAVE WE FILLED UP THE PAGE YET                               35380071
367400*                                                                 35390071
367500     SET DONE TO TRUE                                             35400071
367600     PERFORM VARYING J FROM 1 BY 1                                35410071
367700             UNTIL J > CRAFT-ARRAY-MAX                            35420071
367800        IF WS-CRAFT-SUB(J) NOT > WS-CRAFT-MAX(J)                  35430071
367900           MOVE '0' TO DONE-CODE                                  35440071
368000        END-IF                                                    35450071
368100     END-PERFORM.                                                 35460071
368200*===============================================================* 35470071
368300 P1510-CALC-PERS-MTOD-REST.                                       35480071
368400*===============================================================* 35490071
368500     MOVE ZEROS                           TO WS-REST-DATE-TIME    35500071
368600                                             DE-COMPARE1-DATE-TIME35510071
368700     IF EMP-PERS-REST > '0000000000'                              35520071
368800        SET DE-YYMMDD-FORMAT              TO TRUE                 35530071
368900        MOVE EMP-PERS-REST-DATE           TO DE-YYMMDD            35540071
369000        PERFORM P8998-DATEEDIT                                    35550071
369100        MOVE DE-CCYYMMDD                  TO DE-COMPARE1-DATE     35560071
369200        MOVE EMP-PERS-REST-TIME           TO DE-COMPARE1-TIME     35570071
369300     END-IF                                                       35580071
369400                                                                  35590071
369500     IF HOLD-POOL-REST-CODE = '2' OR '3'                          35600071
369600        IF EMP-MTOD > '0000000000'                                35610071
369700           SET DE-YYMMDD-FORMAT           TO TRUE                 35620071
369800           MOVE EMP-MTOD-DATE             TO DE-YYMMDD            35630071
369900           PERFORM P8998-DATEEDIT                                 35640071
370000           MOVE DE-CCYYMMDD               TO DE-COMPARE2-DATE     35650071
370100           MOVE EMP-MTOD-TIME             TO DE-COMPARE2-TIME     35660071
370200           IF DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME       35670071
370300              MOVE DE-COMPARE2-DATE-TIME  TO DE-COMPARE1-DATE-TIME35680071
370400           END-IF                                                 35690071
370500        END-IF                                                    35700071
370600     END-IF                                                       35710071
370700     IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT           35720071
370800        MOVE DE-COMPARE1-TIME             TO WS-VL3-REST-TIME     35730071
370900        MOVE DE-COMPARE1-DD               TO WS-VL3-REST-DY       35740071
371000     END-IF                                                       35750071
371100*MET 04/12/2006                                                   35760071
371200     IF  EXCUSED-ABSENCE                                          35770071
371300     AND LAYOFF-EM-CODE = '69'                                    35780071
371400        PERFORM P1520-GET-DUEBACK-DATE                            35790071
371500        IF WS-DUEBACK-FOUND-Y                                     35800071
371600*CNC0556<<                                                        35810071
371700***        MOVE TASK-LO-EXP-TIME          TO WS-VL3-RETURN-DATE   35820071
371800***        MOVE TASK-LO-EXP-DATE (5:2)    TO WS-VL3-RETURN-DY     35830071
371900           IF TASK-DUE-BACK                                       35840071
372000              CONTINUE                                            35850071
372100           ELSE                                                   35860071
372200              IF TASK-LO-EXP-DATE-TIME > SPACES                   35870071
372300                MOVE TASK-LO-EXP-TIME       TO WS-VL3-RETURN-DATE 35880071
372400                MOVE TASK-LO-EXP-DATE (5:2) TO WS-VL3-RETURN-DY   35890071
372500              ELSE                                                35900071
372600                 IF EFFECTIVE-DATE-TIME-CENT > ZEROES             35910071
372700                    SET TZ-IN-EASTERN-ZONE   TO TRUE              35920071
372800                    MOVE EFFECTIVE-DATE-TIME TO TZ-IN-DATE-TIME   35930071
372900                    MOVE TASK-TIME-ZONE      TO TZ-OUT-ZONE       35940071
373000                    PERFORM P8996-TIMEZONE                        35950071
373100                    MOVE TZ-OUT-TIME      TO WS-VL3-RETURN-DATE   35960071
373200                    MOVE TZ-OUT-DD        TO WS-VL3-RETURN-DY     35970071
373300                 END-IF                                           35980071
373400              END-IF                                              35990071
373500           END-IF                                                 36000071
373600*CNC0556>>                                                        36010071
373700        END-IF                                                    36020071
373800     END-IF                                                       36030071
373900     .                                                            36040071
374000*===============================================================* 36050071
374100 P1520-GET-DUEBACK-DATE.                                          36060071
374200*===============================================================* 36070071
374300                                                                  36080071
374400     MOVE SPACES                        TO TASK-EMPLOYEE-KEY      36090071
374500     SET WS-DUEBACK-FOUND-N             TO TRUE                   36100071
374600     MOVE EMP-NBR OF WS-MSTR            TO EMP-NBR OF WS-TASK     36110071
374700     PERFORM P8300-START-TASK-FILE                                36120071
374800     IF SUCCESS                                                   36130071
374900        PERFORM P8310-READNEXT-TASK-FILE                          36140071
375000        PERFORM UNTIL NOT SUCCESS                                 36150071
375100           OR WS-DUEBACK-FOUND-Y                                  36160071
375200           OR EMP-NBR OF WS-MSTR NOT = EMP-NBR OF WS-TASK         36170071
375300           IF   TASK-LAYOFF-MARKUP                                36180071
375400           AND (TASK-DUE-BACK OR TASK-LO1 = 'A')                  36190071
375500              SET WS-DUEBACK-FOUND-Y    TO TRUE                   36200071
375600           ELSE                                                   36210071
375700              PERFORM P8310-READNEXT-TASK-FILE                    36220071
375800           END-IF                                                 36230071
375900        END-PERFORM                                               36240071
376000     END-IF                                                       36250071
376100     PERFORM P8320-ENDBR-TASK-FILE                                36260071
376200     .                                                            36270071
376300*                                                                 36280071
376400***************************************************************** 36290071
376500 P4150-OFF-MILES-RETURN-DATE.                                     36300071
376600***************************************************************** 36310071
376700     IF EMP-MILES-DATE NUMERIC                                    36320071
376800        AND EMP-MILES-DATE-NUM > 0                                36330071
376900        AND EMP-MILES-DATE-NUM < 32                               36340071
377000*       MOVE SYS-MO TO WS-RETURN-DATE-MM                          36350071
377100        MOVE WS-SYS-MO TO WS-RETURN-DATE-MM                       36360071
377200        MOVE EMP-MILES-DATE-NUM TO WS-RETURN-DATE-DD              36370071
377300*       IF EMP-MILES-DATE-NUM < SYS-DY                            36380071
377400        IF EMP-MILES-DATE-NUM < WS-SYS-DY                         36390071
377500           ADD 1 TO WS-RETURN-DATE-MM                             36400071
377600           IF WS-RETURN-DATE-MM > 12                              36410071
377700              MOVE 1 TO WS-RETURN-DATE-MM                         36420071
377800           END-IF                                                 36430071
377900        END-IF                                                    36440071
378000     ELSE                                                         36450071
378100        MOVE EMP-MILES-DATE TO WS-RETURN-DATE                     36460071
378200     END-IF                                                       36470071
378300     .                                                            36480071
378400                                                                  36490071
378500***************************************************************** 36500071
378600 P4170-VACATION-RETURN-DATE.                                      36510071
378700*    FOR EMPLOYEES ON VACATION, FIND THE BOOK-ON RECORD IN        36520071
378800*    THE TASK LIST.  THIS CONTAINS THE EXPECTED RETURN DATE.      36530071
378900***************************************************************** 36540071
379000     INITIALIZE TASK-EMPLOYEE-KEY                                 36550071
379100     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                36560071
379200     PERFORM P8300-START-TASK-FILE                                36570071
379300     IF SUCCESS                                                   36580071
379400        SET TASK-NOT-DONE TO TRUE                                 36590071
379500        PERFORM P8310-READNEXT-TASK-FILE                          36600071
379600        PERFORM UNTIL TASK-DONE                                   36610071
379700           IF SUCCESS                                             36620071
379800              AND EMP-NBR OF WS-MSTR = EMP-NBR OF WS-TASK         36630071
379900              IF TASK-LAYOFF-MARKUP                               36640092
380000*C1129-BEG                                                        36650092
380100                AND TASK-LO1 = 'A'                                36660092
380200*C1129-END                                                        36670092
380300                 SET TASK-DONE TO TRUE                            36680071
380400                 MOVE EFF-MO OF WS-TASK TO WS-RETURN-DATE-MM      36690071
380500                 MOVE EFF-DY OF WS-TASK TO WS-RETURN-DATE-DD      36700071
380600              ELSE                                                36710071
380700                 PERFORM P8310-READNEXT-TASK-FILE                 36720071
380800              END-IF                                              36730071
380900           ELSE                                                   36740071
381000              SET TASK-DONE TO TRUE                               36750071
381100           END-IF                                                 36760071
381200        END-PERFORM                                               36770071
381300     END-IF                                                       36780071
381400     PERFORM P8320-ENDBR-TASK-FILE                                36790071
381500     .                                                            36800071
381600                                                                  36810071
381700***************************************************************** 36820071
381800 P4190-SET-RETURN-DATE.                                           36830071
381900***************************************************************** 36840071
382000     IF POSITION-BOARD                                            36850071
382100        MOVE WS-RETURN-DATE TO WS-VL3-RETURN-DATE                 36860071
382200        MOVE WS-RETURN-DY   TO WS-VL3-RETURN-DY                   36870071
382300     END-IF                                                       36880071
382400     IF TURN-BOARD                                                36890071
382500       IF PSTCA-SUB = 1                                           36900071
382600          MOVE 'RETURN - ' TO WS-VL1-RETURN-TEXT                  36910071
382700       ELSE                                                       36920071
382800          MOVE 'RETURN - ' TO WS-VL1-RETURN-TEXT                  36930071
382900       END-IF                                                     36940071
383000       MOVE WS-RETURN-DATE TO WS-VL1-RETURN-DATE                  36950071
383100       MOVE WS-RETURN-DY   TO WS-VL1-RETURN-DY                    36960071
383200     END-IF                                                       36970071
383300     .                                                            36980071
496900*                                                                 48170199
497000*CNC0600-B                                                        48170299
497100 P6100-SET-EMP-STATUS-FLAGS.                                      48170399
497200      MOVE SPACES TO WS-EMP-STATUS-FLAG                           48170499
497300      MOVE IN-OUT-TERMINAL            TO AT-HOME-TERM-FLAG        48170599
497400      MOVE EMP-NBR OF WS-MSTR         TO MSTR3NBRK                48170699
497500      PERFORM P8550-READ-MSTR3-READ                               48170799
497600      EVALUATE TRUE                                               48170899
497700         WHEN SUCCESS                                             48170999
497800            MOVE ZEROS                TO DATE-CONVERSION-PARMS    48171099
497900            SET PARM-ADD              TO TRUE                     48171199
498000            MOVE WS-LOCAL-DATE        TO PARM-PRI-DATE-GREG       48171299
498100            MOVE WS-LOCAL-TIME        TO PARM-PRI-HRMN            48171399
498200            MOVE WS-RESET-BK-LEAD-TIME TO PARM-SEC-HRMN           48171499
498300            EXEC CICS LINK                                        48171599
498400                PROGRAM(P903-PGM)                                 48171699
498500                COMMAREA(DATE-CONVERSION-PARMS)                   48171799
498600                LENGTH(P903-LGTH)                                 48171899
498700                RESP(WS-RESPONSE)                                 48171999
498800            END-EXEC                                              48172099
498900            MOVE WS-RESPONSE            TO FILE-STATUS            48172199
499000            IF NOT SUCCESS                                        48172299
499100               MOVE 'P6100-1'           TO ERR-PARAGRAPH          48172399
499200               MOVE 'P903LINK'          TO ERR-KEY                48172499
499300               PERFORM P9999-GOT-PROBLEM                          48172599
499400            END-IF                                                48172699
499500            MOVE PARM-RES-DATE-GREG     TO WS-WRR-6HRS-AFTER-DATE 48172799
499700            MOVE PARM-RES-HRMN          TO WS-WRR-6HRS-AFTER-TIME
                  SET WS-DO-NOT-CHECK-60-192-STATUS TO TRUE
499900            IF (WS-LOCAL-CUR-DTTM-YYMMDDHHSS
                           > MSTR3-SYSTEM-RESET-BRK AND
                              MSTR3-SYSTEM-RESET-BRK > '0000000000')
                        SET WS-CHECK-60-192-STATUS TO TRUE
      **  IF CURRENT TIME > RESET BREAK TIME, THEN CHECK FOR 60/192
      **  VALIDATION ONLY
                  ELSE
                     IF WS-LOCAL-CUR-DTTM-YYMMDDHHSS >
                                            MSTR3-7DAY-END-PERIOD-CH
                           AND MSTR3-7DAY-END-PERIOD > '0000000000'
500210                  SET WS-EMP-STATUS-RED TO TRUE
                     END-IF
                  END-IF
                  IF NOT WS-EMP-STATUS-RED AND
                                      NOT WS-CHECK-60-192-STATUS
499900               IF (MSTR3-7DAY-END-PERIOD-CH
                                > WS-WRR-6HRS-AFTER-CH)
                            OR MSTR3-7DAY-END-PERIOD NOT > '0000000000'
                        SET WS-CHECK-60-192-STATUS TO TRUE
500200               ELSE
                        IF AT-HOME-TERM
500210                     SET WS-EMP-STATUS-RED TO TRUE
500200                  ELSE
                           IF NOT-AT-HOME-TERM
500210                        SET WS-EMP-STATUS-YELLOW TO TRUE
501800                     END-IF
501800                  END-IF
501800               END-IF
501800            END-IF
                  IF WS-CHECK-60-192-STATUS AND NOT WS-EMP-STATUS-RED
500010               PERFORM P6200-GET-EMP-REST-STATUS
                  END-IF
501900         WHEN NO-RECORD-FND                                       48175199
      * WHEN NO RESET DETAILS, CHECK THE 60/192
500010              PERFORM P6200-GET-EMP-REST-STATUS                   48174899
502100         WHEN OTHER                                               48175399
502200            MOVE 'P6100-2' TO ERR-PARAGRAPH                       48175499
502300            MOVE MSTR3NBRK TO ERR-KEY                             48175599
502400            PERFORM P9999-GOT-PROBLEM                             48175699
502500      END-EVALUATE                                                48175799
502600     .                                                            48181599
502700*                                                                 48181699
502800 P6200-GET-EMP-REST-STATUS.                                       48181799
502900* CHECK FOR 07 DAYS TTOD                                          48181899
511400     INITIALIZE PS08-COMMAREA-PARMS
511500     SET PS08-INQUIRY-FUN            TO TRUE
511600     MOVE EMP-NBR OF WS-MSTR            TO PS08-EMP-NBR
512000     MOVE 07                         TO PS08-CALC-NBR-DAYS
512100     SET DE-YYMMDD-FORMAT            TO TRUE
512200     MOVE WS-LOCAL-DATE              TO DE-YYMMDD
512300     PERFORM P8998-DATEEDIT
512400     MOVE DE-CCYYMMDD                TO PS08-OD-DATE-CENT
512500                                        PS08-CALC-DATE-CENT
512600     MOVE WS-LOCAL-TIME              TO PS08-OD-TIME
512700                                        PS08-CALC-TIME
504000     PERFORM P6400-LINK-PS08-PGM                                  48182999
504100                                                                  48183099
504900     IF NOT SUCCESS                                               48183899
505000        MOVE 'P6200-1'                  TO ERR-PARAGRAPH          48183999
505100        MOVE 'PS08LINK'                 TO ERR-KEY                48184099
505200        PERFORM P9999-GOT-PROBLEM                                 48184199
505300     END-IF                                                       48184299
505400     IF NOT PS08-NO-ERRORS                                        48184399
505500        MOVE 'P6200-2'                  TO ERR-PARAGRAPH          48184499
505600        MOVE PS08-RETURN-ERRORS         TO ERR-KEY                48184599
505700        MOVE 'CHECK S08 INPUT PARAMETERS'                         48184699
505800                                     TO ERR-SENTENCE              48184799
505900        PERFORM P9999-GOT-PROBLEM                                 48184899
506000     ELSE                                                         48184999
506200        IF AT-HOME-TERM                                           48185199
506300           AND NOT WS-EMP-STATUS-RED                              48185299
506400           IF PS08-TOTAL-TIME-ON-DUTY >= 6000                     48185399
506500              SET WS-EMP-STATUS-RED TO TRUE                       48185499
506600           ELSE                                                   48185599
506700              IF PS08-TOTAL-TIME-ON-DUTY > 5400                   48185699
506800                 SET WS-EMP-STATUS-RED TO TRUE                    48185799
506900              ELSE                                                48185899
507000                 IF PS08-TOTAL-TIME-ON-DUTY > 4800                48185999
507100                    SET WS-EMP-STATUS-YELLOW TO TRUE              48186099
507200                 END-IF                                           48186199
507300              END-IF                                              48186299
507400           END-IF                                                 48187099
507500**** * CHECK FOR 28 DAYS TTOD                                     48197099
507600           IF NOT WS-EMP-STATUS-RED                               48198099
507700              PERFORM P6300-GET-28D-TOTAL-OD                      48199199
507800              IF PS08-TOTAL-TIME-ON-DUTY >= 19200                 48199299
507900                 SET WS-EMP-STATUS-RED TO TRUE                    48199399
508000              ELSE                                                48199799
508100                 IF PS08-TOTAL-TIME-ON-DUTY > 18600               48199899
508200                    SET WS-EMP-STATUS-RED TO TRUE                 48199999
508300                                                                  48200099
508400                 ELSE                                             48203099
508500                    IF PS08-TOTAL-TIME-ON-DUTY > 18000            48204099
508600                       AND NOT WS-EMP-STATUS-RED                  48205099
508700                       SET WS-EMP-STATUS-YELLOW TO TRUE           48206099
508800                                                                  48207099
508900                    END-IF                                        48208099
509000                 END-IF                                           48209099
509100              END-IF                                              48209199
509200           END-IF                                                 48219399
509300        ELSE                                                      48219499
509400           IF NOT-AT-HOME-TERM                                    48219599
509500              AND NOT WS-EMP-STATUS-RED                           48219699
509600              IF PS08-TOTAL-TIME-ON-DUTY >= 6000                  48219799
509700                 SET WS-EMP-STATUS-YELLOW TO TRUE                 48219899
509800              END-IF                                              48226099
509900              IF NOT WS-EMP-STATUS-RED                            48227099
510000                 PERFORM P6300-GET-28D-TOTAL-OD                   48228099
510100                 IF PS08-TOTAL-TIME-ON-DUTY >= 19200              48229099
510200                    SET WS-EMP-STATUS-YELLOW TO TRUE              48229199
510300                 END-IF                                           48229999
510400              END-IF                                              48230099
510500           END-IF                                                 48231099
510600        END-IF                                                    48232099
510700     END-IF                                                       48232199
510800     .                                                            48240099
510900*                                                                 48241099
511000*=================================================================48250099
511100 P6300-GET-28D-TOTAL-OD.                                          48260099
511200*=================================================================48270099
511300                                                                  48280099
511400     INITIALIZE PS08-COMMAREA-PARMS                               48290099
511500     SET PS08-INQUIRY-FUN            TO TRUE                      48300099
511600     MOVE EMP-NBR OF WS-MSTR            TO PS08-EMP-NBR           48310099
511700*    MOVE PARM-RES-GREG-CENT         TO PS08-CALC-CE              48320099
511800*    MOVE PARM-RES-DATE-GREG         TO PS08-CALC-DATE            48330099
511900*    MOVE PARM-RES-HRMN              TO PS08-CALC-TIME            48340099
512000     MOVE 28                         TO PS08-CALC-NBR-DAYS        48350099
512100     SET DE-YYMMDD-FORMAT            TO TRUE                      48360099
512200     MOVE WS-LOCAL-DATE              TO DE-YYMMDD                 48370099
512300     PERFORM P8998-DATEEDIT                                       48380099
512400     MOVE DE-CCYYMMDD                TO PS08-OD-DATE-CENT         48390099
512500                                        PS08-CALC-DATE-CENT       48400099
512600     MOVE WS-LOCAL-TIME              TO PS08-OD-TIME              48410099
512700                                        PS08-CALC-TIME            48420099
512800     PERFORM P6400-LINK-PS08-PGM                                  48430099
513600     IF NOT SUCCESS                                               48510099
513700        MOVE 'P6300-1'               TO ERR-PARAGRAPH             48520099
513800        MOVE 'PS08LINK'              TO ERR-KEY                   48530099
513900        PERFORM P9999-GOT-PROBLEM                                 48540099
514000     END-IF                                                       48550099
514100     IF NOT PS08-NO-ERRORS                                        48560099
514200        MOVE 'P6300-2'               TO ERR-PARAGRAPH             48570099
514300        MOVE PS08-RETURN-ERRORS      TO ERR-KEY                   48580099
514400        MOVE 'CHECK S08 INPUT PARAMETERS'                         48590099
514500                                     TO ERR-SENTENCE              48600099
514600        PERFORM P9999-GOT-PROBLEM                                 48610099
514700     END-IF                                                       48620099
514800     .                                                            48630099
514900 P6400-LINK-PS08-PGM.                                             48630199
515000     EXEC CICS LINK                                               48630299
515100               PROGRAM(PS08-PGM)                                  48630399
515200               COMMAREA(PS08-COMMAREA-PARMS)                      48630499
515300               LENGTH(PS08-LGTH)                                  48630599
515400               RESP(WS-RESPONSE)                                  48630699
515500     END-EXEC                                                     48630799
515600     MOVE WS-RESPONSE                TO FILE-STATUS               48630899
515700     .                                                            48630999
515900*CNC0600-E                                                        49200099
383400******************************************************************36990071
383500 P7000-WRITE-TSQUEUE.                                             37000071
383600******************************************************************37010071
383700*                                                                 37020071
383800*      WRITE MAP TSQUEUE                                          37030071
383900*                                                                 37040071
384000     EXEC CICS ASSIGN                                             37050071
384100          EXTDS(WS-CICS-EXTDS-CODE)                               37060071
384200     END-EXEC                                                     37070071
384300*                                                                 37080071
384400     IF SCREEN-HAS-EXT-ATTR                                       37090071
384500        EXEC CICS SEND STRFIELD                                   37100071
384600                  FROM(WS-STRFIELD)                               37110071
384700                  LENGTH(WS-STRFIELD-LGTH)                        37120071
384800                  RESP(WS-RESPONSE)                               37130071
384900        END-EXEC                                                  37140071
385000        MOVE WS-RESPONSE           TO FILE-STATUS                 37150071
385100        IF NOT SUCCESS                                            37160071
385200           MOVE 'P7000-1'          TO ERR-PARAGRAPH               37170071
385300           MOVE 'SEND STRFIELD'    TO ERR-KEY                     37180071
385400           PERFORM P9999-GOT-PROBLEM                              37190071
385500        END-IF                                                    37200071
385600     END-IF                                                       37210071
385700*                                                                 37220071
385800     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH              37230071
385900     EXEC CICS RECEIVE BUFFER                                     37240071
386000               INTO(WS-BUFFER-DATA)                               37250071
386100               LENGTH(WS-BUFFER-LGTH)                             37260071
386200               RESP(WS-RESPONSE)                                  37270071
386300     END-EXEC                                                     37280071
386400     MOVE WS-RESPONSE              TO FILE-STATUS                 37290071
386500     IF NOT SUCCESS AND NOT EOC                                   37300071
386600        MOVE 'P7000-2'             TO ERR-PARAGRAPH               37310071
386700        MOVE 'RECEIVE BUFFER'      TO ERR-KEY                     37320071
386800        PERFORM P9999-GOT-PROBLEM                                 37330071
386900     END-IF                                                       37340071
387000     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR            37350071
387100                                                                  37360071
387200                                                                  37370071
387300     MOVE LENGTH OF WS-BUFFER-AREA TO P02ETSQ2-QLGTH              37380071
387400     MOVE EIBTRMID                 TO P02ETSQ-MAP-TERM-ID         37390071
387500     EXEC CICS WRITEQ TS                                          37400071
387600               QUEUE(P02ETSQ-MAP-QUEUE-ID)                        37410071
387700               FROM(WS-BUFFER-AREA)                               37420071
387800               LENGTH(P02ETSQ2-QLGTH)                             37430071
387900               RESP(WS-RESPONSE)                                  37440071
388000     END-EXEC                                                     37450071
388100     MOVE WS-RESPONSE              TO FILE-STATUS                 37460071
388200     IF NOT SUCCESS                                               37470071
388300        MOVE 'P7000-3'             TO ERR-PARAGRAPH               37480071
388400        PERFORM P9999-GOT-PROBLEM                                 37490071
388500     END-IF                                                       37500071
388600     MOVE EIBTRMID TO P02ETSQ-CA-TERM-ID                          37510071
388700     EXEC CICS WRITEQ TS                                          37520071
388800               QUEUE(P02ETSQ-CA-QUEUE-ID)                         37530071
388900               FROM(PSTCOMM-AREA)                                 37540071
389000               LENGTH(P02E-COMM-LGTH)                             37550071
389100               ITEM(P02ETSQ2-QUEUE-ITEM)                          37560071
389200               RESP(WS-RESPONSE)                                  37570071
389300     END-EXEC                                                     37580071
389400     MOVE WS-RESPONSE              TO FILE-STATUS                 37590071
389500     IF NOT SUCCESS                                               37600071
389600        MOVE 'P7000-4'             TO ERR-PARAGRAPH               37610071
389700        PERFORM P9999-GOT-PROBLEM                                 37620071
389800     END-IF.                                                      37630071
389900******************************************************************37640071
390000 P7010-READ-TSQUEUE.                                              37650071
390100******************************************************************37660071
390200*              READ THE MAPS TSQUEUE                              37670071
390300*                                                                 37680071
390400     MOVE LENGTH OF WS-BUFFER-AREA TO P02ETSQ2-QLGTH              37690071
390500     MOVE EIBTRMID                 TO P02ETSQ-MAP-TERM-ID         37700071
390600     EXEC CICS READQ TS                                           37710071
390700               QUEUE(P02ETSQ-MAP-QUEUE-ID)                        37720071
390800               INTO(WS-BUFFER-AREA)                               37730071
390900               LENGTH(P02ETSQ2-QLGTH)                             37740071
391000               ITEM(P02ETSQ2-QUEUE-ITEM)                          37750071
391100               RESP(WS-RESPONSE)                                  37760071
391200     END-EXEC                                                     37770071
391300     MOVE WS-RESPONSE              TO FILE-STATUS                 37780071
391400     IF SUCCESS                                                   37790071
391500        SET SEND-BUFFER            TO TRUE                        37800071
391600     ELSE                                                         37810071
391700        SET CREATE-SCREEN          TO TRUE                        37820071
391800        MOVE LOW-VALUES            TO PSTS02E                     37830071
391900     END-IF                                                       37840071
392000     MOVE EIBTRMID TO P02ETSQ-CA-TERM-ID                          37850071
392100     EXEC CICS READQ TS                                           37860071
392200               QUEUE(P02ETSQ-CA-QUEUE-ID)                         37870071
392300               INTO(PSTCOMM-AREA)                                 37880071
392400               LENGTH(P02E-COMM-LGTH)                             37890071
392500               ITEM(P02ETSQ2-QUEUE-ITEM)                          37900071
392600               RESP(WS-RESPONSE)                                  37910071
392700     END-EXEC                                                     37920071
392800     MOVE WS-RESPONSE TO FILE-STATUS                              37930071
392900     IF NOT SUCCESS                                               37940071
393000        MOVE SPACES TO PSTCOMM-AREA                               37950071
393100     END-IF                                                       37960071
393200     PERFORM P7020-DELETE-TSQUEUE.                                37970071
393300******************************************************************37980071
393400 P7020-DELETE-TSQUEUE.                                            37990071
393500******************************************************************38000071
393600     MOVE EIBTRMID TO P02ETSQ-MAP-TERM-ID                         38010071
393700     EXEC CICS DELETEQ TS                                         38020071
393800               QUEUE(P02ETSQ-MAP-QUEUE-ID)                        38030071
393900               RESP(WS-RESPONSE)                                  38040071
394000     END-EXEC                                                     38050071
394100     MOVE EIBTRMID TO P02ETSQ-CA-TERM-ID                          38060071
394200     EXEC CICS DELETEQ TS                                         38070071
394300               QUEUE(P02ETSQ-CA-QUEUE-ID)                         38080071
394400               RESP(WS-RESPONSE)                                  38090071
394500     END-EXEC.                                                    38100071
394600*                                                                 38110071
394700 P8000-READ-CNTLFILE.                                             38120071
394800*                                                                 38130071
394900     EXEC CICS READ                                               38140071
395000               DATASET(CNTL-FILE-VIA-CNTLKEY)                     38150071
395100               INTO(WS-CNTL-FILE)                                 38160071
395200               LENGTH(CNTLFILE-RLGTH)                             38170071
395300               RIDFLD(CNTLKEY)                                    38180071
395400               KEYLENGTH(CNTLFILE-KLGTH)                          38190071
395500               RESP(WS-RESPONSE)                                  38200071
395600     END-EXEC                                                     38210071
395700     MOVE WS-RESPONSE               TO FILE-STATUS.               38220071
395800*                                                                 38230071
395900 P8010-STARTBR-SCHED.                                             38240071
396000*                                                                 38250071
396100     EXEC CICS STARTBR                                            38260071
396200               DATASET(SJ-VIA-SCHEDKEY1)                          38270071
396300               RIDFLD(SCHEDKEY1)                                  38280071
396400               GTEQ                                               38290071
396500               RESP(WS-RESPONSE)                                  38300071
396600     END-EXEC                                                     38310071
396700     MOVE WS-RESPONSE            TO FILE-STATUS                   38320071
396800     IF NOT (SUCCESS OR NO-RECORD-FND)                            38330071
396900        MOVE 'P8010-1'           TO ERR-PARAGRAPH                 38340071
397000        MOVE SCHEDKEY1           TO ERR-KEY                       38350071
397100        PERFORM P9999-GOT-PROBLEM                                 38360071
397200     END-IF.                                                      38370071
397300*                                                                 38380071
397400 P8020-READNEXT-SCHED.                                            38390071
397500*                                                                 38400071
397600     EXEC CICS READNEXT                                           38410071
397700               DATASET(SJ-VIA-SCHEDKEY1)                          38420071
397800               INTO(WS-SCHED)                                     38430071
397900               LENGTH(SCHEDKEY1-RLGTH)                            38440071
398000               RIDFLD(SCHEDKEY1)                                  38450071
398100               KEYLENGTH(SCHEDKEY1-KLGTH)                         38460071
398200               RESP(WS-RESPONSE)                                  38470071
398300     END-EXEC                                                     38480071
398400     MOVE WS-RESPONSE            TO FILE-STATUS                   38490071
398500     IF NOT (SUCCESS OR NO-RECORD-FND OR END-OF-FILE)             38500071
398600        MOVE 'P8020-1'           TO ERR-PARAGRAPH                 38510071
398700        MOVE SCHEDKEY1           TO ERR-KEY                       38520071
398800        PERFORM P9999-GOT-PROBLEM                                 38530071
398900     END-IF.                                                      38540071
399000*                                                                 38550071
399100 P8025-READ-SCHED.                                                38560071
399200*                                                                 38570071
399300     EXEC CICS READ                                               38580071
399400               DATASET(SJ-VIA-SCHEDKEY1)                          38590071
399500               INTO(WS-SCHED)                                     38600071
399600               LENGTH(SCHEDKEY1-RLGTH)                            38610071
399700               RIDFLD(SCHEDKEY1)                                  38620071
399800               KEYLENGTH(SCHEDKEY1-KLGTH)                         38630071
399900               RESP(WS-RESPONSE)                                  38640071
400000     END-EXEC                                                     38650071
400100     MOVE WS-RESPONSE            TO FILE-STATUS                   38660071
400200     IF NOT SUCCESS                                               38670071
400300        MOVE 'P8025-1'           TO ERR-PARAGRAPH                 38680071
400400        MOVE SCHEDKEY1           TO ERR-KEY                       38690071
400500        PERFORM P9999-GOT-PROBLEM                                 38700071
400600     END-IF.                                                      38710071
400700*                                                                 38720071
400800 P8030-ENDBR-SCHED.                                               38730071
400900*                                                                 38740071
401000     EXEC CICS ENDBR                                              38750071
401100               DATASET(SJ-VIA-SCHEDKEY1)                          38760071
401200               RESP(WS-RESPONSE)                                  38770071
401300     END-EXEC                                                     38780071
401400     MOVE WS-RESPONSE            TO FILE-STATUS.                  38790071
401500***************************************************************** 38800071
401600 P8300-START-TASK-FILE.                                           38810071
401700***************************************************************** 38820071
401800     MOVE TASK-EMPLOYEE-KEY TO TASKEMPK                           38830071
401900     EXEC CICS STARTBR                                            38840071
402000               DATASET(TASK-VIA-EMP-NBR)                          38850071
402100               RIDFLD(TASKEMPK)                                   38860071
402200               GTEQ                                               38870071
402300               RESP(WS-RESPONSE)                                  38880071
402400     END-EXEC                                                     38890071
402500     MOVE WS-RESPONSE TO FILE-STATUS                              38900071
402600     IF NOT SUCCESS                                               38910071
402700        IF NO-RECORD-FND OR END-OF-FILE                           38920071
402800           CONTINUE                                               38930071
402900        ELSE                                                      38940071
403000           MOVE 'P8300-1' TO ERR-PARAGRAPH                        38950071
403100           MOVE TASKEMPK TO ERR-KEY                               38960071
403200           PERFORM P9999-GOT-PROBLEM                              38970071
403300        END-IF                                                    38980071
403400     END-IF                                                       38990071
403500     .                                                            39000071
403600                                                                  39010071
403700***************************************************************** 39020071
403800 P8310-READNEXT-TASK-FILE.                                        39030071
403900***************************************************************** 39040071
404000     EXEC CICS READNEXT                                           39050071
404100               DATASET(TASK-VIA-EMP-NBR)                          39060071
404200               INTO(WS-TASK)                                      39070071
404300               LENGTH(TASKENBR-RLGTH)                             39080071
404400               RIDFLD(TASKEMPK)                                   39090071
404500               KEYLENGTH(TASKENBR-KLGTH)                          39100071
404600               RESP(WS-RESPONSE)                                  39110071
404700     END-EXEC                                                     39120071
404800     MOVE WS-RESPONSE TO FILE-STATUS                              39130071
404900     IF NOT SUCCESS                                               39140071
405000        IF NO-RECORD-FND OR END-OF-FILE                           39150071
405100           CONTINUE                                               39160071
405200        ELSE                                                      39170071
405300           MOVE 'P8310-1' TO ERR-PARAGRAPH                        39180071
405400           MOVE TASKEMPK TO ERR-KEY                               39190071
405500           PERFORM P9999-GOT-PROBLEM                              39200071
405600        END-IF                                                    39210071
405700     END-IF                                                       39220071
405800     .                                                            39230071
405900                                                                  39240071
406000***************************************************************** 39250071
406100 P8320-ENDBR-TASK-FILE.                                           39260071
406200***************************************************************** 39270071
406300     EXEC CICS ENDBR                                              39280071
406400               DATASET(TASK-VIA-EMP-NBR)                          39290071
406500               RESP(WS-RESPONSE)                                  39300071
406600     END-EXEC                                                     39310071
406700     MOVE WS-RESPONSE TO FILE-STATUS                              39320071
406800     IF NOT SUCCESS                                               39330071
406900        MOVE 'P8320-1'  TO ERR-PARAGRAPH                          39340071
407000        MOVE TASKEMPK   TO ERR-KEY                                39350071
407100        PERFORM P9999-GOT-PROBLEM                                 39360071
407200     END-IF                                                       39370071
407300     .                                                            39380071
407400*=================================================================39390071
407500 P8400-LOAD-CALL-ORDER-ARRAY.                                     39400071
407600*=================================================================39410071
407700*CNC0564A - BEG                                                   39420071
407800     IF WS-3A-BIDPK-TIEBRK-PW-BRD                                 39430071
407900        ADD 1                    TO S1                            39440071
408000        IF S1 <= COT-ARRAY-MAX                                    39450071
408100           MOVE CRAFT-ARRAY-SUB  TO S2                            39460071
408200*C1104 - BEG                                                      39470089
408300***  DO NOT ADD TO ARRAY IF THE SAME TURN IS CURRENTLY SCHEDULED  39480089
408400***  TO PROTECT AT GIVEN TIME.                                    39490089
408500           IF  DISPLAY-TURN-RD-RED                                39500089
408600           AND S1 > 1                                             39510089
408700              SET DUP-TURN-NOT-DONE TO TRUE                       39520089
408800*                                                                 39530089
408900*          'S6' INDEXES THE PREVIOUS ARRAY ENTRY THAT WE'RE       39540089
409000*               COMPARING AGAINST.                                39550089
409100*                                                                 39560089
409200              MOVE S1            TO S6                            39570089
409300              PERFORM UNTIL DUP-TURN-DONE                         39580089
409400                         OR S6 = 1                                39590089
409500                 SUBTRACT 1    FROM S6                            39600089
409600                 IF (SCHED1-ASSIGNMENT =                          39610089
409700                     WS-COT-SCHEDKEY1(S2, S6)(5:8))               39620089
409800                 AND WS-TURN-PROTECTED(S2, S6)                    39630089
409900                    SUBTRACT 1 FROM S1                            39640089
410000                    SET DONT-DISPLAY-TURN TO TRUE                 39650089
410100                    SET DUP-TURN-DONE     TO TRUE                 39660089
410200                 END-IF                                           39670089
410300              END-PERFORM                                         39680089
410400           END-IF                                                 39690089
410500*                                                                 39700089
410600           IF DISPLAY-TURN                                        39710089
410700           OR DISPLAY-TURN-RD-RED                                 39720089
410800*C1104 - END                                                      39730089
410900              MOVE SCHED1-START-DATE-TIME                         39740089
411000                                 TO WS-COT-START-DATE-TIME(S2, S1)39750071
411100              MOVE SCHED1-CALL-ORDER TO WS-COT-CALL-ORDER(S2, S1) 39760089
411200                                                                  39770071
411300              MOVE SCHED-END-DATE-TIME TO WS-PREV-SCHED-END-DTTM  39780089
411400                                      WS-COT-END-DATE-TIME(S2, S1)39790071
411500              MOVE SCHED1-ASSIGNMENT TO WS-PREV-SCHED-ASGN        39800089
411600                                          WS-COT-ASGN(S2, S1)     39810089
411700              SET DE-YYMMDD-FORMAT  TO TRUE                       39820089
411800              SET DE-REFORMAT-ONLY  TO TRUE                       39830089
411900              MOVE UFP-POS-DATE-TIME(1:6) TO DE-YYMMDD            39840089
412000              PERFORM P8998-DATEEDIT                              39850089
412100              IF DE-INVALID-DATE                                  39860089
412200                 MOVE 'P8400-1'     TO ERR-PARAGRAPH              39870089
412300                 MOVE UFP-POS-DATE-TIME TO ERR-KEY                39880089
412400                 MOVE SCHED-KEY1    TO ERR-SENTENCE               39890089
412500                 PERFORM P9999-GOT-PROBLEM                        39900089
412600              END-IF                                              39910089
412700              MOVE DE-YYMMDD-CE  TO WS-COT-BOARD-CENT(S2, S1)     39920089
412800              MOVE UFP-POS-DATE-TIME                              39930089
412900                                 TO WS-COT-BOARD-DATE-TIME(S2, S1)39940089
413000              MOVE UFP-POS-TIE-BREAKER TO WS-COT-BOARD-TIE(S2, S1)39950089
413100              MOVE SCHED-KEY1    TO WS-COT-SCHEDKEY1(S2, S1)      39960089
413200              IF DISPLAY-TURN-RD-RED                              39970089
413300                 SET WS-TURN-PROTECTED-NO(S2, S1) TO TRUE         39980089
413400              END-IF                                              39990089
413500*                                                                 40000071
413600*       SORT THE ARRAY ENTRY WE JUST CREATED WITH EXISTING ENTRIES40010071
413700*       BASED ON BOARD DATE-TIME.                                 40020089
413800*                                                                 40030071
413900              IF S1 > 1                                           40040089
414000                 SET SORT-NOT-DONE TO TRUE                        40050089
414100*                                                                 40060071
414200*               'S4' INDEXES THE NEW ARRAY ENTRY WE'RE SORTING IN.40070089
414300*               'S3' INDEXES THE PREVIOUS ARRAY ENTRY THAT WE'RE  40080089
414400*                    COMPARING AGAINST.                           40090089
414500*                                                                 40100071
414600                 MOVE S1         TO S3                            40110089
414700                                    S4                            40120089
414800                 PERFORM UNTIL SORT-DONE                          40130089
414900                            OR S3 = 1                             40140089
415000                    SUBTRACT 1 FROM S3                            40150089
415100                    IF WS-COT-BOARD-DATE-TIME-TIE(S2, S4) <       40160089
415200                       WS-COT-BOARD-DATE-TIME-TIE(S2, S3)         40170089
415300                       MOVE WS-CO-TURN-ARRAY(S2, S3)              40180089
415400                                       TO WS-CALL-ORDER-SAVE-AREA 40190089
415500                       MOVE WS-CO-TURN-ARRAY(S2, S4)              40200089
415600                                       TO WS-CO-TURN-ARRAY(S2, S3)40210089
415700                       MOVE WS-CALL-ORDER-SAVE-AREA               40220089
415800                                       TO WS-CO-TURN-ARRAY(S2, S4)40230089
415900                       SUBTRACT 1 FROM S4                         40240089
416000                    ELSE                                          40250089
416100                       SET SORT-DONE TO TRUE                      40260089
416200                    END-IF                                        40270089
416300                 END-PERFORM                                      40280089
416400              END-IF                                              40290089
416500*C1104 - BEG                                                      40300089
416600           END-IF                                                 40310089
416700*C1104 - END                                                      40320089
416800        END-IF                                                    40330071
416900     ELSE                                                         40340071
417000        ADD 1                    TO S1                            40350071
417100        IF S1 <= COT-ARRAY-MAX                                    40360071
417200           MOVE CRAFT-ARRAY-SUB  TO S2                            40370071
417300           MOVE SCHED1-START-DATE-TIME                            40380071
417400                                 TO WS-COT-START-DATE-TIME(S2, S1)40390071
417500           MOVE SCHED1-CALL-ORDER TO WS-COT-CALL-ORDER(S2, S1)    40400071
417600                                                                  40410071
417700*CNC0510    - SAVE START DTTM AND END DTTM FOR LATER USE          40420071
417800           MOVE SCHED-END-DATE-TIME TO WS-PREV-SCHED-END-DTTM     40430071
417900                                      WS-COT-END-DATE-TIME(S2, S1)40440071
418000           MOVE SCHED1-ASSIGNMENT TO WS-PREV-SCHED-ASGN           40450071
418100                                       WS-COT-ASGN(S2, S1)        40460071
418200*CNC0510-END                                                      40470071
418300                                                                  40480071
418400           SET DE-YYMMDD-FORMAT     TO TRUE                       40490071
418500           SET DE-REFORMAT-ONLY     TO TRUE                       40500071
418600           MOVE UFP-POS-DATE-TIME(1:6) TO DE-YYMMDD               40510071
418700           PERFORM P8998-DATEEDIT                                 40520071
418800           IF DE-INVALID-DATE                                     40530071
418900              MOVE 'P8400-1'        TO ERR-PARAGRAPH              40540071
419000              MOVE UFP-POS-DATE-TIME TO ERR-KEY                   40550071
419100              MOVE SCHED-KEY1       TO ERR-SENTENCE               40560071
419200              PERFORM P9999-GOT-PROBLEM                           40570071
419300           END-IF                                                 40580071
419400           MOVE DE-YYMMDD-CE     TO WS-COT-BOARD-CENT(S2, S1)     40590071
419500                                                                  40600071
419600          MOVE UFP-POS-DATE-TIME TO WS-COT-BOARD-DATE-TIME(S2, S1)40610071
419700           MOVE UFP-POS-TIE-BREAKER TO WS-COT-BOARD-TIE(S2, S1)   40620071
419800           MOVE SCHED-KEY1       TO WS-COT-SCHEDKEY1(S2, S1)      40630071
419900*                                                                 40640071
420000*       SORT THE ARRAY ENTRY WE JUST CREATED WITH EXISTING ENTRIES40650071
420100*          BASED ON START DATE-TIME/CALL ORDER/BOARD DATE-TIME    40660071
420200*                                                                 40670071
420300           IF S1 > 1                                              40680071
420400              SET SORT-NOT-DONE  TO TRUE                          40690071
420500*                                                                 40700071
420600*             'S4' INDEXES THE NEW ARRAY ENTRY WE'RE SORTING IN.  40710071
420700*             'S3' INDEXES THE PREVIOUS ARRAY ENTRY THAT WE'RE    40720071
420800*                  COMPARING AGAINST.                             40730071
420900*                                                                 40740071
421000              MOVE S1            TO S3                            40750071
421100                                       S4                         40760071
421200              PERFORM UNTIL SORT-DONE                             40770071
421300                         OR S3 = 1                                40780071
421400                 SUBTRACT 1    FROM S3                            40790071
421500                 IF WS-COT-KEY(S2, S4) < WS-COT-KEY(S2, S3)       40800071
421600                    MOVE WS-CO-TURN-ARRAY(S2, S3)                 40810071
421700                                    TO WS-CALL-ORDER-SAVE-AREA    40820071
421800                    MOVE WS-CO-TURN-ARRAY(S2, S4)                 40830071
421900                                    TO WS-CO-TURN-ARRAY(S2, S3)   40840071
422000                    MOVE WS-CALL-ORDER-SAVE-AREA                  40850071
422100                                    TO WS-CO-TURN-ARRAY(S2, S4)   40860071
422200                    SUBTRACT 1 FROM S4                            40870071
422300                 ELSE                                             40880071
422400                    SET SORT-DONE TO TRUE                         40890071
422500                 END-IF                                           40900071
422600              END-PERFORM                                         40910071
422700           END-IF                                                 40920071
422800        END-IF                                                    40930071
422900     END-IF                                                       40940071
423000*CNC0564A - END                                                   40950071
423100     .                                                            40960071
423200*                                                                 40970071
423300 P8500-READ-MASTER.                                               40980071
423400*                                                                 40990071
423500     EXEC CICS READ                                               41000071
423600               DATASET(MSTR-VIA-EMP-NBR)                          41010071
423700               INTO(WS-MSTR)                                      41020071
423800               LENGTH(MSTRENBR-RLGTH)                             41030071
423900               RIDFLD(MSTRNBRK)                                   41040071
424000               KEYLENGTH(MSTRENBR-KLGTH)                          41050071
424100               RESP(WS-RESPONSE)                                  41060071
424200     END-EXEC                                                     41070071
424300     MOVE WS-RESPONSE TO FILE-STATUS                              41080071
424400     IF NOT SUCCESS                                               41090071
424500        MOVE 'P8500-1' TO ERR-PARAGRAPH                           41100071
424600        MOVE MSTRNBRK  TO ERR-KEY                                 41110071
424700        PERFORM P9999-GOT-PROBLEM                                 41120071
424800     END-IF                                                       41130071
424900     PERFORM P8510-READ-MASTER-JOBS                               41140071
425000     IF DIST       OF WS-MSTR NOT = PSTCA-DIST                    41150071
425100     OR SUB-DIST   OF WS-MSTR NOT = PSTCA-SUB-DIST                41160071
425200        MOVE SPACES              TO WORK-CNTLKEY                  41170071
425300        MOVE '02'                TO WK-CNTL-REC-TYPE              41180071
425400        MOVE DIST     OF WS-MSTR TO WK-CNTL-DIST                  41190071
425500        MOVE SUB-DIST OF WS-MSTR TO WK-CNTL-SUB-DIST              41200071
425600        MOVE WORK-CNTLKEY        TO CNTLKEY                       41210071
425700        EXEC CICS READ                                            41220071
425800                  DATASET(CNTL-FILE-VIA-CNTLKEY)                  41230071
425900                  INTO(WS-CNTL-FILE)                              41240071
426000                  LENGTH(CNTLFILE-RLGTH)                          41250071
426100                  RIDFLD(CNTLKEY)                                 41260071
426200                  KEYLENGTH(CNTLFILE-KLGTH)                       41270071
426300                  RESP(WS-RESPONSE)                               41280071
426400        END-EXEC                                                  41290071
426500        MOVE WS-RESPONSE TO FILE-STATUS                           41300071
426600        IF NOT SUCCESS                                            41310071
426700           MOVE 'P8500-2' TO ERR-PARAGRAPH                        41320071
426800           MOVE CNTLKEY   TO ERR-KEY                              41330071
426900           PERFORM P9999-GOT-PROBLEM                              41340071
427000        END-IF                                                    41350071
427100        IF EMP-MTOD       > '0000000000'                          41360071
427200            MOVE SPACES            TO TZ-PARAMETERS               41370071
427300            MOVE EMP-MTOD-NUM      TO TZ-IN-DATE-TIME             41380071
427400            MOVE CNTL-TIME-ZONE    TO TZ-IN-ZONE                  41390071
427500            MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                 41400071
427600            PERFORM P8996-TIMEZONE                                41410071
427700            MOVE TZ-OUT-DATE-TIME  TO EMP-MTOD                    41420071
427800        END-IF                                                    41430071
427900*CNLD-309 B  COMMENTING                                           41440071
427900*       IF EMP-MTOY       > '0000000000'                          41440071
428000*           MOVE SPACES            TO TZ-PARAMETERS               41450071
428100*           MOVE EMP-MTOY-NUM      TO TZ-IN-DATE-TIME             41460071
428200*           MOVE CNTL-TIME-ZONE    TO TZ-IN-ZONE                  41470071
428300*           MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                 41480071
428400*           PERFORM P8996-TIMEZONE                                41490071
428500*           MOVE TZ-OUT-DATE-TIME  TO EMP-MTOY                    41500071
428600*       END-IF                                                    41510071
428700*       IF EMP-MTOR       > '0000000000'                          41520071
428800*           MOVE SPACES            TO TZ-PARAMETERS               41530071
428900*           MOVE EMP-MTOR-NUM      TO TZ-IN-DATE-TIME             41540071
429000*           MOVE CNTL-TIME-ZONE    TO TZ-IN-ZONE                  41550071
429100*           MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                 41560071
429200*           PERFORM P8996-TIMEZONE                                41570071
429300*           MOVE TZ-OUT-DATE-TIME  TO EMP-MTOR                    41580071
429400*       END-IF                                                    41590071
427900*CNLD-309 E                                                       41440071
429500        IF EMP-US-RSTD    > '0000000000'                          41600071
429600            MOVE SPACES            TO TZ-PARAMETERS               41610071
429700            MOVE EMP-US-RSTD-NUM   TO TZ-IN-DATE-TIME             41620071
429800            MOVE CNTL-TIME-ZONE    TO TZ-IN-ZONE                  41630071
429900            MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                 41640071
430000            PERFORM P8996-TIMEZONE                                41650071
430100            MOVE TZ-OUT-DATE-TIME  TO EMP-US-RSTD                 41660071
430200        END-IF                                                    41670071
430300        IF EMP-PERS-REST  > '0000000000'                          41680071
430400            MOVE SPACES            TO TZ-PARAMETERS               41690071
430500            MOVE EMP-PERS-REST-NUM TO TZ-IN-DATE-TIME             41700071
430600            MOVE CNTL-TIME-ZONE    TO TZ-IN-ZONE                  41710071
430700            MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                 41720071
430800            PERFORM P8996-TIMEZONE                                41730071
430900            MOVE TZ-OUT-DATE-TIME  TO EMP-PERS-REST               41740071
431000        END-IF                                                    41750071
431100     END-IF.                                                      41760071
431200*                                                                 41770071
431300 P8510-READ-MASTER-JOBS.                                          41780071
431400*                                                                 41790071
431500     MOVE SPACES                 TO WS-ASGN-FILE                  41800071
431600                                    NORMAL-ASGNMT-FLAG            41810071
431700                                    NORMAL-ASGNMT                 41820071
431800                                    TEMPORARY-ASGNMT-FLAG         41830071
431900                                    TEMPORARY-ASGNMT              41840071
432000                                    ON-DUTY-ASGNMT-FLAG           41850071
432100                                    ON-DUTY-ASGNMT                41860071
432200     MOVE EMP-NBR OF WS-MSTR     TO ASGN-EMP-NO                   41870071
432300     PERFORM PXXXX-JOB-OWNED                                      41880071
432400     MOVE ASGN-JOB-TYPE          TO NORMAL-ASGNMT-FLAG            41890071
432500     MOVE ASGN-ASSIGNMENT        TO NORMAL-ASGNMT                 41900071
432600     MOVE SPACES                 TO WS-ASGN-FILE                  41910071
432700     MOVE EMP-NBR OF WS-MSTR     TO ASGN-EMP-NO                   41920071
432800     PERFORM PXXXX-LATEST-TEMP-JOB                                41930071
432900     MOVE ASGN-JOB-TYPE          TO TEMPORARY-ASGNMT-FLAG         41940071
433000     MOVE ASGN-ASSIGNMENT        TO TEMPORARY-ASGNMT              41950071
433100     MOVE SPACE                  TO WS-ASGN-FILE                  41960071
433200     MOVE EMP-NBR OF WS-MSTR     TO ASGN-EMP-NO                   41970071
433300     PERFORM PXXXX-JOB-ON-DUTY                                    41980071
433400     MOVE ASGN-JOB-TYPE          TO ON-DUTY-ASGNMT-FLAG           41990071
433500     MOVE ASGN-ASSIGNMENT        TO ON-DUTY-ASGNMT                42000071
433600     MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE.        42010071
433700*                                                                 42020071
433800*CNC0600-B                                                        42021095
433900*************************************************************     42022095
434000 P8550-READ-MSTR3-READ.                                           42023095
434100*************************************************************     42024095
434200*                                                                 42025095
434300     EXEC CICS READ                                               42026095
434400               DATASET(MSTR3-VIA-EMP-NBR)                         42027095
434500               INTO(WS-MSTR3)                                     42028095
434600               LENGTH(MSTR3ENBR-RLGTH)                            42029095
434700               RIDFLD(MSTR3NBRK)                                  42029195
434800               KEYLENGTH(MSTR3ENBR-KLGTH)                         42029295
434900               RESP(WS-RESPONSE)                                  42029395
435000     END-EXEC                                                     42029495
435100     MOVE WS-RESPONSE TO FILE-STATUS.                             42029595
435200*                                                                 42029695
435300*CNC0600-E                                                        42029795
435400 P8600-READ-UFPTURN.                                              42030071
435500*                                                                 42040071
435600     EXEC CICS READ                                               42050071
435700               DATASET(UFP-VIA-TURN-NBR)                          42060071
435800               INTO(WS-UFP)                                       42070071
435900               LENGTH(UFPTURN-RLGTH)                              42080071
436000               RIDFLD(UFPTURN)                                    42090071
436100               KEYLENGTH(UFPTURN-KLGTH)                           42100071
436200               RESP(WS-RESPONSE)                                  42110071
436300     END-EXEC                                                     42120071
436400     MOVE WS-RESPONSE TO FILE-STATUS.                             42130071
436500*                                                                 42140071
436600 P8700-READ-SCHEDKEY2.                                            42150071
436700*                                                                 42160071
436800     EXEC CICS READ                                               42170071
436900               DATASET(SJ-VIA-SCHEDKEY2)                          42180071
437000               INTO(WS-SCHED)                                     42190071
437100               LENGTH(SCHEDKEY2-RLGTH)                            42200071
437200               RIDFLD(SCHEDKEY2)                                  42210071
437300               KEYLENGTH(SCHEDKEY2-KLGTH)                         42220071
437400               RESP(WS-RESPONSE)                                  42230071
437500     END-EXEC                                                     42240071
437600     MOVE WS-RESPONSE TO FILE-STATUS                              42250071
437700     IF NOT (SUCCESS OR NO-RECORD-FND)                            42260071
437800        MOVE 'P8700-1'               TO ERR-PARAGRAPH             42270071
437900        MOVE SCHEDKEY2               TO ERR-KEY                   42280071
438000        PERFORM P9999-GOT-PROBLEM                                 42290071
438100     END-IF.                                                      42300071
438200*                                                                 42310071
438300 P8710-STARTBR-SCHEDKEY2.                                         42320071
438400*                                                                 42330071
438500     EXEC CICS STARTBR                                            42340071
438600               DATASET(SJ-VIA-SCHEDKEY2)                          42350071
438700               RIDFLD(SCHEDKEY2)                                  42360071
438800               GTEQ                                               42370071
438900               RESP(WS-RESPONSE)                                  42380071
439000     END-EXEC                                                     42390071
439100     MOVE WS-RESPONSE TO FILE-STATUS                              42400071
439200     IF NOT (SUCCESS OR NO-RECORD-FND OR END-OF-FILE)             42410071
439300        MOVE 'P8710-1'               TO ERR-PARAGRAPH             42420071
439400        MOVE SCHEDKEY2               TO ERR-KEY                   42430071
439500        PERFORM P9999-GOT-PROBLEM                                 42440071
439600     END-IF.                                                      42450071
439700*                                                                 42460071
439800 P8720-READNEXT-SCHEDKEY2.                                        42470071
439900*                                                                 42480071
440000     EXEC CICS READNEXT                                           42490071
440100               DATASET(SJ-VIA-SCHEDKEY2)                          42500071
440200               INTO(WS-SCHED)                                     42510071
440300               LENGTH(SCHEDKEY2-RLGTH)                            42520071
440400               RIDFLD(SCHEDKEY2)                                  42530071
440500               KEYLENGTH(SCHEDKEY2-KLGTH)                         42540071
440600               RESP(WS-RESPONSE)                                  42550071
440700     END-EXEC                                                     42560071
440800     MOVE WS-RESPONSE TO FILE-STATUS.                             42570071
440900     IF NOT (SUCCESS OR NO-RECORD-FND OR END-OF-FILE)             42580071
441000        MOVE 'P8720-1'               TO ERR-PARAGRAPH             42590071
441100        MOVE SCHEDKEY2               TO ERR-KEY                   42600071
441200        PERFORM P9999-GOT-PROBLEM                                 42610071
441300     END-IF.                                                      42620071
441400*                                                                 42630071
441500 P8730-ENDBR-SCHEDKEY2.                                           42640071
441600*                                                                 42650071
441700     EXEC CICS ENDBR                                              42660071
441800               DATASET(SJ-VIA-SCHEDKEY2)                          42670071
441900               RESP(WS-RESPONSE)                                  42680071
442000     END-EXEC                                                     42690071
442100     MOVE WS-RESPONSE TO FILE-STATUS.                             42700071
442200*                                                                 42710071
442300 PXXXX-GET-UFP-EMPS.                                              42720071
442400*                                                                 42730071
442500     MOVE ZEROES TO FIRST-EMP-NBR                                 42740071
442600                    TEMP-EMP-ONE                                  42750071
442700                    ON-DUTY-EMP                                   42760071
442800     MOVE SPACES TO WS-ASGN-FILE                                  42770071
442900     SET ASGN-UFP-JOB TO TRUE                                     42780071
443000     MOVE DIST2 TO ASGN-DIST                                      42790071
443100     MOVE SUB-DIST2 TO ASGN-SUB-DIST                              42800071
443200     MOVE POOL-NAME2 TO ASGN-UFP-POOL                             42810071
443300     MOVE TURN-NBR OF WS-UFP TO ASGN-UFP-TURN                     42820071
443400     MOVE POOL-CRAFT-CODE2 TO ASGN-UFP-CC                         42830071
443500     PERFORM PXXXX-JOB-OWNER                                      42840071
443600     IF ASGN-EMP-NO > ZERO                                        42850071
443700        MOVE ASGN-EMP-NO TO FIRST-EMP-NBR                         42860071
443800     END-IF                                                       42870071
443900     MOVE SPACES TO WS-ASGN-FILE                                  42880071
444000     SET ASGN-UFP-JOB TO TRUE                                     42890071
444100     MOVE DIST2 TO ASGN-DIST                                      42900071
444200     MOVE SUB-DIST2 TO ASGN-SUB-DIST                              42910071
444300     MOVE POOL-NAME2 TO ASGN-UFP-POOL                             42920071
444400     MOVE TURN-NBR OF WS-UFP TO ASGN-UFP-TURN                     42930071
444500     MOVE POOL-CRAFT-CODE2 TO ASGN-UFP-CC                         42940071
444600     PERFORM PXXXX-LATEST-TEMP                                    42950071
444700     IF ASGN-EMP-NO > ZERO                                        42960071
444800        MOVE ASGN-EMP-NO TO TEMP-EMP-ONE                          42970071
444900     END-IF                                                       42980071
445000     MOVE SPACES TO WS-ASGN-FILE                                  42990071
445100     SET ASGN-UFP-JOB TO TRUE                                     43000071
445200     MOVE DIST2 TO ASGN-DIST                                      43010071
445300     MOVE SUB-DIST2 TO ASGN-SUB-DIST                              43020071
445400     MOVE POOL-NAME2 TO ASGN-UFP-POOL                             43030071
445500     MOVE TURN-NBR OF WS-UFP TO ASGN-UFP-TURN                     43040071
445600     MOVE POOL-CRAFT-CODE2 TO ASGN-UFP-CC                         43050071
445700     PERFORM PXXXX-ON-DUTY-EMP                                    43060071
445800     IF ASGN-EMP-NO > ZERO                                        43070071
445900        MOVE ASGN-EMP-NO TO ON-DUTY-EMP                           43080071
446000     END-IF.                                                      43090071
446100*                                                                 43100071
446200 PXXXX-JOB-OWNER.                                                 43110071
446300*                                                                 43120071
446400     SET ASGN-OWNER-REC TO TRUE                                   43130071
446500     MOVE ZERO          TO ASGN-DATE-TIME                         43140071
446600     MOVE ASGNKEY1      TO ASGNJOB                                43150071
446700     EXEC CICS READ                                               43160071
446800               DATASET(ASGN-VIA-ASGNJOB)                          43170071
446900               INTO(ASGN-AREA)                                    43180071
447000               LENGTH(ASGNJOB-RLGTH)                              43190071
447100               RIDFLD(ASGNJOB)                                    43200071
447200               KEYLENGTH(ASGNJOB-KLGTH)                           43210071
447300               RESP(WS-RESPONSE)                                  43220071
447400     END-EXEC                                                     43230071
447500     MOVE WS-RESPONSE TO FILE-STATUS                              43240071
447600     IF NOT SUCCESS                                               43250071
447700        MOVE ZEROS TO ASGN-EMP-NO                                 43260071
447800     END-IF.                                                      43270071
447900*                                                                 43280071
448000 PXXXX-LATEST-TEMP.                                               43290071
448100*                                                                 43300071
448200     MOVE SPACES        TO WS-SAVE-ASGN-FILE                      43310071
448300     SET ASGN-TEMP-REC  TO TRUE                                   43320071
448400     MOVE ZERO          TO ASGN-DATE-TIME                         43330071
448500     MOVE ASGNKEY1      TO ASGNJOB                                43340071
448600     MOVE WS-ASGN-FILE  TO WS-SAVE-ASGN-FILE                      43350071
448700     EXEC CICS STARTBR                                            43360071
448800               DATASET(ASGN-VIA-ASGNJOB)                          43370071
448900               RIDFLD(ASGNJOB)                                    43380071
449000               GTEQ                                               43390071
449100               RESP(WS-RESPONSE)                                  43400071
449200     END-EXEC                                                     43410071
449300     MOVE WS-RESPONSE TO FILE-STATUS                              43420071
449400     IF SUCCESS                                                   43430071
449500        MOVE '0' TO ASGN-DONE-CODE                                43440071
449600        PERFORM UNTIL ASGN-DONE                                   43450071
449700           EXEC CICS READNEXT                                     43460071
449800                     DATASET(ASGN-VIA-ASGNJOB)                    43470071
449900                     INTO(ASGN-AREA)                              43480071
450000                     LENGTH(ASGNJOB-RLGTH)                        43490071
450100                     RIDFLD(ASGNJOB)                              43500071
450200                     KEYLENGTH(ASGNJOB-KLGTH)                     43510071
450300                     RESP(WS-RESPONSE)                            43520071
450400           END-EXEC                                               43530071
450500           MOVE WS-RESPONSE TO FILE-STATUS                        43540071
450600           IF SUCCESS                                             43550071
450700              IF WK-ASGN-DIST = ASGN-DIST                         43560071
450800                 AND WK-ASGN-SUB-DIST = ASGN-SUB-DIST             43570071
450900                 AND WK-ASGN-ASGN = ASGN-AJ-JOB OF ASGN-ASSIGNMENT43580071
451000                 AND ASGN-TEMP-REC                                43590071
451100                 MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE              43600071
451200              ELSE                                                43610071
451300                 SET ASGN-DONE TO TRUE                            43620071
451400              END-IF                                              43630071
451500           ELSE                                                   43640071
451600              SET ASGN-DONE TO TRUE                               43650071
451700           END-IF                                                 43660071
451800        END-PERFORM                                               43670071
451900        EXEC CICS ENDBR                                           43680071
452000                  DATASET(ASGN-VIA-ASGNJOB)                       43690071
452100                  RESP(WS-RESPONSE)                               43700071
452200        END-EXEC                                                  43710071
452300     END-IF                                                       43720071
452400     IF WS-SAVE-ASGN-FILE > SPACE                                 43730071
452500        MOVE WS-SAVE-ASGN-FILE TO ASGN-AREA                       43740071
452600     ELSE                                                         43750071
452700        MOVE ZEROS TO ASGN-EMP-NO                                 43760071
452800     END-IF.                                                      43770071
452900*                                                                 43780071
453000 PXXXX-ON-DUTY-EMP.                                               43790071
453100*                                                                 43800071
453200     SET ASGN-ON-DUTY-REC TO TRUE                                 43810071
453300     MOVE ZERO            TO ASGN-DATE-TIME                       43820071
453400     MOVE ASGNKEY1        TO ASGNJOB                              43830071
453500     EXEC CICS READ                                               43840071
453600               DATASET(ASGN-VIA-ASGNJOB)                          43850071
453700               INTO(ASGN-AREA)                                    43860071
453800               LENGTH(ASGNJOB-RLGTH)                              43870071
453900               RIDFLD(ASGNJOB)                                    43880071
454000               KEYLENGTH(ASGNJOB-KLGTH)                           43890071
454100               RESP(WS-RESPONSE)                                  43900071
454200     END-EXEC                                                     43910071
454300     MOVE WS-RESPONSE TO FILE-STATUS                              43920071
454400     IF NOT SUCCESS                                               43930071
454500        MOVE ZEROS TO ASGN-EMP-NO                                 43940071
454600     END-IF.                                                      43950071
454700*                                                                 43960071
454800 PXXXX-JOB-OWNED.                                                 43970071
454900*                                                                 43980071
455000     MOVE '1'      TO ASGN-EMP-NO-REC-TYPE                        43990071
455100     MOVE ZEROES   TO ASGN-EMP-DATE-TIME                          44000071
455200     MOVE ASGNKEY2 TO ASGNEMP                                     44010071
455300     EXEC CICS READ                                               44020071
455400               DATASET(ASGN-VIA-ASGNEMP)                          44030071
455500               INTO(WS-ASGN-FILE)                                 44040071
455600               LENGTH(ASGNEMP-RLGTH)                              44050071
455700               RIDFLD(ASGNEMP)                                    44060071
455800               KEYLENGTH(ASGNEMP-KLGTH)                           44070071
455900               RESP(WS-RESPONSE)                                  44080071
456000     END-EXEC                                                     44090071
456100     MOVE WS-RESPONSE TO FILE-STATUS                              44100071
456200     IF SUCCESS                                                   44110071
456300       CONTINUE                                                   44120071
456400     ELSE                                                         44130071
456500       IF NO-RECORD-FND OR END-OF-FILE                            44140071
456600         MOVE SPACE TO ASGN-ASSIGNMENT                            44150071
456700       ELSE                                                       44160071
456800         MOVE 'PXXXX-JO' TO ERR-PARAGRAPH                         44170071
456900         MOVE ASGNEMP TO ERR-KEY                                  44180071
457000         PERFORM P9999-GOT-PROBLEM                                44190071
457100       END-IF                                                     44200071
457200     END-IF.                                                      44210071
457300*                                                                 44220071
457400 PXXXX-LATEST-TEMP-JOB.                                           44230071
457500*                                                                 44240071
457600     MOVE SPACES        TO WS-SAVE-ASGN-FILE                      44250071
457700     MOVE '2'           TO ASGN-EMP-NO-REC-TYPE                   44260071
457800     MOVE ZERO          TO ASGN-EMP-DATE-TIME                     44270071
457900     MOVE ASGNKEY2      TO ASGNEMP                                44280071
458000     MOVE WS-ASGN-FILE  TO WS-SAVE-ASGN-FILE                      44290071
458100     EXEC CICS STARTBR                                            44300071
458200               DATASET(ASGN-VIA-ASGNEMP)                          44310071
458300               RIDFLD(ASGNEMP)                                    44320071
458400               GTEQ                                               44330071
458500               RESP(WS-RESPONSE)                                  44340071
458600     END-EXEC                                                     44350071
458700     MOVE WS-RESPONSE TO FILE-STATUS                              44360071
458800     IF SUCCESS                                                   44370071
458900        MOVE '0' TO ASGN-DONE-CODE                                44380071
459000        PERFORM UNTIL ASGN-DONE                                   44390071
459100           EXEC CICS READNEXT                                     44400071
459200                     DATASET(ASGN-VIA-ASGNEMP)                    44410071
459300                     INTO(ASGN-AREA)                              44420071
459400                     LENGTH(ASGNEMP-RLGTH)                        44430071
459500                     RIDFLD(ASGNEMP)                              44440071
459600                     KEYLENGTH(ASGNEMP-KLGTH)                     44450071
459700                     RESP(WS-RESPONSE)                            44460071
459800           END-EXEC                                               44470071
459900           MOVE WS-RESPONSE TO FILE-STATUS                        44480071
460000           IF SUCCESS                                             44490071
460100              IF ASGN-EMP-NO = WK-ASGN-EMP-NO                     44500071
460200                 AND ASGN-EMP-NO-REC-TYPE = '2'                   44510071
460300                 MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE              44520071
460400              ELSE                                                44530071
460500                 SET ASGN-DONE TO TRUE                            44540071
460600              END-IF                                              44550071
460700           ELSE                                                   44560071
460800              SET ASGN-DONE TO TRUE                               44570071
460900           END-IF                                                 44580071
461000        END-PERFORM                                               44590071
461100        EXEC CICS ENDBR                                           44600071
461200                  DATASET(ASGN-VIA-ASGNEMP)                       44610071
461300                  RESP(WS-RESPONSE)                               44620071
461400        END-EXEC                                                  44630071
461500     END-IF                                                       44640071
461600     IF WS-SAVE-ASGN-FILE > SPACES                                44650071
461700        MOVE WS-SAVE-ASGN-FILE TO WS-ASGN-FILE                    44660071
461800     ELSE                                                         44670071
461900        MOVE SPACES TO WS-ASGN-FILE                               44680071
462000     END-IF.                                                      44690071
462100*                                                                 44700071
462200 PXXXX-JOB-ON-DUTY.                                               44710071
462300*                                                                 44720071
462400     MOVE '3'      TO ASGN-EMP-NO-REC-TYPE                        44730071
462500     MOVE ZEROES   TO ASGN-EMP-DATE-TIME                          44740071
462600     MOVE ASGNKEY2 TO ASGNEMP                                     44750071
462700     EXEC CICS READ                                               44760071
462800               DATASET(ASGN-VIA-ASGNEMP)                          44770071
462900               INTO(ASGN-AREA)                                    44780071
463000               LENGTH(ASGNEMP-RLGTH)                              44790071
463100               RIDFLD(ASGNEMP)                                    44800071
463200               KEYLENGTH(ASGNEMP-KLGTH)                           44810071
463300               RESP(WS-RESPONSE)                                  44820071
463400     END-EXEC                                                     44830071
463500     MOVE WS-RESPONSE TO FILE-STATUS                              44840071
463600     IF SUCCESS                                                   44850071
463700       CONTINUE                                                   44860071
463800     ELSE                                                         44870071
463900       IF NO-RECORD-FND OR END-OF-FILE                            44880071
464000         MOVE SPACE TO ASGN-ASSIGNMENT                            44890071
464100       ELSE                                                       44900071
464200         MOVE 'PXXXX-JOD' TO ERR-PARAGRAPH                        44910071
464300         MOVE ASGNEMP     TO ERR-KEY                              44920071
464400         PERFORM P9999-GOT-PROBLEM                                44930071
464500       END-IF                                                     44940071
464600     END-IF.                                                      44950071
464700*                                                                 44960071
464800 COPY TIMEZONE.                                                   44970071
464900*                                                                 44980071
465000 COPY DATEEDIT.                                                   44990071
465100*                                                                 45000071
465200 COPY TIMEEDIT.                                                   45010071
465300*                                                                 45020071
465400 P9000-SEND-MAP-AND-RETURN.                                       45030071
465500*                                                                 45040071
465600     IF MSGLOG-CODE > SPACES                                      45050071
465700         PERFORM P9030-GET-MESSAGE                                45060071
465800         MOVE MSGLOG-MESSAGE-AREA TO SCR02E-ERRORMSG              45070071
465900     END-IF                                                       45080071
466000                                                                  45090071
466100     MOVE P02E-MAP-VERSION(PSTCA-SUB) TO P02E-MAP                 45100071
466200     IF CREATE-SCREEN                                             45110071
466300        PERFORM P9010-SEND-PHYSICAL-MAP                           45120071
466400     ELSE                                                         45130071
466500        IF CONTINUE-SCREEN                                        45140071
466600           PERFORM P9020-SEND-DATAONLY-MAP                        45150071
466700        ELSE                                                      45160071
466800           PERFORM P9035-SEND-BUFFER                              45170071
466900        END-IF                                                    45180071
467000     END-IF                                                       45190071
467100     EXEC CICS RETURN                                             45200071
467200               TRANSID(P02E-TRAN)                                 45210071
467300               COMMAREA(PSTCOMM-AREA)                             45220071
467400               LENGTH(P02E-COMM-LGTH)                             45230071
467500     END-EXEC.                                                    45240071
467600*                                                                 45250071
467700 P9010-SEND-PHYSICAL-MAP.                                         45260071
467800*                                                                 45270071
467900     EXEC CICS SEND MAP(P02E-MAP)                                 45280071
468000                    MAPSET(P02E-SET)                              45290071
468100                    FROM(PSTS02E)                                 45300071
468200                    CURSOR                                        45310071
468300                    ERASE                                         45320071
468400                    RESP(WS-RESPONSE)                             45330071
468500     END-EXEC                                                     45340071
468600     MOVE WS-RESPONSE TO FILE-STATUS                              45350071
468700     IF NOT SUCCESS                                               45360071
468800        MOVE 'P9010'   TO ERR-PARAGRAPH                           45370071
468900        PERFORM P9999-GOT-PROBLEM                                 45380071
469000     END-IF.                                                      45390071
469100*                                                                 45400071
469200 P9020-SEND-DATAONLY-MAP.                                         45410071
469300*                                                                 45420071
469400     EXEC CICS SEND MAP(P02E-MAP)                                 45430071
469500                    MAPSET(P02E-SET)                              45440071
469600                    FROM(PSTS02E)                                 45450071
469700                    DATAONLY                                      45460071
469800                    CURSOR                                        45470071
469900                    RESP(WS-RESPONSE)                             45480071
470000     END-EXEC                                                     45490071
470100     MOVE WS-RESPONSE TO FILE-STATUS                              45500071
470200     IF NOT SUCCESS                                               45510071
470300        MOVE 'P9020' TO ERR-PARAGRAPH                             45520071
470400        PERFORM P9999-GOT-PROBLEM                                 45530071
470500     END-IF.                                                      45540071
470600*                                                                 45550071
470700 P9030-GET-MESSAGE.                                               45560071
470800*                                                                 45570071
470900     MOVE PSTCA-SUB TO MSGLOG-SUB-CODE                            45580071
471000     EXEC CICS READ                                               45590071
471100               DATASET(MSGLOG-VIA-CODE)                           45600071
471200               INTO(MSGLOG-AREA)                                  45610071
471300               LENGTH(MSGLOG-RLGTH)                               45620071
471400               RIDFLD(MSGLOG-KEY)                                 45630071
471500               KEYLENGTH(MSGLOG-KLGTH)                            45640071
471600               RESP(WS-RESPONSE)                                  45650071
471700     END-EXEC                                                     45660071
471800     MOVE WS-RESPONSE TO FILE-STATUS                              45670071
471900     IF NOT SUCCESS                                               45680071
472000        IF PSTCA-SUB = 1                                          45690071
472100           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            45700071
472200        ELSE                                                      45710071
472300           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            45720071
472400        END-IF                                                    45730071
472500     END-IF                                                       45740071
472600     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE                      45750071
472700     MOVE '-'             TO MSGLOG-MSG-SEP                       45760071
472800     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.                 45770071
472900*                                                                 45780071
473000 P9035-SEND-BUFFER.                                               45790071
473100*                                                                 45800071
473200     EXEC CICS SEND                                               45810071
473300               FROM(WS-BUFFER-DATA)                               45820071
473400               LENGTH(WS-BUFFER-LGTH)                             45830071
473500               ERASE                                              45840071
473600               RESP(WS-RESPONSE)                                  45850071
473700     END-EXEC                                                     45860071
473800     MOVE WS-RESPONSE       TO FILE-STATUS                        45870071
473900     IF NOT SUCCESS                                               45880071
474000        MOVE 'P9035-1'      TO ERR-PARAGRAPH                      45890071
474100        MOVE 'SEND BUFFER'  TO ERR-KEY                            45900071
474200        PERFORM P9999-GOT-PROBLEM                                 45910071
474300     END-IF                                                       45920071
474400     EXEC CICS SEND                                               45930071
474500               CONTROL                                            45940071
474600               CURSOR(WS-BUFFER-CURSOR)                           45950071
474700               RESP(WS-RESPONSE)                                  45960071
474800     END-EXEC                                                     45970071
474900     MOVE WS-RESPONSE       TO FILE-STATUS                        45980071
475000     IF NOT SUCCESS                                               45990071
475100        MOVE 'P9035-2'      TO ERR-PARAGRAPH                      46000071
475200        MOVE 'SEND CURSOR'  TO ERR-KEY                            46010071
475300        PERFORM P9999-GOT-PROBLEM                                 46020071
475400     END-IF.                                                      46030071
475500*                                                                 46040071
475600 P9100-SETUP-SCR02.                                               46050071
475700*                                                                 46060071
475800     MOVE SPACES TO PSTCA-VARIABLE-AREA                           46070071
475900     EXEC CICS XCTL                                               46080071
476000               PROGRAM(P02-PGM)                                   46090071
476100               COMMAREA(PSTCOMM-AREA)                             46100071
476200               LENGTH(PSTCOMM-LGTH)                               46110071
476300               RESP(WS-RESPONSE)                                  46120071
476400     END-EXEC                                                     46130071
476500     MOVE WS-RESPONSE TO FILE-STATUS                              46140071
476600     IF NOT SUCCESS                                               46150071
476700         MOVE 'P9100' TO ERR-PARAGRAPH                            46160071
476800         PERFORM P9999-GOT-PROBLEM                                46170071
476900     END-IF.                                                      46180071
477000*                                                                 46190071
477100 P9200-SETUP-SCR27P.                                              46200078
477200*                                                                 46210076
477300     EXEC CICS XCTL                                               46220076
477400               PROGRAM(P27P-PGM)                                  46230078
477500               COMMAREA(PSTCOMM-AREA)                             46240076
477600               LENGTH(PSTCOMM-LGTH)                               46250076
477700               RESP(WS-RESPONSE)                                  46260076
477800     END-EXEC                                                     46270076
477900     MOVE WS-RESPONSE TO FILE-STATUS                              46280076
478000     IF NOT SUCCESS                                               46290076
478100         MOVE 'P9200' TO ERR-PARAGRAPH                            46300078
478200         PERFORM P9999-GOT-PROBLEM                                46310076
478300     END-IF.                                                      46320076
478400*                                                                 46330076
478500 P9400-DELETE-TSQUEUE.                                            46340071
478600*                                                                 46350071
478700     MOVE EIBTRMID TO P02ETSQ-TERM-ID                             46360071
478800     EXEC CICS DELETEQ TS                                         46370071
478900               QUEUE(P02ETSQ-QUEUE-ID)                            46380071
479000               RESP(WS-RESPONSE)                                  46390071
479100     END-EXEC                                                     46400071
479200     MOVE WS-RESPONSE           TO FILE-STATUS.                   46410071
479300*                                                                 46420071
479400 P9500-WRITE-TSQUEUE.                                             46430071
479500*                                                                 46440071
479600     MOVE EIBTRMID TO P02ETSQ-TERM-ID                             46450071
479700     EXEC CICS WRITEQ TS                                          46460071
479800               QUEUE(P02ETSQ-QUEUE-ID)                            46470071
479900               FROM(P02ETSQ-AREA)                                 46480071
480000               LENGTH(P02ETSQ-QLGTH)                              46490071
480100               ITEM(P02ETSQ-QUEUE-ITEM)                           46500071
480200               RESP(WS-RESPONSE)                                  46510071
480300     END-EXEC                                                     46520071
480400     MOVE WS-RESPONSE           TO FILE-STATUS                    46530071
480500     IF NOT SUCCESS                                               46540071
480600        MOVE 'P9500-1'          TO ERR-PARAGRAPH                  46550071
480700        MOVE P02ETSQ-QUEUE-ID   TO ERR-KEY                        46560071
480800        PERFORM P9999-GOT-PROBLEM                                 46570071
480900     END-IF.                                                      46580071
481000*                                                                 46590071
481100 P9600-REWRITE-TSQUEUE.                                           46600071
481200*                                                                 46610071
481300     MOVE EIBTRMID TO P02ETSQ-TERM-ID                             46620071
481400     EXEC CICS WRITEQ TS                                          46630071
481500               QUEUE(P02ETSQ-QUEUE-ID)                            46640071
481600               FROM(P02ETSQ-AREA)                                 46650071
481700               LENGTH(P02ETSQ-QLGTH)                              46660071
481800               ITEM(P02ETSQ-QUEUE-ITEM)                           46670071
481900               REWRITE                                            46680071
482000               RESP(WS-RESPONSE)                                  46690071
482100     END-EXEC                                                     46700071
482200     MOVE WS-RESPONSE           TO FILE-STATUS                    46710071
482300     IF NOT SUCCESS                                               46720071
482400        MOVE 'P9600-1'          TO ERR-PARAGRAPH                  46730071
482500        MOVE P02ETSQ-QUEUE-ID   TO ERR-KEY                        46740071
482600        PERFORM P9999-GOT-PROBLEM                                 46750071
482700     END-IF.                                                      46760071
482800*                                                                 46770071
482900 P9700-READ-TSQUEUE.                                              46780071
483000*                                                                 46790071
483100     MOVE EIBTRMID TO P02ETSQ-TERM-ID                             46800071
483200     EXEC CICS READQ TS                                           46810071
483300               QUEUE(P02ETSQ-QUEUE-ID)                            46820071
483400               INTO(P02ETSQ-AREA)                                 46830071
483500               LENGTH(P02ETSQ-QLGTH)                              46840071
483600               ITEM(P02ETSQ-QUEUE-ITEM)                           46850071
483700               RESP(WS-RESPONSE)                                  46860071
483800     END-EXEC                                                     46870071
483900     MOVE WS-RESPONSE           TO FILE-STATUS.                   46880071
484000*                                                                 46890071
484100 P9750-SETUP-SCR998.                                              46900071
484200*                                                                 46910071
484300     MOVE SPACES            TO P998COMM-AREA                      46920071
484400     MOVE P02E-PGM          TO P998CA-FROM-PROGRAM                46930071
484500     MOVE P02E-MAP          TO P998CA-SCREEN-ID                   46940071
484600     MOVE EIBCPOSN          TO P998CA-CURSOR-POS                  46950071
484700     EXEC CICS XCTL                                               46960071
484800               PROGRAM(P998-PGM)                                  46970071
484900               COMMAREA(PSTCOMM-AREA)                             46980071
485000               LENGTH(PSTCOMM-LGTH)                               46990071
485100               RESP(WS-RESPONSE)                                  47000071
485200     END-EXEC                                                     47010071
485300     MOVE WS-RESPONSE       TO FILE-STATUS                        47020071
485400     IF NOT SUCCESS                                               47030071
485500        MOVE 'P9750'        TO ERR-PARAGRAPH                      47040071
485600        PERFORM P9999-GOT-PROBLEM                                 47050071
485700     END-IF.                                                      47060071
485800*                                                                 47070071
485900 P9810-PROCESS-OFFSET.                                            47080071
486000*                                                                 47090071
486100     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE                 47100071
486200     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY            47110071
486300     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN                  47120071
486400     EXEC CICS LINK                                               47130071
486500               PROGRAM(P903-PGM)                                  47140071
486600               COMMAREA(DATE-CONVERSION-PARMS)                    47150071
486700               LENGTH(P903-LGTH)                                  47160071
486800               RESP(WS-RESPONSE)                                  47170071
486900     END-EXEC                                                     47180071
487000     MOVE WS-RESPONSE           TO FILE-STATUS                    47190071
487100     IF NOT SUCCESS                                               47200071
487200        MOVE 'P9810-1'          TO ERR-PARAGRAPH                  47210071
487300        MOVE 'P903'             TO ERR-KEY                        47220071
487400        PERFORM P9999-GOT-PROBLEM                                 47230071
487500     END-IF.                                                      47240071
487600*                                                                 47250071
487700 P9830-SNAPSHOT-UFP.                                              47260071
487800*                                                                 47270071
487900     MOVE SPACES                TO P915-COMMAREA-PARMS            47280071
488000     SET P915-SNAPSHOT-FUNCTION TO TRUE                           47290071
488100     MOVE CNTL-DIST             TO P915-TURN-DIST                 47300071
488200     MOVE CNTL-SUB-DIST         TO P915-TURN-SUB-DIST             47310071
488300     MOVE CNTL-POOL-CODE        TO P915-TURN-POOL                 47320071
488400     EXEC CICS LINK                                               47330071
488500               PROGRAM(P915-PGM)                                  47340071
488600               COMMAREA(P915-COMMAREA-PARMS)                      47350071
488700               LENGTH(P915-LGTH)                                  47360071
488800               RESP(WS-RESPONSE)                                  47370071
488900     END-EXEC                                                     47380071
489000     MOVE WS-RESPONSE           TO FILE-STATUS                    47390071
489100     IF NOT SUCCESS                                               47400071
489200        MOVE 'P9830-1'          TO ERR-PARAGRAPH                  47410071
489300        MOVE 'P915LINK'         TO ERR-KEY                        47420071
489400        PERFORM P9999-GOT-PROBLEM                                 47430071
489500     END-IF.                                                      47440071
489600*                                                                 47450071
489700 P9856-RETRIEVE-CNTL-INFO.                                        47460071
489800*                                                                 47470071
489900     MOVE SPACES                     TO P956-COMMAREA-PARMS       47480071
490000     MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE          47490071
490100     SET P956-GET-CNTL-STATUS-REASON TO TRUE                      47500071
490200     MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE          47510071
490300     MOVE DIST     OF WS-MSTR        TO P956-DIST                 47520071
490400     MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST                47530071
490500     MOVE CRAFT OF WS-MSTR           TO P956-CC                   47540071
490600     IF TEMPORARY-ASGNMT > SPACE                                  47550071
490700        MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE            47560071
490800        MOVE TA-1                    TO P956-ASGN                 47570071
490900        MOVE TA-DIST                 TO P956-DIST                 47580071
491000        MOVE TA-SUB-DIST             TO P956-SDIST                47590071
491100        IF TEMP-ASGN-XB                                           47600071
491200           MOVE TA-CC                TO P956-XB                   47610071
491300        END-IF                                                    47620071
491400     ELSE                                                         47630071
491500        IF NORMAL-ASGNMT > SPACES                                 47640071
491600           MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE            47650071
491700           MOVE NA-1                 TO P956-ASGN                 47660071
491800           MOVE NA-DIST              TO P956-DIST                 47670071
491900           MOVE NA-SUB-DIST          TO P956-SDIST                47680071
492000           IF NORM-ASGN-XB                                        47690071
492100              MOVE NA-CC             TO P956-XB                   47700071
492200           END-IF                                                 47710071
492300        END-IF                                                    47720071
492400     END-IF                                                       47730071
492500     IF  P956-ERROR-FOUND                                         47740071
492600         MOVE 'P956 LINK'            TO ERR-PARAGRAPH             47750071
492700         MOVE P956-INPUT-PARMS       TO ERR-KEY                   47760071
492800         PERFORM P9999-GOT-PROBLEM                                47770071
492900     END-IF                                                       47780071
493000     EXEC CICS LINK                                               47790071
493100               PROGRAM (P956-PGM)                                 47800071
493200               COMMAREA(P956-COMMAREA-PARMS)                      47810071
493300               LENGTH  (P956-LGTH)                                47820071
493400               RESP    (WS-RESPONSE)                              47830071
493500     END-EXEC                                                     47840071
493600     MOVE WS-RESPONSE           TO FILE-STATUS                    47850071
493700     IF NOT SUCCESS                                               47860071
493800        MOVE 'P9856-1'          TO ERR-PARAGRAPH                  47870071
493900        MOVE P956-INPUT-PARMS   TO ERR-KEY                        47880071
494000        PERFORM P9999-GOT-PROBLEM                                 47890071
494100     END-IF                                                       47900071
494200     .                                                            47910071
494300*                                                                 47920071
494400 P9990-CLEAR-SCREEN.                                              47930071
494500*                                                                 47940071
494600     EXEC CICS SEND CONTROL                                       47950071
494700                    ERASE                                         47960071
494800                    FREEKB                                        47970071
494900     END-EXEC                                                     47980071
495000     EXEC CICS RETURN END-EXEC.                                   47990071
495100*                                                                 48000071
495200 P9999-GOT-PROBLEM.                                               48010071
495300*                                                                 48020071
495400     MOVE P02E-PGM  TO ERR-PROGRAM                                48030071
495500     MOVE DFHEIBLK  TO ERR-EIBLK                                  48040071
495600     EXEC CICS XCTL                                               48050071
495700               PROGRAM(PSTERR-PGM)                                48060071
495800               COMMAREA(PSTERAR-AREA)                             48070071
495900               LENGTH(PSTERAR-LGTH)                               48080071
496000               RESP(WS-RESPONSE)                                  48090071
496100     END-EXEC                                                     48100071
496200     EXEC CICS ABEND                                              48110071
496300               ABCODE(PSTERR-ABCODE)                              48120071
496400               CANCEL                                             48130071
496500     END-EXEC.                                                    48140071
496600*                                                                 48150071
496700 X9999-GOBACK.                                                    48160071
496800     GOBACK.                                                      48170071
