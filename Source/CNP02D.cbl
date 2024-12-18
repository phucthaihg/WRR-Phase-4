000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.  CNP02D.                                             00020000
000300***************************************************************** 00030000
000400*           EXTRABOARD INQUIRY FOR FIELD PERSONNEL                00040000
000500***************************************************************** 00050000
000600*  DATE   INITIAL  LOG#   DESCRIPTION                             00060000
000700*-------- ------- ------  --------------------------------------- 00070000
000800*10/09/91   ERW           NEW PROGRAM.                            00080000
000900*04/27/95   PLS           CHANGES FOR NEW SPAREBOARD TYPE -       00090000
001000*                         FAST/SLOW.                              00100000
001100*04/18/96   FLW  BUG C282 DISPLAY EMPLOYEES ON TEMP. ASSIGNMENT   00110000
001200*                         OUTSIDE OF THEIR POSITIONAL SPAREBOARD  00120000
001300*05/03/96   FLW  CNC0006  DEFINITION OF WORK WEEK.                00130000
001400*08/07/96   RCW  CNC0021/22 DISPLAY RETURN DATE FROM VACATION OR  00140000
001500*                         OFF MILES.                              00150000
001600*09/18/96   RCW  CNC0006  USE P931-MASTER-INQUIRY ON 931 CALL     00160000
001700*10/09/96   RCW  CNC0077  NORTHERN QUEBEC SPAREBOARDS - PHASE 2   00170000
001800*03/30/98   AMF  CNC0155  ENHANCED TRACKING CHANGES.              00180000
001900*04/03/98   WLW  CNC0155  REVERSE ENHANCED TRACKING LOGIC         00190000
002000*04/03/98   AMF           ADDED HELP LOGIC.                       00200000
002100*05/06/98   LPS           Y2K MODIFICATIONS FOR THE TASK LIST.    00210000
002200*07/28/98   GBJ           YEAR 2000 SWEEP.                        00220000
002300*09/21/98   RDH   C442    ENLARGE SELECTION TO 3 DIGITS.          00230000
002400*11/19/98   BLD   C-451   IN P1650 WHEN DATE CONVERSION RETURNS   00240000
002500*                         SPACES IN CENTURY - MOVE HIGH VALUES.   00250000
002600*12/23/98   KMW           ALWAYS SEND DAY AND TIME TO CNP912.     00260000
002700*01/20/99                 YEAR 2000 CLEAN-UP                      00270000
002800*02/24/99   NJB           DISPLAY ALL ON BOARD 12/31/99 & 01/01/0000280000
002900*04/23/99   AJK  CNC0228  ADDED SNAPSHOT LOGIC.                   00290000
003000*05/14/99   SAM  CNC0216  STATUS/REASON CODE ENHANCEMNT.          00300000
003100*12/11/99   MOO  CNC0183  ADD CHECK FOR MTOR & MTOY.              00310000
003200*05/03/00   AJK  CNC0183  REMOVE CHECK FOR MTOR & MTOY.           00320000
003300*09/11/03   AJK  CNC0378  EXTENDED SPAREBOARD SCHEDULE ENH.       00330000
003400*07/20/05   VER  C595     PAGING ERROR. NEXT INCHAIN NOT 1ST ON   00340000
003500*                         NEXT PAGE. CHANGE FLAG = @C595          00350000
003600*04/12/06   MET  CNC0398  TIME OFF                                00360000
003700*02/06/08   AJK  C674     CHECK 'DISPLAY W/O TRACKING' FLAG TO    00370000
003800*                         DETERMINE IF AN EMPLOYEE IN AN OFF      00380000
003900*                         STATUS IS TO BE DISPLAYED ON A SLOW     00390000
004000*                         BOARD INQUIRY.                          00400000
004100*02/10/09   JES  CNC0436B RECOMPILE ONLY FOR WSTASK EXPANSION.    00410001
004200*03/11/09   JES           CHECK STATUS-REASON WHEN OFF-MILES      00420002
004300*                         TO DETERMINE RETURN DATE DISPLAY.       00430004
004400*04/27/10   AXK  CNC0492  ALWAYS CHECK FOR A DUEBACK/BOOK-ON TASK 00440007
004500*                         AND IF EXISTS, DISPLAY RETURN DATE/TIME.00450007
004600*                         PERFORMING P1501-GET-DUEBACK-DATE.      00460011
004700*                         THIS IS ONLY FOR OTHER LAYOFF CODES.    00470008
004800*07/16/10   MXS  CNC0492  DISPLAY UNAVAILABLE EMPLOYEE ON RESTDAY 00480019
004900*                         TO KEEP IN SYNC WITH CNP06 SCREEN.      00490013
005000*11/29/11   BXS  CNC0517  SPAREBOARD INQUIRY BY QUALIFICATION.    00500014
005100*04/11/13   AXD  C979     DISPLAY THE CORRECT RETURN DATE IN CASE 00510021
005200*                         OF MULTIPLE FUTURE BOOK-OFF TASKS.      00520021
005210*07/14/14   MFO  CNC0556  ADD REASON CODE                         00521022
005220*02/24/15   BLD  C1073    ADDED ANOTHER READNEXT ON TAP01 TO      00522027
005230*                         PREVENT LOOPING CONDITION WHEN TASK-TYPE00523026
005240*                         IS NOT TASK-LAYOFF-MARKUP.              00524027
005250*01/07/16   MFO  CNC0573  MASK STATUS/REASON FROM FIELD SCREENS   00525029
005260*02/23/16   MFO  CNC0576  MASK STATUS/REASON - HOLD TURN.         00526033
005300***************************************************************** 00530000
005400 ENVIRONMENT DIVISION.                                            00540000
005500 CONFIGURATION SECTION.                                           00550000
005600 SOURCE-COMPUTER.  IBM-9370.                                      00560000
005700 OBJECT-COMPUTER.  IBM-9370.                                      00570000
005800 DATA DIVISION.                                                   00580000
005900 WORKING-STORAGE SECTION.                                         00590000
006000 01  FILLER                      PIC X(10)  VALUE 'PGM02D W/S'.   00600000
006100 01  P02D-COMM-LGTH              PIC S9(4)  COMP   VALUE +1080.   00610000
006200                                                                  00620000
006300 01  WS-SUBSCRIPTS.                                               00630000
006400     02  NAME-SUB                PIC 99    VALUE ZERO.            00640000
006500     02  TURN-SUB                PIC 99    VALUE ZERO.            00650000
006600     02  SSUB                    PIC 99    VALUE ZERO.            00660000
006700     02  ARRAY-MAX               PIC 99    VALUE 17.              00670000
006800     02  SORT-ARRAY-MAX          PIC 99    VALUE 17.              00680000
006900     02  SAVE-SCR-POS            PIC 99    VALUE ZEROS.           00690000
007000     02  WS-TODAY                PIC S9    VALUE ZERO.            00700000
007100     02  WS-REST-SUB             PIC S9(4) COMP VALUE ZERO.       00710000
007200     02  SUB1                    PIC 9(02) VALUE ZERO.            00720000
007300     02  SEN-SUB                 PIC 9(2)  VALUE ZERO.            00730000
007400     02  SEN-MAX                 PIC 9(2)  VALUE 20.              00740000
007500     02  DUP-SUB                 PIC 9(3)  VALUE ZERO.            00750000
007600     02  DUP-MAX                 PIC 9(3)  VALUE 100.             00760000
007700     02  QUAL-SUB                PIC 99    VALUE ZERO.            00770015
007800                                                                  00780000
007900 01  WS-FLAGS.                                                    00790000
008000     02  WS-PROTECTION-FLAG      PIC X     VALUE '0'.             00800000
008100         88  PROTECTION-IS-IN-EFFECT       VALUE '1'.             00810000
008200     02  SCREEN-FLAG             PIC X     VALUE '0'.             00820000
008300         88  CONTINUE-SCREEN               VALUE '0'.             00830000
008400         88  CREATE-SCREEN                 VALUE '1'.             00840000
008500         88  SEND-BUFFER                   VALUE '2'.             00850000
008600     02  DONE-CODE               PIC 9     VALUE 0.               00860000
008700         88  DONE                          VALUE 1.               00870000
008800     02  GOT-EMPLOYEE-FLAG       PIC X     VALUE '0'.             00880000
008900         88  GOT-EMPLOYEE                  VALUE '1'.             00890000
009000     02  DISPLAY-EMP-FLAG        PIC X     VALUE '0'.             00900000
009100         88  DISPLAY-EMP                   VALUE '1'.             00910000
009200     02  WS-TASK-DONE-CODE       PIC X     VALUE 'N'.             00920000
009300         88  TASK-NOT-DONE                 VALUE 'N'.             00930000
009400         88  TASK-DONE                     VALUE 'Y'.             00940000
009500     02  WS-ASGN-DONE-CODE       PIC X.                           00950000
009600         88  ASGN-DONE                     VALUE 'Y'.             00960000
009700     02  WS-TEMPS-DONE-CODE      PIC X.                           00970000
009800         88  TEMPS-DONE                    VALUE 'Y'.             00980000
009900     02  DONE-WITH-SEN-CODE      PIC 9     VALUE ZERO.            00990000
010000         88  DONE-WITH-SEN                 VALUE 1.               01000000
010100     02  ON-DUTY-OUT-TOWN-CODE   PIC X(10) VALUE '9999999999'.    01010000
010200         88  OUT-TOWN                      VALUE '0000000000'.    01020000
010300     02  TEMP-ASGN-XB-AUG-FLAG   PIC X     VALUE SPACE.           01030000
010400         88  TEMP-ASGN-XB-AUG              VALUE 'Y'.             01040000
010500     02  DUP-EMP-FLAG            PIC 9     VALUE 0.               01050000
010600         88  NOT-DUP-EMP                   VALUE 0.               01060000
010700         88  DUP-EMP                       VALUE 1.               01070000
010800     02  SEN-ROSTER-DONE-CODE    PIC X     VALUE '0'.             01080000
010900         88  SEN-ROSTER-DONE               VALUE '1'.             01090000
011000*                                                                 01100000
011100*@C595 - ADD SCREEN PROCESSING FLAG                               01110000
011200*                                                                 01120000
011300     02  SCREEN-FULL-FLAG        PIC 9     VALUE 0.               01130000
011400         88  SCREEN-NOT-FULL               VALUE 0.               01140000
011500         88  SCREEN-FULL                   VALUE 1.               01150000
011600     02  WS-DUEBACK-FOUND-FLAG   PIC 9     VALUE 0.               01160000
011700         88  WS-DUEBACK-FOUND-N            VALUE 0.               01170000
011800         88  WS-DUEBACK-FOUND-Y            VALUE 1.               01180000
011900*CNC0517                                                          01190015
012000     02  WS-DAILY-MARK-FLAG      PIC X     VALUE '0'.             01200015
012100         88  DAILY-MARK-YARD               VALUE '1'.             01210015
012200     02  SEN-DONE-CODE           PIC 9     VALUE 0.               01220015
012300         88  SEN-DONE                      VALUE 1.               01230015
012400     02  QUAL-DONE-CODE          PIC 9     VALUE 0.               01240015
012500         88  QUAL-DONE                     VALUE 1.               01250015
012600*                                                                 01260015
012700                                                                  01270000
012800 01  WS-MISC.                                                     01280000
012900     02  WS-EMP-TO-REGAIN-PROTECTION  PIC X(9) VALUE SPACE.       01290000
013000     02  WS-EMP-TO-LOSE-PROTECTION    PIC X(9) VALUE SPACE.       01300000
013100     02  WS-HOLD-EMP-TO-BE-CUT        PIC X(9)  VALUE SPACE.      01310000
013200     02  AD-NBR                       PIC 99    VALUE ZERO.       01320000
013300     02  WS-EMP-ADDED-SEN             PIC X(8)  VALUE SPACE.      01330000
013400     02  WS-TURN-WITH-TAG             PIC X(10) VALUE SPACE.      01340000
013500     02  WS-RETURN-DATE               PIC X(4).                   01350000
013600     02  FILLER REDEFINES WS-RETURN-DATE.                         01360000
013700         05  WS-RETURN-DATE-MM        PIC 99.                     01370000
013800         05  WS-RETURN-DATE-DD        PIC 99.                     01380000
013900     02  WS-REST-DATE-TIME            PIC 9(10).                  01390000
014000     02  WS-REST-DATE-TIME-C REDEFINES                            01400000
014100         WS-REST-DATE-TIME            PIC X(10).                  01410000
014200     02  WS-VIEW-DATE-TIME-CENT.                                  01420000
014300         04  WS-VIEW-CENT             PIC X(02) VALUE SPACES.     01430000
014400         04  WS-VIEW-DATE-TIME.                                   01440000
014500             06  WS-VIEW-DATE         PIC X(06) VALUE SPACES.     01450000
014600             06  WS-VIEW-TIME         PIC X(04) VALUE SPACES.     01460000
014700             06  FILLER REDEFINES WS-VIEW-TIME.                   01470000
014800                 08  WS-VIEW-HOUR     PIC 99.                     01480000
014900                 08  WS-VIEW-MINUTE   PIC 99.                     01490000
015000     02  WS-POS-DATE-TIME-TZ-CENT.                                01500000
015100         04  WS-POS-CENT              PIC X(2) VALUE SPACES.      01510000
015200         04  WS-POS-DATE-TIME-TZ      PIC X(10) VALUE SPACES.     01520000
015300     02  WS-LINE-NUMBER-MOD           PIC S9V9 VALUE ZERO.        01530000
015400     02  FILLER REDEFINES WS-LINE-NUMBER-MOD.                     01540000
015500         04  FILLER                   PIC 9.                      01550000
015600         04  WS-MOD                   PIC SV9.                    01560000
015700     02  WS-SORT-DATE-TIME.                                       01570000
015800         04  WS-SORT-DATE             PIC X(06) VALUE SPACES.     01580000
015900         04  WS-SORT-TIME             PIC X(04) VALUE SPACES.     01590000
016000     02  WS-NUM                       PIC 9(01) VALUE ZEROES.     01600000
016100     02  HOLD-RES-DATE-GREG           PIC 9(6).                   01610000
016200*CNC0517                                                          01620015
016300     02  WS-POS-CENT-DATE-TIME-TZ     PIC X(12) VALUE SPACES.     01630015
016400     02  TEST-QUAL.                                               01640015
016500         04  FILLER              PIC X(3)  VALUE SPACES.          01650015
016600         04  FILLER              PIC X(1)  VALUE SPACE.           01660015
016700             88  STEP-RATE-QUAL            VALUE '%'.             01670015
016800*                                                                 01680015
016900 01  WS-TABLES.                                                   01690000
017000     02   WS-ASGN-SEN-AREA.                                       01700000
017100          03 WS-ASGN-SEN-ARRAY OCCURS 20.                         01710000
017200             05  WS-ASGN-SEN-RSTR PIC X(004).                     01720000
017300             05  WS-ASGN-SEN-CC   PIC X(002).                     01730000
017400             05  FILLER           PIC X(004).                     01740000
017500                                                                  01750000
017600*    CNC0006 - FLW, 5/8/96, START                                 01760000
017700 01  WS-VARIABLE-LINE-1-HDR.                                      01770000
017800     02  FILLER                  PIC X(3) VALUE 'POS'.            01780000
017900     02  FILLER                  PIC X    VALUE SPACE.            01790000
018000     02  FILLER                  PIC X(2) VALUE 'ST'.             01800000
018100     02  FILLER                  PIC X    VALUE SPACE.            01810000
018110     02  FILLER                  PIC X(2) VALUE 'RN'.             01811022
018120     02  FILLER                  PIC X    VALUE SPACE.            01812022
018200     02  FILLER                  PIC X(4) VALUE 'RETN'.           01820000
018300     02  FILLER                  PIC X(4) VALUE SPACE.            01830000
018600     02  FILLER                  PIC X(4) VALUE 'REST'.           01860000
018700     02  FILLER                  PIC X(3) VALUE SPACE.            01870000
018800     02  FILLER                  PIC X(4) VALUE 'MTPD'.           01880000
018900     02  FILLER                  PIC X    VALUE SPACE.            01890000
019000     02  FILLER                  PIC X(4) VALUE 'USHR'.           01900000
019100     02  FILLER                  PIC X    VALUE SPACE.            01910000
019200     02  FILLER                  PIC X(4) VALUE ' PD '.           01920000
019300     02  FILLER                  PIC X    VALUE SPACE.            01930000
019400     02  FILLER                  PIC X(3) VALUE 'STT'.            01940022
019500     02  FILLER                  PIC X(1) VALUE SPACE.            01950000
019600     02  FILLER                  PIC X(2) VALUE '#S'.             01960000
019700     02  FILLER                  PIC X(1) VALUE SPACE.            01970000
019800     02  FILLER                  PIC X(2) VALUE '#O'.             01980000
019900     02  FILLER                  PIC X(3) VALUE SPACE.            01990000
020000 01  WS-VARIABLE-LINE-1-HDR-FRENCH.                               02000000
020100     02  FILLER                  PIC X(3) VALUE 'POS'.            02010000
020200     02  FILLER                  PIC X    VALUE SPACE.            02020000
020300     02  FILLER                  PIC X(2) VALUE 'ST'.             02030000
020400     02  FILLER                  PIC X    VALUE SPACE.            02040000
020410     02  FILLER                  PIC X(2) VALUE 'RN'.             02041022
020420     02  FILLER                  PIC X    VALUE SPACE.            02042022
020500     02  FILLER                  PIC X(4) VALUE 'RETN'.           02050000
020600     02  FILLER                  PIC X(4) VALUE SPACE.            02060000
020900     02  FILLER                  PIC X(4) VALUE 'REST'.           02090000
021000     02  FILLER                  PIC X(3) VALUE SPACE.            02100000
021100     02  FILLER                  PIC X(4) VALUE 'REPO'.           02110000
021200     02  FILLER                  PIC X    VALUE SPACE.            02120000
021300     02  FILLER                  PIC X(4) VALUE 'HEEU'.           02130000
021400     02  FILLER                  PIC X    VALUE SPACE.            02140000
021500     02  FILLER                  PIC X(4) VALUE ' AA '.           02150000
021600     02  FILLER                  PIC X    VALUE SPACE.            02160000
021700     02  FILLER                  PIC X(3) VALUE 'RED'.            02170000
021800     02  FILLER                  PIC X(1) VALUE SPACE.            02180000
021900     02  FILLER                  PIC X(2) VALUE 'S#'.             02190000
022000     02  FILLER                  PIC X(1) VALUE SPACE.            02200000
022100     02  FILLER                  PIC X(2) VALUE 'O#'.             02210000
022200     02  FILLER                  PIC X(3) VALUE SPACE.            02220000
022300 01  WS-VARIABLE-LINE-2-HDR.                                      02230000
022400     02  FILLER                  PIC X(3) VALUE 'POS'.            02240000
022500     02  FILLER                  PIC X    VALUE SPACE.            02250000
022600     02  FILLER                  PIC X(2) VALUE 'ST'.             02260000
022700     02  FILLER                  PIC X    VALUE SPACE.            02270000
022800     02  FILLER                  PIC X(4) VALUE 'RETN'.           02280000
022900     02  FILLER                  PIC X(4) VALUE SPACE.            02290000
023000     02  FILLER                  PIC X(8) VALUE 'SENDATE'.        02300000
023100     02  FILLER                  PIC X    VALUE SPACE.            02310000
023200     02  FILLER                  PIC X(8) VALUE '  ASGN  '.       02320000
023300     02  FILLER                  PIC X    VALUE SPACE.            02330000
023400     02  WS-VL2-HDR-BOARD        PIC X(2) VALUE SPACE.            02340000
023500     02  FILLER                  PIC X(9) VALUE '   OFF   '.      02350000
023600     02  FILLER                  PIC X(8) VALUE ' AV TIME'.       02360000
023700 01  WS-VARIABLE-LINE-2-HDR-FRENCH.                               02370000
023800     02  FILLER                  PIC X(3) VALUE 'POS'.            02380000
023900     02  FILLER                  PIC X    VALUE SPACE.            02390000
024000     02  FILLER                  PIC X(2) VALUE 'ST'.             02400000
024100     02  FILLER                  PIC X    VALUE SPACE.            02410000
024200     02  FILLER                  PIC X(4) VALUE 'RETN'.           02420000
024300     02  FILLER                  PIC X(4) VALUE SPACE.            02430000
024400     02  FILLER                  PIC X(8) VALUE 'DATANCE'.        02440000
024500     02  FILLER                  PIC X    VALUE SPACE.            02450000
024600     02  FILLER                  PIC X(8) VALUE '  AFFT  '.       02460000
024700     02  FILLER                  PIC X    VALUE SPACE.            02470000
024800     02  WS-VL2-HDR-BOARD-F      PIC X(2) VALUE SPACE.            02480000
024900     02  FILLER                  PIC X(9) VALUE '   OFF   '.      02490000
025000     02  FILLER                  PIC X(8) VALUE ' AV TIME'.       02500000
025100*CNC0517- START                                                   02510015
025200 01  WS-VARIABLE-LINE-4-HDR.                                      02520015
025300     02  FILLER                  PIC X(3) VALUE 'POS'.            02530015
025400     02  FILLER                  PIC X    VALUE SPACE.            02540015
025500     02  FILLER                  PIC X(2) VALUE 'EN'.             02550015
025600     02  FILLER                  PIC X    VALUE SPACE.            02560015
025700     02  FILLER                  PIC X(2) VALUE 'CO'.             02570015
025800     02  FILLER                  PIC X    VALUE SPACE.            02580015
025900     02  FILLER                  PIC X(2) VALUE 'FO'.             02590015
026000     02  FILLER                  PIC X    VALUE SPACE.            02600015
026100     02  FILLER                  PIC X(2) VALUE 'SW'.             02610015
026200     02  FILLER                  PIC X    VALUE SPACE.            02620015
026300     02  FILLER                  PIC X(2) VALUE 'BK'.             02630015
026400     02  FILLER                  PIC X    VALUE SPACE.            02640015
026500     02  FILLER                  PIC X(2) VALUE 'YA'.             02650015
026600     02  FILLER                  PIC X(2) VALUE SPACES.           02660015
026700     02  FILLER                  PIC X(5) VALUE '-----'.          02670015
026800     02  FILLER                  PIC X(14) VALUE 'QUALIFICATIONS'.02680015
026900     02  FILLER                  PIC X(5) VALUE '-----'.          02690015
027000     02  FILLER                  PIC X(2) VALUE SPACES.           02700015
027100 01  WS-VARIABLE-LINE-4-HDR-FRENCH.                               02710015
027200     02  FILLER                  PIC X(3) VALUE 'POS'.            02720015
027300     02  FILLER                  PIC X    VALUE SPACE.            02730015
027400     02  FILLER                  PIC X(2) VALUE 'EN'.             02740015
027500     02  FILLER                  PIC X    VALUE SPACE.            02750015
027600     02  FILLER                  PIC X(2) VALUE 'CO'.             02760015
027700     02  FILLER                  PIC X    VALUE SPACE.            02770015
027800     02  FILLER                  PIC X(2) VALUE 'FO'.             02780015
027900     02  FILLER                  PIC X    VALUE SPACE.            02790015
028000     02  FILLER                  PIC X(2) VALUE 'SW'.             02800015
028100     02  FILLER                  PIC X    VALUE SPACE.            02810015
028200     02  FILLER                  PIC X(2) VALUE 'BK'.             02820015
028300     02  FILLER                  PIC X    VALUE SPACE.            02830015
028400     02  FILLER                  PIC X(2) VALUE 'YA'.             02840015
028500     02  FILLER                  PIC X(2) VALUE SPACES.           02850015
028600     02  FILLER                  PIC X(5) VALUE '-----'.          02860015
028700     02  FILLER                  PIC X(14) VALUE 'QUALIFICATIONS'.02870015
028800     02  FILLER                  PIC X(5) VALUE '-----'.          02880015
028900     02  FILLER                  PIC X(2) VALUE SPACES.           02890015
029000*CNC0517-FINISH                                                   02900015
029100 01  WS-VARIABLE-LINE-DETAIL     PIC X(52) VALUE SPACES.          02910000
029200 01  WS-VARIABLE-LINE-1 REDEFINES WS-VARIABLE-LINE-DETAIL.        02920000
029300     02  WS-VL1-POS              PIC X(3).                        02930000
029400     02  FILLER                  PIC X.                           02940000
029500     02  WS-VL1-ASSIGNMENT-AREA.                                  02950000
029600         04  WS-VL1-ASSIGNMENT   PIC X(08).                       02960000
029700***      04  FILLER              PIC X(03).                       02970023
029710         04  FILLER              PIC X(06).                       02971023
029800     02  FILLER REDEFINES WS-VL1-ASSIGNMENT-AREA.                 02980000
029900         04 WS-VL1-LO            PIC X(2).                        02990033
030000         04 FILLER               PIC X.                           03000033
030010         04 WS-VL1-RSN           PIC X(2).                        03001033
030030         04 FILLER               PIC X.                           03003022
030100         04 WS-VL1-RETURN-DATE   PIC X(4).                        03010000
030200         04 FILLER REDEFINES WS-VL1-RETURN-DATE.                  03020000
030300             06 WS-VL1-RETURN-DATE-MM PIC 99.                     03030000
030400             06 WS-VL1-RETURN-DATE-DD PIC 99.                     03040000
030500         04 FILLER               PIC X.                           03050000
030600         04 WS-VL1-RETURN-DY     PIC X(2).                        03060000
030700         04 FILLER               PIC X.                           03070000
030800***  02  WS-VL1-TAG              PIC X.                           03080022
030900***  02  FILLER                  PIC X.                           03090022
031000     02  WS-VL1-REST             PIC X(4).                        03100000
031100     02  FILLER                  PIC X.                           03110000
031200     02  WS-VL1-REST-DY          PIC X(2).                        03120000
031300     02  FILLER                  PIC X.                           03130000
031400     02  WS-VL1-MTPD             PIC X(4).                        03140000
031500     02  FILLER                  PIC X.                           03150000
031600     02  WS-VL1-USHR             PIC X(4).                        03160000
031700     02  FILLER                  PIC X.                           03170000
031800     02  WS-VL1-PD               PIC X(4).                        03180000
031900     02  FILLER                  PIC X(2).                        03190000
032000     02  WS-VL1-SHRT             PIC X(2).                        03200000
032100     02  FILLER                  PIC X.                           03210000
032200     02  WS-VL1-STT-STARTS       PIC X(2).                        03220000
032300     02  FILLER                  PIC X.                           03230000
032400     02  WS-VL1-OVT-STARTS       PIC XX.                          03240000
032500***  02  FILLER                  PIC X(3).                        03250023
032510     02  FILLER                  PIC X(2).                        03251023
032600 01  WS-VARIABLE-LINE-2 REDEFINES WS-VARIABLE-LINE-DETAIL.        03260000
032700     02  WS-VL2-POS              PIC X(3).                        03270000
032800     02  FILLER                  PIC X.                           03280000
032900     02  WS-VL2-LO               PIC X(2).                        03290000
033000     02  FILLER                  PIC X.                           03300000
033100     02  WS-VL2-RETURN-DATE          PIC X(4).                    03310000
033200     02  FILLER REDEFINES WS-VL2-RETURN-DATE.                     03320000
033300         05  WS-VL2-RETURN-DATE-MM   PIC 99.                      03330000
033400         05  WS-VL2-RETURN-DATE-DD   PIC 99.                      03340000
033500     02  FILLER                  PIC X.                           03350000
033600     02  WS-VL2-RETURN-DY        PIC X(2).                        03360000
033700     02  FILLER                  PIC X.                           03370000
033800     02  WS-VL2-SEN-LVL          PIC X(2).                        03380000
033900     02  WS-VL2-SEN-DATE         PIC X(6).                        03390000
034000     02  FILLER                  PIC X.                           03400000
034100     02  WS-VL2-ASGN             PIC X(8).                        03410000
034200     02  WS-VL2-BOARD            PIC X(2).                        03420000
034300     02  FILLER                  PIC X.                           03430000
034400     02  WS-VL2-REST-DAYS        PIC X(7).                        03440000
034500     02  FILLER REDEFINES WS-VL2-REST-DAYS.                       03450000
034600         04  WS-VL2-REST-DAY     PIC X OCCURS 7 TIMES.            03460000
034700     02  FILLER                  PIC X.                           03470000
034800     02  WS-VL2-AVAIL-START      PIC X(4).                        03480000
034900     02  WS-VL2-HYPHEN           PIC X.                           03490000
035000     02  WS-VL2-AVAIL-END        PIC X(4).                        03500000
035100*CNC0517  QUAL/SEN DISPLAY VARIABLE PORTION                       03510015
035200 01  WS-VARIABLE-LINE-4.                                          03520015
035300     02  WS-VL4-POS              PIC X(3).                        03530015
035400     02  FILLER                  PIC X.                           03540015
035500     02  WS-VL4-EN               PIC X.                           03550015
035600     02  FILLER                  PIC X(2).                        03560015
035700     02  WS-VL4-CO               PIC X.                           03570015
035800     02  FILLER                  PIC X(2).                        03580015
035900     02  WS-VL4-FO               PIC X.                           03590015
036000     02  FILLER                  PIC X(2).                        03600015
036100     02  WS-VL4-SW               PIC X.                           03610015
036200     02  FILLER                  PIC X(2).                        03620015
036300     02  WS-VL4-BK               PIC X.                           03630015
036400     02  FILLER                  PIC X(2).                        03640015
036500     02  WS-VL4-YA               PIC X.                           03650015
036600     02  FILLER                  PIC X(3).                        03660015
036700     02  WS-VL4-QUAL-ENT OCCURS 5 TIMES.                          03670015
036800         04  WS-VL4-QUAL         PIC X(4).                        03680015
036900         04  FILLER              PIC X.                           03690015
037000     02  FILLER                  PIC X.                           03700015
037100*    CNC0006 - FLW, 5/8/96, END                                   03710000
037200                                                                  03720000
037300*01  TIME-AREA.                                                   03730000
037400*    02  SYSTEM-DATE.                                             03740000
037500*        04  SYS-YR              PIC 99   VALUE ZERO.             03750000
037600*        04  SYS-MO              PIC 99   VALUE ZERO.             03760000
037700*        04  SYS-DY              PIC 99   VALUE ZERO.             03770000
037800*    02  SYSTEM-TIME.                                             03780000
037900*      04  SYSTEM-HR-MN.                                          03790000
038000*          05  SYS-HR            PIC 99   VALUE ZERO.             03800000
038100*          05  SYS-MN            PIC 99   VALUE ZERO.             03810000
038200*      04  SYS-SC                PIC 99   VALUE ZERO.             03820000
038300*      04  SYS-NS                PIC 99   VALUE ZERO.             03830000
038400*01  TIME-AREA-A REDEFINES TIME-AREA.                             03840000
038500*    02  PRESENT-TIME            PIC 9(10).                       03850000
038600*    02  FILLER                  PIC X(4).                        03860000
038700                                                                  03870000
038800*01  WS-LOCAL-DATE-TIME.                                          03880000
038900*    02  WS-LOCAL-DATE           PIC X(06) VALUE SPACE.           03890000
039000*    02  WS-LOCAL-TIME.                                           03900000
039100*        03  WS-LOCAL-HR         PIC X(02) VALUE SPACE.           03910000
039200*        03  WS-LOCAL-MN         PIC X(02) VALUE SPACE.           03920000
039300                                                                  03930000
039400 01  DAY1                              PIC 99   VALUE ZERO.       03940015
039500 01  XB-WORK-KEY.                                                 03950000
039600     02  XB-WORK-DIST                  PIC XX    VALUE SPACE.     03960000
039700     02  XB-WORK-SUB-DIST              PIC XX    VALUE SPACE.     03970000
039800     02  XB-WORK-CC                    PIC XX    VALUE SPACE.     03980000
039900     02  XB-WORK-NBR                   PIC X(4)  VALUE SPACE.     03990000
040000     02  FILLER                        PIC X(12) VALUE SPACES.    04000000
040100 01  XB-WORK-KEY-X REDEFINES XB-WORK-KEY.                         04010000
040200     02  XB-WORK-KEY1.                                            04020000
040300         04  FILLER                    PIC X(6).                  04030000
040400         04  XB-WORK-KEY1-POSITION.                               04040000
040500             05  XB-WK-KEY1-ON-OFF     PIC 9(1).                  04050000
040600             05  XB-WK-KEY1-BOARD-TIME.                           04060000
040700                 06  XB-WK-KEY1-BOARD  PIC 9(1).                  04070000
040800                 06  XB-WK-KEY1-TIME.                             04080000
040900                     08  XB-WK-KEY1-DATE-TIME  PIC X(10).         04090000
041000                     08  XB-WK-KEY1-TIE-CODE   PIC X(4).          04100000
041100 01  XB-WORK-KEY2  REDEFINES XB-WORK-KEY.                         04110000
041200     02  FILLER                        PIC X(6).                  04120000
041300     02  XB-WORK-KEY2-TURN             PIC X(4).                  04130000
041400     02  FILLER                        PIC X(12).                 04140000
041500                                                                  04150000
041600 01  WORK-ASGNKEY1.                                               04160000
041700     02  WK-ASGN-JOB-TYPE              PIC X    VALUE 'X'.        04170000
041800     02  WK-ASGN-DIST                  PIC XX   VALUE SPACE.      04180000
041900     02  WK-ASGN-SUB-DIST              PIC XX   VALUE SPACE.      04190000
042000     02  WK-SWASSGN-ASGN.                                         04200000
042100       04  FILLER                      PIC XX   VALUE 'EX'.       04210000
042200       04  WK-ASGN-XB-TURN             PIC X(4) VALUE SPACE.      04220000
042300       04  WK-ASGN-CC                  PIC XX   VALUE SPACE.      04230000
042400     02  WK-ASGN-REC-TYPE              PIC X    VALUE '1'.        04240000
042500     02  WK-ASGN-DATE-TIME             PIC 9(10) VALUE 0.         04250000
042600                                                                  04260000
042700 01  WORK-ASGNKEY2.                                               04270000
042800     02  WK-ASGN-EMP-NO                PIC 9(9) VALUE 0.          04280000
042900     02  WK-ASGN-EMP-REC-TYPE          PIC X    VALUE SPACE.      04290000
043000     02  WK-ASGN-EMP-DATE-TIME         PIC 9(10) VALUE 0.         04300000
043100                                                                  04310000
043200 01  WS-SAVE-ASGN-FILE                 PIC X(128).                04320000
043300                                                                  04330000
043400 01  NORMAL-ASGNMT-FLAG                PIC X     VALUE SPACE.     04340000
043500     88  NORM-ASGN-UFP                           VALUE 'U'.       04350000
043600     88  NORM-ASGN-XB                            VALUE 'X'.       04360000
043700     88  NORM-ASGN-AJ                            VALUE 'A'.       04370000
043800 01  NORMAL-ASGNMT.                                               04380000
043900     02  NA-DIST                       PIC XX   VALUE SPACE.      04390000
044000     02  NA-SUB-DIST                   PIC XX   VALUE SPACE.      04400000
044100     02  NA-AREA.                                                 04410000
044200       03  NA-1                        PIC X(6).                  04420000
044300       03  NA-2 REDEFINES NA-1.                                   04430000
044400         04  NA-POOL                   PIC XX.                    04440000
044500         04  NA-TURN                   PIC X(4).                  04450000
044600       03  NA-3 REDEFINES NA-1.                                   04460000
044700         04  NA-FILLER                 PIC XX.                    04470000
044800         04  NA-XB-TURN                PIC X(4).                  04480000
044900       03  NA-CC                       PIC XX.                    04490000
045000                                                                  04500000
045100 01  TEMPORARY-ASGNMT-FLAG             PIC X     VALUE SPACE.     04510000
045200     88  TEMP-ASGN-UFP                           VALUE 'U'.       04520000
045300     88  TEMP-ASGN-XB                            VALUE 'X'.       04530000
045400     88  TEMP-ASGN-AJ                            VALUE 'A'.       04540000
045500 01  TEMPORARY-ASGNMT.                                            04550000
045600     02  TA-DIST                        PIC XX   VALUE SPACE.     04560000
045700     02  TA-SUB-DIST                    PIC XX   VALUE SPACE.     04570000
045800     02  TA-AREA.                                                 04580000
045900       03  TA-1                         PIC X(6).                 04590000
046000       03  TA-2 REDEFINES TA-1.                                   04600000
046100         04  TA-POOL                    PIC XX.                   04610000
046200         04  TA-TURN                    PIC X(4).                 04620000
046300       03  TA-3 REDEFINES TA-1.                                   04630000
046400         04  TA-FILLER                  PIC XX.                   04640000
046500         04  TA-XB-TURN                 PIC X(4).                 04650000
046600       03  TA-CC                        PIC XX   VALUE SPACE.     04660000
046700                                                                  04670000
046800 01  ON-DUTY-ASGNMT-FLAG                PIC X    VALUE SPACE.     04680000
046900     88  ON-DUTY-UFP                             VALUE 'U'.       04690000
047000     88  ON-DUTY-AJ                              VALUE 'A'.       04700000
047100 01  ON-DUTY-ASGNMT.                                              04710000
047200     02  OD-DIST                        PIC XX   VALUE SPACE.     04720000
047300     02  OD-SUB-DIST                    PIC XX   VALUE SPACE.     04730000
047400     02  OD-AREA.                                                 04740000
047500       03  OD-1                         PIC X(6).                 04750000
047600       03  OD-2 REDEFINES OD-1.                                   04760000
047700         04  OD-POOL                    PIC XX.                   04770000
047800         04  OD-TURN                    PIC X(4).                 04780000
047900       03  OD-CC                        PIC XX   VALUE SPACE.     04790000
048000                                                                  04800000
048100 01  WS-FORMAT-NAME-AUG.                                          04810000
048200     02  FILLER                        PIC X(21).                 04820000
048300     02  WS-FORMAT-NAME-AUG-FIELD      PIC X(05).                 04830000
048400                                                                  04840000
048500 01  WS-FORMAT-NAME-PROT.                                         04850000
048600     02  FILLER                        PIC X(20).                 04860000
048700     02  WS-FORMAT-NAME-PROT-FIELD     PIC X(06).                 04870000
048800                                                                  04880000
048900 01  WORK-CNTLKEY.                                                04890000
049000     02  WK-CNTL-REC-TYPE              PIC XX    VALUE '08'.      04900000
049100     02  WK-CNTL-DIST                  PIC XX    VALUE SPACE.     04910000
049200     02  WK-CNTL-SUB-DIST              PIC XX    VALUE SPACE.     04920000
049300     02  WK-CNTL-XB                    PIC XX    VALUE SPACE.     04930000
049400     02  FILLER                        PIC X(12) VALUE SPACE.     04940000
049500                                                                  04950000
049600 01  WS-HOLD-SSUB                    PIC 99     VALUE ZERO.       04960000
049700 01  WS-HOLD-SORT                    PIC X(10)  VALUE HIGH-VALUES.04970000
049800 01  WS-SORT-ARRAY                   PIC X(969) VALUE HIGH-VALUES.04980004
049900 01  SORT-ARRAY REDEFINES WS-SORT-ARRAY.                          04990000
050000     02  SORT-AREA OCCURS 17 TIMES.                               05000000
050100         04  SORT-DATE-TIME.                                      05010000
050200             06  SORT-DATE             PIC 9(06).                 05020000
050300             06  SORT-TIME             PIC 9(04).                 05030000
050400         04  SORT-NAME                 PIC X(26).                 05040000
050500         04  SORT-TURN                 PIC X(04).                 05050000
050600         04  SORT-POSITION             PIC X(01).                 05060000
050700         04  SORT-ASSIGNMENT           PIC X(08).                 05070000
050800         04  SORT-STATUS               PIC X(02).                 05080000
050900         04  SORT-RETURN-DATE          PIC X(04).                 05090000
051000         04  SORT-RETURN-DY            PIC X(02).                 05100004
051100******************************************************************05110000
051200***                  TEMPORARY STORAGE QUEUE                   ***05120000
051300******************************************************************05130000
051400 01  P02DTSQ-QUEUE-ITEM         PIC S9(4)  COMP VALUE +1.         05140000
051500 01  P02DTSQ-MAP-QUEUE-ID.                                        05150000
051600     05  P02DTSQ-MAP-QUEUE      PIC X(4)   VALUE '02DM'.          05160000
051700     05  P02DTSQ-MAP-TERM-ID    PIC X(4)   VALUE SPACES.          05170000
051800 01  P02DTSQ-CA-QUEUE-ID.                                         05180000
051900     05  P02DTSQ-CA-QUEUE       PIC X(4)   VALUE '02DC'.          05190000
052000     05  P02DTSQ-CA-TERM-ID     PIC X(4)   VALUE SPACES.          05200000
052100 01  P02DTSQ-QLGTH              PIC S9(4)  COMP VALUE +1.         05210000
052200***************************************************************** 05220000
052300***                 I/O STATUS CHECK FIELDS                       05230000
052400***************************************************************** 05240000
052500 01  WS-RESPONSE             PIC S9(8) COMP VALUE ZEROES.         05250000
052600 01  FILE-STATUS             PIC 9(4)  VALUE ZEROES.              05260000
052700     COPY IOCODES.                                                05270000
052800***************************************************************** 05280000
052900***                    COMMAREA COPYBOOKS                         05290000
053000***************************************************************** 05300000
053100     COPY PSTCOMM.                                                05310000
053200     COPY FICOMM.                                                 05320000
053300     COPY P998COMM.                                               05330000
053400     05  P02D-COMM.                                               05340000
053500         10   P02D-LAST-SS            PIC 99.                     05350000
053600*CNC0517                                                          05360016
053700         10   P02DCA-HOLD-POS         PIC X(03).                  05370016
053800         10   P02DCA-HOLD-POS-NUM REDEFINES                       05380016
053900              P02DCA-HOLD-POS         PIC 9(03).                  05390016
054000*                                                                 05400016
054100         10   FILLER                  PIC X(05).                  05410016
054200     05  P02DCA-DUP-EMPLOYEE-TABLE.                               05420000
054300         10  P02DCA-DUP-EMPLOYEES OCCURS 100 TIMES.               05430000
054400             15  P02DCA-DUP-EMP-NBR  PIC X(9).                    05440000
054500     COPY P913COMM.                                               05450000
054600***************************************************************** 05460000
054700***                     MAP AREA COPYBOOK                         05470000
054800***************************************************************** 05480000
054900     COPY PSM02DRE.                                               05490000
055000***************************************************************** 05500000
055100***                   PROGRAM NAMES COPYBOOKS                     05510000
055200***************************************************************** 05520000
055300     COPY PSTCB02D.                                               05530000
055400     COPY PSTCB02.                                                05540000
055500     COPY PSTCB03.                                                05550000
055600     COPY PSTCB998.                                               05560000
055700***************************************************************** 05570000
055800***                 CALLED ROUTINES COPYBOOKS.                    05580000
055900***************************************************************** 05590000
056000     COPY PSTERAR.                                                05600000
056100     COPY P903COMM.                                               05610000
056200     COPY P907COMM.                                               05620000
056300     COPY P912COMM.                                               05630000
056400     COPY P931COMM.                                               05640000
056500     COPY P942COMM.                                               05650000
056600     COPY P956COMM.                                               05660000
056700     COPY PS42COMM.                                               05670000
056800***************************************************************** 05680000
056900***                     FILE COPYBOOKS                            05690000
057000***************************************************************** 05700000
057100     COPY WSMSTR.                                                 05710000
057200     COPY WSSEN.                                                  05720000
057300     COPY WSEB.                                                   05730000
057400     COPY WSCNTL.                                                 05740000
057500     COPY WSASGN.                                                 05750000
057600     COPY WSFICT.                                                 05760000
057700     COPY WSTASK.                                                 05770000
057800     COPY WSQUAL.                                                 05780015
057900***************************************************************** 05790000
058000***                     MISC. COPYBOOKS                           05800000
058100***************************************************************** 05810000
058200     COPY PSTKEYS.                                                05820000
058300     COPY PSTATTR.                                                05830000
058400     COPY WSDAYWK.                                                05840000
058500     COPY WSBIF.                                                  05850000
058600     COPY WSCENTER.                                               05860000
058700     COPY WSMSG.                                                  05870000
058800     COPY WSZONE.                                                 05880000
058900     COPY WSEDDATE.                                               05890000
059000     COPY WSSYDTTM.                                               05900000
059100     COPY WSBUFFER.                                               05910000
059200                                                                  05920000
059300 LINKAGE SECTION.                                                 05930000
059400 01  DFHCOMMAREA.                                                 05940000
059500     05  FILLER                PIC X(170).                        05950000
059600     05  LINK-P02D-AREA        PIC X(910).                        05960000
059700                                                                  05970000
059800 PROCEDURE DIVISION.                                              05980000
059900*                                                                 05990000
060000 P0000-MAINLINE.                                                  06000000
060100*                                                                 06010000
060200     EXEC CICS IGNORE                                             06020000
060300               CONDITION                                          06030000
060400               ERROR                                              06040000
060500     END-EXEC                                                     06050000
060600     EXEC CICS HANDLE                                             06060000
060700               ABEND                                              06070000
060800               LABEL(P9999-GOT-PROBLEM)                           06080000
060900     END-EXEC                                                     06090000
061000     COPY ABSTIME.                                                06100000
061100     IF EIBCALEN = ZERO                                           06110000
061200        PERFORM P9990-CLEAR-SCREEN                                06120000
061300     END-IF                                                       06130000
061400     MOVE DFHCOMMAREA      TO PSTCOMM-AREA                        06140000
061500     PERFORM P8800-GET-CURRENT-TIME                               06150000
061600     IF EIBTRNID = P998-TRAN                                      06160000
061700        SET CREATE-SCREEN TO TRUE                                 06170000
061800        MOVE LOW-VALUES TO PSTS02D                                06180000
061900        MOVE P998CA-CURSOR-POS TO EIBCPOSN                        06190000
062000        PERFORM P7010-READ-TSQUEUE                                06200000
062100        PERFORM P9000-SEND-MAP-AND-RETURN                         06210000
062200     END-IF                                                       06220000
062300*                                                                 06230000
062400     MOVE FICA-FICT-RECORD-KEY    TO FICTKEY                      06240000
062500     EXEC CICS READ                                               06250000
062600               DATASET(FICT-VIA-LOC-SEQ)                          06260000
062700               INTO(WS-FICT)                                      06270000
062800               LENGTH(FICTLOSQ-RLGTH)                             06280000
062900               RIDFLD(FICTKEY)                                    06290000
063000               KEYLENGTH(FICTLOSQ-KLGTH)                          06300000
063100               RESP(WS-RESPONSE)                                  06310000
063200     END-EXEC                                                     06320000
063300     MOVE WS-RESPONSE TO FILE-STATUS                              06330000
063400     IF NOT SUCCESS                                               06340000
063500        MOVE 'P0000-1'            TO ERR-PARAGRAPH                06350000
063600        MOVE FICTKEY              TO ERR-KEY                      06360000
063700        PERFORM P9999-GOT-PROBLEM                                 06370000
063800     END-IF                                                       06380000
063900     IF EIBTRNID NOT = P02D-TRAN                                  06390000
064000        SET CREATE-SCREEN   TO TRUE                               06400000
064100        MOVE LOW-VALUES     TO PSTS02D                            06410000
064200        MOVE FICT-DIST      TO SCR02D-DIST                        06420000
064300        MOVE FICT-SUB-DIST  TO SCR02D-SUB-DIST                    06430000
064400        MOVE 30             TO CTXT-UNF-FIELD-LEN                 06440000
064500        MOVE FICT-DESC(PSTCA-SUB) TO CTXT-UNF-FIELD               06450000
064600        MOVE ZEROES         TO P02D-LAST-SS                       06460000
064700        PERFORM P8994-CENTER-TEXT                                 06470000
064800        MOVE CTXT-FOR-FIELD TO SCR02D-DESC                        06480000
064900        MOVE ZEROES         TO FICA-HOLD-POS                      06490000
065000                               P02D-LAST-SS                       06500000
065100        PERFORM P1000-LIST-XB                                     06510000
065200        PERFORM P2000-GET-NBR-ASSIGNED                            06520000
065300        PERFORM P9000-SEND-MAP-AND-RETURN                         06530000
065400     ELSE                                                         06540000
065500        MOVE LINK-P02D-AREA        TO P02D-COMM                   06550000
065600     END-IF                                                       06560000
065700     MOVE EIBAID   TO PF-CHECK                                    06570000
065800     IF PFKEY11                                                   06580000
065900        SET PSTCA-FLD-RET-PFKEY11    TO TRUE                      06590000
066000     END-IF                                                       06600000
066100     IF EXIT-KEY OR PFKEY11                                       06610000
066200        PERFORM P9100-SETUP-SCR02                                 06620000
066300     END-IF                                                       06630000
066400     PERFORM P0100-PROCESS-INPUT                                  06640000
066500     PERFORM P1000-LIST-XB                                        06650000
066600     PERFORM P9000-SEND-MAP-AND-RETURN.                           06660000
066700*                                                                 06670000
066800 P0100-PROCESS-INPUT.                                             06680000
066900*                                                                 06690000
067000     MOVE P02D-MAP-VERSION(PSTCA-SUB) TO P02D-MAP                 06700000
067100     EXEC CICS RECEIVE MAP(P02D-MAP)                              06710000
067200                       MAPSET(P02D-SET)                           06720000
067300                       INTO(PSTS02D)                              06730000
067400                       RESP(WS-RESPONSE)                          06740000
067500     END-EXEC                                                     06750000
067600     MOVE WS-RESPONSE TO FILE-STATUS                              06760000
067700     IF NOT SUCCESS                                               06770000
067800        MOVE 'P0100-1' TO ERR-PARAGRAPH                           06780000
067900        PERFORM P9999-GOT-PROBLEM                                 06790000
068000     END-IF                                                       06800000
068100     IF PFKEY1                                                    06810000
068200        PERFORM P7000-WRITE-TSQUEUE                               06820000
068300        PERFORM P9500-SETUP-SCR998                                06830000
068400     END-IF                                                       06840000
068500     IF NOT ENTER-KEY AND NOT PFKEY8                              06850000
068600*            INVALID-FUNC-MSG                                     06860000
068700        MOVE 'I006' TO MSGLOG-CODE                                06870000
068800        PERFORM P9000-SEND-MAP-AND-RETURN                         06880000
068900     END-IF                                                       06890000
069000     PERFORM P0300-EDIT-VIEW-TIME                                 06900000
069100     MOVE SPACES TO SCR02D-ERRORMSG.                              06910000
069200                                                                  06920000
069300                                                                  06930000
069400*************************************************************     06940000
069500 P0300-EDIT-VIEW-TIME.                                            06950000
069600*   ADD VIEW TIME WHICH ALLOWS USERS TO HAVE A SNAPSHOT           06960000
069700*   OF AVAILABILITY OVER A 24 HOUR PERIOD.                        06970000
069800*                        NORTHERN QUEBEC SPAREBOARD - PHASE 2     06980000
069900*************************************************************     06990000
070000     MOVE DEFAULT-ATTR TO SCR02D-VIEW-TIME-HI                     07000000
070100*    MOVE WS-LOCAL-DATE-TIME TO WS-VIEW-DATE-TIME                 07010000
070200     MOVE WS-LOCAL-DATE-TIME-CENT TO WS-VIEW-DATE-TIME-CENT       07020000
070300     IF SCR02D-VIEW-TIME > SPACES                                 07030000
070400        MOVE SCR02D-VIEW-TIME TO BIF-FIELD                        07040000
070500        MOVE LENGTH OF SCR02D-VIEW-TIME TO BIF-LEN                07050000
070600        PERFORM P8999-BIFEDIT                                     07060000
070700        IF BIF-ERROR OR                                           07070000
070800           BIF-EDITED-FIELD = ZEROES                              07080000
070900**          'INVALID VIEW TIME'                                   07090000
071000           MOVE 'B059' TO MSGLOG-CODE                             07100000
071100           MOVE -1 TO SCR02D-VIEW-TIME-CURSOR                     07110000
071200           MOVE REV-VIDEO TO SCR02D-VIEW-TIME-HI                  07120000
071300           PERFORM P9000-SEND-MAP-AND-RETURN                      07130000
071400        END-IF                                                    07140000
071500        MOVE BIF-EDITED-FIELD TO WS-VIEW-TIME                     07150000
071600                                                                  07160000
071700        IF WS-VIEW-HOUR > 23 OR                                   07170000
071800           WS-VIEW-MINUTE > 59                                    07180000
071900**          'INVALID VIEW TIME'                                   07190000
072000           MOVE 'B059' TO MSGLOG-CODE                             07200000
072100           MOVE -1 TO SCR02D-VIEW-TIME-CURSOR                     07210000
072200           MOVE REV-VIDEO TO SCR02D-VIEW-TIME-HI                  07220000
072300           PERFORM P9000-SEND-MAP-AND-RETURN                      07230000
072400        END-IF                                                    07240000
072500     END-IF                                                       07250000
072600                                                                  07260000
072700     IF WS-VIEW-TIME < WS-LOCAL-TIME                              07270000
072800        MOVE ZEROS TO DATE-CONVERSION-PARMS                       07280000
072900        MOVE WS-VIEW-DATE TO PARM-PRI-DATE-GREG                   07290000
073000        SET PARM-ADD TO TRUE                                      07300000
073100        MOVE 1 TO PARM-SEC-GREG-DAY                               07310000
073200        PERFORM P8700-CALL-DATE-ROUTINE                           07320000
073300        MOVE PARM-RES-DATE-GREG TO WS-VIEW-DATE                   07330000
073400        MOVE PARM-RES-GREG-CENT TO WS-VIEW-CENT                   07340000
073500*CNC0517                                                          07350015
073600        MOVE PARM-PRI-DAY-OF-WEEK TO DAY1                         07360015
073700     END-IF                                                       07370000
073800      .                                                           07380000
073900*                                                                 07390000
074000 P1000-LIST-XB.                                                   07400000
074100*                                                                 07410000
074200*CNC0517 - START                                                  07420015
074300*    RETRIEVE SUB-DISTRICT CONTROL RECORD                         07430015
074400*                                                                 07440015
074500     MOVE FICT-JOB-DIST     TO WK-CNTL-DIST                       07450015
074600     MOVE FICT-JOB-SUB-DIST TO WK-CNTL-SUB-DIST                   07460015
074700     MOVE '02'              TO WK-CNTL-REC-TYPE                   07470015
074800     MOVE WORK-CNTLKEY   TO CNTLKEY                               07480015
074900     EXEC CICS READ                                               07490015
075000               DATASET(CNTL-FILE-VIA-CNTLKEY)                     07500015
075100               INTO(WS-CNTL-FILE)                                 07510015
075200               LENGTH(CNTLFILE-RLGTH)                             07520015
075300               RIDFLD(CNTLKEY)                                    07530015
075400               KEYLENGTH(CNTLFILE-KLGTH)                          07540015
075500               RESP(WS-RESPONSE)                                  07550015
075600     END-EXEC                                                     07560015
075700     MOVE WS-RESPONSE TO FILE-STATUS                              07570015
075800     IF NOT SUCCESS                                               07580015
075900        MOVE 'P1000-A' TO ERR-PARAGRAPH                           07590015
076000        MOVE CNTLKEY   TO ERR-KEY                                 07600015
076100        PERFORM P9999-GOT-PROBLEM                                 07610015
076200     END-IF                                                       07620015
076300     IF DAILY-MARKUP-ALLOWED                                      07630015
076400        SET DAILY-MARK-YARD TO TRUE                               07640015
076500     END-IF                                                       07650015
076600*CNC0517 - FINISH                                                 07660015
076700*                                                                 07670015
076800*    CHECK THE EXTRABOARD ID FOR VALIDITY                         07680000
076900*                                                                 07690000
077000     MOVE FICT-JOB-DIST      TO WK-CNTL-DIST                      07700000
077100                                XB-WORK-DIST                      07710000
077200                                WK-ASGN-DIST                      07720000
077300     MOVE FICT-JOB-SUB-DIST  TO WK-CNTL-SUB-DIST                  07730000
077400                                XB-WORK-SUB-DIST                  07740000
077500                                WK-ASGN-SUB-DIST                  07750000
077600     MOVE FICT-JOB-GRP-PL-XB TO WK-CNTL-XB                        07760000
077700                                XB-WORK-CC                        07770000
077800                                WK-ASGN-CC                        07780000
077900     MOVE '08'               TO WK-CNTL-REC-TYPE                  07790000
078000     MOVE WORK-CNTLKEY       TO CNTLKEY                           07800000
078100     EXEC CICS READ                                               07810000
078200               DATASET(CNTL-FILE-VIA-CNTLKEY)                     07820000
078300               INTO(WS-CNTL-FILE)                                 07830000
078400               LENGTH(CNTLFILE-RLGTH)                             07840000
078500               RIDFLD(CNTLKEY)                                    07850000
078600               KEYLENGTH(CNTLFILE-KLGTH)                          07860000
078700               RESP(WS-RESPONSE)                                  07870000
078800     END-EXEC                                                     07880000
078900     MOVE WS-RESPONSE TO FILE-STATUS                              07890000
079000     IF NOT SUCCESS                                               07900000
079100        IF NO-RECORD-FND                                          07910000
079200*               'NO CONTROL RECORD EXISTS FOR THIS SPAREBOARD'    07920000
079300           MOVE 'N025' TO MSGLOG-CODE                             07930000
079400           PERFORM P9000-SEND-MAP-AND-RETURN                      07940000
079500        ELSE                                                      07950000
079600           MOVE 'P0100-4' TO ERR-PARAGRAPH                        07960000
079700           MOVE CNTLKEY   TO ERR-KEY                              07970000
079800           PERFORM P9999-GOT-PROBLEM                              07980000
079900        END-IF                                                    07990000
080000     END-IF                                                       08000000
080100                                                                  08010000
080200     PERFORM P9820-SNAPSHOT-XB                                    08020000
080300*                                                                 08030000
080400*    MOVE THE APPROPRIATE HEADER FOR THE TYPE OF BOARD            08040000
080500*                                                                 08050000
080600     IF FICT-POSITION-ORDER                                       08060000
080700        IF PSTCA-SUB = 2                                          08070000
080800           MOVE WS-VARIABLE-LINE-1-HDR-FRENCH TO SCR02D-HEADER    08080000
080900        ELSE                                                      08090000
081000           MOVE WS-VARIABLE-LINE-1-HDR TO SCR02D-HEADER           08100000
081100        END-IF                                                    08110000
081200     ELSE                                                         08120000
081300        IF DUAL-XB                                                08130000
081400           IF PSTCA-SUB = 2                                       08140000
081500              MOVE 'BD' TO WS-VL2-HDR-BOARD-F                     08150000
081600           ELSE                                                   08160000
081700              MOVE 'BD' TO WS-VL2-HDR-BOARD                       08170000
081800           END-IF                                                 08180000
081900        ELSE                                                      08190000
082000           MOVE '  ' TO WS-VL2-HDR-BOARD   WS-VL2-HDR-BOARD-F     08200000
082100        END-IF                                                    08210000
082200*CNC0517-START                                                    08220015
082300        IF FICT-QUAL-ORDER                                        08230014
082400           IF PSTCA-SUB = 2                                       08240014
082500              MOVE WS-VARIABLE-LINE-4-HDR-FRENCH TO SCR02D-HEADER 08250015
082600           ELSE                                                   08260014
082700              MOVE WS-VARIABLE-LINE-4-HDR        TO SCR02D-HEADER 08270015
082800           END-IF                                                 08280014
082900        ELSE                                                      08290014
083000*CNC0517-FINISH                                                   08300015
083100           IF PSTCA-SUB = 2                                       08310014
083200              MOVE WS-VARIABLE-LINE-2-HDR-FRENCH TO SCR02D-HEADER 08320014
083300           ELSE                                                   08330014
083400              MOVE WS-VARIABLE-LINE-2-HDR TO SCR02D-HEADER        08340014
083500           END-IF                                                 08350014
083600        END-IF                                                    08360014
083700     END-IF                                                       08370000
083800*                                                                 08380000
083900*    CLEAR ARRAY - RESET ATTRIBUTES AND COLORS                    08390000
084000*                                                                 08400000
084100     MOVE -1 TO SCR02D-VIEW-TIME-CURSOR                           08410000
084200     PERFORM VARYING TURN-SUB FROM 1 BY 1                         08420000
084300             UNTIL TURN-SUB > ARRAY-MAX                           08430000
084400        MOVE SPACES         TO SCR02D-NAME(TURN-SUB)              08440000
084500                               SCR02D-TURN(TURN-SUB)              08450000
084600                               SCR02D-VARIABLE(TURN-SUB)          08460000
084700        MOVE WHITE          TO SCR02D-NAME-COLOR(TURN-SUB)        08470000
084800                               SCR02D-TURN-COLOR(TURN-SUB)        08480000
084900***     EVEN LINES ARE CYAN, ODD LINES ARE WHITE                  08490000
085000        DIVIDE TURN-SUB BY 2 GIVING WS-LINE-NUMBER-MOD            08500000
085100        IF WS-MOD > 0                                             08510000
085200           MOVE WHITE       TO SCR02D-VARIABLE-COLOR(TURN-SUB)    08520000
085300        ELSE                                                      08530000
085400           MOVE CYAN        TO SCR02D-VARIABLE-COLOR(TURN-SUB)    08540000
085500        END-IF                                                    08550000
085600     END-PERFORM                                                  08560000
085700     IF ENTER-KEY                                                 08570000
085800        MOVE SPACES TO FICA-SCROLL-KEY                            08580000
085900        MOVE ZEROES TO FICA-HOLD-POS                              08590000
086000        MOVE SPACES TO FICA-SCROLL-OFF-BD                         08600000
086100     END-IF                                                       08610000
086200     MOVE ZEROS TO TURN-SUB                                       08620000
086300*                                                                 08630000
086400*    FOR FASTSLOW BOARDS, BOARDS PRESENTED IN POSITION ORDER      08640000
086500*    MUST USE THE APPROPRIATE POSITION KEY.                       08650000
086600*                                                          **PLS  08660000
086700     IF NOT FICA-SCROLL-OFF-BD-ORD                                08670000
086800*CNC0517 IF FICT-TURN-ORDER                                       08680015
086900*          PERFORM P1100-BUILD-TURN-BOARD                         08690015
087000*       ELSE                                                      08700015
087100*          IF FICT-SENIORITY-ORDER                                08710015
087200*             PERFORM P1200-BUILD-SENIORITY-BOARD                 08720015
087300*          ELSE                                                   08730015
087400*             IF FASTSLOW-XB AND FICT-ROAD-BOARD                  08740015
087500*                PERFORM P1350-BUILD-SLOW-POS-BOARD               08750015
087600*             ELSE                                                08760015
087700*                PERFORM P1300-BUILD-POSITION-BOARD               08770015
087800*             END-IF                                              08780015
087900*          END-IF                                                 08790015
088000*       END-IF                                                    08800015
088100        EVALUATE TRUE                                             08810015
088200           WHEN FICT-TURN-ORDER                                   08820015
088300                PERFORM P1100-BUILD-TURN-BOARD                    08830015
088400           WHEN FICT-SENIORITY-ORDER                              08840015
088500                PERFORM P1200-BUILD-SENIORITY-BOARD               08850015
088600           WHEN FICT-QUAL-ORDER                                   08860015
088700                IF FASTSLOW-XB AND FICT-ROAD-BOARD                08870015
088800                   PERFORM P1450-BUILD-SLOW-QUAL-BOARD            08880015
088900                ELSE                                              08890015
089000                   PERFORM P1400-BUILD-QUAL-BOARD                 08900015
089100                END-IF                                            08910015
089200           WHEN OTHER                                             08920015
089300                IF FASTSLOW-XB AND FICT-ROAD-BOARD                08930015
089400                   PERFORM P1350-BUILD-SLOW-POS-BOARD             08940015
089500                ELSE                                              08950015
089600                   PERFORM P1300-BUILD-POSITION-BOARD             08960015
089700                END-IF                                            08970015
089800        END-EVALUATE                                              08980015
089900*                                                                 08990015
090000     ELSE                                                         09000000
090100        IF FASTSLOW-XB AND FICT-ROAD-BOARD                        09010000
090200           PERFORM P1450-BUILD-SLOW-OFF-BOARD                     09020000
090300        ELSE                                                      09030000
090400           PERFORM P1400-BUILD-OFF-BOARD                          09040000
090500        END-IF                                                    09050000
090600     END-IF                                                       09060000
090700     IF TURN-SUB NOT > ARRAY-MAX                                  09070000
090800*         'END OF SPAREBOARD'                                     09080000
090900        MOVE 'E010' TO MSGLOG-CODE                                09090000
091000     END-IF.                                                      09100000
091100*                                                                 09110000
091200 P1100-BUILD-TURN-BOARD.                                          09120000
091300*                                                                 09130000
091400     IF PFKEY8 AND                                                09140000
091500        FICA-SCROLL-KEY > SPACES                                  09150000
091600         MOVE FICA-SCROLL-KEY   TO EBTURN                         09160000
091700     ELSE                                                         09170000
091800        MOVE SPACES             TO EBTURN                         09180000
091900        MOVE FICT-JOB-DIST      TO DIST OF EBTURN                 09190000
092000        MOVE FICT-JOB-SUB-DIST  TO SUBDIST OF EBTURN              09200000
092100        MOVE FICT-JOB-GRP-PL-XB TO CRAFT-CODE OF EBTURN           09210000
092200     END-IF                                                       09220000
092300     MOVE SPACES TO FICA-SCROLL-KEY                               09230000
092400     EXEC CICS STARTBR                                            09240000
092500               DATASET(EB-VIA-TURN-NBR)                           09250000
092600               RIDFLD(EBTURN)                                     09260000
092700               GTEQ                                               09270000
092800               RESP(WS-RESPONSE)                                  09280000
092900     END-EXEC                                                     09290000
093000     MOVE WS-RESPONSE TO FILE-STATUS                              09300000
093100     IF SUCCESS                                                   09310000
093200        IF PFKEY8                                                 09320000
093300           EXEC CICS READNEXT                                     09330000
093400                     DATASET(EB-VIA-TURN-NBR)                     09340000
093500                     INTO(WS-EXTRA-BOARD)                         09350000
093600                     LENGTH(EBTURNNO-RLGTH)                       09360000
093700                     RIDFLD(EBTURN)                               09370000
093800                     KEYLENGTH(EBTURNNO-KLGTH)                    09380000
093900                     RESP(WS-RESPONSE)                            09390000
094000           END-EXEC                                               09400000
094100           MOVE WS-RESPONSE TO FILE-STATUS                        09410000
094200        END-IF                                                    09420000
094300     END-IF                                                       09430000
094400     IF SUCCESS                                                   09440000
094500        MOVE '0' TO DONE-CODE                                     09450000
094600        PERFORM UNTIL DONE                                        09460000
094700           EXEC CICS READNEXT                                     09470000
094800                     DATASET(EB-VIA-TURN-NBR)                     09480000
094900                     INTO(WS-EXTRA-BOARD)                         09490000
095000                     LENGTH(EBTURNNO-RLGTH)                       09500000
095100                     RIDFLD(EBTURN)                               09510000
095200                     KEYLENGTH(EBTURNNO-KLGTH)                    09520000
095300                     RESP(WS-RESPONSE)                            09530000
095400           END-EXEC                                               09540000
095500           MOVE WS-RESPONSE TO FILE-STATUS                        09550000
095600           IF SUCCESS                                             09560000
095700              IF DIST-REPEAT = FICT-JOB-DIST                      09570000
095800                 AND SUBDIST-REPEAT = FICT-JOB-SUB-DIST           09580000
095900                 AND CRAFT-CODE-REPEAT = FICT-JOB-GRP-PL-XB       09590000
096000*                                                                 09600000
096100*             IF FASTSLOW-XB, THE ENTERED BOARD WILL NOT          09610000
096200*             MATCH THE BOARD ON THE EB FILE.  FAST/SLOW          09620000
096300*             BOARDS ARE MARKED AS REGULAR BOARDS.                09630000
096400*                                                          **PLS  09640000
096500                 IF (FICT-YARD-BOARD AND EB-YARD-BOARD) OR        09650000
096600                    (FICT-ROAD-BOARD AND EB-ROAD-BOARD) OR        09660000
096700                    (FICT-BOARD > SPACE AND FASTSLOW-XB) OR       09670000
096800                     FICT-BOARD NOT > SPACE                       09680000
096900                    ADD 1 TO TURN-SUB                             09690000
097000                    IF TURN-SUB NOT > ARRAY-MAX                   09700000
097100                       IF TURN-SUB = ARRAY-MAX                    09710000
097200                          MOVE EBTURN-AREA TO FICA-SCROLL-KEY     09720000
097300                       END-IF                                     09730000
097400                       MOVE ZERO     TO ASGN-EMP-NO               09740000
097500                                        GOT-EMPLOYEE-FLAG         09750000
097600                       MOVE SPACES   TO WS-MSTR                   09760000
097700                       MOVE TURN-NBR TO WK-ASGN-XB-TURN           09770000
097800                       PERFORM PXXXX-LATEST-TEMP                  09780000
097900                       IF ASGN-EMP-NO NOT > ZERO                  09790000
098000                          PERFORM PXXXX-JOB-OWNER                 09800000
098100                       END-IF                                     09810000
098200                       IF ASGN-EMP-NO > ZERO                      09820000
098300                          MOVE ASGN-EMP-NO TO MSTRNBRK            09830000
098400                          PERFORM P8500-READ-MASTER               09840000
098500                          SET GOT-EMPLOYEE TO TRUE                09850000
098600                       END-IF                                     09860000
098700                       MOVE TURN-SUB TO NAME-SUB                  09870000
098800                       PERFORM P1500-SETUP-NAME-LINE              09880000
098900                    ELSE                                          09890000
099000                       SET DONE TO TRUE                           09900000
099100                    END-IF                                        09910000
099200                 END-IF                                           09920000
099300              ELSE                                                09930000
099400                 SET DONE TO TRUE                                 09940000
099500              END-IF                                              09950000
099600           ELSE                                                   09960000
099700              SET DONE TO TRUE                                    09970000
099800              IF NOT (NO-RECORD-FND OR END-OF-FILE)               09980000
099900                 MOVE 'P1000-4' TO ERR-PARAGRAPH                  09990000
100000                 MOVE EBTURN    TO ERR-KEY                        10000000
100100                 PERFORM P9999-GOT-PROBLEM                        10010000
100200              END-IF                                              10020000
100300           END-IF                                                 10030000
100400        END-PERFORM                                               10040000
100500        EXEC CICS ENDBR                                           10050000
100600                  DATASET(EB-VIA-TURN-NBR)                        10060000
100700                  RESP(WS-RESPONSE)                               10070000
100800        END-EXEC                                                  10080000
100900     ELSE                                                         10090000
101000        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     10100000
101100           MOVE 'P1000-5' TO ERR-PARAGRAPH                        10110000
101200           MOVE EBTURN    TO ERR-KEY                              10120000
101300           PERFORM P9999-GOT-PROBLEM                              10130000
101400        END-IF                                                    10140000
101500     END-IF.                                                      10150000
101600*                                                                 10160000
101700 P1200-BUILD-SENIORITY-BOARD.                                     10170000
101800*                                                                 10180000
101900     MOVE SPACES                TO P942-COMMAREA-PARMS            10190000
102000     SET P942-ASGN-SEN-FUNCTION TO TRUE                           10200000
102100     SET P942-ASGN-XB           TO TRUE                           10210000
102200     MOVE FICT-JOB-DIST         TO P942-ASGN-DIST                 10220000
102300     MOVE FICT-JOB-SUB-DIST     TO P942-ASGN-SUB-DIST             10230000
102400     MOVE FICT-JOB-GRP-PL-XB    TO P942-ASGN-CC                   10240000
102500     MOVE '******'              TO P942-ASGN-ASGN                 10250000
102600                                                                  10260000
102700     PERFORM P8080-LINK-942                                       10270000
102800     MOVE P942-ASGN-SEN-AREA    TO WS-ASGN-SEN-AREA               10280000
102900                                                                  10290000
103000     IF WS-ASGN-SEN-AREA > SPACES                                 10300000
103100        IF PFKEY8                                                 10310000
103200           AND FICA-SCROLL-KEY > SPACES                           10320000
103300           MOVE P02D-LAST-SS    TO SEN-SUB                        10330000
103400        ELSE                                                      10340000
103500           MOVE SPACES          TO P02DCA-DUP-EMPLOYEE-TABLE      10350000
103600           MOVE ZEROS           TO FICA-HOLD-POS                  10360000
103700           MOVE 1               TO SEN-SUB                        10370000
103800        END-IF                                                    10380000
103900*\/*@C595 - SETUP SCREEN-FULL-FLAG                                10390000
104000*                                                                 10400000
104100        SET  SCREEN-NOT-FULL        TO TRUE                       10410000
104200*                                                                 10420000
104300*/\*@C595                                                         10430000
104400        MOVE '0'                TO SEN-ROSTER-DONE-CODE           10440000
104500        PERFORM UNTIL SEN-ROSTER-DONE                             10450000
104600           IF WS-ASGN-SEN-RSTR(SEN-SUB) > SPACES                  10460000
104700              MOVE SPACES       TO WS-CNTL-FILE                   10470000
104800              SET ROSTER-TYPE-REC TO TRUE                         10480000
104900              MOVE WS-ASGN-SEN-RSTR(SEN-SUB)                      10490000
105000                                TO CNTL-P-ROSTER-CODE             10500000
105100              MOVE WS-ASGN-SEN-CC(SEN-SUB)                        10510000
105200                                TO CNTL-ROSTER-CC                 10520000
105300              MOVE CNTLKEY-AREA TO CNTLKEY                        10530000
105400              EXEC CICS READ                                      10540000
105500                        DATASET(CNTL-FILE-VIA-CNTLKEY)            10550000
105600                        INTO(WS-CNTL-FILE)                        10560000
105700                        LENGTH(CNTLFILE-RLGTH)                    10570000
105800                        RIDFLD(CNTLKEY)                           10580000
105900                        KEYLENGTH(CNTLFILE-KLGTH)                 10590000
106000                        RESP(WS-RESPONSE)                         10600000
106100              END-EXEC                                            10610000
106200              MOVE WS-RESPONSE  TO FILE-STATUS                    10620000
106300              IF NOT SUCCESS                                      10630000
106400                 MOVE 'P1200-1' TO ERR-PARAGRAPH                  10640000
106500                 MOVE CNTLKEY   TO ERR-KEY                        10650000
106600                 PERFORM P9999-GOT-PROBLEM                        10660000
106700              END-IF                                              10670000
106800              PERFORM P1220-BUILD-SENIORITY-ROSTER                10680000
106900*                                                                 10690000
107000*\/*@C595 - CHECK ON SCREEN FULL                                  10700000
107100*                                                                 10710000
107200              IF ( SCREEN-FULL )                                  10720000
107300                 SET SEN-ROSTER-DONE TO TRUE                      10730000
107400              END-IF                                              10740000
107500*                                                                 10750000
107600*/\*@C595                                                         10760000
107700           END-IF                                                 10770000
107800           ADD 1                TO SEN-SUB                        10780000
107900           IF SEN-SUB > SEN-MAX                                   10790000
108000              SET SEN-ROSTER-DONE                                 10800000
108100                                TO TRUE                           10810000
108200           END-IF                                                 10820000
108300        END-PERFORM                                               10830000
108400     END-IF.                                                      10840000
108500*                                                                 10850000
108600 P1220-BUILD-SENIORITY-ROSTER.                                    10860000
108700*                                                                 10870000
108800     IF PFKEY8                                                    10880000
108900        AND FICA-SCROLL-KEY > SPACES                              10890000
109000        MOVE FICA-SCROLL-KEY TO SENKEY1                           10900000
109100     ELSE                                                         10910000
109200        SET ENTER-KEY                 TO TRUE                     10920000
109300        MOVE ZEROS                    TO FICA-HOLD-POS            10930000
109400        MOVE SPACES                   TO SF-SENKEY1               10940000
109500        MOVE WS-ASGN-SEN-RSTR(SEN-SUB) TO SF-ROSTER               10950000
109600        MOVE WS-ASGN-SEN-CC(SEN-SUB)  TO SF-CRAFT                 10960000
109700        MOVE SF-SENKEY1               TO SENKEY1                  10970000
109800     END-IF                                                       10980000
109900     IF TURN-SUB < ARRAY-MAX                                      10990000
110000        MOVE SPACES TO FICA-SCROLL-KEY                            11000000
110100     END-IF                                                       11010000
110200     EXEC CICS STARTBR                                            11020000
110300               DATASET(SENFILE-VIA-DIST)                          11030000
110400               RIDFLD(SENKEY1)                                    11040000
110500               GTEQ                                               11050000
110600               RESP(WS-RESPONSE)                                  11060000
110700     END-EXEC                                                     11070000
110800     MOVE WS-RESPONSE TO FILE-STATUS                              11080000
110900     IF SUCCESS                                                   11090000
111000        IF PFKEY8                                                 11100000
111100           EXEC CICS READNEXT                                     11110000
111200                     DATASET(SENFILE-VIA-DIST)                    11120000
111300                     INTO (WS-SENIORITY)                          11130000
111400                     LENGTH(SENDIST-RLGTH)                        11140000
111500                     RIDFLD(SENKEY1)                              11150000
111600                     KEYLENGTH(SENDIST-KLGTH)                     11160000
111700                     RESP(WS-RESPONSE)                            11170000
111800           END-EXEC                                               11180000
111900           MOVE WS-RESPONSE TO FILE-STATUS                        11190000
112000        END-IF                                                    11200000
112100     END-IF                                                       11210000
112200     IF SUCCESS                                                   11220000
112300        MOVE '0' TO DONE-CODE                                     11230000
112400        PERFORM UNTIL DONE                                        11240000
112500           EXEC CICS READNEXT                                     11250000
112600                     DATASET(SENFILE-VIA-DIST)                    11260000
112700                     INTO (WS-SENIORITY)                          11270000
112800                     LENGTH(SENDIST-RLGTH)                        11280000
112900                     RIDFLD(SENKEY1)                              11290000
113000                     KEYLENGTH(SENDIST-KLGTH)                     11300000
113100                     RESP(WS-RESPONSE)                            11310000
113200           END-EXEC                                               11320000
113300           MOVE WS-RESPONSE TO FILE-STATUS                        11330000
113400           IF SUCCESS                                             11340000
113500              IF SF-ROSTER = WS-ASGN-SEN-RSTR(SEN-SUB)            11350000
113600                 AND SF-CRAFT = WS-ASGN-SEN-CC(SEN-SUB)           11360000
113700                 PERFORM P1230-CHECK-EMPLOYEE-SETUP               11370000
113800*                                                                 11380000
113900*\/*@C595 - CHECK ON SCREEN FULL                                  11390000
114000*                                                                 11400000
114100                 IF ( SCREEN-FULL )                               11410000
114200                    SET DONE TO TRUE                              11420000
114300                 END-IF                                           11430000
114400*                                                                 11440000
114500*/\*@C595                                                         11450000
114600              ELSE                                                11460000
114700                 SET DONE TO TRUE                                 11470000
114800              END-IF                                              11480000
114900           ELSE                                                   11490000
115000              SET DONE TO TRUE                                    11500000
115100              IF NOT (NO-RECORD-FND OR END-OF-FILE)               11510000
115200                 MOVE 'P1200-2' TO ERR-PARAGRAPH                  11520000
115300                 MOVE SENKEY1   TO ERR-KEY                        11530000
115400                 PERFORM P9999-GOT-PROBLEM                        11540000
115500              END-IF                                              11550000
115600           END-IF                                                 11560000
115700        END-PERFORM                                               11570000
115800        EXEC CICS ENDBR                                           11580000
115900                  DATASET(SENFILE-VIA-DIST)                       11590000
116000                  RESP(WS-RESPONSE)                               11600000
116100        END-EXEC                                                  11610000
116200     ELSE                                                         11620000
116300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     11630000
116400           MOVE 'P1200-3' TO ERR-PARAGRAPH                        11640000
116500           MOVE SENKEY3   TO ERR-KEY                              11650000
116600           PERFORM P9999-GOT-PROBLEM                              11660000
116700        END-IF                                                    11670000
116800     END-IF.                                                      11680000
116900*                                                                 11690000
117000 P1230-CHECK-EMPLOYEE-SETUP.                                      11700000
117100*                                                                 11710000
117200     MOVE SF-EMP-NO TO MSTRNBRK                                   11720000
117300     PERFORM P8500-READ-MASTER                                    11730000
117400     MOVE ZERO TO GOT-EMPLOYEE-FLAG                               11740000
117500     MOVE SPACES TO EBTURN                                        11750000
117600     IF (TEMPORARY-ASGNMT > SPACES                                11760000
117700        AND TEMP-ASGN-XB                                          11770000
117800        AND TA-DIST = SCR02D-DIST                                 11780000
117900        AND TA-SUB-DIST = SCR02D-SUB-DIST                         11790000
118000        AND TA-CC = FICT-JOB-GRP-PL-XB)                           11800000
118100        MOVE TA-DIST TO DIST OF EBTURN                            11810000
118200        MOVE TA-SUB-DIST TO SUBDIST OF EBTURN                     11820000
118300        MOVE TA-CC TO CRAFT-CODE OF EBTURN                        11830000
118400        MOVE TA-XB-TURN TO EB-TURN-NBR OF EBTURN                  11840000
118500     ELSE                                                         11850000
118600        IF (NORMAL-ASGNMT > SPACE                                 11860000
118700           AND NORM-ASGN-XB                                       11870000
118800           AND NA-DIST = SCR02D-DIST                              11880000
118900           AND NA-SUB-DIST = SCR02D-SUB-DIST                      11890000
119000           AND NA-CC = FICT-JOB-GRP-PL-XB)                        11900000
119100           MOVE NA-DIST TO DIST OF EBTURN                         11910000
119200           MOVE NA-SUB-DIST TO SUBDIST OF EBTURN                  11920000
119300           MOVE NA-CC TO CRAFT-CODE OF EBTURN                     11930000
119400           MOVE NA-XB-TURN TO EB-TURN-NBR OF EBTURN               11940000
119500        END-IF                                                    11950000
119600     END-IF                                                       11960000
119700     IF EBTURN > SPACES                                           11970000
119800        EXEC CICS READ                                            11980000
119900                  DATASET(EB-VIA-TURN-NBR)                        11990000
120000                  INTO(WS-EXTRA-BOARD)                            12000000
120100                  LENGTH(EBTURNNO-RLGTH)                          12010000
120200                  RIDFLD(EBTURN)                                  12020000
120300                  KEYLENGTH(EBTURNNO-KLGTH)                       12030000
120400                  RESP(WS-RESPONSE)                               12040000
120500        END-EXEC                                                  12050000
120600        MOVE WS-RESPONSE TO FILE-STATUS                           12060000
120700        IF NOT SUCCESS                                            12070000
120800           MOVE 'P1200-1' TO ERR-PARAGRAPH                        12080000
120900           MOVE EBTURN  TO ERR-KEY                                12090000
121000           PERFORM P9999-GOT-PROBLEM                              12100000
121100        END-IF                                                    12110000
121200        SET NOT-DUP-EMP     TO TRUE                               12120000
121300        PERFORM VARYING DUP-SUB FROM 1 BY 1                       12130000
121400             UNTIL DUP-SUB > DUP-MAX                              12140000
121500          IF P02DCA-DUP-EMP-NBR(DUP-SUB) NOT > SPACES             12150000
121600             MOVE SF-EMP-NO  TO P02DCA-DUP-EMP-NBR(DUP-SUB)       12160000
121700             MOVE DUP-MAX    TO DUP-SUB                           12170000
121800          ELSE                                                    12180000
121900             IF SF-EMP-NO = P02DCA-DUP-EMP-NBR(DUP-SUB)           12190000
122000                SET DUP-EMP  TO TRUE                              12200000
122100                MOVE DUP-MAX TO DUP-SUB                           12210000
122200             END-IF                                               12220000
122300          END-IF                                                  12230000
122400        END-PERFORM                                               12240000
122500        IF NOT DUP-EMP                                            12250000
122600           ADD 1 TO TURN-SUB                                      12260000
122700           IF TURN-SUB NOT > ARRAY-MAX                            12270000
122800              IF TURN-SUB = ARRAY-MAX                             12280000
122900                 MOVE SENKEY1 TO FICA-SCROLL-KEY                  12290000
123000                 MOVE SEN-SUB  TO P02D-LAST-SS                    12300000
123100*                                                                 12310000
123200*\/*@C595 - SCREEN ARRAY IS NOT FULL, SET FULL FLAG               12320000
123300*                                                                 12330000
123400                 SET SCREEN-FULL      TO TRUE                     12340000
123500*                                                                 12350000
123600*/\*@C595                                                         12360000
123700              END-IF                                              12370000
123800              SET GOT-EMPLOYEE TO TRUE                            12380000
123900              MOVE TURN-SUB TO NAME-SUB                           12390000
124000              PERFORM P1500-SETUP-NAME-LINE                       12400000
124100           ELSE                                                   12410000
124200              SET DONE TO TRUE                                    12420000
124300           END-IF                                                 12430000
124400        END-IF                                                    12440000
124500     END-IF.                                                      12450000
124600*                                                                 12460000
124700 P1300-BUILD-POSITION-BOARD.                                      12470000
124800*                                                                 12480000
124900     IF PFKEY8 AND                                                12490000
125000        FICA-SCROLL-KEY > SPACES                                  12500000
125100         MOVE FICA-SCROLL-KEY TO EBPOS                            12510000
125200     ELSE                                                         12520000
125300        SET ENTER-KEY TO TRUE                                     12530000
125400        MOVE ZEROS              TO FICA-HOLD-POS                  12540000
125500        MOVE SPACES             TO EBPOS-AREA                     12550000
125600        MOVE FICT-JOB-DIST      TO DIST OF WS-EXTRA-BOARD         12560000
125700        MOVE FICT-JOB-SUB-DIST  TO SUB-DIST OF WS-EXTRA-BOARD     12570000
125800        MOVE FICT-JOB-GRP-PL-XB TO CRAFT-CODE OF WS-EXTRA-BOARD   12580000
125900        MOVE ZEROS              TO EB-POSITION                    12590000
126000*                                                          **PLS  12600000
126100        IF NOT FASTSLOW-XB                                        12610000
126200           IF FICT-YARD-BOARD                                     12620000
126300              MOVE '1' TO EB-POS-BOARD                            12630000
126400           ELSE                                                   12640000
126500              IF FICT-ROAD-BOARD                                  12650000
126600                 MOVE '2' TO EB-POS-BOARD                         12660000
126700              END-IF                                              12670000
126800           END-IF                                                 12680000
126900        END-IF                                                    12690000
127000        MOVE EBPOS-AREA TO EBPOS                                  12700000
127100     END-IF                                                       12710000
127200     MOVE SPACES TO FICA-SCROLL-KEY                               12720000
127300     EXEC CICS STARTBR                                            12730000
127400               DATASET(EB-VIA-CRAFT-POSITION)                     12740000
127500               RIDFLD(EBPOS)                                      12750000
127600               GTEQ                                               12760000
127700               RESP(WS-RESPONSE)                                  12770000
127800     END-EXEC                                                     12780000
127900     MOVE WS-RESPONSE TO FILE-STATUS                              12790000
128000     IF SUCCESS                                                   12800000
128100        IF PFKEY8                                                 12810000
128200           EXEC CICS READNEXT                                     12820000
128300                     DATASET(EB-VIA-CRAFT-POSITION)               12830000
128400                     INTO(WS-EXTRA-BOARD)                         12840000
128500                     LENGTH(EBCRPOS-RLGTH)                        12850000
128600                     RIDFLD(EBPOS)                                12860000
128700                     KEYLENGTH(EBCRPOS-KLGTH)                     12870000
128800                     RESP(WS-RESPONSE)                            12880000
128900           END-EXEC                                               12890000
129000           MOVE WS-RESPONSE TO FILE-STATUS                        12900000
129100        END-IF                                                    12910000
129200     END-IF                                                       12920000
129300     IF SUCCESS                                                   12930000
129400        MOVE '0' TO DONE-CODE                                     12940000
129500        PERFORM UNTIL DONE                                        12950000
129600           EXEC CICS READNEXT                                     12960000
129700                     DATASET(EB-VIA-CRAFT-POSITION)               12970000
129800                     INTO(WS-EXTRA-BOARD)                         12980000
129900                     LENGTH(EBCRPOS-RLGTH)                        12990000
130000                     RIDFLD(EBPOS)                                13000000
130100                     KEYLENGTH(EBCRPOS-KLGTH)                     13010000
130200                     RESP(WS-RESPONSE)                            13020000
130300           END-EXEC                                               13030000
130400           MOVE WS-RESPONSE TO FILE-STATUS                        13040000
130500           IF SUCCESS                                             13050000
130600*                                                          **PLS  13060000
130700              IF DIST OF WS-EXTRA-BOARD = FICT-JOB-DIST           13070000
130800                AND SUB-DIST OF WS-EXTRA-BOARD = FICT-JOB-SUB-DIST13080000
130900                AND CRAFT-CODE OF WS-EXTRA-BOARD                  13090000
131000                                             = FICT-JOB-GRP-PL-XB 13100000
131100                AND ((FICT-YARD-BOARD AND EB-YARD-BOARD)          13110000
131200                OR (FICT-ROAD-BOARD AND EB-ROAD-BOARD)            13120000
131300                OR (FICT-BOARD > SPACE AND FASTSLOW-XB)           13130000
131400                OR FICT-BOARD NOT > SPACE)                        13140000
131500                MOVE SPACES       TO TZ-PARAMETERS                13150000
131600                SET TZ-IN-SYSTEM-ZONE TO TRUE                     13160000
131700                MOVE EB-POS-DATE-TIME TO TZ-IN-DATE-TIME          13170000
131800                MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE               13180000
131900                PERFORM P8996-TIMEZONE                            13190000
132000                MOVE TZ-OUT-DATE-TIME TO WS-POS-DATE-TIME-TZ      13200000
132100                MOVE TZ-OUT-CE        TO WS-POS-CENT              13210000
132200*               IF WS-POS-DATE-TIME-TZ <= WS-VIEW-DATE-TIME       13220000
132300                IF (WS-POS-DATE-TIME-TZ-CENT                      13230000
132400                                <=  WS-VIEW-DATE-TIME-CENT        13240000
132500                   OR WS-VIEW-DATE = '991231' OR '000101')        13250000
132600                   AND ((NOT FASTSLOW-XB AND EB-ON-BOARD) OR      13260000
132700                        (FASTSLOW-XB AND EB-ON-BOARD              13270000
132800                                AND EB-SLOW-ON-BOARD))            13280000
132900                   ADD 1 TO TURN-SUB                              13290000
133000                   IF TURN-SUB NOT > ARRAY-MAX                    13300000
133100                      IF TURN-SUB = ARRAY-MAX                     13310000
133200                         MOVE EBPOS-AREA TO FICA-SCROLL-KEY       13320000
133300                      END-IF                                      13330000
133400                      MOVE ZERO     TO ASGN-EMP-NO                13340000
133500                                       GOT-EMPLOYEE-FLAG          13350000
133600                      MOVE SPACES   TO WS-MSTR                    13360000
133700                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            13370000
133800                      PERFORM PXXXX-LATEST-TEMP                   13380000
133900                      IF ASGN-EMP-NO NOT > ZERO                   13390000
134000                         PERFORM PXXXX-JOB-OWNER                  13400000
134100                      END-IF                                      13410000
134200                      IF ASGN-EMP-NO > ZERO                       13420000
134300                         MOVE ASGN-EMP-NO TO MSTRNBRK             13430000
134400                         PERFORM P8500-READ-MASTER                13440000
134500                         SET GOT-EMPLOYEE TO TRUE                 13450000
134600                      END-IF                                      13460000
134700                      IF GOT-EMPLOYEE                             13470000
134800                         PERFORM P1505-CALL-XB-POS-PARMS          13480000
134900                         SET DISPLAY-EMP  TO TRUE                 13490000
135000                         IF (AVAILABLE OR WORKING)                13500000
135100                            CONTINUE                              13510000
135200                         ELSE                                     13520000
135300                            IF NOT FICT-ALL-POSITIONS             13530000
135400                               MOVE '0'        TO DISPLAY-EMP-FLAG13540000
135500*                              IF P912-SCHEDULED-REST-DAY         13550012
135600*                                 CONTINUE                        13560012
135700*                              ELSE                               13570012
135800                                  PERFORM P9830-RETRIEVE-CNTL-INFO13580000
135900*                                 PERFORM VARYING SUB1 FROM 1 BY 113590000
136000*                                    UNTIL SUB1 > 5               13600000
136100*                                    IF CNTL-XB-RETAIN-POS(SUB1) =13610000
136200*                                       (LAYOFF-CODE-1 OR '*')    13620000
136300*                                       SET DISPLAY-EMP TO TRUE   13630000
136400*                                    END-IF                       13640000
136500*                                 END-PERFORM                     13650000
136600*                              END-IF                             13660012
136700                            END-IF                                13670000
136800                         END-IF                                   13680000
136900                         IF DISPLAY-EMP                           13690000
137000                            MOVE TURN-SUB TO NAME-SUB             13700000
137100                            PERFORM P1500-SETUP-NAME-LINE         13710000
137200                         ELSE                                     13720000
137300                            SUBTRACT 1 FROM TURN-SUB              13730000
137400                         END-IF                                   13740000
137500                      ELSE                                        13750000
137600                         SUBTRACT 1 FROM TURN-SUB                 13760000
137700                      END-IF                                      13770000
137800                   ELSE                                           13780000
137900                      SET DONE TO TRUE                            13790000
138000                   END-IF                                         13800000
138100                END-IF                                            13810000
138200              ELSE                                                13820000
138300                 SET DONE TO TRUE                                 13830000
138400              END-IF                                              13840000
138500           ELSE                                                   13850000
138600              SET DONE TO TRUE                                    13860000
138700              IF NOT (NO-RECORD-FND OR END-OF-FILE)               13870000
138800                 MOVE 'P1300-1' TO ERR-PARAGRAPH                  13880000
138900                 MOVE EBPOS     TO ERR-KEY                        13890000
139000                 PERFORM P9999-GOT-PROBLEM                        13900000
139100              END-IF                                              13910000
139200           END-IF                                                 13920000
139300        END-PERFORM                                               13930000
139400        EXEC CICS ENDBR                                           13940000
139500                  DATASET(EB-VIA-CRAFT-POSITION)                  13950000
139600                  RESP(WS-RESPONSE)                               13960000
139700        END-EXEC                                                  13970000
139800        IF FICT-ALL-POSITIONS                                     13980000
139900           IF TURN-SUB NOT > ARRAY-MAX                            13990000
140000              MOVE TURN-SUB TO SAVE-SCR-POS                       14000000
140100              PERFORM P1400-BUILD-OFF-BOARD                       14010000
140200           END-IF                                                 14020000
140300        END-IF                                                    14030000
140400     ELSE                                                         14040000
140500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     14050000
140600           MOVE 'P1300-2' TO ERR-PARAGRAPH                        14060000
140700           MOVE EBTURN    TO ERR-KEY                              14070000
140800           PERFORM P9999-GOT-PROBLEM                              14080000
140900        END-IF                                                    14090000
141000     END-IF.                                                      14100000
141100*                                                                 14110000
141200 P1350-BUILD-SLOW-POS-BOARD.                                      14120000
141300*                                                                 14130000
141400*   BROWSE THROUGH THE EB FILE IN SLOW POSITION KEY ORDER.        14140000
141500*                                                          **PLS  14150000
141600     IF PFKEY8 AND                                                14160000
141700        FICA-SCROLL-KEY > SPACES                                  14170000
141800         MOVE FICA-SCROLL-KEY TO EBPOS                            14180000
141900     ELSE                                                         14190000
142000        SET ENTER-KEY TO TRUE                                     14200000
142100        MOVE ZEROS              TO FICA-HOLD-POS                  14210000
142200        MOVE SPACES             TO EB-SLOW-POS-AREA               14220000
142300        MOVE FICT-JOB-DIST      TO EB-SLOW-DIST                   14230000
142400        MOVE FICT-JOB-SUB-DIST  TO EB-SLOW-SUB-DIST               14240000
142500        MOVE FICT-JOB-GRP-PL-XB TO EB-SLOW-CRAFT-CODE             14250000
142600        MOVE ZEROS              TO EB-SLOW-POSITION               14260000
142700        MOVE EB-SLOW-POS-AREA TO EBSLOW-POS                       14270000
142800     END-IF                                                       14280000
142900     MOVE SPACES TO FICA-SCROLL-KEY                               14290000
143000     EXEC CICS STARTBR                                            14300000
143100               DATASET(EB-VIA-SLOW-POSITION)                      14310000
143200               RIDFLD(EBSLOW-POS)                                 14320000
143300               GTEQ                                               14330000
143400               RESP(WS-RESPONSE)                                  14340000
143500     END-EXEC                                                     14350000
143600     MOVE WS-RESPONSE TO FILE-STATUS                              14360000
143700     IF SUCCESS                                                   14370000
143800        IF PFKEY8                                                 14380000
143900           EXEC CICS READNEXT                                     14390000
144000                     DATASET(EB-VIA-SLOW-POSITION)                14400000
144100                     INTO(WS-EXTRA-BOARD)                         14410000
144200                     LENGTH(EBSLPOS-RLGTH)                        14420000
144300                     RIDFLD(EBSLOW-POS)                           14430000
144400                     KEYLENGTH(EBSLPOS-KLGTH)                     14440000
144500                     RESP(WS-RESPONSE)                            14450000
144600           END-EXEC                                               14460000
144700           MOVE WS-RESPONSE TO FILE-STATUS                        14470000
144800        END-IF                                                    14480000
144900     END-IF                                                       14490000
145000     IF SUCCESS                                                   14500000
145100        MOVE '0' TO DONE-CODE                                     14510000
145200        PERFORM UNTIL DONE                                        14520000
145300           EXEC CICS READNEXT                                     14530000
145400                     DATASET(EB-VIA-SLOW-POSITION)                14540000
145500                     INTO(WS-EXTRA-BOARD)                         14550000
145600                     LENGTH(EBSLPOS-RLGTH)                        14560000
145700                     RIDFLD(EBSLOW-POS)                           14570000
145800                     KEYLENGTH(EBSLPOS-KLGTH)                     14580000
145900                     RESP(WS-RESPONSE)                            14590000
146000           END-EXEC                                               14600000
146100           MOVE WS-RESPONSE TO FILE-STATUS                        14610000
146200           IF SUCCESS                                             14620000
146300*          VERIFY ON-BOARD ON BOTH NORMAL AND SLOW BOARDS  **PLS  14630000
146400              IF EB-SLOW-DIST = FICT-JOB-DIST                     14640000
146500                AND EB-SLOW-SUB-DIST = FICT-JOB-SUB-DIST          14650000
146600                AND EB-SLOW-CRAFT-CODE = FICT-JOB-GRP-PL-XB       14660000
146700                AND (FICT-BOARD > SPACE AND FASTSLOW-XB)          14670000
146800                MOVE SPACES       TO TZ-PARAMETERS                14680000
146900                SET TZ-IN-SYSTEM-ZONE TO TRUE                     14690000
147000                MOVE EB-SLOW-POS-DATE-TIME TO TZ-IN-DATE-TIME     14700000
147100                MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE               14710000
147200                PERFORM P8996-TIMEZONE                            14720000
147300                MOVE TZ-OUT-DATE-TIME TO WS-POS-DATE-TIME-TZ      14730000
147400                MOVE TZ-OUT-CE        TO WS-POS-CENT              14740000
147500*-------------------------------------------------------------    14750000
147600*    WHEN DISPLAYING THE SLOW BOARD, ONLY LOOK TO SEE IF THE      14760000
147700*    TURN IS OFF THE BOARD ON A SLOW(ROAD) JOB.  IF THE TURN      14770000
147800*    IS OFF THE BOARD ON A FAST(YARD) JOB, THEN DISPLAY THE       14780000
147900*    THE TURN / POSITION.                                         14790000
148000*----------------------------------------------------------PLS    14800000
148100*               IF WS-POS-DATE-TIME-TZ <= WS-VIEW-DATE-TIME       14810000
148200                IF (WS-POS-DATE-TIME-TZ-CENT                      14820000
148300                                <=  WS-VIEW-DATE-TIME-CENT        14830000
148400                   OR WS-VIEW-DATE = '991231' OR '000101')        14840000
148500                   AND EB-SLOW-ON-BOARD                           14850000
148600                   ADD 1 TO TURN-SUB                              14860000
148700                   IF TURN-SUB NOT > ARRAY-MAX                    14870000
148800                      IF TURN-SUB = ARRAY-MAX                     14880000
148900                         MOVE EB-SLOW-POS-AREA TO FICA-SCROLL-KEY 14890000
149000                      END-IF                                      14900000
149100                      MOVE ZERO     TO ASGN-EMP-NO                14910000
149200                                       GOT-EMPLOYEE-FLAG          14920000
149300                      MOVE SPACES   TO WS-MSTR                    14930000
149400                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            14940000
149500                      PERFORM PXXXX-LATEST-TEMP                   14950000
149600                      IF ASGN-EMP-NO NOT > ZERO                   14960000
149700                         PERFORM PXXXX-JOB-OWNER                  14970000
149800                      END-IF                                      14980000
149900                      IF ASGN-EMP-NO > ZERO                       14990000
150000                         MOVE ASGN-EMP-NO TO MSTRNBRK             15000000
150100                         PERFORM P8500-READ-MASTER                15010000
150200                         SET GOT-EMPLOYEE TO TRUE                 15020000
150300                      END-IF                                      15030000
150400                      IF GOT-EMPLOYEE                             15040000
150500                         PERFORM P1505-CALL-XB-POS-PARMS          15050000
150600                         SET DISPLAY-EMP  TO TRUE                 15060000
150700                         IF (AVAILABLE OR WORKING)                15070000
150800                            CONTINUE                              15080000
150900                         ELSE                                     15090000
151000                            IF NOT FICT-ALL-POSITIONS             15100000
151100                               MOVE '0'        TO DISPLAY-EMP-FLAG15110000
151200*                              IF P912-SCHEDULED-REST-DAY         15120012
151300*                                 CONTINUE                        15130012
151400*                              ELSE                               15140012
151500                                  PERFORM P9830-RETRIEVE-CNTL-INFO15150000
151600*                                 PERFORM VARYING SUB1 FROM 1 BY 115160000
151700*                                    UNTIL SUB1 > 5               15170000
151800*                                    IF CNTL-XB-RETAIN-POS(SUB1) =15180000
151900*                                       (LAYOFF-CODE-1 OR '*')    15190000
152000*                                       SET DISPLAY-EMP TO TRUE   15200000
152100*                                    END-IF                       15210000
152200*                                 END-PERFORM                     15220000
152300*                              END-IF                             15230012
152400                            END-IF                                15240000
152500                         END-IF                                   15250000
152600                         IF DISPLAY-EMP                           15260000
152700                            MOVE TURN-SUB TO NAME-SUB             15270000
152800                            PERFORM P1500-SETUP-NAME-LINE         15280000
152900                         ELSE                                     15290000
153000                            SUBTRACT 1 FROM TURN-SUB              15300000
153100                         END-IF                                   15310000
153200                      ELSE                                        15320000
153300                         SUBTRACT 1 FROM TURN-SUB                 15330000
153400                      END-IF                                      15340000
153500                   ELSE                                           15350000
153600                      SET DONE TO TRUE                            15360000
153700                   END-IF                                         15370000
153800                END-IF                                            15380000
153900              ELSE                                                15390000
154000                 SET DONE TO TRUE                                 15400000
154100              END-IF                                              15410000
154200           ELSE                                                   15420000
154300              SET DONE TO TRUE                                    15430000
154400              IF NOT (NO-RECORD-FND OR END-OF-FILE)               15440000
154500                 MOVE 'P1350-1' TO ERR-PARAGRAPH                  15450000
154600                 MOVE EBPOS     TO ERR-KEY                        15460000
154700                 PERFORM P9999-GOT-PROBLEM                        15470000
154800              END-IF                                              15480000
154900           END-IF                                                 15490000
155000        END-PERFORM                                               15500000
155100        EXEC CICS ENDBR                                           15510000
155200                  DATASET(EB-VIA-SLOW-POSITION)                   15520000
155300                  RESP(WS-RESPONSE)                               15530000
155400        END-EXEC                                                  15540000
155500        IF FICT-ALL-POSITIONS                                     15550000
155600           IF TURN-SUB NOT > ARRAY-MAX                            15560000
155700              MOVE TURN-SUB TO SAVE-SCR-POS                       15570000
155800              PERFORM P1450-BUILD-SLOW-OFF-BOARD                  15580000
155900           END-IF                                                 15590000
156000        END-IF                                                    15600000
156100     ELSE                                                         15610000
156200        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     15620000
156300           MOVE 'P1350-2' TO ERR-PARAGRAPH                        15630000
156400           MOVE EBTURN    TO ERR-KEY                              15640000
156500           PERFORM P9999-GOT-PROBLEM                              15650000
156600        END-IF                                                    15660000
156700     END-IF.                                                      15670000
156800*                                                                 15680000
156900 P1400-BUILD-OFF-BOARD.                                           15690000
157000*                                                                 15700000
157100     SET FICA-SCROLL-OFF-BD-ORD TO TRUE                           15710000
157200     IF PFKEY8 AND                                                15720000
157300        FICA-SCROLL-KEY > SPACES                                  15730000
157400         MOVE FICA-SCROLL-KEY TO EBPOS                            15740000
157500     ELSE                                                         15750000
157600        SET ENTER-KEY TO TRUE                                     15760000
157700        MOVE ZEROS              TO FICA-HOLD-POS                  15770000
157800        MOVE SPACES             TO EBPOS-AREA                     15780000
157900        MOVE FICT-JOB-DIST      TO DIST OF WS-EXTRA-BOARD         15790000
158000        MOVE FICT-JOB-SUB-DIST  TO SUB-DIST OF WS-EXTRA-BOARD     15800000
158100        MOVE FICT-JOB-GRP-PL-XB TO CRAFT-CODE OF WS-EXTRA-BOARD   15810000
158200        MOVE ZEROS              TO EB-POSITION                    15820000
158300*                                                          **PLS  15830000
158400        IF NOT FASTSLOW-XB                                        15840000
158500           IF FICT-YARD-BOARD                                     15850000
158600              MOVE '1' TO EB-POS-BOARD                            15860000
158700           ELSE                                                   15870000
158800              IF FICT-ROAD-BOARD                                  15880000
158900                 MOVE '2' TO EB-POS-BOARD                         15890000
159000              END-IF                                              15900000
159100           END-IF                                                 15910000
159200        END-IF                                                    15920000
159300        MOVE EBPOS-AREA TO EBPOS                                  15930000
159400     END-IF                                                       15940000
159500     MOVE SPACES TO FICA-SCROLL-KEY                               15950000
159600     EXEC CICS STARTBR                                            15960000
159700               DATASET(EB-VIA-CRAFT-POSITION)                     15970000
159800               RIDFLD(EBPOS)                                      15980000
159900               GTEQ                                               15990000
160000               RESP(WS-RESPONSE)                                  16000000
160100     END-EXEC                                                     16010000
160200     MOVE WS-RESPONSE TO FILE-STATUS                              16020000
160300     IF SUCCESS                                                   16030000
160400        IF PFKEY8                                                 16040000
160500           EXEC CICS READNEXT                                     16050000
160600                     DATASET(EB-VIA-CRAFT-POSITION)               16060000
160700                     INTO(WS-EXTRA-BOARD)                         16070000
160800                     LENGTH(EBCRPOS-RLGTH)                        16080000
160900                     RIDFLD(EBPOS)                                16090000
161000                     KEYLENGTH(EBCRPOS-KLGTH)                     16100000
161100                     RESP(WS-RESPONSE)                            16110000
161200           END-EXEC                                               16120000
161300           MOVE WS-RESPONSE TO FILE-STATUS                        16130000
161400        END-IF                                                    16140000
161500     END-IF                                                       16150000
161600     IF SUCCESS                                                   16160000
161700        MOVE '0' TO DONE-CODE                                     16170000
161800        ADD 1 TO TURN-SUB                                         16180000
161900        PERFORM UNTIL DONE                                        16190000
162000           EXEC CICS READNEXT                                     16200000
162100                     DATASET(EB-VIA-CRAFT-POSITION)               16210000
162200                     INTO(WS-EXTRA-BOARD)                         16220000
162300                     LENGTH(EBCRPOS-RLGTH)                        16230000
162400                     RIDFLD(EBPOS)                                16240000
162500                     KEYLENGTH(EBCRPOS-KLGTH)                     16250000
162600                     RESP(WS-RESPONSE)                            16260000
162700           END-EXEC                                               16270000
162800           MOVE WS-RESPONSE TO FILE-STATUS                        16280000
162900           IF SUCCESS                                             16290000
163000*                                                          **PLS  16300000
163100              IF DIST OF WS-EXTRA-BOARD = FICT-JOB-DIST           16310000
163200                AND SUB-DIST OF WS-EXTRA-BOARD = FICT-JOB-SUB-DIST16320000
163300                AND CRAFT-CODE OF WS-EXTRA-BOARD                  16330000
163400                                             = FICT-JOB-GRP-PL-XB 16340000
163500                AND ((FICT-YARD-BOARD AND EB-YARD-BOARD)          16350000
163600                OR (FICT-ROAD-BOARD AND EB-ROAD-BOARD)            16360000
163700                OR (FICT-BOARD > SPACE AND FASTSLOW-XB)           16370000
163800                OR FICT-BOARD NOT > SPACE)                        16380000
163900                 IF (EB-OFF-BOARD OR (EB-SLOW-OFF-BOARD           16390000
164000                                             AND FASTSLOW-XB))    16400000
164100                   IF TURN-SUB NOT > ARRAY-MAX                    16410000
164200                      IF TURN-SUB = ARRAY-MAX                     16420000
164300                         MOVE EBPOS-AREA TO FICA-SCROLL-KEY       16430000
164400                      END-IF                                      16440000
164500                      MOVE ZERO     TO ASGN-EMP-NO                16450000
164600                                       GOT-EMPLOYEE-FLAG          16460000
164700                      MOVE SPACES   TO WS-MSTR                    16470000
164800                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            16480000
164900                      PERFORM PXXXX-LATEST-TEMP                   16490000
165000                      IF ASGN-EMP-NO NOT > ZERO                   16500000
165100                         PERFORM PXXXX-JOB-OWNER                  16510000
165200                      END-IF                                      16520000
165300                      IF ASGN-EMP-NO > ZERO                       16530000
165400                         MOVE ASGN-EMP-NO TO MSTRNBRK             16540000
165500                         PERFORM P8500-READ-MASTER                16550000
165600                         SET GOT-EMPLOYEE TO TRUE                 16560000
165700                      END-IF                                      16570000
165800                      PERFORM P1600-SETUP-OFF-BD-SORT             16580000
165900                   ELSE                                           16590000
166000                      SET DONE TO TRUE                            16600000
166100                      PERFORM P1650-SORT-OFF-BD-RECORDS           16610000
166200                   END-IF                                         16620000
166300                 END-IF                                           16630000
166400              ELSE                                                16640000
166500                 SET DONE TO TRUE                                 16650000
166600                 PERFORM P1650-SORT-OFF-BD-RECORDS                16660000
166700              END-IF                                              16670000
166800           ELSE                                                   16680000
166900              SET DONE TO TRUE                                    16690000
167000              IF NOT (NO-RECORD-FND OR END-OF-FILE)               16700000
167100                 MOVE 'P1400-1' TO ERR-PARAGRAPH                  16710000
167200                 MOVE EBPOS     TO ERR-KEY                        16720000
167300                 PERFORM P9999-GOT-PROBLEM                        16730000
167400              END-IF                                              16740000
167500              PERFORM P1650-SORT-OFF-BD-RECORDS                   16750000
167600           END-IF                                                 16760000
167700        END-PERFORM                                               16770000
167800        EXEC CICS ENDBR                                           16780000
167900                  DATASET(EB-VIA-CRAFT-POSITION)                  16790000
168000                  RESP(WS-RESPONSE)                               16800000
168100        END-EXEC                                                  16810000
168200     ELSE                                                         16820000
168300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     16830000
168400           MOVE 'P1400-2' TO ERR-PARAGRAPH                        16840000
168500           MOVE EBTURN    TO ERR-KEY                              16850000
168600           PERFORM P9999-GOT-PROBLEM                              16860000
168700        END-IF                                                    16870000
168800     END-IF.                                                      16880000
168900*                                                                 16890000
169000 P1450-BUILD-SLOW-OFF-BOARD.                                      16900000
169100*                                                          **PLS  16910000
169200     SET FICA-SCROLL-OFF-BD-ORD TO TRUE                           16920000
169300     IF PFKEY8 AND                                                16930000
169400        FICA-SCROLL-KEY > SPACES                                  16940000
169500         MOVE FICA-SCROLL-KEY TO EBPOS                            16950000
169600     ELSE                                                         16960000
169700        SET ENTER-KEY TO TRUE                                     16970000
169800        MOVE ZEROS              TO FICA-HOLD-POS                  16980000
169900        MOVE SPACES             TO EB-SLOW-POS-AREA               16990000
170000        MOVE FICT-JOB-DIST      TO EB-SLOW-DIST                   17000000
170100        MOVE FICT-JOB-SUB-DIST  TO EB-SLOW-SUB-DIST               17010000
170200        MOVE FICT-JOB-GRP-PL-XB TO EB-SLOW-CRAFT-CODE             17020000
170300        MOVE ZEROS              TO EB-SLOW-POSITION               17030000
170400        MOVE EB-SLOW-POS-AREA TO EBSLOW-POS                       17040000
170500     END-IF                                                       17050000
170600     MOVE SPACES TO FICA-SCROLL-KEY                               17060000
170700     EXEC CICS STARTBR                                            17070000
170800               DATASET(EB-VIA-SLOW-POSITION)                      17080000
170900               RIDFLD(EBSLOW-POS)                                 17090000
171000               GTEQ                                               17100000
171100               RESP(WS-RESPONSE)                                  17110000
171200     END-EXEC                                                     17120000
171300     MOVE WS-RESPONSE TO FILE-STATUS                              17130000
171400     IF SUCCESS                                                   17140000
171500        IF PFKEY8                                                 17150000
171600           EXEC CICS READNEXT                                     17160000
171700                     DATASET(EB-VIA-SLOW-POSITION)                17170000
171800                     INTO(WS-EXTRA-BOARD)                         17180000
171900                     LENGTH(EBSLPOS-RLGTH)                        17190000
172000                     RIDFLD(EBSLOW-POS)                           17200000
172100                     KEYLENGTH(EBSLPOS-KLGTH)                     17210000
172200                     RESP(WS-RESPONSE)                            17220000
172300           END-EXEC                                               17230000
172400           MOVE WS-RESPONSE TO FILE-STATUS                        17240000
172500        END-IF                                                    17250000
172600     END-IF                                                       17260000
172700     IF SUCCESS                                                   17270000
172800        MOVE '0' TO DONE-CODE                                     17280000
172900        ADD 1 TO TURN-SUB                                         17290000
173000        PERFORM UNTIL DONE                                        17300000
173100           EXEC CICS READNEXT                                     17310000
173200                     DATASET(EB-VIA-SLOW-POSITION)                17320000
173300                     INTO(WS-EXTRA-BOARD)                         17330000
173400                     LENGTH(EBSLPOS-RLGTH)                        17340000
173500                     RIDFLD(EBSLOW-POS)                           17350000
173600                     KEYLENGTH(EBSLPOS-KLGTH)                     17360000
173700                     RESP(WS-RESPONSE)                            17370000
173800           END-EXEC                                               17380000
173900           MOVE WS-RESPONSE TO FILE-STATUS                        17390000
174000           IF SUCCESS                                             17400000
174100*                                                          **PLS  17410000
174200              IF EB-SLOW-DIST = FICT-JOB-DIST                     17420000
174300                AND EB-SLOW-SUB-DIST = FICT-JOB-SUB-DIST          17430000
174400                AND EB-SLOW-CRAFT-CODE = FICT-JOB-GRP-PL-XB       17440000
174500                AND (FICT-BOARD > SPACE AND FASTSLOW-XB)          17450000
174600                IF EB-SLOW-OFF-BOARD                              17460000
174700                   IF TURN-SUB NOT > ARRAY-MAX                    17470000
174800                      IF TURN-SUB = ARRAY-MAX                     17480000
174900                         MOVE EB-SLOW-POS-AREA TO FICA-SCROLL-KEY 17490000
175000                      END-IF                                      17500000
175100                      MOVE ZERO     TO ASGN-EMP-NO                17510000
175200                                       GOT-EMPLOYEE-FLAG          17520000
175300                      MOVE SPACES   TO WS-MSTR                    17530000
175400                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            17540000
175500                      PERFORM PXXXX-LATEST-TEMP                   17550000
175600                      IF ASGN-EMP-NO NOT > ZERO                   17560000
175700                         PERFORM PXXXX-JOB-OWNER                  17570000
175800                      END-IF                                      17580000
175900                      IF ASGN-EMP-NO > ZERO                       17590000
176000                         MOVE ASGN-EMP-NO TO MSTRNBRK             17600000
176100                         PERFORM P8500-READ-MASTER                17610000
176200                         SET GOT-EMPLOYEE TO TRUE                 17620000
176300                      END-IF                                      17630000
176400                      PERFORM P1600-SETUP-OFF-BD-SORT             17640000
176500                   ELSE                                           17650000
176600                      SET DONE TO TRUE                            17660000
176700                      PERFORM P1650-SORT-OFF-BD-RECORDS           17670000
176800                   END-IF                                         17680000
176900                END-IF                                            17690000
177000              ELSE                                                17700000
177100                 SET DONE TO TRUE                                 17710000
177200                 PERFORM P1650-SORT-OFF-BD-RECORDS                17720000
177300              END-IF                                              17730000
177400           ELSE                                                   17740000
177500              SET DONE TO TRUE                                    17750000
177600              IF NOT (NO-RECORD-FND OR END-OF-FILE)               17760000
177700                 MOVE 'P1450-1' TO ERR-PARAGRAPH                  17770000
177800                 MOVE EBPOS     TO ERR-KEY                        17780000
177900                 PERFORM P9999-GOT-PROBLEM                        17790000
178000              END-IF                                              17800000
178100              PERFORM P1650-SORT-OFF-BD-RECORDS                   17810000
178200           END-IF                                                 17820000
178300        END-PERFORM                                               17830000
178400        EXEC CICS ENDBR                                           17840000
178500                  DATASET(EB-VIA-SLOW-POSITION)                   17850000
178600                  RESP(WS-RESPONSE)                               17860000
178700        END-EXEC                                                  17870000
178800     ELSE                                                         17880000
178900        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     17890000
179000           MOVE 'P1450-2' TO ERR-PARAGRAPH                        17900000
179100           MOVE EBTURN    TO ERR-KEY                              17910000
179200           PERFORM P9999-GOT-PROBLEM                              17920000
179300        END-IF                                                    17930000
179400     END-IF.                                                      17940000
179500*                                                                 17950015
179600* P1400-BUILD-QUAL-BOARD AND P1450-BUILD-SLOW-QUAL-BOARD ARE ADDED17960015
179700* NEWLY WITH CNC0517. THESE PARAGRAPHS ARE CLONED FROM CNP06.     17970015
179800*                                                                 17980015
179900 P1400-BUILD-QUAL-BOARD.                                          17990015
180000*                                                                 18000015
180100     IF DAILY-MARK-YARD                                           18010015
180200        AND SWITCHMAN-GEL-BOARD                                   18020015
180300        IF WS-LOCAL-HR > 20 AND FICT-JOB-GRP-PL-XB = 'S1'         18030015
180400          ADD 1 TO DAY1                                           18040015
180500        END-IF                                                    18050015
180600        IF WS-LOCAL-HR < 07 AND FICT-JOB-GRP-PL-XB = 'S3'         18060015
180700          SUBTRACT 1 FROM DAY1                                    18070015
180800        END-IF                                                    18080015
180900        IF DAY1 = ZERO                                            18090015
181000           MOVE 7 TO DAY1                                         18100015
181100        END-IF                                                    18110015
181200        IF DAY1 = 08                                              18120015
181300           MOVE 1 TO DAY1                                         18130015
181400        END-IF                                                    18140015
181500     END-IF                                                       18150015
181600     IF PFKEY8                                                    18160015
181700        AND FICA-SCROLL-KEY > SPACES                              18170015
181800        MOVE FICA-SCROLL-KEY TO EBPOS                             18180015
181900     ELSE                                                         18190015
182000        SET ENTER-KEY TO TRUE                                     18200015
182100        MOVE ZEROS TO P02DCA-HOLD-POS                             18210016
182200        MOVE SPACES TO EBPOS-AREA                                 18220015
182300        MOVE FICT-JOB-DIST       TO DIST OF WS-EXTRA-BOARD        18230015
182400        MOVE FICT-JOB-SUB-DIST   TO SUB-DIST OF WS-EXTRA-BOARD    18240015
182500        MOVE FICT-JOB-GRP-PL-XB  TO CRAFT-CODE OF WS-EXTRA-BOARD  18250015
182600        IF DUAL-XB                                                18260015
182700           IF FICT-YARD-BOARD                                     18270015
182800              MOVE '1' TO EB-POS-BOARD                            18280015
182900           ELSE                                                   18290015
183000              IF FICT-ROAD-BOARD                                  18300015
183100                 MOVE '2' TO EB-POS-BOARD                         18310015
183200              END-IF                                              18320015
183300           END-IF                                                 18330015
183400        END-IF                                                    18340015
183500        MOVE EBPOS-AREA TO EBPOS                                  18350015
183600     END-IF                                                       18360015
183700     MOVE SPACES TO FICA-SCROLL-KEY                               18370015
183800     EXEC CICS STARTBR                                            18380015
183900               DATASET(EB-VIA-CRAFT-POSITION)                     18390015
184000               RIDFLD(EBPOS)                                      18400015
184100               GTEQ                                               18410015
184200               RESP(WS-RESPONSE)                                  18420015
184300     END-EXEC                                                     18430015
184400     MOVE WS-RESPONSE TO FILE-STATUS                              18440015
184500     IF SUCCESS                                                   18450015
184600        IF PFKEY8                                                 18460015
184700           EXEC CICS READNEXT                                     18470015
184800                     DATASET(EB-VIA-CRAFT-POSITION)               18480015
184900                     INTO(WS-EXTRA-BOARD)                         18490015
185000                     LENGTH(EBCRPOS-RLGTH)                        18500015
185100                     RIDFLD(EBPOS)                                18510015
185200                     KEYLENGTH(EBCRPOS-KLGTH)                     18520015
185300                     RESP(WS-RESPONSE)                            18530015
185400           END-EXEC                                               18540015
185500           MOVE WS-RESPONSE TO FILE-STATUS                        18550015
185600        END-IF                                                    18560015
185700     END-IF                                                       18570015
185800     IF SUCCESS                                                   18580015
185900        MOVE '0' TO DONE-CODE                                     18590015
186000        PERFORM UNTIL DONE                                        18600015
186100           EXEC CICS READNEXT                                     18610015
186200                     DATASET(EB-VIA-CRAFT-POSITION)               18620015
186300                     INTO(WS-EXTRA-BOARD)                         18630015
186400                     LENGTH(EBCRPOS-RLGTH)                        18640015
186500                     RIDFLD(EBPOS)                                18650015
186600                     KEYLENGTH(EBCRPOS-KLGTH)                     18660015
186700                     RESP(WS-RESPONSE)                            18670015
186800           END-EXEC                                               18680015
186900           MOVE WS-RESPONSE TO FILE-STATUS                        18690015
187000*                                                          **PLS  18700015
187100           IF SUCCESS                                             18710015
187200              IF DIST OF WS-EXTRA-BOARD = FICT-JOB-DIST           18720015
187300                AND SUB-DIST OF WS-EXTRA-BOARD = FICT-JOB-SUB-DIST18730015
187400                AND CRAFT-CODE OF WS-EXTRA-BOARD =                18740015
187500                                                FICT-JOB-GRP-PL-XB18750015
187600                AND ((FICT-YARD-BOARD AND EB-YARD-BOARD)          18760015
187700                OR (FICT-ROAD-BOARD AND EB-ROAD-BOARD)            18770015
187800                OR (FICT-BOARD > SPACE AND FASTSLOW-XB)           18780015
187900                OR FICT-BOARD NOT > SPACE)                        18790015
188000                 MOVE SPACES       TO TZ-PARAMETERS               18800015
188100                 SET TZ-IN-SYSTEM-ZONE TO TRUE                    18810015
188200                 MOVE EB-POS-DATE-TIME TO TZ-IN-DATE-TIME         18820015
188300                 MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE              18830015
188400                 PERFORM P8996-TIMEZONE                           18840015
188500                 MOVE TZ-OUT-DATE-TIME-CENT TO                    18850015
188600                      WS-POS-CENT-DATE-TIME-TZ                    18860015
188700                                                                  18870015
188800                 IF (WS-POS-CENT-DATE-TIME-TZ <=                  18880015
188900                    WS-VIEW-DATE-TIME-CENT                        18890015
189000                    OR WS-SYSTEM-DATE = '991231' OR '000101')     18900015
189100                    AND ((NOT FASTSLOW-XB AND EB-ON-BOARD) OR     18910015
189200                         (FASTSLOW-XB AND EB-ON-BOARD AND         18920015
189300                                     EB-SLOW-ON-BOARD))           18930015
189400                    ADD 1 TO TURN-SUB                             18940015
189500                    IF TURN-SUB NOT > ARRAY-MAX                   18950015
189600                      IF TURN-SUB = ARRAY-MAX                     18960015
189700                          MOVE EBPOS-AREA TO FICA-SCROLL-KEY      18970015
189800                       END-IF                                     18980015
189900                       MOVE ZERO     TO ASGN-EMP-NO               18990015
190000                                        GOT-EMPLOYEE-FLAG         19000015
190100                       MOVE SPACES   TO WS-MSTR                   19010015
190200                       MOVE TURN-NBR TO WK-ASGN-XB-TURN           19020015
190300                       PERFORM PXXXX-LATEST-TEMP                  19030015
190400                       IF ASGN-EMP-NO NOT > ZERO                  19040015
190500                          PERFORM PXXXX-JOB-OWNER                 19050015
190600                       END-IF                                     19060015
190700                       IF ASGN-EMP-NO > ZERO                      19070015
190800                          MOVE ASGN-EMP-NO TO MSTRNBRK            19080015
190900                          PERFORM P8500-READ-MASTER               19090015
191000                          SET GOT-EMPLOYEE TO TRUE                19100015
191100                       END-IF                                     19110015
191200                       IF GOT-EMPLOYEE                            19120015
191300                          PERFORM P1505-CALL-XB-POS-PARMS         19130015
191400*ET                                                               19140015
191500*ET ENHANCED TRACKING REQUIRES THAT ALL EMPLOYEES RETAIN THEIR    19150015
191600*ET POSITIONS. HOWEVER, IF THE DISPLAY W/O TRACKING FLAGS DO      19160015
191700*ET NOT CONTAIN THEIR STATUS CODE, THEY WILL NOT DISPLAY ON       19170015
191800*ET THE P BOARD, BUT WILL ON THE A BOARD                          19180015
191900*ET                                                               19190015
192000*ET NOTE: CN DOES WANT TO SEE EMPLOYEES ON THEIR REST DAYS ON     19200015
192100*ET       BOTH BOARDS, REGARDLESS OF WHAT PETE SAYS. NJB          19210015
192200*ET                                                               19220015
192300                          SET DISPLAY-EMP TO TRUE                 19230015
192400                          IF AVAILABLE OR WORKING                 19240015
192500                             CONTINUE                             19250015
192600                          ELSE                                    19260015
192700                             IF NOT FICT-ALL-POSITIONS            19270015
192800                                MOVE '0'      TO DISPLAY-EMP-FLAG 19280015
192900                                PERFORM P9830-RETRIEVE-CNTL-INFO  19290015
193000                             END-IF                               19300015
193100                          END-IF                                  19310015
193200                          IF DISPLAY-EMP                          19320015
193300                             MOVE TURN-SUB TO NAME-SUB            19330015
193400                             PERFORM P1500-SETUP-NAME-LINE        19340015
193500                          ELSE                                    19350015
193600                             SUBTRACT 1 FROM TURN-SUB             19360015
193700                          END-IF                                  19370015
193800                       ELSE                                       19380015
193900                          SUBTRACT 1 FROM TURN-SUB                19390015
194000                       END-IF                                     19400015
194100                    ELSE                                          19410015
194200                       SET DONE TO TRUE                           19420015
194300                    END-IF                                        19430015
194400                 END-IF                                           19440015
194500              ELSE                                                19450015
194600                 SET DONE TO TRUE                                 19460015
194700              END-IF                                              19470015
194800           ELSE                                                   19480015
194900              SET DONE TO TRUE                                    19490015
195000              IF NOT (NO-RECORD-FND OR END-OF-FILE)               19500015
195100                 MOVE 'P1400X-1' TO ERR-PARAGRAPH                 19510015
195200                 MOVE EBPOS     TO ERR-KEY                        19520015
195300                 PERFORM P9999-GOT-PROBLEM                        19530015
195400              END-IF                                              19540015
195500           END-IF                                                 19550015
195600        END-PERFORM                                               19560015
195700        EXEC CICS ENDBR                                           19570015
195800                  DATASET(EB-VIA-CRAFT-POSITION)                  19580015
195900                  RESP(WS-RESPONSE)                               19590015
196000        END-EXEC                                                  19600015
196100     ELSE                                                         19610015
196200        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     19620015
196300           MOVE 'P1400X-2' TO ERR-PARAGRAPH                       19630015
196400           MOVE EBTURN    TO ERR-KEY                              19640015
196500           PERFORM P9999-GOT-PROBLEM                              19650015
196600        END-IF                                                    19660015
196700     END-IF.                                                      19670015
196800*                                                                 19680015
196900* P1400-BUILD-QUAL-BOARD AND P1450-BUILD-SLOW-QUAL-BOARD ARE ADDED19690015
197000* NEWLY WITH CNC0517. THESE PARAGRAPHS ARE CLONED FROM CNP06.     19700015
197100*                                                                 19710015
197200 P1450-BUILD-SLOW-QUAL-BOARD.                                     19720015
197300*                                                                 19730015
197400*  USE THE SLOW-POSITION KEY TO SET UP THE POSITION BOARD,        19740015
197500*  IF IT'S A FASTSLOW BOARD, AND "R" (ROAD) WAS REQUESTED.        19750015
197600*                                                          **PLS  19760015
197700     IF DAILY-MARK-YARD                                           19770015
197800        AND SWITCHMAN-GEL-BOARD                                   19780015
197900        IF WS-LOCAL-HR > 20 AND FICT-JOB-GRP-PL-XB = 'S1'         19790015
198000          ADD 1 TO DAY1                                           19800015
198100        END-IF                                                    19810015
198200        IF WS-LOCAL-HR < 07 AND FICT-JOB-GRP-PL-XB = 'S3'         19820015
198300          SUBTRACT 1 FROM DAY1                                    19830015
198400        END-IF                                                    19840015
198500        IF DAY1 = ZERO                                            19850015
198600           MOVE 7 TO DAY1                                         19860015
198700        END-IF                                                    19870015
198800        IF DAY1 = 08                                              19880015
198900           MOVE 1 TO DAY1                                         19890015
199000        END-IF                                                    19900015
199100     END-IF                                                       19910015
199200     IF PFKEY8                                                    19920015
199300        AND FICA-SCROLL-KEY > SPACES                              19930015
199400        MOVE FICA-SCROLL-KEY TO EBSLOW-POS                        19940015
199500     ELSE                                                         19950015
199600        SET ENTER-KEY TO TRUE                                     19960015
199700        MOVE ZEROS TO P02DCA-HOLD-POS                             19970016
199800        MOVE SPACES TO EBSLOW-POS                                 19980015
199900        MOVE FICT-JOB-DIST        TO DIST       OF EBSLOW-POS     19990015
200000        MOVE FICT-JOB-SUB-DIST    TO SUBDIST    OF EBSLOW-POS     20000015
200100        MOVE FICT-JOB-GRP-PL-XB   TO CRAFT-CODE OF EBSLOW-POS     20010015
200200        MOVE ZEROS TO EBSLOWPOS                                   20020015
200300     END-IF                                                       20030015
200400     MOVE SPACES TO FICA-SCROLL-KEY                               20040015
200500     EXEC CICS STARTBR                                            20050015
200600               DATASET(EB-VIA-SLOW-POSITION)                      20060015
200700               RIDFLD(EBSLOW-POS)                                 20070015
200800               GTEQ                                               20080015
200900               RESP(WS-RESPONSE)                                  20090015
201000     END-EXEC                                                     20100015
201100     MOVE WS-RESPONSE TO FILE-STATUS                              20110015
201200     IF SUCCESS                                                   20120015
201300        IF PFKEY8                                                 20130015
201400           EXEC CICS READNEXT                                     20140015
201500                     DATASET(EB-VIA-SLOW-POSITION)                20150015
201600                     INTO(WS-EXTRA-BOARD)                         20160015
201700                     LENGTH(EBSLPOS-RLGTH)                        20170015
201800                     RIDFLD(EBSLOW-POS)                           20180015
201900                     KEYLENGTH(EBSLPOS-KLGTH)                     20190015
202000                     RESP(WS-RESPONSE)                            20200015
202100           END-EXEC                                               20210015
202200           MOVE WS-RESPONSE TO FILE-STATUS                        20220015
202300        END-IF                                                    20230015
202400     END-IF                                                       20240015
202500     IF SUCCESS                                                   20250015
202600        MOVE '0' TO DONE-CODE                                     20260015
202700        PERFORM UNTIL DONE                                        20270015
202800           EXEC CICS READNEXT                                     20280015
202900                     DATASET(EB-VIA-SLOW-POSITION)                20290015
203000                     INTO(WS-EXTRA-BOARD)                         20300015
203100                     LENGTH(EBSLPOS-RLGTH)                        20310015
203200                     RIDFLD(EBSLOW-POS)                           20320015
203300                     KEYLENGTH(EBSLPOS-KLGTH)                     20330015
203400                     RESP(WS-RESPONSE)                            20340015
203500           END-EXEC                                               20350015
203600           MOVE WS-RESPONSE TO FILE-STATUS                        20360015
203700           IF SUCCESS                                             20370015
203800              IF DIST OF WS-EXTRA-BOARD = FICT-JOB-DIST           20380015
203900                AND SUB-DIST OF WS-EXTRA-BOARD = FICT-JOB-SUB-DIST20390015
204000                AND CRAFT-CODE OF WS-EXTRA-BOARD =                20400015
204100                                                FICT-JOB-GRP-PL-XB20410015
204200                 AND ((FICT-YARD-BOARD AND EB-YARD-BOARD)         20420015
204300                 OR (FICT-ROAD-BOARD AND EB-ROAD-BOARD)           20430015
204400                 OR (FICT-BOARD > SPACE AND FASTSLOW-XB)          20440015
204500                 OR FICT-BOARD NOT > SPACE)                       20450015
204600                 MOVE SPACES       TO TZ-PARAMETERS               20460015
204700                 SET TZ-IN-SYSTEM-ZONE TO TRUE                    20470015
204800                 MOVE EB-SLOW-POS-DATE-TIME TO TZ-IN-DATE-TIME    20480015
204900                 MOVE PSTCA-TIME-ZONE TO TZ-OUT-ZONE              20490015
205000                 PERFORM P8996-TIMEZONE                           20500015
205100*                MOVE TZ-OUT-DATE-TIME TO WS-POS-DATE-TIME-TZ     20510015
205200                 MOVE TZ-OUT-DATE-TIME-CENT TO                    20520015
205300                      WS-POS-CENT-DATE-TIME-TZ                    20530015
205400*-------------------------------------------------------------    20540015
205500*       WHEN DISPLAYING THE SLOW SIDE OF A FASTSLOW BOARD,        20550015
205600*       VERIFY THE ON-BOARD STATUS OF THE SLOW BOARD ONLY.        20560015
205700*       IF 'OFF' ON THE FAST(YARD) SIDE, CONSIDER THE TURN        20570015
205800*       AS ON THE BOARD FOR THE SLOW SIDE.                        20580015
205900*----------------------------------------------------------PLS    20590015
206000*                IF WS-POS-DATE-TIME-TZ <= WS-VIEW-DATE-TIME      20600015
206100                 IF (WS-POS-CENT-DATE-TIME-TZ <=                  20610015
206200                    WS-VIEW-DATE-TIME-CENT                        20620015
206300                    OR WS-SYSTEM-DATE = '991231' OR '000101')     20630015
206400                    AND EB-SLOW-ON-BOARD                          20640015
206500                    ADD 1 TO TURN-SUB                             20650015
206600                    IF TURN-SUB NOT > ARRAY-MAX                   20660015
206700                      IF TURN-SUB = ARRAY-MAX                     20670015
206800                        MOVE EB-SLOW-POS-AREA TO FICA-SCROLL-KEY  20680015
206900                      END-IF                                      20690015
207000                      MOVE ZERO     TO ASGN-EMP-NO                20700015
207100                                       GOT-EMPLOYEE-FLAG          20710015
207200                      MOVE SPACES   TO WS-MSTR                    20720015
207300                      MOVE TURN-NBR TO WK-ASGN-XB-TURN            20730015
207400                      PERFORM PXXXX-LATEST-TEMP                   20740015
207500                      IF ASGN-EMP-NO NOT > ZERO                   20750015
207600                         PERFORM PXXXX-JOB-OWNER                  20760015
207700                      END-IF                                      20770015
207800                      IF ASGN-EMP-NO > ZERO                       20780015
207900                         MOVE ASGN-EMP-NO TO MSTRNBRK             20790015
208000                         PERFORM P8500-READ-MASTER                20800015
208100                         SET GOT-EMPLOYEE TO TRUE                 20810015
208200                      END-IF                                      20820015
208300                      IF GOT-EMPLOYEE                             20830015
208400                         PERFORM P1505-CALL-XB-POS-PARMS          20840015
208500*ET                                                               20850015
208600*ET ENHANCED TRACKING REQUIRES THAT ALL EMPLOYEES RETAIN THEIR    20860015
208700*ET POSITIONS. HOWEVER, IT THE DISPLAY W/O TRACKING FLAGS DO      20870015
208800*ET NOT CONTAIN THEIR STATUS CODE, THEY WILL NOT DISPLAY ON       20880015
208900*ET THE P BOARD, BUT WILL ON THE A BOARD                          20890015
209000*ET                                                               20900015
209100*ET NOTE: CN DOES WANT TO SEE EMPLOYEES ON THEIR REST DAYS ON     20910015
209200*ET       BOTH BOARDS, REGARDLESS OF WHAT PETE SAYS. NJB          20920015
209300*ET                                                               20930015
209400                         SET DISPLAY-EMP TO TRUE                  20940015
209500                         IF AVAILABLE OR WORKING                  20950015
209600                            CONTINUE                              20960015
209700                         ELSE                                     20970015
209800                            IF NOT FICT-ALL-POSITIONS             20980015
209900                               MOVE '0'        TO DISPLAY-EMP-FLAG20990015
210000                               PERFORM P9830-RETRIEVE-CNTL-INFO   21000015
210100*                              PERFORM VARYING SUB1 FROM 1 BY 1   21010015
210200*                                 UNTIL SUB1 > 5                  21020015
210300*                                 IF CNTL-XB-RETAIN-POS(SUB1) =   21030015
210400*                                    (LAYOFF-CODE-1 OR '*')       21040015
210500*                                    SET DISPLAY-EMP TO TRUE      21050015
210600*                                 END-IF                          21060015
210700*                              END-PERFORM                        21070015
210800                            END-IF                                21080015
210900                         END-IF                                   21090015
211000                         IF DISPLAY-EMP                           21100015
211100                            MOVE TURN-SUB TO NAME-SUB             21110015
211200                            PERFORM P1500-SETUP-NAME-LINE         21120015
211300                         ELSE                                     21130015
211400                            SUBTRACT 1 FROM TURN-SUB              21140015
211500                         END-IF                                   21150015
211600                      ELSE                                        21160015
211700                         SUBTRACT 1 FROM TURN-SUB                 21170015
211800                      END-IF                                      21180015
211900                   ELSE                                           21190015
212000                      SET DONE TO TRUE                            21200015
212100                   END-IF                                         21210015
212200                END-IF                                            21220015
212300              ELSE                                                21230015
212400                 SET DONE TO TRUE                                 21240015
212500              END-IF                                              21250015
212600           ELSE                                                   21260015
212700              SET DONE TO TRUE                                    21270015
212800              IF NOT (NO-RECORD-FND OR END-OF-FILE)               21280015
212900                 MOVE 'P1450X-1' TO ERR-PARAGRAPH                 21290015
213000                 MOVE EBPOS     TO ERR-KEY                        21300015
213100                 PERFORM P9999-GOT-PROBLEM                        21310015
213200              END-IF                                              21320015
213300           END-IF                                                 21330015
213400        END-PERFORM                                               21340015
213500        EXEC CICS ENDBR                                           21350015
213600                  DATASET(EB-VIA-SLOW-POSITION)                   21360015
213700                  RESP(WS-RESPONSE)                               21370015
213800        END-EXEC                                                  21380015
213900     ELSE                                                         21390015
214000        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     21400015
214100           MOVE 'P1450X-2' TO ERR-PARAGRAPH                       21410015
214200           MOVE EBTURN    TO ERR-KEY                              21420015
214300           PERFORM P9999-GOT-PROBLEM                              21430015
214400        END-IF                                                    21440015
214500     END-IF.                                                      21450015
214600                                                                  21460015
214700*                                                                 21470000
214800 P1500-SETUP-NAME-LINE.                                           21480000
214900*                                                                 21490000
215000     IF GOT-EMPLOYEE                                              21500000
215100        IF TEMPORARY-ASGNMT > SPACE                               21510000
215200            AND TEMP-ASGN-XB-AUG                                  21520000
215300            AND TA-DIST = DIST-REPEAT                             21530000
215400            AND TA-SUB-DIST = SUBDIST-REPEAT                      21540000
215500            AND TA-XB-TURN = TURN-NBR OF WS-EXTRA-BOARD           21550000
215600            AND TA-CC = CRAFT-CODE-REPEAT                         21560000
215700            MOVE EMP-NAME OF WS-MSTR TO WS-FORMAT-NAME-AUG        21570000
215800            MOVE ' /AUG' TO WS-FORMAT-NAME-AUG-FIELD              21580000
215900            MOVE WS-FORMAT-NAME-AUG TO SCR02D-NAME(NAME-SUB)      21590000
216000         ELSE                                                     21600000
216100            IF PROTECTION-IS-IN-EFFECT                            21610000
216200               MOVE SPACES TO PROTECTED-EMPLOYEE-PARMS            21620000
216300               MOVE EMP-NBR OF WS-MSTR TO PROT-EMP-NUMBER-PARM    21630000
216400               EXEC CICS LINK                                     21640000
216500                         PROGRAM(P907-PGM)                        21650000
216600                         COMMAREA(PROTECTED-EMPLOYEE-PARMS)       21660000
216700                         LENGTH(P907-LGTH)                        21670000
216800                         RESP(WS-RESPONSE)                        21680000
216900               END-EXEC                                           21690000
217000               MOVE WS-RESPONSE TO FILE-STATUS                    21700000
217100               IF NOT SUCCESS                                     21710000
217200                  MOVE 'P1500-1' TO ERR-PARAGRAPH                 21720000
217300                  PERFORM P9999-GOT-PROBLEM                       21730000
217400               END-IF                                             21740000
217500            END-IF                                                21750000
217600            IF EMPLOYEE-IS-PROTECTED                              21760000
217700               MOVE EMP-NAME OF WS-MSTR                           21770000
217800                             TO WS-FORMAT-NAME-PROT               21780000
217900               MOVE ' /PROT' TO WS-FORMAT-NAME-PROT-FIELD         21790000
218000               MOVE WS-FORMAT-NAME-PROT                           21800000
218100                             TO SCR02D-NAME(NAME-SUB)             21810000
218200            ELSE                                                  21820000
218300               MOVE EMP-NAME OF WS-MSTR                           21830000
218400                             TO SCR02D-NAME(NAME-SUB)             21840000
218500            END-IF                                                21850000
218600         END-IF                                                   21860000
218700     ELSE                                                         21870000
218800        IF PSTCA-SUB = 2                                          21880000
218900           MOVE '   VACANT TOUR            ' TO                   21890000
219000                                            SCR02D-NAME(NAME-SUB) 21900000
219100        ELSE                                                      21910000
219200           MOVE '   OPEN TURN              ' TO                   21920000
219300                                            SCR02D-NAME(NAME-SUB) 21930000
219400        END-IF                                                    21940000
219500     END-IF                                                       21950000
219600     MOVE TURN-NBR OF WS-EXTRA-BOARD TO SCR02D-TURN(NAME-SUB)     21960000
219700     PERFORM P1505-CALL-XB-POS-PARMS                              21970000
219800                                                                  21980000
219900     IF FICT-POSITION-ORDER                                       21990000
220000        MOVE SPACES TO WS-VARIABLE-LINE-1                         22000000
220100        ADD 1 TO FICA-HOLD-POS                                    22010000
220200        MOVE FICA-HOLD-POS TO WS-VL1-POS                          22020000
220300***     IF TAG-XB                                                 22030022
220400***        AND TAG-YOUR-IT                                        22040022
220500***        MOVE EB-TAG TO WS-VL1-TAG                              22050022
220600***     END-IF                                                    22060022
220700        IF GOT-EMPLOYEE                                           22070000
220800           IF TEMPORARY-ASGNMT > SPACE                            22080000
220900              MOVE 'TV'    TO WS-VL1-LO                           22090000
221000           ELSE                                                   22100000
221100              IF NOT AVAILABLE                                    22110000
221110*CNC0573 - BEG                                                    22111029
221111                 IF  PSTCA-FROM-FLD-MENU                          22111132
221112                 AND PSTCA-FLD-MENU-OPT NOT = '004'               22111232
221113                    PERFORM P9830-RETRIEVE-CNTL-INFO              22111331
221114                    IF P956-MASK-FLD-SCR-YES                      22111431
221115*CNC0576 - BEG                                                    22111533
221117                       MOVE '**'               TO WS-VL1-LO       22111733
221119                       MOVE '**'               TO WS-VL1-RSN      22111933
221120*CNC0576 - END                                                    22112033
221121                    ELSE                                          22112131
221122*CNC0576 - BEG                                                    22112233
221123                       IF P956-MASK-HOLD-TURN                     22112333
221124                          MOVE '**'            TO WS-VL1-LO       22112433
221125                          MOVE 'HT'            TO WS-VL1-RSN      22112533
221126                       ELSE                                       22112633
221127*CNC0576 - END                                                    22112733
221128                          MOVE LAYOFF-CODE     TO WS-VL1-LO       22112833
221129                          IF P956-SHOW-RSN-ON-SCR-YES             22112933
221130                             MOVE LAYOFF-EM-CODE                  22113033
221131                                               TO WS-VL1-RSN      22113133
221132                          ELSE                                    22113233
221133                             MOVE SPACES       TO WS-VL1-RSN      22113333
221134                          END-IF                                  22113433
221135*CNC0576 - BEG                                                    22113533
221136                       END-IF                                     22113633
221137*CNC0576 - END                                                    22113733
221138                    END-IF                                        22113833
221139                 ELSE                                             22113933
221140                    MOVE LAYOFF-CODE           TO WS-VL1-LO       22114033
221220                    IF LAYOFF-EM-CODE > SPACES                    22122031
221230                       PERFORM P9830-RETRIEVE-CNTL-INFO           22123031
221240                       IF P956-SHOW-RSN-ON-SCR-YES                22124029
221270                          MOVE LAYOFF-EM-CODE  TO WS-VL1-RSN      22127029
221292                       ELSE                                       22129229
221293                          MOVE SPACES          TO WS-VL1-RSN      22129329
221294                       END-IF                                     22129429
221296                    END-IF                                        22129631
221299                 END-IF                                           22129929
221300*CNC0573 - END                                                    22130029
221310              ELSE                                                22131000
221400                 IF OUT-TOWN                                      22140000
221500                    MOVE 'OT' TO WS-VL1-LO                        22150000
221600                 END-IF                                           22160000
221700              END-IF                                              22170000
221800           END-IF                                                 22180000
221900***   CNC0021/CNC0022 CW0896 SET RETURN DATE FOR OFF-MILES AND    22190000
222000***   VACATIONS.                                                  22200000
222100           INITIALIZE WS-RETURN-DATE                              22210000
222200           EVALUATE TRUE                                          22220000
222300              WHEN OFF-MILES-DAYS                                 22230000
222400                   PERFORM P9830-RETRIEVE-CNTL-INFO               22240002
222500                   IF P956-ST-RSN-NBR-DAYS-REQ                    22250002
222600                   OR P956-ST-RSN-EXP-DATE-REQ                    22260002
222700                      PERFORM P1501-GET-DUEBACK-DATE              22270002
222800                      IF WS-DUEBACK-FOUND-Y                       22280002
222900                         MOVE TASK-LO-EXP-TIME                    22290002
223000                                          TO WS-VL1-RETURN-DATE   22300002
223100                         MOVE TASK-LO-EXP-DATE(5:2)               22310002
223200                                          TO WS-VL1-RETURN-DY     22320002
223300                      END-IF                                      22330002
223400                   ELSE                                           22340002
223500                      PERFORM P4150-OFF-MILES-RETURN-DATE         22350002
223600                      MOVE WS-RETURN-DATE TO WS-VL1-RETURN-DATE   22360002
223700                   END-IF                                         22370002
223800              WHEN VACATION                                       22380000
223900                   PERFORM P4170-VACATION-RETURN-DATE             22390000
224000                   MOVE WS-RETURN-DATE TO WS-VL1-RETURN-DATE      22400000
224100              WHEN EXCUSED-ABSENCE                                22410000
224200               AND LAYOFF-EM-CODE = '69'                          22420000
224300                   PERFORM P1501-GET-DUEBACK-DATE                 22430000
224400                   IF WS-DUEBACK-FOUND-Y                          22440000
224410*CNC0556<<                                                        22441024
224411***                   MOVE TASK-LO-EXP-TIME TO WS-VL1-RETURN-DATE 22441124
224412***                   MOVE TASK-LO-EXP-DATE(5:2)                  22441224
224413***                                         TO WS-VL1-RETURN-DY   22441324
224414                      IF TASK-DUE-BACK                            22441425
224415                         CONTINUE                                 22441525
224416                      ELSE                                        22441625
224420                         IF TASK-LO-EXP-DATE-TIME > SPACES        22442025
224430                            MOVE TASK-LO-EXP-TIME                 22443025
224431                                        TO WS-VL1-RETURN-DATE     22443124
224440                            MOVE TASK-LO-EXP-DATE(5:2)            22444025
224450                                        TO WS-VL1-RETURN-DY       22445024
224470                         ELSE                                     22447025
224492                            IF EFFECTIVE-DATE-TIME-CENT > ZEROES  22449224
224493                               SET TZ-IN-EASTERN-ZONE   TO TRUE   22449324
224494                               MOVE EFFECTIVE-DATE-TIME           22449424
224495                                              TO TZ-IN-DATE-TIME  22449524
224496                               MOVE TASK-TIME-ZONE                22449624
224497                                              TO TZ-OUT-ZONE      22449724
224498                               PERFORM P8996-TIMEZONE             22449824
224499                               MOVE TZ-OUT-TIME                   22449924
224500                                        TO WS-VL1-RETURN-DATE     22450024
224501                               MOVE TZ-OUT-DD                     22450124
224502                                        TO WS-VL1-RETURN-DY       22450224
224800                            END-IF                                22480024
224810                         END-IF                                   22481024
224820                      END-IF                                      22482024
224821*CNC0556>>                                                        22482124
224830                   END-IF                                         22483024
224900              WHEN OTHER                                          22490000
225000*CNC0492 - FOR OTHER LAYOFF CODES, GET DUEBACK/BOOK-ON DATE/TIME  22500009
225100                   IF NOT AVAILABLE  AND                          22510009
225200                      NOT WORKING    AND                          22520009
225300                      NOT TO-PLACE                                22530009
225400                      AND LAYOFF-TIME NUMERIC                     22540009
225500                      AND LAYOFF-TIME > ZERO                      22550009
225600                      PERFORM P1501-GET-DUEBACK-DATE              22560010
225700                      IF WS-DUEBACK-FOUND-Y                       22570010
225710*CNC0556<<                                                        22571024
225800***                      MOVE TASK-LO-EXP-TIME                    22580024
225900***                                         TO WS-VL1-RETURN-DATE 22590024
226000***                      MOVE TASK-LO-EXP-DATE(5:2)               22600024
226100***                                         TO WS-VL1-RETURN-DY   22610024
226101                         IF TASK-DUE-BACK                         22610125
226102                            CONTINUE                              22610225
226103                         ELSE                                     22610325
226110                            IF TASK-LO-EXP-DATE-TIME > SPACES     22611025
226120                               MOVE TASK-LO-EXP-TIME              22612025
226130                                        TO WS-VL1-RETURN-DATE     22613024
226140                               MOVE TASK-LO-EXP-DATE(5:2)         22614025
226150                                        TO WS-VL1-RETURN-DY       22615024
226160                            ELSE                                  22616025
226191                               IF EFFECTIVE-DATE-TIME-CENT > ZERO 22619124
226192                                  SET TZ-IN-EASTERN-ZONE  TO TRUE 22619224
226193                                  MOVE EFFECTIVE-DATE-TIME        22619324
226194                                              TO TZ-IN-DATE-TIME  22619424
226195                                  MOVE TASK-TIME-ZONE             22619524
226196                                              TO TZ-OUT-ZONE      22619624
226197                                  PERFORM P8996-TIMEZONE          22619724
226198                                  MOVE TZ-OUT-TIME                22619824
226199                                        TO WS-VL1-RETURN-DATE     22619924
226200                                  MOVE TZ-OUT-DD                  22620024
226201                                        TO WS-VL1-RETURN-DY       22620124
226202                               END-IF                             22620224
226203                            END-IF                                22620324
226204                         END-IF                                   22620424
226205*CNC0556>>                                                        22620524
226210                      END-IF                                      22621010
226300                   ELSE                                           22630009
226400                      CONTINUE                                    22640009
226500                   END-IF                                         22650009
226600           END-EVALUATE                                           22660000
226700*                                                                 22670000
226800           SET DE-YYMMDD-FORMAT        TO TRUE                    22680000
226900           MOVE EMP-PERS-REST-DATE     TO DE-YYMMDD               22690000
227000           PERFORM P8998-DATEEDIT                                 22700000
227100           MOVE DE-CCYYMMDD            TO DE-COMPARE1-DATE        22710000
227200           MOVE EMP-PERS-REST-TIME     TO DE-COMPARE1-TIME        22720000
227300*                                                                 22730000
227400           SET DE-YYMMDD-FORMAT        TO TRUE                    22740000
227500           MOVE EMP-MTOD-DATE          TO DE-YYMMDD               22750000
227600           PERFORM P8998-DATEEDIT                                 22760000
227700           MOVE DE-CCYYMMDD            TO DE-COMPARE2-DATE        22770000
227800           MOVE EMP-MTOD-TIME          TO DE-COMPARE2-TIME        22780000
227900                                                                  22790000
228000*                                                                 22800000
228100*          CNC0183 - MATT - 12/11/99                              22810000
228200*                                                                 22820000
228300           IF DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME       22830000
228400              MOVE DE-COMPARE2-DATE-TIME                          22840000
228500                                       TO DE-COMPARE1-DATE-TIME   22850000
228600           END-IF                                                 22860000
228700           MOVE DE-COMPARE1-DATE-TIME(3:10)                       22870000
228800                                       TO WS-REST-DATE-TIME-C     22880000
228900*                                                                 22890000
229000           IF WS-REST-DATE-TIME-C NUMERIC                         22900000
229100              SET DE-YYMMDD-FORMAT          TO TRUE               22910000
229200              MOVE WS-REST-DATE-TIME(1:6)   TO DE-YYMMDD          22920000
229300              PERFORM P8998-DATEEDIT                              22930000
229400              MOVE DE-CCYYMMDD              TO DE-COMPARE1-DATE   22940000
229500              MOVE WS-REST-DATE-TIME(7:4)   TO DE-COMPARE1-TIME   22950000
229600*             AND WS-REST-DATE-TIME > WS-LOCAL-DATE-TIME          22960000
229700              IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT  22970000
229800                 MOVE WS-REST-DATE-TIME(7:4) TO WS-VL1-REST       22980000
229900                 MOVE WS-REST-DATE-TIME(5:2) TO WS-VL1-REST-DY    22990000
230000              END-IF                                              23000000
230100           END-IF                                                 23010000
230200                                                                  23020000
230300           IF EMP-MTOD IS NUMERIC                                 23030000
230400              SET DE-YYMMDD-FORMAT          TO TRUE               23040000
230500              MOVE EMP-MTOD(1:6)            TO DE-YYMMDD          23050000
230600              PERFORM P8998-DATEEDIT                              23060000
230700              MOVE DE-CCYYMMDD              TO DE-COMPARE1-DATE   23070000
230800              MOVE EMP-MTOD(7:4)            TO DE-COMPARE1-TIME   23080000
230900*             AND EMP-MTOD > WS-LOCAL-DATE-TIME                   23090000
231000              IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT  23100000
231100                 IF EMP-PREV-DUTY-MTOD IS NUMERIC                 23110000
231200                    AND EMP-PREV-DUTY-MTOD > '0000'               23120000
231300                    MOVE EMP-PREV-DUTY-MTOD TO WS-VL1-MTPD        23130000
231400                 END-IF                                           23140000
231500              END-IF                                              23150000
231600           END-IF                                                 23160000
231700           IF EMP-US-RSTD IS NUMERIC                              23170000
231800              SET DE-YYMMDD-FORMAT          TO TRUE               23180000
231900              MOVE EMP-US-RSTD(1:6)         TO DE-YYMMDD          23190000
232000              PERFORM P8998-DATEEDIT                              23200000
232100              MOVE DE-CCYYMMDD              TO DE-COMPARE1-DATE   23210000
232200              MOVE EMP-US-RSTD(7:4)         TO DE-COMPARE1-TIME   23220000
232300*             AND EMP-US-RSTD > WS-LOCAL-DATE-TIME                23230000
232400              IF DE-COMPARE1-DATE-TIME > WS-LOCAL-DATE-TIME-CENT  23240000
232500                 MOVE EMP-US-RSTD-TIME TO WS-VL1-USHR             23250000
232600                 IF EMP-PREV-DUTY IS NUMERIC                      23260000
232700                    AND EMP-PREV-DUTY > '0000'                    23270000
232800                    MOVE EMP-PREV-DUTY TO WS-VL1-PD               23280000
232900                 END-IF                                           23290000
233000              END-IF                                              23300000
233100           END-IF                                                 23310000
233200*                                                          **PLS  23320000
233300*                                                                 23330000
233400           IF FASTSLOW-XB                                         23340000
233500              IF ((FICT-YARD-BOARD AND EB-ON-BOARD                23350000
233600                                  AND EB-SLOW-ON-BOARD) OR        23360000
233700                 (FICT-ROAD-BOARD AND EB-SLOW-ON-BOARD))          23370000
233800                AND EB-SHORT-TURN                                 23380000
233900                AND TEMPORARY-ASGNMT NOT > SPACE                  23390000
234000                AND ON-DUTY-ASGNMT NOT > SPACE                    23400000
234100                 MOVE 'ST' TO WS-VL1-SHRT                         23410000
234200              ELSE                                                23420000
234300                 IF EB-ON-BOARD                                   23430000
234400                   AND EB-SHORT-TURN                              23440000
234500                   AND TEMPORARY-ASGNMT NOT > SPACE               23450000
234600                   AND ON-DUTY-ASGNMT NOT > SPACE                 23460000
234700                     MOVE 'ST' TO WS-VL1-SHRT                     23470000
234800                 END-IF                                           23480000
234900              END-IF                                              23490000
235000           END-IF                                                 23500000
235100           IF FICT-ROAD-BOARD                                     23510000
235200*    CNC0006 - FLW, 5/8/96, START                                 23520000
235300              MOVE EB-NBR-YARD-STT-STARTS-PICX TO                 23530000
235400                                               WS-VL1-STT-STARTS  23540000
235500              MOVE EB-NBR-YARD-OVT-STARTS-PICX TO                 23550000
235600                                               WS-VL1-OVT-STARTS  23560000
235700           ELSE                                                   23570000
235800              PERFORM P1510-INQUIRE-STARTS                        23580000
235900*    CNC0006 - FLW, 5/8/96, END                                   23590000
236000           END-IF                                                 23600000
236100        END-IF                                                    23610000
236200        IF CNTL-XB-SCHEDULED                                      23620000
236300           OR CNTL-XB-EXTENDED-SCHED                              23630000
236400           PERFORM P1550-CHECK-SCHEDULE                           23640000
236500        END-IF                                                    23650000
236600        MOVE WS-VARIABLE-LINE-1 TO SCR02D-VARIABLE(NAME-SUB)      23660000
236700     ELSE                                                         23670000
236800*CNC0517 - START                                                  23680015
236900        IF FICT-QUAL-ORDER                                        23690015
237000           MOVE SPACES              TO WS-VARIABLE-LINE-4         23700017
237100           ADD 1                    TO P02DCA-HOLD-POS-NUM        23710017
237200           MOVE P02DCA-HOLD-POS-NUM TO WS-VL4-POS                 23720017
237300           IF GOT-EMPLOYEE                                        23730015
237400              PERFORM P1520-SET-SENIORITY-ARRAY                   23740015
237500              PERFORM P1540-SET-QUAL-ARRAY                        23750015
237600           END-IF                                                 23760015
237700           MOVE WS-VARIABLE-LINE-4 TO SCR02D-VARIABLE(NAME-SUB)   23770015
237800*CNC0517 - FINISH                                                 23780015
237900        ELSE                                                      23790015
238000           MOVE SPACES TO WS-VARIABLE-LINE-2                      23800015
238100           MOVE P912-POS-PARM-NUM TO WS-VL2-POS                   23810015
238200           IF DUAL-XB                                             23820015
238300              IF EB-ON-BOARD                                      23830015
238400                 IF EB-YARD-BOARD                                 23840015
238500                    MOVE 'YD' TO WS-VL2-BOARD                     23850015
238600                 END-IF                                           23860015
238700                 IF EB-ROAD-BOARD                                 23870015
238800                    MOVE 'RD' TO WS-VL2-BOARD                     23880015
238900                 END-IF                                           23890015
239000              END-IF                                              23900015
239100           END-IF                                                 23910015
239200*-------------------------------------------------------------    23920000
239300*    FOR A FASTSLOW SPAREBOARD, THE YD VS. RD FLAG IS             23930000
239400*    DETERMINED BY WHICH BOARD WAS SELECTED TO BE DISPLAYED.      23940000
239500*----------------------------------------------------------PLS    23950000
239600           IF FASTSLOW-XB                                         23960015
239700              IF FICT-YARD-BOARD                                  23970015
239800                 MOVE 'YD' TO WS-VL2-BOARD                        23980015
239900              END-IF                                              23990015
240000              IF FICT-ROAD-BOARD                                  24000015
240100                 MOVE 'RD' TO WS-VL2-BOARD                        24010015
240200              END-IF                                              24020015
240300           END-IF                                                 24030015
240400           IF GOT-EMPLOYEE                                        24040015
240500              IF TEMPORARY-ASGNMT > SPACE                         24050015
240600                  MOVE 'TV' TO WS-VL2-LO                          24060015
240700                  MOVE TA-AREA TO WS-VL2-ASGN                     24070015
240800              ELSE                                                24080015
240900                 IF NOT AVAILABLE                                 24090015
240910*CNC0573 - BEG                                                    24091031
240911                    IF  PSTCA-FROM-FLD-MENU                       24091132
240912                    AND PSTCA-FLD-MENU-OPT NOT = '004'            24091232
240914                       PERFORM P9830-RETRIEVE-CNTL-INFO           24091431
240915                       IF P956-MASK-FLD-SCR-YES                   24091531
240916*CNC0576 - BEG                                                    24091633
240917                       OR P956-MASK-HOLD-TURN                     24091733
240918*CNC0576 - END                                                    24091833
240919                          MOVE '**'        TO WS-VL2-LO           24091931
240920                       ELSE                                       24092031
241000                          MOVE LAYOFF-CODE TO WS-VL2-LO           24100031
241320                       END-IF                                     24132031
241321                    ELSE                                          24132131
241322                       MOVE LAYOFF-CODE    TO WS-VL2-LO           24132231
241323                    END-IF                                        24132331
241324                    IF OD-AREA > SPACES                           24132431
241325                       MOVE OD-AREA TO WS-VL2-ASGN                24132531
241326                    END-IF                                        24132631
241330*CNC0573 - END                                                    24133031
241400                 ELSE                                             24140015
241500                    IF OUT-TOWN                                   24150015
241600                       MOVE 'OT' TO WS-VL2-LO                     24160015
241700                       MOVE OD-AREA TO WS-VL2-ASGN                24170015
241800                    END-IF                                        24180015
241900                 END-IF                                           24190015
242000              END-IF                                              24200015
242100***   CNC0021/CNC0022 CW0896 SET RETURN DATE FOR OFF-MILES AND    24210000
242200***   VACATIONS.                                                  24220000
242300              INITIALIZE WS-RETURN-DATE                           24230015
242400              EVALUATE TRUE                                       24240015
242500                 WHEN OFF-MILES-DAYS                              24250015
242600                    PERFORM P9830-RETRIEVE-CNTL-INFO              24260015
242700                    IF P956-ST-RSN-NBR-DAYS-REQ                   24270015
242800                    OR P956-ST-RSN-EXP-DATE-REQ                   24280015
242900                       PERFORM P1501-GET-DUEBACK-DATE             24290015
243000                       IF WS-DUEBACK-FOUND-Y                      24300015
243100                          MOVE TASK-LO-EXP-TIME                   24310015
243200                                           TO WS-VL2-RETURN-DATE  24320015
243300                          MOVE TASK-LO-EXP-DATE(5:2)              24330015
243400                                           TO WS-VL2-RETURN-DY    24340015
243500                       END-IF                                     24350015
243600                    ELSE                                          24360015
243700                       PERFORM P4150-OFF-MILES-RETURN-DATE        24370015
243800                       MOVE WS-RETURN-DATE TO WS-VL2-RETURN-DATE  24380015
243900                    END-IF                                        24390015
244000                 WHEN VACATION                                    24400015
244100                    PERFORM P4170-VACATION-RETURN-DATE            24410015
244200                    MOVE WS-RETURN-DATE TO WS-VL2-RETURN-DATE     24420015
244300                 WHEN EXCUSED-ABSENCE                             24430015
244400                  AND LAYOFF-EM-CODE = '69'                       24440015
244500                      PERFORM P1501-GET-DUEBACK-DATE              24450015
244600                      IF WS-DUEBACK-FOUND-Y                       24460015
244610*CNC0556<<                                                        24461024
244611***                      MOVE TASK-LO-EXP-TIME                    24461124
244612***                                          TO WS-VL2-RETURN-DATE24461224
244613***                      MOVE TASK-LO-EXP-DATE(5:2)               24461324
244614***                                            TO WS-VL2-RETURN-DY24461424
244615                         IF TASK-DUE-BACK                         24461525
244616                            CONTINUE                              24461625
244617                         ELSE                                     24461725
244618                            IF TASK-LO-EXP-DATE-TIME > SPACES     24461825
244619                               MOVE TASK-LO-EXP-TIME              24461925
244620                                             TO WS-VL2-RETURN-DATE24462024
244621                               MOVE TASK-LO-EXP-DATE(5:2)         24462125
244622                                              TO WS-VL2-RETURN-DY 24462225
244623                            ELSE                                  24462325
244625                               IF EFFECTIVE-DATE-TIME-CENT > ZERO 24462524
244626                                  SET TZ-IN-EASTERN-ZONE TO TRUE  24462624
244627                                  MOVE EFFECTIVE-DATE-TIME        24462724
244628                                              TO TZ-IN-DATE-TIME  24462824
244629                                  MOVE TASK-TIME-ZONE             24462924
244630                                              TO TZ-OUT-ZONE      24463024
244631                                  PERFORM P8996-TIMEZONE          24463124
244632                                  MOVE TZ-OUT-TIME                24463224
244633                                        TO WS-VL2-RETURN-DATE     24463324
244634                                  MOVE TZ-OUT-DD                  24463424
244635                                        TO WS-VL2-RETURN-DY       24463524
244636                               END-IF                             24463624
244637                            END-IF                                24463724
244638                         END-IF                                   24463824
244640*CNC0556>>                                                        24464024
245100                      END-IF                                      24510015
245200                 WHEN OTHER                                       24520015
245300*CNC0492 - FOR OTHER LAYOFF CODES, GET DUEBACK/BOOK-ON DATE/TIME  24530007
245400                      IF NOT AVAILABLE AND                        24540015
245500                         NOT WORKING AND                          24550015
245600                         NOT TO-PLACE                             24560015
245700                         AND LAYOFF-TIME NUMERIC                  24570015
245800                         AND LAYOFF-TIME > ZERO                   24580015
245900                         PERFORM P1501-GET-DUEBACK-DATE           24590015
246000                         IF WS-DUEBACK-FOUND-Y                    24600015
246001*CNC0556<<                                                        24600124
246002***                         MOVE TASK-LO-EXP-TIME                 24600224
246003***                                          TO WS-VL2-RETURN-DATE24600324
246004***                         MOVE TASK-LO-EXP-DATE(5:2)            24600424
246005***                                            TO WS-VL2-RETURN-DY24600524
246006                            IF TASK-DUE-BACK                      24600625
246007                               CONTINUE                           24600725
246008                            ELSE                                  24600825
246010                               IF TASK-LO-EXP-DATE-TIME > SPACES  24601025
246020                                  MOVE TASK-LO-EXP-TIME           24602025
246030                                             TO WS-VL2-RETURN-DATE24603024
246040                                  MOVE TASK-LO-EXP-DATE(5:2)      24604025
246050                                              TO WS-VL2-RETURN-DY 24605025
246060                               ELSE                               24606025
246091                                  IF EFFECTIVE-DATE-TIME-CENT     24609124
246092                                                           > ZERO 24609224
246093                                     SET TZ-IN-EASTERN-ZONE       24609324
246094                                            TO TRUE               24609424
246095                                     MOVE EFFECTIVE-DATE-TIME     24609524
246096                                            TO TZ-IN-DATE-TIME    24609624
246097                                     MOVE TASK-TIME-ZONE          24609724
246098                                            TO TZ-OUT-ZONE        24609824
246099                                     PERFORM P8996-TIMEZONE       24609924
246100                                     MOVE TZ-OUT-TIME             24610024
246101                                            TO WS-VL2-RETURN-DATE 24610124
246102                                     MOVE TZ-OUT-DD               24610224
246103                                            TO WS-VL2-RETURN-DY   24610324
246104                                  END-IF                          24610424
246105                               END-IF                             24610524
246106                            END-IF                                24610624
246108*CNC0556>>                                                        24610824
246500                         END-IF                                   24650015
246600                      ELSE                                        24660015
246700                         CONTINUE                                 24670015
246800                      END-IF                                      24680015
246900              END-EVALUATE                                        24690015
247000              IF FICT-SENIORITY-ORDER                             24700015
247100                 SUBTRACT 1        FROM SEN-SUB                   24710015
247200                 MOVE SEN-SUB        TO WS-VL2-SEN-LVL            24720015
247300                 MOVE SF-DATE        TO WS-VL2-SEN-DATE           24730015
247400                 ADD 1               TO SEN-SUB                   24740015
247500              ELSE                                                24750015
247600                 PERFORM P3050-SET-UP-P942LINK                    24760015
247700                 MOVE P942-EMP-SEN-LEVEL TO WS-VL2-SEN-LVL        24770015
247800                 MOVE P942-EMP-SEN-DATE TO WS-VL2-SEN-DATE        24780015
247900              END-IF                                              24790015
248000           END-IF                                                 24800015
248100           MOVE SPACES TO WS-VL2-REST-DAYS                        24810015
248200                          WS-VL2-AVAIL-START                      24820015
248300                          WS-VL2-HYPHEN                           24830015
248400                          WS-VL2-AVAIL-END                        24840015
248500           IF CNTL-XB-SCHEDULED                                   24850015
248600              OR CNTL-XB-EXTENDED-SCHED                           24860015
248700              PERFORM P1550-CHECK-SCHEDULE                        24870015
248800              PERFORM VARYING WS-REST-SUB FROM +1 BY +1           24880015
248900                 UNTIL WS-REST-SUB > 7                            24890015
249000                 IF P912-JOB-ON-REST-DAY(WS-REST-SUB)             24900015
249100                    IF CNTL-XB-SCHEDULED                          24910015
249200                       MOVE DAYOFWK-2CHAR(WS-REST-SUB, PSTCA-SUB) 24920015
249300                            TO WS-VL2-REST-DAY(WS-REST-SUB)       24930015
249400                    ELSE                                          24940015
249500                       PERFORM P1560-GET-REST-DAY-DESC            24950015
249600                    END-IF                                        24960015
249700                 END-IF                                           24970015
249800              END-PERFORM                                         24980015
249900              IF P912-AVAILABLE-START-TIME > SPACES               24990015
250000                 MOVE P912-AVAILABLE-START-TIME                   25000015
250100                                             TO WS-VL2-AVAIL-START25010015
250200                 MOVE '-' TO WS-VL2-HYPHEN                        25020015
250300                 MOVE P912-AVAILABLE-END-TIME TO WS-VL2-AVAIL-END 25030015
250400              END-IF                                              25040015
250500           END-IF                                                 25050015
250600           MOVE WS-VARIABLE-LINE-2 TO SCR02D-VARIABLE(NAME-SUB)   25060015
250700        END-IF                                                    25070015
250800     END-IF.                                                      25080000
250900                                                                  25090000
251000 P1501-GET-DUEBACK-DATE.                                          25100000
251100                                                                  25110000
251200     MOVE SPACES                        TO TASK-EMPLOYEE-KEY      25120000
251300     SET WS-DUEBACK-FOUND-N             TO TRUE                   25130000
251400     MOVE EMP-NBR OF WS-MSTR            TO EMP-NBR OF WS-TASK     25140000
251500     PERFORM P8300-START-TASK-FILE                                25150000
251600     IF SUCCESS                                                   25160000
251700        PERFORM P8310-READNEXT-TASK-FILE                          25170000
251800        PERFORM UNTIL NOT SUCCESS                                 25180000
251900           OR WS-DUEBACK-FOUND-Y                                  25190000
252000           OR EMP-NBR OF WS-MSTR NOT = EMP-NBR OF WS-TASK         25200000
252100           IF   TASK-LAYOFF-MARKUP                                25210000
252200           AND (TASK-DUE-BACK OR TASK-LO1 = 'A')                  25220000
252300              SET WS-DUEBACK-FOUND-Y    TO TRUE                   25230000
252400           ELSE                                                   25240000
252500              PERFORM P8310-READNEXT-TASK-FILE                    25250000
252600           END-IF                                                 25260000
252700        END-PERFORM                                               25270000
252800     END-IF                                                       25280000
252900     PERFORM P8320-ENDBR-TASK-FILE                                25290000
253000     .                                                            25300000
253100*                                                                 25310000
253200 P1505-CALL-XB-POS-PARMS.                                         25320000
253300*                                                                 25330000
253400*                                                                 25340000
253500*    FOR THE SLOW POSITION OF A FASTSLOW BOARD, SET THE           25350000
253600*    SLOW PARAMETER FOR P912.                                     25360000
253700*                                                          **PLS  25370000
253800*                                                                 25380000
253900*    ALSO USING P912 TO GET SCHEDULED SPAREBOARD INFORMATION      25390000
254000*    SO IT NEEDS TO BE CALLED FOR BOTH POSITION AND TURN          25400000
254100*    REQUESTS.                      NORTHERN QUEBEC SPAREBOARDS   25410000
254200*                                                                 25420000
254300     MOVE SPACES TO P912-XB-POS-PARMS                             25430000
254400     MOVE EBTURN-AREA     TO P912-TURN-PARM                       25440000
254500     IF FASTSLOW-XB AND FICT-ROAD-BOARD                           25450000
254600        SET P912-SLOW-BOARD       TO TRUE                         25460000
254700     END-IF                                                       25470000
254800     IF      WS-VIEW-TIME    NOT = WS-LOCAL-TIME AND              25480000
254900             WS-VIEW-TIME        > SPACES                         25490000
255000        MOVE WS-VIEW-TIME       TO P912-VIEW-TIME                 25500000
255100     ELSE                                                         25510000
255200        MOVE WS-LOCAL-TIME      TO P912-VIEW-TIME                 25520000
255300     END-IF                                                       25530000
255400     IF      P912-VIEW-TIME      < '0001'                         25540000
255500        MOVE '0001'             TO P912-VIEW-TIME                 25550000
255600     END-IF                                                       25560000
255700*CNC0517                                                          25570015
255800     IF FICT-QUAL-ORDER                                           25580015
255900        MOVE DAY1               TO P912-VIEW-DAY-N                25590015
256000     ELSE                                                         25600015
256100*                                                                 25610015
256200        MOVE WS-TODAY           TO P912-VIEW-DAY-N                25620015
256300     END-IF                                                       25630015
256400     IF      P912-VIEW-TIME      < WS-LOCAL-TIME                  25640000
256500        ADD  1                  TO P912-VIEW-DAY-N                25650000
256600        IF P912-VIEW-DAY-N       > 7                              25660000
256700           MOVE 1               TO P912-VIEW-DAY-N                25670000
256800        END-IF                                                    25680000
256900     END-IF                                                       25690000
257000     EXEC CICS LINK                                               25700000
257100               PROGRAM(P912-PGM)                                  25710000
257200               COMMAREA(P912-XB-POS-PARMS)                        25720000
257300               LENGTH(P912-LGTH)                                  25730000
257400               RESP(WS-RESPONSE)                                  25740000
257500     END-EXEC                                                     25750000
257600     MOVE WS-RESPONSE TO FILE-STATUS                              25760000
257700     IF NOT SUCCESS                                               25770000
257800        MOVE 'P1500-1' TO ERR-PARAGRAPH                           25780000
257900        PERFORM P9999-GOT-PROBLEM                                 25790000
258000     END-IF.                                                      25800000
258100*    CNC0006 - FLW, 5/8/96, START                                 25810000
258200*                                                                 25820000
258300 P1510-INQUIRE-STARTS.                                            25830000
258400*                                                                 25840000
258500*    CNC0006 - FLW, 5/8/96, END                                   25850000
258600     MOVE SPACES                    TO P931-COMMAREA-PARMS        25860000
258700     SET P931-INQUIRY-FUN           TO TRUE                       25870000
258800     MOVE EMP-NBR IN WS-MSTR        TO P931-INQ-EMP-NO            25880000
258900     SET P931-INQ-STARTS            TO TRUE                       25890000
259000     SET P931-CURRENT-PERIOD        TO TRUE                       25900000
259100     SET P931-MASTER-INQUIRY        TO TRUE                       25910000
259200     EXEC CICS LINK                                               25920000
259300               PROGRAM(P931-PGM)                                  25930000
259400               COMMAREA(P931-COMMAREA-PARMS)                      25940000
259500               LENGTH(P931-LGTH)                                  25950000
259600               RESP(WS-RESPONSE)                                  25960000
259700     END-EXEC                                                     25970000
259800     MOVE WS-RESPONSE               TO FILE-STATUS                25980000
259900     IF NOT SUCCESS                                               25990000
260000        MOVE 'P1510-1'              TO ERR-PARAGRAPH              26000000
260100        MOVE 'P931LINK'             TO ERR-KEY                    26010000
260200        PERFORM P9999-GOT-PROBLEM                                 26020000
260300     END-IF                                                       26030000
260400*    CNC0006 - FLW, 5/8/96, START                                 26040000
260500     MOVE P931-RET-CRAFT-STT        TO WS-VL1-STT-STARTS.         26050000
260600     MOVE P931-RET-CRAFT-OVT        TO WS-VL1-OVT-STARTS.         26060000
260700*    CNC0006 - FLW, 5/8/96, END                                   26070000
260800*                                                                 26080015
260900* P1520-SET-SENIORITY-ARRAY AND P1540-SET-QUAL-ARRAY ARE ADDED AS 26090015
261000* PART OF CNC0517. THEY ARE CLONED FROM CNP06                     26100015
261100*                                                                 26110015
261200 P1520-SET-SENIORITY-ARRAY.                                       26120015
261300*                                                                 26130015
261400     MOVE SPACES             TO WS-SENIORITY                      26140015
261500     MOVE EMP-NBR OF WS-MSTR TO SF-EMP-NO                         26150015
261600     MOVE SF-SENKEY2         TO SENKEY2                           26160015
261700     EXEC CICS STARTBR                                            26170015
261800               DATASET(SENFILE-VIA-EMP-NO)                        26180015
261900               RIDFLD(SENKEY2)                                    26190015
262000               GTEQ                                               26200015
262100               RESP(WS-RESPONSE)                                  26210015
262200     END-EXEC                                                     26220015
262300     MOVE WS-RESPONSE TO FILE-STATUS                              26230015
262400     IF SUCCESS                                                   26240015
262500        MOVE '0' TO SEN-DONE-CODE                                 26250015
262600        PERFORM UNTIL SEN-DONE                                    26260015
262700           EXEC CICS READNEXT                                     26270015
262800                     DATASET(SENFILE-VIA-EMP-NO)                  26280015
262900                     INTO (WS-SENIORITY)                          26290015
263000                     LENGTH(SENENBR-RLGTH)                        26300015
263100                     RIDFLD(SENKEY2)                              26310015
263200                     KEYLENGTH(SENENBR-KLGTH)                     26320015
263300                     RESP(WS-RESPONSE)                            26330015
263400           END-EXEC                                               26340015
263500           MOVE WS-RESPONSE TO FILE-STATUS                        26350015
263600           IF SUCCESS                                             26360015
263700              IF SF-EMP-NO = EMP-NBR OF WS-MSTR                   26370015
263800                 IF SF-CRAFT = 'EN'                               26380015
263900                    MOVE 'Y' TO WS-VL4-EN                         26390015
264000                 ELSE                                             26400015
264100                    IF SF-CRAFT = 'CO'                            26410015
264200                       MOVE 'Y' TO WS-VL4-CO                      26420015
264300                    ELSE                                          26430015
264400                       IF SF-CRAFT = 'FO'                         26440015
264500                          MOVE 'Y' TO WS-VL4-FO                   26450015
264600                       ELSE                                       26460015
264700                          IF SF-CRAFT = 'SW'                      26470015
264800                             MOVE 'Y' TO WS-VL4-SW                26480015
264900                          ELSE                                    26490015
265000                             IF SF-CRAFT = 'BK'                   26500015
265100                                MOVE 'Y' TO WS-VL4-BK             26510015
265200                             ELSE                                 26520015
265300                                IF SF-CRAFT = 'YA'                26530015
265400                                   MOVE 'Y' TO WS-VL4-YA          26540015
265500                                END-IF                            26550015
265600                             END-IF                               26560015
265700                          END-IF                                  26570015
265800                       END-IF                                     26580015
265900                    END-IF                                        26590015
266000                 END-IF                                           26600015
266100              ELSE                                                26610015
266200                 SET SEN-DONE TO TRUE                             26620015
266300              END-IF                                              26630015
266400           ELSE                                                   26640015
266500              SET SEN-DONE TO TRUE                                26650015
266600              IF NOT (NO-RECORD-FND OR END-OF-FILE)               26660015
266700                 MOVE 'P1520-1' TO ERR-PARAGRAPH                  26670015
266800                 MOVE SENKEY2   TO ERR-KEY                        26680015
266900                 PERFORM P9999-GOT-PROBLEM                        26690015
267000              END-IF                                              26700015
267100           END-IF                                                 26710015
267200        END-PERFORM                                               26720015
267300        EXEC CICS ENDBR                                           26730015
267400                  DATASET(SENFILE-VIA-EMP-NO)                     26740015
267500                  RESP(WS-RESPONSE)                               26750015
267600        END-EXEC                                                  26760015
267700     ELSE                                                         26770015
267800        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     26780015
267900           MOVE 'P1520-2' TO ERR-PARAGRAPH                        26790015
268000           MOVE SENKEY2   TO ERR-KEY                              26800015
268100           PERFORM P9999-GOT-PROBLEM                              26810015
268200        END-IF                                                    26820015
268300     END-IF.                                                      26830015
268400* P1520-SET-SENIORITY-ARRAY AND P1540-SET-QUAL-ARRAY ARE ADDED AS 26840015
268500* PART OF CNC0517. THEY ARE CLONED FROM CNP06                     26850015
268600*                                                                 26860015
268700 P1540-SET-QUAL-ARRAY.                                            26870015
268800*                                                                 26880015
268900     MOVE SPACES             TO WS-QUAL-FILE                      26890015
269000     MOVE EMP-NBR OF WS-MSTR TO QUAL-EMP-NO                       26900015
269100     MOVE QUALEMP-KEY        TO QUALEMP                           26910015
269200     EXEC CICS STARTBR                                            26920015
269300               DATASET(QUAL-FILE-VIA-QUALEMP)                     26930015
269400               RIDFLD(QUALEMP)                                    26940015
269500               GTEQ                                               26950015
269600               RESP(WS-RESPONSE)                                  26960015
269700     END-EXEC                                                     26970015
269800     MOVE WS-RESPONSE TO FILE-STATUS                              26980015
269900     IF SUCCESS                                                   26990015
270000        MOVE '0' TO QUAL-DONE-CODE                                27000015
270100        MOVE 1 TO QUAL-SUB                                        27010015
270200        PERFORM UNTIL QUAL-DONE OR QUAL-SUB > 5                   27020015
270300           EXEC CICS READNEXT                                     27030015
270400                     DATASET(QUAL-FILE-VIA-QUALEMP)               27040015
270500                     INTO (WS-QUAL-FILE)                          27050015
270600                     LENGTH(QUALEMP-RLGTH)                        27060015
270700                     RIDFLD(QUALEMP)                              27070015
270800                     KEYLENGTH(QUALEMP-KLGTH)                     27080015
270900                     RESP(WS-RESPONSE)                            27090015
271000           END-EXEC                                               27100015
271100           MOVE WS-RESPONSE TO FILE-STATUS                        27110015
271200           IF SUCCESS                                             27120015
271300              IF QUAL-EMP-NO = EMP-NBR OF WS-MSTR                 27130015
271400                 MOVE QUALIFICATION TO TEST-QUAL                  27140015
271500                 IF NOT STEP-RATE-QUAL                            27150015
271600                    MOVE SPACES         TO WS-CNTL-FILE           27160015
271700                    SET QUAL-CODE-TYPE-REC TO TRUE                27170015
271800                    MOVE QUALIFICATION  TO CNTL-QUAL-CODE         27180015
271900                    MOVE CNTLKEY-AREA   TO CNTLKEY                27190015
272000                    EXEC CICS READ                                27200015
272100                              DATASET(CNTL-FILE-VIA-CNTLKEY)      27210015
272200                              INTO(WS-CNTL-FILE)                  27220015
272300                              LENGTH(CNTLFILE-RLGTH)              27230015
272400                              RIDFLD(CNTLKEY)                     27240015
272500                              KEYLENGTH(CNTLFILE-KLGTH)           27250015
272600                              RESP(WS-RESPONSE)                   27260015
272700                    END-EXEC                                      27270015
272800                    MOVE WS-RESPONSE TO FILE-STATUS               27280015
272900                    IF SUCCESS AND CNTL-QUAL-DISP-SB-MAINT = 'Y'  27290015
273000                       MOVE QUALIFICATION TO WS-VL4-QUAL(QUAL-SUB)27300015
273100                       ADD 1 TO QUAL-SUB                          27310015
273200                    END-IF                                        27320015
273300                 END-IF                                           27330015
273400              ELSE                                                27340015
273500                 SET QUAL-DONE TO TRUE                            27350015
273600              END-IF                                              27360015
273700           ELSE                                                   27370015
273800              SET QUAL-DONE TO TRUE                               27380015
273900              IF NOT (NO-RECORD-FND OR END-OF-FILE)               27390015
274000                 MOVE 'P1540-1' TO ERR-PARAGRAPH                  27400015
274100                 MOVE QUALEMP   TO ERR-KEY                        27410015
274200                 PERFORM P9999-GOT-PROBLEM                        27420015
274300              END-IF                                              27430015
274400           END-IF                                                 27440015
274500        END-PERFORM                                               27450015
274600        EXEC CICS ENDBR                                           27460015
274700                  DATASET(QUAL-FILE-VIA-QUALEMP)                  27470015
274800                  RESP(WS-RESPONSE)                               27480015
274900        END-EXEC                                                  27490015
275000     ELSE                                                         27500015
275100        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     27510015
275200           MOVE 'P1540-2' TO ERR-PARAGRAPH                        27520015
275300           MOVE QUALEMP   TO ERR-KEY                              27530015
275400           PERFORM P9999-GOT-PROBLEM                              27540015
275500        END-IF                                                    27550015
275600     END-IF.                                                      27560015
275700*************************************************************     27570000
275800 P1550-CHECK-SCHEDULE.                                            27580000
275900*   ALTER DISPLAY LINE BASED ON SCHEDULED SPAREBOARD              27590000
276000*   INFORMATION RETURNED FROM P912.                               27600000
276100*                        NORTHERN QUEBEC SPAREBOARD - PHASE 2     27610000
276200*************************************************************     27620000
276300     IF WS-VL1-LO <= SPACES                                       27630000
276400        IF P912-SCHEDULED-REST-DAY                                27640000
276500           MOVE 'RD' TO WS-VL1-LO                                 27650000
276600        END-IF                                                    27660000
276700        IF P912-SCHEDULED-REST-PERIOD                             27670000
276800           MOVE 'RP' TO WS-VL1-LO                                 27680000
276900        END-IF                                                    27690000
277000     END-IF                                                       27700000
277100     IF P912-SCHEDULED-OFF-BOARD                                  27710000
277200        MOVE RED TO SCR02D-NAME-COLOR(TURN-SUB)                   27720000
277300                    SCR02D-TURN-COLOR(TURN-SUB)                   27730000
277400                    SCR02D-VARIABLE-COLOR(TURN-SUB)               27740000
277500     END-IF                                                       27750000
277600     .                                                            27760000
277700*                                                                 27770000
277800 P1560-GET-REST-DAY-DESC.                                         27780000
277900*                                                                 27790000
278000*    CNPS42 RETURNS THE NEXT 7 DAYS WORTH OF REST DAYS, IF ANY,   27800000
278100*    STARTING WITH THE CURRENT DAY.  DETERMINE THE DAYS OF THE    27810000
278200*    WEEK FOR THE REST DAYS IN QUESTION.                          27820000
278300*                                                                 27830000
278400     IF WS-REST-SUB NOT = 1                                       27840000
278500        MOVE ZEROES                     TO DATE-CONVERSION-PARMS  27850000
278600        SET PARM-ADD                    TO TRUE                   27860000
278700        MOVE WS-LOCAL-DATE              TO PARM-PRI-DATE-GREG     27870000
278800        MOVE WS-LOCAL-TIME              TO PARM-PRI-TIME          27880000
278900        COMPUTE WS-NUM = WS-REST-SUB - 1                          27890000
279000        MOVE WS-NUM                     TO PARM-SEC-DATE-GREG     27900000
279100        EXEC CICS LINK                                            27910000
279200                  PROGRAM(P903-PGM)                               27920000
279300                  COMMAREA(DATE-CONVERSION-PARMS)                 27930000
279400                  LENGTH(P903-LGTH)                               27940000
279500                  RESP(WS-RESPONSE)                               27950000
279600        END-EXEC                                                  27960000
279700        MOVE WS-RESPONSE                TO FILE-STATUS            27970000
279800        IF NOT SUCCESS                                            27980000
279900           MOVE 'P1560-1'               TO ERR-PARAGRAPH          27990000
280000           PERFORM P9999-GOT-PROBLEM                              28000000
280100        END-IF                                                    28010000
280200        MOVE PARM-RES-DATE-GREG         TO HOLD-RES-DATE-GREG     28020000
280300     ELSE                                                         28030000
280400        MOVE WS-LOCAL-DATE              TO HOLD-RES-DATE-GREG     28040000
280500     END-IF                                                       28050000
280600                                                                  28060000
280700     MOVE ZEROS                         TO DATE-CONVERSION-PARMS  28070000
280800     SET PARM-CONV                      TO TRUE                   28080000
280900     MOVE HOLD-RES-DATE-GREG            TO PARM-PRI-DATE-GREG     28090000
281000     EXEC CICS LINK                                               28100000
281100               PROGRAM(P903-PGM)                                  28110000
281200               COMMAREA(DATE-CONVERSION-PARMS)                    28120000
281300               LENGTH(P903-LGTH)                                  28130000
281400               RESP(WS-RESPONSE)                                  28140000
281500     END-EXEC                                                     28150000
281600     MOVE WS-RESPONSE                   TO FILE-STATUS            28160000
281700     IF NOT SUCCESS                                               28170000
281800        MOVE 'P1560-2'                  TO ERR-PARAGRAPH          28180000
281900        PERFORM P9999-GOT-PROBLEM                                 28190000
282000     END-IF                                                       28200000
282100     MOVE DAYOFWK-2CHAR(PARM-PRI-DAY-OF-WEEK, PSTCA-SUB)          28210000
282200                                  TO WS-VL2-REST-DAY(WS-REST-SUB) 28220000
282300     .                                                            28230000
282400*                                                                 28240000
282500 P1600-SETUP-OFF-BD-SORT.                                         28250000
282600*                                                                 28260000
282700     IF GOT-EMPLOYEE                                              28270000
282800        IF ON-DUTY-ASGNMT > SPACES OR                             28280000
282900           FICT-POSITION-ORDER                                    28290000
283000           IF (TEMPORARY-ASGNMT > SPACE                           28300000
283100*            AND TEMP-ASGN-XB-AUG                                 28310000
283200             AND (TA-DIST NOT = DIST-REPEAT                       28320000
283300              OR TA-SUB-DIST NOT = SUBDIST-REPEAT                 28330000
283400              OR TA-XB-TURN NOT = TURN-NBR OF WS-EXTRA-BOARD      28340000
283500              OR TA-CC NOT = CRAFT-CODE-REPEAT))                  28350000
283600              CONTINUE                                            28360000
283700           ELSE                                                   28370000
283800              ADD 1 TO SSUB                                       28380000
283900              ADD 1 TO TURN-SUB                                   28390000
284000              MOVE EB-ODT-TIME            TO SORT-DATE-TIME(SSUB) 28400000
284100              MOVE EMP-NAME OF WS-MSTR    TO SORT-NAME(SSUB)      28410000
284200              MOVE TURN-NBR OF WS-EXTRA-BOARD                     28420000
284300                                          TO SORT-TURN(SSUB)      28430000
284400              MOVE LAYOFF-CODE OF WS-MSTR TO SORT-STATUS(SSUB)    28440000
284500              INITIALIZE WS-RETURN-DATE                           28450000
284600                         SORT-RETURN-DATE(SSUB)                   28460000
284700              EVALUATE TRUE                                       28470000
284800                 WHEN OFF-MILES-DAYS                              28480000
284900                    PERFORM P9830-RETRIEVE-CNTL-INFO              28490003
285000                    IF P956-ST-RSN-NBR-DAYS-REQ                   28500003
285100                    OR P956-ST-RSN-EXP-DATE-REQ                   28510003
285200                       PERFORM P1501-GET-DUEBACK-DATE             28520003
285300                       IF WS-DUEBACK-FOUND-Y                      28530003
285400                          MOVE TASK-LO-EXP-TIME                   28540003
285500                                        TO SORT-RETURN-DATE(SSUB) 28550003
285600                          MOVE TASK-LO-EXP-DATE(5:2)              28560004
285700                                        TO SORT-RETURN-DY(SSUB)   28570004
285800                       END-IF                                     28580003
285900                    ELSE                                          28590003
286000                       PERFORM P4150-OFF-MILES-RETURN-DATE        28600003
286100                       MOVE WS-RETURN-DATE                        28610003
286200                                        TO SORT-RETURN-DATE(SSUB) 28620003
286300                    END-IF                                        28630003
286400                 WHEN VACATION                                    28640000
286500                    PERFORM P4170-VACATION-RETURN-DATE            28650000
286600                    MOVE WS-RETURN-DATE TO SORT-RETURN-DATE(SSUB) 28660000
286700                 WHEN EXCUSED-ABSENCE                             28670004
286800                  AND LAYOFF-EM-CODE = '69'                       28680004
286900                    PERFORM P1501-GET-DUEBACK-DATE                28690004
287000                    IF WS-DUEBACK-FOUND-Y                         28700004
287010*CNC0556<<                                                        28701024
287100***                    MOVE TASK-LO-EXP-TIME                      28710024
287200***                                     TO SORT-RETURN-DATE(SSUB) 28720024
287300***                    MOVE TASK-LO-EXP-DATE(5:2)                 28730024
287400***                                     TO SORT-RETURN-DY(SSUB)   28740024
287401                       IF TASK-DUE-BACK                           28740125
287402                          CONTINUE                                28740225
287403                       ELSE                                       28740325
287404                          IF TASK-LO-EXP-DATE-TIME > SPACES       28740425
287405                             MOVE TASK-LO-EXP-TIME                28740525
287406                                        TO SORT-RETURN-DATE(SSUB) 28740624
287407                             MOVE TASK-LO-EXP-DATE(5:2)           28740725
287408                                        TO SORT-RETURN-DY(SSUB)   28740824
287409                          ELSE                                    28740925
287411                             IF EFFECTIVE-DATE-TIME-CENT > ZEROES 28741124
287412                                SET TZ-IN-EASTERN-ZONE   TO TRUE  28741224
287413                                MOVE EFFECTIVE-DATE-TIME          28741324
287414                                              TO TZ-IN-DATE-TIME  28741424
287415                                MOVE TASK-TIME-ZONE               28741524
287416                                              TO TZ-OUT-ZONE      28741624
287417                                PERFORM P8996-TIMEZONE            28741724
287418                                MOVE TZ-OUT-TIME                  28741824
287419                                        TO SORT-RETURN-DATE(SSUB) 28741924
287420                                MOVE TZ-OUT-DD                    28742024
287421                                        TO SORT-RETURN-DY(SSUB)   28742124
287422                             END-IF                               28742224
287423                          END-IF                                  28742324
287424                       END-IF                                     28742424
287430*CNC0556>>                                                        28743024
287500                    END-IF                                        28750004
287600                 WHEN OTHER                                       28760000
287700*CNC0492 - FOR OTHER LAYOFF CODES, GET DUEBACK/BOOK-ON DATE/TIME  28770007
287800                    IF NOT AVAILABLE  AND                         28780010
287900                       NOT WORKING    AND                         28790010
288000                       NOT TO-PLACE                               28800010
288100                       AND LAYOFF-TIME NUMERIC                    28810010
288200                       AND LAYOFF-TIME > ZERO                     28820010
288300                       PERFORM P1501-GET-DUEBACK-DATE             28830010
288400                       IF WS-DUEBACK-FOUND-Y                      28840010
288410*CNC0556<<                                                        28841024
288500***                       MOVE TASK-LO-EXP-TIME                   28850024
288600***                                     TO SORT-RETURN-DATE(SSUB) 28860024
288700***                       MOVE TASK-LO-EXP-DATE(5:2)              28870024
288800***                                     TO SORT-RETURN-DY(SSUB)   28880024
288801                          IF TASK-DUE-BACK                        28880125
288802                             CONTINUE                             28880225
288803                          ELSE                                    28880325
288810                             IF TASK-LO-EXP-DATE-TIME > SPACES    28881025
288820                                MOVE TASK-LO-EXP-TIME             28882025
288830                                        TO SORT-RETURN-DATE(SSUB) 28883024
288840                                MOVE TASK-LO-EXP-DATE(5:2)        28884025
288850                                        TO SORT-RETURN-DY(SSUB)   28885024
288860                             ELSE                                 28886025
288891                                IF EFFECTIVE-DATE-TIME-CENT       28889124
288892                                                           > ZERO 28889224
288893                                   SET TZ-IN-EASTERN-ZONE TO TRUE 28889324
288894                                   MOVE EFFECTIVE-DATE-TIME       28889424
288895                                              TO TZ-IN-DATE-TIME  28889524
288896                                   MOVE TASK-TIME-ZONE            28889624
288897                                              TO TZ-OUT-ZONE      28889724
288898                                   PERFORM P8996-TIMEZONE         28889824
288899                                   MOVE TZ-OUT-TIME               28889924
288900                                        TO SORT-RETURN-DATE(SSUB) 28890024
288901                                   MOVE TZ-OUT-DD                 28890124
288902                                        TO SORT-RETURN-DY(SSUB)   28890224
288903                                END-IF                            28890324
288904                             END-IF                               28890424
288905                          END-IF                                  28890524
288906*CNC0556>>                                                        28890624
288910                       END-IF                                     28891010
289000                    ELSE                                          28900010
289100                       CONTINUE                                   28910010
289200                    END-IF                                        28920010
289300              END-EVALUATE                                        28930000
289400*              VERIFY OFF-BOARD ON BOTH NORMAL AND SLOW BOARDS    28940000
289500*                                                          **PLS  28950000
289600              IF FASTSLOW-XB                                      28960000
289700                 AND FICT-ROAD-BOARD                              28970000
289800                 MOVE EB-SLOW-POS-ON-OFF  TO SORT-POSITION(SSUB)  28980000
289900              ELSE                                                28990000
290000                 MOVE EB-POS-ON-OFF       TO SORT-POSITION(SSUB)  29000000
290100              END-IF                                              29010000
290200              IF ASGN-AJ-JOB-AREA > SPACES                        29020000
290300                 MOVE ASGN-AJ-JOB-AREA    TO SORT-ASSIGNMENT(SSUB)29030000
290400              END-IF                                              29040000
290500              IF SORT-ASSIGNMENT(SSUB) = HIGH-VALUES              29050000
290600                 MOVE TA-AREA             TO SORT-ASSIGNMENT(SSUB)29060000
290700              END-IF                                              29070000
290800           END-IF                                                 29080000
290900        END-IF                                                    29090000
291000     END-IF.                                                      29100000
291100*                                                                 29110000
291200 P1650-SORT-OFF-BD-RECORDS.                                       29120000
291300*                                                                 29130000
291400     ADD 1         TO SAVE-SCR-POS                                29140000
291500     MOVE SAVE-SCR-POS TO NAME-SUB                                29150000
291600     PERFORM UNTIL WS-SORT-ARRAY = HIGH-VALUES                    29160000
291700        PERFORM VARYING SSUB FROM 1 BY 1                          29170000
291800           UNTIL SSUB > SORT-ARRAY-MAX                            29180000
291900           SET  DE-YYMMDD-FORMAT        TO TRUE                   29190000
292000           MOVE WS-HOLD-SORT(1:6)       TO DE-YYMMDD              29200000
292100           PERFORM P8998-DATEEDIT                                 29210000
292200           IF DE-YYMMDD-CE-ALPHA = SPACES                         29220000
292300              MOVE HIGH-VALUES          TO DE-YYMMDD-CE-ALPHA     29230000
292400           END-IF                                                 29240000
292500           MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE       29250000
292600           MOVE WS-HOLD-SORT(7:4)       TO DE-COMPARE1-TIME       29260000
292700           SET  DE-YYMMDD-FORMAT        TO TRUE                   29270000
292800           MOVE SORT-DATE(SSUB)         TO DE-YYMMDD              29280000
292900           PERFORM P8998-DATEEDIT                                 29290000
293000           IF DE-YYMMDD-CE-ALPHA = SPACES                         29300000
293100              MOVE HIGH-VALUES          TO DE-YYMMDD-CE-ALPHA     29310000
293200           END-IF                                                 29320000
293300           MOVE DE-CCYYMMDD             TO DE-COMPARE2-DATE       29330000
293400           MOVE SORT-TIME(SSUB)         TO DE-COMPARE2-TIME       29340000
293500           IF DE-COMPARE2-DATE-TIME < DE-COMPARE1-DATE-TIME       29350000
293600*          IF SORT-DATE-TIME(SSUB)  < WS-HOLD-SORT                29360000
293700              MOVE SORT-DATE-TIME(SSUB) TO WS-HOLD-SORT           29370000
293800              MOVE SSUB TO                 WS-HOLD-SSUB           29380000
293900           END-IF                                                 29390000
294000        END-PERFORM                                               29400000
294100        MOVE SORT-NAME(WS-HOLD-SSUB)   TO SCR02D-NAME(NAME-SUB)   29410000
294200        MOVE SORT-TURN(WS-HOLD-SSUB)   TO SCR02D-TURN(NAME-SUB)   29420000
294300        MOVE SPACES                    TO WS-VARIABLE-LINE-1      29430000
294400        IF SORT-POSITION(WS-HOLD-SSUB) = '9'                      29440000
294500           MOVE '99'                   TO WS-VL1-POS              29450000
294600        END-IF                                                    29460000
294700        IF SORT-ASSIGNMENT(WS-HOLD-SSUB) > SPACES                 29470000
294800           MOVE SORT-ASSIGNMENT(WS-HOLD-SSUB) TO WS-VL1-ASSIGNMENT29480000
294900        ELSE                                                      29490000
295000           MOVE SORT-STATUS(WS-HOLD-SSUB)                         29500000
295100                                TO WS-VL1-LO                      29510000
295200           MOVE SORT-RETURN-DATE(WS-HOLD-SSUB)                    29520000
295300                                TO WS-VL1-RETURN-DATE             29530000
295400           MOVE SORT-RETURN-DY(WS-HOLD-SSUB)                      29540004
295500                                TO WS-VL1-RETURN-DY               29550004
295600        END-IF                                                    29560000
295700        MOVE HIGH-VALUES        TO SORT-AREA(WS-HOLD-SSUB)        29570000
295800                                   WS-HOLD-SORT                   29580000
295900        MOVE WS-VARIABLE-LINE-1 TO SCR02D-VARIABLE(NAME-SUB)      29590000
296000        ADD 1   TO NAME-SUB                                       29600000
296100     END-PERFORM.                                                 29610000
296200*=================================================================29620000
296300 P2000-GET-NBR-ASSIGNED.                                          29630000
296400*=================================================================29640000
296500     MOVE SPACES                        TO P913-COMMAREA-PARMS    29650000
296600     SET P913-GET-NBR-ASSIGN-FUNCTION   TO TRUE                   29660000
296700     MOVE WS-LOCAL-DATE-TIME            TO P913-EFF-DATE-TIME     29670000
296800     MOVE PSTCA-TIME-ZONE               TO P913-TIME-ZONE         29680000
296900     MOVE FICT-JOB-DIST                 TO P913-TURN-DIST         29690000
297000     MOVE FICT-JOB-SUB-DIST             TO P913-TURN-SUB-DIST     29700000
297100     MOVE FICT-JOB-GRP-PL-XB            TO P913-TURN-CC           29710000
297200     EXEC CICS LINK                                               29720000
297300               PROGRAM(P913-PGM)                                  29730000
297400               COMMAREA(P913-COMMAREA-PARMS)                      29740000
297500               LENGTH(P913-LGTH)                                  29750000
297600               RESP(WS-RESPONSE)                                  29760000
297700     END-EXEC                                                     29770000
297800     MOVE WS-RESPONSE                   TO FILE-STATUS            29780000
297900     IF NOT SUCCESS                                               29790000
298000        MOVE 'P2000-1'                  TO ERR-PARAGRAPH          29800000
298100        MOVE 'P913'                     TO ERR-KEY                29810000
298200        PERFORM P9999-GOT-PROBLEM                                 29820000
298300     END-IF                                                       29830000
298400     IF P913-NBR-ASSIGNED                > SPACES                 29840000
298500        MOVE P913-NBR-ASSIGNED          TO SCR02D-NBR-ASSIGNED    29850000
298600     END-IF                                                       29860000
298700     IF P913-NBR-ELIGIBLE                > SPACES                 29870000
298800        MOVE P913-NBR-ELIGIBLE          TO SCR02D-NBR-ELIGIBLE    29880000
298900     END-IF.                                                      29890000
299000*=================================================================29900000
299100 P3050-SET-UP-P942LINK.                                           29910000
299200*                                                                 29920000
299300     MOVE SPACES                TO P942-COMMAREA-PARMS            29930000
299400     SET P942-EMP-SEN-FUNCTION  TO TRUE                           29940000
299500     SET P942-ASGN-XB           TO TRUE                           29950000
299600     MOVE EMP-NBR IN WS-MSTR    TO P942-EMP-NO                    29960000
299700     MOVE FICT-JOB-DIST         TO P942-ASGN-DIST                 29970000
299800     MOVE FICT-JOB-SUB-DIST     TO P942-ASGN-SUB-DIST             29980000
299900     MOVE 'EX'                  TO P942-ASGN-POOL                 29990000
300000     MOVE SCR02D-TURN(NAME-SUB) TO P942-ASGN-TURN                 30000000
300100     MOVE FICT-JOB-GRP-PL-XB    TO P942-ASGN-CC                   30010000
300200     PERFORM P8080-LINK-942.                                      30020000
300300*                                                                 30030000
300400 P4150-OFF-MILES-RETURN-DATE.                                     30040000
300500*                                                                 30050000
300600     IF EMP-MILES-DATE NUMERIC                                    30060000
300700        AND EMP-MILES-DATE-NUM > 0                                30070000
300800        AND EMP-MILES-DATE-NUM < 32                               30080000
300900*       MOVE SYS-MO TO WS-RETURN-DATE-MM                          30090000
301000        MOVE WS-SYS-MO TO WS-RETURN-DATE-MM                       30100000
301100        MOVE EMP-MILES-DATE-NUM TO WS-RETURN-DATE-DD              30110000
301200*       IF EMP-MILES-DATE-NUM < SYS-DY                            30120000
301300        IF EMP-MILES-DATE-NUM < WS-SYS-DY                         30130000
301400           ADD 1 TO WS-RETURN-DATE-MM                             30140000
301500           IF WS-RETURN-DATE-MM > 12                              30150000
301600              MOVE 1 TO WS-RETURN-DATE-MM                         30160000
301700           END-IF                                                 30170000
301800        END-IF                                                    30180000
301900     ELSE                                                         30190000
302000        MOVE EMP-MILES-DATE TO WS-RETURN-DATE                     30200000
302100     END-IF                                                       30210000
302200     .                                                            30220000
302300                                                                  30230000
302400***************************************************************** 30240000
302500 P4170-VACATION-RETURN-DATE.                                      30250000
302600*    FOR EMPLOYEES ON VACATION, FIND THE BOOK-ON RECORD IN        30260000
302700*    THE TASK LIST.  THIS CONTAINS THE EXPECTED RETURN DATE.      30270000
302800***************************************************************** 30280000
302900     INITIALIZE TASK-EMPLOYEE-KEY                                 30290000
303000     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                30300000
303100     PERFORM P8300-START-TASK-FILE                                30310000
303200     IF SUCCESS                                                   30320000
303300        SET TASK-NOT-DONE TO TRUE                                 30330000
303400        PERFORM P8310-READNEXT-TASK-FILE                          30340000
303500        PERFORM UNTIL TASK-DONE                                   30350000
303600           IF SUCCESS                                             30360000
303700              AND EMP-NBR OF WS-MSTR = EMP-NBR OF WS-TASK         30370000
303800              IF TASK-LAYOFF-MARKUP                               30380000
303900*C979-START                                                       30390018
304000                 IF TASK-LO1 = 'A'                                30400018
304200*C979-END                                                         30420018
304300                    SET TASK-DONE TO TRUE                         30430018
304400                    MOVE EFF-MO OF WS-TASK TO WS-RETURN-DATE-MM   30440018
304500                    MOVE EFF-DY OF WS-TASK TO WS-RETURN-DATE-DD   30450018
304600                 ELSE                                             30460018
304700                    PERFORM P8310-READNEXT-TASK-FILE              30470018
304800*C979-START                                                       30480018
304900                 END-IF                                           30490018
305000*C979-END                                                         30500018
305001*C1073-START                                                      30500128
305002              ELSE                                                30500226
305010                 PERFORM P8310-READNEXT-TASK-FILE                 30501026
305020*C1073-START                                                      30502028
305100              END-IF                                              30510018
305200           ELSE                                                   30520000
305300              SET TASK-DONE TO TRUE                               30530000
305400           END-IF                                                 30540000
305500        END-PERFORM                                               30550000
305600     END-IF                                                       30560000
305700     PERFORM P8320-ENDBR-TASK-FILE                                30570000
305800     .                                                            30580000
305900                                                                  30590000
306000*                                                                 30600000
306100 P7000-WRITE-TSQUEUE.                                             30610000
306200*                                                                 30620000
306300*                                                                 30630000
306400*      WRITE MAP TSQUEUE                                          30640000
306500*                                                                 30650000
306600     EXEC CICS ASSIGN                                             30660000
306700          EXTDS(WS-CICS-EXTDS-CODE)                               30670000
306800     END-EXEC                                                     30680000
306900*                                                                 30690000
307000     IF SCREEN-HAS-EXT-ATTR                                       30700000
307100        EXEC CICS SEND STRFIELD                                   30710000
307200                  FROM(WS-STRFIELD)                               30720000
307300                  LENGTH(WS-STRFIELD-LGTH)                        30730000
307400                  RESP(WS-RESPONSE)                               30740000
307500        END-EXEC                                                  30750000
307600        MOVE WS-RESPONSE           TO FILE-STATUS                 30760000
307700        IF NOT SUCCESS                                            30770000
307800           MOVE 'P7000-1'          TO ERR-PARAGRAPH               30780000
307900           MOVE 'SEND STRFIELD'    TO ERR-KEY                     30790000
308000           PERFORM P9999-GOT-PROBLEM                              30800000
308100        END-IF                                                    30810000
308200     END-IF                                                       30820000
308300*                                                                 30830000
308400     MOVE LENGTH OF WS-BUFFER-DATA TO WS-BUFFER-LGTH              30840000
308500     EXEC CICS RECEIVE BUFFER                                     30850000
308600               INTO(WS-BUFFER-DATA)                               30860000
308700               LENGTH(WS-BUFFER-LGTH)                             30870000
308800               RESP(WS-RESPONSE)                                  30880000
308900     END-EXEC                                                     30890000
309000     MOVE WS-RESPONSE              TO FILE-STATUS                 30900000
309100     IF NOT SUCCESS AND NOT EOC                                   30910000
309200        MOVE 'P7000-2'             TO ERR-PARAGRAPH               30920000
309300        MOVE 'RECEIVE BUFFER'      TO ERR-KEY                     30930000
309400        PERFORM P9999-GOT-PROBLEM                                 30940000
309500     END-IF                                                       30950000
309600     MOVE EIBCPOSN                 TO WS-BUFFER-CURSOR            30960000
309700                                                                  30970000
309800                                                                  30980000
309900     MOVE LENGTH OF WS-BUFFER-AREA TO P02DTSQ-QLGTH               30990000
310000     MOVE EIBTRMID                 TO P02DTSQ-MAP-TERM-ID         31000000
310100     EXEC CICS WRITEQ TS                                          31010000
310200               QUEUE(P02DTSQ-MAP-QUEUE-ID)                        31020000
310300               FROM(WS-BUFFER-AREA)                               31030000
310400               LENGTH(P02DTSQ-QLGTH)                              31040000
310500               RESP(WS-RESPONSE)                                  31050000
310600     END-EXEC                                                     31060000
310700     MOVE WS-RESPONSE              TO FILE-STATUS                 31070000
310800     IF NOT SUCCESS                                               31080000
310900        MOVE 'P7000-3'             TO ERR-PARAGRAPH               31090000
311000        PERFORM P9999-GOT-PROBLEM                                 31100000
311100     END-IF                                                       31110000
311200     MOVE EIBTRMID TO P02DTSQ-CA-TERM-ID                          31120000
311300     EXEC CICS WRITEQ TS                                          31130000
311400               QUEUE(P02DTSQ-CA-QUEUE-ID)                         31140000
311500               FROM(PSTCOMM-AREA)                                 31150000
311600               LENGTH(P02D-COMM-LGTH)                             31160000
311700               RESP(WS-RESPONSE)                                  31170000
311800     END-EXEC                                                     31180000
311900     MOVE WS-RESPONSE TO FILE-STATUS                              31190000
312000     IF NOT SUCCESS                                               31200000
312100        MOVE 'P7000-4'             TO ERR-PARAGRAPH               31210000
312200        PERFORM P9999-GOT-PROBLEM                                 31220000
312300     END-IF.                                                      31230000
312400*                                                                 31240000
312500 P7010-READ-TSQUEUE.                                              31250000
312600*                                                                 31260000
312700*              READ THE MAPS TSQUEUE                              31270000
312800*                                                                 31280000
312900     MOVE LENGTH OF WS-BUFFER-AREA TO P02DTSQ-QLGTH               31290000
313000     MOVE EIBTRMID                 TO P02DTSQ-MAP-TERM-ID         31300000
313100     EXEC CICS READQ TS                                           31310000
313200               QUEUE(P02DTSQ-MAP-QUEUE-ID)                        31320000
313300               INTO(WS-BUFFER-AREA)                               31330000
313400               LENGTH(P02DTSQ-QLGTH)                              31340000
313500               ITEM(P02DTSQ-QUEUE-ITEM)                           31350000
313600               RESP(WS-RESPONSE)                                  31360000
313700     END-EXEC                                                     31370000
313800     MOVE WS-RESPONSE              TO FILE-STATUS                 31380000
313900     IF SUCCESS                                                   31390000
314000        SET SEND-BUFFER            TO TRUE                        31400000
314100     ELSE                                                         31410000
314200        SET CREATE-SCREEN          TO TRUE                        31420000
314300        MOVE LOW-VALUES            TO PSTS02D                     31430000
314400     END-IF                                                       31440000
314500     MOVE EIBTRMID TO P02DTSQ-CA-TERM-ID                          31450000
314600     EXEC CICS READQ TS                                           31460000
314700               QUEUE(P02DTSQ-CA-QUEUE-ID)                         31470000
314800               INTO(PSTCOMM-AREA)                                 31480000
314900               LENGTH(P02D-COMM-LGTH)                             31490000
315000               ITEM(P02DTSQ-QUEUE-ITEM)                           31500000
315100               RESP(WS-RESPONSE)                                  31510000
315200     END-EXEC                                                     31520000
315300     MOVE WS-RESPONSE TO FILE-STATUS                              31530000
315400     IF NOT SUCCESS                                               31540000
315500        MOVE SPACES TO PSTCOMM-AREA                               31550000
315600     END-IF                                                       31560000
315700     PERFORM P7020-DELETE-TSQUEUE.                                31570000
315800*                                                                 31580000
315900 P7020-DELETE-TSQUEUE.                                            31590000
316000*                                                                 31600000
316100     MOVE EIBTRMID TO P02DTSQ-MAP-TERM-ID                         31610000
316200     EXEC CICS DELETEQ TS                                         31620000
316300               QUEUE(P02DTSQ-MAP-QUEUE-ID)                        31630000
316400               RESP(WS-RESPONSE)                                  31640000
316500     END-EXEC                                                     31650000
316600     MOVE EIBTRMID TO P02DTSQ-CA-TERM-ID                          31660000
316700     EXEC CICS DELETEQ TS                                         31670000
316800               QUEUE(P02DTSQ-CA-QUEUE-ID)                         31680000
316900               RESP(WS-RESPONSE)                                  31690000
317000     END-EXEC.                                                    31700000
317100*                                                                 31710000
317200 P8080-LINK-942.                                                  31720000
317300*                                                                 31730000
317400     EXEC CICS LINK                                               31740000
317500               PROGRAM(P942-PGM)                                  31750000
317600               COMMAREA(P942-COMMAREA-PARMS)                      31760000
317700               LENGTH(P942-LGTH)                                  31770000
317800               RESP(WS-RESPONSE)                                  31780000
317900     END-EXEC                                                     31790000
318000     MOVE WS-RESPONSE     TO FILE-STATUS                          31800000
318100     IF NOT SUCCESS                                               31810000
318200        MOVE 'P8080-1'    TO ERR-PARAGRAPH                        31820000
318300        MOVE 'P942LINK'   TO ERR-KEY                              31830000
318400        PERFORM P9999-GOT-PROBLEM                                 31840000
318500     END-IF.                                                      31850000
318600*                                                                 31860000
318700 P8300-START-TASK-FILE.                                           31870000
318800*                                                                 31880000
318900     MOVE TASK-EMPLOYEE-KEY TO TASKEMPK                           31890000
319000     EXEC CICS STARTBR                                            31900000
319100               DATASET(TASK-VIA-EMP-NBR)                          31910000
319200               RIDFLD(TASKEMPK)                                   31920000
319300               GTEQ                                               31930000
319400               RESP(WS-RESPONSE)                                  31940000
319500     END-EXEC                                                     31950000
319600     MOVE WS-RESPONSE TO FILE-STATUS                              31960000
319700     IF NOT SUCCESS                                               31970000
319800        IF NO-RECORD-FND OR END-OF-FILE                           31980000
319900           CONTINUE                                               31990000
320000        ELSE                                                      32000000
320100           MOVE 'P8300-1' TO ERR-PARAGRAPH                        32010000
320200           MOVE TASKEMPK TO ERR-KEY                               32020000
320300           PERFORM P9999-GOT-PROBLEM                              32030000
320400        END-IF                                                    32040000
320500     END-IF                                                       32050000
320600     .                                                            32060000
320700                                                                  32070000
320800***************************************************************** 32080000
320900 P8310-READNEXT-TASK-FILE.                                        32090000
321000***************************************************************** 32100000
321100     EXEC CICS READNEXT                                           32110000
321200               DATASET(TASK-VIA-EMP-NBR)                          32120000
321300               INTO(WS-TASK)                                      32130000
321400               LENGTH(TASKENBR-RLGTH)                             32140000
321500               RIDFLD(TASKEMPK)                                   32150000
321600               KEYLENGTH(TASKENBR-KLGTH)                          32160000
321700               RESP(WS-RESPONSE)                                  32170000
321800     END-EXEC                                                     32180000
321900     MOVE WS-RESPONSE TO FILE-STATUS                              32190000
322000     IF NOT SUCCESS                                               32200000
322100        IF NO-RECORD-FND OR END-OF-FILE                           32210000
322200           CONTINUE                                               32220000
322300        ELSE                                                      32230000
322400           MOVE 'P8310-1' TO ERR-PARAGRAPH                        32240000
322500           MOVE TASKEMPK TO ERR-KEY                               32250000
322600           PERFORM P9999-GOT-PROBLEM                              32260000
322700        END-IF                                                    32270000
322800     END-IF                                                       32280000
322900     .                                                            32290000
323000                                                                  32300000
323100***************************************************************** 32310000
323200 P8320-ENDBR-TASK-FILE.                                           32320000
323300***************************************************************** 32330000
323400     EXEC CICS ENDBR                                              32340000
323500               DATASET(TASK-VIA-EMP-NBR)                          32350000
323600               RESP(WS-RESPONSE)                                  32360000
323700     END-EXEC                                                     32370000
323800     MOVE WS-RESPONSE TO FILE-STATUS                              32380000
323900     IF NOT SUCCESS                                               32390000
324000        MOVE 'P8320-1'  TO ERR-PARAGRAPH                          32400000
324100        MOVE TASKEMPK   TO ERR-KEY                                32410000
324200        PERFORM P9999-GOT-PROBLEM                                 32420000
324300     END-IF                                                       32430000
324400     .                                                            32440000
324500                                                                  32450000
324600*                                                                 32460000
324700 P8500-READ-MASTER.                                               32470000
324800*                                                                 32480000
324900     EXEC CICS READ                                               32490000
325000               DATASET(MSTR-VIA-EMP-NBR)                          32500000
325100               INTO(WS-MSTR)                                      32510000
325200               LENGTH(MSTRENBR-RLGTH)                             32520000
325300               RIDFLD(MSTRNBRK)                                   32530000
325400               KEYLENGTH(MSTRENBR-KLGTH)                          32540000
325500               RESP(WS-RESPONSE)                                  32550000
325600     END-EXEC                                                     32560000
325700     MOVE WS-RESPONSE TO FILE-STATUS                              32570000
325800     IF SUCCESS                                                   32580000
325900        PERFORM P8510-READ-MASTER-JOBS                            32590000
326000     ELSE                                                         32600000
326100        MOVE 'P8500' TO ERR-PARAGRAPH                             32610000
326200        MOVE MSTRNBRK TO ERR-KEY                                  32620000
326300        PERFORM P9999-GOT-PROBLEM                                 32630000
326400     END-IF.                                                      32640000
326500*                                                                 32650000
326600 P8510-READ-MASTER-JOBS.                                          32660000
326700*                                                                 32670000
326800     MOVE SPACES                 TO WS-ASGN-FILE                  32680000
326900     MOVE EMP-NBR OF WS-MSTR     TO WK-ASGN-EMP-NO                32690000
327000     PERFORM PXXXX-JOB-OWNED                                      32700000
327100     MOVE ASGN-JOB-TYPE          TO NORMAL-ASGNMT-FLAG            32710000
327200     MOVE ASGN-ASSIGNMENT        TO NORMAL-ASGNMT                 32720000
327300     MOVE SPACES                 TO WS-ASGN-FILE                  32730000
327400     PERFORM PXXXX-LATEST-TEMP-JOB                                32740000
327500     MOVE ASGN-JOB-TYPE          TO TEMPORARY-ASGNMT-FLAG         32750000
327600     MOVE SPACES                 TO TEMP-ASGN-XB-AUG-FLAG         32760000
327700     IF ASGN-JOB-TYPE = 'X'                                       32770000
327800        AND AUGMENTED-TO-EXTRA-BOARD                              32780000
327900        SET TEMP-ASGN-XB-AUG    TO TRUE                           32790000
328000     END-IF                                                       32800000
328100     MOVE ASGN-ASSIGNMENT        TO TEMPORARY-ASGNMT              32810000
328200     MOVE SPACE                  TO WS-ASGN-FILE                  32820000
328300     PERFORM PXXXX-JOB-ON-DUTY                                    32830000
328400     MOVE ASGN-JOB-TYPE          TO ON-DUTY-ASGNMT-FLAG           32840000
328500     MOVE ASGN-ASSIGNMENT        TO ON-DUTY-ASGNMT                32850000
328600     MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE.        32860000
328700                                                                  32870000
328800*************************************************************     32880000
328900 P8700-CALL-DATE-ROUTINE.                                         32890000
329000*                       NORTHERN QUEBEC SPAREBOARD - PHASE 2      32900000
329100*************************************************************     32910000
329200     EXEC CICS LINK                                               32920000
329300               PROGRAM(P903-PGM)                                  32930000
329400               COMMAREA(DATE-CONVERSION-PARMS)                    32940000
329500               LENGTH(P903-LGTH)                                  32950000
329600               RESP(WS-RESPONSE)                                  32960000
329700     END-EXEC                                                     32970000
329800     MOVE WS-RESPONSE     TO FILE-STATUS                          32980000
329900     IF NOT SUCCESS                                               32990000
330000        MOVE 'P8700-0'    TO ERR-PARAGRAPH                        33000000
330100        MOVE 'P903'       TO ERR-KEY                              33010000
330200        PERFORM P9999-GOT-PROBLEM                                 33020000
330300     END-IF.                                                      33030000
330400                                                                  33040000
330500*                                                                 33050000
330600 P8800-GET-CURRENT-TIME.                                          33060000
330700*                                                                 33070000
330800     EXEC CICS ASKTIME                                            33080000
330900               ABSTIME(WS-ABSTIME)                                33090000
331000     END-EXEC                                                     33100000
331100     ADD WS-ABSTIME-OFFSET  TO WS-ABSTIME                         33110000
331200     EXEC CICS FORMATTIME                                         33120000
331300               ABSTIME(WS-ABSTIME)                                33130000
331400               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      33140000
331500               TIME(WS-SYSTEM-TIME-AREA)                          33150000
331600     END-EXEC                                                     33160000
331700*                                                                 33170000
331800*    INSTALL APPLICATION DATE/TIME                                33180000
331900*                                                                 33190000
332000     IF PSTCA-DATE-TIME-OFFSET > SPACES                           33200000
332100        MOVE ZEROS          TO DATE-CONVERSION-PARMS              33210000
332200        MOVE WS-SYSTEM-DATE TO PARM-PRI-DATE-GREG                 33220000
332300        MOVE WS-SYSTEM-TIME TO PARM-PRI-HRMN                      33230000
332400        PERFORM P9810-PROCESS-OFFSET                              33240000
332500        MOVE PARM-RES-DATE-GREG                                   33250000
332600                            TO WS-SYSTEM-DATE                     33260000
332700        MOVE PARM-RES-GREG-CENT                                   33270000
332800                            TO WS-SYSTEM-CENT                     33280000
332900        MOVE PARM-RES-HRMN                                        33290000
333000                            TO WS-SYSTEM-TIME                     33300000
333100     END-IF                                                       33310000
333200*    SET DE-YYMMDD-FORMAT   TO TRUE                               33320000
333300*    MOVE WS-SYSTEM-DATE    TO DE-YYMMDD                          33330000
333400*    PERFORM P8998-DATEEDIT                                       33340000
333500*    MOVE DE-YYMMDD-CE      TO WS-SYSTEM-CENT                     33350000
333600*                                                                 33360000
333700*    CONVERT SYSTEM TIME TO LOCAL TIME                            33370000
333800*                                                                 33380000
333900     MOVE SPACES            TO TZ-PARAMETERS                      33390000
334000     SET TZ-IN-EASTERN-ZONE TO TRUE                               33400000
334100     MOVE WS-PRESENT-TIME   TO TZ-IN-DATE-TIME                    33410000
334200     MOVE PSTCA-TIME-ZONE   TO TZ-OUT-ZONE                        33420000
334300     PERFORM P8996-TIMEZONE                                       33430000
334400     MOVE TZ-OUT-DATE-TIME  TO WS-LOCAL-DATE-TIME                 33440000
334500     MOVE TZ-OUT-CE         TO WS-LOCAL-CENT                      33450000
334600                                                                  33460000
334700     MOVE ZEROS TO DATE-CONVERSION-PARMS                          33470000
334800     SET PARM-CONV TO TRUE                                        33480000
334900     MOVE WS-LOCAL-DATE TO PARM-PRI-DATE-GREG                     33490000
335000     PERFORM P8700-CALL-DATE-ROUTINE                              33500000
335100     MOVE PARM-PRI-DAY-OF-WEEK TO WS-TODAY                        33510000
335200     MOVE WS-LOCAL-DATE-TIME TO WS-VIEW-DATE-TIME                 33520000
335300     MOVE WS-LOCAL-CENT      TO WS-VIEW-CENT                      33530000
335400     .                                                            33540000
335500*                                                                 33550000
335600 PXXXX-JOB-OWNER.                                                 33560000
335700*                                                                 33570000
335800     MOVE WORK-ASGNKEY1 TO ASGNKEY1                               33580000
335900     SET ASGN-OWNER-REC TO TRUE                                   33590000
336000     MOVE ZERO          TO ASGN-DATE-TIME                         33600000
336100     MOVE ASGNKEY1      TO ASGNJOB                                33610000
336200     EXEC CICS READ                                               33620000
336300               DATASET(ASGN-VIA-ASGNJOB)                          33630000
336400               INTO(ASGN-AREA)                                    33640000
336500               LENGTH(ASGNJOB-RLGTH)                              33650000
336600               RIDFLD(ASGNJOB)                                    33660000
336700               KEYLENGTH(ASGNJOB-KLGTH)                           33670000
336800               RESP(WS-RESPONSE)                                  33680000
336900     END-EXEC                                                     33690000
337000     MOVE WS-RESPONSE TO FILE-STATUS                              33700000
337100     IF NOT SUCCESS                                               33710000
337200        MOVE ZEROS TO ASGN-EMP-NO                                 33720000
337300     END-IF.                                                      33730000
337400*                                                                 33740000
337500 PXXXX-LATEST-TEMP.                                               33750000
337600*                                                                 33760000
337700     MOVE SPACES        TO WS-SAVE-ASGN-FILE                      33770000
337800     MOVE WORK-ASGNKEY1 TO ASGNKEY1                               33780000
337900     SET ASGN-TEMP-REC  TO TRUE                                   33790000
338000     MOVE ZERO          TO ASGN-DATE-TIME                         33800000
338100     MOVE ASGNKEY1      TO ASGNJOB                                33810000
338200     EXEC CICS STARTBR                                            33820000
338300               DATASET(ASGN-VIA-ASGNJOB)                          33830000
338400               RIDFLD(ASGNJOB)                                    33840000
338500               GTEQ                                               33850000
338600               RESP(WS-RESPONSE)                                  33860000
338700     END-EXEC                                                     33870000
338800     MOVE WS-RESPONSE TO FILE-STATUS                              33880000
338900     IF SUCCESS                                                   33890000
339000        MOVE 'N' TO WS-ASGN-DONE-CODE                             33900000
339100        PERFORM UNTIL ASGN-DONE                                   33910000
339200           EXEC CICS READNEXT                                     33920000
339300                     DATASET(ASGN-VIA-ASGNJOB)                    33930000
339400                     INTO(ASGN-AREA)                              33940000
339500                     LENGTH(ASGNJOB-RLGTH)                        33950000
339600                     RIDFLD(ASGNJOB)                              33960000
339700                     KEYLENGTH(ASGNJOB-KLGTH)                     33970000
339800                     RESP(WS-RESPONSE)                            33980000
339900           END-EXEC                                               33990000
340000           MOVE WS-RESPONSE TO FILE-STATUS                        34000000
340100           IF SUCCESS                                             34010000
340200              IF WK-ASGN-DIST = ASGN-DIST                         34020000
340300                 AND WK-ASGN-SUB-DIST = ASGN-SUB-DIST             34030000
340400                 AND WK-SWASSGN-ASGN = ASGN-AJ-JOB                34040000
340500                     OF ASGN-ASSIGNMENT                           34050000
340600                 AND ASGN-TEMP-REC                                34060000
340700                 MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE              34070000
340800              ELSE                                                34080000
340900                 SET ASGN-DONE TO TRUE                            34090000
341000              END-IF                                              34100000
341100           ELSE                                                   34110000
341200              SET ASGN-DONE TO TRUE                               34120000
341300           END-IF                                                 34130000
341400        END-PERFORM                                               34140000
341500        EXEC CICS ENDBR                                           34150000
341600                  DATASET(ASGN-VIA-ASGNJOB)                       34160000
341700                  RESP(WS-RESPONSE)                               34170000
341800        END-EXEC                                                  34180000
341900     END-IF                                                       34190000
342000     IF WS-SAVE-ASGN-FILE > SPACE                                 34200000
342100        MOVE WS-SAVE-ASGN-FILE TO ASGN-AREA                       34210000
342200     ELSE                                                         34220000
342300        MOVE ZEROS TO ASGN-EMP-NO                                 34230000
342400     END-IF.                                                      34240000
342500*                                                                 34250000
342600 PXXXX-ON-DUTY-EMP.                                               34260000
342700*                                                                 34270000
342800     MOVE WORK-ASGNKEY1   TO ASGNKEY1                             34280000
342900     SET ASGN-ON-DUTY-REC TO TRUE                                 34290000
343000     MOVE ZERO            TO ASGN-DATE-TIME                       34300000
343100     MOVE ASGNKEY1        TO ASGNJOB                              34310000
343200     EXEC CICS READ                                               34320000
343300               DATASET(ASGN-VIA-ASGNJOB)                          34330000
343400               INTO(ASGN-AREA)                                    34340000
343500               LENGTH(ASGNJOB-RLGTH)                              34350000
343600               RIDFLD(ASGNJOB)                                    34360000
343700               KEYLENGTH(ASGNJOB-KLGTH)                           34370000
343800               RESP(WS-RESPONSE)                                  34380000
343900     END-EXEC                                                     34390000
344000     MOVE WS-RESPONSE TO FILE-STATUS                              34400000
344100     IF NOT SUCCESS                                               34410000
344200        MOVE ZEROS TO ASGN-EMP-NO                                 34420000
344300     END-IF.                                                      34430000
344400*                                                                 34440000
344500 PXXXX-JOB-OWNED.                                                 34450000
344600*                                                                 34460000
344700                                                                  34470000
344800     MOVE WORK-ASGNKEY2 TO ASGNKEY2                               34480000
344900     MOVE '1'           TO ASGN-EMP-NO-REC-TYPE                   34490000
345000     MOVE ZERO          TO ASGN-EMP-DATE-TIME                     34500000
345100     MOVE ASGNKEY2      TO ASGNEMP                                34510000
345200     EXEC CICS READ                                               34520000
345300               DATASET(ASGN-VIA-ASGNEMP)                          34530000
345400               INTO(WS-ASGN-FILE)                                 34540000
345500               LENGTH(ASGNEMP-RLGTH)                              34550000
345600               RIDFLD(ASGNEMP)                                    34560000
345700               KEYLENGTH(ASGNEMP-KLGTH)                           34570000
345800               RESP(WS-RESPONSE)                                  34580000
345900     END-EXEC                                                     34590000
346000     MOVE WS-RESPONSE TO FILE-STATUS                              34600000
346100     IF NOT SUCCESS                                               34610000
346200        MOVE SPACES TO WS-ASGN-FILE                               34620000
346300     END-IF.                                                      34630000
346400*                                                                 34640000
346500 PXXXX-LATEST-TEMP-JOB.                                           34650000
346600*                                                                 34660000
346700     MOVE WORK-ASGNKEY2 TO ASGNKEY2                               34670000
346800     MOVE '2'           TO ASGN-EMP-NO-REC-TYPE                   34680000
346900     MOVE ZERO          TO ASGN-EMP-DATE-TIME                     34690000
347000     MOVE ASGNKEY2      TO ASGNEMP                                34700000
347100     MOVE SPACES        TO WS-ASGN-FILE                           34710000
347200                           WS-SAVE-ASGN-FILE                      34720000
347300     EXEC CICS STARTBR                                            34730000
347400               DATASET(ASGN-VIA-ASGNEMP)                          34740000
347500               RIDFLD(ASGNEMP)                                    34750000
347600               GTEQ                                               34760000
347700               RESP(WS-RESPONSE)                                  34770000
347800     END-EXEC                                                     34780000
347900     MOVE WS-RESPONSE TO FILE-STATUS                              34790000
348000     IF SUCCESS                                                   34800000
348100        MOVE 'N' TO WS-ASGN-DONE-CODE                             34810000
348200        PERFORM UNTIL ASGN-DONE                                   34820000
348300           EXEC CICS READNEXT                                     34830000
348400                     DATASET(ASGN-VIA-ASGNEMP)                    34840000
348500                     INTO(ASGN-AREA)                              34850000
348600                     LENGTH(ASGNEMP-RLGTH)                        34860000
348700                     RIDFLD(ASGNEMP)                              34870000
348800                     KEYLENGTH(ASGNEMP-KLGTH)                     34880000
348900                     RESP(WS-RESPONSE)                            34890000
349000           END-EXEC                                               34900000
349100           MOVE WS-RESPONSE TO FILE-STATUS                        34910000
349200           IF SUCCESS                                             34920000
349300              IF ASGN-EMP-NO = WK-ASGN-EMP-NO                     34930000
349400                 AND ASGN-EMP-NO-REC-TYPE = '2'                   34940000
349500                 MOVE ASGN-AREA TO WS-SAVE-ASGN-FILE              34950000
349600              ELSE                                                34960000
349700                 SET ASGN-DONE TO TRUE                            34970000
349800              END-IF                                              34980000
349900           ELSE                                                   34990000
350000              SET ASGN-DONE TO TRUE                               35000000
350100           END-IF                                                 35010000
350200        END-PERFORM                                               35020000
350300        EXEC CICS ENDBR                                           35030000
350400                  DATASET(ASGN-VIA-ASGNEMP)                       35040000
350500                  RESP(WS-RESPONSE)                               35050000
350600        END-EXEC                                                  35060000
350700     END-IF                                                       35070000
350800     IF WS-SAVE-ASGN-FILE > SPACES                                35080000
350900        MOVE WS-SAVE-ASGN-FILE TO WS-ASGN-FILE                    35090000
351000     ELSE                                                         35100000
351100        MOVE SPACES TO WS-ASGN-FILE                               35110000
351200     END-IF.                                                      35120000
351300*                                                                 35130000
351400 PXXXX-JOB-ON-DUTY.                                               35140000
351500*                                                                 35150000
351600     MOVE WORK-ASGNKEY2 TO ASGNKEY2                               35160000
351700     MOVE '3' TO ASGN-EMP-NO-REC-TYPE                             35170000
351800     MOVE ZERO TO ASGN-EMP-DATE-TIME                              35180000
351900     MOVE ASGNKEY2 TO ASGNEMP                                     35190000
352000     EXEC CICS READ                                               35200000
352100               DATASET(ASGN-VIA-ASGNEMP)                          35210000
352200               INTO(ASGN-AREA)                                    35220000
352300               LENGTH(ASGNEMP-RLGTH)                              35230000
352400               RIDFLD(ASGNEMP)                                    35240000
352500               KEYLENGTH(ASGNEMP-KLGTH)                           35250000
352600               RESP(WS-RESPONSE)                                  35260000
352700     END-EXEC                                                     35270000
352800     MOVE WS-RESPONSE TO FILE-STATUS                              35280000
352900     IF NOT SUCCESS                                               35290000
353000        MOVE SPACES TO WS-ASGN-FILE                               35300000
353100     END-IF.                                                      35310000
353200*                                                                 35320000
353300 COPY CNTRTXT.                                                    35330000
353400*                                                                 35340000
353500 COPY TIMEZONE.                                                   35350000
353600*                                                                 35360000
353700 COPY DATEEDIT.                                                   35370000
353800*                                                                 35380000
353900 COPY BIFEDIT.                                                    35390000
354000*                                                                 35400000
354100 P9000-SEND-MAP-AND-RETURN.                                       35410000
354200*                                                                 35420000
354300     IF MSGLOG-CODE > SPACES                                      35430000
354400         PERFORM P9030-GET-MESSAGE                                35440000
354500         MOVE MSGLOG-MESSAGE-AREA TO SCR02D-ERRORMSG              35450000
354600     END-IF                                                       35460000
354700                                                                  35470000
354800     MOVE P02D-MAP-VERSION(PSTCA-SUB) TO P02D-MAP                 35480000
354900     IF CREATE-SCREEN                                             35490000
355000        PERFORM P9010-SEND-PHYSICAL-MAP                           35500000
355100     ELSE                                                         35510000
355200        IF CONTINUE-SCREEN                                        35520000
355300           PERFORM P9020-SEND-DATAONLY-MAP                        35530000
355400        ELSE                                                      35540000
355500           PERFORM P9035-SEND-BUFFER                              35550000
355600        END-IF                                                    35560000
355700     END-IF                                                       35570000
355800     EXEC CICS RETURN                                             35580000
355900               TRANSID(P02D-TRAN)                                 35590000
356000               COMMAREA(PSTCOMM-AREA)                             35600000
356100               LENGTH(P02D-COMM-LGTH)                             35610000
356200     END-EXEC.                                                    35620000
356300*                                                                 35630000
356400 P9010-SEND-PHYSICAL-MAP.                                         35640000
356500*                                                                 35650000
356600     EXEC CICS SEND MAP(P02D-MAP)                                 35660000
356700                    MAPSET(P02D-SET)                              35670000
356800                    FROM(PSTS02D)                                 35680000
356900                    CURSOR                                        35690000
357000                    FREEKB                                        35700000
357100                    ERASE                                         35710000
357200                    RESP(WS-RESPONSE)                             35720000
357300     END-EXEC                                                     35730000
357400     MOVE WS-RESPONSE TO FILE-STATUS                              35740000
357500     IF NOT SUCCESS                                               35750000
357600        MOVE 'P9010'   TO ERR-PARAGRAPH                           35760000
357700        PERFORM P9999-GOT-PROBLEM                                 35770000
357800     END-IF.                                                      35780000
357900*                                                                 35790000
358000 P9020-SEND-DATAONLY-MAP.                                         35800000
358100*                                                                 35810000
358200     EXEC CICS SEND MAP(P02D-MAP)                                 35820000
358300                    MAPSET(P02D-SET)                              35830000
358400                    FROM(PSTS02D)                                 35840000
358500                    DATAONLY                                      35850000
358600                    CURSOR                                        35860000
358700                    FREEKB                                        35870000
358800                    RESP(WS-RESPONSE)                             35880000
358900     END-EXEC                                                     35890000
359000     MOVE WS-RESPONSE TO FILE-STATUS                              35900000
359100     IF NOT SUCCESS                                               35910000
359200        MOVE 'P9020' TO ERR-PARAGRAPH                             35920000
359300        PERFORM P9999-GOT-PROBLEM                                 35930000
359400     END-IF.                                                      35940000
359500*                                                                 35950000
359600 P9030-GET-MESSAGE.                                               35960000
359700*                                                                 35970000
359800     MOVE PSTCA-SUB TO MSGLOG-SUB-CODE                            35980000
359900     EXEC CICS READ                                               35990000
360000               DATASET(MSGLOG-VIA-CODE)                           36000000
360100               INTO(MSGLOG-AREA)                                  36010000
360200               LENGTH(MSGLOG-RLGTH)                               36020000
360300               RIDFLD(MSGLOG-KEY)                                 36030000
360400               KEYLENGTH(MSGLOG-KLGTH)                            36040000
360500               RESP(WS-RESPONSE)                                  36050000
360600     END-EXEC                                                     36060000
360700     MOVE WS-RESPONSE TO FILE-STATUS                              36070000
360800     IF NOT SUCCESS                                               36080000
360900        IF PSTCA-SUB = 1                                          36090000
361000           MOVE 'NO MESSAGE ON FILE' TO MSGLOG-MESSAGE            36100000
361100        ELSE                                                      36110000
361200           MOVE 'AUCUN MESSAGE'      TO MSGLOG-MESSAGE            36120000
361300        END-IF                                                    36130000
361400     END-IF                                                       36140000
361500     MOVE MSGLOG-CODE     TO MSGLOG-MSG-CODE                      36150000
361600     MOVE '-'             TO MSGLOG-MSG-SEP                       36160000
361700     MOVE MSGLOG-SUB-CODE TO MSGLOG-MSG-SUB-CODE.                 36170000
361800*                                                                 36180000
361900 P9035-SEND-BUFFER.                                               36190000
362000*                                                                 36200000
362100     EXEC CICS SEND                                               36210000
362200               FROM(WS-BUFFER-DATA)                               36220000
362300               LENGTH(WS-BUFFER-LGTH)                             36230000
362400               ERASE                                              36240000
362500               RESP(WS-RESPONSE)                                  36250000
362600     END-EXEC                                                     36260000
362700     MOVE WS-RESPONSE       TO FILE-STATUS                        36270000
362800     IF NOT SUCCESS                                               36280000
362900        MOVE 'P9035-1'      TO ERR-PARAGRAPH                      36290000
363000        MOVE 'SEND BUFFER'  TO ERR-KEY                            36300000
363100        PERFORM P9999-GOT-PROBLEM                                 36310000
363200     END-IF                                                       36320000
363300     EXEC CICS SEND                                               36330000
363400               CONTROL                                            36340000
363500               CURSOR(WS-BUFFER-CURSOR)                           36350000
363600               RESP(WS-RESPONSE)                                  36360000
363700     END-EXEC                                                     36370000
363800     MOVE WS-RESPONSE       TO FILE-STATUS                        36380000
363900     IF NOT SUCCESS                                               36390000
364000        MOVE 'P9035-2'      TO ERR-PARAGRAPH                      36400000
364100        MOVE 'SEND CURSOR'  TO ERR-KEY                            36410000
364200        PERFORM P9999-GOT-PROBLEM                                 36420000
364300     END-IF.                                                      36430000
364400*                                                                 36440000
364500 P9100-SETUP-SCR02.                                               36450000
364600*                                                                 36460000
364700     MOVE SPACES TO PSTCA-VARIABLE-AREA                           36470000
364800     EXEC CICS XCTL                                               36480000
364900               PROGRAM(P02-PGM)                                   36490000
365000               COMMAREA(PSTCOMM-AREA)                             36500000
365100               LENGTH(PSTCOMM-LGTH)                               36510000
365200               RESP(WS-RESPONSE)                                  36520000
365300     END-EXEC                                                     36530000
365400     MOVE WS-RESPONSE TO FILE-STATUS                              36540000
365500     IF NOT SUCCESS                                               36550000
365600        MOVE 'P9100' TO ERR-PARAGRAPH                             36560000
365700        PERFORM P9999-GOT-PROBLEM                                 36570000
365800     END-IF.                                                      36580000
365900*                                                                 36590000
366000 P9500-SETUP-SCR998.                                              36600000
366100*                                                                 36610000
366200     MOVE SPACES            TO P998COMM-AREA                      36620000
366300     MOVE P02D-PGM          TO P998CA-FROM-PROGRAM                36630000
366400     MOVE P02D-MAP          TO P998CA-SCREEN-ID                   36640000
366500     MOVE EIBCPOSN          TO P998CA-CURSOR-POS                  36650000
366600     EXEC CICS XCTL                                               36660000
366700               PROGRAM(P998-PGM)                                  36670000
366800               COMMAREA(PSTCOMM-AREA)                             36680000
366900               LENGTH(PSTCOMM-LGTH)                               36690000
367000               RESP(WS-RESPONSE)                                  36700000
367100     END-EXEC                                                     36710000
367200     MOVE WS-RESPONSE       TO FILE-STATUS                        36720000
367300     IF NOT SUCCESS                                               36730000
367400        MOVE 'P9500'        TO ERR-PARAGRAPH                      36740000
367500        PERFORM P9999-GOT-PROBLEM                                 36750000
367600     END-IF.                                                      36760000
367700*                                                                 36770000
367800 P9810-PROCESS-OFFSET.                                            36780000
367900*                                                                 36790000
368000     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE                 36800000
368100     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY            36810000
368200     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN                  36820000
368300     PERFORM P8700-CALL-DATE-ROUTINE                              36830000
368400     .                                                            36840000
368500*                                                                 36850000
368600 P9820-SNAPSHOT-XB.                                               36860000
368700*                                                                 36870000
368800     MOVE SPACES                TO P913-COMMAREA-PARMS            36880000
368900     SET P913-SNAPSHOT-FUNCTION TO TRUE                           36890000
369000     MOVE WK-CNTL-DIST          TO P913-TURN-DIST                 36900000
369100     MOVE WK-CNTL-SUB-DIST      TO P913-TURN-SUB-DIST             36910000
369200     MOVE WK-CNTL-XB            TO P913-TURN-CC                   36920000
369300     EXEC CICS LINK                                               36930000
369400               PROGRAM(P913-PGM)                                  36940000
369500               COMMAREA(P913-COMMAREA-PARMS)                      36950000
369600               LENGTH(P913-LGTH)                                  36960000
369700               RESP(WS-RESPONSE)                                  36970000
369800     END-EXEC                                                     36980000
369900     MOVE WS-RESPONSE           TO FILE-STATUS                    36990000
370000     IF NOT SUCCESS                                               37000000
370100        MOVE 'P9820-1'          TO ERR-PARAGRAPH                  37010000
370200        MOVE 'P931LINK'         TO ERR-KEY                        37020000
370300        PERFORM P9999-GOT-PROBLEM                                 37030000
370400     END-IF.                                                      37040000
370500*                                                                 37050000
370600 P9830-RETRIEVE-CNTL-INFO.                                        37060000
370700*                                                                 37070000
370800     MOVE SPACES                     TO P956-COMMAREA-PARMS       37080000
370900     MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE          37090000
371000     SET P956-GET-CNTL-STATUS-REASON TO TRUE                      37100000
371100     MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE          37110000
371200     MOVE DIST     OF WS-MSTR        TO P956-DIST                 37120000
371300     MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST                37130000
371400     MOVE CRAFT OF WS-MSTR           TO P956-CC                   37140000
371500     IF TEMPORARY-ASGNMT > SPACE                                  37150000
371600        MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE            37160000
371700        MOVE TA-1                    TO P956-ASGN                 37170000
371800        MOVE TA-DIST                 TO P956-DIST                 37180000
371900        MOVE TA-SUB-DIST             TO P956-SDIST                37190000
372000        IF TEMP-ASGN-XB                                           37200000
372100           MOVE TA-CC                TO P956-XB                   37210000
372200        END-IF                                                    37220000
372300     ELSE                                                         37230000
372400        IF NORMAL-ASGNMT > SPACES                                 37240000
372500           MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE            37250000
372600           MOVE NA-1                 TO P956-ASGN                 37260000
372700           MOVE NA-DIST              TO P956-DIST                 37270000
372800           MOVE NA-SUB-DIST          TO P956-SDIST                37280000
372900           IF NORM-ASGN-XB                                        37290000
373000              MOVE NA-CC             TO P956-XB                   37300000
373100           END-IF                                                 37310000
373200        END-IF                                                    37320000
373300     END-IF                                                       37330000
373400     IF  P956-ERROR-FOUND                                         37340000
373500         MOVE 'P956 LINK'            TO ERR-PARAGRAPH             37350000
373600         MOVE P956-INPUT-PARMS       TO ERR-KEY                   37360000
373700         PERFORM P9999-GOT-PROBLEM                                37370000
373800     END-IF                                                       37380000
373900     EXEC CICS LINK                                               37390000
374000               PROGRAM (P956-PGM)                                 37400000
374100               COMMAREA(P956-COMMAREA-PARMS)                      37410000
374200               LENGTH  (P956-LGTH)                                37420000
374300               RESP    (WS-RESPONSE)                              37430000
374400     END-EXEC                                                     37440000
374500     MOVE WS-RESPONSE           TO FILE-STATUS                    37450000
374600     IF NOT SUCCESS                                               37460000
374700        MOVE 'P9830-1'          TO ERR-PARAGRAPH                  37470000
374800        MOVE P956-INPUT-PARMS   TO ERR-KEY                        37480000
374900        PERFORM P9999-GOT-PROBLEM                                 37490000
375000     END-IF                                                       37500000
375100     IF P956-ST-RSN-DISP-WO-TRACKING                              37510000
375200        SET DISPLAY-EMP TO TRUE                                   37520000
375300     END-IF.                                                      37530000
375400*                                                                 37540000
375500 P9990-CLEAR-SCREEN.                                              37550000
375600*                                                                 37560000
375700     EXEC CICS SEND CONTROL                                       37570000
375800                    ERASE                                         37580000
375900                    FREEKB                                        37590000
376000     END-EXEC                                                     37600000
376100     EXEC CICS RETURN END-EXEC.                                   37610000
376200*                                                                 37620000
376300 P9999-GOT-PROBLEM.                                               37630000
376400*                                                                 37640000
376500     MOVE P02D-PGM TO ERR-PROGRAM                                 37650000
376600     MOVE DFHEIBLK TO ERR-EIBLK                                   37660000
376700     EXEC CICS XCTL                                               37670000
376800               PROGRAM(PSTERR-PGM)                                37680000
376900               COMMAREA(PSTERAR-AREA)                             37690000
377000               LENGTH(PSTERAR-LGTH)                               37700000
377100               RESP(WS-RESPONSE)                                  37710000
377200     END-EXEC                                                     37720000
377300     EXEC CICS ABEND                                              37730000
377400               ABCODE(PSTERR-ABCODE)                              37740000
377500               CANCEL                                             37750000
377600     END-EXEC.                                                    37760000
377700*                                                                 37770000
377800 X9999-GOBACK.                                                    37780000
377900     GOBACK.                                                      37790000
378000*                                                                 37800000
