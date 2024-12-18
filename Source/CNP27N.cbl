000100 IDENTIFICATION DIVISION.                                         00010040
000200 PROGRAM-ID. CNP27N.                                              00020040
000300*----------------------------------------------------------------*00030040
000400*                  ONLINE CALL BOARD REPORT                       00040040
000500*----------------------------------------------------------------*00050040
000600*   DATE    LOG#     PRO  DESCRIPTION                            *00060040
000700* -------------------------------------------------------------- *00070040
000800* 12/14/06  CNC0421  NJB  CLONED FROM CNP779                     *00080040
000900* 02/22/08  CNC0454  RXJ  FOR CANADIAN EMPLOYEES, LEAD TIME WILL *00090040
001000*                         NOT BE APPLIED TO THE REST EXPIRE TIME *00100040
001100* 09/18/08  C780     AJK  CHECK COMPANY CODE, NOT DIVISION CODE, *00110040
001200*                         WHEN DETERMINING IF LEAD TIME IS TO BE *00120040
001300*                         FACTORED INTO AN EMPLOYEE'S AVAILABLE  *00130040
001400*                         TIME (P5200)                           *00140040
001500* 04/16/09  CNC0484  CXB  DISPLAY HOURS OF SERVICE COUNTS FOR    *00150040
001600*                         EACH EMPLOYEE.                         *00160040
001700* 05/21/09  CNC0484  JES  ADDED PS94-TIME-ZONE MOVE              *00170040
001800* 06/16/09  CNC0484  SXO  FIXED THE HOS DISPLAYS.                *00180040
001900* 06/30/09  CNC0484  SXK  FIX P0100-1 (CNPS94) ABEND.            *00190040
002000* 08/04/09  CNC0484  RXS  PROD FIX. USE LOCAL DATE TIME FOR REST *00200040
002100*                         COMPARISIONS.                          *00210040
002200* 08/21/09  C831     DXM  FIX - WAS SETTING TEMP XB TO UNASSIGNED*00220040
002300*                         WHEN ON A GIVEN BOARD.                 *00230040
002400* 10/11/10  C886     AJK  GET STATUS CODE DESCRITPIONS OFF OF     00240040
002500*                         THE STATUS CONTROL RECORD, RATHER THAN  00250040
002600*                         OFF OF WSLOCODE TABLE. (P5100)          00260040
002700* 07/08/11  C913     BXS  CALL BOARD REPORT NOT SHOWING CORRECT   00270040
002800*                         CRAFT CODE FOR TRAINEE (ET)             00280040
002900* 09/12/11  CNC0516  VXD  SHOW LAYOFF CODE AND RETURN DATE/TIME   00290048
003000*                         FOR EMPLOYEE WHO ARE UNAVAILABLE OR     00300048
003100*                         THERE IS VACANCY IN POOL                00310048
003200* 11/02/11  CNC0516  AXK  MADE THE CHANGES TO CHECK FOR           00320058
003300*                         E95'S RETURN > THAN CURRENT OFF-STATUS  00330058
003400* 03/21/13  C970     AXD  DISPLAY TRAINS HAVING SAME SYMBOL IF    00340069
003500*                         ORIGIN STATION FROM AHIST IS DIFFERENT. 00350069
003600* 03/26/13  C975     KXB  REPLACE WS-PRESENT-TIME-CENT BY         00360070
003610*                         WS-LOCAL-DATE-TIME-CENT IN ORDER TO     00370070
003620*                         DISPLAY EXTRABOARD REST.                00380070
003630* 10/21/13  CNC0531  BLD  DO NOT DISPLAY "YT" CRAFTS ON REPORT    00390071
003640* 01/12/16  CNC0573  MFO  MASK STATUS/REASON FIELDS.              00400078
003650* 02/23/16  CNC0576  MFO  MASK STATUS/REASON FIELDS - HOLD TURN.  00410084
003660* 06/30/16  C1129    RXB  ADD BOOK-ON RECORD FROM TASK LIST TO    00420085
003670*                         DISPLAY CORRECT RETURN DATE FOR VACATION00430085
003700*----------------------------------------------------------------*00440040
003800                                                                  00450040
003900 ENVIRONMENT DIVISION.                                            00460040
004000                                                                  00470040
004100 CONFIGURATION SECTION.                                           00480040
004200                                                                  00490040
004300 SOURCE-COMPUTER. IBM-9370.                                       00500040
004400 OBJECT-COMPUTER. IBM-9370.                                       00510040
004500 DATA DIVISION.                                                   00520040
004600 WORKING-STORAGE SECTION.                                         00530040
004700                                                                  00540040
004800 01  SUBCRIPTS.                                                   00550040
004900     05  RPT-MAX                     PIC 9(002) VALUE 98.         00560040
005000     05  I                           PIC 9(003) VALUE ZEROS.      00570040
005100     05  J                           PIC 9(003) VALUE ZEROS.      00580040
005200     05  X2                          PIC 9(003) VALUE ZEROS.      00590040
005300     05  SB-SUB                      PIC 9(002) VALUE ZEROS.      00600040
005400     05  OB-SUB                      PIC 9(002) VALUE ZEROS.      00610040
005500     05  POOL-SUB                    PIC 9(002) VALUE ZEROS.      00620040
005600     05  EXT-SUB                     PIC 9(002) VALUE ZEROS.      00630040
005700     05  SUB2                        PIC 9(002) VALUE ZEROS.      00640040
005800     05  WS-POS-CNT                  PIC 9(003) VALUE ZEROS.      00650040
005900     05  POOL-POS-CNT                PIC 9(002) VALUE ZEROS.      00660040
006000     05  SAVE-CNT                    PIC 9(002) VALUE ZEROS.      00670040
006100     05  WS-YESTERDAY                PIC S9(01) VALUE ZEROS.      00680040
006200                                                                  00690040
006300 01  HARD-CODED-VALUES.                                           00700040
006400     05  WS-DIST-LM                  PIC X(002) VALUE 'LM'.       00710040
006500     05  WS-SUB-DIST-JX              PIC X(002) VALUE 'JX'.       00720040
006600     05  WS-SUB-DIST-MC              PIC X(002) VALUE 'MC'.       00730040
006700     05  WS-POOL-JN                  PIC X(002) VALUE 'JN'.       00740040
006800                                                                  00750040
006900 01  WS-FLAGS-AND-SWITCHES.                                       00760040
007000     05  SCREEN-FLAG              PIC X(001) VALUE '0'.           00770040
007100         88  CONTINUE-SCREEN                 VALUE '0'.           00780040
007200         88  CREATE-SCREEN                   VALUE '1'.           00790040
007300     05  NEW-POOL-FLAG               PIC X(001) VALUE '0'.        00800040
007400         88  NEW-POOL                           VALUE '1'.        00810040
007500     05  GOT-EMPLOYEE-FLAG           PIC X(001) VALUE '0'.        00820040
007600         88  GOT-EMPLOYEE                       VALUE '1'.        00830040
007700     05  DONE-CODE                   PIC X(001) VALUE '0'.        00840040
007800         88  NOT-DONE                           VALUE '0'.        00850040
007900         88  DONE                               VALUE '1'.        00860040
008000     05  STOP-JOB-CODE               PIC X(001) VALUE '0'.        00870040
008100         88  SAME-JOB                           VALUE '0'.        00880040
008200         88  END-OF-JOB                         VALUE '1'.        00890040
008300     05  XB-DONE-CODE                PIC X(001) VALUE '0'.        00900040
008400         88  XB-NOT-DONE                        VALUE '0'.        00910040
008500         88  XB-DONE                            VALUE '1'.        00920040
008600     05  ASGN-DONE-CODE              PIC 9(001) VALUE 0.          00930040
008700         88  ASGN-DONE                          VALUE 1.          00940040
008800     05  EN-ID-POOL-FLAG             PIC X(001) VALUE '0'.        00950040
008900         88  EN-ID-POOL                         VALUE '1'.        00960040
009000     05  TR-ID-POOL-FLAG             PIC X(001) VALUE '0'.        00970040
009100         88  TR-ID-POOL                         VALUE '1'.        00980040
009200     05  POOL-SERVICE-FLAG           PIC X(001) VALUE ' '.        00990040
009300         88  MINE-TURN-SVC                      VALUE 'M'.        01000040
009400     05  SEARCH-DONE-CODE            PIC 9(001) VALUE 0.          01010040
009500         88  SEARCH-NOT-DONE                    VALUE 0.          01020040
009600         88  SEARCH-DONE                        VALUE 1.          01030040
009700     05  TEMP-EMP-FLAG               PIC X(001) VALUE ' '.        01040040
009800         88  TEMP-EMP-FOUND                     VALUE 'Y'.        01050040
009900     05  DIST-SDIST-POOL-FLAG        PIC X(001) VALUE ' '.        01060040
010000         88  DIST-SDIST-POOL-LM-MC-JN           VALUE 'Y'.        01070040
010100     05  TRAINS-DONE-CODE            PIC X(001) VALUE '0'.        01080040
010200         88  TRAINS-DONE                        VALUE '1'.        01090040
010300         88  TRAINS-NOT-DONE                    VALUE '0'.        01100040
010400     05  LOCALS-DONE-CODE            PIC X(001) VALUE '0'.        01110040
010500         88  LOCALS-DONE                        VALUE '1'.        01120040
010600         88  LOCALS-NOT-DONE                    VALUE '0'.        01130040
010700     05  CC-FOUND-FLAG               PIC 9(001) VALUE 0.          01140040
010800         88  CC-FOUND                           VALUE 1.          01150040
010900     05  UNDISTURBED-REST-FLAG       PIC X(001) VALUE ' '.        01160040
011000         88  UNDISTURBED-REST                   VALUE 'Y'.        01170040
011100     05  CURRENT-JOB-FLAG            PIC X(001) VALUE ' '.        01180040
011200         88  JOB-CURRENT                        VALUE 'Y'.        01190040
011300     05  WS-XB-SCHEDULED-FLAG        PIC X(001) VALUE SPACES.     01200040
011400         88  WS-SCHEDULED-XB                    VALUE 'Y' 'E'.    01210040
011500         88  WS-XB-SCHEDULED                    VALUE 'Y'.        01220040
011600         88  WS-XB-EXTENDED-SCHED               VALUE 'E'.        01230040
011700     05  ON-REST-DAY-FLAG            PIC X(001) VALUE SPACES.     01240040
011800         88  NOT-ON-REST-DAY                    VALUE ' '.        01250040
011900         88  ON-REST-DAY                        VALUE 'Y'.        01260040
012000     05  WS-JS-SW                    PIC X(001) VALUE 'F'.        01270040
012100         88  FIRST-JS                           VALUE 'F'.        01280040
012200         88  MORE-JS                            VALUE '1'.        01290040
012300         88  JS-DONE                            VALUE '9'.        01300040
012400     05  WS-SCHEDULED-TURN-SW        PIC X(001) VALUE SPACE.      01310040
012500         88  WS-SCHEDULED-REST-DAY              VALUE 'D'.        01320040
012600         88  WS-SCHEDULED-REST-PERIOD           VALUE 'P'.        01330040
012700     05  DISPLAY-EMP-FLAG            PIC X(001) VALUE 'N'.        01340040
012800         88  DONT-DISPLAY-EMP                   VALUE 'N'.        01350040
012900         88  DISPLAY-EMP                        VALUE 'Y'.        01360040
013000     05  WS-APPLY-LEAD-TIME-FLAG     PIC X(001) VALUE 'Y'.        01370040
013100         88  APPLY-LEAD-TIME                    VALUE 'Y'.        01380040
013200         88  DONT-APPLY-LEAD-TIME               VALUE 'N'.        01390040
013300     05  WS-COMPANY-CODE-FLAG        PIC X(001) VALUE ' '.        01400040
013400         88  WS-CANADIAN-COMPANY                VALUE 'C'.        01410040
013500         88  WS-US-COMPANY                      VALUE 'U'.        01420040
013600     05  WS-DISPLAY-HOS-FLAG         PIC X(001) VALUE 'N'.        01430040
013700         88  DONT-DISPLAY-HOS                   VALUE 'N'.        01440040
013800         88  DISPLAY-HOS                        VALUE 'Y'.        01450040
013900     05  WS-TEMP-BOARD-FLAG          PIC X(001) VALUE 'D'.        01460040
014000         88 TEMP-DIFF-BOARD                     VALUE 'D'.        01470040
014100         88 TEMP-SAME-BOARD                     VALUE 'S'.        01480040
014200*CNC0516-BEG                                                      01490043
014300     05  WS-DUEBACK-FOUND-FLAG       PIC 9      VALUE 0.          01500044
014400         88  WS-DUEBACK-FOUND-N                 VALUE 0.          01510044
014500         88  WS-DUEBACK-FOUND-Y                 VALUE 1.          01520044
014600     05  WS-TASK-DONE-CODE           PIC X      VALUE 'N'.        01530044
014700         88  TASK-NOT-DONE                      VALUE 'N'.        01540044
014800         88  TASK-DONE                          VALUE 'Y'.        01550044
014900*CNC0516-END                                                      01560044
015000*C970-START                                                       01570064
015100     05  WS-IF-SAME-TRAIN-CALLED     PIC X(001) VALUE SPACE.      01580064
015200         88  SAME-TRAIN-CALLED                  VALUE 'Y'.        01590064
015300         88  SAME-TRAIN-NOT-CALLED              VALUE 'N'.        01600064
015400     05  WS-AH-ORIGIN-PREV           PIC X(005) VALUE SPACE.      01610064
015500     05  WS-AH-ORIGIN-CURR           PIC X(005) VALUE SPACE.      01620064
015600*C970-END                                                         01630064
015700                                                                  01640040
015800 01  MARRIED-CRAFT-FLAGS.                                         01650040
015900     05  EN-FI-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01660040
016000         88  EN-FI-MARRIED                      VALUE 'Y', '1'.   01670040
016100         88  EN-SE-MARRIED                      VALUE '1'.        01680040
016200     05  EN-ET-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01690040
016300         88  EN-ET-MARRIED                      VALUE 'Y'.        01700040
016400     05  CO-BK-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01710040
016500         88  CO-BK-MARRIED                      VALUE 'Y'.        01720040
016600     05  CO-B1-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01730040
016700         88  CO-B1-MARRIED                      VALUE 'Y'.        01740040
016800     05  CO-B2-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01750040
016900         88  CO-B2-MARRIED                      VALUE 'Y'.        01760040
017000     05  CO-BG-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01770040
017100         88  CO-BG-MARRIED                      VALUE 'Y'.        01780040
017200     05  CO-TT-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01790040
017300         88  CO-TT-MARRIED                      VALUE 'Y'.        01800040
017400     05  B1-B2-MARRIED-FLAG          PIC X(001) VALUE SPACES.     01810040
017500         88  B1-B2-MARRIED                      VALUE 'Y'.        01820040
017600                                                                  01830040
017700*CNC0516-BEG                                                      01840044
017800 01 WS-EXTRABOARD-RESTED.                                         01850078
017900*CNC0573 - BEG                                                    01860078
018000*    05 WS-LAYOFF-CODE           PIC X(02)      VALUE SPACES.     01870078
018100*    05 WS-LAYOFF-EM-CODE        PIC X(02)      VALUE SPACES.     01880078
018200*    05 FILLER                   PIC X          VALUE SPACE.      01890078
018300     05 WS-LAYOFF-AREA           PIC X(05)      VALUE SPACES.     01900078
018400     05 FILLER REDEFINES WS-LAYOFF-AREA.                          01910078
018500        10 WS-LAYOFF-CODE        PIC X(02).                       01920078
018600        10 WS-LAYOFF-EM-CODE     PIC X(02).                       01930078
018700        10 FILLER                PIC X.                           01940078
018800*CNC0573 - END                                                    01950078
018900     05 WS-RETURN-DATE           PIC X(04)      VALUE SPACES.     01960078
019000     05 FILLER                   PIC X          VALUE SPACE.      01970078
019100     05 WS-RETURN-TIME           PIC X(04)      VALUE SPACES.     01980078
019200 01 WS-CREWS-IT-RESTED.                                           01990078
019300*CNC0573 - BEG                                                    02000078
019400*    05 WS-LAYOFF-CODE1          PIC X(02)      VALUE SPACES.     02010078
019500*    05 WS-LAYOFF-EM-CODE1       PIC X(02)      VALUE SPACES.     02020078
019600*    05 FILLER                   PIC X          VALUE SPACE.      02030078
019700     05 WS-LAYOFF-AREA1          PIC X(05)      VALUE SPACES.     02040078
019800     05 FILLER REDEFINES WS-LAYOFF-AREA1.                         02050078
019900        10 WS-LAYOFF-CODE1       PIC X(02).                       02060078
020000        10 WS-LAYOFF-EM-CODE1    PIC X(02).                       02070078
020100        10 FILLER                PIC X.                           02080078
020200*CNC0573 - END                                                    02090078
020300     05 WS-RETURN-DATE1          PIC X(04)      VALUE SPACES.     02100078
020400     05 FILLER                   PIC X          VALUE SPACE.      02110078
020500     05 WS-RETURN-TIME1          PIC X(04)      VALUE SPACES.     02120078
020510 01 WS-RESTED1.                                                   02130078
020520*CNC0573 - BEG                                                    02140078
020530*    05 WS-LAYOFF-CODE2          PIC X(02)      VALUE SPACES.     02150078
020540*    05 WS-LAYOFF-EM-CODE2       PIC X(02)      VALUE SPACES.     02160078
020550*    05 FILLER                   PIC X          VALUE SPACE.      02170078
020560     05 WS-RESTED-AREA           PIC X(05)      VALUE SPACES.     02180078
020570     05 FILLER REDEFINES WS-RESTED-AREA.                          02190078
020580        10 WS-LAYOFF-CODE2       PIC X(02).                       02200078
020590        10 WS-LAYOFF-EM-CODE2    PIC X(02).                       02210078
020591        10 FILLER                PIC X.                           02220078
020592*CNC0573 - END                                                    02230078
020593     05 WS-RETURN-DATE2          PIC X(04)      VALUE SPACES.     02240078
020594     05 FILLER                   PIC X          VALUE SPACE.      02250078
020595     05 WS-RETURN-TIME2          PIC X(04)      VALUE SPACES.     02260078
020596 01 WS-E95.                                                       02270078
020597*CNC0573 - BEG                                                    02280078
020598*    05 WS-E95-CODE              PIC X(02)      VALUE SPACES.     02290078
020599*    05 WS-E95-EM-CODE           PIC X(02)      VALUE SPACES.     02300078
020600*    05 FILLER                   PIC X          VALUE SPACE.      02310078
020601     05 WS-LO-E95-AREA           PIC X(05)      VALUE SPACES.     02320078
020602     05 FILLER REDEFINES WS-LO-E95-AREA.                          02330078
020603        10 WS-E95-CODE           PIC X(02).                       02340078
020604        10 WS-E95-EM-CODE        PIC X(02).                       02350078
020605        10 FILLER                PIC X.                           02360078
020606*CNC0573 - END                                                    02370078
020607     05 WS-E95-DATE              PIC X(04)      VALUE SPACES.     02380078
020608     05 FILLER                   PIC X          VALUE SPACE.      02390078
020609     05 WS-E95-TIME              PIC X(04)      VALUE SPACES.     02400078
020610*CNC0516-END                                                      02410047
020700 01  WS-LIMBO-TIME.                                               02420040
020800     05 FILLER                       PIC X(002) VALUE SPACES.     02430040
020900     05 WS-LIMBO-TM.                                              02440040
021000        10 WS-LIMBO-TM-HH            PIC 9(002) VALUE ZEROES.     02450040
021100        10 WS-LIMBO-TM-MM            PIC 9(002) VALUE ZEROES.     02460040
021200 01  WS-TOT-TIME.                                                 02470040
021300     05 FILLER                       PIC X(001) VALUE SPACES.     02480040
021400     05 WS-TOT-TM.                                                02490040
021500        10 WS-TOT-TM-HH              PIC 9(003) VALUE ZEROES.     02500040
021600        10 WS-TOT-TM-MM              PIC 9(002) VALUE ZEROES.     02510040
021700 01  WS-CONSEC-STARTS                PIC X(002) VALUE SPACES.     02520040
021800                                                                  02530040
021900 01  WS-SWASSGN-ASGN.                                             02540040
022000     05  WS-SW-EXTRA                 PIC X(002) VALUE 'EX'.       02550040
022100     05  WS-SW-POSITION              PIC X(004) VALUE SPACES.     02560040
022200     05  WS-SW-CRAFT                 PIC X(002) VALUE SPACES.     02570040
022300                                                                  02580040
022400 01  WS-FORMAT-NAME-AUG                         VALUE SPACES.     02590040
022500     05  WS-FORMAT-EMP-NAME          PIC X(021).                  02600040
022600     05  WS-FORMAT-NAME-AUG-FIELD    PIC X(005).                  02610040
022700                                                                  02620040
022800 01  FORMAT-DATE-AREA.                                            02630040
022900     05  FORM-YR                     PIC X(002) VALUE SPACES.     02640040
023000     05  FILLER                      PIC X(001) VALUE '/'.        02650040
023100     05  FORMAT-PART-DATE.                                        02660040
023200         10  FORM-MO                 PIC X(002) VALUE SPACES.     02670040
023300         10  FILLER                  PIC X(001) VALUE '/'.        02680040
023400         10  FORM-DY                 PIC X(002) VALUE SPACES.     02690040
023500         10  FILLER                  PIC X(001) VALUE '-'.        02700040
023600         10  FORM-HRMN               PIC X(004) VALUE SPACES.     02710040
023700                                                                  02720040
023800 01  WS-TODAYS-DATE.                                              02730040
023900     05  CUR-YR                      PIC 9(002) VALUE ZEROS.      02740040
024000     05  CUR-MO                      PIC 9(002) VALUE ZEROS.      02750040
024100     05  CUR-DY                      PIC 9(002) VALUE ZEROS.      02760040
024200                                                                  02770040
024300 01  POOL-TABLE                                 VALUE SPACES.     02780040
024400     05  POOL-DATA OCCURS 98 TIMES.                               02790040
024500         10  POOL-P                  PIC X(002).                  02800040
024600                                                                  02810040
024700 01  SPAREBOARD-TABLE                           VALUE SPACES.     02820040
024800     05  SB-DATA OCCURS 98 TIMES.                                 02830040
024900         10  SB-P                    PIC X(002).                  02840040
025000                                                                  02850040
025100 01  WS-YESTERDAY-START-TIME         PIC X(004) VALUE SPACES.     02860040
025200 01  WS-YESTERDAY-END-TIME           PIC X(004) VALUE SPACES.     02870040
025300 01  WS-JOB-SCHED-REST-DAYS.                                      02880040
025400     02  WS-JOB-SCHED-REST-DAY       PIC X(001) OCCURS 7 TIMES.   02890040
025500         88  WS-JOB-ON-REST-DAY                 VALUE '1'.        02900040
025600                                                                  02910040
025700 01  SAVE-CNTL-AREA                  PIC X(256) VALUE SPACES.     02920040
025800 01  SAVE-ASGN-AREA                  PIC X(128) VALUE SPACES.     02930040
025900 01  XXXX-ASGNKEY1                              VALUE SPACES.     02940040
026000     05  XX-ASGN-JOB-TYPE            PIC X(001).                  02950040
026100     05  XX-ASGN-DIST                PIC X(002).                  02960040
026200     05  XX-ASGN-SUB-DIST            PIC X(002).                  02970040
026300     05  XX-ASGN-JOB                 PIC X(008).                  02980040
026400     05  XX-ASGN-REC-TYPE            PIC X(001).                  02990040
026500     05  XX-ASGN-DATE-TIME           PIC X(010).                  03000040
026600 01  XXXX-ASGNKEY2.                                               03010040
026700     05  XX-ASGN-EMP                 PIC 9(009) VALUE ZEROS.      03020040
026800     05  XX-ASGN2-REC-TYPE           PIC X(001) VALUE SPACES.     03030040
026900     05  XX-ASGN2-DATE-TIME          PIC X(010) VALUE SPACES.     03040040
027000                                                                  03050040
027100 01  WORK-UFPPOS-KEY.                                             03060040
027200     05  POS-DIST                    PIC X(002) VALUE SPACES.     03070040
027300     05  POS-SUB-DIST                PIC X(002) VALUE SPACES.     03080040
027400     05  POS-POOL                    PIC X(002) VALUE SPACES.     03090040
027500     05  POS-CC                      PIC X(002) VALUE SPACES.     03100040
027600     05  POS-TERMINAL.                                            03110040
027700         10  FILLER                  PIC 9(001) VALUE 0.          03120040
027800         10  POS-TERM                PIC 9(001) VALUE 0.          03130040
027900     05  POS-TIME.                                                03140040
028000         10  POS-ON-OFF              PIC X(001) VALUE SPACES.     03150040
028100             88  POS-ON                         VALUE '0'         03160040
028200                                                THRU '8'.         03170040
028300             88  POS-OFF                        VALUE '9'.        03180040
028400         10  POS-DATE-TIME           PIC X(014) VALUE SPACES.     03190040
028500         10  POS-DATE-TIME-NUM       REDEFINES                    03200040
028600             POS-DATE-TIME           PIC 9(014).                  03210040
028700                                                                  03220040
028800 01  WORK-XB-POS-KEY                            VALUE SPACES.     03230040
028900     05  XB-POS-DIST                 PIC X(002).                  03240040
029000     05  XB-POS-SUB-DIST             PIC X(002).                  03250040
029100     05  XB-POS-CC                   PIC X(002).                  03260040
029200     05  XB-POS.                                                  03270040
029300         10  XB-POS-ON-OFF           PIC X(001).                  03280040
029400         10  XB-POS-BOARD            PIC X(001).                  03290040
029500         10  XB-POS-TIME             PIC X(014).                  03300040
029600         10  XB-POS-TIME-NUM         REDEFINES                    03310040
029700             XB-POS-TIME             PIC 9(014).                  03320040
029800                                                                  03330040
029900 01  WORK-XB-TURN-KEY                           VALUE SPACES.     03340040
030000     05  XB-TURN-DIST                PIC X(002).                  03350040
030100     05  XB-TURN-SUB-DIST            PIC X(002).                  03360040
030200     05  XB-TURN-CC                  PIC X(002).                  03370040
030300     05  XB-TURN                     PIC X(004).                  03380040
030400                                                                  03390040
030500 01  WORK-SENKEY2                               VALUE SPACES.     03400040
030600     05  WK-SEN-EMP-NO               PIC X(009).                  03410040
030700     05  WK-SEN-ROSTER               PIC X(004).                  03420040
030800     05  WK-SEN-CC                   PIC X(002).                  03430040
030900                                                                  03440040
031000 01  WEEK-DAY                        PIC 9(002) VALUE 01.         03450040
031100 01  DAY1                            PIC 9(002) VALUE ZEROS.      03460040
031200 01  SW-DAY1                         PIC 9(002) VALUE ZEROS.      03470040
031300 01  WS-TERM                         PIC 9(002) VALUE ZEROS.      03480040
031400 01  CC-MAX                          PIC 9(002) VALUE 10.         03490040
031500 01  CC-SUB                          PIC 9(002) VALUE ZEROS.      03500040
031600 01  CC-SUB2                         PIC 9(002) VALUE ZEROS.      03510040
031700 01  WS-CNTL-XB                      PIC X(002) VALUE SPACES.     03520040
031800                                                                  03530040
031900 01  WORK-TIME.                                                   03540040
032000     05  WORK-DATE.                                               03550040
032100         10  WK-YR                   PIC 9(002) VALUE ZEROS.      03560040
032200         10  WK-MO                   PIC 9(002) VALUE ZEROS.      03570040
032300         10  WK-DY                   PIC 9(002) VALUE ZEROS.      03580040
032400     05  WORK-HR-MN.                                              03590040
032500         10  WK-HR                   PIC 9(002) VALUE ZEROS.      03600040
032600         10  WK-MN                   PIC 9(002) VALUE ZEROS.      03610040
032700*CNC0516-BEG                                                      03620044
032800 01  WS-RETURN-DATE-1                PIC X(008).                  03630054
032900 01  FILLER REDEFINES WS-RETURN-DATE-1.                           03640044
033000     05  WS-RETURN-DATE-MM           PIC 99.                      03650044
033100     05  WS-RETURN-DATE-DD           PIC 99.                      03660044
033200     05  WS-RETURN-DATE-HRMN         PIC 9(004).                  03670054
033300 01  WS-EFFECTIVE-DATE-TIME.                                      03680055
033400     05  WS-EFF-DATE.                                             03690055
033500         10  WS-EFF-YR               PIC 99.                      03700055
033600         10  WS-EFF-MO               PIC 99.                      03710055
033700         10  WS-EFF-DY               PIC 99.                      03720055
033800     05  WS-EFF-HR-MN.                                            03730055
033900         10  WS-EFF-HR               PIC 99.                      03740055
034000         10  WS-EFF-MN               PIC 99.                      03750055
034100*CNC0516-END                                                      03760044
034200                                                                  03770040
034300 01  WS-END-DATE-TIME.                                            03780040
034400     05  WS-END-DATE                 PIC 9(006) VALUE ZEROS.      03790040
034500     05  WS-END-TIME                 PIC 9(004) VALUE ZEROS.      03800040
034600                                                                  03810040
034700 01  CRAFT-TABLE-AREA                           VALUE SPACES.     03820040
034800     05  CRAFT-TABLE-ARRAY OCCURS 10.                             03830040
034900         10  PRIMARY-CC              PIC X(002).                  03840040
035000         10  PRIMARY-CC-LAST-KEY     PIC X(025).                  03850040
035100             88  PRIMARY-CC-DONE                VALUE             03860040
035200                                     'DONE                     '. 03870040
035300         10  ASSOC-CC-ARRAY.                                      03880040
035400             15  ASSOC-CC            PIC X(002) OCCURS 10.        03890040
035500             15  ASSOC-CC-TYPE       PIC X(001) OCCURS 10.        03900040
035600                 88  ASSOC-CC-OPT               VALUE 'Y'.        03910040
035700                                                                  03920040
035800 01  FOREMAN-OR-SWITCHMAN            PIC X(010).                  03930040
035900     88  HE-IS-FOREMAN                         VALUE 'FOREMAN   '.03940040
036000     88  HE-IS-SWITCHMAN                       VALUE 'SWITCHMAN '.03950040
036100                                                                  03960040
036200 01  TITLE-CUSTOMER.                                              03970040
036300     05  FILLER                      PIC X(001) VALUE SPACES.     03980040
036400     05  TITLE-DATE-TIME.                                         03990040
036500         10  TIT-YR                  PIC 9(002) VALUE ZEROS.      04000040
036600         10  FILLER                  PIC X(001) VALUE '/'.        04010040
036700         10  TIT-MO                  PIC 9(002) VALUE ZEROS.      04020040
036800         10  FILLER                  PIC X(001) VALUE '/'.        04030040
036900         10  TIT-DY                  PIC 9(002) VALUE ZEROS.      04040040
037000         10  FILLER                  PIC X(001) VALUE '-'.        04050040
037100         10  TIT-HR                  PIC Z9     VALUE ZEROS.      04060040
037200         10  TIT-MN                  PIC 9(002) VALUE ZEROS.      04070040
037300     05  FILLER                      PIC X(013) VALUE SPACES.     04080040
037400     05  TITLE-CUST-NAME             PIC X(026) VALUE SPACES.     04090040
037500     05  FILLER                      PIC X(011) VALUE SPACES.     04100040
037600     05  FILLER                      PIC X(005) VALUE 'PAGE '.    04110040
037700     05  REPORT-TITLE-PAGE           PIC 9(003) VALUE ZEROS.      04120040
037800     05  FILLER                      PIC X(004) VALUE ' OF '.     04130040
037900     05  REPORT-TITLE-MAX-PAGE       PIC 9(003) VALUE ZEROS.      04140040
038000                                                                  04150040
038100 01  REPORT-TITLE-TERMINAL.                                       04160040
038200     05  FILLER                      PIC X(027) VALUE SPACES.     04170040
038300     05  REPORT-TITLE-TERM           PIC X(026) VALUE SPACES.     04180040
038400     05  FILLER                      PIC X(026) VALUE SPACES.     04190040
038500                                                                  04200040
038600 01  REPORT-TITLE.                                                04210040
038700     05  FILLER                      PIC X(031) VALUE SPACES.     04220040
038800     05  FILLER                      PIC X(017) VALUE             04230040
038900         'CALL BOARD REPORT'.                                     04240040
039000     05  FILLER                      PIC X(017) VALUE SPACES.     04250040
039100     05  FILLER                      PIC X(006) VALUE             04260040
039200         'CNP27N'.                                                04270040
039300                                                                  04280040
039400 01  SCR-SUB                         PIC 9(002) VALUE ZEROS.      04290040
039500 01  SCR-MAX                         PIC 9(002) VALUE 21.         04300040
039600 01  LINE-OF-DASHES                  PIC X(132) VALUE ALL '-'.    04310040
039700 01  SPACE-LINE                      PIC X(001) VALUE SPACES.     04320040
039800                                                                  04330040
039900                                                                  04340040
040000 01  SAVE-CREW-INFORMATION                      VALUE SPACES.     04350040
040100     05  SAVE-CREW-INFO OCCURS 33 TIMES.                          04360074
040200         10  SAVE-CREW-CODE          PIC X(002).                  04370040
040300         10  SAVE-CREW-CRAFT         PIC X(002).                  04380040
040400         10  SAVE-CREW-POSITION      PIC X(002).                  04390040
040500         10  SAVE-CREW-NAME          PIC X(026).                  04400040
040600         10  SAVE-CREW-TEMP-EMP-FLAG PIC X(001).                  04410040
040700             88 SAVE-CREW-TEMP-EMP              VALUE 'Y'.        04420040
040800*CNC0516-BEG                                                      04430051
040900         10  SAVE-CREW-RESTED        PIC X(014).                  04440048
041000*CNC0516-END                                                      04450051
041100         10  SAVE-CREW-TURN          PIC X(004).                  04460040
041200         10  SAVE-CREW-HOS-TOTAL.                                 04470040
041300             20 SAVE-CREW-HOS-TOTAL-HR PIC X(003).                04480040
041400             20 SAVE-CREW-HOS-TOTAL-MM PIC X(002).                04490040
041500         10  SAVE-CREW-HOS-LIMBO.                                 04500040
041600             20 SAVE-CREW-HOS-LIMBO-HR PIC X(002).                04510040
041700             20 SAVE-CREW-HOS-LIMBO-MM PIC X(002).                04520040
041800         10  SAVE-CREW-HOS-ST        PIC X(002).                  04530040
041900                                                                  04540040
042000 01  SAVE-CREW-INFORMATION-HOS                  VALUE SPACES.     04550040
042100     05  SAVE-CREW-INFO-HOS OCCURS 33 TIMES.                      04560074
042200         10  SAVE-CREW-DISP-HOS-FLAG PIC X(001).                  04570040
042300             88 SAVE-CREW-DONT-DISPLAY          VALUE 'Y'.        04580040
042400                                                                  04590040
042500 01  SAVE-TEMP-CREW-INFORMATION                 VALUE SPACES.     04600040
042600     05  SAVE-TEMP-CREW-INFO OCCURS 33 TIMES.                     04610074
042700         10  SAVE-TEMP-CREW-CRAFT    PIC X(010).                  04620040
042800         10  SAVE-TEMP-CREW-NAME     PIC X(026).                  04630040
042900*CNC0516-BEG                                                      04640051
043000         10  SAVE-TEMP-CREW-RESTED   PIC X(014).                  04650048
043100*CNC0516-END                                                      04660051
043200         10  SAVE-TEMP-CREW-TOTAL.                                04670040
043300             20 SAVE-TEMP-CREW-TOTAL-HR PIC X(003).               04680040
043400             20 SAVE-TEMP-CREW-TOTAL-MM PIC X(002).               04690040
043500         10  SAVE-TEMP-CREW-LIMBO.                                04700040
043600             20 SAVE-TEMP-CREW-LIMBO-HR PIC X(002).               04710040
043700             20 SAVE-TEMP-CREW-LIMBO-MM PIC X(002).               04720040
043800         10  SAVE-TEMP-CREW-ST       PIC X(002).                  04730040
043900                                                                  04740040
044000 01  SAVE-TEMP-CREW-INFORMATION-HOS             VALUE SPACES.     04750040
044100     05  SAVE-TEMP-CREW-INFO-HOS OCCURS 33 TIMES.                 04760074
044200         10  SAVE-TEMP-CREW-DISP-HOS-FLAG PIC X(001).             04770040
044300             88 SAVE-TEMP-CREW-DONT-DISPLAY          VALUE 'Y'.   04780040
044400                                                                  04790040
044500 01  TRAIN-COUNT                     PIC 9(003) VALUE ZEROS.      04800040
044600 01  LOCAL-COUNT                     PIC 9(003) VALUE ZEROS.      04810040
044700 01  CREW-COUNT                      PIC 9(003) VALUE ZEROS.      04820040
044800 01  AJ-COUNT                        PIC 9(003) VALUE ZEROS.      04830040
044900 01  POS-COUNT                       PIC 9(002) VALUE ZEROS.      04840040
045000 01  SAVE-TERM                       PIC 9(001) VALUE ZEROS.      04850040
045100 01  WK-DESC                         PIC X(020) VALUE SPACES.     04860040
045200 01  WK-FROM                         PIC X(014) VALUE SPACES.     04870040
045300                                                                  04880040
045400 01  TRAINS-ENROUTE-TITLE.                                        04890040
045500     02  FILLER                   PIC X(01) VALUE SPACES.         04900040
045600     02  FILLER                   PIC X(15)                       04910040
045700                                  VALUE 'TRAINS ENROUTE '.        04920040
045800     02  FILLER                   PIC X(65) VALUE SPACE.          04930040
045900 01  TRAINS-ENROUTE-1.                                            04940040
046000     02  FILLER                   PIC X(03) VALUE SPACE.          04950040
046100     02  TRAIN-ENROUTE            PIC X(10) VALUE SPACE.          04960040
046200     02  FILLER                   PIC X(01) VALUE SPACE.          04970040
046300     02  TRAIN-DUTY               PIC X(14) VALUE SPACES.         04980040
046400     02  TRAIN-ENROUTE-FROM       PIC X(17) VALUE SPACE.          04990040
046500     02  FILLER                   PIC X(01) VALUE SPACE.          05000040
046600     02  FILLER                   PIC X(03) VALUE 'AT '.          05010040
046700     02  TRAIN-YR                 PIC X(02) VALUE SPACE.          05020040
046800     02  TRAIN-FIL1               PIC X(01) VALUE SPACE.          05030040
046900     02  TRAIN-MO                 PIC X(02) VALUE SPACE.          05040040
047000     02  TRAIN-FIL2               PIC X(01) VALUE SPACE.          05050040
047100     02  TRAIN-DY                 PIC X(02) VALUE SPACE.          05060040
047200     02  TRAIN-FIL3               PIC X(01) VALUE SPACE.          05070040
047300     02  TRAIN-HR-MN              PIC X(04) VALUE SPACE.          05080040
047400     02  FILLER                   PIC X(18) VALUE SPACE.          05090040
047500 01  TRAINS-ENROUTE-2.                                            05100040
047600     02  FILLER                   PIC X(10) VALUE SPACE.          05110040
047700     02  TRAIN-CC                 PIC X(02) VALUE SPACE.          05120040
047800     02  FILLER                   PIC X(02) VALUE SPACE.          05130040
047900     02  TRAIN-EMP                PIC X(26) VALUE SPACE.          05140040
048000     02  FILLER                   PIC X(02) VALUE SPACE.          05150040
048100     02  TRAIN-TURN               PIC X(04) VALUE SPACE.          05160040
048200     02  FILLER                   PIC X(02) VALUE SPACE.          05170040
048300     02  TRAIN-HOS-AREA           PIC X(32) VALUE SPACE.          05180040
048400                                                                  05190040
048500 01  LOCALS-ENROUTE-1.                                            05200040
048600     02  FILLER                   PIC X(05) VALUE SPACE.          05210040
048700     02  LOCAL-ENROUTE            PIC X(10) VALUE SPACE.          05220040
048800     02  FILLER                   PIC X(01) VALUE SPACE.          05230040
048900     02  LOCAL-ENROUTE-NAME       PIC X(20) VALUE SPACES.         05240040
049000     02  LOCAL-DUTY               PIC X(13) VALUE SPACES.         05250040
049100     02  LOCAL-ENROUTE-FROM       PIC X(14) VALUE SPACE.          05260040
049200     02  FILLER                   PIC X(01) VALUE SPACE.          05270040
049300     02  LOCAL-YR                 PIC X(02) VALUE SPACE.          05280040
049400     02  LOCAL-FIL1               PIC X(01) VALUE SPACE.          05290040
049500     02  LOCAL-MO                 PIC X(02) VALUE SPACE.          05300040
049600     02  LOCAL-FIL2               PIC X(01) VALUE SPACE.          05310040
049700     02  LOCAL-DY                 PIC X(02) VALUE SPACE.          05320040
049800     02  LOCAL-FIL3               PIC X(01) VALUE SPACE.          05330040
049900     02  LOCAL-HR-MN              PIC X(04) VALUE SPACE.          05340040
050000     02  FILLER                   PIC X(03) VALUE SPACE.          05350040
050100                                                                  05360040
050200 01  MORE-TURNS-CODE              PIC X(01) VALUE '0'.            05370040
050300     88  MORE-TURNS                         VALUE '1'.            05380040
050400     88  NO-MORE-TURNS                      VALUE '0'.            05390040
050500 01  POOLS-DONE-CODE              PIC X(01) VALUE '0'.            05400040
050600     88  POOLS-DONE                         VALUE '1'.            05410040
050700     88  POOLS-NOT-DONE                     VALUE '0'.            05420040
050800*                                                                 05430040
050900 01  CREWS-IN-TOWN-AREA.                                          05440040
051000     02  CREWS-IN-TOWN-TITLE.                                     05450040
051100         03  FILLER              PIC X(01)  VALUE SPACES.         05460040
051200         03  FILLER              PIC X(14)  VALUE                 05470040
051300             'CREWS IN TOWN '.                                    05480040
051400         03  FILLER              PIC X(65)  VALUE SPACES.         05490040
051500     02  CREWS-IN-TOWN-1.                                         05500040
051600         03  FILLER              PIC X(1)   VALUE SPACES.         05510040
051700         03  FILLER              PIC X(6)   VALUE                 05520040
051800             'POOL: '.                                            05530040
051900         03  CREWS-IN-TOWN-POOL  PIC X(14)  VALUE SPACES.         05540040
052000         03  FILLER              PIC X(17)  VALUE SPACES.         05550040
052100*CNC0516-BEG                                                      05560051
052200         03  FILLER              PIC X(14)  VALUE                 05570048
052300             'RESTED/STATUS'.                                     05580040
052400         03  FILLER              PIC X(28)  VALUE SPACES.         05590048
052500*CNC0516-END                                                      05600051
052600     02  CREWS-IN-TOWN-2.                                         05610040
052700         03  FILLER              PIC X(1)   VALUE SPACES.         05620040
052800         03  CREWS-IT-POSITION   PIC X(2)   VALUE SPACES.         05630040
052900         03  FILLER              PIC X(1)   VALUE SPACES.         05640040
053000         03  CREWS-IT-CRAFT      PIC X(2)   VALUE SPACES.         05650040
053100         03  FILLER              PIC X(1)   VALUE SPACES.         05660040
053200         03  CREWS-IT-TURN       PIC X(4)   VALUE SPACES.         05670040
053300         03  FILLER              PIC X(1)   VALUE SPACES.         05680040
053400*CNC0516-BEG                                                      05690051
053500         03  CREWS-IT-NAME       PIC X(25)  VALUE SPACES.         05700046
053600         03  FILLER              PIC X(1)   VALUE SPACES.         05710040
053700         03  CREWS-IT-RESTED     PIC X(14)  VALUE SPACES.         05720046
053800*CNC0516-END                                                      05730051
053900         03  FILLER              PIC X(01)  VALUE SPACES.         05740040
054000         03  CREWS-IT-HOS-AREA   PIC X(27)  VALUE SPACES.         05750040
054100     02  CREWS-MESS-LINE.                                         05760040
054200         03  FILLER              PIC X(09)  VALUE SPACES.         05770040
054300         03  CREW-MESSAGE        PIC X(28)  VALUE SPACES.         05780040
054400         03  FILLER              PIC X(43)  VALUE SPACES.         05790040
054500     02  LOCALS-IN-TOWN-1.                                        05800040
054600         03  FILLER              PIC X(1)   VALUE SPACES.         05810040
054700         03  LIT-ASGN-DESC       PIC X(7)   VALUE                 05820040
054800             'LOCAL: '.                                           05830040
054900         03  LOCAL-IN-TOWN       PIC X(6)   VALUE SPACES.         05840040
055000         03  FILLER              PIC X      VALUE SPACE.          05850040
055100         03  LOCAL-IN-TOWN-DESC  PIC X(20)  VALUE SPACES.         05860040
055200         03  FILLER              PIC X(1)  VALUE SPACES.          05870040
055300*CNC0516-BEG                                                      05880051
055400         03  FILLER              PIC X(14)  VALUE                 05890048
055500             'RESTED/STATUS'.                                     05900040
055600         03  FILLER              PIC X(30)  VALUE SPACES.         05910048
055700*CNC0516-END                                                      05920051
055800     02  LOCALS-IN-TOWN-2.                                        05930040
055900         03  LOCALS-IT-FILLER    PIC X(06)  VALUE SPACES.         05940040
056000         03  LOCALS-IT-CRAFT     PIC X(02)  VALUE SPACES.         05950040
056100         03  FILLER              PIC X(02)  VALUE SPACES.         05960040
056200*CNC0516-BEG                                                      05970051
056300         03  LOCALS-IT-NAME      PIC X(25)  VALUE SPACES.         05980048
056400*CNC0516-END                                                      05990051
056500         03  FILLER              PIC X(02)  VALUE SPACES.         06000040
056600*CNC0516-BEG                                                      06010051
056700         03  LOCALS-IT-RESTED    PIC X(14)  VALUE SPACES.         06020048
056800*CNC0516-END                                                      06030051
056900         03  FILLER              PIC X(02)  VALUE SPACES.         06040040
057000         03  LOCALS-IT-HOS-AREA  PIC X(27)  VALUE SPACES.         06050040
057100*                                                                 06060040
057200 01  CREWS-OUT-TOWN-AREA.                                         06070040
057300     02  CREWS-OUT-TOWN-TITLE.                                    06080040
057400         03  FILLER              PIC X      VALUE SPACES.         06090040
057500         03  FILLER              PIC X(18)  VALUE                 06100040
057600             'CREWS OUT OF TOWN '.                                06110040
057700         03  FILLER              PIC X(61)  VALUE SPACES.         06120040
057800     02  CREWS-OUT-TOWN-1.                                        06130040
057900         03  FILLER              PIC X(5)   VALUE SPACES.         06140040
058000         03  FILLER              PIC X(6)   VALUE                 06150040
058100             'POOL: '.                                            06160040
058200         03  CREWS-OUT-TOWN-POOL PIC X(14)  VALUE SPACES.         06170040
058300         03  FILLER              PIC X(34)  VALUE SPACES.         06180040
058400*CNC0516-BEG                                                      06190051
058500         03  FILLER              PIC X(14)  VALUE                 06200048
058600             'RESTED/STATUS'.                                     06210040
058700         03  FILLER              PIC X(07)  VALUE SPACES.         06220048
058800*CNC0516-END                                                      06230051
058900     02  CREWS-OUT-TOWN-1A.                                       06240040
059000         03  FILLER              PIC X(07)  VALUE SPACES.         06250040
059100         03  CREWS-OUT-TOWN-TERM PIC X(20)  VALUE SPACES.         06260040
059200         03  FILLER              PIC X(53)  VALUE SPACES.         06270040
059300     02  CREWS-OUT-TOWN-2.                                        06280040
059400         03  FILLER              PIC X(09)  VALUE SPACES.         06290040
059500         03  CREWS-OT-POSITION   PIC X(2)   VALUE SPACES.         06300040
059600         03  FILLER              PIC X(2)   VALUE SPACES.         06310040
059700         03  CREWS-OT-CRAFT      PIC X(10)  VALUE SPACES.         06320040
059800         03  FILLER              PIC X(2)   VALUE SPACES.         06330040
059900         03  CREWS-OT-TURN       PIC X(4)   VALUE SPACES.         06340040
060000         03  FILLER              PIC X(2)   VALUE SPACES.         06350040
060100         03  CREWS-OT-NAME       PIC X(26)  VALUE SPACES.         06360051
060200         03  FILLER              PIC X(2)   VALUE SPACES.         06370040
060300*CNC0516-BEG                                                      06380051
060400         03  CREWS-OT-RESTED     PIC X(14)  VALUE SPACES.         06390048
060500         03  FILLER              PIC X(07)  VALUE SPACES.         06400051
060600*CNC0516-END                                                      06410051
060700*                                                                 06420040
060800 01  EXTRABOARDS-AREA.                                            06430040
060900     02  EXTRABOARDS-TITLE.                                       06440040
061000         03  FILLER              PIC X      VALUE SPACES.         06450040
061100         03  FILLER              PIC X(14)  VALUE                 06460040
061200             'EXTRA BOARDS  '.                                    06470040
061300         03  FILLER              PIC X(65)  VALUE SPACES.         06480040
061400     02  EXTRABOARDS-1.                                           06490040
061500         03  FILLER              PIC X(2)   VALUE SPACES.         06500040
061600         03  EXTRABOARD-DESC     PIC X(37)  VALUE SPACES.         06510040
061700         03  FILLER              PIC X(02)  VALUE SPACES.         06520040
061800*CNC0516-BEG                                                      06530051
061900         03  FILLER              PIC X(14)  VALUE                 06540048
062000             'RESTED/STATUS'.                                     06550040
062100         03  FILLER              PIC X(25)  VALUE SPACES.         06560048
062200*CNC0516-END                                                      06570051
062300     02  EXTRABOARDS-2.                                           06580040
062400         03  FILLER              PIC X(05)  VALUE SPACES.         06590046
062500         03  EXTRABOARD-POSITION PIC X(03)  VALUE SPACES.         06600040
062600         03  FILLER              PIC X(02)  VALUE SPACES.         06610040
062700         03  EXTRABOARD-NAME     PIC X(26)  VALUE SPACES.         06620048
062800         03  FILLER              PIC X(02)  VALUE SPACES.         06630040
062900*CNC0516-BEG                                                      06640051
063000         03  EXTRABOARD-RESTED   PIC X(14)  VALUE SPACES.         06650046
063100         03  FILLER              PIC X(01)  VALUE SPACES.         06660048
063200*CNC0516-END                                                      06670051
063300         03  EXTRABOARD-HOS-AREA PIC X(27)  VALUE SPACES.         06680040
063400                                                                  06690040
063500 01  OB-DONE-CODE                PIC X      VALUE '0'.            06700040
063600     88  OB-NOT-DONE                        VALUE '0'.            06710040
063700     88  OB-DONE                            VALUE '1'.            06720040
063800*                                                                 06730040
063900 01  LAYOFF-AREA.                                                 06740040
064000     02  LAYOFF-TITLE.                                            06750040
064100         03  FILLER              PIC X      VALUE SPACES.         06760040
064200         03  LAYOFF-TITLE-FIELD  PIC X(79)  VALUE SPACES.         06770040
064300     02  LAYOFF-1.                                                06780040
064400         03  FILLER              PIC X(03)  VALUE SPACES.         06790040
064500         03  LAYOFF-NAME         PIC X(26)  VALUE SPACES.         06800040
064600         03  FILLER              PIC X(02)  VALUE SPACES.         06810040
064700         03  LAYOFF-STATUS       PIC X(18)  VALUE SPACES.         06820040
064800         03  FILLER              PIC X(01)  VALUE SPACES.         06830040
064900         03  LAYOFF-HOS-AREA     PIC X(30)  VALUE SPACES.         06840040
065000                                                                  06850040
065100 01  FORMAT-TIME.                                                 06860040
065200     02  F-YR                    PIC 99.                          06870040
065300     02  FILLER                  PIC X      VALUE '/'.            06880040
065400     02  F-MO                    PIC 99.                          06890040
065500     02  FILLER                  PIC X      VALUE '/'.            06900040
065600     02  F-DY                    PIC 99.                          06910040
065700     02  FILLER                  PIC X      VALUE '-'.            06920040
065800     02  F-HR-MN                 PIC 9999.                        06930040
065900                                                                  06940040
066000 01  CHECK-REST-TIME-CENT.                                        06950040
066100     02  CHECK-REST-CE           PIC 99     VALUE ZERO.           06960040
066200     02  CHECK-REST-DATE.                                         06970040
066300      03  CK-YR                  PIC 99     VALUE ZERO.           06980040
066400      03  CK-MO                  PIC 99     VALUE ZERO.           06990040
066500      03  CK-DY                  PIC 99     VALUE ZERO.           07000040
066600     02  CK-HR-MN                PIC 9999   VALUE ZERO.           07010040
066700                                                                  07020040
066800*CNC0516-BEG                                                      07030051
066900 01  WS-RESTED                   PIC X(14)  VALUE SPACES.         07040047
067000*CNC0516-END                                                      07050051
067100                                                                  07060040
067200 01  WS-CONVERT-TIME.                                             07070040
067300     02  WS-CONV-HRMN.                                            07080040
067400         03  WS-CONV-HR          PIC 99.                          07090040
067500             88  TIME-IS-AM                 VALUES 00 THRU 11.    07100040
067600             88  TIME-IS-PM                 VALUES 12 THRU 23.    07110040
067700         03  WS-CONV-MN          PIC 99.                          07120040
067800     02  WS-CONV-AMPM            PIC X.                           07130040
067900                                                                  07140040
068000 01  WORK-CNTLKEY.                                                07150040
068100     02  WK-CNTL-REC-TYPE        PIC XX VALUE SPACE.              07160040
068200     02  WK-CNTL-DIST            PIC XX VALUE SPACE.              07170040
068300     02  WK-CNTL-SUB-DIST        PIC XX VALUE SPACE.              07180040
068400     02  WK-CNTL-POOL            PIC XX VALUE SPACE.              07190040
068500     02  WK-CNTL-XB REDEFINES WK-CNTL-POOL PIC XX.                07200040
068600     02  WK-CNTL-CC REDEFINES WK-CNTL-XB   PIC XX.                07210040
068700     02  WK-CNTL-POOL-TYPE       PIC X VALUE SPACE.               07220040
068800     02  FILLER                  PIC X(11) VALUE SPACE.           07230040
068900 01  NORMAL-ASGNMT-FLAG          PIC X VALUE SPACE.               07240040
069000     88  NORM-ASGN-UFP                 VALUE 'U'.                 07250040
069100     88  NORM-ASGN-XB                  VALUE 'X'.                 07260040
069200     88  NORM-ASGN-AJ                  VALUE 'A'.                 07270040
069300 01  NORMAL-ASGNMT.                                               07280040
069400     02  NA-DIST                 PIC XX VALUE SPACE.              07290040
069500     02  NA-SUB-DIST             PIC XX VALUE SPACE.              07300040
069600     02  NA-AREA.                                                 07310040
069700       03  NA-1                  PIC X(6) VALUE SPACE.            07320040
069800       03  NA-2 REDEFINES NA-1.                                   07330040
069900         04  NA-POOL             PIC XX.                          07340040
070000         04  NA-TURN             PIC X(4).                        07350040
070100       03  NA-3 REDEFINES NA-1.                                   07360040
070200         04  NA-FILLER           PIC XX.                          07370040
070300         04  NA-XB-TURN          PIC X(4).                        07380040
070400       03  NA-CC                 PIC XX VALUE SPACE.              07390040
070500 01  TEMPORARY-ASGNMT-FLAG       PIC X VALUE SPACE.               07400040
070600     88  TEMP-UFP                      VALUE 'U'.                 07410040
070700     88  TEMP-ASGN-XB                  VALUE 'X'.                 07420040
070800     88  TEMP-AJ                       VALUE 'A'.                 07430040
070900 01  TEMPORARY-ASGNMT.                                            07440040
071000     02  TA-DIST                 PIC XX VALUE SPACE.              07450040
071100     02  TA-SUB-DIST             PIC XX VALUE SPACE.              07460040
071200     02  TA-AREA.                                                 07470040
071300       03  TA-1                  PIC X(6) VALUE SPACE.            07480040
071400       03  TA-2 REDEFINES TA-1.                                   07490040
071500         04  TA-POOL             PIC XX.                          07500040
071600         04  TA-TURN             PIC X(4).                        07510040
071700       03  TA-3 REDEFINES TA-1.                                   07520040
071800         04  TA-FILLER           PIC XX.                          07530040
071900         04  TA-XB-TURN          PIC X(4).                        07540040
072000       03  TA-CC                 PIC XX VALUE SPACE.              07550040
072100 01  TEMP-ASGNMT-DATE-TIME       PIC X(10).                       07560040
072200 01  TEMP-ASGN-XB-AUG-FLAG       PIC X VALUE SPACE.               07570040
072300     88  TEMP-ASGN-XB-AUG              VALUE 'Y'.                 07580040
072400 01  ON-DUTY-ASGNMT-FLAG         PIC X VALUE SPACE.               07590040
072500     88  ON-DUTY-UFP                   VALUE 'U'.                 07600040
072600     88  ON-DUTY-AJ                    VALUE 'A'.                 07610040
072700 01  ON-DUTY-OUT-TOWN-CODE       PIC 9(10) VALUE 9999999999.      07620040
072800     88  OUT-TOWN                          VALUE ZERO.            07630040
072900 01  ON-DUTY-ASGNMT.                                              07640040
073000     02  OD-DIST                 PIC XX VALUE SPACE.              07650040
073100     02  OD-SUB-DIST             PIC XX VALUE SPACE.              07660040
073200     02  OD-AREA.                                                 07670040
073300       03  OD-1                  PIC X(6) VALUE SPACE.            07680040
073400       03  OD-2 REDEFINES OD-1.                                   07690040
073500         04  OD-POOL             PIC XX.                          07700040
073600         04  OD-TURN             PIC X(4).                        07710040
073700       03  OD-CC                 PIC XX VALUE SPACE.              07720040
073800 01  OWNER-EMP-NBR               PIC 9(9) VALUE ZERO.             07730040
073900 01  TEMP-EMP-ONE                PIC 9(9) VALUE ZERO.             07740040
074000 01  ON-DUTY-EMP                 PIC 9(9) VALUE ZERO.             07750040
074100                                                                  07760040
074200 01  WS-HOLD-CNTL-FILE           PIC X(256) VALUE SPACES.         07770040
074300                                                                  07780040
074400 01  WS-SAVE-P27NCA-DIST         PIC XX   VALUE SPACES.           07790040
074500 01  WS-SAVE-P27NCA-SUB-DIST     PIC XX   VALUE SPACES.           07800040
074600                                                                  07810040
074700 01  PAGE-ARRAY-AREA.                                             07820040
074800     05  PAGE-LINE               PIC X(79) OCCURS 21 TIMES.       07830040
074900******************************************************************07840040
075000*                     COMMAREA COPYBOOKS                          07850040
075100******************************************************************07860040
075200     COPY PSTCOMM.                                                07870040
075300     COPY P27NCOMM.                                               07880040
075400                                                                  07890040
075500     COPY P27NTSQ.                                                07900040
075600     COPY P903COMM.                                               07910040
075700     COPY P907COMM.                                               07920040
075800     COPY P942COMM.                                               07930040
075900     COPY P956COMM.                                               07940040
076000     COPY PS42COMM.                                               07950040
076100     COPY PS94COMM.                                               07960040
076200******************************************************************07970040
076300*                       MAP COPYBOOKS                             07980040
076400******************************************************************07990040
076500     COPY PSM27NRE.                                               08000040
076600******************************************************************08010040
076700*                       MISC  COPYBOOKS                           08020040
076800******************************************************************08030040
076900     COPY PSTCB27N.                                               08040040
077000     COPY PSTCB43C.                                               08050040
077100     COPY PSTCB998.                                               08060040
077200     COPY WSMSG.                                                  08070040
077300     COPY PSTERAR.                                                08080040
077400     COPY PSTKEYS.                                                08090040
077500     COPY WSLOCODE.                                               08100040
077600     COPY PSTCCRFT.                                               08110040
077700     COPY WSCENTER.                                               08120040
077800     COPY WSZONE.                                                 08130040
077900     COPY WSAJDEFN.                                               08140040
078000     COPY WSDAYWK.                                                08150040
078100******************************************************************08160040
078200*                       FILE COPYBOOKS                            08170040
078300******************************************************************08180040
078400     COPY WSMSTR.                                                 08190040
078500     COPY WSSEN.                                                  08200040
078600     COPY WSUFP.                                                  08210040
078700     COPY WSEB.                                                   08220040
078800     COPY WSAJ.                                                   08230040
078900     COPY WSJS.                                                   08240040
079000     COPY WSASGN.                                                 08250040
079100     COPY WSCNTL.                                                 08260040
079200     COPY WSTRCN.                                                 08270040
079300     COPY WSANAL.                                                 08280040
079400     COPY WSSWASGN.                                               08290040
079500     COPY WSSYDTTM.                                               08300040
079600     COPY WSEDDATE.                                               08310040
079700*CNC0516-BEG                                                      08320044
079800     COPY WSTASK.                                                 08330044
079900     COPY WSMSTR2.                                                08340058
080000*CNC0516-END                                                      08350044
080100*C970-START                                                       08360064
080200     COPY WSAHIST.                                                08370064
080300*C970-END                                                         08380064
080400******************************************************************08390040
080500*                    I/O STATUS CHECK COPYBOOK                    08400040
080600******************************************************************08410040
080700 01  WS-RESPONSE                      PIC S9(08) COMP VALUE ZEROS.08420040
080800 01  FILE-STATUS                      PIC 9(004) VALUE ZEROES.    08430040
080900     COPY IOCODES.                                                08440040
081000                                                                  08450040
081100 LINKAGE SECTION.                                                 08460040
081200 01  DFHCOMMAREA.                                                 08470040
081300     05  FILLER                       PIC X(170).                 08480040
081400                                                                  08490040
081500 PROCEDURE DIVISION.                                              08500040
081600*=================================================================08510040
081700 P0000-MAINLINE.                                                  08520040
081800*=================================================================08530040
081900     EXEC CICS IGNORE CONDITION                                   08540040
082000               ERROR                                              08550040
082100     END-EXEC                                                     08560040
082200                                                                  08570040
082300     EXEC CICS HANDLE ABEND                                       08580040
082400               LABEL(P9999-GOT-PROBLEM)                           08590040
082500     END-EXEC                                                     08600040
082600                                                                  08610040
082700     IF EIBCALEN                    NOT  > ZEROS                  08620040
082800        PERFORM P9990-CLEAR-SCREEN                                08630040
082900     END-IF                                                       08640040
083000                                                                  08650040
083100     MOVE DFHCOMMAREA                   TO PSTCOMM-AREA           08660040
083200     IF EIBTRNID                    NOT  = P27N-TRAN              08670040
083300        PERFORM P9300-DELETE-ALL-PAGES                            08680040
083400        MOVE LOW-VALUES                 TO PSTS27N                08690040
083500        SET  CREATE-SCREEN              TO TRUE                   08700040
083600        PERFORM P0100-BEGIN-PROGRAM                               08710040
083700        MOVE 1                          TO P27NCA-CURR-PAGE       08720040
083800        PERFORM P9200-READ-PAGE                                   08730040
083900        PERFORM P9000-SEND-MAP-AND-RETURN                         08740040
084000     END-IF                                                       08750040
084100     MOVE EIBAID                        TO PF-CHECK               08760040
084200     IF EXIT-KEY                                                  08770040
084300        PERFORM P9300-DELETE-ALL-PAGES                            08780040
084400        PERFORM P9700-SETUP-RETURN                                08790040
084500     END-IF                                                       08800040
084600     PERFORM P0010-PROCESS-INPUT                                  08810040
084700     PERFORM P9000-SEND-MAP-AND-RETURN.                           08820040
084800*=================================================================08830040
084900 P0010-PROCESS-INPUT.                                             08840040
085000*=================================================================08850040
085100     MOVE P27N-MAP-VERSION(PSTCA-SUB)   TO P27N-MAP               08860040
085200     EXEC CICS RECEIVE MAP(P27N-MAP)                              08870040
085300                       MAPSET(P27N-SET)                           08880040
085400                       INTO(PSTS27N)                              08890040
085500                       RESP(WS-RESPONSE)                          08900040
085600     END-EXEC                                                     08910040
085700     MOVE WS-RESPONSE                   TO FILE-STATUS            08920040
085800     IF NOT SUCCESS                                               08930040
085900        MOVE 'P0010-01'                 TO ERR-PARAGRAPH          08940040
086000        PERFORM P9999-GOT-PROBLEM                                 08950040
086100     END-IF                                                       08960040
086200     MOVE SPACES                        TO SCR27N-ERRORMSG        08970040
086300                                                                  08980040
086400     IF PFKEY7                                                    08990040
086500        IF P27NCA-CURR-PAGE = 1                                   09000040
086600*          NO-FURTHER-SCROLL-MSG                                  09010040
086700           MOVE 'N012'                         TO MSGLOG-CODE     09020040
086800           PERFORM P9000-SEND-MAP-AND-RETURN                      09030040
086900        END-IF                                                    09040040
087000        SUBTRACT 1          FROM P27NCA-CURR-PAGE                 09050040
087100     ELSE                                                         09060040
087200        IF PFKEY8                                                 09070040
087300           IF P27NCA-CURR-PAGE = P27NCA-MAX-PAGE                  09080040
087400*                  'END OF FILE'                                  09090040
087500              MOVE 'E011' TO MSGLOG-CODE                          09100040
087600              PERFORM P9000-SEND-MAP-AND-RETURN                   09110040
087700           END-IF                                                 09120040
087800           ADD 1              TO P27NCA-CURR-PAGE                 09130040
087900        ELSE                                                      09140040
088000           IF ENTER-KEY                                           09150040
088100              MOVE 1          TO P27NCA-CURR-PAGE                 09160040
088200           ELSE                                                   09170040
088300*             INVALID-FUNC-MSG                                    09180040
088400              MOVE 'I006'     TO MSGLOG-CODE                      09190040
088500              PERFORM P9000-SEND-MAP-AND-RETURN                   09200040
088600           END-IF                                                 09210040
088700        END-IF                                                    09220040
088800     END-IF                                                       09230040
088900     PERFORM P9200-READ-PAGE.                                     09240040
089000*=================================================================09250040
089100 P0100-BEGIN-PROGRAM.                                             09260040
089200                                                                  09270040
089300     MOVE 1                 TO SCR-SUB                            09280040
089400     MOVE SPACES            TO PAGE-ARRAY-AREA                    09290040
089500                                                                  09300040
089600     PERFORM P0125-GET-TITLES                                     09310040
089700                                                                  09320040
089800     PERFORM P0150-GET-REPORTS.                                   09330040
089900                                                                  09340040
090000 P0125-GET-TITLES.                                                09350040
090100                                                                  09360040
090200     MOVE 'P0125'           TO ERR-PARAGRAPH                      09370040
090300                                                                  09380040
090400     MOVE P27NCA-DIST       TO POS-DIST                           09390040
090500                               XB-POS-DIST                        09400040
090600                               XB-TURN-DIST                       09410040
090700                               WK-CNTL-DIST                       09420040
090800     MOVE P27NCA-SUB-DIST   TO POS-SUB-DIST                       09430040
090900                               XB-POS-SUB-DIST                    09440040
091000                               XB-TURN-SUB-DIST                   09450040
091100                               WK-CNTL-SUB-DIST                   09460040
091200                                                                  09470040
091300     MOVE SPACES TO WS-CNTL-FILE                                  09480040
091400     SET COMPANY-TYPE-REC TO TRUE                                 09490040
091500     MOVE CNTLKEY-AREA TO CNTLKEY                                 09500040
091600     PERFORM P8000-READ-CNTLFILE                                  09510040
091700     IF COMPANY-TYPE-REC AND CNTL-COMPANY-NAME > SPACES           09520040
091800        MOVE 26                TO CTXT-UNF-FIELD-LEN              09530040
091900        MOVE CNTL-COMPANY-NAME TO CTXT-UNF-FIELD                  09540040
092000        PERFORM P8994-CENTER-TEXT                                 09550040
092100        MOVE CTXT-FOR-FIELD    TO TITLE-CUST-NAME                 09560040
092200     ELSE                                                         09570040
092300        MOVE '<< NONE ON FILE >>' TO TITLE-CUST-NAME              09580040
092400     END-IF                                                       09590040
092500                                                                  09600040
092600     PERFORM P9820-GET-CURRENT-TIME                               09610040
092700     MOVE ZEROS                TO DATE-CONVERSION-PARMS           09620040
092800     SET PARM-CONV             TO TRUE                            09630040
092900     MOVE WS-SYSTEM-DATE       TO PARM-PRI-DATE-GREG              09640040
093000     PERFORM P9991-CALL-P903-PROGRAM                              09650040
093100     MOVE PARM-PRI-DAY-OF-WEEK TO DAY1                            09660040
093200                                  WEEK-DAY                        09670040
093300     MOVE WS-LOCAL-YR          TO TIT-YR  CUR-YR                  09680072
093400     MOVE WS-LOCAL-MO          TO TIT-MO  CUR-MO                  09690072
093500     MOVE WS-LOCAL-DY          TO TIT-DY  CUR-DY                  09700072
093600     MOVE WS-LOCAL-HR          TO TIT-HR                          09710072
093700     MOVE WS-LOCAL-MN          TO TIT-MN                          09720072
093800                                                                  09730040
093900     MOVE SPACES              TO POOL-TABLE                       09740040
094000     MOVE SPACES              TO SPAREBOARD-TABLE                 09750040
094100     PERFORM P0160-LOAD-TABLES.                                   09760040
094200                                                                  09770040
094300 P0150-GET-REPORTS.                                               09780040
094400                                                                  09790040
094500     MOVE SPACES                 TO CNTLKEY                       09800040
094600                                    CNTLKEY-AREA                  09810040
094700     MOVE P27NCA-DIST            TO CNTL-DIST                     09820040
094800     SET DIST-TYPE-REC           TO TRUE                          09830040
094900     MOVE CNTLKEY-AREA           TO CNTLKEY                       09840040
095000     PERFORM P8000-READ-CNTLFILE                                  09850040
095100     IF CNTL-BCR-COMPANY                                          09860040
095200        OR CNTL-CN-COMPANY                                        09870040
095300        OR CNTL-ACR-COMPANY                                       09880040
095400        SET WS-CANADIAN-COMPANY  TO TRUE                          09890040
095500     ELSE                                                         09900040
095600        SET WS-US-COMPANY        TO TRUE                          09910040
095700     END-IF                                                       09920040
095800                                                                  09930040
095900     MOVE SPACES TO DIST-SDIST-POOL-FLAG                          09940040
096000                                                                  09950040
096100     PERFORM P1000-TRAINS-ENROUTE                                 09960040
096200                                                                  09970040
096300     IF P27NCA-DIST = WS-DIST-LM AND                              09980040
096400        P27NCA-SUB-DIST = WS-SUB-DIST-JX                          09990040
096500        SET DIST-SDIST-POOL-LM-MC-JN TO TRUE                      10000040
096600        MOVE WS-SUB-DIST-MC  TO P27NCA-SUB-DIST                   10010040
096700        PERFORM P1000-TRAINS-ENROUTE                              10020040
096800        MOVE WS-SUB-DIST-JX  TO P27NCA-SUB-DIST                   10030040
096900        MOVE SPACES          TO DIST-SDIST-POOL-FLAG              10040040
097000     END-IF                                                       10050040
097100                                                                  10060040
097200     PERFORM P2000-POOLS                                          10070040
097300                                                                  10080040
097400     IF P27NCA-DIST = WS-DIST-LM AND                              10090040
097500        P27NCA-SUB-DIST = WS-SUB-DIST-MC AND                      10100040
097600        DIST-SDIST-POOL-LM-MC-JN                                  10110040
097700        MOVE WS-SUB-DIST-JX  TO P27NCA-SUB-DIST                   10120040
097800        MOVE SPACES          TO DIST-SDIST-POOL-FLAG              10130040
097900     END-IF                                                       10140040
098000                                                                  10150040
098100     PERFORM P3000-SPAREBOARDS                                    10160040
098200                                                                  10170040
098300     PERFORM P5000-OFF-BOARDS                                     10180040
098400                                                                  10190040
098500     ADD 1                TO SCR-SUB                              10200040
098600     IF SCR-SUB > SCR-MAX                                         10210040
098700        PERFORM P7900-TITLE                                       10220040
098800     END-IF                                                       10230040
098900     MOVE '                              << END OF REPORT >>'     10240040
099000                          TO PAGE-LINE(SCR-SUB)                   10250040
099100     PERFORM P9100-WRITE-PAGE.                                    10260040
099200                                                                  10270040
099300 P0160-LOAD-TABLES.                                               10280040
099400                                                                  10290040
099500     SET  NOT-DONE                  TO TRUE                       10300040
099600     MOVE SPACES                    TO WS-CNTL-FILE               10310040
099700     MOVE P27NCA-DIST               TO CNTL-DIST                  10320040
099800     MOVE P27NCA-SUB-DIST           TO CNTL-SUB-DIST              10330040
099900     SET  POOL-TYPE-REC             TO TRUE                       10340040
100000     MOVE CNTLKEY-AREA              TO WORK-CNTLKEY               10350040
100100                                       CNTLKEY                    10360040
100200     PERFORM P8000-START-CNTLFILE                                 10370040
100300     IF SUCCESS                                                   10380040
100400        PERFORM VARYING POOL-SUB FROM 1 BY 1                      10390040
100500           UNTIL DONE OR POOL-SUB    > RPT-MAX                    10400040
100600           PERFORM P8000-READ-NEXT-CNTLFILE                       10410040
100700           IF  SUCCESS                                            10420040
100800           AND POOL-TYPE-REC                                      10430040
100900           AND CNTL-DIST             = P27NCA-DIST                10440040
101000           AND CNTL-SUB-DIST         = P27NCA-SUB-DIST            10450040
101100           AND CNTL-POOL-TYPE        = 'F'                        10460040
101200              MOVE CNTL-POOL-CODE   TO POOL-P(POOL-SUB)           10470040
101300           ELSE                                                   10480040
101400              IF  P27NCA-DIST        = WS-DIST-LM                 10490040
101500              AND P27NCA-SUB-DIST    = WS-SUB-DIST-JX             10500040
101600                 MOVE WS-POOL-JN    TO POOL-P(POOL-SUB)           10510040
101700              END-IF                                              10520040
101800              SET  DONE             TO TRUE                       10530040
101900           END-IF                                                 10540040
102000        END-PERFORM                                               10550040
102100        PERFORM P8000-END-CNTLFILE                                10560040
102200     END-IF                                                       10570040
102300                                                                  10580040
102400     SET  NOT-DONE                  TO TRUE                       10590040
102500     MOVE SPACES                    TO WS-CNTL-FILE               10600040
102600     MOVE P27NCA-DIST               TO CNTL-DIST                  10610040
102700     MOVE P27NCA-SUB-DIST           TO CNTL-SUB-DIST              10620040
102800     SET  EXTRABOARD-TYPE-REC       TO TRUE                       10630040
102900     MOVE CNTLKEY-AREA              TO WORK-CNTLKEY               10640040
103000                                       CNTLKEY                    10650040
103100     PERFORM P8000-START-CNTLFILE                                 10660040
103200     IF SUCCESS                                                   10670040
103300        PERFORM VARYING EXT-SUB FROM 1 BY 1                       10680040
103400           UNTIL DONE OR EXT-SUB     > RPT-MAX                    10690040
103500           PERFORM P8000-READ-NEXT-CNTLFILE                       10700040
103600           IF  SUCCESS                                            10710040
103700           AND EXTRABOARD-TYPE-REC                                10720040
103800           AND CNTL-DIST             = P27NCA-DIST                10730040
103900           AND CNTL-SUB-DIST         = P27NCA-SUB-DIST            10740040
104000               MOVE CNTL-XB         TO SB-P(EXT-SUB)              10750040
104100           ELSE                                                   10760040
104200              SET  DONE             TO TRUE                       10770040
104300           END-IF                                                 10780040
104400        END-PERFORM                                               10790040
104500        PERFORM P8000-END-CNTLFILE                                10800040
104600     END-IF                                                       10810040
104700     .                                                            10820040
104800                                                                  10830040
104900 P1000-TRAINS-ENROUTE.                                            10840040
105000                                                                  10850040
105100     MOVE 'P1000'       TO ERR-PARAGRAPH                          10860040
105200     IF NOT DIST-SDIST-POOL-LM-MC-JN                              10870040
105300        MOVE 0          TO SCR-SUB                                10880040
105400     END-IF                                                       10890040
105500     MOVE SPACES        TO TRAIN-ENROUTE                          10900040
105600     PERFORM P1010-GET-TRAINS-ENROUTE                             10910040
105700     PERFORM P1500-GET-LOCALS-ENROUTE.                            10920040
105800                                                                  10930040
105900 P1010-GET-TRAINS-ENROUTE.                                        10940040
106000                                                                  10950040
106100     MOVE 'P1010'              TO ERR-PARAGRAPH                   10960040
106200     MOVE SPACES               TO UFPTRAIN-AREA                   10970040
106300     MOVE P27NCA-DIST          TO DIST3 OF WS-UFP                 10980040
106400     MOVE P27NCA-SUB-DIST      TO SUB-DIST3 OF WS-UFP             10990040
106500     MOVE UFPTRAIN-AREA        TO UFPTRAIN                        11000040
106600***  START UFP-FILE KEY > UFPTRAIN                                11010040
106700***        INVALID KEY CONTINUE                                   11020040
106800***  END-START                                                    11030040
106900     EXEC CICS STARTBR                                            11040040
107000               DATASET(UFP-VIA-TRAIN-SYMBOL)                      11050040
107100               RIDFLD(UFPTRAIN)                                   11060040
107200               GTEQ                                               11070040
107300               RESP(WS-RESPONSE)                                  11080040
107400     END-EXEC                                                     11090040
107500     MOVE WS-RESPONSE           TO FILE-STATUS                    11100040
107600                                                                  11110040
107700     IF SUCCESS                                                   11120040
107800        SET TRAINS-NOT-DONE TO TRUE                               11130040
107900        MOVE ZEROES         TO TRAIN-COUNT                        11140040
108000        PERFORM UNTIL TRAINS-DONE                                 11150040
108100***       READ UFP-FILE NEXT RECORD INTO WS-UFP                   11160040
108200***            AT END CONTINUE                                    11170040
108300***       END-READ                                                11180040
108400          EXEC CICS READNEXT                                      11190040
108500                    DATASET(UFP-VIA-TRAIN-SYMBOL)                 11200040
108600                    INTO(WS-UFP)                                  11210040
108700                    LENGTH(UFPTRSYM-RLGTH)                        11220040
108800                    RIDFLD(UFPTRAIN)                              11230040
108900                    KEYLENGTH(UFPTRSYM-KLGTH)                     11240040
109000                    RESP(WS-RESPONSE)                             11250040
109100          END-EXEC                                                11260040
109200          MOVE WS-RESPONSE TO FILE-STATUS                         11270040
109300          IF DIST3     OF WS-UFP = P27NCA-DIST   AND              11280040
109400             SUB-DIST3 OF WS-UFP = P27NCA-SUB-DIST AND            11290040
109500             POOL-NAME OF WS-UFP = WS-POOL-JN    AND              11300040
109600             DIST-SDIST-POOL-LM-MC-JN                             11310040
109700             ADD 1 TO SCR-SUB                                     11320040
109800             PERFORM P1015-PROCESS-UFP-RECORD                     11330040
109900          ELSE                                                    11340040
110000             IF NOT DIST-SDIST-POOL-LM-MC-JN                      11350040
110100                PERFORM P1015-PROCESS-UFP-RECORD                  11360040
110200             ELSE                                                 11370040
110300                SET TRAINS-DONE TO TRUE                           11380040
110400             END-IF                                               11390040
110500          END-IF                                                  11400040
110600        END-PERFORM                                               11410040
110700        EXEC CICS ENDBR                                           11420040
110800                  DATASET(UFP-VIA-TRAIN-SYMBOL)                   11430040
110900                  RESP(WS-RESPONSE)                               11440040
111000        END-EXEC                                                  11450040
111100     ELSE                                                         11460040
111200        SET TRAINS-DONE TO TRUE                                   11470040
111300        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     11480040
111400           MOVE UFPTRAIN              TO ERR-KEY                  11490040
111500           MOVE 'CANNOT OPEN BROWSER UFPFILE' TO ERR-SENTENCE     11500086
111600           PERFORM P9999-GOT-PROBLEM                              11510040
111700        ELSE                                                      11520040
111800           IF NOT DIST-SDIST-POOL-LM-MC-JN                        11530040
111900              SET TRAINS-DONE TO TRUE                             11540040
112000              PERFORM P1020-SET-TRAIN-INFO THRU                   11550040
112100                      P1020-SET-TRAIN-INFO-EXIT                   11560040
112200           END-IF                                                 11570040
112300        END-IF                                                    11580040
112400     END-IF.                                                      11590040
112500                                                                  11600040
112600 P1015-PROCESS-UFP-RECORD.                                        11610040
112700                                                                  11620040
112800     IF SUCCESS                                                   11630040
112900        IF DIST3      OF WS-UFP = P27NCA-DIST AND                 11640040
113000           SUB-DIST3  OF WS-UFP = P27NCA-SUB-DIST                 11650040
113100           IF TRAIN-SYMBOL > SPACES                               11660040
113200              ADD  1     TO TRAIN-COUNT                           11670040
113300              PERFORM P1020-SET-TRAIN-INFO THRU                   11680040
113400                      P1020-SET-TRAIN-INFO-EXIT                   11690040
113500           END-IF                                                 11700040
113600        ELSE                                                      11710040
113700           SET TRAINS-DONE TO TRUE                                11720040
113800           PERFORM P1020-SET-TRAIN-INFO THRU                      11730040
113900                   P1020-SET-TRAIN-INFO-EXIT                      11740040
114000        END-IF                                                    11750040
114100     ELSE                                                         11760040
114200        SET TRAINS-DONE TO TRUE                                   11770040
114300        PERFORM P1020-SET-TRAIN-INFO THRU                         11780040
114400                P1020-SET-TRAIN-INFO-EXIT                         11790040
114500     END-IF.                                                      11800040
114600                                                                  11810040
114700 P1020-SET-TRAIN-INFO.                                            11820040
114800                                                                  11830040
114900     IF TRAINS-DONE                                               11840040
115000        IF TRAIN-COUNT NOT > ZEROES                               11850040
115100           PERFORM P1050-TRAINS-ENROUTE-TITLE                     11860040
115200           MOVE SPACES              TO TRAINS-ENROUTE-1           11870040
115300           MOVE 'NO TRAINS ENROUTE' TO TRAIN-ENROUTE-FROM         11880040
115400           MOVE SPACES              TO PAGE-LINE(SCR-SUB)         11890040
115500           MOVE TRAINS-ENROUTE-1    TO PAGE-LINE(SCR-SUB)         11900040
115600           ADD 1                    TO SCR-SUB                    11910040
115700           GO TO P1020-SET-TRAIN-INFO-EXIT                        11920040
115800        ELSE                                                      11930040
115900           PERFORM P1300-WRITE-TRAIN-DETAIL                       11940040
116000           GO TO P1020-SET-TRAIN-INFO-EXIT                        11950040
116100        END-IF                                                    11960040
116200     END-IF                                                       11970040
116300*C970-START                                                       11980064
116310     PERFORM P8710-READ-AHKEY2                                    11990068
116400     IF TRAIN-ENROUTE  > SPACES                                   12000068
116402        MOVE AH-ORIGIN-STATION TO WS-AH-ORIGIN-CURR               12010068
116410     ELSE                                                         12020068
116600        MOVE AH-ORIGIN-STATION TO WS-AH-ORIGIN-PREV               12030067
117000     END-IF                                                       12040064
117100*C970-END                                                         12050064
117200     IF TRAIN-SYMBOL = TRAIN-ENROUTE                              12060061
117300*C970-START                                                       12070062
117400        IF WS-AH-ORIGIN-PREV = WS-AH-ORIGIN-CURR                  12080064
117500           SET SAME-TRAIN-CALLED     TO TRUE                      12090064
117600        ELSE                                                      12100064
117700           SET SAME-TRAIN-NOT-CALLED TO TRUE                      12110064
117800        END-IF                                                    12120064
117900        IF SAME-TRAIN-NOT-CALLED                                  12130064
118000           PERFORM P1300-WRITE-TRAIN-DETAIL                       12140064
118100           PERFORM P1100-WRITE-NEW-TRAIN-LINE                     12150064
118200           PERFORM P1200-GET-TRAIN-DETAIL-INFO                    12160064
118300        ELSE                                                      12170064
118400           PERFORM P1200-GET-TRAIN-DETAIL-INFO                    12180064
118500        END-IF                                                    12190064
118600        MOVE WS-AH-ORIGIN-CURR      TO WS-AH-ORIGIN-PREV          12200064
118700*C970-END                                                         12210064
118800     ELSE                                                         12220040
118900        IF TRAIN-ENROUTE = SPACES                                 12230040
119000           IF NOT DIST-SDIST-POOL-LM-MC-JN                        12240040
119100              PERFORM P1050-TRAINS-ENROUTE-TITLE                  12250040
119200           END-IF                                                 12260040
119300           PERFORM P1100-WRITE-NEW-TRAIN-LINE                     12270040
119400           PERFORM P1200-GET-TRAIN-DETAIL-INFO                    12280040
119500        ELSE                                                      12290040
119600           PERFORM P1300-WRITE-TRAIN-DETAIL                       12300040
119700           PERFORM P1100-WRITE-NEW-TRAIN-LINE                     12310040
119800           PERFORM P1200-GET-TRAIN-DETAIL-INFO                    12320040
119900*C970-START                                                       12330064
120000           MOVE WS-AH-ORIGIN-CURR   TO WS-AH-ORIGIN-PREV          12340064
120100*C970-END                                                         12350064
120200        END-IF                                                    12360040
120300     END-IF.                                                      12370064
120400                                                                  12380040
120500 P1020-SET-TRAIN-INFO-EXIT.                                       12390040
120600     EXIT.                                                        12400040
120700                                                                  12410040
120800*                                                                 12420064
120900 P1050-TRAINS-ENROUTE-TITLE.                                      12430040
121000                                                                  12440040
121100     PERFORM P7900-TITLE                                          12450040
121200     ADD 1 TO SCR-SUB                                             12460040
121300     MOVE SPACES              TO PAGE-LINE(SCR-SUB)               12470040
121400     MOVE TRAINS-ENROUTE-TITLE TO PAGE-LINE(SCR-SUB)              12480040
121500     ADD 1 TO SCR-SUB.                                            12490040
121600                                                                  12500040
121700 P1100-WRITE-NEW-TRAIN-LINE.                                      12510040
121800                                                                  12520040
121900     MOVE SPACES           TO TRAINS-ENROUTE-1                    12530040
122000     MOVE '  ON DUTY AT: ' TO TRAIN-DUTY                          12540040
122100     MOVE TRAIN-SYMBOL   TO TRAIN-ENROUTE                         12550040
122200     MOVE SPACE          TO TRAIN-ENROUTE-FROM                    12560040
122300     MOVE SPACE          TO WORK-CNTLKEY                          12570040
122400     MOVE '03'           TO WK-CNTL-REC-TYPE                      12580040
122500     MOVE DIST OF WS-UFP TO WK-CNTL-DIST                          12590040
122600     MOVE SUB-DIST OF WS-UFP  TO WK-CNTL-SUB-DIST                 12600040
122700     MOVE POOL-NAME OF WS-UFP TO WK-CNTL-POOL                     12610040
122800     MOVE 'F' TO WK-CNTL-POOL-TYPE                                12620040
122900     MOVE WORK-CNTLKEY TO CNTLKEY                                 12630040
123000     PERFORM P8000-READ-CNTLFILE                                  12640040
123100     IF SUCCESS                                                   12650040
123200       IF IN-TOWN                                                 12660040
123300          MOVE CNTL-POOL-HOME           TO TRAIN-ENROUTE-FROM     12670040
123400       ELSE                                                       12680040
123500          MOVE CNTL-AWAY-TERM(IN-OUT-TERMINAL)                    12690040
123600                                        TO TRAIN-ENROUTE-FROM     12700040
123700       END-IF                                                     12710040
123800     ELSE                                                         12720040
123900       MOVE 'NO CNTL RECORD' TO TRAIN-ENROUTE-FROM                12730040
124000     END-IF                                                       12740040
124100     IF ON-DUTY-TIME > ZEROES                                     12750040
124200        MOVE ON-DUTY-TIME TO WORK-TIME                            12760040
124300        MOVE WK-YR TO TRAIN-YR                                    12770040
124400        MOVE WK-MO TO TRAIN-MO                                    12780040
124500        MOVE WK-DY TO TRAIN-DY                                    12790040
124600        MOVE WORK-HR-MN TO TRAIN-HR-MN                            12800040
124700        MOVE '/' TO TRAIN-FIL1 TRAIN-FIL2                         12810040
124800        MOVE ' - ' TO TRAIN-FIL3                                  12820040
124900     ELSE                                                         12830040
125000        MOVE SPACES TO TRAIN-HR-MN TRAIN-YR TRAIN-MO TRAIN-DY     12840040
125100             TRAIN-FIL1 TRAIN-FIL2 TRAIN-FIL3                     12850040
125200     END-IF                                                       12860040
125300     ADD 1                 TO SCR-SUB                             12870040
125400     IF SCR-SUB > SCR-MAX                                         12880040
125500       PERFORM P1050-TRAINS-ENROUTE-TITLE                         12890040
125600     END-IF                                                       12900040
125700     MOVE SPACES              TO PAGE-LINE(SCR-SUB)               12910040
125800     MOVE TRAINS-ENROUTE-1 TO PAGE-LINE(SCR-SUB)                  12920040
125900     ADD 1 TO SCR-SUB.                                            12930040
126000                                                                  12940040
126100 P1200-GET-TRAIN-DETAIL-INFO.                                     12950040
126200                                                                  12960040
126300     SET SEARCH-NOT-DONE TO TRUE                                  12970040
126400     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   12980040
126500                    OR SEARCH-DONE                                12990040
126600       IF POOL-CRAFT-CODE = CT-CRAFT-CODE(J)                      13000040
126700          SET SEARCH-DONE           TO TRUE                       13010040
126800          MOVE TURN-NBR OF WS-UFP   TO SAVE-CREW-TURN(J)          13020040
126900          PERFORM P7500-GET-UFP-EMPS                              13030040
127000          IF ON-DUTY-EMP > ZERO                                   13040040
127100             MOVE ON-DUTY-EMP TO MSTRNBRK                         13050040
127200             PERFORM P8500-READ-MASTER                            13060040
127300             MOVE EMP-NAME OF WS-MSTR TO SAVE-CREW-NAME(J)        13070040
127400             IF DISPLAY-HOS                                       13080040
127500                INITIALIZE PS94-COMMAREA-PARMS                    13090040
127600                MOVE EMP-NBR  OF WS-MSTR TO PS94-EMP-NBR          13100040
127700                PERFORM P4000-GET-HOS                             13110040
127800                MOVE WS-TOT-TM           TO SAVE-CREW-HOS-TOTAL(J)13120040
127900                MOVE WS-LIMBO-TM         TO SAVE-CREW-HOS-LIMBO(J)13130040
128000                MOVE WS-CONSEC-STARTS    TO SAVE-CREW-HOS-ST(J)   13140040
128100             ELSE                                                 13150040
128200                SET SAVE-CREW-DONT-DISPLAY(J) TO TRUE             13160040
128300             END-IF                                               13170040
128400          ELSE                                                    13180040
128500             MOVE '<< VACANT TURN >>' TO SAVE-CREW-NAME(J)        13190040
128600          END-IF                                                  13200040
128700          MOVE CT-CRAFT-CODE(J)       TO SAVE-CREW-CRAFT(J)       13210040
128800*         IF CT-CRAFT-CODE(J) = 'B1'                              13220040
128900*            MOVE 'REAR BRKM:'         TO SAVE-CREW-CRAFT(J)      13230040
129000*         ELSE                                                    13240040
129100*            IF CT-CRAFT-CODE(J) = 'B2'                           13250040
129200*               MOVE 'HEAD BRKM:'         TO SAVE-CREW-CRAFT(J)   13260040
129300*            ELSE                                                 13270040
129400*               MOVE CT-CRAFT-DESC(J)     TO SAVE-CREW-CRAFT(J)   13280040
129500*            END-IF                                               13290040
129600*          END-IF                                                 13300040
129700       END-IF                                                     13310040
129800     END-PERFORM.                                                 13320040
129900                                                                  13330040
130000 P1300-WRITE-TRAIN-DETAIL.                                        13340040
130100                                                                  13350040
130200     PERFORM VARYING J FROM 1 BY 1                                13360040
130300         UNTIL J > 30                                             13370040
130400            MOVE SPACES                   TO TRAINS-ENROUTE-2     13380040
130500            IF SAVE-CREW-CRAFT(J) > SPACES                        13390040
130600               MOVE SAVE-CREW-CRAFT(J)    TO TRAIN-CC             13400040
130700               MOVE SAVE-CREW-NAME(J)     TO TRAIN-EMP            13410040
130800               MOVE SAVE-CREW-TURN(J)     TO TRAIN-TURN           13420040
130900               IF SAVE-CREW-NAME(J) = '<< VACANT TURN >>' OR      13430040
131000                                      '<< VACANT JOB  >>'         13440040
131100                  MOVE SPACES             TO TRAIN-HOS-AREA       13450040
131200               ELSE                                               13460040
131300                  IF WS-CANADIAN-COMPANY OR                       13470040
131400                     SAVE-CREW-DONT-DISPLAY(J)                    13480040
131500                     MOVE SPACES          TO TRAIN-HOS-AREA       13490040
131600                  ELSE                                            13500040
131700                     STRING 'TOT '                                13510040
131800                       SAVE-CREW-HOS-TOTAL-HR(J) ':'              13520040
131900                       SAVE-CREW-HOS-TOTAL-MM(J)                  13530040
132000                         ' LIM '                                  13540040
132100                       SAVE-CREW-HOS-LIMBO-HR(J) ':'              13550040
132200                       SAVE-CREW-HOS-LIMBO-MM(J)                  13560040
132300                         ' ST:'                                   13570040
132400                       SAVE-CREW-HOS-ST(J)                        13580040
132500                       DELIMITED BY SIZE INTO TRAIN-HOS-AREA      13590040
132600                  END-IF                                          13600040
132700               END-IF                                             13610040
132800               IF SCR-SUB > SCR-MAX                               13620040
132900                  PERFORM P1050-TRAINS-ENROUTE-TITLE              13630040
133000               END-IF                                             13640040
133100               MOVE SPACES                TO PAGE-LINE(SCR-SUB)   13650040
133200               MOVE TRAINS-ENROUTE-2      TO PAGE-LINE(SCR-SUB)   13660040
133300               ADD    1                   TO SCR-SUB              13670040
133400            END-IF                                                13680040
133500     END-PERFORM                                                  13690040
133600                                                                  13700040
133700     MOVE SPACES              TO SAVE-CREW-INFORMATION            13710040
133800                                 SAVE-CREW-INFORMATION-HOS        13720040
133900                                 SAVE-TEMP-CREW-INFORMATION-HOS   13730040
134000                                 SAVE-TEMP-CREW-INFORMATION.      13740040
134100                                                                  13750040
134200 P1500-GET-LOCALS-ENROUTE.                                        13760040
134300                                                                  13770040
134400     MOVE 'P1500'              TO ERR-PARAGRAPH                   13780040
134500     MOVE SPACES               TO AJTRAINKEY-AREA                 13790040
134600     MOVE P27NCA-DIST          TO AJ-TRAIN-DIST                   13800040
134700     MOVE P27NCA-SUB-DIST      TO AJ-TRAIN-SUB-DIST               13810040
134800     MOVE AJTRAINKEY-AREA      TO AJTRAINKEY                      13820040
134900***  START AJ-FILE KEY > AJ-FS-TRAINKEY                           13830040
135000***        INVALID KEY CONTINUE                                   13840040
135100***  END-START                                                    13850040
135200     EXEC CICS STARTBR                                            13860040
135300               DATASET(AJ-VIA-TRAIN)                              13870040
135400               RIDFLD(AJTRAINKEY)                                 13880040
135500               GTEQ                                               13890040
135600               RESP(WS-RESPONSE)                                  13900040
135700     END-EXEC                                                     13910040
135800     MOVE WS-RESPONSE           TO FILE-STATUS                    13920040
135900     IF SUCCESS                                                   13930040
136000        SET LOCALS-NOT-DONE TO TRUE                               13940040
136100        MOVE ZEROES         TO LOCAL-COUNT                        13950040
136200        PERFORM UNTIL LOCALS-DONE                                 13960040
136300***        READ AJ-FILE NEXT RECORD INTO WS-ASGNED-JOBS           13970040
136400***               AT END CONTINUE                                 13980040
136500***        END-READ                                               13990040
136600           EXEC CICS READNEXT                                     14000040
136700                     DATASET(AJ-VIA-TRAIN)                        14010040
136800                     INTO(WS-ASGNED-JOBS)                         14020040
136900                     LENGTH(AJTRAIN-RLGTH)                        14030040
137000                     RIDFLD(AJTRAINKEY)                           14040040
137100                     KEYLENGTH(AJTRAIN-KLGTH)                     14050040
137200                     RESP(WS-RESPONSE)                            14060040
137300           END-EXEC                                               14070040
137400           MOVE WS-RESPONSE TO FILE-STATUS                        14080040
137500           IF SUCCESS                                             14090040
137600              IF AJ-TRAIN-DIST        = P27NCA-DIST AND           14100040
137700                 AJ-TRAIN-SUB-DIST    = P27NCA-SUB-DIST           14110040
137800                 IF DIST-SDIST-POOL-LM-MC-JN                      14120040
137900                    CONTINUE                                      14130040
138000                 ELSE                                             14140040
138100                    MOVE AJ-JOB-ASGN-ID    TO JOB-DEF-CHECK       14150040
138200                    IF AJ-JOB-ON-DUTY                             14160040
138300                       ADD  1     TO LOCAL-COUNT                  14170040
138400                       PERFORM P1520-SET-LOCAL-INFO THRU          14180040
138500                               P1520-SET-LOCAL-INFO-EXIT          14190040
138600                    END-IF                                        14200040
138700                 END-IF                                           14210040
138800              ELSE                                                14220040
138900                 SET LOCALS-DONE TO TRUE                          14230040
139000                 PERFORM P1520-SET-LOCAL-INFO THRU                14240040
139100                         P1520-SET-LOCAL-INFO-EXIT                14250040
139200              END-IF                                              14260040
139300           ELSE                                                   14270040
139400              SET LOCALS-DONE TO TRUE                             14280040
139500              PERFORM P1520-SET-LOCAL-INFO THRU                   14290040
139600                      P1520-SET-LOCAL-INFO-EXIT                   14300040
139700           END-IF                                                 14310040
139800        END-PERFORM                                               14320040
139900        EXEC CICS ENDBR                                           14330040
140000                  DATASET(AJ-VIA-TRAIN)                           14340040
140100                  RESP(WS-RESPONSE)                               14350040
140200        END-EXEC                                                  14360040
140300     ELSE                                                         14370040
140400        SET LOCALS-DONE TO TRUE                                   14380040
140500        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     14390040
140600           MOVE AJTRAINKEY            TO ERR-KEY                  14400040
140700           MOVE 'CANNOT OPEN AJFILE FOR BROWSE' TO ERR-SENTENCE   14410086
140800           PERFORM P9999-GOT-PROBLEM                              14420040
140900        ELSE                                                      14430040
141000           SET LOCALS-DONE TO TRUE                                14440040
141100           PERFORM P1520-SET-LOCAL-INFO THRU                      14450040
141200                   P1520-SET-LOCAL-INFO-EXIT                      14460040
141300        END-IF                                                    14470040
141400     END-IF.                                                      14480040
141500                                                                  14490040
141600 P1520-SET-LOCAL-INFO.                                            14500040
141700                                                                  14510040
141800     IF LOCALS-DONE                                               14520040
141900        IF DIST-SDIST-POOL-LM-MC-JN                               14530040
142000           PERFORM P1015-PROCESS-UFP-RECORD                       14540040
142100           GO TO P1520-SET-LOCAL-INFO-EXIT                        14550040
142200        END-IF                                                    14560040
142300        IF LOCAL-COUNT NOT > ZEROES                               14570040
142400           GO TO P1520-SET-LOCAL-INFO-EXIT                        14580040
142500        ELSE                                                      14590040
142600           PERFORM P1300-WRITE-TRAIN-DETAIL                       14600040
142700           GO TO P1520-SET-LOCAL-INFO-EXIT                        14610040
142800        END-IF                                                    14620040
142900     END-IF                                                       14630040
143000                                                                  14640040
143100     IF AJ-TRAIN-SYMBOL = LOCAL-ENROUTE                           14650040
143200        PERFORM P1700-GET-LOCAL-DETAIL-INFO                       14660040
143300     ELSE                                                         14670040
143400        IF LOCAL-ENROUTE = SPACES                                 14680040
143500           PERFORM P1600-WRITE-NEW-LOCAL-LINE                     14690040
143600           PERFORM P1700-GET-LOCAL-DETAIL-INFO                    14700040
143700        ELSE                                                      14710040
143800           PERFORM P1300-WRITE-TRAIN-DETAIL                       14720040
143900           PERFORM P1600-WRITE-NEW-LOCAL-LINE                     14730040
144000           PERFORM P1700-GET-LOCAL-DETAIL-INFO                    14740040
144100        END-IF                                                    14750040
144200     END-IF.                                                      14760040
144300                                                                  14770040
144400 P1520-SET-LOCAL-INFO-EXIT.                                       14780040
144500     EXIT.                                                        14790040
144600                                                                  14800040
144700 P1600-WRITE-NEW-LOCAL-LINE.                                      14810040
144800                                                                  14820040
144900     MOVE SPACES              TO LOCALS-ENROUTE-1                 14830040
145000     MOVE ' ON DUTY AT: '     TO LOCAL-DUTY                       14840040
145100     MOVE AJ-TRAIN-SYMBOL     TO LOCAL-ENROUTE                    14850040
145200     PERFORM P1800-GET-LOCAL-DESCS                                14860040
145300     MOVE WK-DESC             TO LOCAL-ENROUTE-NAME               14870040
145400     MOVE WK-FROM             TO LOCAL-ENROUTE-FROM               14880040
145500     IF AJ-ON-DUTY-TIME > ZEROES                                  14890040
145600        MOVE AJ-ON-DUTY-TIME TO WORK-TIME                         14900040
145700        MOVE WK-YR TO LOCAL-YR                                    14910040
145800        MOVE WK-MO TO LOCAL-MO                                    14920040
145900        MOVE WK-DY TO LOCAL-DY                                    14930040
146000        MOVE WORK-HR-MN TO LOCAL-HR-MN                            14940040
146100        MOVE '/' TO LOCAL-FIL1 LOCAL-FIL2                         14950040
146200        MOVE ' - ' TO LOCAL-FIL3                                  14960040
146300     ELSE                                                         14970040
146400        MOVE SPACES TO LOCAL-HR-MN LOCAL-YR LOCAL-MO LOCAL-DY     14980040
146500             LOCAL-FIL1 LOCAL-FIL2 LOCAL-FIL3                     14990040
146600     END-IF                                                       15000040
146700     ADD 1                    TO SCR-SUB                          15010040
146800     IF SCR-SUB > SCR-MAX                                         15020040
146900       PERFORM P1050-TRAINS-ENROUTE-TITLE                         15030040
147000     END-IF                                                       15040040
147100     MOVE SPACES              TO PAGE-LINE(SCR-SUB)               15050040
147200     MOVE LOCALS-ENROUTE-1 TO PAGE-LINE(SCR-SUB)                  15060040
147300     ADD 1 TO SCR-SUB.                                            15070040
147400                                                                  15080040
147500 P1700-GET-LOCAL-DETAIL-INFO.                                     15090040
147600                                                                  15100040
147700     SET SEARCH-NOT-DONE               TO TRUE                    15110040
147800     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   15120040
147900        OR SEARCH-DONE                                            15130040
148000       IF AJ-TRAIN-CC = CT-CRAFT-CODE(J)                          15140040
148100          SET SEARCH-DONE             TO TRUE                     15150040
148200          MOVE SPACES                 TO SAVE-CREW-TURN(J)        15160040
148300          PERFORM P7800-GET-AJ-EMPS                               15170040
148400          IF ON-DUTY-EMP > ZERO                                   15180040
148500             MOVE ON-DUTY-EMP TO MSTRNBRK                         15190040
148600             PERFORM P8500-READ-MASTER                            15200040
148700             MOVE EMP-NAME OF WS-MSTR TO SAVE-CREW-NAME(J)        15210040
148800             IF DISPLAY-HOS                                       15220040
148900                INITIALIZE PS94-COMMAREA-PARMS                    15230040
149000                MOVE EMP-NBR  OF WS-MSTR TO PS94-EMP-NBR          15240040
149100                PERFORM P4000-GET-HOS                             15250040
149200                MOVE WS-TOT-TM           TO SAVE-CREW-HOS-TOTAL(J)15260040
149300                MOVE WS-LIMBO-TM         TO SAVE-CREW-HOS-LIMBO(J)15270040
149400                MOVE WS-CONSEC-STARTS    TO SAVE-CREW-HOS-ST(J)   15280040
149500             ELSE                                                 15290040
149600                SET SAVE-CREW-DONT-DISPLAY(J) TO TRUE             15300040
149700             END-IF                                               15310040
149800          ELSE                                                    15320040
149900             MOVE '<< VACANT JOB  >>' TO SAVE-CREW-NAME(J)        15330040
150000          END-IF                                                  15340040
150100          MOVE CT-CRAFT-CODE(J)       TO SAVE-CREW-CRAFT(J)       15350040
150200**        IF CT-CRAFT-CODE(J) = 'B1'                              15360040
150300**           MOVE 'REAR BRKM:'         TO SAVE-CREW-CRAFT(J)      15370040
150400**        ELSE                                                    15380040
150500**           IF CT-CRAFT-CODE(J) = 'B2'                           15390040
150600**              MOVE 'HEAD BRKM:'         TO SAVE-CREW-CRAFT(J)   15400040
150700*            ELSE                                                 15410040
150800*               MOVE CT-CRAFT-DESC(J)     TO SAVE-CREW-CRAFT(J)   15420040
150900*            END-IF                                               15430040
151000*         END-IF                                                  15440040
151100       END-IF                                                     15450040
151200     END-PERFORM.                                                 15460040
151300                                                                  15470040
151400                                                                  15480040
151500 P1800-GET-LOCAL-DESCS.                                           15490040
151600                                                                  15500040
151700     MOVE SPACES                    TO WS-TRCN-FILE               15510040
151800                                       TRCNKEY3                   15520040
151900     IF AJ-TRAIN-DIST > SPACES                                    15530040
152000        MOVE AJ-TRAIN-DIST             TO TRCN-DIST3              15540040
152100     ELSE                                                         15550040
152200        MOVE AJ-JOB-DIST               TO TRCN-DIST3              15560040
152300     END-IF                                                       15570040
152400     IF AJ-TRAIN-SUB-DIST > SPACES                                15580040
152500        MOVE AJ-TRAIN-SUB-DIST         TO TRCN-SDIST3             15590040
152600     ELSE                                                         15600040
152700        MOVE AJ-JOB-SUB-DIST           TO TRCN-SDIST3             15610040
152800     END-IF                                                       15620040
152900     MOVE AJ-JOB-ASGN-ID            TO TRCN-ASSIGNMENT            15630040
153000     MOVE TRCN-KEY3                 TO TRCNKEY3                   15640040
153100***  READ TRCN-FILE INTO WS-TRCN-FILE                             15650040
153200***     KEY IS TRCN-FS-KEY3                                       15660040
153300***     INVALID KEY CONTINUE                                      15670040
153400***  END-READ                                                     15680040
153500     EXEC CICS READ                                               15690040
153600               DATASET(TRAIN-CN-VIA-DSD-ASGN)                     15700040
153700               INTO(WS-TRCN-FILE)                                 15710040
153800               LENGTH(TRAINCN-DSD-RLGTH)                          15720040
153900               RIDFLD(TRCNKEY3)                                   15730040
154000               KEYLENGTH(TRAINCN-DSD-KLGTH)                       15740040
154100               RESP(WS-RESPONSE)                                  15750040
154200     END-EXEC                                                     15760040
154300     MOVE WS-RESPONSE TO FILE-STATUS                              15770040
154400     IF SUCCESS                                                   15780040
154500        IF TRCN-DIST = (AJ-TRAIN-DIST OR AJ-JOB-DIST)          AND15790040
154600           TRCN-SDIST = (AJ-TRAIN-SUB-DIST OR AJ-JOB-SUB-DIST) AND15800040
154700           TRCN-DESCRIPTION > SPACES                              15810040
154800           MOVE TRCN-DESCRIPTION       TO WK-DESC                 15820040
154900        ELSE                                                      15830040
155000           MOVE SPACES               TO WK-DESC                   15840040
155100        END-IF                                                    15850040
155200     ELSE                                                         15860040
155300        MOVE SPACES                 TO WK-DESC                    15870040
155400        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     15880040
155500           MOVE 'P1800'             TO ERR-PARAGRAPH              15890040
155600           MOVE TRCNKEY3            TO ERR-KEY                    15900040
155700           MOVE 'COULD NOT READ ASGN FILE' TO ERR-SENTENCE        15910086
155800           PERFORM P9999-GOT-PROBLEM                              15920040
155900        END-IF                                                    15930040
156000     END-IF                                                       15940040
156100                                                                  15950040
156200     MOVE SPACES                     TO WS-CNTL-FILE              15960040
156300     SET SUB-DIST-TYPE-REC           TO TRUE                      15970040
156400     MOVE AJ-TRAIN-DIST              TO CNTL-DIST                 15980040
156500     MOVE AJ-TRAIN-SUB-DIST          TO CNTL-SUB-DIST             15990040
156600     MOVE CNTLKEY-AREA               TO CNTLKEY                   16000040
156700     PERFORM P8000-READ-CNTLFILE                                  16010040
156800     IF SUB-DIST-TYPE-REC AND CNTL-SUB-DIST-NAME > SPACES         16020040
156900        MOVE CNTL-SUB-DIST-NAME      TO WK-FROM                   16030040
157000     ELSE                                                         16040040
157100        MOVE '<NOT ON FILE> '        TO WK-FROM                   16050040
157200     END-IF.                                                      16060040
157300                                                                  16070040
157400 P2000-POOLS.                                                     16080040
157500                                                                  16090040
157600     MOVE 'P2000' TO ERR-PARAGRAPH                                16100040
157700     ADD 1                      TO SCR-SUB                        16110040
157800     IF SCR-SUB > SCR-MAX                                         16120040
157900       PERFORM P2300-POOL-TITLE                                   16130040
158000     ELSE                                                         16140040
158100       MOVE SPACES              TO PAGE-LINE(SCR-SUB)             16150040
158200       MOVE CREWS-IN-TOWN-TITLE  TO PAGE-LINE(SCR-SUB)            16160040
158300       ADD 1 TO SCR-SUB                                           16170040
158400     END-IF                                                       16180040
158500     MOVE ZEROES         TO WS-TERM                               16190040
158600     PERFORM P2100-POOLS-IN-TOWN                                  16200040
158700                                                                  16210040
158800     IF P27NCA-DIST = WS-DIST-LM AND                              16220040
158900        P27NCA-SUB-DIST = WS-SUB-DIST-MC AND                      16230040
159000        DIST-SDIST-POOL-LM-MC-JN                                  16240040
159100        MOVE WS-SUB-DIST-JX  TO P27NCA-SUB-DIST                   16250040
159200        MOVE SPACES          TO DIST-SDIST-POOL-FLAG              16260040
159300     END-IF                                                       16270040
159400                                                                  16280040
159500     PERFORM P2400-LOCALS-IN-TOWN                                 16290040
159600     MOVE 1              TO WS-TERM                               16300040
159700     ADD 1               TO SCR-SUB                               16310040
159800     IF SCR-SUB > SCR-MAX                                         16320040
159900       PERFORM P2300-POOL-TITLE                                   16330040
160000     ELSE                                                         16340040
160100       MOVE SPACES              TO PAGE-LINE(SCR-SUB)             16350040
160200       MOVE CREWS-OUT-TOWN-TITLE TO PAGE-LINE(SCR-SUB)            16360040
160300       ADD 1             TO SCR-SUB                               16370040
160400     END-IF                                                       16380040
160500     PERFORM P2100-POOLS-IN-TOWN.                                 16390040
160600                                                                  16400040
160700 P2100-POOLS-IN-TOWN.                                             16410040
160800                                                                  16420040
160900     MOVE 'P2100'                 TO ERR-PARAGRAPH                16430040
161000     MOVE SPACES                  TO DIST-SDIST-POOL-FLAG         16440040
161100     SET NOT-DONE                 TO TRUE                         16450040
161200     PERFORM VARYING POOL-SUB FROM 1 BY 1                         16460040
161300         UNTIL DONE OR POOL-SUB > RPT-MAX                         16470040
161400       IF POOL-P(POOL-SUB) = SPACES                               16480040
161500          SET DONE                TO TRUE                         16490040
161600       ELSE                                                       16500040
161700          MOVE ZEROS              TO CREW-COUNT                   16510040
161800          MOVE SPACES             TO WS-CNTL-FILE                 16520040
161900          MOVE SPACE              TO WORK-CNTLKEY                 16530040
162000                                     UNDISTURBED-REST-FLAG        16540040
162100          MOVE '3A'               TO WK-CNTL-REC-TYPE             16550040
162200          MOVE P27NCA-DIST        TO WK-CNTL-DIST                 16560040
162300          MOVE P27NCA-SUB-DIST    TO WK-CNTL-SUB-DIST             16570040
162400          IF DIST-SDIST-POOL-LM-MC-JN                             16580040
162500             MOVE WS-SUB-DIST-JX  TO WK-CNTL-SUB-DIST             16590040
162600                                     P27NCA-SUB-DIST              16600040
162700          END-IF                                                  16610040
162800          IF P27NCA-DIST = WS-DIST-LM AND                         16620040
162900             P27NCA-SUB-DIST = WS-SUB-DIST-JX AND                 16630040
163000             POOL-P(POOL-SUB) = WS-POOL-JN                        16640040
163100             MOVE WS-DIST-LM      TO WK-CNTL-DIST                 16650040
163200             MOVE WS-SUB-DIST-MC  TO WK-CNTL-SUB-DIST             16660040
163300                                     P27NCA-SUB-DIST              16670040
163400             SET DIST-SDIST-POOL-LM-MC-JN                         16680040
163500                                  TO TRUE                         16690040
163600          END-IF                                                  16700040
163700          MOVE POOL-P(POOL-SUB)   TO WK-CNTL-POOL                 16710040
163800          MOVE 'F'                TO WK-CNTL-POOL-TYPE            16720040
163900          MOVE WORK-CNTLKEY       TO CNTLKEY                      16730040
164000          PERFORM P8000-READ-CNTLFILE                             16740040
164100          MOVE CNTL-POOL-UNDIST-REST-FLAG                         16750040
164200                                  TO UNDISTURBED-REST-FLAG        16760040
164300          MOVE '03'               TO WK-CNTL-REC-TYPE             16770040
164400          MOVE WORK-CNTLKEY       TO CNTLKEY                      16780040
164500          PERFORM P8000-READ-CNTLFILE                             16790040
164600          MOVE SPACES          TO EN-ID-POOL-FLAG                 16800040
164700                                  TR-ID-POOL-FLAG                 16810040
164800                                  POOL-SERVICE-FLAG               16820040
164900          SET NEW-POOL         TO TRUE                            16830040
165000          IF SUCCESS                                              16840040
165100             MOVE WS-LOCAL-DATE-TIME-CENT                         16850040
165200                                TO CHECK-REST-TIME-CENT           16860040
165300              IF UNDISTURBED-REST                                 16870040
165400                 MOVE ZEROS        TO DATE-CONVERSION-PARMS       16880040
165500                 SET PARM-SUBTRACT TO TRUE                        16890040
165600                 MOVE CHECK-REST-DATE                             16900040
165700                                   TO PARM-PRI-DATE-GREG          16910040
165800                 MOVE CK-HR-MN     TO PARM-PRI-HRMN               16920040
165900                 MOVE '0400'       TO PARM-SEC-HRMN               16930040
166000                 PERFORM P9991-CALL-P903-PROGRAM                  16940040
166100                 MOVE PARM-RES-DATE-GREG                          16950040
166200                                   TO CHECK-REST-DATE             16960040
166300                 MOVE PARM-RES-GREG-CENT                          16970040
166400                                   TO CHECK-REST-CE               16980040
166500                 MOVE PARM-RES-HRMN                               16990040
166600                                   TO CK-HR-MN                    17000040
166700              END-IF                                              17010040
166800              IF CNTL-ALT-DIST > SPACE                            17020040
166900                 AND CNTL-ALT-SUB-DIST > SPACE                    17030040
167000                 IF CNTL-ALT-EN-POOL > SPACE                      17040040
167100                    SET EN-ID-POOL TO TRUE                        17050040
167200                 END-IF                                           17060040
167300                 IF CNTL-ALT-TR-POOL > SPACE                      17070040
167400                    SET TR-ID-POOL TO TRUE                        17080040
167500                 END-IF                                           17090040
167600              END-IF                                              17100040
167700             MOVE CNTL-POOL-SVC   TO POOL-SERVICE-FLAG            17110040
167800             MOVE CNTL-POOL-NAME  TO CREWS-IN-TOWN-POOL           17120040
167900             PERFORM P2110-LOAD-CC-ARRAY                          17130040
168000             ADD 1                 TO SCR-SUB                     17140040
168100             IF SCR-SUB > SCR-MAX                                 17150040
168200                 PERFORM P2300-POOL-TITLE                         17160040
168300             END-IF                                               17170040
168400             MOVE SPACES           TO PAGE-LINE(SCR-SUB)          17180040
168500             MOVE CREWS-IN-TOWN-1  TO PAGE-LINE(SCR-SUB)          17190040
168600             ADD 1                 TO SCR-SUB                     17200040
168700             MOVE SPACES           TO WORK-UFPPOS-KEY             17210040
168800             MOVE P27NCA-DIST      TO POS-DIST                    17220040
168900             MOVE P27NCA-SUB-DIST  TO POS-SUB-DIST                17230040
169000             MOVE POOL-P(POOL-SUB) TO POS-POOL                    17240040
169100             MOVE ZEROES           TO POS-TERMINAL                17250040
169200                                      POS-TIME                    17260040
169300             MOVE ZEROES           TO CREW-COUNT                  17270040
169400                                      SAVE-TERM                   17280040
169500             PERFORM P2150-CHECK-POOL-STATUS                      17290040
169600             IF SCR-SUB > SCR-MAX                                 17300040
169700                PERFORM P2300-POOL-TITLE                          17310040
169800             END-IF                                               17320040
169900             IF CREW-COUNT NOT > ZEROES                           17330040
170000                IF WS-TERM = ZEROES                               17340040
170100                   MOVE '<<   NO CREWS IN TOWN   >>'              17350040
170200                                      TO CREW-MESSAGE             17360040
170300                   MOVE SPACES           TO PAGE-LINE(SCR-SUB)    17370040
170400                   MOVE CREWS-MESS-LINE TO PAGE-LINE(SCR-SUB)     17380040
170500                   ADD 1              TO SCR-SUB                  17390040
170600                ELSE                                              17400040
170700                   MOVE '<< NO CREWS OUT OF TOWN >>'              17410040
170800                                      TO CREW-MESSAGE             17420040
170900                   MOVE SPACES           TO PAGE-LINE(SCR-SUB)    17430040
171000                   MOVE CREWS-MESS-LINE TO PAGE-LINE(SCR-SUB)     17440040
171100                   ADD 1              TO SCR-SUB                  17450040
171200                END-IF                                            17460040
171300             END-IF                                               17470040
171400          END-IF                                                  17480040
171500       END-IF                                                     17490040
171600     END-PERFORM.                                                 17500040
171700                                                                  17510040
171800 P2110-LOAD-CC-ARRAY.                                             17520040
171900                                                                  17530040
172000     MOVE SPACES                TO CRAFT-TABLE-AREA               17540040
172100     MOVE CNTL-EN-FI TO EN-FI-MARRIED-FLAG                        17550040
172200     IF EN-FI-MARRIED                                             17560040
172300        IF CNTL-POOL-FI-CRAFT NOT = ('Y' AND 'O')                 17570040
172400           SET EN-SE-MARRIED TO TRUE                              17580040
172500        END-IF                                                    17590040
172600     END-IF                                                       17600040
172700     MOVE CNTL-CO-B1-B2 TO CO-BK-MARRIED-FLAG                     17610040
172800     IF CO-BK-MARRIED                                             17620040
172900        IF CNTL-POOL-B1-CRAFT = ('Y' OR 'O')                      17630040
173000           SET CO-B1-MARRIED TO TRUE                              17640040
173100        END-IF                                                    17650040
173200        IF CNTL-POOL-B2-CRAFT = ('Y' OR 'O')                      17660040
173300           SET CO-B2-MARRIED TO TRUE                              17670040
173400        END-IF                                                    17680040
173500        IF CNTL-POOL-BG-CRAFT = ('Y' OR 'O')                      17690040
173600           SET CO-BG-MARRIED TO TRUE                              17700040
173700        END-IF                                                    17710040
173800     END-IF                                                       17720040
173900     MOVE CNTL-B1-B2 TO B1-B2-MARRIED-FLAG                        17730040
174000     MOVE CNTL-EN-ET TO EN-ET-MARRIED-FLAG                        17740040
174100     MOVE CNTL-CO-TT TO CO-TT-MARRIED-FLAG                        17750040
174200*                                                                 17760040
174300*    LOAD THE 'CRAFT CODES'                                       17770040
174400*                                                                 17780040
174500     MOVE ZERO                  TO CC-SUB                         17790040
174600                                   CC-SUB2                        17800040
174700*                                                                 17810040
174800*    PROCESS ENGINEER                                             17820040
174900*                                                                 17830040
175000     IF CNTL-POOL-EN-CRAFT = ('Y' OR 'O')                         17840040
175100        ADD 1                      TO CC-SUB                      17850040
175200        MOVE ENGINEER-MSTR-CC      TO PRIMARY-CC(CC-SUB)          17860040
175300        IF EN-FI-MARRIED                                          17870040
175400           ADD 1 TO CC-SUB2                                       17880040
175500           IF EN-SE-MARRIED                                       17890040
175600              MOVE 2ND-ENGINEER-MSTR-CC                           17900040
175700                                   TO ASSOC-CC(CC-SUB, CC-SUB2)   17910040
175800              IF CNTL-POOL-SE-CRAFT = 'O'                         17920040
175900                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        17930040
176000              END-IF                                              17940040
176100           ELSE                                                   17950040
176200              MOVE FIREMAN-MSTR-CC                                17960040
176300                                  TO ASSOC-CC(CC-SUB, CC-SUB2)    17970040
176400              IF CNTL-POOL-FI-CRAFT = 'O'                         17980040
176500                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        17990040
176600              END-IF                                              18000040
176700           END-IF                                                 18010040
176800        END-IF                                                    18020040
176900        IF EN-ET-MARRIED                                          18030040
177000           ADD 1 TO CC-SUB2                                       18040040
177100           MOVE ENGINEER-TRAINEE-MSTR-CC                          18050040
177200                     TO ASSOC-CC(CC-SUB, CC-SUB2)                 18060040
177300           SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE              18070040
177400        END-IF                                                    18080040
177500*                                                                 18090040
177600*       PROCESS RESERVE ENGINEER/SECOND ENGINEER                  18100040
177700*                                                                 18110040
177800        IF NOT EN-FI-MARRIED                                      18120040
177900           IF CNTL-POOL-FI-CRAFT = ('Y' OR 'O')                   18130040
178000              ADD 1 TO CC-SUB                                     18140040
178100              MOVE FIREMAN-MSTR-CC TO PRIMARY-CC(CC-SUB)          18150040
178200           ELSE                                                   18160040
178300              IF CNTL-POOL-SE-CRAFT  = ('Y' OR 'O')               18170040
178400                 ADD 1 TO CC-SUB                                  18180040
178500                 MOVE 2ND-ENGINEER-MSTR-CC TO PRIMARY-CC(CC-SUB)  18190040
178600              END-IF                                              18200040
178700           END-IF                                                 18210040
178800        END-IF                                                    18220040
178900*                                                                 18230040
179000*       PROCESS ENGINEER TRAINEE                                  18240040
179100*                                                                 18250040
179200        IF NOT EN-ET-MARRIED                                      18260040
179300           IF CNTL-POOL-EN-CRAFT = ('Y' OR 'O')                   18270040
179400              ADD 1 TO CC-SUB                                     18280040
179500              MOVE ENGINEER-TRAINEE-MSTR-CC TO PRIMARY-CC(CC-SUB) 18290040
179600           END-IF                                                 18300040
179700        END-IF                                                    18310040
179800     END-IF                                                       18320040
179900*                                                                 18330040
180000*    PROCESS CONDUCTOR                                            18340040
180100*                                                                 18350040
180200     IF CNTL-POOL-CO-CRAFT = ('Y' OR 'O')                         18360040
180300        ADD 1                       TO CC-SUB                     18370040
180400        MOVE ZEROS                  TO CC-SUB2                    18380040
180500        MOVE CONDUCTOR-MSTR-CC      TO PRIMARY-CC(CC-SUB)         18390040
180600        IF CO-BK-MARRIED                                          18400040
180700           IF CO-B1-MARRIED                                       18410040
180800              ADD 1 TO CC-SUB2                                    18420040
180900              MOVE B1-MSTR-CC TO ASSOC-CC(CC-SUB, CC-SUB2)        18430040
181000              IF CNTL-POOL-B1-CRAFT = 'O'                         18440040
181100                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        18450040
181200              END-IF                                              18460040
181300           END-IF                                                 18470040
181400           IF CO-B2-MARRIED                                       18480040
181500              ADD 1 TO CC-SUB2                                    18490040
181600              MOVE B2-MSTR-CC TO ASSOC-CC(CC-SUB, CC-SUB2)        18500040
181700              IF CNTL-POOL-B2-CRAFT = 'O'                         18510040
181800                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        18520040
181900              END-IF                                              18530040
182000           END-IF                                                 18540040
182100           IF CO-BG-MARRIED                                       18550040
182200              ADD 1 TO CC-SUB2                                    18560040
182300              MOVE BAGGAGEMAN-MSTR-CC TO ASSOC-CC(CC-SUB, CC-SUB2)18570040
182400              IF CNTL-POOL-BG-CRAFT = 'O'                         18580040
182500                 SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE        18590040
182600              END-IF                                              18600040
182700           END-IF                                                 18610040
182800        END-IF                                                    18620040
182900        IF CO-TT-MARRIED                                          18630040
183000           ADD 1 TO CC-SUB2                                       18640040
183100           MOVE TRAINMAN-TRAINEE-MSTR-CC                          18650040
183200                     TO ASSOC-CC(CC-SUB, CC-SUB2)                 18660040
183300           SET ASSOC-CC-OPT(CC-SUB, CC-SUB2) TO TRUE              18670040
183400        END-IF                                                    18680040
183500     END-IF                                                       18690040
183600*                                                                 18700040
183700*    PROCESS BRAKEMAN                                             18710071
183800*                                                                 18720040
183900     IF NOT CO-BK-MARRIED                                         18730071
184000        IF CNTL-POOL-B1-CRAFT = ('Y' OR 'O')                      18740071
184100           ADD 1                     TO CC-SUB                    18750071
184200           MOVE ZEROS                TO CC-SUB2                   18760071
184300           MOVE B1-MSTR-CC                TO PRIMARY-CC(CC-SUB)   18770071
184400           IF B1-B2-MARRIED                                       18780071
184500              ADD 1                  TO CC-SUB2                   18790071
184600              MOVE B2-MSTR-CC        TO ASSOC-CC(CC-SUB, CC-SUB2) 18800071
184700           END-IF                                                 18810071
184800        END-IF                                                    18820071
184900     END-IF                                                       18830071
185000*                                                                 18840040
185100*    PROCESS HEAD BRAKEMAN                                        18850071
185200*                                                                 18860040
185300     IF NOT CO-BK-MARRIED                                         18870071
185400        AND NOT B1-B2-MARRIED                                     18880071
185500        IF CNTL-POOL-B2-CRAFT = ('Y' OR 'O')                      18890071
185600           ADD 1 TO CC-SUB                                        18900071
185700           MOVE B2-MSTR-CC TO PRIMARY-CC(CC-SUB)                  18910071
185800        END-IF                                                    18920071
185900     END-IF                                                       18930071
186000*                                                                 18940040
186100*    PROCESS TRAINMAN TRAINEE                                     18950071
186200*                                                                 18960040
186300     IF NOT CO-TT-MARRIED                                         18970071
186400        ADD 1 TO CC-SUB                                           18980071
186500        MOVE TRAINMAN-TRAINEE-MSTR-CC TO PRIMARY-CC(CC-SUB)       18990071
186600     END-IF                                                       19000071
186700*                                                                 19010040
186800*    PROCESS BAGGAGEMAN                                           19020071
186900*                                                                 19030040
187000     IF NOT CO-BG-MARRIED                                         19040071
187100        IF CNTL-POOL-BG-CRAFT = ('Y' OR 'O')                      19050071
187200           ADD 1 TO CC-SUB                                        19060071
187300           MOVE BAGGAGEMAN-MSTR-CC TO PRIMARY-CC(CC-SUB)          19070071
187400        END-IF                                                    19080071
187500     END-IF                                                       19090071
187600*                                                                 19100040
187700*    PROCESS ASSISTANT CONDUCTOR                                  19110071
187800*                                                                 19120040
187900     IF CNTL-POOL-AC-CRAFT = ('Y' OR 'O')                         19130071
188000        ADD 1 TO CC-SUB                                           19140071
188100        MOVE ASST-COND-MSTR-CC TO PRIMARY-CC(CC-SUB)              19150071
188200     END-IF.                                                      19160071
188300                                                                  19170040
188400 P2150-CHECK-POOL-STATUS.                                         19180040
188500                                                                  19190040
188600     MOVE SPACES                 TO WS-UFP                        19200040
188700     MOVE ZEROS                  TO POOL-POS-CNT                  19210040
188800     SET POOLS-NOT-DONE          TO TRUE                          19220040
188900     PERFORM UNTIL POOLS-DONE                                     19230040
189000        MOVE ZEROS               TO CC-FOUND-FLAG                 19240040
189100        ADD 1                    TO POOL-POS-CNT                  19250040
189200        MOVE ZEROES              TO POS-TIME                      19260040
189300        IF MINE-TURN-SVC                                          19270040
189400           MOVE SPACES           TO POS-DATE-TIME                 19280040
189500        END-IF                                                    19290040
189600        PERFORM VARYING CC-SUB FROM 1 BY 1                        19300040
189700           UNTIL CC-SUB > CC-MAX                                  19310040
189800           IF PRIMARY-CC-LAST-KEY(CC-SUB) (11:14)                 19320040
189900                                           = '99999999999999'     19330040
190000              SET PRIMARY-CC-DONE(CC-SUB) TO TRUE                 19340040
190100           END-IF                                                 19350040
190200           IF  PRIMARY-CC(CC-SUB) > SPACE                         19360040
190300           AND NOT PRIMARY-CC-DONE(CC-SUB)                        19370040
190400              IF PRIMARY-CC-LAST-KEY(CC-SUB) NOT > SPACE          19380040
190500                 MOVE PRIMARY-CC(CC-SUB) TO POS-CC                19390040
190600              ELSE                                                19400040
190700                 MOVE PRIMARY-CC-LAST-KEY(CC-SUB)                 19410040
190800                                         TO WORK-UFPPOS-KEY       19420040
190900              END-IF                                              19430040
191000              IF  POS-DATE-TIME-NUM NUMERIC                       19440040
191100              AND POS-DATE-TIME-NUM > ZEROES                      19450040
191200                 ADD 1                   TO POS-DATE-TIME-NUM     19460040
191300              END-IF                                              19470040
191400              MOVE WORK-UFPPOS-KEY       TO UFPPOS                19480040
191500***           START UFP-FILE KEY > UFPPOS-FS-KEY                  19490040
191600***                 INVALID KEY CONTINUE                          19500040
191700***           END-START                                           19510040
191800              EXEC CICS STARTBR                                   19520040
191900                        DATASET(UFP-VIA-POSITION)                 19530040
192000                        RIDFLD(UFPPOS)                            19540040
192100                        GTEQ                                      19550040
192200                        RESP(WS-RESPONSE)                         19560040
192300              END-EXEC                                            19570040
192400              MOVE WS-RESPONSE  TO FILE-STATUS                    19580040
192500              IF SUCCESS                                          19590040
192600                 EXEC CICS READNEXT                               19600040
192700                           DATASET(UFP-VIA-POSITION)              19610040
192800                           INTO(WS-UFP)                           19620040
192900                           LENGTH(UFPPOS-RLGTH)                   19630040
193000                           RIDFLD(UFPPOS)                         19640040
193100                           KEYLENGTH(UFPPOS-KLGTH)                19650040
193200                           RESP(WS-RESPONSE)                      19660040
193300                 END-EXEC                                         19670040
193400                 MOVE WS-RESPONSE TO FILE-STATUS                  19680040
193500              END-IF                                              19690040
193600              IF SUCCESS                                          19700040
193700                 MOVE POOL-CRAFT-CODE2 IN WS-UFP                  19710040
193800                                       TO WS-CRAFT-CODE-CHECK     19720040
193900                 SET DE-YYMMDD-FORMAT     TO TRUE                 19730040
194000                 MOVE UFP-POS-DATE-TIME(1:6) TO DE-YYMMDD         19740040
194100                 PERFORM P8998-DATEEDIT                           19750040
194200                 MOVE DE-CCYYMMDD         TO DE-COMPARE1-DATE     19760040
194300                 MOVE UFP-POS-DATE-TIME(7:4)                      19770040
194400                                       TO DE-COMPARE1-TIME        19780040
194500                 IF DIST OF WS-UFP = POS-DIST                     19790040
194600                   AND SUB-DIST OF WS-UFP = POS-SUB-DIST          19800040
194700                   AND POOL-NAME OF WS-UFP = POS-POOL             19810040
194800                   AND POOL-CRAFT-CODE2 = POS-CC                  19820040
194900                   AND ((ENGINE-CRAFT AND EN-ID-POOL)             19830040
195000                     OR (TRAIN-CRAFT AND TR-ID-POOL)              19840040
195100                     OR MINE-TURN-SVC OR                          19850040
195200                   DE-COMPARE1-DATE-TIME NOT >                    19860040
195300                             WS-PRESENT-TIME-CENT)                19870040
195400                   MOVE UFPPOS-AREA                               19880040
195500                       TO PRIMARY-CC-LAST-KEY(CC-SUB)             19890040
195600                   SET CC-FOUND            TO TRUE                19900040
195700                   IF UFP-ON-BOARD                                19910040
195800                     IF (WS-TERM = ZEROES AND IN-TOWN)            19920040
195900                        OR                                        19930040
196000                        (WS-TERM > ZEROES AND OUT-OF-TOWN)        19940040
196100                         ADD 1                   TO CREW-COUNT    19950040
196200                         PERFORM P2200-GET-TURN-DETAIL            19960040
196300                         PERFORM P2250-WRITE-POOL-DETAIL          19970040
196400                     END-IF                                       19980040
196500                   END-IF                                         19990040
196600                 ELSE                                             20000040
196700                    SET PRIMARY-CC-DONE(CC-SUB) TO TRUE           20010040
196800                 END-IF                                           20020040
196900              ELSE                                                20030040
197000                 SET PRIMARY-CC-DONE(CC-SUB) TO TRUE              20040040
197100              END-IF                                              20050040
197200              EXEC CICS ENDBR                                     20060040
197300                        DATASET(UFP-VIA-POSITION)                 20070040
197400                        RESP(WS-RESPONSE)                         20080040
197500              END-EXEC                                            20090040
197600           END-IF                                                 20100040
197700        END-PERFORM                                               20110040
197800        IF NOT CC-FOUND                                           20120040
197900           SET POOLS-DONE TO TRUE                                 20130040
198000        END-IF                                                    20140040
198100     END-PERFORM.                                                 20150040
198200                                                                  20160040
198300 P2200-GET-TURN-DETAIL.                                           20170040
198400                                                                  20180040
198500     MOVE POOL-CRAFT-CODE2     TO WS-CRAFT-CODE-CHECK             20190040
198600                                                                  20200040
198700     IF WS-TERM > ZEROES                                          20210040
198800        IF SAVE-TERM NOT = IN-OUT-TERMINAL OF WS-UFP              20220040
198900           MOVE IN-OUT-TERMINAL OF WS-UFP TO SAVE-TERM            20230040
199000           MOVE WS-CNTL-FILE   TO SAVE-CNTL-AREA                  20240040
199100           MOVE SPACE          TO WORK-CNTLKEY                    20250040
199200           MOVE '03'           TO WK-CNTL-REC-TYPE                20260040
199300           MOVE DIST OF WS-UFP TO WK-CNTL-DIST                    20270040
199400           MOVE SUB-DIST OF WS-UFP  TO WK-CNTL-SUB-DIST           20280040
199500           MOVE POOL-NAME OF WS-UFP TO WK-CNTL-POOL               20290040
199600           MOVE 'F' TO WK-CNTL-POOL-TYPE                          20300040
199700           MOVE WORK-CNTLKEY TO CNTLKEY                           20310040
199800           PERFORM P8000-READ-CNTLFILE                            20320040
199900           MOVE CNTL-AWAY-TERM(SAVE-TERM)                         20330040
200000                                        TO CREWS-OUT-TOWN-TERM    20340040
200100           IF SCR-SUB > SCR-MAX                                   20350040
200200              PERFORM P2300-POOL-TITLE                            20360040
200300           END-IF                                                 20370040
200400           MOVE SPACES           TO PAGE-LINE(SCR-SUB)            20380040
200500           MOVE CREWS-OUT-TOWN-1A       TO PAGE-LINE(SCR-SUB)     20390040
200600           ADD 1                          TO SCR-SUB              20400040
200700           MOVE SAVE-CNTL-AREA TO WS-CNTL-FILE                    20410040
200800           MOVE CNTLKEY        TO WORK-CNTLKEY                    20420040
200900        END-IF                                                    20430040
201000     END-IF                                                       20440040
201100                                                                  20450040
201200     PERFORM P7500-GET-UFP-EMPS                                   20460040
201300                                                                  20470040
201400     MOVE TURN-NBR OF WS-UFP TO CREWS-IT-TURN                     20480040
201500                                                                  20490040
201600     MOVE SPACES             TO CREWS-IT-CRAFT                    20500040
201700     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   20510040
201800        IF POOL-CRAFT-CODE2 = CT-CRAFT-CODE(J)                    20520040
201900*         IF CT-CRAFT-CODE(J) = 'B1'                              20530040
202000*            MOVE 'REAR BRKM:'         TO CREWS-IT-CRAFT          20540040
202100*         ELSE                                                    20550040
202200*            IF CT-CRAFT-CODE(J) = 'B2'                           20560040
202300*               MOVE 'HEAD BRKM:'      TO CREWS-IT-CRAFT          20570040
202400*            ELSE                                                 20580040
202500*C913           MOVE CT-CRAFT-DESC(J)  TO CREWS-IT-CRAFT          20590040
202600                MOVE CT-CRAFT-CODE(J)  TO CREWS-IT-CRAFT          20600040
202700*            END-IF                                               20610040
202800*         END-IF                                                  20620040
202900        END-IF                                                    20630040
203000     END-PERFORM                                                  20640040
203100     IF CREWS-IT-CRAFT = SPACES                                   20650040
203200        MOVE 'UN'    TO CREWS-IT-CRAFT                            20660040
203300     END-IF                                                       20670040
203400                                                                  20680040
203500     IF NOT NEW-POOL                                              20690040
203600        IF POOL-POS-CNT NOT = SAVE-CNT                            20700040
203700            MOVE POOL-POS-CNT   TO CREWS-IT-POSITION              20710040
203800                                   SAVE-CNT                       20720040
203900        ELSE                                                      20730040
204000            MOVE SPACES         TO CREWS-IT-POSITION              20740040
204100        END-IF                                                    20750040
204200     ELSE                                                         20760040
204300        MOVE POOL-POS-CNT   TO CREWS-IT-POSITION                  20770040
204400                               SAVE-CNT                           20780040
204500        MOVE ZEROES         TO NEW-POOL-FLAG                      20790040
204600     END-IF                                                       20800040
204700                                                                  20810040
204800     MOVE SPACES            TO CREWS-IT-HOS-AREA                  20820040
204900     IF (NOT IN-TOWN OR SUB-DIST OF WS-UFP NOT = SUB-DIST2        20830040
205000         OF WS-UFP) AND ON-DUTY-EMP NOT > ZERO                    20840040
205100       IF UFP-BLANK-TURN                                          20850040
205200         MOVE 'VACNT/BLANKABLE TURN' TO CREWS-IT-NAME             20860040
205300       ELSE                                                       20870040
205400         IF UFP-CLOSED-TURN                                       20880040
205500           MOVE 'CLOSED TURN         '                            20890040
205600                TO CREWS-IT-NAME                                  20900040
205700         ELSE                                                     20910040
205800           MOVE '<< VACANT TURN >>' TO CREWS-IT-NAME              20920040
205900         END-IF                                                   20930040
206000       END-IF                                                     20940040
206100       MOVE ZERO TO MSTRNBRK                                      20950040
206200       IF TEMP-EMP-ONE > ZERO AND TEMP-EMP-ONE NOT = ON-DUTY-EMP  20960040
206300         MOVE TEMP-EMP-ONE TO MSTRNBRK                            20970040
206400       ELSE                                                       20980040
206500         IF OWNER-EMP-NBR NOT = ON-DUTY-EMP                       20990040
206600           MOVE OWNER-EMP-NBR TO MSTRNBRK                         21000040
206700         END-IF                                                   21010040
206800       END-IF                                                     21020040
206900       IF MSTRNBRK > ZERO                                         21030040
207000         PERFORM P8500-READ-MASTER                                21040040
207100         MOVE EMP-NAME TO CREWS-IT-NAME                           21050040
207200         IF WS-CANADIAN-COMPANY OR DONT-DISPLAY-HOS               21060040
207300            MOVE SPACES          TO CREWS-IT-HOS-AREA             21070040
207400         ELSE                                                     21080040
207500            INITIALIZE PS94-COMMAREA-PARMS                        21090040
207600*CNC0516-BEG                                                      21100051
207700            MOVE EMP-NBR OF WS-MSTR  TO PS94-EMP-NBR              21110042
207800*CNC0516-END                                                      21120051
207900            PERFORM P4000-GET-HOS                                 21130040
208000            STRING 'TOT '                                         21140040
208100              WS-TOT-TM-HH ':'                                    21150040
208200              WS-TOT-TM-MM                                        21160040
208300                ' LIM '                                           21170040
208400              WS-LIMBO-TM-HH ':'                                  21180040
208500              WS-LIMBO-TM-MM                                      21190040
208600                ' ST:'                                            21200040
208700              WS-CONSEC-STARTS                                    21210040
208800              DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA            21220040
208900         END-IF                                                   21230040
209000       END-IF                                                     21240040
209100     ELSE                                                         21250040
209200       IF ON-DUTY-EMP > ZERO                                      21260040
209300         MOVE ON-DUTY-EMP TO MSTRNBRK                             21270040
209400         PERFORM P8500-READ-MASTER                                21280040
209500         PERFORM P2210-GET-MASTER-INFO-FOR-TURN                   21290040
209600         MOVE EMP-NAME TO CREWS-IT-NAME                           21300040
209700         IF WS-CANADIAN-COMPANY OR DONT-DISPLAY-HOS               21310040
209800            MOVE SPACES          TO CREWS-IT-HOS-AREA             21320040
209900         ELSE                                                     21330040
210000            INITIALIZE PS94-COMMAREA-PARMS                        21340040
210100*CNC0516-BEG                                                      21350051
210200            MOVE EMP-NBR OF WS-MSTR TO PS94-EMP-NBR               21360042
210300*CNC0516-END                                                      21370051
210400            PERFORM P4000-GET-HOS                                 21380040
210500            STRING 'TOT '                                         21390040
210600              WS-TOT-TM-HH ':'                                    21400040
210700              WS-TOT-TM-MM                                        21410040
210800                ' LIM '                                           21420040
210900              WS-LIMBO-TM-HH ':'                                  21430040
211000              WS-LIMBO-TM-MM                                      21440040
211100                ' ST:'                                            21450040
211200              WS-CONSEC-STARTS                                    21460040
211300              DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA            21470040
211400         END-IF                                                   21480040
211500       ELSE                                                       21490040
211600         IF TEMP-EMP-ONE > ZERO                                   21500040
211700           MOVE TEMP-EMP-ONE TO MSTRNBRK                          21510040
211800           PERFORM P8500-READ-MASTER                              21520040
211900           PERFORM P2210-GET-MASTER-INFO-FOR-TURN                 21530040
212000           MOVE EMP-NAME TO CREWS-IT-NAME                         21540040
212100           IF WS-CANADIAN-COMPANY OR DONT-DISPLAY-HOS             21550040
212200              MOVE SPACES          TO CREWS-IT-HOS-AREA           21560040
212300           ELSE                                                   21570040
212400              INITIALIZE PS94-COMMAREA-PARMS                      21580040
212500*CNC0516-BEG                                                      21590051
212600              MOVE EMP-NBR OF WS-MSTR  TO PS94-EMP-NBR            21600042
212700*CNC0516-END                                                      21610051
212800              PERFORM P4000-GET-HOS                               21620040
212900              STRING 'TOT '                                       21630040
213000                WS-TOT-TM-HH ':'                                  21640040
213100                WS-TOT-TM-MM                                      21650040
213200                  ' LIM '                                         21660040
213300                WS-LIMBO-TM-HH ':'                                21670040
213400                WS-LIMBO-TM-MM                                    21680040
213500                  ' ST:'                                          21690040
213600                WS-CONSEC-STARTS                                  21700040
213700                DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA          21710040
213800           END-IF                                                 21720040
213900         ELSE                                                     21730040
214000            IF OWNER-EMP-NBR = ZERO                               21740040
214100               MOVE SPACES        TO CREWS-IT-HOS-AREA            21750040
214200               IF UFP-BLANK-TURN                                  21760040
214300                 MOVE '  BLANK  ' TO CREWS-IT-NAME                21770040
214400               ELSE                                               21780040
214500                 IF UFP-CLOSED-TURN                               21790040
214600                   MOVE '  CLOSED  ' TO CREWS-IT-NAME             21800040
214700                 ELSE                                             21810040
214800                   MOVE '<< OPEN TURN >>'                         21820040
214900                                     TO CREWS-IT-NAME             21830040
215000                 END-IF                                           21840040
215100               END-IF                                             21850040
215200            ELSE                                                  21860040
215300              MOVE OWNER-EMP-NBR TO MSTRNBRK                      21870040
215400              PERFORM P8500-READ-MASTER                           21880040
215500              PERFORM P2210-GET-MASTER-INFO-FOR-TURN              21890040
215600              MOVE EMP-NAME TO CREWS-IT-NAME                      21900040
215700              IF WS-CANADIAN-COMPANY OR DONT-DISPLAY-HOS          21910040
215800                 MOVE SPACES          TO CREWS-IT-HOS-AREA        21920040
215900              ELSE                                                21930040
216000                 INITIALIZE PS94-COMMAREA-PARMS                   21940040
216100*CNC0516-BEG                                                      21950051
216200                 MOVE EMP-NBR OF WS-MSTR  TO PS94-EMP-NBR         21960042
216300*CNC0516-END                                                      21970051
216400                 PERFORM P4000-GET-HOS                            21980040
216500                 STRING 'TOT '                                    21990040
216600                   WS-TOT-TM-HH ':'                               22000040
216700                   WS-TOT-TM-MM                                   22010040
216800                     ' LIM '                                      22020040
216900                   WS-LIMBO-TM-HH ':'                             22030040
217000                   WS-LIMBO-TM-MM                                 22040040
217100                     ' ST:'                                       22050040
217200                   WS-CONSEC-STARTS                               22060040
217300                   DELIMITED BY SIZE INTO CREWS-IT-HOS-AREA       22070040
217400              END-IF                                              22080040
217500            END-IF                                                22090040
217600         END-IF                                                   22100040
217700       END-IF                                                     22110040
217800     END-IF.                                                      22120040
217900*                                                                 22130040
218000 P2210-GET-MASTER-INFO-FOR-TURN.                                  22140040
218100*                                                                 22150040
218200     MOVE SPACES                 TO CREWS-IT-RESTED               22160040
218300     IF MSTRNBRK = ZEROES                                         22170040
218400         MOVE '<< VACANCY >>'    TO CREWS-IT-RESTED               22180040
218500     ELSE                                                         22190040
218600        IF AVAILABLE                                              22200040
218700           IF ON-DUTY-ASGNMT > SPACES                             22210040
218800               AND ON-DUTY-ASGNMT NOT = UFPTURN-AREA              22220040
218900               MOVE '<< VACANCY >>'  TO CREWS-IT-RESTED           22230040
219000           ELSE                                                   22240040
219100              IF TEMPORARY-ASGNMT > SPACES                        22250040
219200                  AND TEMPORARY-ASGNMT NOT = UFPTURN-AREA         22260040
219300                  MOVE '<< VACANCY >>'  TO CREWS-IT-RESTED        22270040
219400              ELSE                                                22280040
219500                   SET  DE-YYMMDD-FORMAT     TO TRUE              22290040
219600                   MOVE EMP-US-RSTD-DATE     TO DE-YYMMDD         22300040
219700                   PERFORM P8998-DATEEDIT                         22310040
219800                   MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE  22320040
219900                   MOVE EMP-US-RSTD-TIME     TO DE-COMPARE1-TIME  22330040
220000*                                                                 22340040
220100                   IF EMP-PERS-REST-NUM NOT > ZEROES              22350040
220200                      MOVE '000101010001'  TO EMP-PERS-REST       22360040
220300                   END-IF                                         22370040
220400                   PERFORM P5200-CHECK-COMPANY-CD                 22380040
220500                   IF APPLY-LEAD-TIME                             22390040
220600                      MOVE ZEROS          TO DATE-CONVERSION-PARMS22400040
220700                      SET PARM-ADD        TO TRUE                 22410040
220800                      MOVE EMP-PERS-REST-DATE                     22420040
220900                                           TO PARM-PRI-DATE-GREG  22430040
221000                      MOVE EMP-PERS-REST-TIME                     22440040
221100                                           TO PARM-PRI-HRMN       22450040
221200                      MOVE '0200'          TO PARM-SEC-HRMN       22460040
221300                      PERFORM P9991-CALL-P903-PROGRAM             22470040
221400                      MOVE PARM-RES-DATE-GREG                     22480040
221500                                           TO DE-COMPARE2-YYMMDD  22490040
221600                      MOVE PARM-RES-GREG-CENT                     22500040
221700                                           TO DE-COMPARE2-CE      22510040
221800                      MOVE PARM-RES-HRMN   TO DE-COMPARE2-TIME    22520040
221900                   ELSE                                           22530040
222000                      SET  DE-YYMMDD-FORMAT   TO TRUE             22540040
222100                      MOVE EMP-PERS-REST-DATE TO DE-YYMMDD        22550040
222200                      PERFORM P8998-DATEEDIT                      22560040
222300                      MOVE DE-CCYYMMDD        TO DE-COMPARE2-DATE 22570040
222400                      MOVE EMP-PERS-REST-TIME TO DE-COMPARE2-TIME 22580040
222500                   END-IF                                         22590040
222600*                                                                 22600040
222700                   SET  DE-YYMMDD-FORMAT     TO TRUE              22610040
222800                   MOVE EMP-MTOD-DATE        TO DE-YYMMDD         22620040
222900                   PERFORM P8998-DATEEDIT                         22630040
223000                   MOVE DE-CCYYMMDD          TO DE-COMPARE3-DATE  22640040
223100                   MOVE EMP-MTOD-TIME        TO DE-COMPARE3-TIME  22650040
223200                   SET  DE-YYMMDD-FORMAT     TO TRUE              22660040
223300                   MOVE CHECK-REST-DATE      TO DE-YYMMDD         22670040
223400                   PERFORM P8998-DATEEDIT                         22680040
223500                   MOVE DE-YYMMDD-CE         TO CHECK-REST-CE     22690040
223600                   IF DE-COMPARE1-DATE-TIME >                     22700040
223700                                  CHECK-REST-TIME-CENT            22710040
223800                      MOVE EMP-US-RSTD-DATE TO WORK-DATE          22720040
223900                      MOVE EMP-US-RSTD-TIME TO WORK-HR-MN         22730040
224000                      MOVE WK-YR            TO FORM-YR            22740040
224100                      MOVE WK-DY            TO FORM-DY            22750040
224200                      MOVE WK-MO            TO FORM-MO            22760040
224300                      MOVE WORK-HR-MN       TO FORM-HRMN          22770040
224400                      MOVE FORMAT-DATE-AREA TO CREWS-IT-RESTED    22780040
224500                   END-IF                                         22790040
224600                   IF DE-COMPARE2-DATE-TIME >                     22800040
224700                                  CHECK-REST-TIME-CENT  AND       22810040
224800                      DE-COMPARE2-DATE-TIME >                     22820040
224900                                  DE-COMPARE1-DATE-TIME           22830040
225000                      MOVE DE-COMPARE2-YYMMDD TO WORK-DATE        22840040
225100                      MOVE DE-COMPARE2-TIME   TO WORK-HR-MN       22850040
225200                      MOVE WK-YR            TO FORM-YR            22860040
225300                      MOVE WK-DY            TO FORM-DY            22870040
225400                      MOVE WK-MO            TO FORM-MO            22880040
225500                      MOVE WORK-HR-MN       TO FORM-HRMN          22890040
225600                      MOVE FORMAT-DATE-AREA TO CREWS-IT-RESTED    22900040
225700                   END-IF                                         22910040
225800                   IF DE-COMPARE3-DATE-TIME >                     22920040
225900                                  CHECK-REST-TIME-CENT  AND       22930040
226000                      DE-COMPARE3-DATE-TIME >                     22940040
226100                                  DE-COMPARE2-DATE-TIME AND       22950040
226200                      DE-COMPARE3-DATE-TIME >                     22960040
226300                                  DE-COMPARE1-DATE-TIME           22970040
226400                      MOVE EMP-MTOD-DATE    TO WORK-DATE          22980040
226500                      MOVE EMP-MTOD-TIME    TO WORK-HR-MN         22990040
226600                      MOVE WK-YR            TO FORM-YR            23000040
226700                      MOVE WK-DY            TO FORM-DY            23010040
226800                      MOVE WK-MO            TO FORM-MO            23020040
226900                      MOVE WORK-HR-MN       TO FORM-HRMN          23030040
227000                      MOVE FORMAT-DATE-AREA TO CREWS-IT-RESTED    23040040
227100                   END-IF                                         23050040
227200                   IF CREWS-IT-RESTED NOT > SPACE                 23060040
227300                      MOVE '** RESTED **' TO CREWS-IT-RESTED      23070040
227400                   END-IF                                         23080040
227500              END-IF                                              23090040
227600           END-IF                                                 23100040
227700        ELSE                                                      23110040
227800*CNC0516-BEG                                                      23120048
227900*           MOVE '<< VACANCY >>'  TO CREWS-IT-RESTED              23130044
228000            PERFORM P2215-GET-POOL-STATUS                         23140047
228100*CNC0516-END                                                      23150045
228200        END-IF                                                    23160040
228300     END-IF.                                                      23170040
228400*CNC0516-BEG                                                      23180044
228500*                                                                 23190045
228600 P2215-GET-POOL-STATUS.                                           23200047
228700                                                                  23210045
228800     INITIALIZE WS-CREWS-IT-RESTED                                23220044
228900     EVALUATE TRUE                                                23230044
229000          WHEN OFF-MILES-DAYS                                     23240044
229100               PERFORM P9830-RETRIEVE-CNTL-INFO                   23250044
229200               IF P956-ST-RSN-NBR-DAYS-REQ                        23260044
229300               OR P956-ST-RSN-EXP-DATE-REQ                        23270044
229400                  PERFORM P3265-GET-DUEBACK-DATE                  23280044
229500                  IF WS-DUEBACK-FOUND-Y                           23290044
229600                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE123300044
229700                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME123310044
229710*CNC0573 - BEG                                                    23320078
229720                     IF  PSTCA-FROM-FLD-MENU                      23330078
229730                     AND PSTCA-FLD-MENU-OPT NOT = '004'           23340078
229740                        IF P956-MASK-FLD-SCR-YES                  23350078
229741*CNC0576 - BEG                                                    23360082
229742                        OR P956-MASK-HOLD-TURN                    23370082
229751                           MOVE '**'            TO WS-LAYOFF-CODE123380082
229752                           IF P956-MASK-HOLD-TURN                 23390082
229753                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE123400082
229754                           ELSE                                   23410082
229755                              MOVE '**'      TO WS-LAYOFF-EM-CODE123420082
229756                           END-IF                                 23430082
229757*CNC0576 - END                                                    23440082
229760                        ELSE                                      23450078
229770                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE123460078
229780                           MOVE LAYOFF-EM-CODE  TO                23470078
229790                                WS-LAYOFF-EM-CODE1                23480078
229791                        END-IF                                    23490078
229792                     ELSE                                         23500078
229793                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE123510078
229794                        MOVE LAYOFF-EM-CODE     TO                23520078
229795                             WS-LAYOFF-EM-CODE1                   23530078
229796                     END-IF                                       23540078
229797*CNC0573 - END                                                    23550078
230100                     MOVE WS-CREWS-IT-RESTED    TO CREWS-IT-RESTED23560048
230200                  ELSE                                            23570046
230300                     MOVE '<< VACANCY >>'       TO                23580046
230400                           CREWS-IT-RESTED                        23590046
230500                  END-IF                                          23600046
230600               ELSE                                               23610046
230700                  PERFORM P3266-OFF-MILES-RETURN-DATE             23620046
230800                  MOVE WS-RETURN-DATE-1(1:4)    TO WS-RETURN-DATE123630053
230900                  MOVE SPACES                   TO WS-RETURN-TIME123640048
231000*CNC0573 - BEG                                                    23650078
231100                  IF  PSTCA-FROM-FLD-MENU                         23660078
231200                  AND PSTCA-FLD-MENU-OPT NOT = '004'              23670078
231210                     IF P956-MASK-FLD-SCR-YES                     23680078
231211*CNC0576 - BEG                                                    23690082
231212                     OR P956-MASK-HOLD-TURN                       23700082
231213                        MOVE '**'            TO WS-LAYOFF-CODE1   23710082
231214                        IF P956-MASK-HOLD-TURN                    23720082
231215                           MOVE 'HT'      TO WS-LAYOFF-EM-CODE1   23730082
231216                        ELSE                                      23740082
231217                           MOVE '**'      TO WS-LAYOFF-EM-CODE1   23750082
231218                        END-IF                                    23760082
231219*CNC0576 - END                                                    23770082
231230                     ELSE                                         23780078
231240                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE123790078
231250                        MOVE LAYOFF-EM-CODE     TO                23800078
231260                             WS-LAYOFF-EM-CODE1                   23810078
231270                     END-IF                                       23820078
231280                  ELSE                                            23830078
231290                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE123840078
231291                     MOVE LAYOFF-EM-CODE        TO                23850078
231292                          WS-LAYOFF-EM-CODE1                      23860078
231293                  END-IF                                          23870078
231294*CNC0573 - END                                                    23880078
231300                  MOVE WS-CREWS-IT-RESTED       TO CREWS-IT-RESTED23890048
231400               END-IF                                             23900046
231500          WHEN VACATION                                           23910046
231600               PERFORM P3267-VACATION-RETURN-DATE                 23920046
231700               MOVE WS-RETURN-DATE-1(1:4)       TO WS-RETURN-DATE123930053
231800               MOVE WS-RETURN-DATE-1(5:4)       TO WS-RETURN-TIME123940053
231810*CNC0573 - BEG                                                    23950078
231820               IF  PSTCA-FROM-FLD-MENU                            23960078
231830               AND PSTCA-FLD-MENU-OPT NOT = '004'                 23970078
231840                  PERFORM P9830-RETRIEVE-CNTL-INFO                23980078
231850                  IF P956-MASK-FLD-SCR-YES                        23990078
231851*CNC0576 - BEG                                                    24000082
231852                  OR P956-MASK-HOLD-TURN                          24010082
231853                     MOVE '**'                  TO WS-LAYOFF-CODE124020082
231854                     IF P956-MASK-HOLD-TURN                       24030082
231855                        MOVE 'HT'            TO WS-LAYOFF-EM-CODE124040082
231856                     ELSE                                         24050082
231857                        MOVE '**'            TO WS-LAYOFF-EM-CODE124060082
231858                     END-IF                                       24070082
231859*CNC0576 - END                                                    24080082
231870                  ELSE                                            24090078
231880                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE124100078
231890                     MOVE LAYOFF-EM-CODE        TO                24110078
231891                          WS-LAYOFF-EM-CODE1                      24120078
231892               ELSE                                               24130078
231893                  MOVE LAYOFF-CODE              TO WS-LAYOFF-CODE124140078
231894                  MOVE LAYOFF-EM-CODE           TO                24150078
231895                       WS-LAYOFF-EM-CODE1                         24160078
231896               END-IF                                             24170078
231897*CNC0573 - END                                                    24180078
232200               MOVE WS-CREWS-IT-RESTED          TO CREWS-IT-RESTED24190048
232300          WHEN EXCUSED-ABSENCE                                    24200046
232400           AND LAYOFF-EM-CODE = '69'                              24210046
232500               PERFORM P3265-GET-DUEBACK-DATE                     24220046
232600               IF WS-DUEBACK-FOUND-Y                              24230046
232700                  MOVE TASK-LO-EXP-DATE(3:4)    TO WS-RETURN-DATE124240048
232800                  MOVE TASK-LO-EXP-TIME         TO WS-RETURN-TIME124250048
232810*CNC0573 - BEG                                                    24260078
232820                 IF  PSTCA-FROM-FLD-MENU                          24270078
232830                 AND PSTCA-FLD-MENU-OPT NOT = '004'               24280078
232840                    PERFORM P9830-RETRIEVE-CNTL-INFO              24290078
232850                    IF P956-MASK-FLD-SCR-YES                      24300078
232851*CNC0576 - BEG                                                    24310082
232852                    OR P956-MASK-HOLD-TURN                        24320082
232853                       MOVE '**'                TO WS-LAYOFF-CODE124330082
232854                       IF P956-MASK-HOLD-TURN                     24340082
232855                          MOVE 'HT'          TO WS-LAYOFF-EM-CODE124350082
232856                       ELSE                                       24360082
232857                          MOVE '**'          TO WS-LAYOFF-EM-CODE124370082
232858                       END-IF                                     24380082
232859*CNC0576 - END                                                    24390082
232870                    ELSE                                          24400078
232880                       MOVE LAYOFF-CODE         TO WS-LAYOFF-CODE124410078
232890                       MOVE LAYOFF-EM-CODE      TO                24420078
232891                            WS-LAYOFF-EM-CODE1                    24430078
232892                    END-IF                                        24440078
232893                 ELSE                                             24450078
232894                    MOVE LAYOFF-CODE            TO WS-LAYOFF-CODE124460078
232895                    MOVE LAYOFF-EM-CODE         TO                24470078
232896                         WS-LAYOFF-EM-CODE1                       24480078
232897                 END-IF                                           24490078
232898*CNC0573 - END                                                    24500078
233200                  MOVE WS-CREWS-IT-RESTED       TO                24510048
233300                       CREWS-IT-RESTED                            24520048
233400               ELSE                                               24530046
233500                  MOVE '<< VACANCY >>'          TO CREWS-IT-RESTED24540048
233600               END-IF                                             24550046
233700          WHEN OTHER                                              24560046
233800               IF NOT AVAILABLE  AND                              24570046
233900                  NOT WORKING    AND                              24580046
234000                  NOT TO-PLACE                                    24590046
234100                  AND LAYOFF-TIME NUMERIC                         24600046
234200                  AND LAYOFF-TIME > ZERO                          24610046
234300                  PERFORM P3265-GET-DUEBACK-DATE                  24620046
234400                  IF WS-DUEBACK-FOUND-Y                           24630046
234500                    PERFORM P3268-CHECK-FOR-E95-DTTM              24640058
234600                    IF WS-E95 > SPACES                            24650057
234610*CNC0573 - BEG                                                    24660078
234620                       IF  PSTCA-FROM-FLD-MENU                    24670078
234630                       AND PSTCA-FLD-MENU-OPT NOT = '004'         24680078
234640                          PERFORM P9830-RETRIEVE-CNTL-INFO        24690079
234650                          IF P956-MASK-FLD-SCR-YES                24700078
234651*CNC0576 - BEG                                                    24710082
234652                          OR P956-MASK-HOLD-TURN                  24720082
234653                             MOVE '**'          TO WS-E95-CODE    24730082
234654                             IF P956-MASK-HOLD-TURN               24740082
234655                                MOVE 'HT'       TO WS-E95-EM-CODE 24750082
234656                             ELSE                                 24760082
234657                                MOVE '**'       TO WS-E95-EM-CODE 24770082
234658                             END-IF                               24780082
234670                          END-IF                                  24790083
234691                       END-IF                                     24800083
234694                       MOVE WS-E95              TO CREWS-IT-RESTED24810083
234695*CNC0576 - END                                                    24820083
234696*CNC0573 - END                                                    24830078
234800                    ELSE                                          24840057
234900                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE124850058
235000                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME124860058
235010*CNC0573 - BEG                                                    24870078
235020                     IF  PSTCA-FROM-FLD-MENU                      24880078
235030                     AND PSTCA-FLD-MENU-OPT NOT = '004'           24890078
235040                        PERFORM P9830-RETRIEVE-CNTL-INFO          24900078
235050                        IF P956-MASK-FLD-SCR-YES                  24910078
235051*CNC0576 - BEG                                                    24920082
235052                        OR P956-MASK-HOLD-TURN                    24930082
235053                           MOVE '**'            TO WS-LAYOFF-CODE124940082
235054                           IF P956-MASK-HOLD-TURN                 24950082
235055                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE124960082
235056                           ELSE                                   24970082
235057                              MOVE '**'      TO WS-LAYOFF-EM-CODE124980082
235058                           END-IF                                 24990082
235059*CNC0576 - END                                                    25000082
235070                        ELSE                                      25010078
235080                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE125020078
235090                           MOVE LAYOFF-EM-CODE  TO                25030078
235091                                WS-LAYOFF-EM-CODE1                25040078
235092                        END-IF                                    25050078
235093                     ELSE                                         25060078
235094                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE125070078
235095                        MOVE LAYOFF-EM-CODE     TO                25080078
235096                             WS-LAYOFF-EM-CODE1                   25090078
235097                     END-IF                                       25100078
235098*CNC0573 - END                                                    25110078
235400                     MOVE WS-CREWS-IT-RESTED    TO CREWS-IT-RESTED25120058
235500                    END-IF                                        25130057
235600                  ELSE                                            25140046
235700                    MOVE '<< VACANCY >>'        TO CREWS-IT-RESTED25150048
235800                  END-IF                                          25160046
235900               END-IF                                             25170046
236000     END-EVALUATE.                                                25180046
236100*CNC0516-END                                                      25190046
236200                                                                  25200040
236210                                                                  25210078
236300 P2250-WRITE-POOL-DETAIL.                                         25220040
236400                                                                  25230040
236500     IF SCR-SUB > SCR-MAX                                         25240040
236600       PERFORM P2300-POOL-TITLE                                   25250040
236700     END-IF                                                       25260040
236800     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  25270040
236900     MOVE CREWS-IN-TOWN-2 TO PAGE-LINE(SCR-SUB)                   25280040
237000     MOVE SPACES           TO CREWS-IN-TOWN-2                     25290040
237100     ADD 1 TO SCR-SUB.                                            25300040
237200                                                                  25310040
237300                                                                  25320040
237400 P2300-POOL-TITLE.                                                25330040
237500                                                                  25340040
237600     PERFORM P7900-TITLE                                          25350040
237700     ADD 1                 TO SCR-SUB                             25360040
237800     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  25370040
237900     IF WS-TERM = ZEROES                                          25380040
238000        MOVE CREWS-IN-TOWN-TITLE TO PAGE-LINE(SCR-SUB)            25390040
238100     ELSE                                                         25400040
238200        MOVE CREWS-OUT-TOWN-TITLE TO PAGE-LINE(SCR-SUB)           25410040
238300     END-IF                                                       25420040
238400     ADD 1 TO SCR-SUB.                                            25430040
238500                                                                  25440040
238600 P2400-LOCALS-IN-TOWN.                                            25450040
238700                                                                  25460040
238800*                                                                 25470040
238900*       THE FOLLOWING LOGIC WAS CHANGED 2/13/97 TO ALSO PRINT     25480040
239000*       YARD ASSIGNMENTS AT IC'S REQUEST. ERW                     25490040
239100*                                                                 25500040
239200     MOVE ZEROES               TO LOCAL-COUNT                     25510040
239300     MOVE SPACES               TO SAVE-CREW-INFORMATION           25520040
239400                                  SAVE-CREW-INFORMATION-HOS       25530040
239500                                  SAVE-TEMP-CREW-INFORMATION-HOS  25540040
239600                                  SAVE-TEMP-CREW-INFORMATION      25550040
239700     MOVE SPACES               TO AJJOBKEY-AREA                   25560040
239800     MOVE P27NCA-DIST          TO AJ-JOB-DIST                     25570040
239900     MOVE P27NCA-SUB-DIST      TO AJ-JOB-SUB-DIST                 25580040
240000     MOVE AJJOBKEY-AREA        TO AJJOBKEY                        25590040
240100***  START AJ-FILE KEY > AJ-FS-JOBKEY                             25600040
240200***        INVALID KEY CONTINUE                                   25610040
240300***  END-START                                                    25620040
240400     EXEC CICS STARTBR                                            25630040
240500               DATASET(AJ-VIA-JNAME-JCRAFT)                       25640040
240600               RIDFLD(AJJOBKEY)                                   25650040
240700               GTEQ                                               25660040
240800               RESP(WS-RESPONSE)                                  25670040
240900     END-EXEC                                                     25680040
241000     MOVE WS-RESPONSE           TO FILE-STATUS                    25690040
241100     IF SUCCESS                                                   25700040
241200        SET LOCALS-NOT-DONE TO TRUE                               25710040
241300        MOVE ZEROES         TO LOCAL-COUNT                        25720040
241400        PERFORM UNTIL LOCALS-DONE                                 25730040
241500***       READ AJ-FILE NEXT RECORD INTO WS-ASGNED-JOBS            25740040
241600***            AT END CONTINUE                                    25750040
241700***       END-READ                                                25760040
241800          EXEC CICS READNEXT                                      25770040
241900                    DATASET(AJ-VIA-JNAME-JCRAFT)                  25780040
242000                    INTO(WS-ASGNED-JOBS)                          25790040
242100                    LENGTH(AJNAMECR-RLGTH)                        25800040
242200                    RIDFLD(AJJOBKEY)                              25810040
242300                    KEYLENGTH(AJNAMECR-KLGTH)                     25820040
242400                    RESP(WS-RESPONSE)                             25830040
242500          END-EXEC                                                25840040
242600          MOVE WS-RESPONSE TO FILE-STATUS                         25850040
242700          IF SUCCESS                                              25860040
242800             IF AJ-JOB-DIST          = P27NCA-DIST AND            25870040
242900                AJ-JOB-SUB-DIST      = P27NCA-SUB-DIST            25880040
243000                SET DE-YYMMDD-FORMAT   TO TRUE                    25890040
243100                MOVE AJ-EFF-DATE       TO DE-YYMMDD               25900040
243200                PERFORM P8998-DATEEDIT                            25910040
243300                MOVE DE-CCYYMMDD       TO DE-COMPARE1-DATE        25920040
243400*               IF AJ-EFF-DATE        NOT  > SYSTEM-DATE          25930040
243500                IF DE-COMPARE1-DATE   NOT  > WS-SYSTEM-DATE-CENT  25940040
243600                   PERFORM P2405-CHECK-JOB-SCHEDULE               25950040
243700                   IF JOB-CURRENT                                 25960040
243800                      MOVE AJ-JOB-ASGN-ID TO JOB-DEF-CHECK        25970040
243900                      IF JOB-DEF-LOCAL-ASGN                       25980040
244000                         MOVE 'LOCAL:' TO LIT-ASGN-DESC           25990040
244100                      ELSE                                        26000040
244200                         MOVE 'YARD :' TO LIT-ASGN-DESC           26010040
244300                      END-IF                                      26020040
244400                      ADD 1 TO LOCAL-COUNT                        26030040
244500                      PERFORM P2410-SET-LOCAL-INFO THRU           26040040
244600                              P2410-SET-LOCAL-INFO-EXIT           26050040
244700                   END-IF                                         26060040
244800                END-IF                                            26070040
244900             ELSE                                                 26080040
245000                SET LOCALS-DONE         TO TRUE                   26090040
245100                PERFORM P2410-SET-LOCAL-INFO THRU                 26100040
245200                        P2410-SET-LOCAL-INFO-EXIT                 26110040
245300             END-IF                                               26120040
245400          ELSE                                                    26130040
245500             SET LOCALS-DONE         TO TRUE                      26140040
245600             PERFORM P2410-SET-LOCAL-INFO THRU                    26150040
245700                     P2410-SET-LOCAL-INFO-EXIT                    26160040
245800          END-IF                                                  26170040
245900        END-PERFORM                                               26180040
246000     END-IF                                                       26190040
246100     EXEC CICS ENDBR                                              26200040
246200               DATASET(AJ-VIA-JNAME-JCRAFT)                       26210040
246300               RESP(WS-RESPONSE)                                  26220040
246400     END-EXEC.                                                    26230040
246500                                                                  26240040
246600 P2405-CHECK-JOB-SCHEDULE.                                        26250040
246700                                                                  26260040
246800*                                                                 26270040
246900*    LOCK IN ON THE "ACTIVE" SCHEDULE                             26280040
247000*                                                                 26290040
247100     MOVE SPACES                    TO WORK-JS-KEY1               26300040
247200                                       CURRENT-JOB-FLAG           26310040
247300     MOVE AJJOBKEY-AREA             TO JS-KEY1                    26320040
247400     MOVE WS-SYSTEM-DATE            TO JSK1-EXP-DATE              26330040
247500     SET JSK1-ASGN-STATS            TO TRUE                       26340040
247600     MOVE JS-KEY1                   TO JSKEY1                     26350040
247700***  START JS-FILE                                                26360040
247800***        KEY NOT LESS JS-FS-KEY1                                26370040
247900***        INVALID KEY CONTINUE                                   26380040
248000***  END-START                                                    26390040
248100     PERFORM P8200-STARTBR-JS                                     26400040
248200     IF SUCCESS                                                   26410040
248300        PERFORM P8220-READNEXT-JS                                 26420040
248400     END-IF                                                       26430040
248500     IF SUCCESS                                                   26440040
248600        IF JSK1-ASGN-DIST = AJ-JOB-DIST                           26450040
248700         AND JSK1-ASGN-SUB-DIST = AJ-JOB-SUB-DIST                 26460040
248800         AND JSK1-ASGN = AJ-JOB-ASGN-ID                           26470040
248900         AND JSK1-ASGN-CC = AJ-JOB-ASGN-CC                        26480040
249000         AND JSK1-ASGN-STATS                                      26490040
249100           PERFORM P2407-CHECK-FOR-PROFILE                        26500040
249200        ELSE                                                      26510040
249300           SET NO-RECORD-FND        TO TRUE                       26520040
249400        END-IF                                                    26530040
249500     END-IF                                                       26540040
249600     PERFORM P8230-END-JS.                                        26550040
249700                                                                  26560040
249800 P2407-CHECK-FOR-PROFILE.                                         26570040
249900                                                                  26580040
250000     MOVE SPACES                    TO WS-TRCN-FILE               26590040
250100                                       TRCNKEY3                   26600040
250200     IF AJ-TRAIN-DIST > SPACES                                    26610040
250300        MOVE AJ-TRAIN-DIST          TO TRCN-DIST3                 26620040
250400     ELSE                                                         26630040
250500        MOVE AJ-JOB-DIST            TO TRCN-DIST3                 26640040
250600     END-IF                                                       26650040
250700     IF AJ-TRAIN-SUB-DIST > SPACES                                26660040
250800        MOVE AJ-TRAIN-SUB-DIST      TO TRCN-SDIST3                26670040
250900     ELSE                                                         26680040
251000        MOVE AJ-JOB-SUB-DIST        TO TRCN-SDIST3                26690040
251100     END-IF                                                       26700040
251200     MOVE AJ-JOB-ASGN-ID            TO TRCN-ASSIGNMENT            26710040
251300     MOVE TRCN-KEY3                 TO TRCNKEY3                   26720040
251400***  READ TRCN-FILE INTO WS-TRCN-FILE                             26730040
251500***     KEY IS TRCN-FS-KEY3                                       26740040
251600***     INVALID KEY CONTINUE                                      26750040
251700***  END-READ                                                     26760040
251800     EXEC CICS READ                                               26770040
251900               DATASET(TRAIN-CN-VIA-DSD-ASGN)                     26780040
252000               INTO(WS-TRCN-FILE)                                 26790040
252100               LENGTH(TRAINCN-DSD-RLGTH)                          26800040
252200               RIDFLD(TRCNKEY3)                                   26810040
252300               KEYLENGTH(TRAINCN-DSD-KLGTH)                       26820040
252400               RESP(WS-RESPONSE)                                  26830040
252500     END-EXEC                                                     26840040
252600     MOVE WS-RESPONSE TO FILE-STATUS                              26850040
252700     IF SUCCESS                                                   26860040
252800        IF TRCN-DIST = (AJ-TRAIN-DIST OR AJ-JOB-DIST)          AND26870040
252900           TRCN-SDIST = (AJ-TRAIN-SUB-DIST OR AJ-JOB-SUB-DIST) AND26880040
253000           TRCN-DESCRIPTION > SPACES                              26890040
253100           SET JOB-CURRENT           TO TRUE                      26900040
253200        END-IF                                                    26910040
253300     ELSE                                                         26920040
253400        IF NOT (NO-RECORD-FND OR END-OF-FILE)                     26930040
253500           MOVE 'P2407'             TO ERR-PARAGRAPH              26940040
253600           MOVE TRCNKEY3            TO ERR-KEY                    26950040
253700           MOVE 'READ TRCNKEY3'     TO ERR-SENTENCE               26960040
253800           PERFORM P9999-GOT-PROBLEM                              26970040
253900        END-IF                                                    26980040
254000     END-IF.                                                      26990040
254100                                                                  27000040
254200 P2410-SET-LOCAL-INFO.                                            27010040
254300                                                                  27020040
254400     IF LOCALS-DONE                                               27030040
254500        IF LOCAL-COUNT NOT > ZEROES                               27040040
254600           MOVE '<< NO LOCAL/YARD JOBS IN TOWN >>' TO CREW-MESSAGE27050040
254700           ADD 1                 TO SCR-SUB                       27060040
254800           IF SCR-SUB > SCR-MAX                                   27070040
254900              PERFORM P2300-POOL-TITLE                            27080040
255000           END-IF                                                 27090040
255100           MOVE SPACES           TO PAGE-LINE(SCR-SUB)            27100040
255200           MOVE CREWS-MESS-LINE  TO PAGE-LINE(SCR-SUB)            27110040
255300           ADD 1                 TO SCR-SUB                       27120040
255400           GO TO P2410-SET-LOCAL-INFO-EXIT                        27130040
255500         ELSE                                                     27140040
255600           PERFORM P2600-WRITE-LOCALS-IN-TOWN                     27150040
255700           GO TO P2410-SET-LOCAL-INFO-EXIT                        27160040
255800         END-IF                                                   27170040
255900     END-IF                                                       27180040
256000                                                                  27190040
256100     IF AJ-JOB-ASGN-ID = LOCAL-IN-TOWN                            27200040
256200        PERFORM P2500-GET-CREW-DETAIL                             27210040
256300     ELSE                                                         27220040
256400        IF LOCAL-IN-TOWN = SPACES                                 27230040
256500           PERFORM P2450-WRITE-NEW-IN-TOWN-LINE                   27240040
256600           PERFORM P2500-GET-CREW-DETAIL                          27250040
256700        ELSE                                                      27260040
256800           PERFORM P2600-WRITE-LOCALS-IN-TOWN                     27270040
256900           PERFORM P2450-WRITE-NEW-IN-TOWN-LINE                   27280040
257000           PERFORM P2500-GET-CREW-DETAIL                          27290040
257100        END-IF                                                    27300040
257200     END-IF.                                                      27310040
257300                                                                  27320040
257400 P2410-SET-LOCAL-INFO-EXIT.                                       27330040
257500     EXIT.                                                        27340040
257600                                                                  27350040
257700 P2450-WRITE-NEW-IN-TOWN-LINE.                                    27360040
257800                                                                  27370040
257900     MOVE SPACES              TO LOCAL-IN-TOWN                    27380040
258000                                 LOCAL-IN-TOWN-DESC               27390040
258100     MOVE AJ-JOB-ASGN-ID      TO LOCAL-IN-TOWN                    27400040
258200     PERFORM  P1800-GET-LOCAL-DESCS                               27410040
258300     MOVE WK-DESC             TO LOCAL-IN-TOWN-DESC               27420040
258400     ADD 1                    TO SCR-SUB                          27430040
258500     IF SCR-SUB > SCR-MAX                                         27440040
258600       PERFORM P2300-POOL-TITLE                                   27450040
258700     END-IF                                                       27460040
258800     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  27470040
258900     MOVE LOCALS-IN-TOWN-1    TO PAGE-LINE(SCR-SUB)               27480040
259000     ADD 1 TO SCR-SUB.                                            27490040
259100                                                                  27500040
259200 P2500-GET-CREW-DETAIL.                                           27510040
259300                                                                  27520040
259400     PERFORM P7800-GET-AJ-EMPS                                    27530040
259500     MOVE SPACES                TO TEMP-EMP-FLAG                  27540040
259600     MOVE ZEROES                TO MSTRNBRK                       27550040
259700     IF OWNER-EMP-NBR > ZEROES                                    27560040
259800        MOVE OWNER-EMP-NBR      TO MSTRNBRK                       27570040
259900        PERFORM P8500-READ-MASTER                                 27580040
260000        PERFORM P2510-GET-CREW-DETAIL                             27590040
260100     ELSE                                                         27600040
260200        MOVE '<< OPEN TURN >>'  TO                                27610040
260300                       EMP-NAME OF WS-MSTR                        27620040
260400        SET DONT-DISPLAY-HOS    TO TRUE                           27630040
260500        PERFORM P2510-GET-CREW-DETAIL                             27640040
260600     END-IF                                                       27650040
260700     IF TEMP-EMP-ONE > ZEROES                                     27660040
260800        SET  TEMP-EMP-FOUND     TO TRUE                           27670040
260900        MOVE TEMP-EMP-ONE       TO MSTRNBRK                       27680040
261000        PERFORM P8500-READ-MASTER                                 27690040
261100        PERFORM P2510-GET-CREW-DETAIL                             27700040
261200     END-IF.                                                      27710040
261300                                                                  27720040
261400 P2510-GET-CREW-DETAIL.                                           27730040
261500                                                                  27740040
261600     MOVE SPACES                TO WS-RESTED                      27750040
261700     IF MSTRNBRK = ZEROES                                         27760040
261800        MOVE '<< VACANCY >>'    TO WS-RESTED                      27770040
261900     ELSE                                                         27780040
262000        IF AVAILABLE                                              27790040
262100           CONTINUE                                               27800040
262200        ELSE                                                      27810040
262300           IF WORKING                                             27820040
262400              MOVE '<< WORKING >>' TO WS-RESTED                   27830040
262500           ELSE                                                   27840040
262600*CNC0516-BEG                                                      27850044
262700              PERFORM P2515-GET-CREW-STATUS                       27860047
262800*             MOVE '<< VACANCY >>' TO WS-RESTED                   27870044
262900*CNC0516-END                                                      27880044
263000           END-IF                                                 27890040
263100        END-IF                                                    27900040
263200     END-IF                                                       27910040
263300                                                                  27920040
263400     IF WS-RESTED = SPACES                                        27930040
263500        MOVE WS-LOCAL-DATE-TIME-CENT TO CHECK-REST-TIME-CENT      27940040
263600        MOVE ZEROS         TO DATE-CONVERSION-PARMS               27950040
263700        SET PARM-SUBTRACT  TO TRUE                                27960040
263800        MOVE CHECK-REST-DATE                                      27970040
263900                           TO PARM-PRI-DATE-GREG                  27980040
264000        MOVE CK-HR-MN      TO PARM-PRI-HRMN                       27990040
264100        MOVE '0400'        TO PARM-SEC-HRMN                       28000040
264200        PERFORM P9991-CALL-P903-PROGRAM                           28010040
264300        MOVE PARM-RES-DATE-GREG                                   28020040
264400                           TO CHECK-REST-DATE                     28030040
264500        MOVE PARM-RES-GREG-CENT                                   28040040
264600                           TO CHECK-REST-CE                       28050040
264700        MOVE PARM-RES-HRMN                                        28060040
264800                           TO CK-HR-MN                            28070040
264900*                                                                 28080040
265000        IF EMP-US-RSTD-NUM NOT > ZEROES                           28090040
265100           MOVE '000101010001'    TO EMP-US-RSTD                  28100040
265200        END-IF                                                    28110040
265300        SET  DE-YYMMDD-FORMAT     TO TRUE                         28120040
265400        MOVE EMP-US-RSTD-DATE     TO DE-YYMMDD                    28130040
265500        PERFORM P8998-DATEEDIT                                    28140040
265600        MOVE DE-CCYYMMDD          TO DE-COMPARE1-DATE             28150040
265700        MOVE EMP-US-RSTD-TIME     TO DE-COMPARE1-TIME             28160040
265800*                                                                 28170040
265900        IF EMP-PERS-REST-NUM NOT > ZEROES                         28180040
266000           MOVE '000101010001'    TO EMP-PERS-REST                28190040
266100        END-IF                                                    28200040
266200        PERFORM P5200-CHECK-COMPANY-CD                            28210040
266300        IF APPLY-LEAD-TIME                                        28220040
266400           MOVE ZEROS                TO DATE-CONVERSION-PARMS     28230040
266500           SET PARM-ADD              TO TRUE                      28240040
266600           MOVE EMP-PERS-REST-DATE   TO PARM-PRI-DATE-GREG        28250040
266700           MOVE EMP-PERS-REST-TIME   TO PARM-PRI-HRMN             28260040
266800           MOVE '0200'               TO PARM-SEC-HRMN             28270040
266900           PERFORM P9991-CALL-P903-PROGRAM                        28280040
267000           MOVE PARM-RES-DATE-GREG   TO DE-COMPARE2-YYMMDD        28290040
267100           MOVE PARM-RES-GREG-CENT   TO DE-COMPARE2-CE            28300040
267200           MOVE PARM-RES-HRMN        TO DE-COMPARE2-TIME          28310040
267300        ELSE                                                      28320040
267400           SET  DE-YYMMDD-FORMAT     TO TRUE                      28330040
267500           MOVE EMP-PERS-REST-DATE   TO DE-YYMMDD                 28340040
267600           PERFORM P8998-DATEEDIT                                 28350040
267700           MOVE DE-CCYYMMDD          TO DE-COMPARE2-DATE          28360040
267800           MOVE EMP-PERS-REST-TIME   TO DE-COMPARE2-TIME          28370040
267900        END-IF                                                    28380040
268000*                                                                 28390040
268100        SET  DE-YYMMDD-FORMAT     TO TRUE                         28400040
268200        MOVE EMP-MTOD-DATE        TO DE-YYMMDD                    28410040
268300        PERFORM P8998-DATEEDIT                                    28420040
268400        MOVE DE-CCYYMMDD          TO DE-COMPARE3-DATE             28430040
268500        MOVE EMP-MTOD-TIME        TO DE-COMPARE3-TIME             28440040
268600        IF DE-COMPARE1-DATE-TIME > CHECK-REST-TIME-CENT           28450040
268700           MOVE EMP-US-RSTD-DATE TO WORK-DATE                     28460040
268800           MOVE EMP-US-RSTD-TIME TO WORK-HR-MN                    28470040
268900           MOVE WK-YR            TO FORM-YR                       28480040
269000           MOVE WK-DY            TO FORM-DY                       28490040
269100           MOVE WK-MO            TO FORM-MO                       28500040
269200           MOVE WORK-HR-MN       TO FORM-HRMN                     28510040
269300           MOVE FORMAT-DATE-AREA TO WS-RESTED                     28520040
269400        END-IF                                                    28530040
269500        IF DE-COMPARE2-DATE-TIME > CHECK-REST-TIME-CENT AND       28540040
269600           DE-COMPARE2-DATE-TIME > DE-COMPARE1-DATE-TIME          28550040
269700           MOVE DE-COMPARE2-YYMMDD   TO WORK-DATE                 28560040
269800           MOVE DE-COMPARE2-TIME     TO WORK-HR-MN                28570040
269900           MOVE WK-YR                TO FORM-YR                   28580040
270000           MOVE WK-DY                TO FORM-DY                   28590040
270100           MOVE WK-MO                TO FORM-MO                   28600040
270200           MOVE WORK-HR-MN           TO FORM-HRMN                 28610040
270300           MOVE FORMAT-DATE-AREA     TO WS-RESTED                 28620040
270400        END-IF                                                    28630040
270500        IF DE-COMPARE3-DATE-TIME > CHECK-REST-TIME-CENT AND       28640040
270600           DE-COMPARE3-DATE-TIME > DE-COMPARE2-DATE-TIME AND      28650040
270700           DE-COMPARE3-DATE-TIME > DE-COMPARE1-DATE-TIME          28660040
270800           MOVE EMP-MTOD-DATE    TO WORK-DATE                     28670040
270900           MOVE EMP-MTOD-TIME    TO WORK-HR-MN                    28680040
271000           MOVE WK-YR            TO FORM-YR                       28690040
271100           MOVE WK-DY            TO FORM-DY                       28700040
271200           MOVE WK-MO            TO FORM-MO                       28710040
271300           MOVE WORK-HR-MN       TO FORM-HRMN                     28720040
271400           MOVE FORMAT-DATE-AREA TO WS-RESTED                     28730040
271500        END-IF                                                    28740040
271600        IF WS-RESTED NOT > SPACE                                  28750040
271700           MOVE ' ** RESTED **'  TO WS-RESTED                     28760040
271800        END-IF                                                    28770040
271900     END-IF                                                       28780040
272000                                                                  28790040
272100     SET SEARCH-NOT-DONE              TO TRUE                     28800040
272200     PERFORM VARYING J FROM 1 BY 1 UNTIL J > WS-CRAFT-TABLE-MAX   28810040
272300        OR SEARCH-DONE                                            28820040
272400        IF AJ-JOB-ASGN-CC = CT-CRAFT-CODE(J)                      28830040
272500           IF TEMP-EMP-FOUND                                      28840040
272600              SET SAVE-CREW-TEMP-EMP(J)   TO TRUE                 28850040
272700              MOVE EMP-NAME OF WS-MSTR    TO                      28860040
272800                                          SAVE-TEMP-CREW-NAME(J)  28870040
272900              IF DISPLAY-HOS                                      28880040
273000                 INITIALIZE PS94-COMMAREA-PARMS                   28890040
273100                 MOVE EMP-NBR  OF WS-MSTR    TO PS94-EMP-NBR      28900040
273200                 PERFORM P4000-GET-HOS                            28910040
273300                 MOVE WS-TOT-TM        TO SAVE-TEMP-CREW-TOTAL(J) 28920040
273400                 MOVE WS-LIMBO-TM      TO SAVE-TEMP-CREW-LIMBO(J) 28930040
273500                 MOVE WS-CONSEC-STARTS TO SAVE-TEMP-CREW-ST(J)    28940040
273600              ELSE                                                28950040
273700                 SET SAVE-TEMP-CREW-DONT-DISPLAY(J) TO TRUE       28960040
273800              END-IF                                              28970040
273900              MOVE WS-RESTED              TO                      28980040
274000                                          SAVE-TEMP-CREW-RESTED(J)28990040
274100              IF CT-CRAFT-CODE(J) = 'B1'                          29000040
274200                 MOVE 'REAR BRKM:'        TO                      29010040
274300                                          SAVE-TEMP-CREW-CRAFT(J) 29020040
274400              ELSE                                                29030040
274500                 IF CT-CRAFT-CODE(J) = 'B2'                       29040040
274600                    MOVE 'HEAD BRKM:'     TO                      29050040
274700                                          SAVE-TEMP-CREW-CRAFT(J) 29060040
274800                 ELSE                                             29070040
274900                    MOVE CT-CRAFT-DESC(J) TO                      29080040
275000                                          SAVE-TEMP-CREW-CRAFT(J) 29090040
275100                 END-IF                                           29100040
275200              END-IF                                              29110040
275300           ELSE                                                   29120040
275400              MOVE EMP-NAME OF WS-MSTR    TO SAVE-CREW-NAME(J)    29130040
275500              IF DISPLAY-HOS                                      29140040
275600                 INITIALIZE PS94-COMMAREA-PARMS                   29150040
275700                 MOVE EMP-NBR  OF WS-MSTR    TO PS94-EMP-NBR      29160040
275800                 PERFORM P4000-GET-HOS                            29170040
275900                 MOVE WS-TOT-TM          TO SAVE-CREW-HOS-TOTAL(J)29180040
276000                 MOVE WS-LIMBO-TM        TO SAVE-CREW-HOS-LIMBO(J)29190040
276100                 MOVE WS-CONSEC-STARTS   TO SAVE-CREW-HOS-ST(J)   29200040
276200              ELSE                                                29210040
276300                 SET SAVE-CREW-DONT-DISPLAY(J) TO TRUE            29220040
276400              END-IF                                              29230040
276500              IF TEMP-EMP-ONE > ZEROES                            29240040
276600                 IF NOT AVAILABLE                                 29250040
276700                    IF NOT WORKING                                29260040
276800                       MOVE ' *** OFF *** ' TO WS-RESTED          29270040
276900                    END-IF                                        29280040
277000                 END-IF                                           29290040
277100                 IF NORMAL-ASGNMT > SPACES AND                    29300040
277200                    NORMAL-ASGNMT NOT = AJJOBKEY-AREA             29310040
277300                       MOVE '<< ON TEMP >>' TO WS-RESTED          29320040
277400                 END-IF                                           29330040
277500                 IF TEMPORARY-ASGNMT > SPACES AND                 29340040
277600                    TEMPORARY-ASGNMT NOT = AJJOBKEY-AREA          29350040
277700                       MOVE '<< ON TEMP >>' TO WS-RESTED          29360040
277800                 END-IF                                           29370040
277900              ELSE                                                29380040
278000                 IF NORMAL-ASGNMT > SPACES AND                    29390040
278100                    NORMAL-ASGNMT NOT = AJJOBKEY-AREA             29400040
278200                       MOVE '<<TEMP VCNY>>' TO WS-RESTED          29410040
278300                 END-IF                                           29420040
278400                 IF TEMPORARY-ASGNMT > SPACES AND                 29430040
278500                    TEMPORARY-ASGNMT NOT = AJJOBKEY-AREA          29440040
278600                       MOVE '<<TEMP VCNY>>' TO WS-RESTED          29450040
278700                 END-IF                                           29460040
278800              END-IF                                              29470040
278900              MOVE WS-RESTED              TO SAVE-CREW-RESTED(J)  29480040
279000              MOVE CT-CRAFT-CODE(J)       TO SAVE-CREW-CRAFT(J)   29490040
279100*             IF CT-CRAFT-CODE(J) = 'B1'                          29500040
279200*                MOVE 'REAR BRKM:'        TO SAVE-CREW-CRAFT(J)   29510040
279300*             ELSE                                                29520040
279400*                IF CT-CRAFT-CODE(J) = 'B2'                       29530040
279500*                   MOVE 'HEAD BRKM:'     TO SAVE-CREW-CRAFT(J)   29540040
279600*                ELSE                                             29550040
279700*                   MOVE CT-CRAFT-DESC(J) TO SAVE-CREW-CRAFT(J)   29560040
279800*                END-IF                                           29570040
279900*             END-IF                                              29580040
280000           END-IF                                                 29590040
280100        END-IF                                                    29600040
280200     END-PERFORM.                                                 29610040
280300                                                                  29620047
280400*CNC0516-BEG                                                      29630047
280500 P2515-GET-CREW-STATUS.                                           29640047
280600                                                                  29650047
280700     INITIALIZE WS-RESTED1                                        29660047
280800     EVALUATE TRUE                                                29670047
280900          WHEN OFF-MILES-DAYS                                     29680047
281000               PERFORM P9830-RETRIEVE-CNTL-INFO                   29690047
281100               IF P956-ST-RSN-NBR-DAYS-REQ                        29700047
281200               OR P956-ST-RSN-EXP-DATE-REQ                        29710047
281300                  PERFORM P3265-GET-DUEBACK-DATE                  29720047
281400                  IF WS-DUEBACK-FOUND-Y                           29730047
281500                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE229740047
281600                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME229750047
281610*CNC0573 - BEG                                                    29760078
281620                     IF  PSTCA-FROM-FLD-MENU                      29770078
281630                     AND PSTCA-FLD-MENU-OPT NOT = '004'           29780078
281640                        IF P956-MASK-FLD-SCR-YES                  29790078
281641*CNC0576 - BEG                                                    29800082
281642                        OR P956-MASK-HOLD-TURN                    29810082
281643                           MOVE '**'            TO WS-LAYOFF-CODE229820082
281644                           IF P956-MASK-HOLD-TURN                 29830082
281645                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE229840082
281646                           ELSE                                   29850082
281647                              MOVE '**'      TO WS-LAYOFF-EM-CODE229860082
281648                           END-IF                                 29870082
281649*CNC0576 - END                                                    29880082
281660                        ELSE                                      29890078
281670                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE229900078
281680                           MOVE LAYOFF-EM-CODE  TO                29910078
281690                                WS-LAYOFF-EM-CODE1                29920078
281691                        END-IF                                    29930078
281692                     ELSE                                         29940078
281693                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE229950078
281694                        MOVE LAYOFF-EM-CODE     TO                29960078
281695                             WS-LAYOFF-EM-CODE1                   29970078
281696                     END-IF                                       29980078
281697*CNC0573 - END                                                    29990078
282000                     MOVE WS-RESTED1            TO WS-RESTED      30000048
282100                  ELSE                                            30010047
282200                     MOVE '<< VACANCY >>'       TO WS-RESTED      30020048
282300                  END-IF                                          30030047
282400               ELSE                                               30040047
282500                  PERFORM P3266-OFF-MILES-RETURN-DATE             30050047
282600                  MOVE WS-RETURN-DATE-1(1:4)    TO WS-RETURN-DATE230060053
282700                  MOVE SPACES                   TO WS-RETURN-TIME230070048
282710*CNC0573 - BEG                                                    30080078
282720                  IF  PSTCA-FROM-FLD-MENU                         30090078
282730                  AND PSTCA-FLD-MENU-OPT NOT = '004'              30100078
282740                     IF P956-MASK-FLD-SCR-YES                     30110078
282741*CNC0576 - BEG                                                    30120082
282742                     OR P956-MASK-HOLD-TURN                       30130082
282743                        MOVE '**'               TO WS-LAYOFF-CODE230140082
282744                        IF P956-MASK-HOLD-TURN                    30150082
282745                           MOVE 'HT'         TO WS-LAYOFF-EM-CODE230160082
282746                        ELSE                                      30170082
282747                           MOVE '**'         TO WS-LAYOFF-EM-CODE230180082
282748                        END-IF                                    30190082
282749*CNC0576 - END                                                    30200082
282760                     ELSE                                         30210078
282770                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE230220078
282780                        MOVE LAYOFF-EM-CODE     TO                30230078
282790                             WS-LAYOFF-EM-CODE2                   30240078
282791                     END-IF                                       30250078
282792                  ELSE                                            30260078
282793                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE230270078
282794                     MOVE LAYOFF-EM-CODE        TO                30280078
282795                         WS-LAYOFF-EM-CODE2                       30290078
282796                  END-IF                                          30300078
282797*CNC0573 - END                                                    30310078
283100                  MOVE WS-RESTED1               TO WS-RESTED      30320048
283200               END-IF                                             30330047
283300          WHEN VACATION                                           30340047
283400               PERFORM P3267-VACATION-RETURN-DATE                 30350047
283500               MOVE WS-RETURN-DATE-1(1:4)       TO WS-RETURN-DATE230360053
283600               MOVE WS-RETURN-DATE-1(5:4)       TO WS-RETURN-TIME230370053
283610*CNC0573 - BEG                                                    30380078
283620               IF  PSTCA-FROM-FLD-MENU                            30390078
283630               AND PSTCA-FLD-MENU-OPT NOT = '004'                 30400078
283640                  PERFORM P9830-RETRIEVE-CNTL-INFO                30410078
283650                  IF P956-MASK-FLD-SCR-YES                        30420078
283651*CNC0576 - BEG                                                    30430082
283652                  OR P956-MASK-HOLD-TURN                          30440082
283653                     MOVE '**'                  TO WS-LAYOFF-CODE230450082
283654                     IF P956-MASK-HOLD-TURN                       30460082
283655                        MOVE 'HT'            TO WS-LAYOFF-EM-CODE230470082
283656                     ELSE                                         30480082
283657                        MOVE '**'            TO WS-LAYOFF-EM-CODE230490082
283658                     END-IF                                       30500082
283659*CNC0576 - END                                                    30510082
283670                  ELSE                                            30520078
283680                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE230530078
283690                     MOVE LAYOFF-EM-CODE        TO                30540078
283691                          WS-LAYOFF-EM-CODE2                      30550078
283692                  END-IF                                          30560078
283693               ELSE                                               30570078
283694                  MOVE LAYOFF-CODE              TO WS-LAYOFF-CODE230580078
283695                  MOVE LAYOFF-EM-CODE           TO                30590078
283696                       WS-LAYOFF-EM-CODE2                         30600078
283697               END-IF                                             30610078
283698*CNC0573 - END                                                    30620078
284000               MOVE WS-RESTED1                  TO WS-RESTED      30630048
284100          WHEN EXCUSED-ABSENCE                                    30640047
284200           AND LAYOFF-EM-CODE = '69'                              30650047
284300               PERFORM P3265-GET-DUEBACK-DATE                     30660047
284400               IF WS-DUEBACK-FOUND-Y                              30670047
284500                  MOVE TASK-LO-EXP-DATE(3:4)    TO WS-RETURN-DATE230680048
284600                  MOVE TASK-LO-EXP-TIME         TO WS-RETURN-TIME230690048
284610*CNC0573 - BEG                                                    30700078
284620                  IF  PSTCA-FROM-FLD-MENU                         30710078
284630                  AND PSTCA-FLD-MENU-OPT NOT = '004'              30720078
284640                     PERFORM P9830-RETRIEVE-CNTL-INFO             30730078
284650                     IF P956-MASK-FLD-SCR-YES                     30740078
284651*CNC0576 - BEG                                                    30750082
284652                     OR P956-MASK-HOLD-TURN                       30760082
284653                        MOVE '**'               TO WS-LAYOFF-CODE230770082
284654                        IF P956-MASK-HOLD-TURN                    30780082
284655                           MOVE 'HT'         TO WS-LAYOFF-EM-CODE230790082
284656                        ELSE                                      30800082
284657                           MOVE '**'         TO WS-LAYOFF-EM-CODE230810082
284658                        END-IF                                    30820082
284659*CNC0576 - END                                                    30830082
284670                     ELSE                                         30840078
284680                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE230850078
284690                        MOVE LAYOFF-EM-CODE     TO                30860078
284691                             WS-LAYOFF-EM-CODE2                   30870078
284692                     END-IF                                       30880078
284693                  ELSE                                            30890078
284694                     MOVE LAYOFF-CODE           TO WS-LAYOFF-CODE230900078
284695                     MOVE LAYOFF-EM-CODE        TO                30910078
284696                          WS-LAYOFF-EM-CODE2                      30920078
284697                  END-IF                                          30930078
284698*CNC0573 - END                                                    30940078
285000                  MOVE WS-RESTED1               TO WS-RESTED      30950048
285100               ELSE                                               30960047
285200                  MOVE '<< VACANCY >>'          TO WS-RESTED      30970048
285300               END-IF                                             30980047
285400          WHEN OTHER                                              30990047
285500               IF NOT AVAILABLE  AND                              31000047
285600                  NOT WORKING    AND                              31010047
285700                  NOT TO-PLACE                                    31020047
285800                  AND LAYOFF-TIME NUMERIC                         31030047
285900                  AND LAYOFF-TIME > ZERO                          31040047
286000                  PERFORM P3265-GET-DUEBACK-DATE                  31050047
286100                  IF WS-DUEBACK-FOUND-Y                           31060047
286200                    PERFORM P3268-CHECK-FOR-E95-DTTM              31070058
286300                    IF WS-E95 > SPACES                            31080057
286310*CNC0573 - BEG                                                    31090078
286320                       IF  PSTCA-FROM-FLD-MENU                    31100078
286330                       AND PSTCA-FLD-MENU-OPT NOT = '004'         31110078
286340                          PERFORM P9830-RETRIEVE-CNTL-INFO        31120079
286350                          IF P956-MASK-FLD-SCR-YES                31130078
286351*CNC0576 - BEG                                                    31140082
286352                          OR P956-MASK-HOLD-TURN                  31150082
286353                             MOVE '**'          TO WS-E95-CODE    31160082
286354                             IF P956-MASK-HOLD-TURN               31170082
286355                                MOVE 'HT'       TO WS-E95-EM-CODE 31180082
286356                             ELSE                                 31190082
286357                                MOVE '**'       TO WS-E95-EM-CODE 31200082
286358                             END-IF                               31210082
286370                          END-IF                                  31220083
286393                       END-IF                                     31230078
286394                       MOVE WS-E95              TO WS-RESTED      31240083
286395*CNC0576 - END                                                    31250083
286396*CNC0573 - END                                                    31260078
286500                    ELSE                                          31270057
286600                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE231280058
286700                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME231290058
286710*CNC0573 - BEG                                                    31300078
286720                     IF  PSTCA-FROM-FLD-MENU                      31310078
286730                     AND PSTCA-FLD-MENU-OPT NOT = '004'           31320078
286740                        PERFORM P9830-RETRIEVE-CNTL-INFO          31330078
286750                        IF P956-MASK-FLD-SCR-YES                  31340078
286751*CNC0576 - BEG                                                    31350082
286752                        OR P956-MASK-HOLD-TURN                    31360082
286753                           MOVE '**'            TO WS-LAYOFF-CODE231370082
286754                           IF P956-MASK-HOLD-TURN                 31380082
286755                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE231390082
286756                           ELSE                                   31400082
286757                              MOVE '**'      TO WS-LAYOFF-EM-CODE231410082
286758                           END-IF                                 31420082
286759*CNC0576 - END                                                    31430082
286770                        ELSE                                      31440078
286780                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE231450078
286790                           MOVE LAYOFF-EM-CODE  TO                31460078
286791                                WS-LAYOFF-EM-CODE2                31470078
286792                        END-IF                                    31480078
286793                     ELSE                                         31490078
286794                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE231500078
286795                        MOVE LAYOFF-EM-CODE     TO                31510078
286796                             WS-LAYOFF-EM-CODE2                   31520078
286797                     END-IF                                       31530078
286798*CNC0573 - END                                                    31540078
287100                     MOVE WS-RESTED1            TO WS-RESTED      31550058
287200                    END-IF                                        31560057
287300                  ELSE                                            31570047
287400                    MOVE '<< VACANCY >>'        TO WS-RESTED      31580048
287500                  END-IF                                          31590047
287600               END-IF                                             31600047
287700     END-EVALUATE.                                                31610047
287800*CNC0516-END                                                      31620048
288000 P2600-WRITE-LOCALS-IN-TOWN.                                      31630040
288100                                                                  31640040
288200     PERFORM VARYING J FROM 1 BY 1                                31650040
288300         UNTIL J > 30                                             31660040
288400            MOVE SPACES                   TO LOCALS-IN-TOWN-2     31670040
288500            IF SAVE-CREW-CRAFT(J) > SPACES                        31680040
288600               MOVE SAVE-CREW-CRAFT(J)    TO LOCALS-IT-CRAFT      31690040
288700*              MOVE SAVE-CREW-CRAFT(J)    TO FOREMAN-OR-SWITCHMAN 31700040
288800*              IF HE-IS-FOREMAN                                   31710040
288900*                 MOVE 'CONDUCTOR'        TO LOCALS-IT-CRAFT      31720040
289000*              END-IF                                             31730040
289100*              IF HE-IS-SWITCHMAN                                 31740040
289200*                 MOVE 'BRAKEMAN'         TO LOCALS-IT-CRAFT      31750040
289300*              END-IF                                             31760040
289400               MOVE SAVE-CREW-NAME(J)     TO LOCALS-IT-NAME       31770040
289500               IF WS-CANADIAN-COMPANY OR SAVE-CREW-DONT-DISPLAY(J)31780040
289600                  MOVE SPACES             TO LOCALS-IT-HOS-AREA   31790040
289700               ELSE                                               31800040
289800                  STRING 'TOT '                                   31810040
289900                    SAVE-CREW-HOS-TOTAL-HR(J) ':'                 31820040
290000                    SAVE-CREW-HOS-TOTAL-MM(J)                     31830040
290100                      ' LIM '                                     31840040
290200                    SAVE-CREW-HOS-LIMBO-HR(J) ':'                 31850040
290300                    SAVE-CREW-HOS-LIMBO-MM(J)                     31860040
290400                      ' ST:'                                      31870040
290500                    SAVE-CREW-HOS-ST(J)                           31880040
290600                    DELIMITED BY SIZE INTO LOCALS-IT-HOS-AREA     31890040
290700               END-IF                                             31900040
290800               IF LOCALS-IT-NAME  = '<< OPEN TURN >> '            31910040
290900                  MOVE SPACES             TO LOCALS-IT-HOS-AREA   31920040
291000               END-IF                                             31930040
291100               MOVE SAVE-CREW-RESTED(J)   TO LOCALS-IT-RESTED     31940040
291200               IF SCR-SUB > SCR-MAX                               31950040
291300                  PERFORM P2300-POOL-TITLE                        31960040
291400               END-IF                                             31970040
291500               MOVE SPACES           TO PAGE-LINE(SCR-SUB)        31980040
291600               MOVE LOCALS-IN-TOWN-2     TO PAGE-LINE(SCR-SUB)    31990040
291700               ADD    1                   TO SCR-SUB              32000040
291800               IF SAVE-CREW-TEMP-EMP(J)                           32010040
291900                  MOVE ' TEMP '           TO LOCALS-IT-FILLER     32020040
292000                  MOVE SAVE-TEMP-CREW-NAME(J)                     32030040
292100                                          TO LOCALS-IT-NAME       32040040
292200                  IF WS-CANADIAN-COMPANY OR                       32050040
292300                     SAVE-TEMP-CREW-DONT-DISPLAY(J)               32060040
292400                     MOVE SPACES          TO LOCALS-IT-HOS-AREA   32070040
292500                  ELSE                                            32080040
292600                     STRING 'TOT '                                32090040
292700                       SAVE-TEMP-CREW-TOTAL-HR(J) ':'             32100040
292800                       SAVE-TEMP-CREW-TOTAL-MM(J)                 32110040
292900                         ' LIM '                                  32120040
293000                       SAVE-TEMP-CREW-LIMBO-HR(J) ':'             32130040
293100                       SAVE-TEMP-CREW-LIMBO-MM(J)                 32140040
293200                         ' ST:'                                   32150040
293300                       SAVE-TEMP-CREW-ST(J)                       32160040
293400                       DELIMITED BY SIZE INTO LOCALS-IT-HOS-AREA  32170040
293500                  END-IF                                          32180040
293600                  MOVE SAVE-TEMP-CREW-RESTED(J)                   32190040
293700                                          TO LOCALS-IT-RESTED     32200040
293800                  IF SCR-SUB > SCR-MAX                            32210040
293900                     PERFORM P2300-POOL-TITLE                     32220040
294000                  END-IF                                          32230040
294100                  MOVE SPACES           TO PAGE-LINE(SCR-SUB)     32240040
294200                  MOVE LOCALS-IN-TOWN-2   TO PAGE-LINE(SCR-SUB)   32250040
294300                  ADD    1                TO SCR-SUB              32260040
294400               END-IF                                             32270040
294500            END-IF                                                32280040
294600     END-PERFORM                                                  32290040
294700                                                                  32300040
294800     MOVE SPACES              TO SAVE-CREW-INFORMATION            32310040
294900                                 SAVE-CREW-INFORMATION-HOS        32320040
295000                                 SAVE-TEMP-CREW-INFORMATION-HOS   32330040
295100                                 SAVE-TEMP-CREW-INFORMATION.      32340040
295200 P3000-SPAREBOARDS.                                               32350040
295300                                                                  32360040
295400     MOVE 'P3000'            TO ERR-PARAGRAPH                     32370040
295500     ADD 1                   TO SCR-SUB                           32380040
295600     IF SCR-SUB > SCR-MAX                                         32390040
295700       PERFORM P3800-XB-TITLE                                     32400040
295800     ELSE                                                         32410040
295900       MOVE SPACES           TO PAGE-LINE(SCR-SUB)                32420040
296000       MOVE EXTRABOARDS-TITLE TO PAGE-LINE(SCR-SUB)               32430040
296100       ADD 1                 TO SCR-SUB                           32440040
296200     END-IF                                                       32450040
296300     SET NOT-DONE            TO TRUE                              32460040
296400     PERFORM VARYING EXT-SUB FROM 1 BY 1 UNTIL DONE               32470040
296500                             OR EXT-SUB > RPT-MAX                 32480040
296600        IF SB-P(EXT-SUB) = SPACES                                 32490040
296700           SET DONE             TO TRUE                           32500040
296800        ELSE                                                      32510040
296900           MOVE SPACES              TO WS-CNTL-FILE               32520040
297000           MOVE P27NCA-DIST         TO CNTL-DIST                  32530040
297100           MOVE P27NCA-SUB-DIST     TO CNTL-SUB-DIST              32540040
297200           SET  EXTRABOARD-TYPE-REC TO TRUE                       32550040
297300           MOVE SB-P(EXT-SUB)       TO CNTL-XB                    32560040
297400           MOVE CNTLKEY-AREA        TO WORK-CNTLKEY               32570040
297500                                       CNTLKEY                    32580040
297600           PERFORM P8000-READ-CNTLFILE                            32590040
297700           IF SUCCESS                                             32600040
297800              MOVE WS-CNTL-FILE  TO WS-HOLD-CNTL-FILE             32610040
297900              MOVE SPACES        TO WORK-XB-POS-KEY               32620040
298000              MOVE CNTL-DIST     TO XB-POS-DIST XB-TURN-DIST      32630040
298100                                    ASGN-DIST                     32640040
298200              MOVE CNTL-SUB-DIST TO XB-POS-SUB-DIST               32650040
298300                                    XB-TURN-SUB-DIST              32660040
298400                                    ASGN-SUB-DIST                 32670040
298500              MOVE CNTL-XB       TO XB-POS-CC XB-TURN-CC          32680040
298600                                    WS-CNTL-XB                    32690040
298700              MOVE CNTL-XB-DESC  TO EXTRABOARD-DESC               32700040
298800              IF CNTL-XB-EXTENDED-SCHED                           32710040
298900                 SET WS-XB-EXTENDED-SCHED  TO TRUE                32720040
299000              ELSE                                                32730040
299100                 IF CNTL-XB-SCHEDULED                             32740040
299200                    SET WS-XB-SCHEDULED    TO TRUE                32750040
299300                 ELSE                                             32760040
299400                    MOVE SPACES            TO WS-XB-SCHEDULED-FLAG32770040
299500                 END-IF                                           32780040
299600              END-IF                                              32790040
299700              IF EXT-SUB          > 1                             32800040
299800                 IF SCR-SUB NOT > SCR-MAX                         32810040
299900                    MOVE SPACES  TO PAGE-LINE(SCR-SUB)            32820040
300000                 END-IF                                           32830040
300100                 ADD  1          TO SCR-SUB                       32840040
300200              END-IF                                              32850040
300300              IF SCR-SUB > SCR-MAX                                32860040
300400                 PERFORM P3800-XB-TITLE                           32870040
300500              END-IF                                              32880040
300600              MOVE SPACES        TO PAGE-LINE(SCR-SUB)            32890040
300700              MOVE EXTRABOARDS-1 TO PAGE-LINE(SCR-SUB)            32900040
300800              ADD 1              TO SCR-SUB                       32910040
300900              MOVE SPACES        TO P942-COMMAREA-PARMS           32920040
301000              SET P942-ASGN-SEN-FUNCTION                          32930040
301100                                 TO TRUE                          32940040
301200              SET P942-ASGN-XB   TO TRUE                          32950040
301300              MOVE CNTL-DIST     TO P942-ASGN-DIST                32960040
301400              MOVE CNTL-SUB-DIST TO P942-ASGN-SUB-DIST            32970040
301500              MOVE '******'      TO P942-ASGN-ASGN                32980040
301600              MOVE CNTL-XB-ROSTER-CC    TO P942-ASGN-CC           32990040
301700*                                                                 33000040
301800              EXEC CICS LINK                                      33010040
301900                        PROGRAM(P942-PGM)                         33020040
302000                        COMMAREA(P942-COMMAREA-PARMS)             33030040
302100                        LENGTH(P942-LGTH)                         33040040
302200                        RESP(WS-RESPONSE)                         33050040
302300              END-EXEC                                            33060040
302400              MOVE WS-RESPONSE     TO FILE-STATUS                 33070040
302500              IF NOT SUCCESS                                      33080040
302600                 MOVE 'P3000-1'    TO ERR-PARAGRAPH               33090040
302700                 MOVE 'P942LINK'   TO ERR-KEY                     33100040
302800                 PERFORM P9999-GOT-PROBLEM                        33110040
302900              END-IF                                              33120040
303000*                                                                 33130040
303100              PERFORM VARYING X2 FROM 1 BY 1                      33140040
303200                UNTIL X2 > SA-ARRAY-MAX                           33150040
303300                  IF P942-ASGN-SEN-CC(X2) = CNTL-XB-ROSTER-CC     33160040
303400                     MOVE P942-ASGN-SEN-RSTR(X2)                  33170040
303500                                     TO WK-SEN-ROSTER             33180040
303600                     MOVE P942-ASGN-SEN-CC(X2)                    33190040
303700                                     TO WK-SEN-CC                 33200040
303800                     MOVE SA-ARRAY-MAX                            33210040
303900                                     TO X2                        33220040
304000                  END-IF                                          33230040
304100              END-PERFORM                                         33240040
304200              MOVE ZEROES            TO WS-POS-CNT                33250040
304300              MOVE ZERO              TO XB-POS                    33260040
304400              IF DUAL-XB                                          33270040
304500                 MOVE 1              TO XB-POS-BOARD              33280040
304600                 PERFORM P3100-GET-XB                             33290040
304700                 MOVE ZEROES         TO SCR-SUB                   33300040
304800                 MOVE ZEROES         TO WS-POS-CNT                33310040
304900                 MOVE ZEROES         TO XB-POS                    33320040
305000                 MOVE 2              TO XB-POS-BOARD              33330040
305100              END-IF                                              33340040
305200              PERFORM P3100-GET-XB                                33350040
305300           ELSE                                                   33360040
305400              IF NOT (END-OF-FILE OR NO-RECORD-FND)               33370040
305500                 MOVE CNTLKEY               TO ERR-KEY            33380040
305600                 MOVE 'READ FAIL ON CNTLFILE' TO ERR-SENTENCE     33390086
297400                 MOVE CNTLKEY-AREA        TO ERR-KEY              33391086
305700                 PERFORM P9999-GOT-PROBLEM                        33400040
305800              END-IF                                              33410040
305900           END-IF                                                 33420040
306000        END-IF                                                    33430040
306100     END-PERFORM.                                                 33440040
306200                                                                  33450040
306300 P3100-GET-XB.                                                    33460040
306400                                                                  33470040
306500     MOVE WS-HOLD-CNTL-FILE    TO WS-CNTL-FILE                    33480040
306600                                                                  33490040
306700     SET XB-NOT-DONE TO TRUE                                      33500040
306800     PERFORM P3900-START-XB-POS                                   33510040
306900     IF SUCCESS                                                   33520040
307000       PERFORM UNTIL XB-DONE                                      33530040
307100         PERFORM P3920-READ-NEXT-XB-POS                           33540040
307200         IF SUCCESS                                               33550040
307300           SET DE-YYMMDD-FORMAT TO TRUE                           33560040
307400           MOVE EB-POS-DATE-TIME(1:6) TO DE-YYMMDD                33570040
307500           PERFORM P8998-DATEEDIT                                 33580040
307600           MOVE DE-CCYYMMDD     TO DE-COMPARE1-DATE               33590040
307700           MOVE EB-POS-DATE-TIME(7:4) TO DE-COMPARE1-TIME         33600040
307800           IF XB-POS-DIST = DIST OF WS-EXTRA-BOARD                33610040
307900             AND XB-POS-SUB-DIST = SUB-DIST OF WS-EXTRA-BOARD     33620040
308000             AND XB-POS-CC = CRAFT-CODE OF WS-EXTRA-BOARD         33630040
308100             AND EB-ON-BOARD                                      33640040
308200             AND XB-POS-BOARD = EB-POS-BOARD OF WS-EXTRA-BOARD    33650040
308300             AND DE-COMPARE1-DATE-TIME NOT >                      33660040
308400                                  WS-PRESENT-TIME-CENT            33670040
308500               PERFORM P3200-GET-XB-DETAIL                        33680040
308600           ELSE                                                   33690040
308700               SET XB-DONE TO TRUE                                33700040
308800           END-IF                                                 33710040
308900         ELSE                                                     33720040
309000           SET XB-DONE TO TRUE                                    33730040
309100         END-IF                                                   33740040
309200       END-PERFORM                                                33750040
309300     END-IF                                                       33760040
309400     PERFORM P3930-END-XB-POS.                                    33770040
309500                                                                  33780040
309600 P3200-GET-XB-DETAIL.                                             33790040
309700                                                                  33800040
309800     MOVE SPACES                     TO EXTRABOARDS-2             33810040
309900     IF EB-ON-BOARD                                               33820040
310000       ADD 1                         TO WS-POS-CNT                33830040
310100       MOVE WS-POS-CNT               TO EXTRABOARD-POSITION       33840040
310200     ELSE                                                         33850040
310300       MOVE '999'                    TO EXTRABOARD-POSITION       33860040
310400     END-IF                                                       33870040
310500     MOVE ZERO                       TO ASGN-EMP-NO               33880040
310600                                        GOT-EMPLOYEE-FLAG         33890040
310700     MOVE SPACES                     TO WS-MSTR                   33900040
310800     MOVE TURN-NBR OF WS-EXTRA-BOARD TO ASGN-XB-TURN              33910040
310900     MOVE 'EX'                       TO ASGN-XB-PREFIX            33920040
311000     MOVE 'X'                        TO ASGN-JOB-TYPE             33930040
311100     MOVE WS-CNTL-XB                 TO ASGN-XB-CC                33940040
311200     SET ASGN-TEMP-REC               TO TRUE                      33950040
311300     MOVE ZEROES                     TO ASGN-DATE-TIME            33960040
311400     MOVE P27NCA-DIST                TO ASGN-DIST                 33970040
311500     MOVE P27NCA-SUB-DIST            TO ASGN-SUB-DIST             33980040
311600     PERFORM PXXXX-LATEST-TEMP                                    33990040
311700     IF ASGN-EMP-NO               NOT > ZERO                      34000040
311800        MOVE TURN-NBR OF WS-EXTRA-BOARD                           34010040
311900                                     TO ASGN-XB-TURN              34020040
312000        MOVE 'EX'                    TO ASGN-XB-PREFIX            34030040
312100        MOVE 'X'                     TO ASGN-JOB-TYPE             34040040
312200        MOVE WS-CNTL-XB              TO ASGN-XB-CC                34050040
312300        SET ASGN-OWNER-REC           TO TRUE                      34060040
312400        MOVE ZEROES                  TO ASGN-DATE-TIME            34070040
312500        MOVE P27NCA-DIST             TO ASGN-DIST                 34080040
312600        MOVE P27NCA-SUB-DIST         TO ASGN-SUB-DIST             34090040
312700        PERFORM PXXXX-JOB-OWNER                                   34100040
312800     END-IF                                                       34110040
312900     IF ASGN-EMP-NO               NOT > ZERO                      34120040
313000        PERFORM P3300-SEARCH-SWASSGN                              34130040
313100     END-IF                                                       34140040
313200     IF ASGN-EMP-NO                   > ZERO                      34150040
313300        MOVE ASGN-EMP-NO             TO MSTRNBRK                  34160040
313400                                        EMP-NBR-KEY               34170040
313500        PERFORM P8500-READ-MASTER                                 34180040
313600        SET GOT-EMPLOYEE             TO TRUE                      34190040
313700*                                                                 34200040
313800*       IF EMPLOYEE IS UNAVAILABLE, ONLY DISPLAY THEM IF THEIR    34210040
313900*       STATUS CODE'S 'DISPLAY WITHOUT TRACKING' FLAG IS SET.     34220040
314000*                                                                 34230040
314100        IF AVAILABLE                                              34240040
314200           SET DISPLAY-EMP                 TO TRUE                34250040
314300        ELSE                                                      34260040
314400           SET DONT-DISPLAY-EMP            TO TRUE                34270040
314500           MOVE SPACES                     TO P956-COMMAREA-PARMS 34280040
314600           MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE    34290040
314700           SET P956-GET-CNTL-STATUS-REASON TO TRUE                34300040
314800           MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE    34310040
314900           MOVE DIST  OF WS-MSTR           TO P956-DIST           34320040
315000           MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST          34330040
315100           MOVE CRAFT OF WS-MSTR           TO P956-CC             34340040
315200           IF TEMPORARY-ASGNMT              > SPACE               34350040
315300              MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE      34360040
315400              MOVE TA-1                    TO P956-ASGN           34370040
315500              MOVE TA-DIST                 TO P956-DIST           34380040
315600              MOVE TA-SUB-DIST             TO P956-SDIST          34390040
315700              IF TEMP-ASGN-XB                                     34400040
315800                 MOVE TA-CC                TO P956-XB             34410040
315900              END-IF                                              34420040
316000           ELSE                                                   34430040
316100              IF NORMAL-ASGNMT              > SPACES              34440040
316200                 MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE      34450040
316300                 MOVE NA-1                 TO P956-ASGN           34460040
316400                 MOVE NA-DIST              TO P956-DIST           34470040
316500                 MOVE NA-SUB-DIST          TO P956-SDIST          34480040
316600                 IF NORM-ASGN-XB                                  34490040
316700                    MOVE NA-CC             TO P956-XB             34500040
316800                 END-IF                                           34510040
316900              END-IF                                              34520040
317000           END-IF                                                 34530040
317100           EXEC CICS LINK                                         34540040
317200                     PROGRAM (P956-PGM)                           34550040
317300                     COMMAREA(P956-COMMAREA-PARMS)                34560040
317400                     LENGTH  (P956-LGTH)                          34570040
317500                     RESP    (WS-RESPONSE)                        34580040
317600           END-EXEC                                               34590040
317700           MOVE WS-RESPONSE                TO FILE-STATUS         34600040
317800           IF NOT SUCCESS                                         34610040
317900              MOVE 'P3000-A'               TO ERR-PARAGRAPH       34620040
318000              MOVE P956-INPUT-PARMS        TO ERR-KEY             34630040
318100              PERFORM P9999-GOT-PROBLEM                           34640040
318200           END-IF                                                 34650040
318300           IF P956-ST-RSN-DISP-WO-TRACKING                        34660040
318400              SET DISPLAY-EMP              TO TRUE                34670040
318500           END-IF                                                 34680040
318600        END-IF                                                    34690040
318700     END-IF                                                       34700040
318800     IF GOT-EMPLOYEE                                              34710040
318900        IF DISPLAY-EMP                                            34720040
319000           IF TEMPORARY-ASGNMT              > SPACE               34730040
319100               AND TEMP-ASGN-XB-AUG                               34740040
319200               AND TA-DIST     = DIST-REPEAT                      34750040
319300               AND TA-SUB-DIST = SUBDIST-REPEAT                   34760040
319400               AND TA-XB-TURN  = TURN-NBR OF WS-EXTRA-BOARD       34770040
319500               AND TA-CC       = CRAFT-CODE-REPEAT                34780040
319600               MOVE EMP-NAME OF WS-MSTR    TO WS-FORMAT-NAME-AUG  34790040
319700               MOVE ' /AUG'            TO WS-FORMAT-NAME-AUG-FIELD34800040
319800               MOVE WS-FORMAT-NAME-AUG TO EXTRABOARD-NAME         34810040
319900           ELSE                                                   34820040
320000               MOVE EMP-NAME OF WS-MSTR    TO EXTRABOARD-NAME     34830040
320100               IF WS-CANADIAN-COMPANY                             34840040
320200                  MOVE SPACES          TO EXTRABOARD-HOS-AREA     34850040
320300               ELSE                                               34860040
320400                  INITIALIZE PS94-COMMAREA-PARMS                  34870040
320500                  MOVE EMP-NBR  OF WS-MSTR    TO PS94-EMP-NBR     34880040
320600                  PERFORM P4000-GET-HOS                           34890040
320700                  STRING 'TOT '                                   34900040
320800                    WS-TOT-TM-HH ':'                              34910040
320900                    WS-TOT-TM-MM                                  34920040
321000                      ' LIM '                                     34930040
321100                    WS-LIMBO-TM-HH ':'                            34940040
321200                    WS-LIMBO-TM-MM                                34950040
321300                      ' ST:'                                      34960040
321400                    WS-CONSEC-STARTS                              34970040
321500                    DELIMITED BY SIZE INTO EXTRABOARD-HOS-AREA    34980040
321600               END-IF                                             34990040
321700           END-IF                                                 35000040
321800        END-IF                                                    35010040
321900     ELSE                                                         35020040
322000        MOVE '<<  OPEN TURN  >>'           TO EXTRABOARD-NAME     35030040
322100        MOVE SPACES                        TO EXTRABOARD-HOS-AREA 35040040
322200     END-IF                                                       35050040
322300* IF XB TEMP-ASGN AND EMP IS ON THE BOARD WE ARE PROCESSING,      35060040
322400* HE COULD STILL BE CONSIDERED AVAILABLE.                         35070040
322500     SET TEMP-DIFF-BOARD TO TRUE                                  35080040
322600     IF TEMPORARY-ASGNMT > SPACES                                 35090040
322700        IF TEMP-ASGN-XB                                           35100040
322800           IF TA-DIST     = DIST       OF WS-EXTRA-BOARD AND      35110040
322900              TA-SUB-DIST = SUB-DIST   OF WS-EXTRA-BOARD AND      35120040
323000              TA-CC       = CRAFT-CODE OF WS-EXTRA-BOARD          35130040
323100              SET TEMP-SAME-BOARD TO TRUE                         35140040
323200           END-IF                                                 35150040
323300       END-IF                                                     35160040
323400     END-IF                                                       35170040
323500                                                                  35180040
323600     IF GOT-EMPLOYEE                                              35190040
323700        AND DISPLAY-EMP                                           35200040
323800        IF (TEMPORARY-ASGNMT   > SPACES  AND                      35210040
323900            TEMP-DIFF-BOARD)                                      35220040
324000           OR NOT (AVAILABLE)                                     35230040
324100           OR (AVAILABLE AND NOT-NOTIFIED)                        35240040
324200           OR OUT-TOWN                                            35250040
324300*CNC0516-BEG                                                      35260044
324400*          MOVE '<UNAVAILABLE>'            TO EXTRABOARD-RESTED   35270041
324500           PERFORM P3260-GET-STATUS                               35280044
324600*CNC0516-END                                                      35290044
324700        END-IF                                                    35300044
324800     END-IF                                                       35310040
324900*                                                                 35320040
325000*    IF THIS IS A SCHEDULED EXTRABOARD (EXTENDED OR REGULAR), SEE 35330040
325100*    IF THE TURN IS ON A REST DAY.                                35340040
325200*                                                                 35350040
325300     IF EXTRABOARD-RESTED               NOT > SPACES              35360040
325400        AND WS-SCHEDULED-XB                                       35370040
325500        SET NOT-ON-REST-DAY                TO TRUE                35380040
325600        MOVE SPACES                        TO WS-SCHEDULED-TURN-SW35390040
325700        IF WS-XB-SCHEDULED                                        35400040
325800           PERFORM P3210-CHECK-REST-DAY                           35410040
325900        ELSE                                                      35420040
326000           PERFORM P3250-CHECK-EXTENDED-REST-DAY                  35430040
326100        END-IF                                                    35440040
326200        IF ON-REST-DAY                                            35450040
326300           MOVE '<UNAVAILABLE>'            TO EXTRABOARD-RESTED   35460040
326400        END-IF                                                    35470040
326500     END-IF                                                       35480040
326600     IF GOT-EMPLOYEE                                              35490040
326700        AND DISPLAY-EMP                                           35500040
326800        IF EMP-MTOD IS NUMERIC                                    35510040
326900           SET DE-YYMMDD-FORMAT         TO TRUE                   35520040
327000           MOVE EMP-MTOD-DATE           TO DE-YYMMDD              35530040
327100           PERFORM P8998-DATEEDIT                                 35540040
327200           MOVE DE-CCYYMMDD             TO DE-COMPARE1-DATE       35550040
327300           MOVE EMP-MTOD-TIME           TO DE-COMPARE1-TIME       35560040
327400        END-IF                                                    35570040
327500        IF EMP-US-RSTD IS NUMERIC                                 35580040
327600           SET DE-YYMMDD-FORMAT         TO TRUE                   35590040
327700           MOVE EMP-US-RSTD-DATE        TO DE-YYMMDD              35600040
327800           PERFORM P8998-DATEEDIT                                 35610040
327900           MOVE DE-CCYYMMDD             TO DE-COMPARE2-DATE       35620040
328000           MOVE EMP-US-RSTD-TIME        TO DE-COMPARE2-TIME       35630040
328100        END-IF                                                    35640040
328200        IF EMP-PERS-REST IS NUMERIC                               35650040
328300           AND EMP-PERS-REST-NUM         > ZERO                   35660040
328400           PERFORM P5200-CHECK-COMPANY-CD                         35670040
328500           IF APPLY-LEAD-TIME                                     35680040
328600              MOVE ZEROS                TO DATE-CONVERSION-PARMS  35690040
328700              SET PARM-ADD              TO TRUE                   35700040
328800              MOVE EMP-PERS-REST-DATE   TO PARM-PRI-DATE-GREG     35710040
328900              MOVE EMP-PERS-REST-TIME   TO PARM-PRI-HRMN          35720040
329000              MOVE '0200'               TO PARM-SEC-HRMN          35730040
329100              PERFORM P9991-CALL-P903-PROGRAM                     35740040
329200              MOVE PARM-RES-DATE-GREG   TO DE-COMPARE3-YYMMDD     35750040
329300              MOVE PARM-RES-GREG-CENT   TO DE-COMPARE3-CE         35760040
329400              MOVE PARM-RES-HRMN        TO DE-COMPARE3-TIME       35770040
329500           ELSE                                                   35780040
329600              SET DE-YYMMDD-FORMAT      TO TRUE                   35790040
329700              MOVE EMP-PERS-REST-DATE   TO DE-YYMMDD              35800040
329800              PERFORM P8998-DATEEDIT                              35810040
329900              MOVE DE-CCYYMMDD          TO DE-COMPARE3-DATE       35820040
330000              MOVE EMP-PERS-REST-TIME   TO DE-COMPARE3-TIME       35830040
330100           END-IF                                                 35840040
330200        ELSE                                                      35850040
330300           MOVE '000101010001'          TO DE-COMPARE3-DATE-TIME  35860040
330400        END-IF                                                    35870040
330500     END-IF                                                       35880040
330600     IF GOT-EMPLOYEE                                              35890040
330700        AND DISPLAY-EMP                                           35900040
330800        AND EXTRABOARD-RESTED        NOT > SPACES                 35910040
330900        IF EMP-MTOD IS NUMERIC                                    35920040
330910*C975-BEG                                                         35930070
331000*          AND DE-COMPARE1-DATE-TIME     > WS-PRESENT-TIME-CENT   35940070
331010           AND DE-COMPARE1-DATE-TIME     > WS-LOCAL-DATE-TIME-CENT35950070
331020*C975-END                                                         35960070
331100           MOVE EMP-MTOD-NUM            TO WORK-TIME              35970040
331200           MOVE WK-YR                   TO FORM-YR                35980040
331300           MOVE WK-DY                   TO FORM-DY                35990040
331400           MOVE WK-MO                   TO FORM-MO                36000040
331500           MOVE WORK-HR-MN              TO FORM-HRMN              36010040
331600           MOVE FORMAT-DATE-AREA        TO EXTRABOARD-RESTED      36020040
331700        END-IF                                                    36030040
331800        IF EMP-US-RSTD IS NUMERIC                                 36040040
331810*C975-BEG                                                         36050070
331900*          AND DE-COMPARE2-DATE-TIME     > WS-PRESENT-TIME-CENT   36060070
331910           AND DE-COMPARE2-DATE-TIME     > WS-LOCAL-DATE-TIME-CENT36070070
331920*C975-END                                                         36080070
332000           MOVE EMP-US-RSTD             TO WORK-TIME              36090040
332100           MOVE WK-YR                   TO FORM-YR                36100040
332200           MOVE WK-DY                   TO FORM-DY                36110040
332300           MOVE WK-MO                   TO FORM-MO                36120040
332400           MOVE WORK-HR-MN              TO FORM-HRMN              36130040
332500           MOVE FORMAT-DATE-AREA        TO EXTRABOARD-RESTED      36140040
332600        END-IF                                                    36150040
332700        IF EMP-PERS-REST IS NUMERIC                               36160040
332710*C975-BEG                                                         36170070
332800*          AND DE-COMPARE3-DATE-TIME     > WS-PRESENT-TIME-CENT   36180070
332810           AND DE-COMPARE3-DATE-TIME     > WS-LOCAL-DATE-TIME-CENT36190070
332820*C975-END                                                         36200070
332900           MOVE DE-COMPARE3-YYMMDD      TO WORK-DATE              36210040
333000           MOVE DE-COMPARE3-TIME        TO WORK-HR-MN             36220040
333100           MOVE WK-YR                   TO FORM-YR                36230040
333200           MOVE WK-DY                   TO FORM-DY                36240040
333300           MOVE WK-MO                   TO FORM-MO                36250040
333400           MOVE WORK-HR-MN              TO FORM-HRMN              36260040
333500           MOVE FORMAT-DATE-AREA        TO EXTRABOARD-RESTED      36270040
333600        END-IF                                                    36280040
333700     END-IF                                                       36290040
333800*                                                                 36300040
333900*    IF DONT-DISPLAY-EMP, BACK OUT THE WS-POS-CNT CHANGES WE MADE 36310040
334000*    EARLIER.                                                     36320040
334100*                                                                 36330040
334200     IF GOT-EMPLOYEE                                              36340040
334300        AND DONT-DISPLAY-EMP                                      36350040
334400        IF EB-ON-BOARD                                            36360040
334500           AND WS-POS-CNT                > 0                      36370040
334600           SUBTRACT 1                 FROM WS-POS-CNT             36380040
334700        END-IF                                                    36390040
334800     ELSE                                                         36400040
334900        IF SCR-SUB > SCR-MAX                                      36410040
335000           PERFORM P3800-XB-TITLE                                 36420040
335100        END-IF                                                    36430040
335200        MOVE SPACES                     TO PAGE-LINE(SCR-SUB)     36440040
335300        MOVE EXTRABOARDS-2              TO PAGE-LINE(SCR-SUB)     36450040
335400        ADD 1                           TO SCR-SUB                36460040
335500     END-IF                                                       36470040
335600     MOVE SPACES                        TO EXTRABOARDS-2          36480040
335700     .                                                            36490040
335800*                                                                 36500040
335900 P3210-CHECK-REST-DAY.                                            36510040
336000*                                                                 36520040
336100     MOVE SPACES                        TO WS-JOB-SCHED-REST-DAYS 36530040
336200     MOVE SPACES                        TO WORK-JS-KEY1           36540040
336300     MOVE DIST       IN WS-EXTRA-BOARD  TO WK-JSK1-ASGN-DIST      36550040
336400     MOVE SUB-DIST   IN WS-EXTRA-BOARD  TO WK-JSK1-ASGN-SUB-DIST  36560040
336500     STRING 'EX' TURN-NBR OF WS-EXTRA-BOARD                       36570040
336600            DELIMITED BY SIZE                                     36580040
336700            INTO WK-JSK1-ASGN                                     36590040
336800     MOVE CRAFT-CODE IN WS-EXTRA-BOARD  TO WK-JSK1-ASGN-CC        36600040
336900     MOVE 999999                        TO WK-JSK1-EXP-DATE       36610040
337000     MOVE '000'                         TO WK-JSK1-ASGN-DAY       36620040
337100     MOVE WORK-JS-KEY1                  TO JSKEY1                 36630040
337200     PERFORM P8240-READ-JS                                        36640040
337300     IF SUCCESS                                                   36650040
337400        AND JSK1-ASGN-DIST               = WK-JSK1-ASGN-DIST      36660040
337500        AND JSK1-ASGN-SUB-DIST           = WK-JSK1-ASGN-SUB-DIST  36670040
337600        AND JSK1-ASSIGNMENT              = WK-JSK1-ASSIGNMENT     36680040
337700        AND JSK1-EXP-DATE                = WK-JSK1-EXP-DATE       36690040
337800        AND JSK1-ASGN-DAY                = WK-JSK1-ASGN-DAY-NUM   36700040
337900        MOVE JOB-SCHED-REST-DAYS        TO WS-JOB-SCHED-REST-DAYS 36710040
338000     END-IF                                                       36720040
338100                                                                  36730040
338200     MOVE SPACES                        TO WORK-JS-KEY1           36740040
338300     MOVE DIST       OF WS-EXTRA-BOARD  TO WK-JSK1-ASGN-DIST      36750040
338400     MOVE SUB-DIST   OF WS-EXTRA-BOARD  TO WK-JSK1-ASGN-SUB-DIST  36760040
338500     STRING 'EX' TURN-NBR OF WS-EXTRA-BOARD                       36770040
338600            DELIMITED BY SIZE                                     36780040
338700            INTO WK-JSK1-ASGN                                     36790040
338800     MOVE CRAFT-CODE OF WS-EXTRA-BOARD  TO WK-JSK1-ASGN-CC        36800040
338900     MOVE 999999                        TO WK-JSK1-EXP-DATE       36810040
339000     MOVE WEEK-DAY                      TO WK-JSK1-ASGN-DAY-NUM   36820040
339100     MOVE WORK-JS-KEY1                  TO JSKEY1                 36830040
339200     SET FIRST-JS                       TO TRUE                   36840040
339300     PERFORM P8200-STARTBR-JS                                     36850040
339400     IF SUCCESS                                                   36860040
339500        PERFORM P8220-READNEXT-JS                                 36870040
339600     END-IF                                                       36880040
339700     PERFORM UNTIL JS-DONE                                        36890040
339800        IF SUCCESS                                                36900040
339900           AND JSK1-ASGN-DIST            = WK-JSK1-ASGN-DIST      36910040
340000           AND JSK1-ASGN-SUB-DIST        = WK-JSK1-ASGN-SUB-DIST  36920040
340100           AND JSK1-ASSIGNMENT           = WK-JSK1-ASSIGNMENT     36930040
340200           AND JSK1-EXP-DATE             = WK-JSK1-EXP-DATE       36940040
340300           AND JSK1-ASGN-DAY             = WK-JSK1-ASGN-DAY-NUM   36950040
340400           PERFORM P3220-CHECK-REST-PERIOD                        36960040
340500        ELSE                                                      36970040
340600           SET WS-SCHEDULED-REST-PERIOD TO TRUE                   36980040
340700           SET JS-DONE                  TO TRUE                   36990040
340800        END-IF                                                    37000040
340900        IF FIRST-JS                                               37010040
341000           SET MORE-JS                  TO TRUE                   37020040
341100        END-IF                                                    37030040
341200        IF MORE-JS                                                37040040
341300           PERFORM P8220-READNEXT-JS                              37050040
341400        END-IF                                                    37060040
341500     END-PERFORM                                                  37070040
341600     PERFORM P8230-END-JS                                         37080040
341700     IF WS-SCHEDULED-REST-PERIOD                                  37090040
341800        OR WS-SCHEDULED-REST-DAY                                  37100040
341900        SET ON-REST-DAY                 TO TRUE                   37110040
342000     END-IF                                                       37120040
342100     .                                                            37130040
342200                                                                  37140040
342300*************************************************************     37150040
342400 P3220-CHECK-REST-PERIOD.                                         37160040
342500*   THIS ROUTINE DETERMINES IF THE EMPLOYEE IS ON A REST          37170040
342600*   PERIOD, BY EVALUATING THE TIMES HE IS SCHEDULED TO            37180040
342700*   WORK.                                                         37190040
342800*************************************************************     37200040
342900      EVALUATE TRUE                                               37210040
343000         WHEN JSK1-ASGN-START-TIME > WS-LOCAL-TIME                37220040
343100            IF FIRST-JS                                           37230040
343200               IF WS-JOB-ON-REST-DAY(WEEK-DAY)                    37240040
343300                  SET WS-SCHEDULED-REST-PERIOD   TO TRUE          37250040
343400               ELSE                                               37260040
343500                  PERFORM P3230-CHECK-YESTERDAY                   37270040
343600               END-IF                                             37280040
343700            ELSE                                                  37290040
343800               SET WS-SCHEDULED-REST-PERIOD      TO TRUE          37300040
343900            END-IF                                                37310040
344000            SET JS-DONE                          TO TRUE          37320040
344100         WHEN JSK1-ASGN-START-TIME <= WS-LOCAL-TIME               37330040
344200            IF WS-JOB-ON-REST-DAY(WEEK-DAY)                       37340040
344300               SET WS-SCHEDULED-REST-DAY         TO TRUE          37350040
344400               SET JS-DONE                       TO TRUE          37360040
344500            END-IF                                                37370040
344600            IF JOB-SCHED-END-TIME >= WS-LOCAL-TIME                37380040
344700               SET JS-DONE                       TO TRUE          37390040
344800            ELSE                                                  37400040
344900               IF JOB-SCHED-END-TIME < JSK1-ASGN-START-TIME       37410040
345000                  SET JS-DONE                    TO TRUE          37420040
345100               END-IF                                             37430040
345200            END-IF                                                37440040
345300      END-EVALUATE                                                37450040
345400      .                                                           37460040
345500                                                                  37470040
345600*************************************************************     37480040
345700 P3230-CHECK-YESTERDAY.                                           37490040
345800*      IF THE VIEW TIME IS PRIOR TO THE FIRST SCHEDULED           37500040
345900*      WORK PERIOD, THEN THE PREVIOUS DAYS RECORDS HAVE TO BE     37510040
346000*      EVALUATED TO DETERMINE WHETHER THE EMPLOYEE IS STILL ON    37520040
346100*      THE PREVIOUS DAYS ASSIGNMENT.                              37530040
346200*************************************************************     37540040
346300      MOVE ZERO                         TO WS-YESTERDAY-START-TIME37550040
346400                                           WS-YESTERDAY-END-TIME  37560040
346500      IF WEEK-DAY = 1                                             37570040
346600         MOVE 07                         TO WS-YESTERDAY          37580040
346700      ELSE                                                        37590040
346800         SUBTRACT 01 FROM WEEK-DAY   GIVING WS-YESTERDAY          37600040
346900      END-IF                                                      37610040
347000      MOVE SPACES                       TO WORK-JS-KEY1           37620040
347100      MOVE DIST       OF WS-EXTRA-BOARD TO WK-JSK1-ASGN-DIST      37630040
347200      MOVE SUB-DIST   OF WS-EXTRA-BOARD TO WK-JSK1-ASGN-SUB-DIST  37640040
347300      STRING 'EX' TURN-NBR OF WS-EXTRA-BOARD                      37650040
347400             DELIMITED BY SIZE                                    37660040
347500             INTO WK-JSK1-ASGN                                    37670040
347600      MOVE CRAFT-CODE OF WS-EXTRA-BOARD TO WK-JSK1-ASGN-CC        37680040
347700      MOVE 999999                       TO WK-JSK1-EXP-DATE       37690040
347800      MOVE WS-YESTERDAY                 TO WK-JSK1-ASGN-DAY-NUM   37700040
347900      MOVE WORK-JS-KEY1                 TO JSKEY1                 37710040
348000      PERFORM P8200-STARTBR-JS                                    37720040
348100      IF SUCCESS                                                  37730040
348200         PERFORM P8220-READNEXT-JS                                37740040
348300      END-IF                                                      37750040
348400      PERFORM UNTIL JS-DONE                                       37760040
348500         IF SUCCESS                                               37770040
348600            AND JSK1-ASGN-DIST           = WK-JSK1-ASGN-DIST      37780040
348700            AND JSK1-ASGN-SUB-DIST       = WK-JSK1-ASGN-SUB-DIST  37790040
348800            AND JSK1-ASSIGNMENT          = WK-JSK1-ASSIGNMENT     37800040
348900            AND JSK1-EXP-DATE            = WK-JSK1-EXP-DATE       37810040
349000            AND JSK1-ASGN-DAY            = WK-JSK1-ASGN-DAY-NUM   37820040
349100            MOVE JSK1-ASGN-START-TIME   TO WS-YESTERDAY-START-TIME37830040
349200            MOVE JOB-SCHED-END-TIME     TO WS-YESTERDAY-END-TIME  37840040
349300            SET MORE-JS                 TO TRUE                   37850040
349400         ELSE                                                     37860040
349500            SET JS-DONE                 TO TRUE                   37870040
349600         END-IF                                                   37880040
349700         IF MORE-JS                                               37890040
349800            PERFORM P8220-READNEXT-JS                             37900040
349900         END-IF                                                   37910040
350000      END-PERFORM                                                 37920040
350100      PERFORM P8230-END-JS                                        37930040
350200      IF WS-YESTERDAY-END-TIME < WS-YESTERDAY-START-TIME          37940040
350300         AND WS-YESTERDAY-END-TIME >= WS-LOCAL-TIME               37950040
350400         CONTINUE                                                 37960040
350500      ELSE                                                        37970040
350600         SET WS-SCHEDULED-REST-PERIOD   TO TRUE                   37980040
350700      END-IF                                                      37990040
350800     .                                                            38000040
350900*                                                                 38010040
351000 P3250-CHECK-EXTENDED-REST-DAY.                                   38020040
351100*                                                                 38030040
351200     MOVE SPACES                        TO PS42-COMMAREA-PARMS    38040040
351300     SET PS42-GET-CURRENT-SCHED         TO TRUE                   38050040
351400     MOVE DIST       OF WS-EXTRA-BOARD  TO PS42-DIST-CD           38060040
351500     MOVE SUB-DIST   OF WS-EXTRA-BOARD  TO PS42-SUB-DIST-CD       38070040
351600     MOVE CRAFT-CODE OF WS-EXTRA-BOARD  TO PS42-BOARD-ID          38080040
351700     MOVE TURN-NBR   OF WS-EXTRA-BOARD  TO PS42-TURN-CD           38090040
351800     MOVE WS-LOCAL-DATE                 TO PS42-START-DATE        38100040
351900     MOVE WS-LOCAL-TIME                 TO PS42-FROM-TIME         38110040
352000*                                                                 38120040
352100***  CALL P656-PGM USING PS42-COMMAREA-PARMS                      38130040
352200     EXEC CICS LINK                                               38140040
352300               PROGRAM(PS42-PGM)                                  38150040
352400               COMMAREA(PS42-COMMAREA-PARMS)                      38160040
352500               LENGTH(PS42-LGTH)                                  38170040
352600               RESP(WS-RESPONSE)                                  38180040
352700     END-EXEC                                                     38190040
352800     MOVE WS-RESPONSE              TO FILE-STATUS                 38200040
352900     IF NOT SUCCESS                                               38210040
353000        MOVE 'P3250-1'             TO ERR-PARAGRAPH               38220040
353100        MOVE 'PS42LINK'            TO ERR-KEY                     38230040
353200        PERFORM P9999-GOT-PROBLEM                                 38240040
353300     END-IF                                                       38250040
353400*                                                                 38260040
353500     IF PS42-SCHEDULES-EXIST                                      38270040
353600        OR PS42-CURR-SCHED-FOUND                                  38280040
353700        IF PS42-ON-REST-DAY                                       38290040
353800           SET ON-REST-DAY              TO TRUE                   38300040
353900        END-IF                                                    38310040
354000     ELSE                                                         38320040
354100        SET ON-REST-DAY                 TO TRUE                   38330040
354200     END-IF                                                       38340040
354300     .                                                            38350040
354400*CNC0516-BEG                                                      38360044
354500 P3260-GET-STATUS.                                                38370044
354600     INITIALIZE WS-EXTRABOARD-RESTED                              38380044
354700     EVALUATE TRUE                                                38390044
354800          WHEN OFF-MILES-DAYS                                     38400044
354900               PERFORM P9830-RETRIEVE-CNTL-INFO                   38410044
355000               IF P956-ST-RSN-NBR-DAYS-REQ                        38420044
355100               OR P956-ST-RSN-EXP-DATE-REQ                        38430044
355200                  PERFORM P3265-GET-DUEBACK-DATE                  38440044
355300                  IF WS-DUEBACK-FOUND-Y                           38450044
355400                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE 38460044
355500                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME 38470044
355510*CNC0573 - BEG                                                    38480078
355520                     IF  PSTCA-FROM-FLD-MENU                      38490078
355530                     AND PSTCA-FLD-MENU-OPT NOT = '004'           38500078
355540                        IF P956-MASK-FLD-SCR-YES                  38510078
355541*CNC0576 - BEG                                                    38520082
355542                        OR P956-MASK-HOLD-TURN                    38530082
355543                           MOVE '**'            TO WS-LAYOFF-CODE 38540082
355544                           IF P956-MASK-HOLD-TURN                 38550082
355545                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE 38560082
355546                           ELSE                                   38570082
355547                              MOVE '**'      TO WS-LAYOFF-EM-CODE 38580082
355548                           END-IF                                 38590082
355549*CNC0576 - END                                                    38600082
355560                        ELSE                                      38610078
355570                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE 38620078
355580                           MOVE LAYOFF-EM-CODE  TO                38630078
355590                                WS-LAYOFF-EM-CODE                 38640078
355591                        END-IF                                    38650078
355592                     ELSE                                         38660078
355593                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE 38670078
355594                        MOVE LAYOFF-EM-CODE     TO                38680078
355595                             WS-LAYOFF-EM-CODE                    38690078
355596                     END-IF                                       38700078
355597*CNC0573 - END                                                    38710078
355900                     MOVE WS-EXTRABOARD-RESTED  TO                38720044
356000                          EXTRABOARD-RESTED                       38730044
356100                  ELSE                                            38740044
356200                     MOVE '<UNAVAILABLE>'       TO                38750044
356300                           EXTRABOARD-RESTED                      38760044
356400                  END-IF                                          38770044
356500               ELSE                                               38780044
356600                  PERFORM P3266-OFF-MILES-RETURN-DATE             38790044
356700                  MOVE WS-RETURN-DATE-1(1:4) TO WS-RETURN-DATE    38800053
356800                  MOVE SPACES                TO WS-RETURN-TIME    38810053
356810*CNC0573 - BEG                                                    38820078
356820                  IF  PSTCA-FROM-FLD-MENU                         38830078
356830                  AND PSTCA-FLD-MENU-OPT NOT = '004'              38840078
356840                     IF P956-MASK-FLD-SCR-YES                     38850078
356851*CNC0576 - BEG                                                    38860082
356852                     OR P956-MASK-HOLD-TURN                       38870082
356853                        MOVE '**'            TO WS-LAYOFF-CODE    38880082
356854                        IF P956-MASK-HOLD-TURN                    38890082
356855                           MOVE 'HT'      TO WS-LAYOFF-EM-CODE    38900082
356856                        ELSE                                      38910082
356857                           MOVE '**'      TO WS-LAYOFF-EM-CODE    38920082
356858                        END-IF                                    38930082
356859*CNC0576 - END                                                    38940082
356860                     ELSE                                         38950078
356870                        MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE    38960078
356880                        MOVE LAYOFF-EM-CODE  TO WS-LAYOFF-EM-CODE 38970078
356890                     END-IF                                       38980078
356891                  ELSE                                            38990078
356892                     MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE    39000078
356893                     MOVE LAYOFF-EM-CODE     TO WS-LAYOFF-EM-CODE 39010078
356894                  END-IF                                          39020078
356895*CNC0573 - END                                                    39030078
357100                  MOVE WS-EXTRABOARD-RESTED  TO EXTRABOARD-RESTED 39040053
357200               END-IF                                             39050044
357300          WHEN VACATION                                           39060044
357400               PERFORM P3267-VACATION-RETURN-DATE                 39070044
357500               MOVE WS-RETURN-DATE-1(1:4) TO WS-RETURN-DATE       39080053
357600               MOVE WS-RETURN-DATE-1(5:4) TO WS-RETURN-TIME       39090053
357610*CNC0573 - BEG                                                    39100078
357620               IF  PSTCA-FROM-FLD-MENU                            39110078
357630               AND PSTCA-FLD-MENU-OPT NOT = '004'                 39120078
357640                  PERFORM P9830-RETRIEVE-CNTL-INFO                39130078
357650                  IF P956-MASK-FLD-SCR-YES                        39140078
357661*CNC0576 - BEG                                                    39150082
357662                  OR P956-MASK-HOLD-TURN                          39160082
357663                     MOVE '**'            TO WS-LAYOFF-CODE       39170082
357664                     IF P956-MASK-HOLD-TURN                       39180082
357665                        MOVE 'HT'         TO WS-LAYOFF-EM-CODE    39190082
357666                     ELSE                                         39200082
357667                        MOVE '**'         TO WS-LAYOFF-EM-CODE    39210082
357668                     END-IF                                       39220082
357669*CNC0576 - END                                                    39230082
357670                  ELSE                                            39240078
357680                     MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE       39250078
357690                     MOVE LAYOFF-EM-CODE  TO WS-LAYOFF-EM-CODE    39260078
357691                  END-IF                                          39270078
357692               ELSE                                               39280078
357693                  MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE       39290078
357694                  MOVE LAYOFF-EM-CODE     TO WS-LAYOFF-EM-CODE    39300078
357695               END-IF                                             39310078
357696*CNC0573 - END                                                    39320078
358000               MOVE WS-EXTRABOARD-RESTED  TO EXTRABOARD-RESTED    39330053
358100          WHEN EXCUSED-ABSENCE                                    39340044
358200           AND LAYOFF-EM-CODE = '69'                              39350044
358300               PERFORM P3265-GET-DUEBACK-DATE                     39360044
358400               IF WS-DUEBACK-FOUND-Y                              39370044
358500                  MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE    39380044
358600                  MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME    39390044
358610*CNC0573 - BEG                                                    39400078
358620                  IF  PSTCA-FROM-FLD-MENU                         39410078
358630                  AND PSTCA-FLD-MENU-OPT NOT = '004'              39420078
358640                     PERFORM P9830-RETRIEVE-CNTL-INFO             39430078
358650                     IF P956-MASK-FLD-SCR-YES                     39440078
358661*CNC0576 - BEG                                                    39450082
358662                     OR P956-MASK-HOLD-TURN                       39460082
358663                        MOVE '**'         TO WS-LAYOFF-CODE       39470082
358664                        IF P956-MASK-HOLD-TURN                    39480082
358665                           MOVE 'HT'      TO WS-LAYOFF-EM-CODE    39490082
358666                        ELSE                                      39500082
358667                           MOVE '**'      TO WS-LAYOFF-EM-CODE    39510082
358668                        END-IF                                    39520082
358669*CNC0576 - END                                                    39530082
358670                     ELSE                                         39540078
358680                        MOVE LAYOFF-CODE  TO WS-LAYOFF-CODE       39550078
358690                        MOVE LAYOFF-EM-CODE                       39560078
358691                                          TO WS-LAYOFF-EM-CODE    39570078
358692                     END-IF                                       39580078
358693                  ELSE                                            39590078
358694                     MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE       39600078
358695                     MOVE LAYOFF-EM-CODE  TO WS-LAYOFF-EM-CODE    39610078
358696                  END-IF                                          39620078
358697*CNC0573 - END                                                    39630078
359000                  MOVE WS-EXTRABOARD-RESTED  TO                   39640044
359100                       EXTRABOARD-RESTED                          39650044
359200               ELSE                                               39660044
359300                  MOVE '<UNAVAILABLE>'       TO                   39670044
359400                       EXTRABOARD-RESTED                          39680044
359500               END-IF                                             39690044
359600          WHEN OTHER                                              39700044
359700               IF NOT AVAILABLE  AND                              39710044
359800                  NOT WORKING    AND                              39720044
359900                  NOT TO-PLACE                                    39730044
360000                  AND LAYOFF-TIME NUMERIC                         39740044
360100                  AND LAYOFF-TIME > ZERO                          39750044
360200                  PERFORM P3265-GET-DUEBACK-DATE                  39760044
360300                  IF WS-DUEBACK-FOUND-Y                           39770044
360400                    PERFORM P3268-CHECK-FOR-E95-DTTM              39780058
360500                    IF WS-E95 > SPACES                            39790057
360510*CNC0573 - BEG                                                    39800078
360520                       IF  PSTCA-FROM-FLD-MENU                    39810078
360530                       AND PSTCA-FLD-MENU-OPT NOT = '004'         39820078
360540                          PERFORM P9830-RETRIEVE-CNTL-INFO        39830079
360550                          IF P956-MASK-FLD-SCR-YES                39840078
360561*CNC0576 - BEG                                                    39850082
360562                          OR P956-MASK-HOLD-TURN                  39860082
360563                             MOVE '**'          TO WS-E95-CODE    39870082
360564                             IF P956-MASK-HOLD-TURN               39880082
360565                                MOVE 'HT'       TO WS-E95-EM-CODE 39890082
360566                             ELSE                                 39900082
360567                                MOVE '**'       TO WS-E95-EM-CODE 39910082
360568                             END-IF                               39920082
360569                          END-IF                                  39930083
360593                       END-IF                                     39940078
360594                       MOVE WS-E95            TO EXTRABOARD-RESTED39950083
360595*CNC0576 - END                                                    39960083
360596*CNC0573 - END                                                    39970078
360800                    ELSE                                          39980057
360900                     MOVE TASK-LO-EXP-DATE(3:4) TO WS-RETURN-DATE 39990058
361000                     MOVE TASK-LO-EXP-TIME      TO WS-RETURN-TIME 40000058
361010*CNC0573 - BEG                                                    40010078
361020                     IF  PSTCA-FROM-FLD-MENU                      40020078
361030                     AND PSTCA-FLD-MENU-OPT NOT = '004'           40030078
361040                        PERFORM P9830-RETRIEVE-CNTL-INFO          40040078
361050                        IF P956-MASK-FLD-SCR-YES                  40050078
361061*CNC0576 - BEG                                                    40060082
361062                        OR P956-MASK-HOLD-TURN                    40070082
361063                           MOVE '**'            TO WS-LAYOFF-CODE 40080082
361064                           IF P956-MASK-HOLD-TURN                 40090082
361065                              MOVE 'HT'      TO WS-LAYOFF-EM-CODE 40100082
361066                           ELSE                                   40110082
361067                              MOVE '**'      TO WS-LAYOFF-EM-CODE 40120082
361068                           END-IF                                 40130082
361069*CNC0576 - END                                                    40140082
361070                        ELSE                                      40150078
361080                           MOVE LAYOFF-CODE     TO WS-LAYOFF-CODE 40160078
361090                           MOVE LAYOFF-EM-CODE  TO                40170078
361091                                WS-LAYOFF-EM-CODE                 40180078
361092                        END-IF                                    40190078
361093                     ELSE                                         40200078
361094                        MOVE LAYOFF-CODE        TO WS-LAYOFF-CODE 40210078
361095                        MOVE LAYOFF-EM-CODE     TO                40220078
361096                             WS-LAYOFF-EM-CODE                    40230078
361097                     END-IF                                       40240078
361098*CNC0573 - END                                                    40250078
361400                     MOVE WS-EXTRABOARD-RESTED  TO                40260058
361500                          EXTRABOARD-RESTED                       40270058
361600                    END-IF                                        40280057
361700                  ELSE                                            40290044
361800                    MOVE '<UNAVAILABLE>'        TO                40300058
361900                         EXTRABOARD-RESTED                        40310044
362000                  END-IF                                          40320044
362100               END-IF                                             40330044
362200     END-EVALUATE.                                                40340044
362300                                                                  40350044
362400 P3265-GET-DUEBACK-DATE.                                          40360044
362500                                                                  40370044
362600     MOVE SPACES                        TO TASK-EMPLOYEE-KEY      40380044
362700     SET WS-DUEBACK-FOUND-N             TO TRUE                   40390044
362800     MOVE EMP-NBR OF WS-MSTR            TO EMP-NBR OF WS-TASK     40400044
362900     PERFORM P8300-START-TASK-FILE                                40410044
363000     IF SUCCESS                                                   40420044
363100        PERFORM P8310-READNEXT-TASK-FILE                          40430044
363200        PERFORM UNTIL NOT SUCCESS                                 40440044
363300           OR WS-DUEBACK-FOUND-Y                                  40450044
363400           OR EMP-NBR OF WS-MSTR NOT = EMP-NBR OF WS-TASK         40460044
363500           IF   TASK-LAYOFF-MARKUP                                40470044
363600           AND ((TASK-DUE-BACK AND TASK-LO1 = 'A') OR             40480052
363700                 TASK-LO1 = 'A')                                  40490052
363800              SET WS-DUEBACK-FOUND-Y    TO TRUE                   40500044
363900           ELSE                                                   40510044
364000              PERFORM P8310-READNEXT-TASK-FILE                    40520044
364100           END-IF                                                 40530044
364200        END-PERFORM                                               40540044
364300     END-IF                                                       40550044
364400     PERFORM P8320-ENDBR-TASK-FILE                                40560044
364500     .                                                            40570044
364600*                                                                 40580044
364700 P3266-OFF-MILES-RETURN-DATE.                                     40590044
364800*                                                                 40600044
364900     INITIALIZE WS-RETURN-DATE-1                                  40610054
365000     IF EMP-MILES-DATE NUMERIC                                    40620044
365100        AND EMP-MILES-DATE-NUM > 0                                40630044
365200        AND EMP-MILES-DATE-NUM < 32                               40640044
365300        MOVE WS-SYS-MO TO WS-RETURN-DATE-MM                       40650044
365400        MOVE EMP-MILES-DATE-NUM TO WS-RETURN-DATE-DD              40660044
365500        IF EMP-MILES-DATE-NUM < WS-SYS-DY                         40670044
365600           ADD 1 TO WS-RETURN-DATE-MM                             40680044
365700           IF WS-RETURN-DATE-MM > 12                              40690044
365800              MOVE 1 TO WS-RETURN-DATE-MM                         40700044
365900           END-IF                                                 40710044
366000        END-IF                                                    40720044
366100     ELSE                                                         40730044
366200        MOVE EMP-MILES-DATE TO WS-RETURN-DATE-1(1:4)              40740054
366300     END-IF                                                       40750044
366400     .                                                            40760044
366500*                                                                 40770044
366600 P3267-VACATION-RETURN-DATE.                                      40780044
366700*                                                                 40790044
366800     INITIALIZE WS-RETURN-DATE-1                                  40800054
366900                WS-EFFECTIVE-DATE-TIME                            40810056
367000     INITIALIZE TASK-EMPLOYEE-KEY                                 40820044
367100     MOVE EMP-NBR OF WS-MSTR TO EMP-NBR OF WS-TASK                40830044
367200     PERFORM P8300-START-TASK-FILE                                40840044
367300     IF SUCCESS                                                   40850044
367400        SET TASK-NOT-DONE    TO TRUE                              40860055
367500        PERFORM P8310-READNEXT-TASK-FILE                          40870044
367600        PERFORM UNTIL TASK-DONE                                   40880044
367700           IF SUCCESS                                             40890044
367800              AND EMP-NBR OF WS-MSTR = EMP-NBR OF WS-TASK         40900044
367900              IF TASK-LAYOFF-MARKUP                               40910044
368000*C1129-BEG                                                        40920085
368100                AND TASK-LO1 = 'A'                                40930085
368101*C1129-END                                                        40940085
368102                 SET TASK-DONE           TO TRUE                  40950055
368103                 MOVE SPACES             TO WS-CNTL-FILE          40960055
368200                 SET SUB-DIST-TYPE-REC   TO TRUE                  40970055
368300                 MOVE TASK-DIST          TO CNTL-DIST             40980055
368400                 MOVE TASK-SUB-DIST      TO CNTL-SUB-DIST         40990055
368500                 MOVE CNTLKEY-AREA       TO CNTLKEY               41000055
368600                 PERFORM P8000-READ-CNTLFILE                      41010055
368700                 MOVE SPACES             TO TZ-PARAMETERS         41020055
368800                 SET TZ-IN-EASTERN-ZONE  TO TRUE                  41030055
368900                 MOVE EFFECTIVE-DATE-TIME    TO TZ-IN-DATE-TIME   41040055
369000                 MOVE CNTL-TIME-ZONE     TO TZ-OUT-ZONE           41050055
369100                 PERFORM P8996-TIMEZONE                           41060055
369200                 MOVE TZ-OUT-DATE-TIME   TO WS-EFFECTIVE-DATE-TIME41070055
369300                 MOVE WS-EFF-MO          TO WS-RETURN-DATE-MM     41080055
369400                 MOVE WS-EFF-DY          TO WS-RETURN-DATE-DD     41090055
369500                 MOVE WS-EFF-HR-MN       TO WS-RETURN-DATE-HRMN   41100055
369600              ELSE                                                41110044
369700                 PERFORM P8310-READNEXT-TASK-FILE                 41120044
369800              END-IF                                              41130044
369900           ELSE                                                   41140044
370000              SET TASK-DONE TO TRUE                               41150044
370100           END-IF                                                 41160044
370200        END-PERFORM                                               41170044
370300     END-IF                                                       41180044
370400     PERFORM P8320-ENDBR-TASK-FILE                                41190044
370500     .                                                            41200044
370600 P3268-CHECK-FOR-E95-DTTM.                                        41210058
370700     MOVE SPACES                       TO WS-E95                  41220058
370800     MOVE EMP-NBR OF WS-MSTR           TO MSTR2NBRK               41230057
370900     PERFORM P8600-READ-MSTR2                                     41240057
371000     IF SUCCESS                                                   41250057
371100        IF MSTR2-RSA-LO-ST-RSN         = 'E95'                    41260057
371200           IF MSTR2-RSA-EXP-DTTM       > TASK-LO-EXP-DATE-TIME    41270057
371300              MOVE MSTR2-RSA-EXP-DTTM(3:4)  TO WS-E95-DATE        41280057
371400              MOVE MSTR2-RSA-EXP-DTTM(7:4)  TO WS-E95-TIME        41290057
371500              MOVE MSTR2-RSA-STATUS         TO WS-E95-CODE        41300057
371600              MOVE MSTR2-RSA-RSN-CD         TO WS-E95-EM-CODE     41310057
371700           END-IF                                                 41320057
371800        END-IF                                                    41330057
371900     END-IF.                                                      41340057
372000*CNC0516-END                                                      41350044
372100 P3300-SEARCH-SWASSGN.                                            41360040
372200                                                                  41370040
372300     MOVE SPACES                       TO WS-SWCNTL-FILE          41380040
372400     MOVE DIST OF WS-EXTRA-BOARD       TO SWCNTL-K-DISTRICT       41390040
372500     MOVE SUB-DIST OF WS-EXTRA-BOARD   TO SWCNTL-K-SUB-DIST       41400040
372600     MOVE DAY1                         TO SWCNTL-K-DAY            41410040
372700     MOVE ALL '9'                      TO SWCNTL-K-ASSIGN         41420040
372800     MOVE SWCNTL-KEY                   TO SWJOBKEY                41430040
372900***  READ SWASSGN-FILE RECORD                                     41440040
373000***       INTO WS-SWCNTL-FILE                                     41450040
373100***       INVALID KEY CONTINUE                                    41460040
373200***  END-READ                                                     41470040
373300     EXEC CICS READ                                               41480040
373400               DATASET(SWASSGN-VIA-ASSIGNMENT)                    41490040
373500               INTO(WS-SWCNTL-FILE)                               41500040
373600               LENGTH(SWASSIGN-RLGTH)                             41510040
373700               RIDFLD(SWJOBKEY)                                   41520040
373800               KEYLENGTH(SWASSIGN-KLGTH)                          41530040
373900               RESP(WS-RESPONSE)                                  41540040
374000     END-EXEC                                                     41550040
374100     MOVE WS-RESPONSE TO FILE-STATUS                              41560040
374200     MOVE DAY1                         TO SW-DAY1                 41570040
374300     IF SUCCESS AND SWCNTL-DATE = WS-SYSTEM-DATE                  41580040
374400        IF SWCNTL-SHIFT3 NOT = 'X'                                41590040
374500          IF SW-DAY1 = 1                                          41600040
374600             MOVE 7                 TO SW-DAY1                    41610040
374700          ELSE                                                    41620040
374800             SUBTRACT 1 FROM SW-DAY1                              41630040
374900          END-IF                                                  41640040
375000        END-IF                                                    41650040
375100     ELSE                                                         41660040
375200       IF SW-DAY1 = 1                                             41670040
375300          MOVE 7                    TO SW-DAY1                    41680040
375400       ELSE                                                       41690040
375500          SUBTRACT 1 FROM SW-DAY1                                 41700040
375600       END-IF                                                     41710040
375700     END-IF                                                       41720040
375800*                                                                 41730040
375900     MOVE SPACES                       TO WS-SWASSGN-FILE         41740040
376000     MOVE DIST OF WS-EXTRA-BOARD       TO SWASSGN-K-DISTRICT      41750040
376100     MOVE SUB-DIST OF WS-EXTRA-BOARD   TO SWASSGN-K-SUB-DIST      41760040
376200     MOVE SW-DAY1                      TO SWASSGN-K-DAY           41770040
376300     MOVE TURN-NBR OF WS-EXTRA-BOARD   TO WS-SW-POSITION          41780040
376400     MOVE CRAFT-CODE OF WS-EXTRA-BOARD TO WS-SW-CRAFT             41790040
376500     MOVE WS-SWASSGN-ASGN              TO SWASSGN-K-ASSIGN        41800040
376600     MOVE SWASSGN-KEY                  TO SWJOBKEY                41810040
376700***  READ SWASSGN-FILE RECORD                                     41820040
376800***       INTO WS-SWASSGN-FILE                                    41830040
376900***       INVALID KEY CONTINUE                                    41840040
377000***  END-READ                                                     41850040
377100     EXEC CICS READ                                               41860040
377200               DATASET(SWASSGN-VIA-ASSIGNMENT)                    41870040
377300               INTO(WS-SWASSGN-FILE)                              41880040
377400               LENGTH(SWASSIGN-RLGTH)                             41890040
377500               RIDFLD(SWJOBKEY)                                   41900040
377600               KEYLENGTH(SWASSIGN-KLGTH)                          41910040
377700               RESP(WS-RESPONSE)                                  41920040
377800     END-EXEC                                                     41930040
377900     MOVE WS-RESPONSE TO FILE-STATUS                              41940040
378000     IF SUCCESS                                                   41950040
378100        MOVE SWASSGN-EMP-NO            TO ASGN-EMP-NO             41960040
378200     END-IF.                                                      41970040
378300                                                                  41980040
378400*                                                                 41990040
378500 P3800-XB-TITLE.                                                  42000040
378600                                                                  42010040
378700     PERFORM P7900-TITLE                                          42020040
378800     ADD 1                       TO SCR-SUB                       42030040
378900     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  42040040
379000     MOVE EXTRABOARDS-TITLE      TO PAGE-LINE(SCR-SUB)            42050040
379100     ADD 1                       TO SCR-SUB.                      42060040
379200                                                                  42070040
379300 P3900-START-XB-POS.                                              42080040
379400                                                                  42090040
379500     IF XB-POS-TIME-NUM NUMERIC AND                               42100040
379600        XB-POS-TIME-NUM > ZEROES                                  42110040
379700        ADD 1                   TO XB-POS-TIME-NUM                42120040
379800     END-IF                                                       42130040
379900     MOVE WORK-XB-POS-KEY TO EBPOS                                42140040
380000***  START EB-FILE KEY > EBPOS-FS-KEY                             42150040
380100***    INVALID KEY CONTINUE                                       42160040
380200***  END-START                                                    42170040
380300     EXEC CICS STARTBR                                            42180040
380400               DATASET(EB-VIA-CRAFT-POSITION)                     42190040
380500               RIDFLD(EBPOS)                                      42200040
380600               GTEQ                                               42210040
380700               RESP(WS-RESPONSE)                                  42220040
380800     END-EXEC                                                     42230040
380900     MOVE WS-RESPONSE           TO FILE-STATUS                    42240040
381000     IF NOT SUCCESS                                               42250040
381100        IF NOT (END-OF-FILE OR NO-RECORD-FND)                     42260040
381200           MOVE EBPOS                 TO ERR-KEY                  42270040
381300           MOVE 'FAILED START EBPOS'  TO ERR-SENTENCE             42280040
381400           PERFORM P9999-GOT-PROBLEM                              42290040
381500        END-IF                                                    42300040
381600     END-IF.                                                      42310040
381700                                                                  42320040
381800 P3920-READ-NEXT-XB-POS.                                          42330040
381900                                                                  42340040
382000     EXEC CICS READNEXT                                           42350040
382100               DATASET(EB-VIA-CRAFT-POSITION)                     42360040
382200               INTO(WS-EXTRA-BOARD)                               42370040
382300               LENGTH(EBCRPOS-RLGTH)                              42380040
382400               RIDFLD(EBPOS)                                      42390040
382500               KEYLENGTH(EBCRPOS-KLGTH)                           42400040
382600               RESP(WS-RESPONSE)                                  42410040
382700     END-EXEC                                                     42420040
382800     MOVE WS-RESPONSE TO FILE-STATUS                              42430040
382900     IF NOT SUCCESS                                               42440040
383000        IF NOT (END-OF-FILE OR NO-RECORD-FND)                     42450040
383100           MOVE EBPOS                 TO ERR-KEY                  42460040
383200           MOVE 'FAILED READNXT EBPOS'  TO ERR-SENTENCE           42470040
383300           PERFORM P9999-GOT-PROBLEM                              42480040
383400        END-IF                                                    42490040
383500     END-IF.                                                      42500040
383600                                                                  42510040
383700 P3930-END-XB-POS.                                                42520040
383800                                                                  42530040
383900     EXEC CICS ENDBR                                              42540040
384000               DATASET(EB-VIA-CRAFT-POSITION)                     42550040
384100               RESP(WS-RESPONSE)                                  42560040
384200     END-EXEC.                                                    42570040
384300*                                                                 42580040
384400 P4000-GET-HOS.                                                   42590040
384500*                                                                 42600040
384600     MOVE SPACES                        TO WS-CNTL-FILE           42610040
384700     SET SUB-DIST-TYPE-REC              TO TRUE                   42620040
384800     MOVE P27NCA-DIST                   TO CNTL-DIST              42630040
384900     MOVE P27NCA-SUB-DIST               TO CNTL-SUB-DIST          42640040
385000     MOVE CNTLKEY-AREA                  TO CNTLKEY                42650040
385100     PERFORM P8000-READ-CNTLFILE                                  42660040
385200     IF NOT SUCCESS                                               42670040
385300        MOVE 'P9820-1'                  TO ERR-PARAGRAPH          42680040
385400        MOVE CNTLKEY                    TO ERR-KEY                42690040
385500        PERFORM P9999-GOT-PROBLEM                                 42700040
385600     END-IF                                                       42710040
385700                                                                  42720040
385800     INITIALIZE WS-TOT-TIME                                       42730040
385900                WS-LIMBO-TIME                                     42740040
386000                WS-CONSEC-STARTS                                  42750040
386100     SET PS94-SUM-FUNC       TO TRUE                              42760040
386200     MOVE WS-LOCAL-DATE-TIME TO PS94-HSL-EVENT-STRT-TS-DTTM       42770040
386300                                PS94-HSL-EVENT-END-TS-DTTM        42780040
386400     MOVE CNTL-TIME-ZONE           TO PS94-TIME-ZONE              42790040
386500     PERFORM P9992-CALL-PS94-PROGRAM                              42800040
386600     MOVE PS94-MTD-TOTAL-TM        TO WS-TOT-TIME                 42810040
386700     MOVE PS94-MTD-LIMBO-TM        TO WS-LIMBO-TIME               42820040
386800     MOVE PS94-CONSECUTIVE-STARTS  TO WS-CONSEC-STARTS            42830040
386900     .                                                            42840040
387000*                                                                 42850040
387100 P5000-OFF-BOARDS.                                                42860040
387200*                                                                 42870040
387300     MOVE 'P5000'            TO ERR-PARAGRAPH                     42880040
387400                                                                  42890040
387500     PERFORM VARYING OB-SUB FROM 1 BY 1 UNTIL OB-SUB > 3          42900040
387600       ADD 1                     TO SCR-SUB                       42910040
387700       PERFORM P5010-OFF-BOARD-TITLE                              42920040
387800       MOVE SPACES         TO WS-MSTR                             42930040
387900       SET OB-NOT-DONE     TO TRUE                                42940040
388000       MOVE P27NCA-DIST    TO DIST OF WS-MSTR                     42950040
388100       MOVE P27NCA-SUB-DIST TO SUB-DIST OF WS-MSTR                42960040
388200       MOVE DIST-SDIST-CRFT-EMPNO-KEY TO MSTRDSCEK                42970040
388300       EXEC CICS STARTBR                                          42980040
388400                 DATASET(MSTR-VIA-DIST-SDIST-CRFT-EMP)            42990040
388500                 RIDFLD(MSTRDSCEK)                                43000040
388600                 GTEQ                                             43010040
388700                 RESP(WS-RESPONSE)                                43020040
388800       END-EXEC                                                   43030040
388900       MOVE WS-RESPONSE         TO FILE-STATUS                    43040040
389000       IF SUCCESS                                                 43050040
389100          PERFORM UNTIL OB-DONE                                   43060040
389200             EXEC CICS READNEXT                                   43070040
389300                       DATASET(MSTR-VIA-DIST-SDIST-CRFT-EMP)      43080040
389400                       INTO(WS-MSTR)                              43090040
389500                       LENGTH(MSTRDSCE-RLGTH)                     43100040
389600                       RIDFLD(MSTRDSCEK)                          43110040
389700                       KEYLENGTH(MSTRDSCE-KLGTH)                  43120040
389800                       RESP(WS-RESPONSE)                          43130040
389900             END-EXEC                                             43140040
390000             MOVE WS-RESPONSE TO FILE-STATUS                      43150040
390100             IF SUCCESS                                           43160040
390200                IF DIST OF WS-MSTR = P27NCA-DIST                  43170040
390300                   AND SUB-DIST OF WS-MSTR = P27NCA-SUB-DIST      43180040
390400                   IF NOT AVAILABLE AND NOT WORKING               43190040
390500                      MOVE CRAFT OF WS-MSTR TO                    43200040
390600                                 WS-CRAFT-CODE-CHECK              43210040
390700                      PERFORM P5100-WRITE-OFF-DETAIL              43220040
390800                          THRU P5100-WRITE-OFF-DETAIL-EXIT        43230040
390900                   END-IF                                         43240040
391000                ELSE                                              43250040
391100                   SET OB-DONE TO TRUE                            43260040
391200                END-IF                                            43270040
391300             ELSE                                                 43280040
391400                SET OB-DONE TO TRUE                               43290040
391500             END-IF                                               43300040
391600          END-PERFORM                                             43310040
391700       END-IF                                                     43320040
391800       EXEC CICS ENDBR                                            43330040
391900                 DATASET(MSTR-VIA-DIST-SDIST-CRFT-EMP)            43340040
392000                 RESP(WS-RESPONSE)                                43350040
392100       END-EXEC                                                   43360040
392200     END-PERFORM.                                                 43370040
392300*                                                                 43380040
392400 P5010-OFF-BOARD-TITLE.                                           43390040
392500*                                                                 43400040
392600     IF SCR-SUB > SCR-MAX                                         43410040
392700        PERFORM P7900-TITLE                                       43420040
392800     END-IF                                                       43430040
392900     EVALUATE OB-SUB                                              43440040
393000        WHEN 1                                                    43450040
393100             MOVE 'ENGINEERS OFF'       TO LAYOFF-TITLE-FIELD     43460040
393200        WHEN 2                                                    43470040
393300             MOVE 'TRAINMEN OFF '       TO LAYOFF-TITLE-FIELD     43480040
393400        WHEN OTHER                                                43490040
393500             MOVE 'SWITCHMAN OFF'       TO LAYOFF-TITLE-FIELD     43500040
393600     END-EVALUATE                                                 43510040
393700     MOVE SPACES                        TO PAGE-LINE(SCR-SUB)     43520040
393800     MOVE LAYOFF-TITLE                  TO PAGE-LINE(SCR-SUB)     43530040
393900     ADD  1                             TO SCR-SUB                43540040
394000     .                                                            43550040
394100*                                                                 43560040
394200 P5100-WRITE-OFF-DETAIL.                                          43570040
394300*                                                                 43580040
394400     IF OB-SUB = 1                                                43590040
394500        IF NOT ENGINE-CRAFT                                       43600040
394600           GO TO P5100-WRITE-OFF-DETAIL-EXIT                      43610040
394700        ELSE                                                      43620040
394800           IF HOSTLER-CRAFT                                       43630040
394900              GO TO P5100-WRITE-OFF-DETAIL-EXIT                   43640040
395000           END-IF                                                 43650040
395100        END-IF                                                    43660040
395200     ELSE                                                         43670040
395300        IF OB-SUB = 2                                             43680040
395400           IF NOT CONDUCTOR-CRAFT AND NOT BRAKEMAN-CRAFT          43690040
395500                 GO TO P5100-WRITE-OFF-DETAIL-EXIT                43700040
395600           END-IF                                                 43710040
395700        ELSE                                                      43720040
395800           IF NOT SWITCHMAN-CRAFT                                 43730040
395900              GO TO P5100-WRITE-OFF-DETAIL-EXIT                   43740040
396000           END-IF                                                 43750040
396100        END-IF                                                    43760040
396200     END-IF                                                       43770040
396300*                                                                 43780040
396400     PERFORM P8500-GET-MASTER-JOBS                                43790040
396500     MOVE SPACES TO LAYOFF-1                                      43800040
396600     MOVE EMP-NAME TO LAYOFF-NAME                                 43810040
396700     IF WS-CANADIAN-COMPANY                                       43820040
396800        MOVE SPACES                 TO LAYOFF-HOS-AREA            43830040
396900     ELSE                                                         43840040
397000        INITIALIZE PS94-COMMAREA-PARMS                            43850040
397100        MOVE EMP-NBR  OF WS-MSTR    TO PS94-EMP-NBR               43860040
397200        PERFORM P4000-GET-HOS                                     43870040
397300        MOVE SPACES                 TO LAYOFF-HOS-AREA            43880040
397400        STRING 'TOT '                                             43890040
397500          WS-TOT-TM-HH ':'                                        43900040
397600          WS-TOT-TM-MM                                            43910040
397700            ' LIM '                                               43920040
397800          WS-LIMBO-TM-HH ':'                                      43930040
397900          WS-LIMBO-TM-MM                                          43940040
398000            ' ST:'                                                43950040
398100          WS-CONSEC-STARTS                                        43960040
398200          DELIMITED BY SIZE INTO LAYOFF-HOS-AREA                  43970040
398300     END-IF                                                       43980040
398400*    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LO-ARRAY-MAX         43990040
398500*      IF LAYOFF-CODE-1 = WS-LO-CODE(I)                           44000040
398600*        MOVE WS-LO-CODE-DESC(I, 1) TO LAYOFF-STATUS              44010040
398700*        MOVE  LO-ARRAY-MAX TO I                                  44020040
398800*      END-IF                                                     44030040
398900*    END-PERFORM                                                  44040040
399000                                                                  44050040
399100     MOVE SPACES                    TO P956-COMMAREA-PARMS        44060040
399200     SET P956-GET-CNTL-STATUS-CODE  TO TRUE                       44070040
399300     MOVE LAYOFF-CODE-1             TO P956-STATUS-CODE           44080040
399400     PERFORM P9840-RETRIEVE-CNTL-INFO                             44090040
399500     IF P956-ERROR-FOUND                                          44100040
399600        MOVE 'NOT FOUND'            TO LAYOFF-STATUS              44110040
399700     ELSE                                                         44120040
399800        MOVE P956-STATUS-LONG-DESC  TO LAYOFF-STATUS              44130040
399810*CNC0573 - BEG                                                    44140080
399811        IF  PSTCA-FROM-FLD-MENU                                   44150081
399812        AND PSTCA-FLD-MENU-OPT NOT = '004'                        44160081
399820           PERFORM P9830-RETRIEVE-CNTL-INFO                       44170081
399830           IF P956-MASK-FLD-SCR-YES                               44180081
399840              MOVE '**     OFF     ** ' TO LAYOFF-STATUS          44190082
399841*CNC0576 - BEG                                                    44200082
399842           ELSE                                                   44210082
399843              IF P956-MASK-HOLD-TURN                              44220082
399844                 MOVE '**OFF HOLD TURN** ' TO LAYOFF-STATUS       44230082
399845              END-IF                                              44240082
399850           END-IF                                                 44250081
399851*CNC0576 - END                                                    44260082
399852        END-IF                                                    44270081
399860*CNC0573 - END                                                    44280080
399900     END-IF                                                       44290040
400000                                                                  44300040
400100*    EVALUATE TRUE                                                44310040
400200*      WHEN NOT-NOTIFIED   MOVE 'NOT NOTIFIED' TO LAYOFF-STATUS   44320040
400300*      WHEN ON-CALL        MOVE 'ON CALL'      TO LAYOFF-STATUS   44330040
400400*    END-EVALUATE                                                 44340040
400500                                                                  44350040
400600     IF SCR-SUB > SCR-MAX                                         44360040
400700        PERFORM P5010-OFF-BOARD-TITLE                             44370040
400800     END-IF                                                       44380040
400900     MOVE SPACES                 TO PAGE-LINE(SCR-SUB)            44390040
401000     MOVE LAYOFF-1               TO PAGE-LINE(SCR-SUB)            44400040
401100     ADD  1                      TO SCR-SUB                       44410040
401200     .                                                            44420040
401300*                                                                 44430040
401400 P5100-WRITE-OFF-DETAIL-EXIT.                                     44440040
401500     EXIT.                                                        44450040
401600*                                                                 44460040
401700*                                                                 44470040
401800 P5200-CHECK-COMPANY-CD.                                          44480040
401900*                                                                 44490040
402000     MOVE SPACES                 TO CNTLKEY                       44500040
402100                                    CNTLKEY-AREA                  44510040
402200     MOVE DIST OF WS-MSTR        TO CNTL-DIST                     44520040
402300     SET DIST-TYPE-REC           TO TRUE                          44530040
402400     MOVE CNTLKEY-AREA           TO CNTLKEY                       44540040
402500     PERFORM P8000-READ-CNTLFILE                                  44550040
402600     IF CNTL-BCR-COMPANY                                          44560040
402700        OR CNTL-CN-COMPANY                                        44570040
402800        OR CNTL-ACR-COMPANY                                       44580040
402900        SET WS-CANADIAN-COMPANY  TO TRUE                          44590040
403000     ELSE                                                         44600040
403100        SET WS-US-COMPANY        TO TRUE                          44610040
403200     END-IF                                                       44620040
403300     IF WS-CANADIAN-COMPANY                                       44630040
403400        SET DONT-APPLY-LEAD-TIME TO TRUE                          44640040
403500     ELSE                                                         44650040
403600        SET APPLY-LEAD-TIME      TO TRUE                          44660040
403700     END-IF                                                       44670040
403800     .                                                            44680040
403900*                                                                 44690040
404000 P7500-GET-UFP-EMPS.                                              44700040
404100                                                                  44710040
404200     MOVE ZERO TO OWNER-EMP-NBR TEMP-EMP-ONE ON-DUTY-EMP          44720040
404300     MOVE SPACE TO WS-ASGN-FILE                                   44730040
404400     MOVE 'U' TO ASGN-JOB-TYPE                                    44740040
404500     MOVE UFPTURN-AREA TO ASGN-ASSIGNMENT                         44750040
404600     PERFORM PXXXX-JOB-OWNER                                      44760040
404700     MOVE ASGN-EMP-NO TO OWNER-EMP-NBR                            44770040
404800                                                                  44780040
404900     MOVE SPACE TO WS-ASGN-FILE                                   44790040
405000     MOVE 'U' TO ASGN-JOB-TYPE                                    44800040
405100     MOVE UFPTURN-AREA TO ASGN-ASSIGNMENT                         44810040
405200     PERFORM PXXXX-ON-DUTY-EMP                                    44820040
405300     MOVE ASGN-EMP-NO TO ON-DUTY-EMP                              44830040
405400                                                                  44840040
405500     MOVE SPACE TO WS-ASGN-FILE                                   44850040
405600     MOVE 'U' TO ASGN-JOB-TYPE                                    44860040
405700     MOVE UFPTURN-AREA TO ASGN-ASSIGNMENT                         44870040
405800     PERFORM PXXXX-LATEST-TEMP                                    44880040
405900     MOVE ASGN-EMP-NO TO TEMP-EMP-ONE.                            44890040
406000                                                                  44900040
406100 P7800-GET-AJ-EMPS.                                               44910040
406200                                                                  44920040
406300     MOVE ZERO TO OWNER-EMP-NBR TEMP-EMP-ONE ON-DUTY-EMP          44930040
406400     MOVE SPACE TO WS-ASGN-FILE                                   44940040
406500     SET ASGN-AJ-JOB IN ASGN-JOB-TYPE TO TRUE                     44950040
406600     MOVE AJ-JOB-DIST                 TO ASGN-DIST                44960040
406700     MOVE AJ-JOB-SUB-DIST             TO ASGN-SUB-DIST            44970040
406800     MOVE AJ-JOB-ASSIGNMENT           TO ASGN-AJ-JOB              44980040
406900                                         IN ASGN-ASSIGNMENT       44990040
407000     PERFORM PXXXX-JOB-OWNER                                      45000040
407100     MOVE ASGN-EMP-NO TO OWNER-EMP-NBR                            45010040
407200                                                                  45020040
407300     MOVE SPACE TO WS-ASGN-FILE                                   45030040
407400     SET ASGN-AJ-JOB IN ASGN-JOB-TYPE TO TRUE                     45040040
407500     MOVE AJ-JOB-DIST                 TO ASGN-DIST                45050040
407600     MOVE AJ-JOB-SUB-DIST             TO ASGN-SUB-DIST            45060040
407700     MOVE AJ-JOB-ASSIGNMENT           TO ASGN-AJ-JOB              45070040
407800                                         IN ASGN-ASSIGNMENT       45080040
407900     PERFORM PXXXX-ON-DUTY-EMP                                    45090040
408000     MOVE ASGN-EMP-NO TO ON-DUTY-EMP                              45100040
408100                                                                  45110040
408200     MOVE SPACE TO WS-ASGN-FILE                                   45120040
408300     SET ASGN-AJ-JOB IN ASGN-JOB-TYPE TO TRUE                     45130040
408400     MOVE AJ-JOB-DIST                 TO ASGN-DIST                45140040
408500     MOVE AJ-JOB-SUB-DIST             TO ASGN-SUB-DIST            45150040
408600     MOVE AJ-JOB-ASSIGNMENT           TO ASGN-AJ-JOB              45160040
408700                                         IN ASGN-ASSIGNMENT       45170040
408800     PERFORM PXXXX-LATEST-TEMP                                    45180040
408900     MOVE ASGN-EMP-NO TO TEMP-EMP-ONE.                            45190040
409000                                                                  45200040
409100 P7900-TITLE.                                                     45210040
409200                                                                  45220040
409300     MOVE SPACES TO WS-CNTL-FILE                                  45230040
409400     SET SUB-DIST-TYPE-REC TO TRUE                                45240040
409500     MOVE P27NCA-DIST   TO CNTL-DIST                              45250040
409600     IF DIST-SDIST-POOL-LM-MC-JN                                  45260040
409700        MOVE WS-SUB-DIST-JX TO CNTL-SUB-DIST                      45270040
409800     ELSE                                                         45280040
409900        MOVE P27NCA-SUB-DIST TO CNTL-SUB-DIST                     45290040
410000     END-IF                                                       45300040
410100     MOVE CNTLKEY-AREA  TO CNTLKEY                                45310040
410200     PERFORM P8000-READ-CNTLFILE                                  45320040
410300     IF SUB-DIST-TYPE-REC AND CNTL-SUB-DIST-NAME > SPACES         45330040
410400        MOVE 26                 TO CTXT-UNF-FIELD-LEN             45340040
410500        MOVE CNTL-SUB-DIST-NAME TO CTXT-UNF-FIELD                 45350040
410600        PERFORM P8994-CENTER-TEXT                                 45360040
410700        MOVE CTXT-FOR-FIELD    TO REPORT-TITLE-TERM               45370040
410800     ELSE                                                         45380040
410900        MOVE '<< NONE ON FILE >>' TO REPORT-TITLE-TERM            45390040
411000     END-IF                                                       45400040
411100                                                                  45410040
411200     MOVE WS-LOCAL-YR           TO TIT-YR                         45420073
411300     MOVE WS-LOCAL-MO           TO TIT-MO                         45430073
411400     MOVE WS-LOCAL-DY           TO TIT-DY                         45440073
411500     MOVE WS-LOCAL-HR           TO TIT-HR                         45450073
411510     MOVE WS-LOCAL-MN           TO TIT-MN                         45460073
411520                                                                  45470073
411600     ADD 1 TO REPORT-TITLE-PAGE                                   45480040
411700     PERFORM P9100-WRITE-PAGE                                     45490040
411800     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  45500040
411900     MOVE TITLE-CUSTOMER                   TO PAGE-LINE(SCR-SUB)  45510040
412000     ADD 1                                 TO SCR-SUB             45520040
412100     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  45530040
412200     MOVE REPORT-TITLE-TERMINAL            TO PAGE-LINE(SCR-SUB)  45540040
412300     ADD 1                                 TO SCR-SUB             45550040
412400     MOVE SPACES           TO PAGE-LINE(SCR-SUB)                  45560040
412500     MOVE REPORT-TITLE                     TO PAGE-LINE(SCR-SUB)  45570040
412600     ADD 1                                 TO SCR-SUB.            45580040
412700                                                                  45590040
412800 P8000-READ-CNTLFILE.                                             45600040
412900                                                                  45610040
413000***  READ CONTROL-FILE RECORD                                     45620040
413100***                    INTO WS-CNTL-FILE                          45630040
413200***                    KEY IS CNTLKEY                             45640040
413300***       INVALID KEY CONTINUE                                    45650040
413400***  END-READ.                                                    45660040
413500     EXEC CICS READ                                               45670040
413600               DATASET(CNTL-FILE-VIA-CNTLKEY)                     45680040
413700               INTO(WS-CNTL-FILE)                                 45690040
413800               LENGTH(CNTLFILE-RLGTH)                             45700040
413900               RIDFLD(CNTLKEY)                                    45710040
414000               KEYLENGTH(CNTLFILE-KLGTH)                          45720040
414100               RESP(WS-RESPONSE)                                  45730040
414200     END-EXEC                                                     45740040
414300     MOVE WS-RESPONSE TO FILE-STATUS.                             45750040
414400                                                                  45760040
414500 P8000-START-CNTLFILE.                                            45770040
414600                                                                  45780040
414700***  START CONTROL-FILE KEY > CNTLKEY                             45790040
414800***        INVALID KEY CONTINUE                                   45800040
414900***  END-START.                                                   45810040
415000     EXEC CICS STARTBR                                            45820040
415100               DATASET(CNTL-FILE-VIA-CNTLKEY)                     45830040
415200               RIDFLD(CNTLKEY)                                    45840040
415300               GTEQ                                               45850040
415400               RESP(WS-RESPONSE)                                  45860040
415500     END-EXEC                                                     45870040
415600     MOVE WS-RESPONSE           TO FILE-STATUS.                   45880040
415700*                                                                 45890040
415800 P8000-READ-NEXT-CNTLFILE.                                        45900040
415900                                                                  45910040
416000***  READ CONTROL-FILE NEXT RECORD INTO WS-CNTL-FILE              45920040
416100***       AT END CONTINUE                                         45930040
416200***  END-READ.                                                    45940040
416300     EXEC CICS READNEXT                                           45950040
416400               DATASET(CNTL-FILE-VIA-CNTLKEY)                     45960040
416500               INTO(WS-CNTL-FILE)                                 45970040
416600               LENGTH(CNTLFILE-RLGTH)                             45980040
416700               RIDFLD(CNTLKEY)                                    45990040
416800               KEYLENGTH(CNTLFILE-KLGTH)                          46000040
416900               RESP(WS-RESPONSE)                                  46010040
417000     END-EXEC                                                     46020040
417100     MOVE WS-RESPONSE TO FILE-STATUS.                             46030040
417200                                                                  46040040
417300 P8000-END-CNTLFILE.                                              46050040
417400                                                                  46060040
417500     EXEC CICS ENDBR                                              46070040
417600               DATASET(CNTL-FILE-VIA-CNTLKEY)                     46080040
417700               RESP(WS-RESPONSE)                                  46090040
417800     END-EXEC.                                                    46100040
417900                                                                  46110040
418000 P8200-STARTBR-JS.                                                46120040
418100*                                                                 46130040
418200     EXEC CICS STARTBR                                            46140040
418300               DATASET(JS-VIA-JSKEY1)                             46150040
418400               RIDFLD(JSKEY1)                                     46160040
418500               GTEQ                                               46170040
418600               RESP(WS-RESPONSE)                                  46180040
418700     END-EXEC                                                     46190040
418800     MOVE WS-RESPONSE           TO FILE-STATUS.                   46200040
418900*                                                                 46210040
419000 P8220-READNEXT-JS.                                               46220040
419100*                                                                 46230040
419200***  READ JS-FILE                                                 46240040
419300***       NEXT RECORD                                             46250040
419400***       INTO WS-JOB-SCHEDULE                                    46260040
419500***       AT END CONTINUE                                         46270040
419600***  END-READ                                                     46280040
419700     EXEC CICS READNEXT                                           46290040
419800               DATASET(JS-VIA-JSKEY1)                             46300040
419900               INTO(WS-JOB-SCHEDULE)                              46310040
420000               LENGTH(JSKEY1-RLGTH)                               46320040
420100               RIDFLD(JSKEY1)                                     46330040
420200               KEYLENGTH(JSKEY1-KLGTH)                            46340040
420300               RESP(WS-RESPONSE)                                  46350040
420400     END-EXEC                                                     46360040
420500     MOVE WS-RESPONSE TO FILE-STATUS.                             46370040
420600*                                                                 46380040
420700 P8230-END-JS.                                                    46390040
420800*                                                                 46400040
420900     EXEC CICS ENDBR                                              46410040
421000               DATASET(JS-VIA-JSKEY1)                             46420040
421100               RESP(WS-RESPONSE)                                  46430040
421200     END-EXEC.                                                    46440040
421300*                                                                 46450040
421400 P8240-READ-JS.                                                   46460040
421500*                                                                 46470040
421600***  READ JS-FILE RECORD                                          46480040
421700***       INTO WS-JOB-SCHEDULE                                    46490040
421800***       INVALID KEY CONTINUE                                    46500040
421900***  END-READ                                                     46510040
422000     EXEC CICS READ                                               46520040
422100               DATASET(JS-VIA-JSKEY1)                             46530040
422200               INTO(WS-JOB-SCHEDULE)                              46540040
422300               LENGTH(JSKEY1-RLGTH)                               46550040
422400               RIDFLD(JSKEY1)                                     46560040
422500               KEYLENGTH(JSKEY1-KLGTH)                            46570040
422600               RESP(WS-RESPONSE)                                  46580040
422700     END-EXEC                                                     46590040
422800     MOVE WS-RESPONSE TO FILE-STATUS.                             46600040
422900                                                                  46610040
423000*                                                                 46620041
423100 P8300-START-TASK-FILE.                                           46630041
423200*                                                                 46640041
423300     MOVE TASK-EMPLOYEE-KEY TO TASKEMPK                           46650041
423400     EXEC CICS STARTBR                                            46660041
423500               DATASET(TASK-VIA-EMP-NBR)                          46670041
423600               RIDFLD(TASKEMPK)                                   46680041
423700               GTEQ                                               46690041
423800               RESP(WS-RESPONSE)                                  46700041
423900     END-EXEC                                                     46710041
424000     MOVE WS-RESPONSE TO FILE-STATUS                              46720041
424100     IF NOT SUCCESS                                               46730041
424200        IF NO-RECORD-FND OR END-OF-FILE                           46740041
424300           CONTINUE                                               46750041
424400        ELSE                                                      46760041
424500           MOVE 'P8300-1' TO ERR-PARAGRAPH                        46770041
424600           MOVE TASKEMPK TO ERR-KEY                               46780041
424700           PERFORM P9999-GOT-PROBLEM                              46790041
424800        END-IF                                                    46800041
424900     END-IF                                                       46810041
425000     .                                                            46820041
425100                                                                  46830041
425200***************************************************************** 46840041
425300 P8310-READNEXT-TASK-FILE.                                        46850041
425400***************************************************************** 46860041
425500     EXEC CICS READNEXT                                           46870041
425600               DATASET(TASK-VIA-EMP-NBR)                          46880041
425700               INTO(WS-TASK)                                      46890041
425800               LENGTH(TASKENBR-RLGTH)                             46900041
425900               RIDFLD(TASKEMPK)                                   46910041
426000               KEYLENGTH(TASKENBR-KLGTH)                          46920041
426100               RESP(WS-RESPONSE)                                  46930041
426200     END-EXEC                                                     46940041
426300     MOVE WS-RESPONSE TO FILE-STATUS                              46950041
426400     IF NOT SUCCESS                                               46960041
426500        IF NO-RECORD-FND OR END-OF-FILE                           46970041
426600           CONTINUE                                               46980041
426700        ELSE                                                      46990041
426800           MOVE 'P8310-1' TO ERR-PARAGRAPH                        47000041
426900           MOVE TASKEMPK TO ERR-KEY                               47010041
427000           PERFORM P9999-GOT-PROBLEM                              47020041
427100        END-IF                                                    47030041
427200     END-IF                                                       47040041
427300     .                                                            47050041
427400                                                                  47060041
427500***************************************************************** 47070041
427600 P8320-ENDBR-TASK-FILE.                                           47080041
427700***************************************************************** 47090041
427800     EXEC CICS ENDBR                                              47100041
427900               DATASET(TASK-VIA-EMP-NBR)                          47110041
428000               RESP(WS-RESPONSE)                                  47120041
428100     END-EXEC                                                     47130041
428200     MOVE WS-RESPONSE TO FILE-STATUS                              47140041
428300     IF NOT SUCCESS                                               47150041
428400        MOVE 'P8320-1'  TO ERR-PARAGRAPH                          47160041
428500        MOVE TASKEMPK   TO ERR-KEY                                47170041
428600        PERFORM P9999-GOT-PROBLEM                                 47180041
428700     END-IF                                                       47190041
428800     .                                                            47200041
428900 P8500-READ-MASTER.                                               47210040
429000                                                                  47220040
429100     MOVE MSTRNBRK TO ERR-KEY                                     47230040
429200                      EMP-NBR-KEY                                 47240040
429300     SET DISPLAY-HOS TO TRUE                                      47250040
429400     IF MSTRNBRK > ZERO                                           47260040
429500       EXEC CICS READ                                             47270040
429600                 DATASET(MSTR-VIA-EMP-NBR)                        47280040
429700                 INTO(WS-MSTR)                                    47290040
429800                 LENGTH(MSTRENBR-RLGTH)                           47300040
429900                 RIDFLD(MSTRNBRK)                                 47310040
430000                 KEYLENGTH(MSTRENBR-KLGTH)                        47320040
430100                 RESP(WS-RESPONSE)                                47330040
430200       END-EXEC                                                   47340040
430300       MOVE WS-RESPONSE TO FILE-STATUS                            47350040
430400       IF NOT SUCCESS                                             47360040
430500          IF NOT (END-OF-FILE OR NO-RECORD-FND)                   47370040
430600             MOVE MSTRNBRK              TO ERR-KEY                47380040
430700             MOVE 'MASTERFILE READ FAIL' TO ERR-SENTENCE          47390086
430800             PERFORM P9999-GOT-PROBLEM                            47400040
430900          END-IF                                                  47410040
431000          SET DONT-DISPLAY-HOS TO TRUE                            47420040
431100       ELSE                                                       47430040
431200         PERFORM P8500-GET-MASTER-JOBS                            47440040
431300       END-IF                                                     47450040
431400     ELSE                                                         47460040
431500       SET DONT-DISPLAY-HOS TO TRUE                               47470040
431600     END-IF.                                                      47480040
431700                                                                  47490040
431800 P8500-GET-MASTER-JOBS.                                           47500040
431900                                                                  47510040
432000     MOVE SPACES TO WS-ASGN-FILE                                  47520040
432100          NORMAL-ASGNMT TEMPORARY-ASGNMT ON-DUTY-ASGNMT           47530040
432200     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       47540040
432300     PERFORM PXXXX-JOB-OWNED                                      47550040
432400     MOVE ASGN-JOB-TYPE TO NORMAL-ASGNMT-FLAG                     47560040
432500     MOVE ASGN-ASSIGNMENT TO NORMAL-ASGNMT                        47570040
432600                                                                  47580040
432700     MOVE SPACES TO WS-ASGN-FILE                                  47590040
432800     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       47600040
432900     PERFORM PXXXX-LATEST-TEMP-JOB                                47610040
433000     MOVE ASGN-JOB-TYPE TO TEMPORARY-ASGNMT-FLAG                  47620040
433100     MOVE SPACE TO TEMP-ASGN-XB-AUG-FLAG                          47630040
433200     IF ASGN-JOB-TYPE = 'X'                                       47640040
433300       AND AUGMENTED-TO-EXTRA-BOARD                               47650040
433400           SET TEMP-ASGN-XB-AUG TO TRUE                           47660040
433500     END-IF                                                       47670040
433600     MOVE ASGN-ASSIGNMENT TO TEMPORARY-ASGNMT                     47680040
433700                                                                  47690040
433800     MOVE SPACES TO WS-ASGN-FILE                                  47700040
433900     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       47710040
434000     PERFORM PXXXX-JOB-ON-DUTY                                    47720040
434100     MOVE ASGN-JOB-TYPE TO ON-DUTY-ASGNMT-FLAG                    47730040
434200     MOVE ASGN-ASSIGNMENT TO ON-DUTY-ASGNMT                       47740040
434300     MOVE ASGN-ON-DUTY-DATE-TIME TO ON-DUTY-OUT-TOWN-CODE         47750040
434400     .                                                            47760040
434500*CNC0516-BEG                                                      47770058
434600*                                                                 47780057
434700 P8600-READ-MSTR2.                                                47790057
434800*                                                                 47800057
434900     EXEC CICS READ                                               47810057
435000               DATASET(MSTR2-VIA-EMP-NBR)                         47820057
435100               INTO(WS-MSTR2)                                     47830057
435200               LENGTH(MSTR2ENBR-RLGTH)                            47840057
435300               RIDFLD(MSTR2NBRK)                                  47850057
435400               KEYLENGTH(MSTR2ENBR-KLGTH)                         47860057
435500               RESP(WS-RESPONSE)                                  47870057
435600     END-EXEC                                                     47880057
435700     MOVE WS-RESPONSE TO FILE-STATUS.                             47890057
435800*CNC0516-END                                                      47900058
435900*                                                                 47910064
436000*C970-START                                                       47920064
436100*                                                                 47930064
436200 P8710-READ-AHKEY2.                                               47940067
436300*                                                                 47950064
436400     MOVE TRAIN-SYMBOL        TO AHK2-ASSIGNMENT                  47960064
436500     MOVE UFP-TRAIN-TIME-DATE TO AHK2-EFF-DATE                    47970064
436600     MOVE UFP-TRAIN-TIME-HRMN TO AHK2-EFF-TIME                    47980064
436700                                                                  47990064
436800     MOVE AH-KEY-2            TO AH2KEY                           48000064
436900     MOVE AH-FILE-VIA-K2      TO AH-DATASET                       48010064
437000                                                                  48020064
437100     EXEC CICS READ                                               48030064
437200               DATASET(AH-DATASET)                                48040064
437300               INTO(WS-AHIST)                                     48050064
437400               LENGTH(AH2-RLGTH)                                  48060064
437500               RIDFLD(AH2KEY)                                     48070064
437600               KEYLENGTH(AH2-KLGTH)                               48080064
437700               RESP(WS-RESPONSE)                                  48090064
437800     END-EXEC.                                                    48100065
437900     MOVE WS-RESPONSE         TO FILE-STATUS                      48110067
438000     IF SUCCESS                                                   48120067
438100        OR NO-RECORD-FND                                          48130067
438200        CONTINUE                                                  48140067
438300     ELSE                                                         48150067
438400        MOVE 'P8700-1'                  TO ERR-PARAGRAPH          48160067
438500        MOVE  AH2KEY                    TO ERR-KEY                48170067
438600        PERFORM P9999-GOT-PROBLEM                                 48180067
438700     END-IF.                                                      48190067
438800*C970-END                                                         48200064
438900*                                                                 48210064
439000*=================================================================48220040
439100 P9000-SEND-MAP-AND-RETURN.                                       48230040
439200*=================================================================48240040
439300     IF MSGLOG-CODE                      > SPACES                 48250040
439400         PERFORM P9030-GET-MESSAGE                                48260040
439500         MOVE MSGLOG-MESSAGE-AREA       TO SCR27N-ERRORMSG        48270040
439600     END-IF                                                       48280040
439700                                                                  48290040
439800     MOVE P27N-MAP-VERSION(PSTCA-SUB)   TO P27N-MAP               48300040
439900     IF CREATE-SCREEN                                             48310040
440000        PERFORM P9010-SEND-PHYSICAL-MAP                           48320040
440100     ELSE                                                         48330040
440200        PERFORM P9020-SEND-DATAONLY-MAP                           48340040
440300     END-IF                                                       48350040
440400     EXEC CICS RETURN                                             48360040
440500               TRANSID(P27N-TRAN)                                 48370040
440600               COMMAREA(PSTCOMM-AREA)                             48380040
440700               LENGTH(PSTCOMM-LGTH)                               48390040
440800     END-EXEC.                                                    48400040
440900*=================================================================48410040
441000 P9010-SEND-PHYSICAL-MAP.                                         48420040
441100*=================================================================48430040
441200     EXEC CICS SEND MAP(P27N-MAP)                                 48440040
441300                    MAPSET(P27N-SET)                              48450040
441400                    FROM(PSTS27N)                                 48460040
441500                    CURSOR                                        48470040
441600                    FREEKB                                        48480040
441700                    ERASE                                         48490040
441800                    RESP(WS-RESPONSE)                             48500040
441900     END-EXEC                                                     48510040
442000     MOVE WS-RESPONSE                   TO FILE-STATUS            48520040
442100     IF NOT SUCCESS                                               48530040
442200        MOVE 'P9010-1'                  TO ERR-PARAGRAPH          48540040
442300        PERFORM P9999-GOT-PROBLEM                                 48550040
442400     END-IF.                                                      48560040
442500*=================================================================48570040
442600 P9020-SEND-DATAONLY-MAP.                                         48580040
442700*=================================================================48590040
442800     EXEC CICS SEND MAP(P27N-MAP)                                 48600040
442900                    MAPSET(P27N-SET)                              48610040
443000                    FROM(PSTS27N)                                 48620040
443100                    DATAONLY                                      48630040
443200                    FREEKB                                        48640040
443300                    CURSOR                                        48650040
443400                    RESP(WS-RESPONSE)                             48660040
443500     END-EXEC                                                     48670040
443600     MOVE WS-RESPONSE                   TO FILE-STATUS            48680040
443700     IF NOT SUCCESS                                               48690040
443800        MOVE 'P9020-1'                  TO ERR-PARAGRAPH          48700040
443900        PERFORM P9999-GOT-PROBLEM                                 48710040
444000     END-IF.                                                      48720040
444100*=================================================================48730040
444200 P9030-GET-MESSAGE.                                               48740040
444300*=================================================================48750040
444400     MOVE PSTCA-SUB                     TO MSGLOG-SUB-CODE        48760040
444500     EXEC CICS READ                                               48770040
444600               DATASET(MSGLOG-VIA-CODE)                           48780040
444700               INTO(MSGLOG-AREA)                                  48790040
444800               LENGTH(MSGLOG-RLGTH)                               48800040
444900               RIDFLD(MSGLOG-KEY)                                 48810040
445000               KEYLENGTH(MSGLOG-KLGTH)                            48820040
445100               RESP(WS-RESPONSE)                                  48830040
445200     END-EXEC                                                     48840040
445300     MOVE WS-RESPONSE                   TO FILE-STATUS            48850040
445400     IF NOT SUCCESS                                               48860040
445500        IF PSTCA-SUB                     = 1                      48870040
445600           MOVE 'NO MESSAGE ON FILE'    TO MSGLOG-MESSAGE         48880040
445700        ELSE                                                      48890040
445800           MOVE 'AUCUN MESSAGE'         TO MSGLOG-MESSAGE         48900040
445900        END-IF                                                    48910040
446000     END-IF                                                       48920040
446100     MOVE MSGLOG-CODE                   TO MSGLOG-MSG-CODE        48930040
446200     MOVE '-'                           TO MSGLOG-MSG-SEP         48940040
446300     MOVE MSGLOG-SUB-CODE               TO MSGLOG-MSG-SUB-CODE.   48950040
446400*=================================================================48960040
446500 P9100-WRITE-PAGE.                                                48970040
446600*                                                                 48980040
446700     IF PAGE-ARRAY-AREA > SPACES                                  48990040
446800        MOVE PAGE-ARRAY-AREA               TO P27NTSQ-AREA        49000040
446900        ADD 1                              TO P27NCA-MAX-PAGE     49010040
447000        MOVE P27NCA-MAX-PAGE               TO P27NTSQ-QUEUE-ITEM  49020040
447100        MOVE EIBTRMID                      TO P27NTSQ-TERM-ID     49030040
447200        EXEC CICS WRITEQ TS                                       49040040
447300                  QUEUE(P27NTSQ-QUEUE-ID)                         49050040
447400                  FROM(P27NTSQ-AREA)                              49060040
447500                  LENGTH(P27NTSQ-QLGTH)                           49070040
447600                  ITEM(P27NTSQ-QUEUE-ITEM)                        49080040
447700                  RESP(WS-RESPONSE)                               49090040
447800        END-EXEC                                                  49100040
447900        MOVE WS-RESPONSE        TO FILE-STATUS                    49110040
448000        IF NOT SUCCESS                                            49120040
448100           MOVE 'P9100-1'       TO ERR-PARAGRAPH                  49130040
448200           MOVE P27NTSQ-QUEUE-ID TO ERR-KEY                       49140040
448300           PERFORM P9999-GOT-PROBLEM                              49150040
448400        END-IF                                                    49160040
448500     END-IF                                                       49170040
448600                                                                  49180040
448700     MOVE SPACES                           TO PAGE-ARRAY-AREA     49190040
448800     MOVE 1                                TO SCR-SUB.            49200040
448900*                                                                 49210040
449000 P9200-READ-PAGE.                                                 49220040
449100*                                                                 49230040
449200     MOVE EIBTRMID                         TO P27NTSQ-TERM-ID     49240040
449300     MOVE P27NCA-CURR-PAGE                 TO P27NTSQ-QUEUE-ITEM  49250040
449400     EXEC CICS READQ TS                                           49260040
449500               QUEUE(P27NTSQ-QUEUE-ID)                            49270040
449600               INTO(P27NTSQ-AREA)                                 49280040
449700               LENGTH(P27NTSQ-QLGTH)                              49290040
449800               ITEM(P27NTSQ-QUEUE-ITEM)                           49300040
449900               RESP(WS-RESPONSE)                                  49310040
450000     END-EXEC                                                     49320040
450100     MOVE WS-RESPONSE           TO FILE-STATUS                    49330040
450200     IF NOT SUCCESS                                               49340040
450300        MOVE 'P9200-1'          TO ERR-PARAGRAPH                  49350040
450400        MOVE P27NTSQ-QUEUE-ID TO ERR-KEY                          49360040
450500        PERFORM P9999-GOT-PROBLEM                                 49370040
450600     END-IF                                                       49380040
450700     MOVE P27NTSQ-AREA               TO PAGE-ARRAY-AREA           49390040
450800     MOVE PAGE-LINE(1)               TO TITLE-CUSTOMER            49400040
450900     MOVE P27NCA-MAX-PAGE            TO REPORT-TITLE-MAX-PAGE     49410040
451000     MOVE TITLE-CUSTOMER             TO SCR27N-LINE(1)            49420040
451100     PERFORM VARYING SCR-SUB FROM 2 BY 1                          49430040
451200               UNTIL SCR-SUB > SCR-MAX                            49440040
451300        MOVE PAGE-LINE(SCR-SUB)      TO SCR27N-LINE(SCR-SUB)      49450040
451400     END-PERFORM.                                                 49460040
451500*                                                                 49470040
451600 P9300-DELETE-ALL-PAGES.                                          49480040
451700*                                                                 49490040
451800     MOVE EIBTRMID                         TO P27NTSQ-TERM-ID     49500040
451900     EXEC CICS DELETEQ TS                                         49510040
452000               QUEUE(P27NTSQ-QUEUE-ID)                            49520040
452100               RESP(WS-RESPONSE)                                  49530040
452200     END-EXEC                                                     49540040
452300     .                                                            49550040
452400*                                                                 49560040
452500 PXXXX-JOB-OWNER.                                                 49570040
452600                                                                  49580040
452700     SET ASGN-OWNER-REC TO TRUE                                   49590040
452800     MOVE ZERO TO ASGN-DATE-TIME                                  49600040
452900     MOVE ASGN-AREA TO ASGNJOB                                    49610040
453000***  READ ASGN-FILE RECORD INTO ASGN-AREA                         49620040
453100***    INVALID KEY CONTINUE                                       49630040
453200***  END-READ                                                     49640040
453300     EXEC CICS READ                                               49650040
453400               DATASET(ASGN-VIA-ASGNJOB)                          49660040
453500               INTO(WS-ASGN-FILE)                                 49670040
453600               LENGTH(ASGNJOB-RLGTH)                              49680040
453700               RIDFLD(ASGNJOB)                                    49690040
453800               KEYLENGTH(ASGNJOB-KLGTH)                           49700040
453900               RESP(WS-RESPONSE)                                  49710040
454000     END-EXEC                                                     49720040
454100     MOVE WS-RESPONSE TO FILE-STATUS                              49730040
454200     IF SUCCESS                                                   49740040
454300       CONTINUE                                                   49750040
454400     ELSE                                                         49760040
454500       IF NO-RECORD-FND OR END-OF-FILE                            49770040
454600         MOVE 000000 TO ASGN-EMP-NO                               49780040
454700       END-IF                                                     49790040
454800     END-IF.                                                      49800040
454900                                                                  49810040
455000 PXXXX-LATEST-TEMP.                                               49820040
455100                                                                  49830040
455200     MOVE SPACES       TO SAVE-ASGN-AREA                          49840040
455300     SET ASGN-TEMP-REC TO TRUE                                    49850040
455400     MOVE ZEROS        TO ASGN-DATE-TIME                          49860040
455500     MOVE ASGNKEY1     TO ASGNJOB XXXX-ASGNKEY1                   49870040
455600     MOVE ASGN-AREA    TO ASGNJOB                                 49880040
455700***  START ASGN-FILE KEY > ASGN-FS-JOB                            49890040
455800***        INVALID KEY CONTINUE                                   49900040
455900***  END-START                                                    49910040
456000     EXEC CICS STARTBR                                            49920040
456100               DATASET(ASGN-VIA-ASGNJOB)                          49930040
456200               RIDFLD(ASGNJOB)                                    49940040
456300               GTEQ                                               49950040
456400               RESP(WS-RESPONSE)                                  49960040
456500     END-EXEC                                                     49970040
456600     MOVE WS-RESPONSE           TO FILE-STATUS                    49980040
456700     IF SUCCESS                                                   49990040
456800       MOVE 0 TO ASGN-DONE-CODE                                   50000040
456900       PERFORM UNTIL ASGN-DONE                                    50010040
457000***      READ ASGN-FILE NEXT RECORD INTO ASGN-AREA                50020040
457100***        AT END CONTINUE                                        50030040
457200***      END-READ                                                 50040040
457300         EXEC CICS READNEXT                                       50050040
457400                   DATASET(ASGN-VIA-ASGNJOB)                      50060040
457500                   INTO(WS-ASGN-FILE)                             50070040
457600                   LENGTH(ASGNJOB-RLGTH)                          50080040
457700                   RIDFLD(ASGNJOB)                                50090040
457800                   KEYLENGTH(ASGNJOB-KLGTH)                       50100040
457900                   RESP(WS-RESPONSE)                              50110040
458000         END-EXEC                                                 50120040
458100         MOVE WS-RESPONSE TO FILE-STATUS                          50130040
458200         IF SUCCESS                                               50140040
458300           IF XX-ASGN-DIST = ASGN-DIST AND                        50150040
458400              XX-ASGN-SUB-DIST = ASGN-SUB-DIST AND                50160040
458500              XX-ASGN-JOB = ASGN-AJ-JOB OF ASGN-ASSIGNMENT        50170040
458600             AND ASGN-TEMP-REC                                    50180040
458700             MOVE ASGN-AREA TO SAVE-ASGN-AREA                     50190040
458800*              SET ASGN-DONE TO TRUE                              50200040
458900           ELSE                                                   50210040
459000             MOVE ZERO TO ASGN-EMP-NO                             50220040
459100             SET ASGN-DONE TO TRUE                                50230040
459200           END-IF                                                 50240040
459300         ELSE                                                     50250040
459400           IF NO-RECORD-FND OR END-OF-FILE                        50260040
459500             MOVE ZERO TO ASGN-EMP-NO                             50270040
459600             SET ASGN-DONE TO TRUE                                50280040
459700           ELSE                                                   50290040
459800             MOVE ZERO TO ASGN-EMP-NO                             50300040
459900             SET ASGN-DONE TO TRUE                                50310040
460000           END-IF                                                 50320040
460100         END-IF                                                   50330040
460200       END-PERFORM                                                50340040
460300     ELSE                                                         50350040
460400         MOVE ZERO TO ASGN-EMP-NO                                 50360040
460500     END-IF.                                                      50370040
460600     EXEC CICS ENDBR                                              50380040
460700               DATASET(ASGN-VIA-ASGNJOB)                          50390040
460800               RESP(WS-RESPONSE)                                  50400040
460900     END-EXEC                                                     50410040
461000     IF SAVE-ASGN-AREA > SPACE                                    50420040
461100       MOVE SAVE-ASGN-AREA TO ASGN-AREA                           50430040
461200     ELSE                                                         50440040
461300       MOVE ZERO TO ASGN-EMP-NO                                   50450040
461400     END-IF.                                                      50460040
461500                                                                  50470040
461600 PXXXX-ON-DUTY-EMP.                                               50480040
461700                                                                  50490040
461800     SET ASGN-ON-DUTY-REC TO TRUE                                 50500040
461900     MOVE ZERO            TO ASGN-DATE-TIME                       50510040
462000     MOVE ASGNKEY1        TO ASGNJOB                              50520040
462100***  READ ASGN-FILE RECORD INTO ASGN-AREA                         50530040
462200***       INVALID KEY CONTINUE                                    50540040
462300***  END-READ                                                     50550040
462400     EXEC CICS READ                                               50560040
462500               DATASET(ASGN-VIA-ASGNJOB)                          50570040
462600               INTO(WS-ASGN-FILE)                                 50580040
462700               LENGTH(ASGNJOB-RLGTH)                              50590040
462800               RIDFLD(ASGNJOB)                                    50600040
462900               KEYLENGTH(ASGNJOB-KLGTH)                           50610040
463000               RESP(WS-RESPONSE)                                  50620040
463100     END-EXEC                                                     50630040
463200     MOVE WS-RESPONSE TO FILE-STATUS                              50640040
463300     IF SUCCESS                                                   50650040
463400       CONTINUE                                                   50660040
463500     ELSE                                                         50670040
463600       MOVE ZERO TO ASGN-EMP-NO                                   50680040
463700     END-IF.                                                      50690040
463800                                                                  50700040
463900 PXXXX-JOB-OWNED.                                                 50710040
464000                                                                  50720040
464100     MOVE '1'       TO ASGN-EMP-NO-REC-TYPE                       50730040
464200     MOVE ZEROS     TO ASGN-EMP-DATE-TIME                         50740040
464300     MOVE ASGNKEY2  TO ASGNEMP                                    50750040
464400***  READ ASGN-FILE RECORD INTO WS-ASGN-FILE                      50760040
464500***       KEY IS ASGN-FS-EMP                                      50770040
464600***       INVALID KEY CONTINUE                                    50780040
464700***  END-READ                                                     50790040
464800     EXEC CICS READ                                               50800040
464900               DATASET(ASGN-VIA-ASGNEMP)                          50810040
465000               INTO(WS-ASGN-FILE)                                 50820040
465100               LENGTH(ASGNEMP-RLGTH)                              50830040
465200               RIDFLD(ASGNEMP)                                    50840040
465300               KEYLENGTH(ASGNEMP-KLGTH)                           50850040
465400               RESP(WS-RESPONSE)                                  50860040
465500     END-EXEC                                                     50870040
465600     MOVE WS-RESPONSE TO FILE-STATUS                              50880040
465700     IF SUCCESS                                                   50890040
465800       CONTINUE                                                   50900040
465900     ELSE                                                         50910040
466000         MOVE SPACE TO ASGN-ASSIGNMENT                            50920040
466100     END-IF.                                                      50930040
466200                                                                  50940040
466300 PXXXX-LATEST-TEMP-JOB.                                           50950040
466400                                                                  50960040
466500     MOVE SPACES        TO SAVE-ASGN-AREA                         50970040
466600     SET ASGN-EMP-TEMP-REC TO TRUE                                50980040
466700     MOVE ZEROS         TO ASGN-EMP-DATE-TIME                     50990040
466800     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       51000040
466900     MOVE ASGNKEY2      TO XXXX-ASGNKEY2                          51010040
467000     MOVE XXXX-ASGNKEY2 TO ASGNKEY2 ASGNEMP                       51020040
467100***  START ASGN-FILE KEY > ASGN-FS-EMP                            51030040
467200***       INVALID KEY CONTINUE                                    51040040
467300***  END-START                                                    51050040
467400     EXEC CICS STARTBR                                            51060040
467500               DATASET(ASGN-VIA-ASGNEMP)                          51070040
467600               RIDFLD(ASGNEMP)                                    51080040
467700               GTEQ                                               51090040
467800               RESP(WS-RESPONSE)                                  51100040
467900     END-EXEC                                                     51110040
468000     MOVE WS-RESPONSE           TO FILE-STATUS                    51120040
468100     IF SUCCESS                                                   51130040
468200        MOVE 0 TO ASGN-DONE-CODE                                  51140040
468300        PERFORM UNTIL ASGN-DONE                                   51150040
468400***        READ ASGN-FILE NEXT RECORD INTO WS-ASGN-FILE           51160040
468500***             AT END CONTINUE                                   51170040
468600***        END-READ                                               51180040
468700           EXEC CICS READNEXT                                     51190040
468800                     DATASET(ASGN-VIA-ASGNEMP)                    51200040
468900                     INTO(WS-ASGN-FILE)                           51210040
469000                     LENGTH(ASGNEMP-RLGTH)                        51220040
469100                     RIDFLD(ASGNEMP)                              51230040
469200                     KEYLENGTH(ASGNEMP-KLGTH)                     51240040
469300                     RESP(WS-RESPONSE)                            51250040
469400           END-EXEC                                               51260040
469500           MOVE WS-RESPONSE TO FILE-STATUS                        51270040
469600           IF SUCCESS                                             51280040
469700              IF ASGN-EMP-NO = XX-ASGN-EMP AND                    51290040
469800                 ASGN-EMP-NO-REC-TYPE = '2'                       51300040
469900                 MOVE ASGN-AREA TO SAVE-ASGN-AREA                 51310040
470000              ELSE                                                51320040
470100                 SET ASGN-DONE TO TRUE                            51330040
470200              END-IF                                              51340040
470300           ELSE                                                   51350040
470400              IF NO-RECORD-FND OR END-OF-FILE                     51360040
470500                 SET ASGN-DONE TO TRUE                            51370040
470600              END-IF                                              51380040
470700           END-IF                                                 51390040
470800        END-PERFORM                                               51400040
470900     END-IF                                                       51410040
471000     EXEC CICS ENDBR                                              51420040
471100               DATASET(ASGN-VIA-ASGNEMP)                          51430040
471200               RESP(WS-RESPONSE)                                  51440040
471300     END-EXEC                                                     51450040
471400     IF SAVE-ASGN-AREA > SPACES                                   51460040
471500        MOVE SAVE-ASGN-AREA TO ASGN-AREA                          51470040
471600     ELSE                                                         51480040
471700        MOVE SPACE          TO ASGN-ASSIGNMENT                    51490040
471800     END-IF.                                                      51500040
471900                                                                  51510040
472000 PXXXX-JOB-ON-DUTY.                                               51520040
472100                                                                  51530040
472200     MOVE '3'       TO ASGN-EMP-NO-REC-TYPE                       51540040
472300     MOVE EMP-NBR OF WS-MSTR TO ASGN-EMP-NO                       51550040
472400     MOVE ZEROS     TO ASGN-EMP-DATE-TIME                         51560040
472500     MOVE ASGNKEY2  TO ASGNEMP                                    51570040
472600                       XXXX-ASGNKEY2                              51580040
472700***  READ ASGN-FILE RECORD INTO WS-ASGN-FILE                      51590040
472800***       KEY IS ASGN-FS-EMP                                      51600040
472900***       INVALID KEY CONTINUE                                    51610040
473000***  END-READ                                                     51620040
473100     EXEC CICS READ                                               51630040
473200               DATASET(ASGN-VIA-ASGNEMP)                          51640040
473300               INTO(WS-ASGN-FILE)                                 51650040
473400               LENGTH(ASGNEMP-RLGTH)                              51660040
473500               RIDFLD(ASGNEMP)                                    51670040
473600               KEYLENGTH(ASGNEMP-KLGTH)                           51680040
473700               RESP(WS-RESPONSE)                                  51690040
473800     END-EXEC                                                     51700040
473900     MOVE WS-RESPONSE TO FILE-STATUS                              51710040
474000     IF SUCCESS                                                   51720040
474100        IF ASGN-EMP-NO = XX-ASGN-EMP AND                          51730040
474200          ASGN-EMP-NO-REC-TYPE = '3'                              51740040
474300            CONTINUE                                              51750040
474400        ELSE                                                      51760040
474500            MOVE SPACES TO ASGN-ASSIGNMENT                        51770040
474600        END-IF                                                    51780040
474700     ELSE                                                         51790040
474800       MOVE SPACE TO ASGN-ASSIGNMENT                              51800040
474900     END-IF.                                                      51810040
475000*                                                                 51820040
475100 P9700-SETUP-RETURN.                                              51830040
475200*                                                                 51840040
475300     EXEC CICS XCTL                                               51850040
475400               PROGRAM(P43C-PGM)                                  51860040
475500               COMMAREA(PSTCOMM-AREA)                             51870040
475600               LENGTH(PSTCOMM-LGTH)                               51880040
475700               RESP(WS-RESPONSE)                                  51890040
475800     END-EXEC                                                     51900040
475900     MOVE WS-RESPONSE                   TO FILE-STATUS            51910040
476000     IF NOT SUCCESS                                               51920040
476100        MOVE 'P9700-1'                  TO ERR-PARAGRAPH          51930040
476200        PERFORM P9999-GOT-PROBLEM                                 51940040
476300     END-IF.                                                      51950040
476400*                                                                 51960040
476500 P9810-PROCESS-OFFSET.                                            51970040
476600*                                                                 51980040
476700     MOVE PSTCA-DT-OS-FUN       TO PARM-CONV-TYPE                 51990040
476800     MOVE PSTCA-DT-OS-DAYS      TO PARM-SEC-JULIAN-DAY            52000040
476900     MOVE PSTCA-DT-OS-HRMN      TO PARM-SEC-HRMN                  52010040
477000     EXEC CICS LINK                                               52020040
477100               PROGRAM(P903-PGM)                                  52030040
477200               COMMAREA(DATE-CONVERSION-PARMS)                    52040040
477300               LENGTH(P903-LGTH)                                  52050040
477400               RESP(WS-RESPONSE)                                  52060040
477500     END-EXEC                                                     52070040
477600     MOVE WS-RESPONSE           TO FILE-STATUS                    52080040
477700     IF NOT SUCCESS                                               52090040
477800        MOVE 'P9810-1'          TO ERR-PARAGRAPH                  52100040
477900        MOVE 'P903'             TO ERR-KEY                        52110040
478000        PERFORM P9999-GOT-PROBLEM                                 52120040
478100     END-IF.                                                      52130040
478200*                                                                 52140040
478300 P9820-GET-CURRENT-TIME.                                          52150040
478400*                                                                 52160040
478500     EXEC CICS ASKTIME                                            52170040
478600               ABSTIME(WS-ABSTIME)                                52180040
478700     END-EXEC                                                     52190040
478800     EXEC CICS FORMATTIME                                         52200040
478900               ABSTIME(WS-ABSTIME)                                52210040
479000               YYYYMMDD(WS-SYSTEM-DATE-CENT)                      52220040
479100               TIME(WS-SYSTEM-TIME-AREA)                          52230040
479200     END-EXEC                                                     52240040
479300*                                                                 52250040
479400*    INSTALL APPLICATION DATE/TIME                                52260040
479500*                                                                 52270040
479600     IF PSTCA-DATE-TIME-OFFSET > SPACES                           52280040
479700        MOVE ZEROS            TO DATE-CONVERSION-PARMS            52290040
479800        MOVE WS-SYSTEM-DATE   TO PARM-PRI-DATE-GREG               52300040
479900        MOVE WS-SYSTEM-TIME   TO PARM-PRI-HRMN                    52310040
480000        PERFORM P9810-PROCESS-OFFSET                              52320040
480100        MOVE PARM-RES-DATE-GREG                                   52330040
480200                            TO WS-SYSTEM-DATE                     52340040
480300        MOVE PARM-RES-GREG-CENT                                   52350040
480400                            TO WS-SYSTEM-CENT                     52360040
480500        MOVE PARM-RES-HRMN                                        52370040
480600                            TO WS-SYSTEM-TIME                     52380040
480700     END-IF                                                       52390040
480800*                                                                 52400040
480900*    CONVERT SYSTEM TIME TO LOCAL TIME                            52410040
481000*                                                                 52420040
481100     MOVE SPACES                        TO WS-CNTL-FILE           52430040
481200     SET SUB-DIST-TYPE-REC              TO TRUE                   52440040
481300     MOVE P27NCA-DIST                   TO CNTL-DIST              52450040
481400     MOVE P27NCA-SUB-DIST               TO CNTL-SUB-DIST          52460040
481500     MOVE CNTLKEY-AREA                  TO CNTLKEY                52470040
481600     PERFORM P8000-READ-CNTLFILE                                  52480040
481700     IF NOT SUCCESS                                               52490040
481800        MOVE 'P9820-1'                  TO ERR-PARAGRAPH          52500040
481900        MOVE CNTLKEY                    TO ERR-KEY                52510040
482000        PERFORM P9999-GOT-PROBLEM                                 52520040
482100     END-IF                                                       52530040
482200                                                                  52540040
482300     MOVE SPACES                        TO TZ-PARAMETERS          52550040
482400     SET TZ-IN-EASTERN-ZONE             TO TRUE                   52560040
482500     MOVE WS-PRESENT-TIME               TO TZ-IN-DATE-TIME        52570040
482600     MOVE CNTL-TIME-ZONE                TO TZ-OUT-ZONE            52580040
482700     PERFORM P8996-TIMEZONE                                       52590040
482800     MOVE TZ-OUT-DATE-TIME-CENT         TO WS-LOCAL-DATE-TIME-CENT52600040
482900     .                                                            52610050
483000*CNC0516-BEG                                                      52620050
483100*                                                                 52630041
483200 P9830-RETRIEVE-CNTL-INFO.                                        52640041
483300*                                                                 52650041
483400     MOVE SPACES                     TO P956-COMMAREA-PARMS       52660041
483500     MOVE LAYOFF-CODE-1 OF WS-MSTR   TO P956-STATUS-CODE          52670041
483600     SET P956-GET-CNTL-STATUS-REASON TO TRUE                      52680041
483700     MOVE LAYOFF-EM-CODE OF WS-MSTR  TO P956-REASON-CODE          52690041
483800     MOVE DIST     OF WS-MSTR        TO P956-DIST                 52700041
483900     MOVE SUB-DIST OF WS-MSTR        TO P956-SDIST                52710041
484000     MOVE CRAFT OF WS-MSTR           TO P956-CC                   52720041
484100     IF TEMPORARY-ASGNMT > SPACE                                  52730041
484200        MOVE TEMPORARY-ASGNMT-FLAG   TO P956-ASGN-TYPE            52740041
484300        MOVE TA-1                    TO P956-ASGN                 52750041
484400        MOVE TA-DIST                 TO P956-DIST                 52760041
484500        MOVE TA-SUB-DIST             TO P956-SDIST                52770041
484600        IF TEMP-ASGN-XB                                           52780041
484700           MOVE TA-CC                TO P956-XB                   52790041
484800        END-IF                                                    52800041
484900     ELSE                                                         52810041
485000        IF NORMAL-ASGNMT > SPACES                                 52820041
485100           MOVE NORMAL-ASGNMT-FLAG   TO P956-ASGN-TYPE            52830041
485200           MOVE NA-1                 TO P956-ASGN                 52840041
485300           MOVE NA-DIST              TO P956-DIST                 52850041
485400           MOVE NA-SUB-DIST          TO P956-SDIST                52860041
485500           IF NORM-ASGN-XB                                        52870041
485600              MOVE NA-CC             TO P956-XB                   52880041
485700           END-IF                                                 52890041
485800        END-IF                                                    52900041
485900     END-IF                                                       52910041
486000     EXEC CICS LINK                                               52920041
486100               PROGRAM (P956-PGM)                                 52930041
486200               COMMAREA(P956-COMMAREA-PARMS)                      52940041
486300               LENGTH  (P956-LGTH)                                52950041
486400               RESP    (WS-RESPONSE)                              52960041
486500     END-EXEC                                                     52970041
486600     MOVE WS-RESPONSE           TO FILE-STATUS                    52980050
486700     IF NOT SUCCESS                                               52990050
486800        MOVE 'P9830-1'          TO ERR-PARAGRAPH                  53000050
486900        MOVE P956-INPUT-PARMS   TO ERR-KEY                        53010050
487000        PERFORM P9999-GOT-PROBLEM                                 53020050
487100     END-IF.                                                      53030050
487200*CNC0516-END                                                      53040050
487300*                                                                 53050040
487400 P9840-RETRIEVE-CNTL-INFO.                                        53060040
487500*                                                                 53070040
487600     EXEC CICS LINK                                               53080040
487700               PROGRAM (P956-PGM)                                 53090040
487800               COMMAREA(P956-COMMAREA-PARMS)                      53100040
487900               LENGTH  (P956-LGTH)                                53110040
488000               RESP    (WS-RESPONSE)                              53120040
488100     END-EXEC                                                     53130040
488200     MOVE WS-RESPONSE           TO FILE-STATUS                    53140040
488300     IF NOT SUCCESS                                               53150040
488400        MOVE 'P9840-1'          TO ERR-PARAGRAPH                  53160040
488500        MOVE P956-INPUT-PARMS   TO ERR-KEY                        53170040
488600        PERFORM P9999-GOT-PROBLEM                                 53180040
488700     END-IF.                                                      53190040
488800*=================================================================53200040
488900 P9991-CALL-P903-PROGRAM.                                         53210040
489000*=================================================================53220040
489100     EXEC CICS LINK                                               53230040
489200               PROGRAM(P903-PGM)                                  53240040
489300               COMMAREA(DATE-CONVERSION-PARMS)                    53250040
489400               LENGTH(P903-LGTH)                                  53260040
489500               RESP(WS-RESPONSE)                                  53270040
489600     END-EXEC                                                     53280040
489700     MOVE WS-RESPONSE              TO FILE-STATUS                 53290040
489800     IF NOT SUCCESS                                               53300040
489900        MOVE 'P9991-1'             TO ERR-PARAGRAPH               53310040
490000        MOVE 'P903LINK'            TO ERR-KEY                     53320040
490100        PERFORM P9999-GOT-PROBLEM                                 53330040
490200     END-IF.                                                      53340040
490300*=================================================================53350040
490400 P9992-CALL-PS94-PROGRAM.                                         53360040
490500*=================================================================53370040
490600     EXEC CICS LINK                                               53380040
490700               PROGRAM  (PS94-PGM)                                53390040
490800               COMMAREA (PS94-COMMAREA-PARMS)                     53400040
490900               LENGTH   (PS94-LGTH)                               53410040
491000               RESP     (WS-RESPONSE)                             53420040
491100     END-EXEC                                                     53430040
491200                                                                  53440040
491300     MOVE WS-RESPONSE                   TO FILE-STATUS            53450040
491400     IF NOT SUCCESS                                               53460040
491500        MOVE 'P9992-S94'                TO ERR-PARAGRAPH          53470040
491600        PERFORM P9999-GOT-PROBLEM                                 53480040
491700     ELSE                                                         53490040
491800        MOVE PS94-FILE-STATUS           TO FILE-STATUS            53500040
491900     END-IF                                                       53510040
492000                                                                  53520040
492100     IF PS94-ERROR-ENCOUNTERED                                    53530040
492200        EXEC CICS RETURN END-EXEC                                 53540040
492300     END-IF.                                                      53550040
492400*=================================================================53560040
492500 P9990-CLEAR-SCREEN.                                              53570040
492600*=================================================================53580040
492700     EXEC CICS SEND CONTROL                                       53590040
492800                    ERASE                                         53600040
492900                    FREEKB                                        53610040
493000     END-EXEC                                                     53620040
493100     EXEC CICS RETURN END-EXEC.                                   53630040
493200*=================================================================53640040
493300 P9999-GOT-PROBLEM.                                               53650040
493400*=================================================================53660040
493500     PERFORM P9300-DELETE-ALL-PAGES                               53670040
RKW200*    MOVE P43C-PGM                      TO ERR-PROGRAM            53680086
RKW200     MOVE P27N-PGM                      TO ERR-PROGRAM            53681086
493700     MOVE DFHEIBLK                      TO ERR-EIBLK              53690040
493800     EXEC CICS XCTL                                               53700040
493900               PROGRAM(PSTERR-PGM)                                53710040
494000               COMMAREA(PSTERAR-AREA)                             53720040
494100               LENGTH(PSTERAR-LGTH)                               53730040
494200               RESP(WS-RESPONSE)                                  53740040
494300     END-EXEC                                                     53750040
494400     EXEC CICS ABEND                                              53760040
494500               ABCODE(PSTERR-ABCODE)                              53770040
494600               CANCEL                                             53780040
494700     END-EXEC.                                                    53790040
494800*=================================================================53800040
494900 COPY CNTRTXT.                                                    53810040
495000 COPY DATEEDIT.                                                   53820040
495100 COPY TIMEZONE.                                                   53830040
